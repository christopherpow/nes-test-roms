/*
aPACK reimplementation
Written by and copyright (C) Joel Yliluoma in 2011; http://iki.fi/bisqwit/

This program reimplements the compression and decompression
algorithm used by Joergen Ibsen's aPACK version 1.0.
Specifically, its default algorithm, which is an LZ variant...

With the following modification: End of compressed stream
is indicated the same way as in aPLib, rather than the way
it is done in aPACK.

Also, this version uses a custom stub that loads the code
at 0xA110 rather than at 0x100, and contains code to cope
with a NES header.

*/
#include <algorithm>
#include <vector>
#include <cstdio>
#include <assert.h>
#include <cstring>

#include <unistd.h>
#include <stdlib.h>

#ifdef _OPENMP
# include <omp.h>
#endif
#include <map>

bool DoVerySlow = false;

const size_t MaxDistance = 0x7FFF;

static const unsigned ORG = 0x110 + 0x4000 + 0x2000*3;

struct EncodingParams
{
    unsigned n_calls;
    unsigned call_mask;
    unsigned constbyte;
    unsigned constbyte2;
    unsigned literalbits;
    bool     appends;
    unsigned offsconstmask; // 1 = +1, 2 = -1, 4 = +2
    bool     useliteraltable;
    unsigned short_threshold; // 7F or FF
    unsigned shortmatchmin;
    unsigned limit_at_1byte;
    unsigned limit_at_2byte;

    unsigned MaxByteCopyOffset() const
    {
        unsigned m = (1u << literalbits); // 1..16 (encoded as 0..15)
        if(offsconstmask & 1)  m -= 1; if(!m) return 0;
        if(offsconstmask & 2)  m -= 1; if(!m) return 0;
        if(offsconstmask & 4)  m -= 1; if(!m) return 0;
        if(constbyte  != 0x100) m -= 1; if(!m) return 0;
        if(constbyte2 != 0x100) m -= 1; if(!m) return 0;
        return m;
    }
    unsigned ShortLengthMod() const
    {
        return 0x100 / (1 + short_threshold);
    }
    unsigned ShortLengthBits() const
    {
        for(unsigned res = 0; res <= 8; ++res)
            if( ShortLengthMod() == (1u << res))
                return res;
        return 0;
    }
    unsigned ShortLengthMax() const
    {
        return shortmatchmin + ShortLengthMod() - 1;
    }
    unsigned ShortLongDelta() const
    {
        return shortmatchmin + ShortLengthMod() - 2;
    }
    size_t OffsConstPlusIndex() const
    {
        size_t result = 0;
        if(offsconstmask == 5 && constbyte < 2 && constbyte2 == 0x100) ++result;
        return result;
    }
    size_t OffsConstMinusIndex() const
    {
        size_t result = 0;
        if(offsconstmask & 1) ++result;
        return result;
    }
    size_t OffsConstPlusTwoIndex() const
    {
        size_t result = 0;
        if(offsconstmask == 5 && constbyte < 2 && constbyte2 == 0x100) ++result;
        if(offsconstmask & 1) ++result;
        if(offsconstmask & 2) ++result;
        return result;
    }
    size_t ConstByte1Index() const
    {
        size_t result = 0;
        if(offsconstmask == 5 && constbyte < 2 && constbyte2 == 0x100) return result;
        if(offsconstmask & 1) ++result;
        if(offsconstmask & 2) ++result;
        if(offsconstmask & 4) ++result;
        return result;
    }
    size_t ConstByte2Index() const
    {
        size_t result = 0;
        if(offsconstmask & 1) ++result;
        if(offsconstmask & 2) ++result;
        if(offsconstmask & 4) ++result;
        if(constbyte != 0x100) ++result;
        return result;
    }
    size_t ByteCopyOffset(size_t o) const
    {
        size_t result = o-1; // 1..15 -> 0..14
        if(offsconstmask & 1) ++result;
        if(offsconstmask & 2) ++result;
        if(offsconstmask & 4) ++result;
        if(constbyte  != 0x100) ++result;
        if(constbyte2 != 0x100) ++result;
        return result;
    }
};

namespace
{
    static size_t MatchingLength
        (const unsigned char* in1,
         size_t size,
         const unsigned char* in2)
    {
        // Trivial implementation would be:
        //   return std::mismatch(in1,in1+size, in2).second-in2;
        // However this function attempts to gain something
        // by comparing sizeof(unsigned) bytes at time instead
        // of 1 byte at time.
        size_t len = 0;
        typedef size_t T;
        const size_t intsize = sizeof(T);
        const T*const p1 = reinterpret_cast<const T*>(in1);
        const T*const p2 = reinterpret_cast<const T*>(in2);
        while(size >= intsize && p1[len] == p2[len]) { ++len; size -= intsize; }
        len *= intsize;
        while(size > 0 && in1[len] == in2[len]) { --size; ++len; }
        return len;
    }

    static unsigned GammaScore(size_t value)
    {
        assert(value >= 2);
        unsigned highest_bit = 0x80000000u;
        while(highest_bit != 0
           && !(value & highest_bit))
            highest_bit >>= 1;
        if(value & highest_bit)
            highest_bit >>= 1;
        unsigned num_bits_sent = 0;
        for(; ; ++num_bits_sent)
        {
            ++num_bits_sent;
            highest_bit >>= 1;
            if(!highest_bit) break;
        }
        return ++num_bits_sent;
    }

    static unsigned Cost(
        size_t offset, size_t length,
        size_t prev_offset, size_t default_cost,
        const EncodingParams& params)
    {
        if(length < 2) return default_cost;
        if(offset == prev_offset)
        {
            // Penalty of an append
            const unsigned GammaScore2  = 2;
            return 2 + GammaScore2 + GammaScore(length);
        }
        if(length >= params.shortmatchmin
        && length <= params.ShortLengthMax()
        && offset >= 1 && offset <= params.short_threshold)
        {
            // Penalty of a short match
            return 3 + 8;
        }
        // Penalty of a long match
        if(offset <= params.short_threshold
        && length < 2+params.shortmatchmin)
            return 1000; // Penalize illegal combination

        size_t tmp = offset + 0x300, tmp2 = length;

        if(offset <= params.short_threshold)
            tmp2 -= params.ShortLongDelta();
        else if(params.limit_at_2byte && offset >= params.limit_at_2byte)
        {
            tmp2 -= 2;
            if(tmp2 < 2) return 1000; // Penalize illegal combination
        }
        else if(params.limit_at_1byte && offset >= params.limit_at_1byte)
        {
            tmp2 -= 1;
            if(tmp2 < 2) return 1000; // Penalize illegal combination
        }

        return 2 + 8 + GammaScore(tmp >> 8) + GammaScore(tmp2);
    }
}

class Apack
{
public:
    std::vector<unsigned char> result;
    unsigned BitsRemaining, BitsPosition;
    std::vector<unsigned> GammaSummary, GammaSummary2;

    unsigned char LiteralsInOrderOfCommonness[256]; // commonness# -> literal
    unsigned LiteralIndices[256];                   // literal -> commonness#
    unsigned NumUsedLiterals;

private:
    // First: copy length, second: begin offset
    std::vector< std::pair<size_t,size_t> > MaxMatchLen;
    std::vector< unsigned char >            DefaultCost;
    std::vector< std::pair<size_t,size_t> > Decisions;
    std::vector<bool> bomb; // bytes which must be encoded verbatim
                            // due to containing author's name

    std::vector<unsigned short> MatchCache;

private:
    size_t TestMatch(const std::vector<unsigned char>& source,
                     size_t pos1,
                     size_t pos2,
                     size_t limit)
    {
        if(pos2 > pos1) { size_t tmp = pos1; pos1=pos2; pos2=tmp; }
        const size_t p = pos1 * source.size() + pos2;
        if(MatchCache[p]) return MatchCache[p] - 1;

        size_t result = MatchingLength(&source[pos1], limit, &source[pos2]);
        MatchCache[ p ] = 1+result;
        return result;
    }

    void CreateAuxLiteralTables(const unsigned LiteralUsageTable[256])
    {
        NumUsedLiterals = 0;
        for(unsigned a=0; a<256; ++a)
        {
            LiteralIndices[a] = 9999;
            if(LiteralUsageTable[a])
                LiteralsInOrderOfCommonness[NumUsedLiterals++] = a;
        }
        for(unsigned j=1; j<NumUsedLiterals; ++j)
        {
            unsigned k = LiteralsInOrderOfCommonness[j], i;
            for(i=j; i>=1 && LiteralUsageTable[LiteralsInOrderOfCommonness[i-1]]
                           < LiteralUsageTable[k]; --i)
                LiteralsInOrderOfCommonness[i] = LiteralsInOrderOfCommonness[i-1];
            LiteralsInOrderOfCommonness[i] = k;
        }
        for(unsigned j=0; j<NumUsedLiterals; ++j)
            LiteralIndices[ LiteralsInOrderOfCommonness[j] ] = j;
    }

    void AnalyzeSource(
        const std::vector<unsigned char>& source,
        const EncodingParams& params)
    {
        const size_t endpos  = source.size();
        MaxMatchLen.resize(endpos);
        DefaultCost.resize(endpos);
        Decisions.resize(endpos);
        bomb.resize(endpos);

        unsigned LiteralUsageTable[256] = { 0 }; // literal -> num times encoded
        for(size_t a=0; a<source.size(); ++a)
            LiteralUsageTable[ source[a] ] += 1;
        CreateAuxLiteralTables(LiteralUsageTable);

        MatchCache.resize(endpos * endpos);

        // Find out all prospects
        for(size_t a=0; a<endpos; ++a)
        {
            unsigned default_cost = params.useliteraltable
                ? 1 + GammaScore( LiteralIndices[ source[a] ] + 2 )
                : 9;
            unsigned better_cost = 9999;

            if(source[a] == params.constbyte)
                better_cost = 3 + params.literalbits;
            else if(source[a] == params.constbyte2)
                better_cost = 3 + params.literalbits;
            else if((params.offsconstmask & 1) && source[a] == ((source[a-1]+1)&0xFF))
                better_cost = 3 + params.literalbits;
            else if((params.offsconstmask & 2) && source[a] == ((source[a-1]-1)&0xFF))
                better_cost = 3 + params.literalbits;
            else if((params.offsconstmask & 4) && source[a] == ((source[a-1]+2)&0xFF))
                better_cost = 3 + params.literalbits;
            else
                for(size_t offset = 1; a >= offset && offset <= params.MaxByteCopyOffset(); ++offset)
                    if(source[a] == source[a-offset])
                        { better_cost = 3 + params.literalbits; break; }
            DefaultCost[a] = std::min(default_cost, better_cost);

            size_t limit = endpos - a;
            size_t longest = 1, where = 0;
            for(size_t test = a-std::min(a,MaxDistance); test<a; ++test)
            {
                size_t matchlen = TestMatch(source, test,a, limit);
                if(matchlen > longest)
                {
                    //if(matchlen >= 0x100) matchlen = 0xFF;
                    longest = matchlen;
                    where   = test;
                }
            }
            MaxMatchLen[a].first  = longest;
            MaxMatchLen[a].second = where;
        }
        if(!params.useliteraltable)
        {
            static const char* const MagicWords[] =
            {
                "bisqwit"/*, "Joel Yliluoma"*/, "Joel", "Yliluoma"
                //"0123456789ABCDEF"
            };
            for(unsigned tokenno=0; tokenno < sizeof(MagicWords)/sizeof(*MagicWords); ++tokenno)
            {
                const char* const token = MagicWords[tokenno];
                const size_t token_size = strlen(token);

                size_t begin = 0;
                while(begin + token_size <= source.size())
                {
                    void* p = memmem( &source[begin], source.size()-begin,
                                      token, token_size);
                    if(!p) break;
                    size_t found_pos = (const unsigned char*)p - &source[0];
                    for(unsigned a=0; a<token_size; ++a)
                    {
                        MaxMatchLen[found_pos + a].first = 1;
                        bomb[found_pos + a] = true;
                        DefaultCost[found_pos + a] = 9;
                    }
                    begin = found_pos + token_size;
                }
            }

            // Update match-cache for bombs
            for(size_t a=0; a<endpos; ++a)
            {
                size_t limit = endpos-a;
                for(size_t test=0; test<a; ++test)
                {
                    size_t len = TestMatch(source, a,test, limit);
                    for(size_t p=0; p<len; ++p)
                    {
                        if(bomb[test+p])
                        {
                            MatchCache[a * endpos + test] = p;
                            break;
                        }
                    }
                }
            }
        }
    }

    unsigned DetermineDecision(const std::vector<unsigned char>& source,
                               size_t a,
                               size_t test_last/* = ~size_t(0)*/,
                               unsigned recursion_limit,unsigned max_recursion,
                               const EncodingParams& params)
    {
        const size_t endpos  = source.size();

        unsigned total_cost = 0;

        // Determine decision
        size_t make_fast_when = std::min((a+endpos) / 2, a + 512);
        unsigned n_actions = 0;
        while(a < endpos)
        {
            if(recursion_limit == 2 && max_recursion==3 && (a >= make_fast_when || ++n_actions >= 2))
                recursion_limit -= 1;
            if(recursion_limit == 1 && max_recursion>1 && (a >= make_fast_when || ++n_actions >= 4))
                recursion_limit -= 1;

            unsigned best_cost   = 0;
            unsigned my_cost     = 0;
            size_t   best_eat    = MaxMatchLen[a].first;
            size_t   best_source = MaxMatchLen[a].second;

            if(!recursion_limit)
            {
                // Simply take predefine maximum (be greedy)
                if(best_eat > 1 && best_eat < params.shortmatchmin && a-best_source <= params.short_threshold)
                    best_eat = 1;
                if(best_eat == 2 && params.limit_at_1byte &&  a-best_source >= params.limit_at_1byte)
                    best_eat = 1;
                if(best_eat == 3 && params.limit_at_2byte &&  a-best_source >= params.limit_at_2byte)
                    best_eat = 1;
                my_cost = Cost(a - best_source, best_eat, test_last, DefaultCost[a], params);
            }
            else
            {
              #ifdef qq_OPENMP
                omp_lock_t lock;
                omp_init_lock(&lock);
               #pragma omp parallel for \
                    schedule(guided) \
                    default(none) \
                    shared(best_cost,best_eat,best_source,my_cost,source,lock,DoVerySlow,params) \
                    firstprivate(a,recursion_limit,max_recursion,test_last)
              #endif
                for(size_t eating_count = best_eat; eating_count >= 1; --eating_count)
                {
                    if(eating_count < 2)
                    {
                        size_t cheapest_source = a;
                        unsigned cheapest_cost = DefaultCost[a];

                        unsigned trailing_cost = cheapest_cost + DetermineDecision(
                            source,
                            a + eating_count,
                            eating_count >= 2 && params.appends ? a-cheapest_source : test_last,
                            recursion_limit - 1, max_recursion, params);

                      #ifdef qq_OPENMP
                        omp_set_lock(&lock);
                       #pragma omp flush(best_cost,best_eat,best_source,my_cost)
                      #endif
                        if(!best_cost || trailing_cost < best_cost)
                        {
                            best_cost   = trailing_cost;
                            best_eat    = eating_count;
                            best_source = cheapest_source;
                            my_cost     = cheapest_cost;
                        }
                      #ifdef qq_OPENMP
                       #pragma omp flush(best_cost,best_eat,best_source,my_cost)
                        omp_unset_lock(&lock);
                      #endif
                    }
                    else if(DoVerySlow
                       && recursion_limit <= 3
                       && eating_count >= 1u << (1u<<recursion_limit)/*
                       && eating_count >= best_eat / 2*/)
                    {
                        for(size_t test=a - std::min(a,MaxDistance); test<a; ++test)
                        {
                            if(eating_count < params.shortmatchmin && a-test <= params.short_threshold)
                                continue;
                            if(TestMatch(source, test,a, eating_count) >= eating_count)
                            {
                                /*for(size_t p=0; p<eating_count; ++p)
                                    if(bomb[test+p]) goto discard_match;*/

                                size_t cheapest_source = test;
                                unsigned cheapest_cost = Cost(a-test, eating_count, test_last, DefaultCost[a], params);

                                unsigned trailing_cost = cheapest_cost + DetermineDecision(
                                    source,
                                    a + eating_count,
                                    eating_count >= 2 && params.appends ? a-cheapest_source : test_last,
                                    recursion_limit - 1, max_recursion, params);

                              #ifdef qq_OPENMP
                                omp_set_lock(&lock);
                               #pragma omp flush(best_cost,best_eat,best_source,my_cost)
                              #endif
                                if(!best_cost || trailing_cost < best_cost)
                                {
                                    best_cost   = trailing_cost;
                                    best_eat    = eating_count;
                                    best_source = cheapest_source;
                                    my_cost     = cheapest_cost;
                                }
                              #ifdef qq_OPENMP
                               #pragma omp flush(best_cost,best_eat,best_source,my_cost)
                                omp_unset_lock(&lock);
                              #endif
                            }
                        }
                    }
                    else
                    {
                        size_t cheapest_source = 0;
                        unsigned cheapest_cost = 0;
                        for(size_t test=a - std::min(a,MaxDistance); test<a; ++test)
                        {
                            if(eating_count < params.shortmatchmin && a-test <= params.short_threshold)
                                continue;
                            if(TestMatch(source, test,a, eating_count) >= eating_count)
                            {
                                /*for(size_t p=0; p<eating_count; ++p)
                                    if(bomb[test+p]) goto discard_match;*/

                                unsigned cost = Cost(a-test, eating_count, test_last, DefaultCost[a], params);
                                if(!cheapest_cost || cost < cheapest_cost)
                                {
                                    cheapest_cost   = cost;
                                    cheapest_source = test;
                                }
                            }
                        }

                        if(!cheapest_cost) continue;

                        unsigned trailing_cost = cheapest_cost + DetermineDecision(
                            source,
                            a + eating_count,
                            eating_count >= 2 && params.appends ? a-cheapest_source : test_last,
                            recursion_limit - 1, max_recursion, params);

                      #ifdef qq_OPENMP
                        omp_set_lock(&lock);
                       #pragma omp flush(best_cost,best_eat,best_source,my_cost)
                      #endif
                        if(!best_cost || trailing_cost < best_cost)
                        {
                            best_cost   = trailing_cost;
                            best_eat    = eating_count;
                            best_source = cheapest_source;
                            my_cost     = cheapest_cost;
                        }
                      #ifdef qq_OPENMP
                       #pragma omp flush(best_cost,best_eat,best_source,my_cost)
                        omp_unset_lock(&lock);
                      #endif
                    }
                }
              #ifdef qq_OPENMP
                omp_destroy_lock(&lock);
              #endif
            }

            if(best_eat > 1 && best_eat < params.shortmatchmin && a-best_source <= params.short_threshold)
            {
                best_eat = 1;
                my_cost = Cost(a - best_source, best_eat, test_last, DefaultCost[a], params);
            }
            if(best_eat == 2 && params.limit_at_1byte && a-best_source >= params.limit_at_1byte)
            {
                best_eat = 1;
                my_cost = Cost(a - best_source, best_eat, test_last, DefaultCost[a], params);
            }
            if(best_eat == 3 && params.limit_at_2byte && a-best_source >= params.limit_at_2byte)
            {
                best_eat = 1;
                my_cost = Cost(a - best_source, best_eat, test_last, DefaultCost[a], params);
            }

            Decisions[a].first  = best_eat;
            Decisions[a].second = best_source;
            if(best_eat >= 2 && params.appends) test_last = a - best_source;
            a          += best_eat;
            total_cost += my_cost;
        }

        if(params.useliteraltable)
        {
            // Create literal table
            bool used[256] = { false };
            unsigned NumLit = 0;
            for(size_t p=0; p<endpos; ++p)
            {
                const size_t copy_length = Decisions[p].first;
                if(copy_length >= 2) { p += copy_length-1; continue; }
                if(!used[source[p]]) { used[source[p]] = true; ++NumLit; }
            }
            total_cost += NumLit * 8;
        }
        return total_cost;
    }
public:
    void Compress(
        const std::vector<unsigned char>& source,
        unsigned max_recursion,
        const EncodingParams& params)
    {
        const size_t endpos  = source.size();

        AnalyzeSource(source, params);

        DetermineDecision(source,
            params.useliteraltable ? 0 : 1,
            ~size_t(0), max_recursion, max_recursion, params);

        // Encode the decision
        result.clear();
        BitsRemaining = 0;
        BitsPosition  = 0;
        size_t last_offset = 0;

        size_t readpos = 0;
        if(params.useliteraltable)
        {
            // Create literal table
            unsigned LiteralUsageTable[256] = { 0 }; // literal -> num times encoded
            for(size_t p=0; p<endpos; ++p)
            {
                const size_t copy_length = Decisions[p].first;
                if(copy_length >= 2) { p += copy_length-1; continue; }
                LiteralUsageTable[ source[p] ] += 1;
            }
            CreateAuxLiteralTables(LiteralUsageTable);
            // Progressively reduce the literal table, dropping elements
            // that are never referenced. At each round, more elements
            // may become available because the indices get smaller.
            for(;;)
            {
                bool used[256] = { false };

                for(size_t p=0; p<endpos; ++p)
                {
                    const size_t copy_length = Decisions[p].first;
                    if(copy_length >= 2) { p += copy_length-1; continue; }
                    unsigned char byte = source[p];
                    unsigned default_cost = 1 + GammaScore( LiteralIndices[ byte ] + 2);
                    if(3 + params.literalbits < default_cost && !bomb[p])
                    {
                        bool maybe_bytecopy = false;
                        if(byte == params.constbyte
                        || byte == params.constbyte2
                        || ((params.offsconstmask & 1) && byte == ((source[p-1]+1)&0xFF))
                        || ((params.offsconstmask & 2) && byte == ((source[p-1]-1)&0xFF))
                        || ((params.offsconstmask & 4) && byte == ((source[p-1]+2)&0xFF))
                          )
                            maybe_bytecopy = true;
                        else
                        {
                            for(size_t o=1; p>=o && o<=params.MaxByteCopyOffset(); ++o)
                                if(byte == source[p-o])
                                    { maybe_bytecopy = true; break; }
                        }
                        if(maybe_bytecopy) continue;
                    }
                    used[byte] = true;
                }

                unsigned n = NumUsedLiterals;
                for(unsigned a=0; a<256; ++a)
                    if(!used[a])
                        LiteralUsageTable[ a] = 0;
                CreateAuxLiteralTables(LiteralUsageTable);
                if(n == NumUsedLiterals) break;
            }
        }
        else
        {
            result.push_back( source[readpos++] );
            NumUsedLiterals = 0;
        }

        while(readpos < endpos)
        {
            const size_t copy_length = Decisions[readpos].first;
            const size_t copy_begin  = Decisions[readpos].second;

            if(copy_length < 2)
            {
                unsigned char byte = source[readpos];
                unsigned default_cost = params.useliteraltable
                    ? 1 + GammaScore( LiteralIndices[ byte ] + 2)
                    : 9;

                /*fprintf(stderr, "[%u]: literal %02X\n", (unsigned)readpos, byte);*/
                if(3+params.literalbits > default_cost
                || bomb[readpos])
                    goto LiteralInAnycase;

                if(byte == params.constbyte)
                    SendByteCopy(params.ConstByte1Index(), params);
                else if(byte == params.constbyte2)
                    SendByteCopy(params.ConstByte2Index(), params);
                else if((params.offsconstmask & 1) && byte == ((source[readpos-1]+1)&0xFF))
                    SendByteCopy(params.OffsConstPlusIndex(), params);
                else if((params.offsconstmask & 2) && byte == ((source[readpos-1]-1)&0xFF))
                    SendByteCopy(params.OffsConstMinusIndex(), params);
                else if((params.offsconstmask & 4) && byte == ((source[readpos-1]+2)&0xFF))
                    SendByteCopy(params.OffsConstPlusTwoIndex(), params);
                else
                {
                    for(size_t offset = 1, offslim = params.MaxByteCopyOffset();
                        readpos >= offset && offset <= offslim;
                        ++offset)
                    {
                        if(byte == source[readpos-offset])
                            { SendByteCopy( params.ByteCopyOffset(offset), params);
                              goto didbytecopy; } // Range: 1..15
                    }
                LiteralInAnycase:
                    if(params.useliteraltable)
                        SendGammaEncodedLiteral( LiteralIndices[byte] + 2);
                    else
                        SendLiteral(byte);
                didbytecopy:;
                }
                ++readpos;
                continue;
            }

            size_t my_offset = readpos - copy_begin;

            // Append is *always* better than LongMatch. (6 vs 11 bits)
            // ShortMatch is *always* better than LongMatch. (11 vs 12+ bits)
            if(my_offset == last_offset)
            {
                // Length: 2..N
                SendAppendMatch( copy_length, params ); // Range: 1..inf
            }
            else if(copy_length >= params.shortmatchmin
                 && copy_length <= params.ShortLengthMax()
                 && my_offset   >= 1 && my_offset <= params.short_threshold)
            {
                // Range: 1..127, 2..3
                SendShortMatch( my_offset, copy_length, params );
            }
            else
            {
                // Length: M..N
                // where M is 1 for offset=80..4FF
                //            2 for offset=500..7CFF
                //            3 for others
                assert(my_offset   >= 1);
                if((my_offset <= params.short_threshold
                 && copy_length < 2 + params.ShortLongDelta())
                || copy_length >= 0x100)
                {
                    fprintf(stderr, "offset=%u, copy_length=%u, short=%u..%u, long=%u..\n",
                        (unsigned) my_offset,
                        (unsigned) copy_length,
                        (unsigned) params.shortmatchmin,
                        (unsigned) params.ShortLengthMax(),
                        (unsigned) (2 + params.ShortLongDelta() ) );
                }
                if(my_offset <= params.short_threshold)
                     assert(copy_length >= 2 + params.ShortLongDelta());
                else assert(copy_length >= 2);

                assert(copy_length < 0x100);

                SendLongMatch( my_offset, copy_length, params );
            }
            if(params.appends) last_offset = my_offset;
            readpos += copy_length;
        }
        SendEnd(params);
    }
    void Uncompress(
        const std::vector<unsigned char>& source,
        const EncodingParams& params,
        const unsigned char* LiteralTable)
    {
        unsigned tagpos=0, tag=0;
        size_t inpos = 0;

        #define GetBit() \
           ( ( (tagpos%8 ? (tag                  ) << (tagpos++ % 8) \
                         : (tag = source[inpos++]) << (tagpos++ % 8) \
               ) & 0x80) >> 7)

        result.clear();
        size_t last_offs = 0;
        if(!params.useliteraltable)
        {
        Literal:
            result.push_back(source[inpos++]);
        }
        for(;;)
        {
            if(inpos >= source.size()) return; // data error
            if(! GetBit() )
            {
                if(!params.useliteraltable)
                {
                    goto Literal; // 0, literal
                }
                // 0, gamma-encoded literal
                unsigned bh = 1; do bh = bh*2 + GetBit(); while( GetBit() );
                unsigned char byte = LiteralTable[bh - 2];
                result.push_back(byte);
                continue;
            }
            if(! GetBit() ) // 10, codepair
            {
                unsigned bh = 1; do bh = bh*2 + GetBit(); while( GetBit() );
                if(bh > 0xFF) break; // end
                unsigned cx = 1; do cx = cx*2 + GetBit(); while( GetBit() );
                unsigned o = params.appends ? 3 : 2;
                if(bh >= o) // offshi=2, append; otherwise, offslo
                {
                    unsigned bx = ((bh - o) << 8) | source[inpos++];
                    if(bx <= params.short_threshold)
                        cx += params.ShortLongDelta();
                    else if(params.limit_at_2byte && bx >= params.limit_at_2byte)
                        cx += 2;
                    else if(params.limit_at_1byte && bx >= params.limit_at_1byte)
                        cx += 1;
                    last_offs = bx;
                }
                do result.push_back( result[result.size() - last_offs] );
                while(--cx > 0);
            }
            else if(! GetBit() ) // 110, bytecopy
            {
                unsigned bx = 0;
                for(unsigned bit = params.literalbits; bit-- > 0; )
                    bx |= GetBit() << bit;
                if(params.offsconstmask == 5 && params.constbyte < 2 && params.constbyte2 == 0x100)
                {
                    if(!bx) { result.push_back(params.constbyte); continue; }
                    --bx;
                    if(!bx) { result.push_back( (result.back()+1) & 0xFF ); continue; }
                    --bx;
                    if(!bx) { result.push_back( (result.back()+2) & 0xFF ); continue; }
                    --bx;
                }
                else
                {
                    if(params.offsconstmask & 1)
                    {
                        if(!bx) { result.push_back( (result.back()+1) & 0xFF ); continue; }
                        --bx;
                    }
                    if(params.offsconstmask & 2)
                    {
                        if(!bx) { result.push_back( (result.back()-1) & 0xFF ); continue; }
                        --bx;
                    }
                    if(params.offsconstmask & 4)
                    {
                        if(!bx) { result.push_back( (result.back()+2) & 0xFF ); continue; }
                        --bx;
                    }
                    if(params.constbyte != 0x100)
                    {
                        if(!bx) { result.push_back(params.constbyte); continue; }
                        --bx;
                    }
                    if(params.constbyte2 != 0x100)
                    {
                        if(!bx) { result.push_back(params.constbyte2); continue; }
                        --bx;
                    }
                }
                ++bx;
                result.push_back( result[result.size() - bx] );
            }
            else // 111, shortmatch
            {
                last_offs = source[inpos++];
                unsigned cx = last_offs % params.ShortLengthMod();
                last_offs /= params.ShortLengthMod();
                if(!last_offs) break; // end
                if(params.ShortLengthMod() == 2) cx ^= 1;
                cx += params.shortmatchmin;
                do result.push_back( result[result.size() - last_offs] );
                while(--cx > 0);
           }
        }
        unsigned n_calls = params.n_calls;
        for(size_t a=0; a+3<=result.size() && n_calls > 0; ++a)
        {
            if(((params.call_mask & 1) && result[a] == 0xE8)
            || ((params.call_mask & 2) && result[a] == 0xE9))
            {
                ++a;
                unsigned offset = result[a] | (result[a+1] << 8);
                offset -= (a+ORG);
                result[a]   = offset & 0xFF;
                result[a+1] = offset >> 8;
                --n_calls;
                ++a;
            }
            else
            if((params.call_mask & 4) && result[a] == 0x0F)
            {
                a += 2;
                unsigned offset = result[a] | (result[a+1] << 8);
                offset -= (a+ORG);
                result[a]   = offset & 0xFF;
                result[a+1] = offset >> 8;
                --n_calls;
                ++a;
            }
        }
        if(n_calls)
            fprintf(stderr, "%u calls were not found\n", n_calls);
    }
private:
    // Primitives:
    void PrimSendByte(unsigned char value)
    {
        result.push_back(value);
    }
    void PrimSendBit(unsigned char bit)
    {
        if(!BitsRemaining)
            { BitsPosition = result.size();
              PrimSendByte(0x00);
              BitsRemaining = 8; }
        result[BitsPosition] |= bit << --BitsRemaining;
        /*fprintf(stderr, "sending bit: %02X -- tag@%u is now %02X\n", bit, BitsPosition,result[BitsPosition]);*/
    }
    void PrimSendGamma(unsigned value) // The smallest value it can encode is 2.
    {
        unsigned origvalue = value;
        assert(value >= 2);
        unsigned n_bits_sent = 0;
        unsigned highest_bit = 0x80000000u;
        while(highest_bit != 0
           && !(value & highest_bit))
            highest_bit >>= 1;
        if(value & highest_bit)
            highest_bit >>= 1;
        for(; ; PrimSendBit(1))
        {
            n_bits_sent += 2;
            PrimSendBit( !! ( value & highest_bit) );
            highest_bit >>= 1;
            if(!highest_bit) break;
        }
        PrimSendBit(0);

        if(GammaSummary.size() <= n_bits_sent) GammaSummary.resize(n_bits_sent+1);
        GammaSummary[n_bits_sent] += 1;
        if(GammaSummary2.size() <= origvalue) GammaSummary2.resize(origvalue+1);
        GammaSummary2[origvalue] += 1;
    }
public:
    // Actual encodes
    void SendLongMatch(
        unsigned offset,
            // Offset: 0..n
        unsigned len,
            // Len, if offset:
            //    0..7F:    3..any (1 + 2)
            //   80..FFFF:  1..any (1 + 0)
        const EncodingParams& params
    )
    {
        /*fprintf(stderr, "Sending long match: %u %u\n", offset, len);*/
        PrimSendBit(1); PrimSendBit(0);
        unsigned tmp = offset + (params.appends ? 0x300 : 0x200);
        PrimSendGamma(tmp >> 8);
        if(offset <= params.short_threshold)
            len -= params.ShortLongDelta();
        else if(params.limit_at_2byte && offset >= params.limit_at_2byte)
            len -= 2;
        else if(params.limit_at_1byte && offset >= params.limit_at_1byte)
            len -= 1;
        PrimSendGamma(len);
        PrimSendByte(tmp & 0xFF);
    }
    void SendAppendMatch(unsigned len, const EncodingParams& params)
    {
        assert(params.appends);
        /*fprintf(stderr, "Sending APPEND: %u\n", len);*/
        PrimSendBit(1);  PrimSendBit(0);
        PrimSendGamma(2);
        PrimSendGamma(len);
    }
    void SendShortMatch(unsigned offset/*1..127*/, unsigned len/*2..3*/, const EncodingParams& params)
    {
        // 11 bits. For len=2, gets 5.5 bits per byte; for len=3, 3.67 bits per byte.
        /*fprintf(stderr, "Sending short match: %u %u\n", offset, len);*/
        PrimSendBit(1); PrimSendBit(1); PrimSendBit(1);
        assert(offset <= params.short_threshold);
        assert(offset > 0x00);
        assert(len >= params.shortmatchmin);
        assert(len <= params.ShortLengthMax());
        unsigned lenval = (len - params.shortmatchmin);
        if(params.ShortLengthMod() == 2) lenval ^= 1;
        PrimSendByte( lenval + offset * params.ShortLengthMod() );
    }
    void SendByteCopy(unsigned offset/*1..15*/, const EncodingParams& params)
    {
        /*fprintf(stderr, "Sending byte copy: %u\n", offset);*/
        PrimSendBit(1); PrimSendBit(1); PrimSendBit(0);
        const unsigned bits = params.literalbits;

        if(offset >= (1u << bits))
            fprintf(stderr, "WRONG OFFSET: %u, LIMIT %u\n", offset, 1u << bits);
        assert(offset < (1u << bits));

        for(unsigned bit = bits; bit-- > 0; )
            PrimSendBit( (offset >> bit) & 1 );
    }
    void SendGammaEncodedLiteral(unsigned table_index)
    {
        PrimSendBit(0);
        PrimSendGamma(table_index);
    }
    void SendLiteral(unsigned char literal)
    {
        /*fprintf(stderr, "Sending literal: %02X\n", literal);*/
        PrimSendBit(0);
        PrimSendByte(literal);
    }
    void SendEnd(const EncodingParams& params)
    {
        //    PrimSendBit(1); PrimSendBit(0); PrimSendGamma(0x100);
        //    // ^ 16 bits
        PrimSendBit(1); PrimSendBit(1); PrimSendBit(1);
        PrimSendByte(0x00);
        // ^ 11 bits
    }
};

std::vector<unsigned char> GetAsm(const EncodingParams& params,
    unsigned NumLiterals=0, const unsigned char* LiteralTable=0,
    unsigned PayLoadLength=0)
{
    std::vector<unsigned char> result;

  #pragma omp critical(asmlock)
  {
    char tempfn1[64], tempfn2[64];
    std::sprintf(tempfn1, "apack-%d.tm1", getpid());
    std::sprintf(tempfn2, "apack-%d.tm2", getpid());

    FILE* fp = std::fopen(tempfn1, "wt");
    std::fprintf(fp,
"bits 16\n"
"org 0x110\n"
"TGT_ORG           equ 0x%X\n"
"calls             equ %d\n"
"constbyte         equ 0x%X\n"
"constbyte2        equ 0x%X\n"
"callmask          equ %d\n"
"appends           equ %d\n"
"literaloffsetbits equ 0x%X\n"
"offsconstmask     equ %u\n"
"useliteraltable   equ %u\n"
"literaltable_length equ %u\n"
"shortmatch_min    equ %u\n"
"short_threshold   equ %u\n"
"short_lengthmod   equ %u\n"
"short_lengthbits  equ %u ; 0 if not power of 2\n"
"limit_at_1byte    equ %u\n"
"limit_at_2byte    equ %u\n"
"PayLoadSize       equ %d\n"

"%%macro addsub 2 ; %%1 = register, %%2 = count\n"
"	%%if %%2==0\n"
"	%%elif %%2==1 || %%2==2 || %%2==3\n"
"		times %%2 inc %%1\n"
"	%%elif %%2==-1 || %%2==-2 || %%2==-3\n"
"		times -%%2 dec %%1\n"
"	%%else\n"
"		add %%1, %%2\n"
"	%%endif\n"
"%%endmacro\n"
"\n"
"	main:\n"
"       db 0xFC ; Second byte of a dummy \"add ah, bh\"\n"
"       pop bx  ; Undoes stuff done by NES header\n"
"       ;mov ah, 9\n"
"       ;mov dx, DATABEGIN+PayLoadSize\n"
"       ;int 21h\n"
"		mov dl, 0x80\n"
"		mov bp, GETBIT\n"
"		mov di, TGT_ORG\n"
"		mov si, DATABEGIN\n"
"		xor cx, cx\n"
"       cld\n"
"	    jmp short DECOMPRESS\n"
"       db $1A,'Incorrect MS-DOS version',13,10\n"
"DECOMPRESS:\n"
"		push di ; This is the return address to program's start\n"
"	%%if calls <> 0 && callmask <> 0\n"
"		push di ; This is the begin address for calls and jumps conversion\n"
"	%%endif\n"
"	%%if appends\n"
"		push di ; This should actually be zero, but it's funny to push di 3 times\n"
"	%%endif\n"
"	%%if useliteraltable=0\n"
"LITERAL:\n"
"		movsb\n"
"	%%endif\n"
"NEXTTAG:\n"
"	%%if useliteraltable=1\n"
"		inc cx\n"
"	%%endif\n"
"		call bp\n"
"		jnc LITERAL\n"
"	%%if useliteraltable=0\n"
"		inc cx\n"
"	%%endif\n"
"		call bp\n"
"		jnc CODEPAIR\n"
"		mov ax, 0x100 >> literaloffsetbits\n"
"		call bp\n"
"		jc SHORTMATCH\n"
"	.fourbits:\n"
"		 call bp\n"
"		 adc al,al\n"
"		jnc .fourbits\n"
"\n"
"%%if (offsconstmask == 5 && (constbyte == 0x100 || constbyte < 2) && constbyte2 == 0x100)\n"
"	%%if constbyte == 0\n"
"		jz STORE\n"
"		dec ax\n"
"	%%elif constbyte == 1\n"
"		jz .inc_one_store\n"
"		dec ax\n"
"	%%endif\n"
"		; Very special case for this particular setup.\n"
"		; 3 bytes shorter than the generic one below.\n"
"		cmp al,2     ;test al,0xFE\n"
"		jae .skip5   ;jnz .skip5\n"
"		add al,[di-1]\n"
"	.inc_one_store:\n"
"		inc ax ; +0 or +1 becomes +1 or +2\n"
"		jmp STORE\n"
"	.skip5:\n"
"		dec ax\n"
"		jmp DOMATCH_AX\n"
"%%else\n"
"	%%define get_prev mov al,[di-1] ; Surprisingly this is just 3 bytes.\n"
"	%%assign get_prev_skip 0x39 ;cmp <something>\n"
"	%%if ((offsconstmask & 1) + ((offsconstmask & 2) >> 1) + ((offsconstmask & 4) >> 2)) > 1\n"
"		lea bx,[di-1] ;However, 3+1+1 is better than 3+3, and 3+1+1+1 is way better than 3+3+3.\n"
"		%%undef get_prev\n"
"		%%define get_prev xlatb ;mov al,[bx+al], with al assumed to be 0\n"
"		%%define get_prev_is_onebyte 1\n"
"		%%assign get_prev_skip 0xA8 ;test al,...\n"
"	%%else\n"
"		%%define get_prev_is_onebyte 0\n"
"	%%endif\n"
"	%%if (offsconstmask & 1)\n"
"		%%if ((offsconstmask & (2+4)) || constbyte < 0x100 || constbyte2 < 0x100)\n"
"			jz .offsconst_plus\n"
"			dec ax\n"
"		%%else\n"
"			jnz DOMATCH_AX\n"
"			%%define did_matchjump\n"
"		%%endif\n"
"	%%endif\n"
"	%%if (offsconstmask & 2)\n"
"		%%if ((offsconstmask & (4)) || constbyte < 0x100 || constbyte2 < 0x100)\n"
"			jz .offsconst_minus\n"
"			dec ax\n"
"		%%else\n"
"			jnz DOMATCH_AX\n"
"			%%define did_matchjump\n"
"		%%endif\n"
"	%%endif\n"
"	%%if (offsconstmask & 4)\n"
"		%%if (constbyte < 0x100 || constbyte2 < 0x100)\n"
"			jz .offsconst_plustwo\n"
"			dec ax\n"
"		%%else\n"
"			jnz DOMATCH_AX\n"
"			%%define did_matchjump\n"
"		%%endif\n"
"	%%endif\n"
"	%%if(constbyte < 0x100)\n"
"		%%if(constbyte == 0x00)\n"
"			jz STORE\n"
"		%%elif(constbyte == 0x01 && (offsconstmask & 1))\n"
"			jz .inc_one_store\n"
"		%%elif(constbyte == 0xFF && (offsconstmask & 2))\n"
"			jz .dec_one_store\n"
"		%%elif(constbyte == 0x02 && (offsconstmask & 4))\n"
"			jz .inc_two_store\n"
"		%%else\n"
"			%%if(constbyte2 < 0x100)\n"
"				jz .constbyte\n"
"			%%else\n"
"				jnz DOMATCH_AX\n"
"				%%define did_matchjump\n"
"			%%endif\n"
"			%%define constbyte_needed\n"
"		%%endif\n"
"		%%if (constbyte2 < 0x100)\n"
"			dec ax\n"
"		%%endif\n"
"	%%endif\n"
"	%%if(constbyte2 < 0x100)\n"
"		%%if(constbyte2 == 0x00)\n"
"			jz STORE\n"
"		%%elif(constbyte2 == 0x01 && (offsconstmask & 1))\n"
"			jz .inc_one_store\n"
"		%%elif(constbyte2 == 0xFF && (offsconstmask & 2))\n"
"			jz .dec_one_store\n"
"		%%elif(constbyte2 == 0x02 && (offsconstmask & 4))\n"
"			jz .inc_two_store\n"
"		%%else\n"
"			;jz .constbyte2\n"
"			jnz DOMATCH_AX\n"
"			%%define did_matchjump\n"
"			%%define constbyte2_needed\n"
"		%%endif\n"
"	%%endif\n"
"	%%if(offsconstmask == 0 && constbyte == 0x100 && constbyte2 == 0x100)\n"
"			inc ax ; No literal support, so increase offset range from 0..15 to 1..16\n"
"	%%endif\n"
"	%%ifndef did_matchjump\n"
"			jmp DOMATCH_AX\n"
"	%%endif\n"
"	%%ifdef constbyte2_needed\n"
"		.constbyte2:\n"
"		%%ifdef constbyte_needed\n"
"			; Because all means to construct constbyte2 need at least 3 bytes,\n"
"			; synthesize special code that only needs at most 2 bytes by partially\n"
"			; reusing the code for constbyte, which follows immediately hereafter.\n"
"			%%if constbyte2 == (constbyte-1)&0xFF\n"
"				dec ax\n"
"			%%elif constbyte2 == (constbyte+1)&0xFF\n"
"				inc ax\n"
"			%%else\n"
"				add al, constbyte2 - constbyte\n"
"			%%endif\n"
"		%%else\n"
"			; At this point, ax is known to be zero.\n"
"			%%if (offsconstmask & 4)\n"
"				addsub ax, constbyte2-2\n"
"				db get_prev_skip\n"
"			%%elif (offsconstmask & 2)\n"
"				addsub al, constbyte2+1\n"
"				db get_prev_skip\n"
"			%%elif (offsconstmask & 1)\n"
"				addsub ax, constbyte2-1\n"
"				db get_prev_skip\n"
"			%%elif constbyte2 == 1\n"
"				inc ax\n"
"				jmp STORE\n"
"			%%elif constbyte2 == 0xFF\n"
"				dec ax\n"
"				jmp STORE\n"
"			%%elif constbyte2 == 2 && (offsconstmask & 1)\n"
"				inc ax\n"
"				jmp short .inc_one_store ; This is 3 bytes in total, whereas mov al;jmp would be 4.\n"
"			%%elif constbyte2 == 3 && (offsconstmask & 4)\n"
"				inc ax\n"
"				jmp short .inc_two_store\n"
"			%%elif constbyte2 == 0xFE && (offsconstmask & 2)\n"
"				dec ax\n"
"				jmp short .dec_one_store ; Similar rationale as above.\n"
"			%%else\n"
"				add al, constbyte2 ; 2 bytes opcode\n"
"				jmp STORE\n"
"			%%endif\n"
"		%%endif\n"
"	%%endif\n"
"	%%ifdef constbyte_needed\n"
"	.constbyte:\n"
"		; At this point, ax is known to be zero.\n"
"		%%if (offsconstmask & 4)\n"
"			addsub ax, constbyte-2\n"
"			db get_prev_skip\n"
"		%%elif (offsconstmask & 2)\n"
"			addsub al, constbyte+1\n"
"			db get_prev_skip\n"
"		%%elif (offsconstmask & 1)\n"
"			addsub ax, constbyte-1\n"
"			db get_prev_skip\n"
"		%%elif constbyte == 1\n"
"			inc ax\n"
"			jmp STORE\n"
"		%%elif constbyte == 0xFF\n"
"			dec ax\n"
"			jmp STORE\n"
"		%%elif constbyte == 2 && (offsconstmask & 1)\n"
"			inc ax\n"
"			jmp short .inc_one_store ; This is 3 bytes in total, whereas mov al;jmp would be 4.\n"
"		%%elif constbyte == 3 && (offsconstmask & 4)\n"
"			inc ax\n"
"			jmp short .inc_two_store\n"
"		%%elif constbyte == 0xFE && (offsconstmask & 2)\n"
"			dec ax\n"
"			jmp short .dec_one_store ; Similar rationale as above.\n"
"		%%else\n"
"			add al, constbyte ; 2 bytes opcode\n"
"			; Note: It is important that this is an ADD and not a MOV,\n"
"			;       otherwise constbyte2 won't work.\n"
"			jmp STORE\n"
"		%%endif\n"
"	%%endif\n"
"	%%if offsconstmask & 4\n"
"	.offsconst_plustwo:\n"
"			get_prev\n"
"	.inc_two_store:\n"
"			inc ax\n"
"		%%if offsconstmask & 1\n"
"			db 0xA8 ; test al,\n"
"	.offsconst_plus:\n"
"			get_prev\n"
"		%%endif\n"
"	.inc_one_store:\n"
"			inc ax\n"
"			jmp STORE\n"
"	%%endif\n"
"	%%if offsconstmask & 2\n"
"	.offsconst_minus:\n"
"			get_prev\n"
"	.dec_one_store:\n"
"			dec ax\n"
"			jmp STORE\n"
"	%%endif\n"
"	%%if (offsconstmask & 1) && !(offsconstmask & 4)\n"
"	.offsconst_plus:\n"
"			get_prev\n"
"	.inc_one_store:\n"
"			inc ax\n"
"			jmp STORE\n"
"	%%endif\n"
"%%endif\n"
"CODEPAIR:\n"
"		mov ah,cl ; = 1\n"
"	.bhloop:\n"
"		call bp\n"
"		adc ah,ah\n"
"		; jc OVER\n"
"		call bp\n"
"		jc .bhloop\n"
"	.cxloop:\n"
"		call bp\n"
"		adc cx,cx\n"
"		call bp\n"
"		jc .cxloop\n"
"	%%if appends\n"
"		sub ah, 3\n"
"		jc DOMATCH_LASTPOS\n"
"	%%else\n"
"		sub ah, 2\n"
"	%%endif\n"
"SHORTMATCH:\n"
"		lodsb ; ah is zero if entered to SHORTMATCH\n"
"	%%if short_lengthmod == 1\n"
"		jc .offset_except_one\n"
"	%%elif short_lengthmod == 2\n"
"		jnc .waslong\n"
"		 shr al,1  ; 2+2+2 = 6 bytes in total\n"
"		 jz OVER\n"
"		 sbb cl,ch ; cx was 0001 at this point; now 0001(nc) or 0000(c).\n"
"	%%else\n"
"		jnc .waslong\n"
"		 aam short_lengthmod ; AL=remainder=length, AH=quotient=offset\n"
"		 mov cl, al ; below, use shr instead of movzx, to set flags\n"
"		 shr ax, 8 ; if al was zero, we could use AAD 1 here (2 bytes).\n"
        /* goal: ch<-0 (no change)
         *       cl<-al  (cl was 1)
         *       ah<-0
         *       al<-ah
         *       ZF<-ah.z
         * the above accomplishes this in 5 bytes.
         */
"		 jz OVER ; total: 2+2+3 = 7 bytes, plus jz (not counted)\n"
"		; mov cl, al\n"
"		; and cl, (1 << short_lengthbits) - 1 ; 3 bytes\n"
"		; shr al, short_lengthbits ; note: 3 bytes\n"
"		; jz OVER ; total: 2+3+3 = 8 bytes, plus jz (not counted)\n"
"		 jmp .offset\n"
"	%%endif\n"
"	.waslong:\n"
"	%%if limit_at_2byte\n"
"		cmp ah, limit_at_2byte >> 8\n"
"		jae .offset_just_one - 1\n"
"	%%endif\n"
"	%%if limit_at_1byte\n"
"		cmp ah, limit_at_1byte >> 8\n"
"		jae .offset_just_one\n"
"	%%endif\n"
"	%%if short_threshold == 0xFF\n"
"		test ah,ah\n"
"		jnz DOMATCH_NEWLASTPOS\n"
"	%%else\n"
"		cmp ax, short_threshold\n"
"		ja DOMATCH_NEWLASTPOS\n"
"	%%endif\n"
"	%%if (short_lengthmod-2) == -1\n"
"	 db 0xA8 ; test al, <byte> -- skip one inc cx TODO verify if this is correct\n"
"	%%else\n"
"	 addsub cx, short_lengthmod - 2\n"
"	%%endif\n"
".offset:\n"
"		 inc cx ; This is separately so short_lengthbits may skip one inc.\n"
".offset_except_one:\n"
"		 times shortmatch_min-2 inc cx\n"
".offset_just_one:\n"
"		 inc cx\n"
"DOMATCH_NEWLASTPOS:\n"
"	%%if appends\n"
"		pop bx ;i.e. discard old lastpos\n"
"		db 0xA8 ; test al, <byte>\n"
"DOMATCH_LASTPOS:\n"
"		pop ax ;restore old lastpos\n"
"		push ax ;save lastpos\n"
"	%%endif\n"
"DOMATCH_AX:\n"
"		neg ax\n"
"		xchg ax,bx\n"
"COPYLOOP:\n"
"		mov al,[bx+di]\n"
"STORE:\n"
"		stosb\n"
"		loop COPYLOOP\n"
"		jmp short NEXTTAG\n"
"	%%if useliteraltable=1\n"
"LITERAL:\n"
"		mov bx,cx\n"
"	.blloop:\n"
"		call bp\n"
"		adc bx,bx\n"
"		call bp\n"
"		jc .blloop\n"
"		mov al,[bx+LITERALTABLE-2]\n"
"		jmp STORE\n"
"	%%endif\n"
"GETBIT:\n"
"		add dl,dl\n"
"		jnz .getbitret\n"
"		mov dl,[si]\n"
"		inc si\n"
"		adc dl,dl\n"
"	.getbitret:\n"
"	%%if (calls <> 0 && callmask <> 0) || appends\n"
"		ret\n"
"	%%endif\n"
"OVER:\n"
"	%%if appends\n"
"		pop ax\n"
"	%%endif\n"
"	%%if calls <> 0 && callmask <> 0\n"
"			pop si\n"
"		 .findcalls:\n"
"		%%if calls >= 0x100\n"
"			mov cx, calls\n"
"		%%else\n"
"			mov cl, calls\n"
"		%%endif\n"
"		 .findcall:\n"
"			lodsb\n"
"		%%if callmask & 4\n"
"			cmp al, 0x0F\n"
"		  %%if callmask & 3\n"
"			jz .patch_longbranch\n"
"		  %%endif\n"
"		%%endif\n"
"		%%if callmask & 3\n"
"			%%if (callmask & 3) == 3\n"
"				xor al, 0xE8 ; E8->00, E9->01\n"
"				shr al, 1    ; 00->00, 01->00, other->nonzero\n"
"			%%elif (callmask & 3) == 2\n"
"				cmp al, 0xE9\n"
"			%%else\n"
"				cmp al, 0xE8\n"
"			%%endif\n"
"		%%endif\n"
"			jnz .findcall\n"
"		%%if callmask & 4\n"
"			%%if callmask & 3\n"
"			db 0xA8 ; test al, <byte>\n"
"		 .patch_longbranch:\n"
"			%%endif\n"
"			inc si\n"
"		%%endif\n"
"			sub [si],si\n"
"			lodsw\n"
"			loop .findcall\n"
"	%%endif ;calls <> 0 && callmask <> 0\n"
"		ret\n"
"LITERALTABLE:\n"
"		times literaltable_length db 0\n"
"DATABEGIN:\n",
        ORG,
        params.n_calls,
        params.constbyte,
        params.constbyte2,
        params.call_mask,
        params.appends ? 1u : 0u,
        params.literalbits,
        params.offsconstmask,
        params.useliteraltable, NumLiterals,
        params.shortmatchmin,
        params.short_threshold,
        params.ShortLengthMod(),
        params.ShortLengthBits(),
        params.limit_at_1byte,
        params.limit_at_2byte,
        PayLoadLength);
    std::fclose(fp);

    char command[512];
    std::sprintf(command, "nasm -w+all -O2 %1$s -o %2$s", tempfn1,tempfn2);
    system(command);

    fp = std::fopen(tempfn2, "rb");
    if(fp)
    {
        std::fseek(fp, 0, SEEK_END);
        result.resize( ftell(fp) );
        std::rewind(fp);
        std::fread(&result[0], 1, result.size(), fp);
        std::fclose(fp);
    }
    std::remove(tempfn1);
    std::remove(tempfn2);
  }
    if(NumLiterals)
        std::memcpy(&result[result.size() - NumLiterals], LiteralTable, NumLiterals);
    return result;
}

int main()
{
    std::vector<unsigned char> buffer_orig;
    for(;;)
    {
        size_t end = buffer_orig.size();
        buffer_orig.resize(end + 4096);
        size_t n = std::fread(&buffer_orig[end], 1, 4096, stdin);
        buffer_orig.resize(end + n);
        if(n == 0) break;
    }

    DoVerySlow = false;

    EncodingParams params = {0,0,0x00,0x00,4,true, 0, true, 2, 0x7F, 0,0};
    Apack          best_packer;
    size_t         best_asm_skip = 0;
    std::vector<unsigned char> best_buffer;

  #ifdef _OPENMP
    omp_set_nested(0);
  #endif

    //fprintf(stderr, "Translated %u calls\n", n_calls_translated);
 #pragma omp parallel for schedule(dynamic,1)
    for(unsigned progress = 0; progress < 8*2*8*1*4*8*1*1*2*3*1*3*3; ++progress)
    {
        unsigned tv = progress;
        EncodingParams test_params;
        test_params.call_mask   = tv%8;     tv/=8;
        test_params.appends     = tv%2;     tv/=2;
        test_params.limit_at_1byte = (tv%8)<<8;   tv/=8;
        test_params.limit_at_2byte = 0;//(tv%12)<<8;  tv/=12;
        test_params.literalbits = tv%4+2;   tv/=4;
        test_params.offsconstmask = tv%8; tv/=8;
        static const unsigned cb[3]={0,1,0x100};
        test_params.constbyte   = cb[tv%3]; tv/=3; //tv%0x101; tv/=0x101;
        test_params.constbyte2  = cb[tv%3]; tv/=3; //tv%0x101; tv/=0x101;
        test_params.useliteraltable = 1-tv%2;  tv/=2;
        test_params.shortmatchmin   = 2+tv%3; tv/=3;
        test_params.short_threshold = 127;//0x7F + 0x80*(tv%2); tv/=2;
        //test_params.short_threshold = 1 + tv%0xFF; tv/=0xFF;
        test_params.n_calls = 0;

        if(test_params.constbyte == 0x100
        && test_params.constbyte2 != 0x100)
            continue;
        if(test_params.constbyte != 0x100
        && test_params.constbyte2 < test_params.constbyte)
            continue;

        if(test_params.limit_at_2byte
        && (!test_params.limit_at_1byte
          || test_params.limit_at_1byte >= test_params.limit_at_2byte))
        {
            continue;
        }
        //if(!test_params.limit_at_2byte) continue;

        //if(!(test_params.call_mask&1)) continue;
        //if(test_params.call_mask&4) continue;
        if(test_params.call_mask != 1) continue;
        if(!test_params.appends) continue;
        if(test_params.limit_at_1byte != 0x500
        && test_params.limit_at_1byte != 0x600) continue;
        //if(test_params.literalbits == 5) continue;
        if(test_params.literalbits != 4
        && test_params.literalbits != 3) continue;
        if(test_params.useliteraltable) continue;
        //if(!(test_params.offsconstmask & 1)) continue;
        if(test_params.offsconstmask != 0/*
        && test_params.offsconstmask != 1*/) continue;
        //if(test_params.offsconstmask != 5) continue;
        if(test_params.shortmatchmin != 2) continue;
        /*if(test_params.short_threshold > 0x13-1
        && test_params.short_threshold != 0x15-1
        && test_params.short_threshold != 0x17-1
        && test_params.short_threshold != 0x19-1
        && test_params.short_threshold != 0x1C-1
        && test_params.short_threshold != 0x20-1
        && test_params.short_threshold != 0x24-1
        && test_params.short_threshold != 0x2A-1
        && test_params.short_threshold != 0x33-1
        && test_params.short_threshold != 0x40-1
        && test_params.short_threshold != 0x55-1
        && test_params.short_threshold != 0x80-1
        && test_params.short_threshold != 0xA0-1
        && test_params.short_threshold != 0xC0-1
        && test_params.short_threshold != 0xE0-1
        && test_params.short_threshold != 0x100-1) continue;*/

        if(!!(test_params.offsconstmask & 1)
         + !!(test_params.offsconstmask & 2)
         + !!(test_params.offsconstmask & 4)
         + (test_params.constbyte  != 0x100)
         + (test_params.constbyte2 != 0x100)
         > (1u << test_params.literalbits))
        {
            // Ignore combination that simply cannot be encoded
            continue;
        }
    #if 1
        bool byte1ok = test_params.constbyte == 0x00
                    || test_params.constbyte == 0x01
                    || /*test_params.constbyte == 0x02
                    || test_params.constbyte == 0x03
                    || test_params.constbyte == 0x0E
                    || test_params.constbyte == 0x0F
                    || test_params.constbyte == 0x10
                    || test_params.constbyte == 0xF6
                    || test_params.constbyte == 0xFE
                    || test_params.constbyte == 0xFF
                    ||*/ test_params.constbyte == 0x100;
        bool byte2ok = test_params.constbyte2 == 0x00
                    || test_params.constbyte2 == 0x01
                    || /*test_params.constbyte2 == 0x02
                    || test_params.constbyte2 == 0x03
                    || test_params.constbyte2 == 0x0E
                    || test_params.constbyte2 == 0x0F
                    || test_params.constbyte2 == 0x10
                    || test_params.constbyte2 == 0xF6
                    || test_params.constbyte2 == 0xFE
                    || test_params.constbyte2 == 0xFF
                    ||*/ test_params.constbyte2 == 0x100;
        if(!byte1ok || !byte2ok) continue;
    #endif
        /*
        if(test_params.constbyte >= 0x10
        && test_params.constbyte != 0xFF
        && test_params.constbyte != 0x100) continue; // artificial speedup...
        */

                                ////////////
        std::vector<unsigned char> buffer(buffer_orig);
        for(size_t a=0; a+3<=buffer.size(); ++a)
        {
            if(((test_params.call_mask & 1) && buffer[a] == 0xE8)
            || ((test_params.call_mask & 2) && buffer[a] == 0xE9))
            {
                ++a;
                unsigned offset = buffer[a] | (buffer[a+1] << 8);
                offset += (a+ORG);
                buffer[a]   = offset & 0xFF;
                buffer[a+1] = offset >> 8;
                ++a;
                ++test_params.n_calls;
            }
            else
            if(((test_params.call_mask & 4) && buffer[a] == 0x0F))
            {
                a += 2;
                unsigned offset = buffer[a] | (buffer[a+1] << 8);
                offset += (a+ORG);
                buffer[a]   = offset & 0xFF;
                buffer[a+1] = offset >> 8;
                ++a;
                ++test_params.n_calls;
            }
        }

        std::fprintf(stderr, "Trying constbyte=%02X:%02X,literal=%d,callmask=%u(%u calls),appends=%u,limit1=%X;2=%X,offsconst=%u,table=%u(%u),shortmin=%u/%u ...\n",
            test_params.constbyte, test_params.constbyte2,
            test_params.literalbits,
            test_params.call_mask,
            test_params.n_calls,
            test_params.appends,
            test_params.limit_at_1byte,
            test_params.limit_at_2byte,
            test_params.offsconstmask,
            test_params.useliteraltable, 0,
            test_params.shortmatchmin, test_params.short_threshold);
        std::fflush(stderr);

        Apack packer;
        packer.Compress(buffer, 2, test_params);
        /*
        If you need better compression, put 2 or 3 above. 1 is default.
        But beware, it will make the program a LOT slower.
        */

        std::vector<unsigned char> stub = GetAsm(test_params,
            packer.NumUsedLiterals, packer.LiteralsInOrderOfCommonness,
            packer.result.size());

        packer.result.insert(packer.result.begin(), stub.begin(), stub.end());
        static const unsigned char signa[] = "";//Loading...\r$";
        packer.result.insert(packer.result.end(), signa, signa + sizeof(signa)-1);

      #pragma omp critical
      {
        #pragma omp flush(params,best_packer,best_asm_skip,best_buffer)
        unsigned res_size  = best_packer.result.size();
        unsigned test_size = packer.result.size();

        if(!res_size || test_size <= res_size)
        {
            std::fprintf(stderr, "%u+%u=%u bytes for constbyte=%02X:%02X,literal=%d,callmask=%u(%u calls),appends=%u,limit1=%X;2=%X,offsconst=%u,table=%u(%u),shortmin=%u/%u\n",
                (unsigned) (test_size - stub.size()),
                (unsigned) stub.size(),
                (unsigned) test_size,
                test_params.constbyte, test_params.constbyte2,
                test_params.literalbits,
                test_params.call_mask,
                test_params.n_calls,
                test_params.appends,
                test_params.limit_at_1byte,
                test_params.limit_at_2byte,
                test_params.offsconstmask,
                test_params.useliteraltable, packer.NumUsedLiterals,
                test_params.shortmatchmin, test_params.short_threshold);
            std::fflush(stderr);

            best_packer = packer;
            params      = test_params;
            best_asm_skip = stub.size();
            best_buffer.swap(buffer);

            std::fwrite(&best_packer.result[0], 1, best_packer.result.size(), stdout);
            ftruncate(1, best_packer.result.size());
            std::rewind(stdout);
            std::fflush(stdout);

            ///////////
            std::vector<unsigned char> dataonly(best_packer.result);
            dataonly.erase(dataonly.begin(), dataonly.begin() + best_asm_skip);
            Apack unpacker;
            unpacker.Uncompress(dataonly, params, best_packer.LiteralsInOrderOfCommonness);
            const std::vector<unsigned char>& decompress( unpacker.result);
            if(decompress != buffer_orig)
            {
                size_t ndiff = 0;
                for(size_t a=0; a<buffer_orig.size() && a!=decompress.size(); ++a)
                    if(buffer_orig[a] != decompress[a])
                        ++ndiff;

                fprintf(stderr, "--DECOMPRESSION FAILURE (%u<->%u bytes, %u bytes differ)\n",
                    (unsigned) buffer_orig.size(), (unsigned) decompress.size(), (unsigned) ndiff);
            }
        }
        #pragma omp flush(params,best_packer,best_asm_skip,best_buffer)
      }
    }

    if(buffer_orig.size() < 2000)
    {
        fprintf(stderr, "Enabling DoVerySlow mode... (single-thread only, sorry)\n");
        DoVerySlow = true;
        best_packer = Apack();
        best_packer.Compress(best_buffer, 3, params);
        std::vector<unsigned char> stub = GetAsm(params,
            best_packer.NumUsedLiterals, best_packer.LiteralsInOrderOfCommonness);
        best_packer.result.insert(best_packer.result.begin(), stub.begin(), stub.end());

        size_t best_size = best_packer.result.size();
        fprintf(stderr, "%u+%u=%u bytes after enabling DoVerySlow mode\n",
            (unsigned) (best_size - stub.size()),
            (unsigned) stub.size(),
            (unsigned) best_size);
    }

    std::fwrite(&best_packer.result[0], 1, best_packer.result.size(), stdout);

#if 1
    Apack unpacker;
    std::vector<unsigned char> dataonly(best_packer.result);
    dataonly.erase(dataonly.begin(), dataonly.begin() + best_asm_skip);
    unpacker.Uncompress(dataonly, params, best_packer.LiteralsInOrderOfCommonness);

    std::vector<unsigned char> decompress( unpacker.result);

    bool ok = true;
    unsigned r = decompress.size();

    if(buffer_orig.size() != r) ok = false;

    for(size_t a=0; a<best_packer.GammaSummary.size(); ++a)
        if(best_packer.GammaSummary[a])
        {
            unsigned min = 1 << a/2;
            unsigned max = min*2 - 1;
            fprintf(stderr, "Sent gamma sequences that encode in %u bits: %u (range: %u..%u)\n",
                (unsigned) a, (unsigned) best_packer.GammaSummary[a],
                min, max
            );
            // 2 bits: 10,11
            // 4 bits: 100,101,110,111,
            // 6 bits: 1000,1001,1010,1011,1100,1101,1110,1111
            for(unsigned m=min; m<=max && m<best_packer.GammaSummary2.size(); ++m)
                if(best_packer.GammaSummary2[m])
                    fprintf(stderr, "[%3u]: %u\n", (unsigned) m, best_packer.GammaSummary2[m]);
        }

    unsigned nmismatch=0, firstfail=~0u, lastfail=~0u;
    for(unsigned a=0; a<r; ++a)
        if(buffer_orig[a] != decompress[a])
        {
            ++nmismatch;
            if(firstfail==~0u) firstfail=a;
            lastfail=a;
        }
    if(nmismatch)
    {
        ok = false;
        fprintf(stderr, "%u failures, within %u..%u range (total size: %u)\n",
            nmismatch,firstfail,lastfail, r);
    }
    if(!ok) fprintf(stderr, "= ERROR\n");
#endif

    return 0;
}
