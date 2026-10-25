#include <cstdio>
#include <string>
#include <vector>
#include <cmath>
#include <array>
#include <tuple>
#include <map>
#include <algorithm>
#include <random>
#include <set>
#include <gd.h>

#define USE_SPRITES 1

#define HUE 3.9
//#define HUE 4.2

#define USE_SCOLORQ 0


#if USE_SCOLORQ
# include "scolorq.inc"
#endif

static const unsigned dither8x8[8][8] = {
    {  0,  48,  12,  60,   3,  51,  15,  63 },
    { 32,  16,  44,  28,  35,  19,  47,  31 },
    {  8,  56,   4,  52,  11,  59,   7,  55 },
    { 40,  24,  36,  20,  43,  27,  39,  23 },
    {  2,  50,  14,  62,   1,  49,  13,  61 },
    { 34,  18,  46,  30,  33,  17,  45,  29 },
    { 10,  58,   6,  54,   9,  57,   5,  53 },
    { 42,  26,  38,  22,  41,  25,  37,  21 },
};

static void gdImagePng(gdImagePtr im, const char* fn)
{
    FILE* fp = std::fopen(fn, "wb");
    if(!fp)
        std::perror(fn);
    else
    {
        gdImagePng(im, fp);
        std::fclose(fp);
    }
}
static int rand(int a, int b)
{
    static std::mt19937 engine;
    return a + engine() % (b-a+1);
}

#include "ntsc.inc"

struct Color
{
    float c[4];     // r,g,b, y
    unsigned rgb;   // RGB
    unsigned index; // NES palette index

    Color() {}
    Color(char) : c{0.f,0.f,0.f} {}
    Color(unsigned RGB) : rgb(RGB) // GammaRGB: Convert gamma-corrected RGB into linear RGB
    {
        for(unsigned n=0; n<3; ++n)
            c[n] = (rgb >> (16-n*8)) & 0xFF;
        for(unsigned n=0; n<3; ++n)
            c[n] = std::pow((c[n]/255.0), 2.2);
        c[3] = (c[0]*299 + c[1]*587 + c[2]*114) / 255e3;
    }
    void RemakeRGB()
    {
        unsigned r = std::pow(c[0] < 0.f ? 0.f : c[0], 1/2.2f) * 255.f;
        unsigned g = std::pow(c[1] < 0.f ? 0.f : c[1], 1/2.2f) * 255.f;
        unsigned b = std::pow(c[2] < 0.f ? 0.f : c[2], 1/2.2f) * 255.f;
        rgb = ((r<255?r:255)<<16) | ((g<255?g:255)<<8) | ((b<255?b:255)<<0);
    }

    float Difference(const Color& b) const
    {
        float result=0, factors[4] = {1,1,1,0};//0.75, 0.75, 0.75, 0.25};
        /*
        float avg_a = (  c[0]+  c[1]+  c[2]) / 3.f;
        float avg_b = (b.c[0]+b.c[1]+b.c[2]) / 3.f;
        float variance_a = std::abs(avg_a-  c[0]) + std::abs(avg_a-  c[1]) + std::abs(avg_a-  c[2]);
        float variance_b = std::abs(avg_b-b.c[0]) + std::abs(avg_b-b.c[1]) + std::abs(avg_b-b.c[2]);
        float avg_variance = (variance_a+variance_b)*0.5f;
        // With very low variance, luma difference becomes important.
        avg_variance = 0.25f + avg_variance*0.75f;
        */
        for(unsigned n=0; n<4; ++n)
        {
            float d = c[n]-b.c[n];
            result += d*d * factors[n];
        }
        /*
        float lumadiff = (c[3] - b.c[3]);
        result = result*avg_variance + (1-avg_variance)*lumadiff*lumadiff;
        */
        return result;
    }
};

struct Combination
{
    std::vector<unsigned> items;
    Color result;
};

struct Combinations: public std::vector<Combination>
{
    Combinations() {}
    template<typename PaletteType>
    Combinations(const PaletteType& colors)
    {
        unsigned n=colors.size();
        /*
        for(unsigned c1=0; c1<n; ++c1)
        for(unsigned c2=c1; c2<n; ++c2)
        for(unsigned c3=c2; c3<n; ++c3)
        for(unsigned c4=c3; c4<n; ++c4)
        {
            if(c1==4||c2==4||c3==4||c4==4) continue;
            Color outcome('\0');
            unsigned ind[] = {c1,c2,c3,c4};
            for(auto c: ind)
                for(unsigned n=0; n<4; ++n)
                    outcome.c[n] += colors[c].c[n];
            emplace_back( std::move(Combination{ {ind,ind+4}, outcome }) );
        }
        */
        /**/
        for(unsigned c1=0; c1<n; ++c1)
        for(unsigned c2=c1; c2<n; ++c2)
        for(unsigned c3=c2; c3<n; ++c3)
        for(unsigned c4=c3; c4<n; ++c4)
        for(unsigned c5=c4; c5<n; ++c5)
        for(unsigned c6=c5; c6<n; ++c6)
        for(unsigned c7=c6; c7<n; ++c7)
        for(unsigned c8=c7; c8<n; ++c8)
        {
            if(c1==4||c2==4||c3==4||c4==4||c5==4||c6==4||c7==4||c8==4) continue;
            Color outcome('\0');
            unsigned ind[] = {c1,c2,c3,c4,c5,c6,c7,c8};
            for(auto c: ind)
                for(unsigned n=0; n<4; ++n)
                    outcome.c[n] += colors[c].c[n];
            emplace_back( std::move(Combination{ {ind,ind+8}, outcome }) );
        }
        /**/
        /*
        for(unsigned c1=0; c1<n; ++c1)
        for(unsigned c2=c1; c2<n; ++c2)
        {
            Color outcome('\0');
            unsigned ind[] = {c1,c2};
            for(auto c: ind)
                for(unsigned n=0; n<4; ++n)
                    outcome.c[n] += colors[c].c[n];
            emplace_back( std::move(Combination{ {ind,ind+2}, outcome }) );
        }
        */
    }
};

template<unsigned n_elements, typename PaletteType>
std::array<int,n_elements> DitherPixel(
    const Color& pixel,
    const PaletteType& palette,
    const Combinations& combinations)
{
    std::array<int,n_elements> result;

    unsigned n_combinations = combinations.size();
    unsigned n_sofar = 0;
    while(n_sofar < n_elements)
    {
        Color so_far('\0');
        for(unsigned c=0; c<n_sofar; ++c)
            for(unsigned n=0; n<4; ++n)
                so_far.c[n] += palette[ result[c] ].c[n];

        float best_difference = 0.f;
        std::vector<unsigned> chosen;

        for(unsigned combno=0; combno<n_combinations; ++combno)
        {
            const auto& items   = combinations[combno].items;
            const auto& outcome = combinations[combno].result;

            unsigned n_mix = items.size();
            unsigned limit = ((n_elements - n_sofar) / n_mix);
            for(unsigned n=1; n<=limit; ++n)
            {
                float mul = 1.f / (n_sofar + n_mix * n); // How to achieve a mathematical mean
                Color test('\0');
                for(unsigned p=0; p<4; ++p)
                    test.c[p] = (so_far.c[p] + outcome.c[p] * n) * mul;
                float difference = pixel.Difference(test);
                if(chosen.empty() || difference < best_difference)
                {
                    best_difference = difference;
                    chosen          = items;
                }
            }
        }
        // Add the chosen color(s) into the mix
        for(auto c: chosen)
            result[n_sofar++] = c;
    }
    // Sort the result according to luma
    std::sort(result.begin(), result.end(),
        [&palette](unsigned a, unsigned b)
        { return palette[a].c[3] < palette[b].c[3]; });
    return result;
}

/* Determine the amount of jitter that happens if pixels of these two colors are placed adjacent to each others */
double NTSCjitterStrengthMap[128][128];
void BuildNTSCjitterStrengthMap()
{
    std::array<std::array<float, 4*8>, 3> signal_buffers;
    for(unsigned color1=0; color1<128; ++color1)
    for(unsigned color2=0; color2<128; ++color2)
    {
        Color c1[3], c2[3];
        for(unsigned n=0; n<3; ++n)
        {
            // Generate two pixels of both colors
            GenNTSCsignalTo(signal_buffers[n], color1, n*4,  0, 16);
            GenNTSCsignalTo(signal_buffers[n], color2, n*4, 16, 16);
            // Decode the middle two pixels
            c1[n] = std::move(Color(DecodeNTSCsignal<4*8>(signal_buffers[n], 8, 8+12, HUE)));
            c2[n] = std::move(Color(DecodeNTSCsignal<4*8>(signal_buffers[n],16,16+12, HUE)));
        }
        // The jitter strength is the difference between successive decodes of each pixel
        NTSCjitterStrengthMap[color1][color2] =
            c1[0].Difference(c1[1])
          + c1[0].Difference(c1[2])
          + c1[1].Difference(c1[2])
          + c2[0].Difference(c2[1])
          + c2[0].Difference(c2[2])
          + c2[1].Difference(c2[2]);
        //fprintf(stderr, "%5.2f", NTSCjitterStrengthMap[color1][color2]);
        //if(color2==127) fprintf(stderr, "\n");
    }
}

template<int n_colors, typename PaletteType>
unsigned OrderedDitheringChoose(unsigned x,unsigned y, const std::array<int,n_colors>& mix, const PaletteType& /*palette*/)
{
    unsigned yp = y, xp = x;
    /*
        We might normally use [y%8][x%8] for the dithering pattern
        index, but in the case where doing so might cause severe
        diagonal NTSC artifacts, we must tweak the address a bit,
        by specifying it as [y%8][(x+y)%8].
    */
    double bestjitter = 0.0;
    for(unsigned n=0; n<8; ++n)
    {
        double each_jitter[8][7] = {};
        for(unsigned py=0; py<8; ++py)
        {
            int stripe[8];
            for(unsigned px=0; px<8; ++px)
                stripe[px] = mix[ dither8x8[py][(px + n*py)%8] * n_colors / 64 ];
            // Determine the amount of jitter on this scanline
            for(unsigned px=0; px<7; ++px)
                each_jitter[py][px] = NTSCjitterStrengthMap[ stripe[px] ] [ stripe[px+1] ];
        }
        // The "badness" of this jitter is determined by a product of two things:
        // How much it varies on scanline, and how much it coincides vertically.
        double variance = 0;
        double coinciding = 0;
        for(unsigned py=0; py<8; ++py)
            for(unsigned px=0; px<6; ++px)
                variance += std::abs(each_jitter[py][px] - each_jitter[py][px+1]);
        for(unsigned px=0; px<7; ++px)
            for(unsigned py=0; py<7; ++py)
            {
                coinciding += each_jitter[py][px] * each_jitter[py+1][(px+1)%7];
                coinciding += each_jitter[py][px] * each_jitter[py+1][(px+6)%7];
                //coinciding += each_jitter[py][px] * each_jitter[py+1][px];
            }
        double jitter = variance * coinciding;
        //fprintf(stderr, "For n=%u, jitter=%g\n", n,jitter);
        if(!n || jitter < bestjitter)
            { bestjitter = jitter;
              xp = x + n*y; }
    }
    return mix[ dither8x8[yp%8][xp%8] * n_colors / 64 ];
}


// Dither given image using the given palette
typedef std::tuple<gdImagePtr, std::vector<std::vector<Color>>> DitheredImageType;

template<int n_colors, typename PaletteType>
DitheredImageType DitherImage(
    const std::vector<std::vector<Color>>& pixels,
    const PaletteType& palette,
    const Combinations& combinations,
    const std::string& fn)
{
    std::map<unsigned, std::array<int,n_colors>> cache;
    typedef std::pair<unsigned, std::array<int,n_colors>> cache_elem_type;

    unsigned wid = pixels[0].size(), hei = pixels.size();
    gdImagePtr im2 = gdImageCreateTrueColor(wid, hei);

    std::vector<std::vector<Color>> result(hei);

    float uncount = 1.0 / n_colors;
    unsigned phase1 = 0;
    unsigned phase2 = 341*262;

#if USE_SCOLORQ
    using namespace scolorq;

    array2d< vector_fixed<double, 3> > filter1_weights(1, 1);
    array2d< vector_fixed<double, 3> > filter3_weights(3, 3);
    array2d< vector_fixed<double, 3> > filter5_weights(5, 5);
    std::vector< vector_fixed<double, 3> > scolorq_palette( palette.size() );
    for(int k=0; k<3; k++) filter1_weights(0,0)(k) = 1.0;
    int filter_size = 3;

    for(unsigned a=0; a< (unsigned)palette.size(); ++a)
    {
        vector_fixed<double,3>& c = scolorq_palette[a];
        for(unsigned n=0; n<3; ++n) c(n) = std::pow(palette[a].c[n], 1/1.0);
    }

    array2d< vector_fixed<double, 3> > image(wid, hei);
    array2d< int > quantized_image(wid, hei);

    for(unsigned y=0; y<hei; ++y)
        for(unsigned x=0; x<wid; ++x)
            for(unsigned n=0; n<3; ++n)
                image(x,y)(n) = std::pow(pixels[y][x].c[n], 1/1.0);

    array3d<double>* coarse_variables;
    double dithering_level = 0.09*std::log((double)image.get_width()*image.get_height()) - 0.04*std::log((double)palette.size()) + 0.001;

    dithering_level *= 1.0;

    double stddev = dithering_level;
    double sum = 0.0;
    for(int i=0; i<3; i++) {
        for(int j=0; j<3; j++) {
            for(int k=0; k<3; k++) {
                sum += filter3_weights(i,j)(k) =
                    std::exp(-std::sqrt((double)((i-1)*(i-1) + (j-1)*(j-1)))/(stddev*stddev));
            }
        }
    }
    sum /= 3;
    for(int i=0; i<3; i++) {
        for(int j=0; j<3; j++) {
            for(int k=0; k<3; k++) {
                filter3_weights(i,j)(k) /= sum;
            }
        }
    }
    sum = 0.0;
    for(int i=0; i<5; i++) {
        for(int j=0; j<5; j++) {
            for(int k=0; k<3; k++) {
                sum += filter5_weights(i,j)(k) =
                    std::exp(-std::sqrt((double)((i-2)*(i-2) + (j-2)*(j-2)))/(stddev*stddev));
            }
        }
    }
    sum /= 3;
    for(int i=0; i<5; i++) {
        for(int j=0; j<5; j++) {
            for(int k=0; k<3; k++) {
                filter5_weights(i,j)(k) /= sum;
            }
        }
    }

    array2d< vector_fixed<double, 3> >* filters[] =
        {NULL, &filter1_weights, NULL, &filter3_weights,
         NULL, &filter5_weights};
    std::printf("\rQuantizing..."); std::fflush(stdout);
    spatial_color_quant(image, *filters[filter_size], quantized_image, scolorq_palette, coarse_variables, 1.0, 0.001, 3, 1);
    //spatial_color_quant(image, filter3_weights, quantized_image, palette, coarse_variables, 0.05, 0.02);

    for(unsigned y=0; y<hei; ++y)
    {
        result[y].resize(wid);
        std::printf("\rFiltering... %d/%d", y,hei); std::fflush(stdout);

        std::array<float, 256*8> ntsc_signal_1;
        std::array<float, 256*8> ntsc_signal_2;

        const auto& xspan = pixels[y];
        for(unsigned x=0; x<wid; ++x)
        {
            unsigned rgb = xspan[x].rgb;
            auto i = cache.lower_bound(rgb);
            if(i == cache.end() || i->first != rgb)
            {
                i = cache.insert(i, std::move(
                    cache_elem_type{rgb, DitherPixel<n_colors>(xspan[x],palette,combinations)}));
            }
            const auto& mix = i->second;

            unsigned choose = quantized_image(x,y);

            //gdImageSetPixel(im2, x,y, palette[choose].rgb);

            unsigned nes_color_index = palette[choose].index;

            // FIXME: Avoid buffer overflow if wid > 256
            GenNTSCsignalTo(ntsc_signal_1, nes_color_index, (phase1+x)*8,  x*8, 8);
            GenNTSCsignalTo(ntsc_signal_2, nes_color_index, (phase2+x)*8,  x*8, 8);

            Color m('\0');
            m.index = choose;
            for(auto c: mix)
                for(unsigned n=0; n<4; ++n)
                    m.c[n] += palette[c].c[n];
            for(unsigned n=0; n<4; ++n)
                m.c[n] *= uncount;

            result[y][x] = std::move(m);
        }
        for(int x=0; x< int(wid); ++x)
        {
            Color c1(DecodeNTSCsignal<256*8>(ntsc_signal_1, x*8, x*8+12, ((phase1*8)%12)+HUE));
            Color c2(DecodeNTSCsignal<256*8>(ntsc_signal_2, x*8, x*8+12, ((phase2*8)%12)+HUE));
            auto& t = result[y][x];
            //for(unsigned n=0; n<4; ++n)
            //    t.c[n] = (t.c[n]*3 + c1.c[n] + c2.c[n]) / 5.0f;
            t.RemakeRGB();
            gdImageSetPixel(im2, x,y, t.rgb);
        }
        phase1 += 341;
        phase2 += 341;
    }
#else

    for(unsigned y=0; y<hei; ++y)
    {
        result[y].resize(wid);
        std::printf("\rQuantizing... %d/%d", y,hei); std::fflush(stdout);
        const auto& xspan = pixels[y];

        std::array<float, 256*8> ntsc_signal_1;
        std::array<float, 256*8> ntsc_signal_2;

        for(unsigned x=0; x<wid; ++x)
        {
            unsigned rgb = xspan[x].rgb;
            auto i = cache.lower_bound(rgb);
            if(i == cache.end() || i->first != rgb)
            {
                i = cache.insert(i, std::move(
                    cache_elem_type{rgb, DitherPixel<n_colors>(xspan[x],palette,combinations)}));
            }
            const auto& mix = i->second;

            unsigned choose = OrderedDitheringChoose<n_colors>(x, y, mix, palette);
            //gdImageSetPixel(im2, x,y, palette[choose].rgb);

            unsigned nes_color_index = palette[choose].index;

            // FIXME: Avoid buffer overflow if wid > 256
            GenNTSCsignalTo(ntsc_signal_1, nes_color_index, (phase1+x)*8,  x*8, 8);
            GenNTSCsignalTo(ntsc_signal_2, nes_color_index, (phase2+x)*8,  x*8, 8);

            Color m('\0');
            m.index = choose;
            for(auto c: mix)
                for(unsigned n=0; n<4; ++n)
                    m.c[n] += palette[c].c[n];
            for(unsigned n=0; n<4; ++n)
                m.c[n] *= uncount;

            result[y][x] = std::move(m);
        }
        for(int x=0; x< int(wid); ++x)
        {
            Color c1(DecodeNTSCsignal<256*8>(ntsc_signal_1, x*8, x*8+12, ((phase1*8)%12)+HUE));
            Color c2(DecodeNTSCsignal<256*8>(ntsc_signal_2, x*8, x*8+12, ((phase2*8)%12)+HUE));
            auto& t = result[y][x];
            for(unsigned n=0; n<4; ++n)
            {
                t.c[n] = (t.c[n]*3.0f + c1.c[n] + c2.c[n]) / 5.0f;
                //t.c[n] = (c1.c[n] + c2.c[n]) / 2.0f;
            }
            t.RemakeRGB();
            gdImageSetPixel(im2, x,y, t.rgb);
        }
        phase1 += 341;
        phase2 += 341;
    }
#endif

    if(!fn.empty()) gdImagePng(im2, fn.c_str());
    return DitheredImageType{im2, std::move(result)};
}

int main(int argc, char** argv)
{
    const char* SOURCE_IMAGE  = argv[1];
    const char* RESULT_IMAGE  = argv[2];
    const char* RESULT_DATA   = argv[3];
    const unsigned MAX_ROUNDS = std::atoi( argv[4] );
    const int n_colors = 16;

    // Load NES palette
    Color palette[64];
    for(unsigned c=0; c<64; ++c)
        palette[c] = DecodeNTSCsignal<12>( GenNTSCsignal<12>(c,0), 0,12, HUE);
    BuildNTSCjitterStrengthMap();

    // Load source image
    FILE* fp = std::fopen(SOURCE_IMAGE, "rb");
    if(!fp) { std::perror(SOURCE_IMAGE); return 1; }
    gdImagePtr im = gdImageCreateFromPng(fp);
    std::fclose(fp);
    unsigned sx = gdImageSX(im), sy = gdImageSY(im);
    /* Convert the image into a gamma-corrected array of pixels */
    std::vector<std::vector<Color>> pixels(240);
    for(unsigned y=0; y<240; ++y)
        for(unsigned x=0; x<sx; ++x)
            pixels[y].push_back( Color(0x000000u) );
    for(unsigned y=0; y<sy; ++y)
        for(unsigned x=0; x<sx; ++x)
            pixels[y+6][x] = Color( (unsigned) gdImageGetTrueColorPixel(im, x,y));
    sy = 240;

    // Create palettes.
    auto f = [&](unsigned i) -> Color
        { Color result = palette[i]; result.index = i; return result; };

    std::array<std::array<Color,4>,8> palettes =
    {{
#if 0
        // Background:
        {{ f(0x3D),f(0x18),f(0x37),f(0x1D) }},
        {{ f(0x3D),f(0x31),f(0x1D),f(0x00) }},
        {{ f(0x3D),f(0x37),f(0x3C),f(0x1D) }},
        {{ f(0x3D),f(0x1D),f(0x17),f(0x18) }},
        // Sprites:
        {{ f(0x3D),f(0x00),f(0x30),f(0x10) }},
        {{ f(0x3D),f(0x17),f(0x27),f(0x00) }},
        {{ f(0x3D),f(0x08),f(0x30),f(0x38) }},
        {{ f(0x3D),f(0x17),f(0x30),f(0x28) }},
#else
        {{ f(0x1D),f(0x18),f(0x37),f(0x3D) }},
        {{ f(0x1D),f(0x31),f(0x3D),f(0x00) }},
        {{ f(0x1D),f(0x38),f(0x3C),f(0x30) }},
        {{ f(0x1D),f(0x10),f(0x16),f(0x18) }},
        // Sprites:
        {{ f(0x1D),f(0x14),f(0x32),f(0x1C) }},
        {{ f(0x1D),f(0x28),f(0x26),f(0x00) }},
        {{ f(0x1D),f(0x16),f(0x37),f(0x3B) }},
        {{ f(0x1D),f(0x15),f(0x30),f(0x28) }},
#endif
    }}, backup_palettes;

    std::array<Combinations,      20> combinations, backup_combinations;
    std::array<DitheredImageType, 20> dithereds,    backup_dithereds;

    #pragma omp parallel for schedule(dynamic,1)
    for(unsigned n=0; n<20; ++n)
    {
    #if !USE_SPRITES
        if(n >= 4) continue;
    #endif
        unsigned p1 = n & 3;
        unsigned p2 = n >= 4 ? 4 + ((n-4)/4) : ~0u;
        std::printf("Regenerating %d,%d\n", p1,p2); std::fflush(stdout);

        std::vector<Color> combined_palette;
        combined_palette.insert(combined_palette.end(),
            palettes[p1].begin(), palettes[p1].end());
        if(n >= 4)
            combined_palette.insert(combined_palette.end(),
                palettes[p2].begin(), palettes[p2].end());

        combinations[n] = std::move( Combinations(combined_palette) );

        std::string fn;
        //{ char Buf[32]; sprintf(Buf, "out%d.png", n); fn = Buf; }
        dithereds[n] = std::move( DitherImage<n_colors>(pixels, combined_palette, combinations[n], fn) );
        //std::printf("\nwrote %s\n", fn.c_str()); std::fflush(stdout);
    }

    typedef std::array<std::array<char,4>,8> IdentityType;
    auto CreateIdentity = [&]() -> IdentityType
    {
        IdentityType result;
        for(unsigned p=0; p<8; ++p)
            for(unsigned n=0; n<4; ++n)
                result[p][n] = (char) palettes[p][n].index;
        return result;
    };
    std::set<IdentityType> tried;

    float best_totaldiff = 9e30f;

    for(unsigned round=0; round<MAX_ROUNDS; ++round)
    {
        tried.insert( CreateIdentity() );

        std::vector<std::vector<unsigned>> AttributeTable(sy);

        float total_diff = 0.f;
        gdImagePtr im2 = gdImageCreateTrueColor(sx,sy);
        #pragma omp parallel for schedule(static) reduction(+:total_diff)
        for(unsigned y=0; y<sy; y+=16)
        {
            for(unsigned py=0; py<16; py+=8) AttributeTable[y+py].resize(sx, 555);

            std::printf("\rChoosing tiles, %d/%d", y, sy); std::fflush(stdout);
            for(unsigned x=0; x<sx; x+=16)
            {
                float bestdiff  = 0.f;
                unsigned chosen = ~0u;
                for(unsigned p=0; p<4; ++p)
                {
                    float diff = 0.f;
                    for(unsigned py=0; py<16; py+=1)
                        for(unsigned px=0; px<16; px+=1)
                        {
                            const auto& revised_pixel  = std::get<1>(dithereds[p]) [y+py][x+px];
                            const auto& original_pixel = pixels[y+py][x+px];
                            diff += original_pixel.Difference(revised_pixel);
                        }
                    if(chosen == ~0u || diff < bestdiff)
                    {
                        bestdiff = diff;
                        chosen   = p;
                    }
                }
                total_diff += bestdiff;
                gdImageCopy(im2, std::get<0>(dithereds[chosen]), x,y, x,y, 16,16);
                for(unsigned py=0; py<16; py+=8)
                for(unsigned px=0; px<16; px+=8)
                    AttributeTable[y+py][x+px] = chosen;
            }
        } // end background select Y-loop

#if USE_SPRITES
        // For each scanline, record the number of sprites already there
        // (this will avoid the problem of 8-sprite limit per scanline)
        std::vector<unsigned> n_sprites_on_scanline(sy);
        struct Sprite
        {
            unsigned x,y, attr;
            std::string pattern;
        };
        std::array<Sprite,64> chosen_sprites;

        for(unsigned spriteno=0; spriteno<64; ++spriteno)
        {
            std::printf("\rChoosing sprite %d/64...", spriteno); std::fflush(stdout);
            // Find the best 8x16 region from each of the sprite-augmented
            // pictures. Rules: It must not overlap with any existing sprite
            // so far.
            float best_score = 0.f;
            unsigned best_x = 0, best_y = 0, best_attr = 0;

            #pragma omp parallel for schedule(static)
            for(unsigned y=4; y <= sy-16-8; ++y)
            {
                // Make sure that a sprite can at all be placed in this Y coordinate
                bool scanline_full = false;
                for(unsigned py=0; py<16; ++py)
                    if(n_sprites_on_scanline[y+py] >= 8)
                        { scanline_full = true; break; }
                if(scanline_full) continue;

                if(y< 64 && y+16 >= 64) continue; // Don't cross split-point 1
                if(y<128 && y+16 >=128) continue; // Don't cross split-point 2
                if(y<184 && y+16 >=184) continue; // Don't cross split-point 3

                for(unsigned x=0; x <= sx-8; ++x)
                {
                    // Make sure that this region does not overlap with any existing sprite
                    bool overlaps = false;
                    for(unsigned s=0; s<spriteno; ++s)
                    {
                        auto& sp = chosen_sprites[s];
                        unsigned top1 = y,    bottom1 = y+16,    left1 = x,    right1 = x+8;
                        unsigned top2 = sp.y, bottom2 = sp.y+16, left2 = sp.x, right2 = sp.x+8;

                        if( !(left1 >= right2 || right1 <= left2)
                        &&  !(top1 >= bottom2 || bottom1 <= top2))
                            { overlaps = true; break; }
                    }
                    if(overlaps) continue;

                    // From each sprite-augmented picture, test how much
                    // this particular 8x16 block would improve the picture:
                    for(unsigned spritecolor=4; spritecolor<8; ++spritecolor)
                    {
                        // Old_improvement = (so_far   - original)
                        // New_improvement = (augmented - original)
                        // Score = new_improvement - old_improvement
                        float this_score = 0;
                        for(unsigned py=0; py<16; ++py)
                        for(unsigned px=0; px< 8; ++px)
                        {
                            unsigned q = AttributeTable[ (y+py)&~7 ][ (x+px)&~7 ];
                            unsigned p = 4 + q + (spritecolor-4)*4;
                            const auto& revised_pixel   = std::get<1>(dithereds[q]) [y+py][x+px];
                            const auto& original_pixel  = pixels[y+py][x+px];
                            const auto& augmented_pixel = std::get<1>(dithereds[p]) [y+py][x+px];

                            float previous_diff = original_pixel.Difference(revised_pixel);
                            float new_diff      = original_pixel.Difference(augmented_pixel);
                            //if(new_diff < previous_diff)
                            {
                                float d = previous_diff - new_diff;
                                this_score += d;
                            }
                        }
                        #pragma omp critical(sprite_check)
                      {
                        if(this_score > best_score)
                        {
                            best_score = this_score;
                            best_x     = x;
                            best_y     = y;
                            best_attr  = spritecolor;
                        }
                      }
                    }
                }
            }
            std::printf("Best sprite: %u,%u with attribute %u (score %f)\n",
                best_x,best_y, best_attr, best_score);
            chosen_sprites[spriteno] = {best_x,best_y,best_attr|4, std::string(8*16,'?')};
            for(unsigned y=0; y<16; ++y)
                n_sprites_on_scanline[best_y+y] += 1;
        }

        // After choosing sprites, recalculate the image
        total_diff = 0.f;
        #pragma omp parallel for schedule(static) reduction(+:total_diff) collapse(2)
        for(unsigned y=0; y<sy; ++y)
            for(unsigned x=0; x<sx; ++x)
            {
                const auto& original_pixel  = pixels[y][x];

                unsigned q = (AttributeTable[y & ~7][x & ~7] % 4); // Background attribute
                bool handled = false;
                for(unsigned spriteno=0; spriteno<64; ++spriteno)
                {
                    auto& sp = chosen_sprites[spriteno];
                    if(sp.x <= x && sp.x+8  > x
                    && sp.y <= y && sp.y+16 > y)
                    {
                        unsigned spritecolor = sp.attr;
                        unsigned p = 4 + q + (spritecolor-4)*4;

                        const auto& augmented_pixel = std::get<1>(dithereds[p]) [y][x];

                        total_diff += augmented_pixel.Difference(original_pixel);
                        handled = true;

                        unsigned index = augmented_pixel.index;
                        if(index <= 4)
                        {
                            /*gdImageSetPixel(im2, x,y, palettes[q][index&3].rgb);*/
                        }
                        else
                        {
                            /*gdImageSetPixel(im2, x,y, palettes[spritecolor|4][index&3].rgb);*/
                            gdImageSetPixel(im2, x,y, augmented_pixel.rgb);
                        }

                        sp.pattern[ (y-sp.y)*8 + (x-sp.x) ] = index <= 4 ? '0' : ('0' + (index-4));
                        break;
                    }
                }
                if(!handled)
                {
                    // Background only
                    const auto& revised_pixel = std::get<1>(dithereds[q]) [y][x];
                    total_diff += revised_pixel.Difference(original_pixel);
                }
            }
#endif

        if(total_diff < best_totaldiff)
        {
            backup_palettes     = palettes;
            backup_dithereds    = dithereds;
            backup_combinations = combinations;

            std::printf("\nGot a new good result (score: %f, better than %f) with palette:\n", total_diff, best_totaldiff);
            for(unsigned p=0; p<8; ++p)
            {
                std::printf(".byte ");
                for(unsigned n=0; n<4; ++n) std::printf("$%02X,", palettes[p][n].index);
                std::printf("\n");
            }
            std::printf("Writing %s and %s\n", RESULT_IMAGE, RESULT_DATA);
            gdImagePng(im2, RESULT_IMAGE);
            gdImageDestroy(im2);

            best_totaldiff = total_diff;

            fp = std::fopen(RESULT_DATA, "wt");
            for(unsigned p=0; p<8; ++p)
            {
                std::fprintf(fp, ".palette ");
                for(unsigned n=0; n<4; ++n) std::fprintf(fp, "$%02X,", palettes[p][n].index);
                std::fprintf(fp, "\n");
            }
            for(unsigned y=0; y<sy; y+=8)
                for(unsigned x=0; x<sx; x+=8)
                {
                    unsigned p = (AttributeTable[y][x] % 4); // Which source image?
                    std::string s;
                    for(unsigned py=0; py<8; ++py)
                        for(unsigned px=0; px<8; ++px)
                        {
                            unsigned q = p;
                        #if USE_SPRITES
                            for(unsigned spriteno=0; spriteno<64; ++spriteno)
                            {
                                auto& sp = chosen_sprites[spriteno];
                                if(sp.x <= x+px && sp.x+8  > x+px
                                && sp.y <= y+py && sp.y+16 > y+py)
                                {
                                    unsigned spritecolor = sp.attr;
                                    q = 4 + q + (spritecolor-4)*4;
                                    break;
                                }
                            }
                        #endif
                            int d = std::get<1>(dithereds[q]) [y+py][x+px].index;

                            if(d >= 4)
                            {
                                if(d >= 5)
                                {
                                    // If the pixel is masked by sprite, try representing some of the
                                    // non-augmented graphics below it. This makes the image look nice
                                    // even when sprites are disabled or not supported.
                                    d = std::get<1>(dithereds[p]) [y+py][x+px].index;
                                }
                                else
                                    d=0;
                            }

                            s += ('0' + d);
                        }
                    std::fprintf(fp, "%-4d %-4d %d %s\n", x,y, p, s.c_str());
                }
        #if USE_SPRITES
            for(unsigned spriteno=0; spriteno<64; ++spriteno)
            {
                auto& sp = chosen_sprites[spriteno];
                std::fprintf(fp, ".sprite %3u %3u %u %s\n",
                    sp.x, sp.y,
                    sp.attr,
                    sp.pattern.c_str()
                );
            }
        #endif
            std::fclose(fp);
        }

        if(round+1 >= MAX_ROUNDS) break;

        // Issue a random change to one of the palettes
        palettes     = backup_palettes;
        dithereds    = backup_dithereds;
        combinations = backup_combinations;

        unsigned p=0, n_attempts = 0;
        unsigned which_color, new_color, current_color;
        IdentityType attempt;
        do {
    #if !USE_SPRITES
            p             = rand(0,3);
    #else
            p             = rand(0,7);
    #endif
            which_color   = rand(1,3);
            current_color = palettes[p][which_color].index;

            if(n_attempts < 64)
            {
                new_color = current_color;
                new_color = ((((new_color >> 4) + rand(-1,1)) & 3) << 4)
                           | (((new_color & 0x0F) + rand(13,15)) % 14);
            }
            else
            {
                new_color = rand(0x0,0xD) | (rand(0,3)<<4);
            }
            attempt = CreateIdentity();
            attempt[p][which_color] = (char) new_color;
            ++n_attempts;
        } while(new_color == 0x0F || new_color == 0x20
             || new_color == 0x3D || new_color == 0x0D
             || new_color == current_color
             || tried.find(attempt) != tried.end());

        palettes[p][which_color] = f(new_color);

        std::printf("\rTrying for %d: ", p);
        for(unsigned n=0; n<4; ++n)
            std::printf("$%02X,", palettes[p][n].index);
        std::printf("\n");
        std::fflush(stdout);

        #pragma omp parallel for schedule(dynamic,1)
        for(unsigned n=0; n<20; ++n)
        {
    #if !USE_SPRITES
            if(n >= 4) continue;
    #endif
            unsigned p1 = n & 3;
            unsigned p2 = n >= 4 ? 4 + ((n-4)/4) : ~0u;
            if(p1 == p || p2 == p)
            {
                std::printf("Regenerating %d,%d\n", p1,p2); std::fflush(stdout);

                std::vector<Color> combined_palette;
                combined_palette.insert(combined_palette.end(),
                    palettes[p1].begin(), palettes[p1].end());
                if(n >= 4)
                    combined_palette.insert(combined_palette.end(),
                        palettes[p2].begin(), palettes[p2].end());

                combinations[n] = std::move( Combinations(combined_palette) );

                std::string fn;
                //{ char Buf[32]; sprintf(Buf, "test%d.png", n); fn = Buf; }
                dithereds[n] = std::move( DitherImage<n_colors>(pixels, combined_palette, combinations[n], fn) );
                //std::printf("\nwrote %s\n", fn.c_str()); std::fflush(stdout);
            }
        }
    } // next round
}
