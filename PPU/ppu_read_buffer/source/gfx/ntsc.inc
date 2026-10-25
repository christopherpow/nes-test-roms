#include <array>

template<typename T>
void GenNTSCsignalTo(T& result, int pixel, int phase, int begin,int length) // Generate NTSC signal corresponding to NES color
{
    // Voltage levels, relative to synch voltage
    static const
    float levels[8] = {.350, .518, .962,1.550,  // Signal low
                      1.094,1.506,1.962,1.962}; // Signal high
    // Decode the NES color ("eeellcccc" format).
    int color = (pixel & 0x0F);    // 0..15 "cccc"
    int level = (pixel >> 4) & 3;  // 0..3  "ll"
    int emphasis = (pixel >> 6);   // 0..7  "eee"
    if(color > 13) { level = 1;  } // For colors 14..15, level 1 is forced.

    for(int p=0; p<length; ++p, ++phase)
    {
        // Generate the square wave:
        float sig = levels[ level + 4 * (color <= 12*((color+phase)%12 < 6)) ];
        // When de-emphasis bits are set, some parts of the signal are attenuated:
        if(emphasis & (0264513 >> (3*((phase%12)>>1)))) sig *= 0.746f;
        // Normalize the signal to 0..1 range, and save:
        result[begin+p] = (sig - levels[1]) / (levels[7]-levels[1]);
    }
}

template<int length=8>
std::array<float,length> GenNTSCsignal(int pixel, int phase) // Generate NTSC signal corresponding to NES color
{
    std::array<float, length> result;
    GenNTSCsignalTo(result, pixel, phase,  0,length);
    return result;
}



template<int c>
unsigned DecodeNTSCsignal(const std::array<float,c>& signal,
                          int begin,int end,
                          float phase,
                          float saturation = 1.3, // 1.7
                          float brightness = 1.0) // 1.0
{
    int length = end-begin;
    float factor = brightness/(length);
    if(begin < 0) begin = 0;
    if(end > c)  end = c;
    double y=0, i=0, q=0;
    for(int p = begin; p < end; ++p)
    {
        double value = signal[p] * factor;
        y += value;
        value *= saturation;
        i += value * std::cos( (3.141592653/6) * double(phase + p));
        q += value * std::sin( (3.141592653/6) * double(phase + p));
    }
    double gamma = 2.2;
    return
      0x10000*(int)std::min(255., 255.95 * std::pow(std::max(0.,y +  0.946882*i +  0.623557*q), 2.2 / gamma))
    + 0x00100*(int)std::min(255., 255.95 * std::pow(std::max(0.,y + -0.274788*i + -0.635691*q), 2.2 / gamma))
    + 0x00001*(int)std::min(255., 255.95 * std::pow(std::max(0.,y + -1.108545*i +  1.709007*q), 2.2 / gamma));
}
