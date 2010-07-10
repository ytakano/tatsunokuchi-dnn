#include "hist.hpp"

#include <string.h>

namespace dnn {

#ifdef __GNUC__
typedef float v4sf __attribute__ ((vector_size (16))) __attribute__((aligned(16)));

typedef union {
        v4sf v;
        float f[4];
} f4vec;
#endif // __GNUC__

static const char *head = "histgram";

float
dist(float *v1, float *v2, uint32_t len)
{
        float dist = 0.0;
        f4vec d0, d1, d2, d3, d4, d5, d6, d7;

        uint32_t i;

#ifdef __GNUC__
        for (i = 0; i < (len / 32) * 32; i += 32) {
#define vectorize(IDX)                                          \
                do {                                            \
                        f4vec *f4v1, *f4v2;                     \
                        int j = i + IDX * 4;                    \
                                                                \
                        f4v1 = (f4vec*)&v1[j];                  \
                        f4v2 = (f4vec*)&v2[j];                  \
                                                                \
                        d##IDX.v  = f4v1->v - f4v2->v;          \
                        d##IDX.v *= d##IDX.v;                   \
                } while (0);

                vectorize(0);
                vectorize(1);
                vectorize(2);
                vectorize(3);
                vectorize(4);
                vectorize(5);
                vectorize(6);
                vectorize(7);

                d0.v += d1.v;
                d2.v += d3.v;
                d4.v += d5.v;
                d6.v += d7.v;

                d0.v += d2.v;
                d4.v += d6.v;

                d0.v += d4.v;

                dist += d0.f[0] + d0.f[1] + d0.f[2] + d0.f[3];
        }
#else
        i = 0;
#endif // #ifdef __GNUC__

        for (; i < len; i++) {
                float d = v1[i] - v2[i];
                d *= d;
                dist += d;
        }

        return dist;
}

std::ostream&
operator<< (std::ostream &out, const histgram &hist)
{
        out.write(head, strlen(head));
        out.write((char*)&hist.m_dim, sizeof(hist.m_dim));

        for (uint32_t i = 0; i < hist.m_dim; i++)
                out.write((char*)&hist.m_hist[i], sizeof(hist.m_hist[0]));

        return out;
}

std::istream&
operator>> (std::istream &in, histgram &hist)
{
        std::string h;
        size_t len = strlen(head);
        size_t i;

        for (i = 0; i < len && in; i++) {
                char c;
                in.read(&c, sizeof(c));
                h.push_back(c);
        }

        if (i < len || ! in)
                throw error_read_hist();

        in.read((char*)&hist.m_dim, sizeof(hist.m_dim));

        hist.m_hist = histgram::float_arr(new float[hist.m_dim]);

        uint32_t j;
        for (j = 0; j < hist.m_dim && in; j++) {
                float val;
                in.read((char*)&val, sizeof(val));
                hist.m_hist[j] = val;
        }

        if (j < hist.m_dim)
                throw error_read_hist();

        return in;
}

}
