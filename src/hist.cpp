#include "hist.hpp"

#include <string.h>

namespace dnn {

const char *head = "histgram";

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
