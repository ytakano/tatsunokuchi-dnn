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

}
