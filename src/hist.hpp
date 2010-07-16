#ifndef HIST_HPP
#define HIST_HPP

#include <stdint.h>

#include <iostream>

#include <boost/shared_array.hpp>

namespace dnn {

float dist(float *v1, float *v2, uint32_t len);


struct hist {
        typedef boost::shared_array<float> float_arr;
        float_arr       m_hist;
        uint32_t        m_dim;

        float operator- (const hist &rhs) const
        {
                if (m_dim != rhs.m_dim)
                        return 0.0f;

                return dist(m_hist.get(), rhs.m_hist.get(), m_dim);
        }
};

class error_read_hist { };

std::ostream& operator<< (std::ostream &out, const hist &hist);
std::istream& operator>> (std::istream &in, hist &hist);

}

#endif // HIST_HPP
