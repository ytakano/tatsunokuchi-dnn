#ifndef HIST_HPP
#define HIST_HPP

#include <iostream>

#include <boost/shared_array.hpp>

namespace dnn {

struct histgram {
        typedef boost::shared_array<float> float_arr;
        float_arr       m_hist;
        uint32_t        m_dim;
};

std::ostream& operator<< (std::ostream &out, const histgram &km);

}

#endif // HIST_HPP
