#ifndef HASH_T_HPP
#define HASH_T_HPP

#include <stdint.h>

#include <iostream>

#include <boost/shared_array.hpp>

namespace dnn {

struct hash_t {
        uint32_t m_num;
        boost::shared_array<uint32_t> m_hash;
};

class error_read_hash_t { };

std::ostream& operator<< (std::ostream &out, const hash_t &hash);
std::istream& operator>> (std::istream &in, hash_t &hash);

}

#endif // HASH_HPP
