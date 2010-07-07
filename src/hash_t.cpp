#include "hash_t.hpp"

#include <string.h>

namespace dnn {

static const char *type = "dbh";

std::ostream&
operator<< (std::ostream &out, const hash_t &hash)
{
        out.write(type, 4);
        out.write((char*)&hash.m_num, sizeof(hash.m_num));

        for (uint32_t i = 0; i < hash.m_num; i++)
                out.write((char*)&hash.m_hash[i], sizeof(uint32_t));

        return out;
}

std::istream&
operator>> (std::istream &in, hash_t &hash)
{
        char head[4];

        in.read(head, 4);

        if (memcmp(head, type, 4) != 0)
                throw error_read_hash_t();

        if (! in)
                throw error_read_hash_t();

        in.read((char*)&hash.m_num, sizeof(hash.m_num));

        hash.m_hash = boost::shared_array<uint32_t>(new uint32_t[hash.m_num]);

        for (uint32_t i = 0; i < hash.m_num; i++) {
                if (! in)
                        throw error_read_hash_t();

                in.read((char*)&hash.m_hash[i], sizeof(uint32_t));
        }

        return in;
}

}
