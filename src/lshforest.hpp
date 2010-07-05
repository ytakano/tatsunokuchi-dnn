#ifndef LSHFOREST_HPP
#define LSHFOREST_HPP

#include "radix_tree.hpp"

#include <set>
#include <string>

#include <boost/shared_array.hpp>
#include <boost/shared_ptr.hpp>

namespace dnn {

class lshforest {
public:

        void    init(int num_tree);
        void    add_hash(std::string str, boost::shared_array<uint32_t> hash);
        void    remove_hash(std::string str);
        
private:
        class hash_val {
        public:
                uint32_t m_val;
                int      m_len;

                hash_val() : m_val(0), m_len(0) { }

                uint32_t operator[] (int n) const
                {
                        uint32_t bit;

                        if (m_val & (0x80000000 >> n))
                                bit = 1;
                        else
                                bit = 0;

                        return bit;
                }

                bool operator== (const hash_val &rhs) const
                {
                        return m_len == rhs.m_len && m_val == rhs.m_val;
                }

                bool operator< (const hash_val &rhs) const
                {
                        if (m_val == rhs.m_val)
                                return m_len < rhs.m_len;
                        else
                                return m_val < rhs.m_val;
                }
        };

        friend hash_val radix_substr(const lshforest::hash_val &entry,
                                     int begin, int num);
        friend hash_val radix_join(const lshforest::hash_val &entry1,
                                   const lshforest::hash_val &entry2);
        friend int      radix_length(const lshforest::hash_val &entry);

        typedef boost::shared_ptr<std::set<std::string> > str_set;
        typedef radix_tree<hash_val, str_set> tree_t;
        typedef boost::shared_array<tree_t>   forest_t;
        typedef std::map<std::string, boost::shared_array<uint32_t> > hash_arr;

        forest_t        m_forest;
        hash_arr        m_str2hash;
        int             m_num_tree;
};

}

#endif // LSHFOREST_HPP
