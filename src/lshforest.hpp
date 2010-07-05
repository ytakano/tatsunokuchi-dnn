#ifndef LSHFOREST_HPP
#define LSHFOREST_HPP

#include "radix_tree.hpp"

#include <string>

#include <boost/shared_array.hpp>

namespace dnn {

class lshforest {
public:

        void    init(int num_tree);
        
private:
        class hash_val {
        public:
                uint32_t m_val;
                int      m_len;
        };

        typedef radix_tree<hash_val, std::string> tree_t;
        typedef boost::shared_array<tree_t>       trie_t;

        trie_t  m_trie;
        int     m_num_tree;
};

}

#endif // LSHFOREST_HPP
