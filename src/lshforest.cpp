#include "lshforest.hpp"

namespace dnn {

void
lshforest::init(int num_tree)
{
        m_trie     = trie_t(new tree_t[num_tree]);
        m_num_tree = num_tree;
}

}
