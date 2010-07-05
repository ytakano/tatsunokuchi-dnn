#include "lshforest.hpp"

namespace dnn {

lshforest::hash_val
radix_substr(const lshforest::hash_val &entry, int begin, int num)
{
        lshforest::hash_val ret;
        uint32_t mask;

        mask   = (1 << num) - 1;
        mask <<= 32 - num - begin;

        ret.m_val = (entry.m_val & mask) << begin;
        ret.m_len = num;

        return ret;
}

lshforest::hash_val
radix_join(const lshforest::hash_val &entry1, const lshforest::hash_val &entry2)
{
        lshforest::hash_val ret;

        ret.m_val  = entry1.m_val;
        ret.m_val |= entry2.m_val >> entry1.m_len;
        ret.m_len  = entry1.m_len + entry2.m_len;

        return ret;
}

int
radix_length(const lshforest::hash_val &entry)
{
        return entry.m_len;
}

void
lshforest::init(int num_tree)
{
        if (num_tree <= 0)
                return;

        m_forest   = forest_t(new tree_t[num_tree]);
        m_num_tree = num_tree;
}

void
lshforest::remove_hash(std::string str)
{
        int i;
        hash_val h;
        hash_arr::iterator it1;

        it1 = m_str2hash.find(str);

        if (it1 != m_str2hash.end()) {
                for (i = 0; i < m_num_tree; i++) {
                        h.m_val = it1->second[i];
                        h.m_len = 32;

                        tree_t::iterator it2;

                        it2 = m_forest[i].find(h);
       
                        if (it2 != m_forest[i].end()) {
                                it2->second->erase(str);

                                if (it2->second->empty())
                                        m_forest[i].erase(it2);
                        }
                }

                m_str2hash.erase(it1);
        }
}

void
lshforest::add_hash(std::string str, boost::shared_array<uint32_t> hash)
{
        remove_hash(str);

        for (int i = 0; i < m_num_tree; i++) {
                tree_t::iterator it;
                hash_val h;

                h.m_val = hash[i];
                h.m_len = 32;

                it = m_forest[i].find(h);

                if (it == m_forest[i].end()) {
                        str_set strs(new std::set<std::string>);

                        strs->insert(str);
                        m_forest[i].insert(std::pair<hash_val, str_set>(h, strs));
                } else {
                        it->second->insert(str);
                }
        }

        m_str2hash[str] = hash;
}

}
