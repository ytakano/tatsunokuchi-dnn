#include "lshforest.hpp"

#include <math.h>

#include <fstream>

#include <boost/foreach.hpp>
#include <boost/unordered_set.hpp>

namespace dnn {

lshforest::hash_val
radix_substr(const lshforest::hash_val &entry, int begin, int num)
{
        lshforest::hash_val ret;
        uint32_t mask;

        if (num == 32)
                mask = 0;
        else
                mask   = 1 << num;

        mask  -= 1;
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
lshforest::init(uint32_t num_tree)
{
        if (num_tree <= 0)
                return;

        m_forest   = forest_t(new tree_t[num_tree]);
        m_num_tree = num_tree;
}

void
lshforest::get_similar(vec_objs &result, hash_t &hash,
                       hist &hs)
{
        if (hash.m_num != m_num_tree)
                return;

        typedef boost::unordered_map<std::string, uint32_t>::iterator it_t;
        boost::unordered_map<std::string, uint32_t> str_set;

        for (uint32_t i = 0; i < m_num_tree; i++) {
                std::vector<tree_t::iterator> vec;
                hash_val h;

                h.m_val = hash.m_hash[i];
                h.m_len = 32;

                m_forest[i].greedy_match(h, vec);

                BOOST_FOREACH(tree_t::iterator it, vec) {
                        BOOST_FOREACH(std::string val, *it->second) {
                                it_t it = str_set.find(val);
                                if (it != str_set.end()) {
                                        it->second++;
                                } else {
                                        str_set[val] = 0;
                                }
                        }
                }
        }

        for (it_t it = str_set.begin(); it != str_set.end(); ++it) {
                sim_info info = m_str2hash[it->first];
                sim_obj  dist;
                std::ifstream ifs(info.m_hist_file.c_str());

                if (ifs) {
                        hist hs2;
                        try {
                                ifs >> hs2;
                                dist.m_dist = hs2 - hs;
                        } catch (...) {
                                dist.m_dist = 1.0f;
                        }
                } else {
                        dist.m_dist = 1.0f;
                }

                if (dist.m_dist > m_threshold)
                        continue;

                dist.m_str = it->first;

                result.push_back(dist);
        }

        std::sort(result.begin(), result.end());
}

void
lshforest::remove_hash(std::string str)
{
        uint32_t i;
        hash_val h;
        hash_arr::iterator it1;

        it1 = m_str2hash.find(str);

        if (it1 != m_str2hash.end()) {
                for (i = 0; i < m_num_tree; i++) {
                        h.m_val = it1->second.m_hash[i];
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

bool
lshforest::add_hash(std::string str, std::string histfile, hash_t &hash)
{
        if (hash.m_num != m_num_tree)
                return false;

        remove_hash(str);

        for (uint32_t i = 0; i < m_num_tree; i++) {
                tree_t::iterator it;
                hash_val h;

                h.m_val = hash.m_hash[i];
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

        sim_info info;

        info.m_hash      = hash.m_hash;
        info.m_hist_file = histfile;

        m_str2hash[str] = info;

        return true;
}

}
