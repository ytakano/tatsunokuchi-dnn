#include "dbh.hpp"

#include <math.h>
#include <stdlib.h>
#include <time.h>

#include <algorithm>

#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/property_tree/json_parser.hpp>

namespace dnn {

using boost::property_tree::ptree;

std::ostream&
operator<< (std::ostream &out, const dbh &d)
{
        d.print_json(out);

        return out;
}

std::istream&
operator>> (std::istream &in, dbh &d)
{

        return in;
}

/* {
 *   "dim" : 1024,
 *   "bits" : 32,
 *   "num_table" : 5,
 *   "pivots" : [
 *     {
 *       "median" : 0.5,
 *       "x1" : [...],
 *       "x2" : [...]
 *     },
 *     {
 *       "median" : 0.5,
 *       "x1" : [...],
 *       "x2" : [...]
 *     }
 *   ]
 * }
 */

void
dbh::print_json(std::ostream &out) const
{
        ptree pt;

        if (m_pivot.size() < m_bits * m_num_table) {
                write_json(out, pt);
                return;
        }

        pt.add("dim", m_dim);
        pt.add("bits", m_bits);
        pt.add("num_table", m_num_table);

        ptree child;
        BOOST_FOREACH(const pivot &pv, m_pivot) {
                ptree p;
                ptree x1, x2;

                for (int i = 0; i < m_dim; i++) {
                        std::ostringstream s1, s2;

                        s1 << pv.m_x1[i];
                        s2 << pv.m_x2[i];

                        x1.push_back(ptree::value_type("", ptree(s1.str())));
                        x2.push_back(ptree::value_type("", ptree(s2.str())));
                }

                p.add("median", pv.m_median);
                p.push_back(ptree::value_type("x1", x1));
                p.push_back(ptree::value_type("x2", x2));

                child.push_back(ptree::value_type("", p));
        }

        pt.add_child("pivots", child);

        write_json(out, pt);
}

dbh::rnd_pair
dbh::gen_pair(std::set<uint32_t> &pair_set)
{
        static const int i_max = 100;
        rnd_pair pair;
        int i;

        for (i = 0; i < i_max; i++) {
                pair.m_keys.m_key1 = random() % m_hist.size();
                pair.m_keys.m_key2 = random() % m_hist.size();

                if (pair.m_keys.m_key1 == pair.m_keys.m_key2)
                        continue;

                if (pair.m_keys.m_key1 > pair.m_keys.m_key2) {
                        uint16_t tmp = pair.m_keys.m_key1;
                        pair.m_keys.m_key1 = pair.m_keys.m_key2;
                        pair.m_keys.m_key2 = tmp;
                }

                if (pair_set.find(pair.m_key) == pair_set.end())
                        break;
        }

        if (i == i_max)
                throw error_gen_pair();

        return pair;
}

bool
dbh::build_pivot()
{
        std::set<uint32_t> pair_set;

        srandom(time(NULL));

        m_pivot.clear();

        for (int i = 0; i < m_num_table; i++) {
                for (unsigned int j = 0; j < m_bits; i++) {
                        rnd_pair pair;
                        pivot    pv;

                        try {
                                pair = gen_pair(pair_set);
                        } catch (...) {
                                m_pivot.clear();
                                return false;
                        }

                        pair_set.insert(pair.m_key);

                        pv.m_x1 = m_hist[pair.m_keys.m_key1];
                        pv.m_x2 = m_hist[pair.m_keys.m_key2];


                        std::vector<float> dist;

                        BOOST_FOREACH(float_arr &hist, m_hist) {
                                float d = get_dist(hist, pv.m_x1, pv.m_x2);
                                dist.push_back(d);
                        }

                        std::sort(dist.begin(), dist.end());

                        pv.m_median = dist[dist.size() / 2];

                        m_pivot.push_back(pv);
                }
        }

        m_hist.clear();

        return true;
}

float
dbh::get_dist(float_arr x, float_arr x1, float_arr x2)
{
        float xx1  = 0.0;
        float xx2  = 0.0;
        float x1x2 = 0.0;

        for (int i = 0; i < m_dim; i++) {
                float d;

                d = x[i] - x1[i];
                xx1 += d * d;

                d = x[i] - x2[i];
                xx2 += d * d;

                d = x1[i] - x2[i];
                x1x2 += d * d;
        }

        return (xx1 + x1x2 - xx2) / 2 / sqrtf(x1x2);
}

void
dbh::get_hash(uint32_t *hash, float_arr hist)
{
        for (int i = 0; i < m_num_table; i++) {
                uint32_t h = 0;
                for (unsigned int j = 0; j < m_bits; j++) {
                        pivot pv = m_pivot[i * m_bits + j];
                        float d  = get_dist(hist, pv.m_x1, pv.m_x2);

                        if (d < pv.m_median) {
                                h <<= 1;
                                h  |= 1;
                        }
                }

                hash[i] = h;
        }
}

}
