#include "dbh.hpp"

#include <math.h>
#include <stdlib.h>
#include <time.h>

#include <algorithm>

#include <boost/foreach.hpp>

namespace dnn {

const int dbh::bits = 32;

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
                for (int j = 0; j < bits; i++) {
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
                for (int j = 0; j < bits; j++) {
                        pivot pv = m_pivot[i * bits + j];
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
