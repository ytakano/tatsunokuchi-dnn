#include "dbh.hpp"

#include <math.h>
#include <stdlib.h>
#include <time.h>

namespace dnn {

const int dbh::bits = 32;

dbh::rnd_pair
dbh::gen_pair()
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

                if (m_pivot.find(pair.m_key) == m_pivot.end())
                        break;
        }

        if (i == i_max)
                throw error_gen_pair();

        return pair;
}

bool
dbh::gen_pivot()
{
        srandom(time(NULL));

        for (int i = 0; i < m_num_table; i++) {
                for (int j = 0; j < bits; i++) {
                        rnd_pair pair;

                        try {
                                pair = gen_pair();
                        } catch (...) {
                                m_pivot.clear();
                                return false;
                        }
                }
        }

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

}
