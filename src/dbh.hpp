#ifndef DBH_HPP
#define DBH_HPP

#include "hist.hpp"

#include <map>
#include <vector>

namespace dnn {

class dbh {
        static const int bits;

public:
        typedef boost::shared_array<float> float_arr;

        void    add_hist(float_arr hist) { m_hist.push_back(hist); }
        void    set_dim(int dim) { m_dim = dim; }

        dbh() : m_num_table(5), m_dim(1) { }

private:
        union rnd_pair {
                uint32_t m_key;
                struct {
                        uint16_t m_key1;
                        uint16_t m_key2;
                } m_keys;
        };

        class pivot {
                float_arr       m_x1;
                float_arr       m_x2;
                float           m_median;
        };

        class error_gen_pair { };

        bool            gen_pivot();
        rnd_pair        gen_pair();  //throw error_gen_pair
        float           get_dist(float_arr x, float_arr x1, float_arr x2);

        std::vector<float_arr>  m_hist;
        std::map<uint32_t, pivot>       m_pivot;
        int     m_num_table;
        int     m_dim;
};

}

#endif // DBH_HPP
