#ifndef DBH_HPP
#define DBH_HPP

#include "hist.hpp"

#include <map>
#include <set>
#include <vector>

namespace dnn {

class dbh {
public:
        typedef boost::shared_array<float> float_arr;

        dbh() : m_num_table(5), m_dim(1) { }

        void    add_hist(float_arr hist) { m_hist.push_back(hist); }
        void    set_dim(int dim) { m_dim = dim; }
        int     get_dim() const { return m_dim; }
        void    set_bits(unsigned int bits) { m_bits = bits < 32 ? bits : 32; }
        void    set_num_table(int num) { m_num_table = num; }
        int     get_num_table() const { return m_num_table; }

        bool    build_pivot();
        void    get_hash(uint32_t *hash, float_arr hist);

        friend std::ostream& operator<< (std::ostream &out, const dbh &d);
        friend std::istream& operator>> (std::istream &in, dbh &d);

private:
        union rnd_pair {
                uint32_t m_key;
                struct {
                        uint16_t m_key1;
                        uint16_t m_key2;
                } m_keys;
        };

        struct pivot {
                float_arr       m_x1;
                float_arr       m_x2;
                float           m_median;
        };

        class error_gen_pair { };

        // throw error_gen_pair
        rnd_pair        gen_pair(std::set<uint32_t> &pair_set);
        float           get_dist(float_arr x, float_arr x1, float_arr x2);

        void    print_json(std::ostream &out) const;
        void    load_json(std::istream &in);

        std::vector<float_arr>  m_hist;
        std::vector<pivot>      m_pivot;
        unsigned int    m_bits;
        int     m_num_table;
        int     m_dim;
};

}

#endif // DBH_HPP
