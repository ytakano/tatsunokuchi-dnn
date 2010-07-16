#ifndef KMEANS_HPP
#define KMEANS_HPP

#include "feature.hpp"
#include "hist.hpp"

#include <stdint.h>

#include <iostream>
#include <string>
#include <vector>

#include <boost/property_tree/ptree.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/shared_array.hpp>

namespace dnn {

class kmeans {
public:
        typedef boost::shared_array<float> float_arr;

        kmeans() : m_dim(1), m_depth(10) { }

        void    add_features(features_t features);
        void    set_dim(int dim) { m_dim = dim; }
        void    set_depth(int depth) { m_depth = depth; }

        void    build_tree();
        hist    get_hist(features_t features);

        friend std::ostream& operator<< (std::ostream &out, const kmeans &km);
        friend std::istream& operator>> (std::istream &in, kmeans &km);

private:
        class kmeans_node {
        public:
                float_arr       m_centroid;
                boost::shared_ptr<kmeans_node>  m_left;
                boost::shared_ptr<kmeans_node>  m_right;
                int     m_depth;
        };

        class feature {
        public:
                features_t      m_features;
                int             m_index;

                float operator[] (int n) const
                {
                        return (*m_features)[m_index + n];
                }
        };

        typedef boost::shared_ptr<kmeans_node>  kmnode_t;

        uint32_t        classify(float *features);
        void    build_tree(kmnode_t node, float_arr left_cent,
                           float_arr right_cent, std::vector<feature> features,
                           int depth = 0);
        void    gen_centroids(float_arr &left_cent, float_arr &right_cent,
                              std::vector<feature> features);
        void    init_centroids(float_arr left_cent, float_arr right_cent,
                               std::vector<feature> &features);
        void    update_centroids(float_arr left_cent, float_arr right_cent,
                                 float_arr left_cent_old,
                                 float_arr right_cent_old,
                                 std::vector<feature> &features);
        void    fill_zero(float_arr arr);
        void    divide_features(std::vector<feature> &left,
                                std::vector<feature> &right,
                                float_arr left_cent, float_arr right_cent,
                                std::vector<feature> features);
        void    print_json(std::ostream &out) const;
        void    load_json(std::istream &in);
        void    build_ptree(boost::property_tree::ptree &pt, kmnode_t node,
                            std::string prefix) const;
        void    build_tree_from_json(boost::property_tree::ptree &pt,
                                     kmnode_t node);

        std::vector<feature>            m_features;
        kmnode_t        m_root;
        int             m_dim;
        int             m_depth;
};

}

#endif // KMEANS_HPP
