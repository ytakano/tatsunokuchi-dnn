#include "kmeans.hpp"

#include <boost/foreach.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/property_tree/json_parser.hpp>

namespace dnn {

using boost::property_tree::ptree;

std::ostream&
operator<< (std::ostream &out, const dnn::kmeans &km)
{
        km.print_json(out);

        return out;
}

std::istream&
operator>> (std::istream &in, dnn::kmeans &km)
{
        km.load_json(in);

        return in;
}

/*
 * {
 *   "root" : {
 *     "depth" : "m_depth",
 *     "dim"   : "m_dim",
 *     "left" : {
 *       "centroid" : [...],
 *       "left"  : { "centroid" : [...] },
 *       "right" : { "centroid" : [...] }
 *     },
 *     "right" : {
 *       "centroid" : [...],
 *       "left"  : { "centroid" : [...] },
 *       "right" : { "centroid" : [...] }
 *     }
 *   }
 * }
 */

void
kmeans::load_json(std::istream &in)
{
        ptree pt;

        try {
                read_json(in, pt);

                m_depth = pt.get<int>("depth");
                m_dim   = pt.get<int>("dim");

                m_root = kmnode_t(new kmeans_node);

                ptree &root = pt.get_child("root");
                build_tree_from_json(root, m_root);
        } catch (...) { }
}

void
kmeans::build_tree_from_json(ptree &pt, kmnode_t node)
{
        ptree *left, *right;

        try {
                left  = &pt.get_child("left");
                right = &pt.get_child("right");
        } catch (...) {
                return;
        }

        // for left
        node->m_left = kmnode_t(new kmeans_node);
        node->m_left->m_centroid = float_arr(new float[m_dim]);

        int    i;
        i = 0;

        try {
                ptree &cent = left->get_child("centroid");

                BOOST_FOREACH(const ptree::value_type &val, cent) {
                        float f = boost::lexical_cast<float>(val.second.data());
                        node->m_left->m_centroid[i++] = f;

                        if (i >= m_dim)
                                break;
                }
        } catch (...) { }

        for (; i < m_dim; i++) {
                node->m_left->m_centroid[i] = 0.0f;
        }

        build_tree_from_json(*left, node->m_left);


        // for right
        node->m_right = kmnode_t(new kmeans_node);
        node->m_right->m_centroid = float_arr(new float[m_dim]);

        i = 0;

        try {
                ptree &cent = right->get_child("centroid");

                BOOST_FOREACH(const ptree::value_type &val, cent) {
                        float f = boost::lexical_cast<float>(val.second.data());
                        node->m_right->m_centroid[i++] = f;

                        if (i >= m_dim)
                                break;
                }
        } catch (...) { }

        for (; i < m_dim; i++) {
                node->m_right->m_centroid[i] = 0.0;
        }

        build_tree_from_json(*right, node->m_right);
}

void
kmeans::print_json(std::ostream &out) const
{
        ptree pt;

        if (m_root.get() == NULL) {
                write_json(out, pt);
        } else {
                pt.add("depth", m_depth);
                pt.add("dim", m_dim);
                build_ptree(pt, m_root, "root");
                write_json(out, pt);
        }
}

void
kmeans::build_ptree(ptree &pt, kmnode_t node, std::string prefix) const
{
        if (node->m_left.get() == NULL || node->m_right.get() == NULL)
                return;

        std::string pre;
        ptree       child;
        int i;

        // for left child
        pre = prefix + ".left";
        for (i = 0; i < m_dim; i++) {
                std::ostringstream s;
                s << node->m_left->m_centroid[i];

                child.push_back(ptree::value_type("", ptree(s.str())));
        }
        pt.add_child(pre + ".centroid", child);

        child.clear();

        build_ptree(pt, node->m_left, pre);


        // for right child
        pre = prefix + ".right";

        for (i = 0; i < m_dim; i++) {
                std::ostringstream s;
                s << node->m_right->m_centroid[i];

                child.push_back(ptree::value_type("", ptree(s.str())));
        }
        pt.add_child(pre + ".centroid", child);

        child.clear();

        build_ptree(pt, node->m_right, pre);
}

hist
kmeans::get_hist(features_t features)
{
        hist hs;

        hs.m_dim  = 1 << m_depth;
        hs.m_hist = float_arr(new float[hs.m_dim]);

        for (uint32_t k = 0; k < hs.m_dim; k++)
                hs.m_hist[k] = 0.0f;

        if (m_root.get() == NULL)
                return hs;

        int n = 0;
        for (size_t i = 0; i < features->size(); i += m_dim) {
                uint32_t feat = classify(&(*features)[i]);

                hs.m_hist[feat] += 1.0f;

                n++;
        }

        return hs;
}

uint32_t
kmeans::classify(float *features)
{
        kmnode_t ptr = m_root;
        uint32_t ret   = 0;
        int      i;

        for (i = 0; i < m_depth; i++) {
                if (ptr->m_left.get() == NULL || ptr->m_right.get() == NULL)
                        break;

                float dist1 = 0.0f, dist2 = 0.0f;
                float d1, d2;

                for (int j = 0; j < m_dim; j++) {
                        d1  = features[j] - ptr->m_left->m_centroid[j];
                        d1 *= d1;
                        dist1 += d1;

                        d2  = features[j] - ptr->m_right->m_centroid[j];
                        d2 *= d2;
                        dist2 += d2;
                }

                ret <<= 1;
                if (dist1 < dist2) {
                        ptr  = ptr->m_left;
                        ret |= 1;
                } else {
                        ptr = ptr->m_right;
                }
        }

        ret <<= m_depth - i;

        return ret;
}

void
kmeans::add_features(features_t features)
{
        for (size_t i = 0; i < features->size(); i += m_dim) {
                feature f;

                f.m_features = features;
                f.m_index    = i;

                m_features.push_back(f);
        }
}

void
kmeans::fill_zero(float_arr arr)
{
        for (int i = 0; i < m_dim; i++)
                arr[i] = 0.0f;
}

void
kmeans::build_tree()
{
        float_arr left_cent, right_cent;
        gen_centroids(left_cent, right_cent, m_features);

        m_root = kmnode_t(new kmeans_node);

        build_tree(m_root, left_cent, right_cent, m_features);

        m_features.clear();
}

void
kmeans::build_tree(kmnode_t node, float_arr left_cent, float_arr right_cent,
                   std::vector<feature> features, int depth)
{
        if (depth >= m_depth)
                return;

        std::vector<feature> lfeats, rfeats;
        divide_features(lfeats, rfeats, left_cent, right_cent, features);

        float_arr cent1, cent2;

        // for left child
        if (lfeats.size() >= 2) {
                gen_centroids(cent1, cent2, lfeats);

                node->m_left = kmnode_t(new kmeans_node);
                node->m_left->m_centroid = left_cent;

                build_tree(node->m_left, cent1, cent2, lfeats, depth + 1);
        }


        // for right child
        if (rfeats.size() >= 2) {
                gen_centroids(cent1, cent2, rfeats);

                node->m_right = kmnode_t(new kmeans_node);
                node->m_right->m_centroid = right_cent;

                build_tree(node->m_right, cent1, cent2, rfeats, depth + 1);
        }
}

void
kmeans::gen_centroids(float_arr &left_cent, float_arr &right_cent,
                      std::vector<feature> features)
{
        float_arr lc1(new float[m_dim]);
        float_arr lc2(new float[m_dim]);
        float_arr rc1(new float[m_dim]);
        float_arr rc2(new float[m_dim]);

        init_centroids(lc2, rc2, features);

        float f0 = lc2[0];
        for (int i = 0; i < 10; i++) {
                update_centroids(lc1, rc1, lc2, rc2, features);

                float_arr tmp;
                tmp = rc1;
                rc1 = rc2;
                rc2 = tmp;

                tmp = lc1;
                lc1 = lc2;
                lc2 = tmp;

                if (f0 == lc2[0])
                        break;

                f0 = lc2[0];
        }

        left_cent  = lc2;
        right_cent = rc2;
}

void
kmeans::update_centroids(float_arr left_cent, float_arr right_cent,
                         float_arr left_cent_old, float_arr right_cent_old,
                         std::vector<feature> &features)
{
        std::vector<features_t>::iterator it;
        int left_num  = 0;
        int right_num = 0;
        int i;

        fill_zero(left_cent);
        fill_zero(right_cent);

        BOOST_FOREACH(feature &feat, features) {
                float dist1, dist2;
                dist1 = 0.0f;
                dist2 = 0.0f;

                for (i = 0; i < m_dim; i++) {
                        float d1, d2;
                        float val = feat[i];

                        d1  = left_cent_old[i] - val;
                        d1 *= d1;
                        dist1 += d1;

                        d2  = right_cent_old[i] - val;
                        d2 *= d2;
                        dist2 += d2;
                }

                if (dist1 < dist2) {
                        left_num++;
                        for (i = 0; i < m_dim; i++)
                                left_cent[i] += feat[i];
                } else {
                        right_num++;
                        for (i = 0; i < m_dim; i++)
                                right_cent[i] += feat[i];
                }
        }

        for (i = 0; i < m_dim; i++) {
                left_cent[i]  /= left_num;
                right_cent[i] /= right_num;
        }
}

void
kmeans::init_centroids(float_arr left_cent, float_arr right_cent,
                       std::vector<feature> &features)
{
        std::vector<feature>::iterator it;
        int left_num  = 0;
        int right_num = 0;
        int i = 0;

        fill_zero(left_cent);
        fill_zero(right_cent);

        BOOST_FOREACH(feature &feat, features) {
                int j;
                if (i == 0) {
                        i = 1;
                        left_num++;
                        for (j = 0; j < m_dim; j++)
                                left_cent[j] += feat[j];
                } else {
                        i = 0;
                        right_num++;
                        for (j = 0; j < m_dim; j++)
                                right_cent[j] += feat[j];
                }
        }

        for (i = 0; i < m_dim; i++) {
                left_cent[i]  /= left_num;
                right_cent[i] /= right_num;
        }
}

void
kmeans::divide_features(std::vector<feature> &left, std::vector<feature> &right,
                        float_arr left_cent, float_arr right_cent,
                        std::vector<feature> features)
{
        int i;

        BOOST_FOREACH(feature &feat, features) {
                float dist1, dist2;
                dist1 = 0.0f;
                dist2 = 0.0f;

                for (i = 0; i < m_dim; i++) {
                        float d1, d2;

                        d1  = left_cent[i] - feat[i];
                        d1 *= d1;
                        dist1 += d1;

                        d2  = right_cent[i] - feat[i];
                        d2 *= d2;
                        dist2 += d2;
                }

                if (dist1 < dist2)
                        left.push_back(feat);
                else
                        right.push_back(feat);
        }
}

}
