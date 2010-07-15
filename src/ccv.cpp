#include "ccv.hpp"

#include <stdint.h>

#include <opencv/highgui.h>

#include <boost/shared_array.hpp>
#include <boost/unordered_map.hpp>

/*
 * The color coherence vector (CCV) is a refined histogram technique.
 * However, instead of counting only the number of pixels of a certain color,
 * the color coherence vector also differentiates between pixels of the same
 * color depending on the size of the color region they belong to. If the region
 * (i.e., the connected 8-neighbor component of that color) is larger than
 * t_ccv, a pixel is regarded as coherent, otherwise, as incoherent. Thus, in an
 * image there are two values associated with each color j:
 * - alpha_j, the number of coherent pixels of color j, and
 * - beta_j, the number of incoherent pixels of color j.
 *
 * A color coherence vector then is defined as the vector
 * <(alpha_1, beta_1), ..., (alpha_n, beta_n)>
 *
 * Before calculating the color coherence vector we scaled the input image to
 * 240 x 160 pixels and smoothed the image by a Gaussian filter of sigma 1 as
 * also done by Pass et. al. t_ccv was set to 25, and the color space used only
 * the two most siginificant bits of each RGB color component.
 */

namespace dnn {

static const char *head = "histgram";

union rgb {
        uint32_t m_color32;
        uint8_t  m_color8[4];

        bool operator== (const rgb &rhs) const
        {
                return m_color32 == rhs.m_color32;
        }
};

struct label_info {
        rgb m_color;
        int m_count;
        uint32_t m_alias;
};

//#define ccv_width 240
//#define ccv_height 160
//#define t_ccv 25
static const int ccv_width  = 240;
static const int ccv_height = 160;
static const int t_ccv = 25;

inline
void
get_rgb(rgb &color, const cv::Mat &img, int x, int y)
{
        uint8_t r, b, g;
        int idx = (x + y * ccv_width) * 3;

        b = img.data[idx];
        g = img.data[idx + 1];
        r = img.data[idx + 2];

        b >>= 6;
        g >>= 6;
        r >>= 6;

        color.m_color8[0] = b;
        color.m_color8[1] = g;
        color.m_color8[2] = r;
        color.m_color8[3] = 0;
}

inline
int
rgb2idx(const rgb &color)
{
        return (int)color.m_color8[0] | (int)color.m_color8[1] << 2 |
                (int)color.m_color8[2] << 4;
}

inline
uint32_t
find_label(uint32_t label, boost::unordered_map<uint32_t, label_info> &labels)
{
        boost::unordered_map<uint32_t, label_info>::iterator it;

        for (it = labels.find(label); label != it->second.m_alias;) {
                label = it->second.m_alias;
                it = labels.find(label);
        }

        return label;
}

void
ccv(const cv::Mat &src, feature_ccv &ret)
{
        boost::shared_array<uint32_t>  labeled;
        boost::unordered_map<uint32_t, label_info> labels;
        cv::Mat resized;
        int i;

        if (src.channels() != 3)
                return;

        cv::resize(src, resized, cv::Size(ccv_width, ccv_height));
        cv::GaussianBlur(resized, resized, cv::Size(3, 3), 0);

        ret.m_alpha = feature_ccv::float_arr(new float[NUM_CCV_COLOR]);
        ret.m_beta  = feature_ccv::float_arr(new float[NUM_CCV_COLOR]);

        for (i = 0; i < NUM_CCV_COLOR; i++) {
                ret.m_alpha[i] = 0.0f;
                ret.m_beta[i]  = 0.0f;
        }

        label_info info;
        rgb        color;
        uint32_t   label;
        int        size;

        size = ccv_width * ccv_height;
        labeled = boost::shared_array<uint32_t>(new uint32_t[size]);

        get_rgb(color, resized, 0, 0);

        label = 0;

        labeled[0] = label;

        info.m_color = color;
        info.m_alias = label;
        info.m_count = 1;

        labels[label] = info;

        int x, y;
        for (x = 1; x < ccv_width; x++) {
                rgb color_here, color_left;

                get_rgb(color_here, resized, x,     0);
                get_rgb(color_left, resized, x - 1, 0);

                if (color_here.m_color32 == color_left.m_color32) {
                        labels[label].m_count++;
                        labeled[x] = label;
                } else {
                        label++;
                        labeled[x] = label;

                        info.m_color = color_here;
                        info.m_alias = label;
                        info.m_count = 1;

                        labels[label] = info;
                }
        }

        for (y = 1; y < ccv_height; y++) {
                rgb c_here, c_left, c_above, c_above_l, c_above_r;
                for (x = 0; x < ccv_width; x++) {
                        get_rgb(c_here, resized, x, y);

                        if (x == 0) {
                                c_left.m_color32    = 0xffffffff;
                                c_above_l.m_color32 = 0xffffffff;
                        }

                        if (x + 1 < ccv_width)
                                get_rgb(c_above_r, resized, x + 1, y - 1);
                        else
                                c_above_r.m_color32 = 0xffffffff;

                        get_rgb(c_above, resized, x, y - 1);


                        uint32_t same[4];
                        uint32_t l;
                        int      num_same = 0;

                        if (c_here == c_left) {
                                l = labeled[x - 1 + y * ccv_width];
                                same[num_same] = find_label(l, labels);
                                num_same++;
                        }

                        if (c_here == c_above) {
                                l = labeled[x + (y - 1) * ccv_width];
                                same[num_same] = find_label(l, labels);
                                num_same++;
                        }

                        if (c_here == c_above_l) {
                                l = labeled[x - 1 + (y - 1) * ccv_width];
                                same[num_same] = find_label(l, labels);
                                num_same++;
                        }

                        if (c_here == c_above_r) {
                                l = labeled[x + 1 + (y - 1) * ccv_width];
                                same[num_same] = find_label(l, labels);
                                num_same++;
                        }

                        if (num_same > 0) {
                                boost::unordered_map<uint32_t,
                                        label_info>::iterator it1, it2;
                                uint32_t label_min = same[0];

                                for (i = 1; i < num_same; i++) {
                                        if (same[i] < label_min) {
                                                label_min = same[i];
                                        }
                                }

                                it1 = labels.find(label_min);
                                it1->second.m_count++;

                                for (i = 0; i < num_same; i++) {
                                        if (label_min < same[i]) {
                                                it2 = labels.find(same[i]);

                                                int count = it1->second.m_count;

                                                it2->second.m_alias = label_min;
                                                it2->second.m_count = 0;

                                                it1->second.m_count += count;
                                        }
                                }

                                labeled[x + y * ccv_width] = label_min;
                        } else {
                                label++;
                                labeled[x + y * ccv_width] = label;

                                info.m_color = c_here;
                                info.m_alias = label;
                                info.m_count = 1;

                                labels[label] = info;
                        }
                }

                c_left    = c_here;
                c_above   = c_above_r;
                c_above_l = c_above;
        }

        boost::unordered_map<uint32_t, label_info>::iterator it;
        for (it = labels.begin(); it != labels.end(); ++it) {
                int idx;
                idx = rgb2idx(it->second.m_color);

                if (it->second.m_count > t_ccv) {
                        ret.m_alpha[idx] += it->second.m_count;
                } else {
                        ret.m_beta[idx] += it->second.m_count;
                }
        }

        float num_pix = ccv_width * ccv_height;
        for (i = 0; i < NUM_CCV_COLOR; i++) {
                ret.m_alpha[i] /= num_pix;
                ret.m_beta[i]  /= num_pix;
        }
}

bool
get_ccv_feat(const char *file, feature_ccv &feat)
{
        try {
                cv::Mat colorImage = cv::imread(file, 1);
                if(colorImage.empty())
                        return false;

                ccv(colorImage, feat);
        } catch (...) {
                return false;
        }

        return true;
}

std::ostream&
operator<< (std::ostream &out, const feature_ccv &feat)
{
        uint32_t dim = NUM_CCV_COLOR * 2;
        uint32_t i;

        out.write(head, strlen(head));
        out.write((char*)&dim, sizeof(dim));

        for (i = 0; i < NUM_CCV_COLOR; i++)
                out.write((char*)&feat.m_alpha[i], sizeof(float));

        for (i = 0; i < NUM_CCV_COLOR; i++)
                out.write((char*)&feat.m_beta[i], sizeof(float));

        return out;
}

std::istream&
operator>> (std::istream &in, feature_ccv &feat)
{

        std::string h;
        size_t len = strlen(head);
        size_t i;

        for (i = 0; i < len && in; i++) {
                char c;
                in.read(&c, sizeof(c));
                h.push_back(c);
        }

        if (i < len || ! in)
                throw error_read_ccv();


        uint32_t dim;

        in.read((char*)&dim, sizeof(dim));

        if (dim != NUM_CCV_COLOR * 2)
                throw error_read_ccv();

        uint32_t j;
        for (j = 0; j < NUM_CCV_COLOR; j++)
                in.read((char*)&feat.m_alpha[j], sizeof(float));

        for (j = 0; j < NUM_CCV_COLOR; j++)
                in.read((char*)&feat.m_beta[j], sizeof(float));

        return in;
}

}
