#ifndef CCV_HPP
#define CCV_HPP

#include <opencv/cv.h>

#include <boost/shared_array.hpp>

#define NUM_CCV_COLOR (4 * 4 * 4)

namespace dnn {

struct feature_ccv {
        typedef boost::shared_array<float> float_arr;
        float_arr m_alpha;
        float_arr m_beta;
};

class error_read_ccv { };

// colour coherence vector
void ccv(const cv::Mat &src, feature_ccv &ret);
bool get_ccv_feat(const char *file, feature_ccv &feat);

std::ostream& operator<< (std::ostream &out, const feature_ccv &feat);
std::istream& operator>> (std::istream &in, feature_ccv &feat);

}

#endif // CCV_HPP
