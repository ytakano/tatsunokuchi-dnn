#ifndef CCV_HPP
#define CCV_HPP

#include <opencv/cv.h>

#define NUM_CCV_COLOR (3 * 3 * 3)

namespace dnn {

struct feature_ccv {
        float alpha[NUM_CCV_COLOR];
        float beta[NUM_CCV_COLOR];
};

class error_read_ccv { };

// color coherence vector
void ccv(cv::Mat &src, feature_ccv &ret);
feature_ccv get_ccv_feat(const char *file);

std::ostream& operator<< (std::ostream &out, const feature_ccv &feat);
std::istream& operator>> (std::istream &in, feature_ccv &feat);

}

#endif // CCV_HPP
