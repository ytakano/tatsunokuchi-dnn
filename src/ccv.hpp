#ifndef CCV_HPP
#define CCV_HPP

#include <opencv/cv.h>

#define NUM_CCV_COLOR (3 * 3 * 3)

namespace dnn {

struct feature_ccv {
        float alpha[NUM_CCV_COLOR];
        float beta[NUM_CCV_COLOR];
};

// color coherence vector
void ccv(cv::Mat &src, feature_ccv &ret);

}

#endif // CCV_HPP
