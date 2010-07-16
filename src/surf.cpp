#include "surf.hpp"

#include <iostream>
#include <string>

#include <opencv/cv.h>
#include <opencv/highgui.h>

namespace dnn {

static const int surf_width  = 240;
static const int surf_height = 160;

features_t
get_surf_feat(const char *file)
{
        // (1)load Color Image
        cv::Mat colorImage = cv::imread(file, 1);
        if(colorImage.empty())
                return features_t();

        cv::Mat resized;
        cv::resize(colorImage, resized, cv::Size(surf_width, surf_height));

        colorImage.release();

        // (2)convert Color Image to Grayscale for Feature Extraction
        cv::Mat grayImage;
        cv::cvtColor(resized, grayImage, CV_BGR2GRAY);

        if (grayImage.empty())
                return features_t();

        resized.release();

        // (3)initialize SURF class
        cv::SURF calc_surf = cv::SURF(500, 4, 2, true);

        // (4)extract SURF
        std::vector<cv::KeyPoint> kp_vec;
        dnn::features_t desc_vec = dnn::gen_features();

        calc_surf(grayImage, cv::Mat(), kp_vec, *desc_vec);

        return desc_vec;
}

}
