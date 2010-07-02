#include "surf.hpp"

#include <iostream>
#include <string>

#include <opencv/cv.h>
#include <opencv/highgui.h>

namespace dnn {

features_t
get_features(const char *file)
{
        // (1)load Color Image
        cv::Mat colorImage = cv::imread(file, 1);
        if(colorImage.empty())
                return features_t();

        // (2)convert Color Image to Grayscale for Feature Extraction
        cv::Mat grayImage;
        cv::cvtColor(colorImage, grayImage, CV_BGR2GRAY);

        // (3)initialize SURF class
        cv::SURF calc_surf = cv::SURF(500, 4, 2, true);

        // (4)extract SURF
        std::vector<cv::KeyPoint> kp_vec;
        dnn::features_t desc_vec = dnn::gen_features();

        calc_surf(grayImage, cv::Mat(), kp_vec, *desc_vec);

        return desc_vec;
}

}

#if 0
#include "kmeans.hpp"

using namespace std;
using namespace cv;


int main(int argc, char *argv[])
{
        // (1)load Color Image
        if (argc <= 1) {
                cerr << "usage: " << argv[0] << " image" << endl;
                return -1;
        }
        const char *imagename = argv[1];
        Mat colorImage = imread(imagename, 1);
        if(colorImage.empty())
                return -1;

        // (2)convert Color Image to Grayscale for Feature Extraction
        Mat grayImage;
        cvtColor(colorImage, grayImage, CV_BGR2GRAY);

        // (3)initialize SURF class
        SURF calc_surf = SURF(500, 4, 2, true);

        // (4)extract SURF
        vector<KeyPoint> kp_vec;
        dnn::features_t desc_vec = dnn::gen_features();
        calc_surf(grayImage, Mat(), kp_vec, *desc_vec);
        dnn::kmeans km;

        km.set_dim(128);
        km.add_features(desc_vec);
        km.build_tree();

        dnn::kmeans::histgram hist;
        hist = km.get_hist(desc_vec);
/*
        for (int i = 0; i < hist.m_dim; i++) {
                cout << hist.m_hist[i] << std::endl;
        }
*/

        std::cout << km << std::endl;

/*
        // (5)draw keypoints
        cout << "Image Keypoints: " << kp_vec.size() << endl;
#if 1
        vector<KeyPoint>::iterator it = kp_vec.begin(), it_end = kp_vec.end();
        for(; it!=it_end; ++it) {
                circle(colorImage, Point(it->pt.x, it->pt.y), 
                       saturate_cast<int>(it->size*0.25), Scalar(255,255,0));
        }
#else
        for(int i = 0; i < kp_vec.size(); i++) {
                KeyPoint* point = &(kp_vec[i]);
                Point center;  // Key Point's Center
                int radius;      // Radius of Key Point
                center.x = cvRound(point->pt.x);
                center.y = cvRound(point->pt.y);
                radius = cvRound(point->size*0.25);
                circle(colorImage, center, radius, Scalar(255,255,0), 1, 8, 0);
        }
#endif
        //namedWindow("SURF",CV_WINDOW_AUTOSIZE);
        //imshow("SURF", colorImage);
        //waitKey(0);
        */

        return 0;
}

#endif
