#include <iostream>

#include <opencv/cv.h>
#include <opencv/highgui.h>

#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/path.hpp>

#define MAX_WIDTH  150
#define MAX_HEIGHT 150

namespace fs = boost::filesystem;

bool resize(const char *file, const char *thumb);

int
main( int argc, char** argv )
{
        bool is_src = true;

        std::string src;
        while (std::cin) {
                if (is_src) {
                        std::getline(std::cin, src, '\n');
                        is_src = false;
                } else {
                        std::string dst;
                        std::getline(std::cin, dst, '\n');

                        fs::path path(dst);

                        fs::create_directories(path.branch_path());

                        if (resize(src.c_str(), dst.c_str()))
                                std::cout << "true" << std::endl;
                        else
                                std::cout << "false" << std::endl;

                        is_src = true;
                }
        }

        return 0;
}

bool
resize(const char *file, const char *thumb)
{
        IplImage *img = NULL, *dst = NULL;

        img = cvLoadImage(file, 1);
        if (img == NULL)
                return false;

        if (img->width > img->height) {
                double r = (double)MAX_WIDTH / (double)img->width;
                dst = cvCreateImage(cvSize(MAX_WIDTH, img->height * r),
                                    IPL_DEPTH_8U, 3);
        } else {
                double r = (double)MAX_HEIGHT / (double)img->height;
                dst = cvCreateImage(cvSize(img->width * r, MAX_HEIGHT),
                                    IPL_DEPTH_8U, 3);
        }

        cvResize(img, dst, CV_INTER_CUBIC); 

        cvSaveImage(thumb, dst);

        cvReleaseImage(&img);
        cvReleaseImage(&dst);

        return true;
}
