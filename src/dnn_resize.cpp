#include <iostream>

#include <opencv/cv.h>
//#include <opencv/cxcore.h>
//#include <opencv/cvaux.h>
#include <opencv/highgui.h>

#define MAX_WIDTH  150
#define MAX_HEIGHT 150

void resize(const char *file, const char *thumb);

int
main( int argc, char** argv )
{
        bool is_src = true;

        std::string src;
        while (std::cin) {
                if (is_src) {
                        std::cin >> src;
                        is_src = false;
                } else {
                        std::string dst;
                        std::cin >> dst;

                        resize(src.c_str(), dst.c_str());

                        is_src = true;
                }
        }

        return 0;
}

void
resize(const char *file, const char *thumb)
{
        IplImage *img = NULL, *dst = NULL;

        img = cvLoadImage(file, 1);
        if (img == NULL)
                return;

        if (img->width > img->height) {
                double r = (double)MAX_WIDTH / (double)img->width;
                dst = cvCreateImage(cvSize(MAX_WIDTH, img->height * r),
                                    IPL_DEPTH_8U, 3);
        } else {
                double r = (double)MAX_HEIGHT / (double)img->height;
                dst = cvCreateImage(cvSize(img->width * r, MAX_HEIGHT),
                                    IPL_DEPTH_8U, 3);
        }

        cvResize(img,dst,CV_INTER_CUBIC); 

        cvSaveImage(thumb, dst);

        cvReleaseImage(&img);
        cvReleaseImage(&dst);
}
