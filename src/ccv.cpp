#include "ccv.hpp"

#include <stdint.h>

#include <opencv/highgui.h>

#include <vector>

#include <boost/shared_array.hpp>

namespace dnn {

static const char *head = "histgram";

class rgb {
public:
        uint8_t r, g, b;

        int
        toIndex() {
                return NUM_CCV_COLOR - 1 - (3 * 3 * r + 3 * g + b);
        }

        void
        fromIndex(int i) {
                b = i % 3;
                i -= b;

                g = i % (3 * 3);
                i -= g * 3;

                r = i / (3 * 3);
        }
};

void
ccv(cv::Mat &src, feature_ccv &ret)
{
        int x, y;
        int w = src.size().width;
        int h = src.size().height;
        int label;
        int tau = 4;
        bool flagLeft;
        bool flagRight;
        std::vector<rgb> labelColor;
        std::vector<int> labelArea;
        boost::shared_array<int> bufferNew(new int[w]);
        boost::shared_array<int> bufferOld(new int[w]);
        boost::shared_array<uint8_t> image(new uint8_t[3 * w * h]);
        int ch = src.channels();
        rgb c;
        int i;

        if (ch != 3)
                return;

        for (i = 0; i < NUM_CCV_COLOR; i++) {
                ret.alpha[i] = 0.0f;
                ret.beta[i]  = 0.0f;
        }

        for (i = 0; i < w * h; i++) {
                uint8_t r, g, b;

                r = src.data[i];
                g = src.data[i + 1];
                b = src.data[i + 2];

                r = r < 85 ? 0 : r < 170 ? 1 : 2;
                g = g < 85 ? 0 : g < 170 ? 1 : 2;
                b = b < 85 ? 0 : b < 170 ? 1 : 2;

                uint8_t *p = &image[i * 3];
                p[0] = r;
                p[1] = g;
                p[2] = b;
        }


        label = 0;
        c.r = image[0];
        c.g = image[1];
        c.b = image[2];
        labelColor.push_back(c);
        labelArea.push_back(1);
        bufferNew[0] = label;

        for (x = 1; x < w; x++) {
                if (image[x * 3 ]    == image[(x - 1) * 3] &&
                    image[x * 3 + 1] == image[(x - 1) * 3 + 1] &&
                    image[x * 3 + 2] == image[(x - 1) * 3 + 2]) {
                        bufferNew[x] = bufferNew[x - 1];
                        labelArea[bufferNew[x]]++;
                } else {
                        label++;

                        c.r = image[x * 3];
                        c.g = image[x * 3 + 1];
                        c.b = image[x * 3 + 2];

                        labelColor.push_back(c);
                        labelArea.push_back(1);
                        bufferNew[x] = label;
                }
        }

        for (y = 1; y < h; y++) {
                for (x = 0; x < w; x++) {
                        bufferOld[x] = bufferNew[x];
                }

                int idx1, idx2, idx3;

                for (x = 0; x < w; x++) {
                        idx1 = y * w * 3 + x * 3;
                        idx2 = (y - 1) * w * 3 + x * 3;
                        if (image[idx1 ]    == image[idx2] &&
                            image[idx1 + 1] == image[idx2 + 1] &&
                            image[idx1 + 2] == image[idx2 + 2]) {
                                bufferNew[x] = bufferOld[x];
                                labelArea[bufferNew[x]]++;
                        } else {
                                flagLeft  = false;
                                flagRight = false;

                                if (x > 0) {
                                        idx1 = y * w * 3 + x * 3;
                                        idx2 = (y - 1) * w * 3 + (x - 1) * 3;
                                        idx3 = y + w * 3 + (x - 1) * 3;
                                        if (image[idx1 ]    == image[idx2] &&
                                            image[idx1 + 1] == image[idx2 + 1] &&
                                            image[idx1 + 2] == image[idx2 + 2]) {
                                                bufferNew[x] = bufferOld[x - 1];
                                                labelArea[bufferNew[x]]++;
                                                flagLeft = true;
                                        } else if (image[idx1 ]    == image[idx3] &&
                                                   image[idx1 + 1] == image[idx3 + 1] &&
                                                   image[idx1 + 2] == image[idx3 + 2]) {
                                                bufferNew[x] = bufferNew[x - 1];
                                                labelArea[bufferNew[x]]++;
                                                flagLeft = true;
                                        }
                                }

                                if (x < w - 1) {
                                        idx1 = y * w * 3 + x * 3;
                                        idx2 = (y - 1) * w * 3 + (x + 1) * 3;
                                        if (image[idx1]     == image[idx2] &&
                                            image[idx1 + 1] == image[idx2 + 1] &&
                                            image[idx1 + 2] == image[idx2 + 2]) {
                                                if (! flagLeft) {
                                                        bufferNew[x] = bufferOld[x + 1];
                                                        labelArea[bufferNew[x]]++;
                                                } else if (bufferNew[x] != bufferOld[x + 1]) {
                                                        labelArea[bufferNew[x]] += labelArea[bufferOld[x + 1]];
                                                        labelArea[bufferOld[x + 1]] = 0;
                                                }
                                                flagRight = true;
                                        }
                                }

                                if (! flagLeft && ! flagRight) {
                                        label++;

                                        idx1 = y * w * 3 + x * 3;
                         
                                        c.r = image[idx1];
                                        c.g = image[idx1 + 1];
                                        c.b = image[idx1 + 2];

                                        labelColor.push_back(c);
                                        labelArea.push_back(1);
                                        bufferNew[x] = label;
                                }
                        }
                }
        }

        for (int i = 0; i <= label; i++) {
                if (labelArea[i] > 0) {
                        int idx;
                        if (labelArea[i] > tau) {
                                idx = labelColor[i].toIndex();
                                ret.alpha[idx] += labelArea[i];
                        } else {
                                idx = labelColor[i].toIndex();
                                ret.beta[idx] += labelArea[i];
                        }
                }
        }

        float size = w * h;
        for (int i = 0; i < NUM_CCV_COLOR; i++) {
                ret.alpha[i] = (float)ret.alpha[i] / size;
                ret.beta[i]  = (float)ret.beta[i] / size;
        }
}

bool
get_ccv_feat(const char *file, feature_ccv &feat)
{
        cv::Mat colorImage = cv::imread(file, 1);
        if(colorImage.empty())
                return false;

        ccv(colorImage, feat);

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
                out.write((char*)&feat.alpha[i], sizeof(float));

        for (i = 0; i < NUM_CCV_COLOR; i++)
                out.write((char*)&feat.beta[i], sizeof(float));

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
                in.read((char*)&feat.alpha[j], sizeof(float));

        for (j = 0; j < NUM_CCV_COLOR; j++)
                in.read((char*)&feat.beta[j], sizeof(float));

        return in;
}

}
