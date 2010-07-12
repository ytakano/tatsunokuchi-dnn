#include "hog.hpp"

// COPYRIGHT
// http://d.hatena.ne.jp/poor_code/20091111/1257944372

#include <opencv/cv.h>
#include <opencv/highgui.h>

namespace dnn {

/*****************************
	HoG計算のパラメータ
******************************/
static const int CELL_X = 5;   //1セル内の横画素数
static const int CELL_Y = 5;   //1セル内の縦画素数
static const int CELL_BIN = 9; //輝度勾配方向の分割数（普通は９）
static const int BLOCK_X  = 3; //1ブロック内の横セル数
static const int BLOCK_Y  = 3; //1ブロック内の縦セル数

static const int RESIZE_X = 40;	//リサイズ後の画像の横幅
static const int RESIZE_Y = 40;	//リサイズ後の画像の縦幅

//以下のパラメータは、上の値から自動的に決まります

static const int CELL_WIDTH  = RESIZE_X / CELL_X;          //セルの数（横）
static const int CELL_HEIGHT = RESIZE_Y / CELL_Y;          //セルの数（縦）
static const int BLOCK_WIDTH  = CELL_WIDTH - BLOCK_X + 1;  //ブロックの数（横）
static const int BLOCK_HEIGHT = CELL_HEIGHT - BLOCK_Y + 1; //ブロックの数（縦）

static const int BLOCK_DIM = BLOCK_X * BLOCK_Y * CELL_BIN;  //１ブロックの特徴量次元
static const int TOTAL_DIM = BLOCK_DIM * BLOCK_WIDTH * BLOCK_HEIGHT; //HoG全体の次元

void get_hog_g(IplImage* src, float* feat);

histgram
get_hog_feat(const char *file)
{
        IplImage* img = cvLoadImage(file, 0);

        if (img == NULL)
                return histgram();

        histgram hist;

        hist.m_dim  = TOTAL_DIM;
        hist.m_hist = histgram::float_arr(new float[TOTAL_DIM]);

        get_hog_g(img, hist.m_hist.get());

        return hist;
}


/************************************
	HoG特徴量を計算
	img：グレースケール画像
	feat：計算された特徴量が入る
*************************************/
void
get_hog_g(IplImage* src, float* feat)
{

        //はじめに、画像サイズを変換（CELL_X/Yの倍数とする）
        IplImage* img = cvCreateImage(cvSize(RESIZE_X,RESIZE_Y), 8, 1);
        cvResize(src, img);

        //画像サイズ
        const int width = RESIZE_X;
        const int height = RESIZE_Y;

        //各セルの輝度勾配ヒストグラム
        float hist[CELL_WIDTH][CELL_HEIGHT][CELL_BIN];
        memset(hist, 0, CELL_WIDTH*CELL_HEIGHT*CELL_BIN*sizeof(float));

        //各ピクセルにおける輝度勾配強度mと勾配方向degを算出し、ヒストグラムへ
        //※端っこのピクセルでは、計算しない
        for(int y=0; y<height; y++){
                for(int x=0; x<width; x++){
                        if(x==0 || y==0 || x==width-1 || y==height-1){
                                continue;
                        }
                        float dx = img->imageData[y*img->widthStep+(x+1)] - img->imageData[y*img->widthStep+(x-1)];
                        float dy = img->imageData[(y+1)*img->widthStep+x] - img->imageData[(y-1)*img->widthStep+x];
                        float m = sqrt(dx*dx+dy*dy);
                        float deg = (atan2(dy, dx)+CV_PI) * 180.0 / CV_PI;	//0.0〜360.0の範囲になるよう変換
                        int bin = CELL_BIN * deg/360.0;
                        if(bin < 0) bin=0;
                        if(bin >= CELL_BIN) bin = CELL_BIN-1;
                        hist[(int)(x/CELL_X)][(int)(y/CELL_Y)][bin] += m;
                }
        }

        //ブロックごとで正規化
        for(int y=0; y<BLOCK_HEIGHT; y++){
                for(int x=0; x<BLOCK_WIDTH; x++){
		
                        //このブロックの特徴ベクトル（次元BLOCK_DIM=BLOCK_X*BLOCK_Y*CELL_BIN）
                        float vec[BLOCK_DIM];
                        memset(vec, 0, BLOCK_DIM*sizeof(float));
                        for(int j=0; j<BLOCK_Y; j++){
                                for(int i=0; i<BLOCK_X; i++){
                                        for(int d=0; d<CELL_BIN; d++){
                                                int index = j*(BLOCK_X*CELL_BIN) + i*CELL_BIN + d; 
                                                vec[index] = hist[x+i][y+j][d];
                                        }
                                }
                        }

                        //ノルムを計算し、正規化
                        float norm = 0.0;
                        for(int i=0; i<BLOCK_DIM; i++){
                                norm += vec[i]*vec[i];
                        }
                        for(int i=0; i<BLOCK_DIM; i++){
                                vec[i] /= sqrt(norm + 1.0);
                        }

                        //featに代入
                        for(int i=0; i<BLOCK_DIM; i++){
                                int index = y*BLOCK_WIDTH*BLOCK_DIM + x*BLOCK_DIM + i;
                                feat[index] = vec[i];
                        }
                }
        }
        cvReleaseImage(&img);
        return;
}

}
