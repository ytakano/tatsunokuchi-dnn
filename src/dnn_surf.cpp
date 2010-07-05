#include "kmeans.hpp"
#include "surf.hpp"

#include <getopt.h>

#include <fstream>
#include <iostream>

#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>

namespace fs = boost::filesystem;

struct option longopts[] = {
        {"help",        no_argument,       NULL, 'h'},
        {"create-conf", no_argument,       NULL, 'c'},
        {"read-conf",   required_argument, NULL, 'r'},
        {"output",      required_argument, NULL, 'o'},
        {"dir",         required_argument, NULL, 'd'},
        {NULL,          0,                 NULL, 0}
};

bool read_conf(dnn::kmeans &km, char *conf);
void create_conf(std::vector<char*> &files, std::ostream &out);
void create_hist(dnn::kmeans &km, const std::string &file, const char *dir);


void
usage(char *progname)
{
        std::cout << progname << " --create-conf img1.jpg img2.png\n"
                  << progname << " -c img1.jpg img2.png\n"
                  << "    the config file created by using "
                  << "img1.jpg and img2.jpg is outputted to the\n"
                  << "    standard output\n"
                  << std::endl;

        std::cout << progname
                  << " --create-conf --output surf.conf img1.jpg img2.png\n"
                  << progname << " -c --output surf.conf img1.jpg img2.png\n"
                  << "    the config file is outputted to surf.conf\n"
                  << std::endl;

        std::cout << progname << " --read-conf surf.conf\n"
                  << progname << " -r surf.conf\n"
                  << "    read file names from the standard input and create "
                  << "histgram files\n"
                  << "    the histgram files are outputted on the same "
                  << "directory of it\n"
                  << "    the config file must be specified to create histgrams"
                  << "\n"
                  << std::endl;

        std::cout << progname << " --read-conf surf.conf --dir /hist/to/sotre\n"
                  << progname << " -r surf.conf -d /hist/to/store\n"
                  << "    histgram files are created in the /hist/to/store"
                  << "directory\n"
                  << std::endl;

        std::cout << progname << " --help\n"
                  << progname << " -h\n"
                  << "    show this help"
                  << std::endl;
}

int
main(int argc, char *argv[])
{
        std::vector<char*> files;
        char *progname = argv[0];
        char *output = NULL;
        char *conf   = NULL;
        char *dir    = NULL;
        bool cflag, rflag;
        int  ch;

        cflag = false;
        rflag = false;
        while ((ch = getopt_long(argc, argv, "hcr:o:d:", longopts,
                                 NULL)) != -1) {
                switch (ch) {
                case 'c':
                        cflag = true;
                        break;
                case 'r':
                        rflag = true;
                        conf  = optarg;
                        break;
                case 'o':
                        output = optarg;
                        break;
                case 'd':
                        dir = optarg;
                        break;
                default:
                        usage(progname);
                        return -1;
                }
        }

        if (optind < argc) {
                while (optind < argc)
                        files.push_back(argv[optind++]);
        }

        if (cflag) {
                if (output == NULL) {
                        create_conf(files, std::cout);
                } else {
                        std::ofstream ofile(output);

                        if (ofile) {
                                create_conf(files, ofile);
                        } else {
                                std::cerr << "failed to open file: "
                                          << output << std::endl;
                                return -1;
                        }
                }

                return 0;
        }

        dnn::kmeans km;

        if (rflag) {
                if (! read_conf(km, conf)) {
                        std::cerr << "failed to load the config file of \""
                                  << conf << "\"" << std::endl;
                        return -1;
                }
        } else {
                usage(progname);
                return -1;
        }

        while (std::cin) {
                std::string str;
                std::cin >> str;

                create_hist(km, str, dir);
        }

        return 0;
}

fs::path
get_hist_path(const std::string &file, const char *dir)
{
        if (dir == NULL) {
                return fs::path(file + ".surf.hist");
        }

        fs::path path(fs::initial_path<fs::path>());
        path = dir / fs::system_complete(fs::path(file + ".surf.hist"));

        fs::path branch(path.branch_path());

        return path;
}

void
create_hist(dnn::kmeans &km, const std::string &file, const char *dir)
{
        dnn::features_t feat = dnn::get_features(file.c_str());

        if (feat.get() == NULL)
                return;


        dnn::histgram hist = km.get_hist(feat);

        fs::path path = get_hist_path(file, dir);

        if (dir != NULL)
                fs::create_directories(path.branch_path());

        fs::ofstream ofile(path);

        if (! ofile)
                return;

        ofile << hist;

        ofile.close();

        std::cout << path.string() << std::endl;
}

bool
read_conf(dnn::kmeans &km, char *conf)
{
        std::ifstream ifile(conf);

        if (! ifile)
                return false;

        std::cout << "building the k-means tree from \"" << conf << "\"..."
                  << std::endl;

        ifile >> km;

        std::cout << "finished building" << std::endl;

        return true;
}

void
create_conf(std::vector<char*> &files, std::ostream &out)
{
        dnn::kmeans km;

        km.set_dim(128);

        BOOST_FOREACH(char* &c, files) {
                std::cerr << "loading \"" << c << "\"..." << std::endl;

                dnn::features_t feat = dnn::get_features(c);

                if (feat.get() != NULL)
                        km.add_features(feat);
        }

        std::cerr << "building the k-means tree..." << std::endl;
        km.build_tree();

        std::cerr << "wrting..." << std::endl;
        out << km;
}
