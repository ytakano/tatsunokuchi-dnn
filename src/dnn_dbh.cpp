#include "dbh.hpp"
#include "hist.hpp"

#include <getopt.h>

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/foreach.hpp>

namespace fs = boost::filesystem;

struct option longopts[] = {
        {"help",        no_argument,       NULL, 'h'},
        {"create-conf", no_argument,       NULL, 'c'},
        {"read-conf",   required_argument, NULL, 'r'},
        {"output",      required_argument, NULL, 'o'},
        {"dir",         required_argument, NULL, 'd'},
        {NULL,          0,                 NULL, 0}
};

static void create_conf(std::vector<char*> &files, std::ostream &out);

static void
usage(char *progname)
{

}

int
main(int argc, char *argv[])
{
        std::vector<char*> files;
        bool  cflag, rflag;
        char *progname = argv[0];
        char *output   = NULL;
        char *conf     = NULL;
        char *dir      = NULL;
        int   ch;

        cflag = false;
        rflag = false;
        while ((ch = getopt_long(argc, argv, "hcr:o:d:", longopts,
                                 NULL)) != -1) {
                switch (ch) {
                case 'c':
                        cflag = true;
                        break;
                case 'o':
                        output = optarg;
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

        while (std::cin) {
        }

        return 0;
}

fs::path
get_dbh_path(const std::string &file, const char *dir)
{
        if (dir == NULL) {
                return fs::path(file);
        }

        fs::path path(fs::initial_path<fs::path>());
        path = dir / fs::system_complete(fs::path(file + ".dbh"));

        fs::path branch(path.branch_path());

        return path;
}

bool
read_hist(dnn::histgram &hist, const char *file)
{
        std::ifstream in(file);

        if (! in)
                return false;

        try {
                in >> hist;
        } catch (...) {
                return false;
        }

        return true;
}

void
create_conf(std::vector<char*> &files, std::ostream &out)
{
        dnn::dbh dbh;

        dbh.set_dim(1024);
        dbh.set_bits(32);

        BOOST_FOREACH(char* &c, files) {
                std::cerr << "loading \"" << c << "\"..." << std::endl;

                dnn::histgram hist;

                if (! read_hist(hist, c))
                        continue;

                if (hist.m_dim == 1024)
                        dbh.add_hist(hist.m_hist);
        }

        dbh.build_pivot();
}
