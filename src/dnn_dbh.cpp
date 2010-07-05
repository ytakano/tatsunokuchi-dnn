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

void create_conf(std::vector<char*> &files, std::ostream &out);
bool read_conf(dnn::dbh &dbh, const char *conf);
bool read_hist(dnn::histgram &hist, const char *file);
void create_dbh(dnn::dbh &dbh, const std::string &file, const char *dir);
fs::path get_dbh_path(const std::string &file, const char *dir);


void
usage(char *progname)
{
        std::cout << progname << " --create-conf hist1.hist hist2.hist\n"
                  << progname << " -c hist1.hist hist2.hist\n"
                  << "    the config file created by using "
                  << "hist1.hist and hist2.hist is outputted to the\n"
                  << "    standard output\n"
                  << std::endl;

        std::cout << progname
                  << " --create-conf --output dbh.conf hist1.hist hist2.hist\n"
                  << progname << " -c --output hist.conf hist1.hist hist2.hist\n"
                  << "    the config file is outputted to hist.conf\n"
                  << std::endl;

        std::cout << progname << " --read-conf hist.conf\n"
                  << progname << " -r hist.conf\n"
                  << "    read file names from the standard input and create "
                  << "DBH files\n"
                  << "    the DBH files are outputted on the same "
                  << "directory of it\n"
                  << "    the config file must be specified to compute hash values"
                  << "\n"
                  << std::endl;

        std::cout << progname << " --read-conf surf.conf --dir /dbh/to/sotre\n"
                  << progname << " -r dbh.conf -d /dbh/to/store\n"
                  << "    DBH files are created in the /dbh/to/store"
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
                case 'h':
                        usage(progname);
                        return 0;
                case 'c':
                        cflag = true;
                        break;
                case 'o':
                        output = optarg;
                        break;
                case 'r':
                        rflag = true;
                        conf  = optarg;
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

        dnn::dbh dbh;

        if (rflag) {
                if (! read_conf(dbh, conf)) {
                        std::cerr << "failed to load the config file of \""
                                  << conf << "\"" << std::endl;
                        return -1;
                }
        } else {
                usage(progname);
                return -1;
        }

        while (std::cin) {
                std::string file;

                std::cin >> file;

                create_dbh(dbh, file, dir);
        }

        return 0;
}

void
create_dbh(dnn::dbh &dbh, const std::string &file, const char *dir)
{
        dnn::histgram hist;

        if (! read_hist(hist, file.c_str()))
                return;


        boost::shared_array<uint32_t> hash(new uint32_t[dbh.get_num_table()]);

        if (hist.m_dim != (uint32_t)dbh.get_dim())
                return;

        dbh.get_hash(hash.get(), hist.m_hist);

        fs::path path = get_dbh_path(file, dir);

        if (dir != NULL)
                fs::create_directories(path.branch_path());


        fs::ofstream ofile(path);

        if (! ofile)
                return;

        const char *type = "dbh";

        ofile.write(type, 4);

        for (int i = 0; i < dbh.get_num_table(); i++)
                ofile.write((char*)&hash[i], sizeof(uint32_t));

        ofile.close();

        std::cout << path.string() << std::endl;
}

bool
read_conf(dnn::dbh &dbh, const char *conf)
{
        std::ifstream ifile(conf);

        if (! ifile)
                return false;

        std::cout << "loading \"" << conf << "\"..."
                  << std::endl;

        ifile >> dbh;

        std::cout << "finished loading" << std::endl;

        return true;
}

fs::path
get_dbh_path(const std::string &file, const char *dir)
{
        if (dir == NULL) {
                return fs::path(file + ".dbh");
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

        out << dbh;
}
