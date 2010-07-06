#include "dbh.hpp"
#include "hist.hpp"

#include <fstream>
#include <iostream>

#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

void create_conf(std::vector<std::string> &files, std::ostream &out);
bool read_conf(dnn::dbh &dbh, std::string conf);
bool read_hist(dnn::histgram &hist, std::string file);
void create_dbh(dnn::dbh &dbh, const std::string &file, const char *dir);
fs::path get_dbh_path(const std::string &file, const char *dir);


int
main(int argc, char *argv[])
{
        try {
                po::variables_map vm;

                po::options_description generic("generic options");

                generic.add_options()
                        ("help,h", "show this help");

                po::options_description createconf("options for creating a config file");

                createconf.add_options()
                        ("create-config,c", "create config file")
                        ("output,o", po::value<std::string>(),
                         "name of a config file to be outputted.\n"
                         "configuration is outputted to the stdout if ommited");

                po::options_description createdbh("options for creating hashed files");

                createdbh.add_options()
                        ("read-config,r", po::value<std::string>(),
                         "read a config file")
                        ("dir,d", po::value<std::string>(),
                         "directory to save hashed files.\n"
                         "hashed files are saved to same directory of original files if ommited");

                po::options_description hidden("Hidden options");
                hidden.add_options()
                        ("input-file", po::value< std::vector<std::string> >(),
                         "input file");

                po::options_description cmdline_options;
                cmdline_options.add(generic).add(createconf).add(createdbh).add(hidden);

                po::positional_options_description p;
                p.add("input-file", -1);

                po::store(po::command_line_parser(argc, argv).
                          options(cmdline_options).positional(p).run(), vm);
                po::notify(vm);


                if (vm.count("help")) {
                        std::cout << generic << std::endl
                                  << createconf << std::endl
                                  << createdbh;
                        return 0;
                }


                if (vm.count("create-config")) {
                        if (! vm.count("input-file")) {
                                std::cout << "error: no histgram files!"
                                          << std::endl;
                                return -1;
                        }

                        std::string ofile;
                        if (vm.count("output")) {
                                ofile = vm["output"].as<std::string>();
                        }


                        std::vector<std::string> files;
                        files = vm["input-file"].as<std::vector<std::string> >();


                        if (ofile.empty()) {
                                create_conf(files, std::cout);
                        } else {
                                std::ofstream of(ofile.c_str());

                                if (of) {
                                        create_conf(files, of);
                                } else {
                                        std::cerr << "failed to open file: "
                                                  << ofile << std::endl;
                                        return -1;
                                }
                        }

                        return 0;
                }


                dnn::dbh dbh;

                if (vm.count("read-config")) {
                        std::string conf;

                        conf = vm["read-config"].as<std::string>();

                        if (! read_conf(dbh, conf)) {
                                std::cerr << "failed to load \""
                                          << conf << "\"" << std::endl;
                                return -1;
                        }
                } else {
                        std::cout << createdbh;
                        return -1;
                }


                std::string dir;
                if (vm.count("dir")) {
                        dir = vm["dir"].as<std::string>();
                }

                while (std::cin) {
                        std::string str;
                        std::cin >> str;

                        if (dir.empty())
                                create_dbh(dbh, str, NULL);
                        else
                                create_dbh(dbh, str, dir.c_str());
                }
        } catch (...) {
                return -1;
        }

        return 0;
}

void
create_dbh(dnn::dbh &dbh, const std::string &file, const char *dir)
{
        dnn::histgram hist;

        if (! read_hist(hist, file.c_str())) {
                std::cout << "false" << std::endl;
                return;
        }


        boost::shared_array<uint32_t> hash(new uint32_t[dbh.get_num_table()]);

        if (hist.m_dim != (uint32_t)dbh.get_dim())
                return;

        dbh.get_hash(hash.get(), hist.m_hist);

        fs::path path = get_dbh_path(file, dir);

        if (dir != NULL)
                fs::create_directories(path.branch_path());


        fs::ofstream ofile(path);

        if (! ofile) {
                std::cout << "false" << std::endl;
                return;
        }

        const char *type = "dbh";

        ofile.write(type, 4);

        for (int i = 0; i < dbh.get_num_table(); i++)
                ofile.write((char*)&hash[i], sizeof(uint32_t));

        ofile.close();

        std::cout << path.string() << std::endl;
}

bool
read_conf(dnn::dbh &dbh, std::string conf)
{
        std::ifstream ifile(conf.c_str());

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
read_hist(dnn::histgram &hist, std::string file)
{
        std::ifstream in(file.c_str());

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
create_conf(std::vector<std::string> &files, std::ostream &out)
{
        dnn::dbh dbh;

        dbh.set_dim(1024);
        dbh.set_bits(32);

        BOOST_FOREACH(const std::string &c, files) {
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
