#include "kmeans.hpp"
#include "surf.hpp"

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

bool read_conf(dnn::kmeans &km, std::string conf);
void create_conf(std::vector<std::string> &files, std::ostream &out);
void create_hist(dnn::kmeans &km, const std::string &file, const char *dir);


int
main(int argc, char *argv[])
{
        dnn::kmeans km;
        std::string dir;

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

                po::options_description createhist("options for creating histgram files");

                createhist.add_options()
                        ("read-config,r", po::value<std::string>(),
                         "read a config file")
                        ("dir,d", po::value<std::string>(),
                         "directory to save histgram files.\n"
                         "histgram files are saved to same directory of original files if ommited");

                po::options_description hidden("Hidden options");
                hidden.add_options()
                        ("input-file", po::value< std::vector<std::string> >(),
                         "input file");

                po::options_description cmdline_options;
                cmdline_options.add(generic).add(createconf).add(createhist).add(hidden);

                po::positional_options_description p;
                p.add("input-file", -1);

                po::store(po::command_line_parser(argc, argv).
                          options(cmdline_options).positional(p).run(), vm);
                po::notify(vm);


                if (vm.count("help")) {
                        std::cout << generic << std::endl
                                  << createconf << std::endl
                                  << createhist;
                        return 0;
                }


                if (vm.count("create-config")) {
                        if (! vm.count("input-file")) {
                                std::cout << "error: no image files!"
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


                if (vm.count("read-config")) {
                        std::string conf;

                        conf = vm["read-config"].as<std::string>();

                        if (! read_conf(km, conf)) {
                                std::cerr << "failed to load \""
                                          << conf << "\"" << std::endl;
                                return -1;
                        }
                } else {
                        std::cout << createhist;
                        return -1;
                }


                if (vm.count("dir")) {
                        dir = vm["dir"].as<std::string>();
                }
        } catch (std::exception &e) {
                std::cout << e.what() << std::endl;
                return -1;
        }

        while (std::cin) {
                std::string str;
                std::cin >> str;

                if (dir.empty())
                        create_hist(km, str, NULL);
                else
                        create_hist(km, str, dir.c_str());
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
        dnn::features_t feat = dnn::get_surf_feat(file.c_str());

        if (feat.get() == NULL) {
                std::cout << "false" << std::endl;
                return;
        }


        dnn::histgram hist = km.get_hist(feat);

        fs::path path = get_hist_path(file, dir);

        if (dir != NULL)
                fs::create_directories(path.branch_path());

        fs::ofstream ofile(path);

        if (! ofile) {
                std::cout << "false" << std::endl;
                return;
        }

        ofile << hist;

        ofile.close();

        std::cout << path.string() << std::endl;
}

bool
read_conf(dnn::kmeans &km, std::string conf)
{
        std::ifstream ifile(conf.c_str());

        if (! ifile)
                return false;

        std::cerr << "building the k-means tree from \"" << conf << "\"..."
                  << std::endl;

        ifile >> km;

        std::cerr << "finished building" << std::endl;

        return true;
}

void
create_conf(std::vector<std::string> &files, std::ostream &out)
{
        dnn::kmeans km;

        km.set_dim(128);

        BOOST_FOREACH(std::string &c, files) {
                std::cerr << "loading \"" << c << "\"..." << std::endl;

                dnn::features_t feat = dnn::get_surf_feat(c.c_str());

                if (feat.get() != NULL)
                        km.add_features(feat);
        }

        std::cerr << "building the k-means tree..." << std::endl;
        km.build_tree();

        std::cerr << "writing..." << std::endl;
        out << km;
}
