#include "ccv.hpp"

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

void create_hist(const std::string &file, const char *dir);


int
main(int argc, char *argv[])
{
        std::string dir;

        try {
                po::variables_map vm;

                po::options_description generic("generic options");

                generic.add_options()
                        ("help,h", "show this help")
                        ("dir,d", po::value<std::string>(),
                         "directory to save histogram files.\n"
                         "histogram files are saved to same directory of original files if ommited");

                po::options_description cmdline_options;
                cmdline_options.add(generic);

                po::store(po::command_line_parser(argc, argv).
                          options(cmdline_options).run(), vm);
                po::notify(vm);


                if (vm.count("help")) {
                        std::cout << generic << std::endl;
                        return 0;
                }

                if (vm.count("dir")) {
                        dir = vm["dir"].as<std::string>();
                }
        } catch (std::exception &e) {
                std::cout << e.what() << std::endl;
        }

        while (std::cin) {
                std::string line;
                std::getline(std::cin, line);

                if (dir.empty())
                        create_hist(line, NULL);
                else
                        create_hist(line, dir.c_str());
        }

        return 0;
}

fs::path
get_hist_path(const std::string &file, const char *dir)
{
        if (dir == NULL) {
                return fs::path(file + ".ccv.hist");
        }

        fs::path path(fs::initial_path<fs::path>());
        path = dir / fs::system_complete(fs::path(file + ".ccv.hist"));

        fs::path branch(path.branch_path());

        return path;
}

void
create_hist(const std::string &file, const char *dir)
{
        dnn::feature_ccv feat;

        if (! dnn::get_ccv_feat(file.c_str(), feat)) {
                std::cout << "false" << std::endl;
                return;
        }


        fs::path path = get_hist_path(file, dir);

        if (dir != NULL)
                fs::create_directories(path.branch_path());

        fs::ofstream ofile(path);

        if (! ofile) {
                std::cout << "false" << std::endl;
                return;
        }

        ofile << feat;

        ofile.close();

        std::cout << path.string() << std::endl;
}
