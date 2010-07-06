#include "lshforest.hpp"

#include <stdint.h>

#include <iostream>

#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/foreach.hpp>
#include <boost/program_options.hpp>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

static const char *op_add = "add";
static const char *op_del = "del";
static const char *op_get = "get";

enum sim_state {
        SIM_OP,
        SIM_ADD_STR,
        SIM_ADD_HIST,
        SIM_DEL,
        SIM_GET
};

sim_state      state = SIM_OP;
dnn::lshforest forest;

int
main(int argc, char *argv[])
{
        try {
                po::variables_map vm;

                po::options_description generic("options");

                generic.add_options()
                        ("help,h", "show this help")
                        ("trees,t", po::value<uint32_t>(),
                         "the number of trees");

                po::options_description cmdline_options;
                cmdline_options.add(generic);

                po::store(po::command_line_parser(argc, argv).
                          options(cmdline_options).run(), vm);
                po::notify(vm);

                if (vm.count("help")) {
                        std::cout << generic << std::endl;
                        return 0;
                }

        } catch (...) {
                return -1;
        }

        return 0;
}
