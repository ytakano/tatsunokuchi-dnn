#include "hash_t.hpp"
#include "hist.hpp"
#include "lshforest.hpp"

#include <stdint.h>

#include <fstream>
#include <iostream>

#include <boost/foreach.hpp>
#include <boost/program_options.hpp>

namespace po = boost::program_options;

static const char *op_add = "add";
static const char *op_del = "del";
static const char *op_get = "get";
static const char *op_threshold = "threshold";

enum sim_state {
        SIM_OP,
        SIM_ADD_STR,
        SIM_ADD_HASH,
        SIM_ADD_HIST,
        SIM_DEL,
        SIM_GET_HASH,
        SIM_GET_FEAT,
        SIM_THRESHOLD,
};

sim_state      state = SIM_OP;
dnn::lshforest forest;

void read_op(std::string line);
void add_hash(std::string str, std::string hashfile, std::string histfile);
void get_similar(std::string hashfile, std::string histfile);


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

                if (vm.count("trees")) {
                        uint32_t trees = vm["trees"].as<uint32_t>();

                        forest.init(trees);
                } else {
                        std::cout << "error: the number of tree is required"
                                  << std::endl;
                        return -1;
                }
        } catch (std::exception &e) {
                std::cout << e.what() << std::endl;
                return -1;
        }

        std::string str1, str2;
        while (std::cin) {
                std::string line;
                std::getline(std::cin, line);

                if (line.empty())
                        continue;

                switch (state) {
                case SIM_OP:
                        read_op(line);
                        break;
                case SIM_ADD_STR:
                        str1  = line;
                        state = SIM_ADD_HASH;
                        std::cout << "input the hash file" << std::endl;
                        break;
                case SIM_ADD_HASH:
                        str2 = line;
                        state = SIM_ADD_HIST;
                        std::cout << "input the histgram file" << std::endl;
                        break;
                case SIM_ADD_HIST:
                        add_hash(str1, str2, line);
                        state = SIM_OP;
                        break;
                case SIM_DEL:
                        forest.remove_hash(line);
                        std::cout << "true" << std::endl;
                        state = SIM_OP;
                        break;
                case SIM_GET_HASH:
                        str1  = line;
                        state = SIM_GET_FEAT;
                        std::cout << "input the histgram file" << std::endl;
                        break;
                case SIM_GET_FEAT:
                        get_similar(str1, line);
                        state = SIM_OP;
                        break;
                case SIM_THRESHOLD:
                        try {
                                forest.set_threshold(boost::lexical_cast<float>(line));
                                std::cout << "true" << std::endl;
                        } catch (...) {
                                std::cout << "false" << std::endl;
                        }
                        state = SIM_OP;
                        break;
                }
        }

        return 0;
}

void
read_op(std::string line)
{
        if (line == op_add) {
                state = SIM_ADD_STR;
                std::cout << "input string" << std::endl;
        } else if (line == op_del) {
                state = SIM_DEL;
                std::cout << "input string" << std::endl;
        } else if (line == op_get) {
                state = SIM_GET_HASH;
                std::cout << "input the hash file" << std::endl;
        } else if (line == op_threshold) {
                state = SIM_THRESHOLD;
                std::cout << "input the threshold" << std::endl;
        }

}

void
add_hash(std::string str, std::string hashfile, std::string histfile)
{
        std::ifstream ifs(hashfile.c_str());
        dnn::hash_t   hash;

        if (! ifs) {
                std::cout << "false" << std::endl;
                return;
        }

        try {
                ifs >> hash;
        } catch (...) {
                std::cout << "false" << std::endl;
                return;
        }

        if (! forest.add_hash(str, histfile, hash)) {
                std::cout << "false" << std::endl;
                return;
        }

        std::cout << "true" << std::endl;
}

void
get_similar(std::string hashfile, std::string histfile)
{
        std::ifstream ifhash(hashfile.c_str());
        std::ifstream ifhist(histfile.c_str());
        dnn::hash_t   hash;
        dnn::histgram hist;

        if (! ifhash || ! ifhist) {
                std::cout << "." << std::endl;
                return;
        }

        try {
                ifhash >> hash;
        } catch (...) {
                std::cout << "." << std::endl;
                return;
        }

        try {
                ifhist >> hist;
        } catch (...) {
                std::cout << "." << std::endl;
                return;
        }

        std::vector<std::string> strset;

        forest.get_similar(strset, hash, hist);

        BOOST_FOREACH(std::string s, strset) {
                std::cout << s << std::endl;

                while (std::cin) {
                        std::string line;
                        std::getline(std::cin, line);

                        if (line == "next")
                                break;
                }
        }

        std::cout << "." << std::endl;
}
