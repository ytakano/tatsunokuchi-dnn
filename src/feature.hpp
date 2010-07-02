#ifndef FEATURE_HPP
#define FEATURE_HPP

#include <vector>

#include <boost/shared_ptr.hpp>

namespace dnn {

typedef boost::shared_ptr<std::vector<float> > features_t;

inline features_t
gen_features()
{
        return features_t(new std::vector<float>);
}

}

#endif // FEATURE_HPP
