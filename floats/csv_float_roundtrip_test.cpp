#include <iostream>
#include <vector>
#include <random>
#include <limits>
#include <cstring>
#include <fstream>
#include <iomanip>

int main(int argc, char** argv)
{
    int seed = atoi(argv[1]);
    int make_output = atoi(argv[2]);
    std::string file = argv[3];

    std::cout << std::fixed << std::setprecision(std::numeric_limits<float>::max_digits10);
    std::cerr << std::fixed  << std::setprecision(std::numeric_limits<float>::max_digits10);

    // std::random_device rd{};
    std::mt19937 gen{seed};
    std::normal_distribution<float> d{1000000,100};

    std::vector<float> x;
    std::vector<float> y;
    std::vector<float> difference;

    const auto TEST_SIZE = 1000;

    for(int i = 0; i < TEST_SIZE; ++i) {
        x.emplace_back(d(gen));
        y.emplace_back(d(gen));
        difference.emplace_back(y.back() - x.back());
    }

    // output CSV
    if(make_output)
    {
        std::ofstream o(file, std::ofstream::out);
        if(!o.good()) {
            return 1;
        }
        o << std::fixed; // doesn't matter just makes output consistent
        o << std::setprecision(std::numeric_limits<double>::max_digits10); // matters!!!

        o << "x,y,d\n";

        for(int i = 0; i < TEST_SIZE; ++i) {
            o << x[i] << "," << y[i] << "," << difference[i] << "\n";
        }

        o.close();
    }

    std::ifstream i(file, std::ifstream::in);
    if(!i.good()) {
        return 1;
    }

    std::vector<float> ix;
    std::vector<float> iy;
    std::vector<float> idifference;
    char ignore;
    bool read_header = false;
    while(i.good()) {
        if(!read_header) {
            std::string unused;
            std::getline(i, unused);
            read_header = true;
            continue;
        }
        float x = std::numeric_limits<float>::quiet_NaN();
        i >> x;
        i >> ignore; // skip comma
        float y = std::numeric_limits<float>::quiet_NaN();
        i >> y;
        i >> ignore; // skip comma
        float d = std::numeric_limits<float>::quiet_NaN();
        i >> d;

        if(i.eof()) {
            // happens when the file has a ending newline,
            // reading the floats above will have silently failed
            break;
        }

        ix.emplace_back(x);
        iy.emplace_back(y);
        idifference.emplace_back(d);
    }

    i.close();

    for(int i = 0; i < TEST_SIZE; ++i) {
        if(x[i] != ix[i]) {
            std::cerr << "x's index " << i << " doesn't match: " << x[i] << " " << ix[i] << std::endl;
        }
    }

    for(int i = 0; i < TEST_SIZE; ++i) {
        if(y[i] != iy[i]) {
            std::cerr << "y's index " << i << " doesn't match: " << y[i] << " " << iy[i] << std::endl;
        }
    }

    for(int i = 0; i < TEST_SIZE; ++i) {
        if(difference[i] != idifference[i]) {
            std::cerr << "difference's index " << i << " doesn't match: " << difference[i] << " " << idifference[i] << std::endl;
        }
    }

    if(std::memcmp(&x[0], &ix[0], TEST_SIZE*sizeof(float)) != 0) {
        std::cerr << "x's don't match!" << std::endl;
    }

    if(std::memcmp(&y[0], &iy[0], TEST_SIZE*sizeof(float)) != 0) {
        std::cerr << "y's don't match!" << std::endl;
    }

    if(std::memcmp(&difference[0], &idifference[0], TEST_SIZE*sizeof(float)) != 0) {
        std::cerr << "d's don't match!" << std::endl;
    }

    return 0;
}
