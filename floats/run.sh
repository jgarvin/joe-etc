#!/usr/bin/env bash

set -e
set -o pipefail

set -x

# Conclusion: round tripping floats through CSV is lossless for all of C++ iostreams, pandas, numpy

g++ -g --std=c++11 csv_float_roundtrip_test.cpp
./a.out 123 1 dataset.csv # generate original data set, compare against original C++ in process
python csv_roundtrip.py dataset.csv dataset.pandas.csv dataset.numpy.csv
./a.out 123 0 dataset.pandas.csv # compare against original
./a.out 123 0 dataset.numpy.csv # compare against original
