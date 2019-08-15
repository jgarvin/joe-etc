#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys

import pandas as pd
import numpy as np

def main():
    df = pd.read_csv(sys.argv[1])
    df.to_csv(sys.argv[2], index=False)
    data = np.genfromtxt(sys.argv[1], skip_header=1, delimiter=",")
    with open(sys.argv[3], "w") as f:
        f.write("x,y,d\n")
        np.savetxt(f, data, delimiter=",")

if __name__ == "__main__":
    main()