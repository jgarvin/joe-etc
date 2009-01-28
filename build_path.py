#!/usr/bin/python3

"""User script to go through my /opt folder and add anything with a bin
subdirectory to the current path"""

import os
import glob

def main():
    home_folder = os.getenv("HOME")
    opt_folder = home_folder + "/opt"

    print(opt_folder)

    bin_folders = glob.glob(opt_folder + "/*/bin")

    new_path = old_path = os.getenv("PATH")
    for folder in bin_folders:
        new_path = folder + ":" + new_path

    print(new_path)

if __name__ == "__main__":
    main()
