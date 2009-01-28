#!/usr/bin/python3

"""User script that generates a new path variable, by taking the current
path and adding any bin subfolders in subfolders of /opt"""

import os
import glob
import sys
import subprocess

def has_duplicates(x):
    return len(x) != len(set(x))

def any_redundant_folders(proposed_path):
    paths = proposed_path.split(":")
    return has_duplicates(paths)

def x_server_running():
    return os.getenv("DISPLAY") != None

def fail_with_error(error):
    if x_server_running():
        subprocess.Popen("xmessage " + error, shell=True)
    sys.exit(error)

def main():
    home_folder = os.getenv("HOME")
    opt_folder = home_folder + "/opt"

    bin_folders = glob.glob(opt_folder + "/*/bin")

    new_path = old_path = os.getenv("PATH")
    for folder in bin_folders:
        new_path = folder + ":" + new_path

    if any_redundant_folders(new_path):
        print(old_path)
        fail_with_error("Error, trying to add redundant bin folders from /opt!")

    print(new_path)

if __name__ == "__main__":
    main()
