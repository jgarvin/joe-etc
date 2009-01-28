#!/usr/bin/python3

"""User script that generates a new path variable, by taking the current
path and adding any bin subfolders in subfolders of /opt"""

import os
import glob
import sys
import subprocess

def find_duplicates(x):
    unique_list = set(x)
    dup_list = [i for i in unique_list if x.count(i) > 1]

    return dup_list

def any_redundant_folders(proposed_paths):
    return find_duplicates(proposed_paths)

def any_redundant_executables(proposed_paths):
    files = []
    for path in proposed_paths:
        files += glob.glob(path + "/*")

    # Strip off leading part of path
    files = [bin.split("/")[-1] for bin in files]

    return find_duplicates(files)

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

    old_path_string = os.getenv("PATH")

    # Only check for redundancy within ~/opt, because the default install
    # of ubuntu already has some redundant bin entries
    duplicates = any_redundant_executables(bin_folders)
    if duplicates:
        print(old_path_string)
        fail_with_error("Error: redundant bins: " + str(duplicates))

    new_paths = bin_folders + old_path_string.split(":")

    duplicates = any_redundant_folders(new_paths)
    if duplicates:
        print(old_path_string)
        fail_with_error("Error: redundant bin folders: " + str(duplicates))

    final_path_string = ":".join(new_paths)
    print(final_path_string)

if __name__ == "__main__":
    main()
