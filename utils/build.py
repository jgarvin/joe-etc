#!/usr/bin/env python2.6

import os, sys
import os.path as op
import time
import re
from copy import copy

import argparse
from proc import run

# TODO: Install missing ubuntu build packages for each module type

parser = argparse.ArgumentParser(description="Build anything.")
parser.add_argument('-p', '--prefix', type=str, default=None,
                    dest="prefix", help='General install prefix.')
parser.add_argument(type=str, dest="source_archive",
                    help='Try to install this source archive.')

LOG_FILE_PATH = "/tmp/build_log"

args = parser.parse_args()

def fail(msg, code=1):
    print >>sys.stderr, msg
    sys.exit(code)

if args.prefix:
    args.prefix = op.abspath(args.prefix)
    os.putenv("PKG_CONFIG_PATH", op.join(args.prefix, "usr", "share", "pkgconfig"))
    pypath = [op.join(args.prefix, "lib", "python")]
    pypath.extend(sys.path)
    os.putenv("PYTHONPATH",  ":".join(pypath))
else:
    fail("You have to specify a prefix.")

source_archive = sys.argv[1]

decompress_method = None

# Use python logging module.
log_file = open(LOG_FILE_PATH, 'w')
def logrun(command, timeout=None):
    command_str = " ".join(command) + "\n"
    if timeout:
        logmsg(command_str  + (" TIMEOUT=%d" % timeout))
        (out, err, ret) = run(command, timeout)
    else:
        logmsg(command_str)
        (out, err, ret) = run(command)

    if out:
        logmsg("stdout: " + out)
    if err:
        logmsg("stderr: " + err)
    if ret:
        logmsg("Returned: %d" % ret)

    return (out, err, ret)

def logmsg(msg):
    log_file.write(time.asctime() + ": " + msg + "\n")

def extract_gzipped_tarball(tarball):
    logrun(["tar", "xvf", tarball])

if not op.exists(source_archive):
    fail(source_archive + " does not exist.")

if op.splitext(source_archive)[1] == ".gz":
    decompress_method = extract_gzipped_tarball
else:
    fail("Don't know how to extract " + source_archive)

decompress_method(source_archive)

def remove_extensions(some_file):
    return some_file.split('.')[0]

def find_and_install(project):
    (uname_output, unused, unused) = logrun(["uname", "-a"])
    package_command = None
    if "ubuntu" in uname_output.lower():
        package_command = "aptitude"
    elif "suse" in uname_output.lower():
        package_command = "zypper"
    else:
        # TODO: Fallback to google search or users passed in URLs
        fail("Don't know how to find packages for your unix.")

    (pkgs, unused, pkg_ret) = logrun([package_command, "search", project])
    if pkg_ret:
        fail("Error running package search command!")

    if not project in pkgs:
        # TODO: fallback
        fail("Couldn't find package for: " + project)

    (install_output, unused, install_ret) = logrun(["sudo", package_command, "install", project])
    if install_ret:
        fail("Error installing package for: " + project)

    return True # Success

def recover_from_known_errors(error_output):
    m = re.search("No package '(.+)' found", error_output)
    print m
    if not m:
        m = re.search("Failed to load the (.+ Python) package!", error_output)
    if m:
        pkg_name = m.group(1)
        if pkg_name.endswith("Python"):
            pkg_name = "python-" + pkg_name.split(" Python")[0]

        logmsg("You need: " + pkg_name)
        return find_and_install(pkg_name)

def try_recover(out, err):
    return recover_from_known_errors(err) or recover_from_known_errors(out)

def build_proj(folder):
    if os.getcwd() != folder:
        os.chdir(folder)

    if op.exists("bootstrap"):
        logrun(["sh", "bootstrap"])

    if op.exists("autogen.sh"):
        (conf_out, conf_err, conf_ret) = logrun(["./autogen.sh", "--prefix=" + args.prefix])
    elif op.exists("configure"):
        (conf_out, conf_err, conf_ret) = logrun(["./configure", "--prefix=" + args.prefix])
    else:
        # TODO: Some projects (e.g. dzen) are Makefile's only.
        fail("Don't know how to configure module!")

    if conf_ret != 0:
        if try_recover(conf_out, conf_err):
            return build_proj(folder)
        fail("Build failed for unknown reason! See log file %s" % LOG_FILE_PATH)

    if not op.exists("Makefile"):
        fail("Not sure what to do, no Makefile.")

    (build_out, build_err, build_ret) = logrun(["make"])
    if build_ret != 0:
        if try_recover(build_out, build_err):
            return build_proj(folder)
        fail("Make failed.")
    (install_out, install_err, install_ret) = logrun(["make install"])
    if install_ret:
        fail("Install failed.")

build_proj(op.join(os.getcwd(), remove_extensions(source_archive)))

# TODO: Need to pass autogen the configure arguments, since it invokes
# Need to run configure in bootstrap case
# Examine configure stderr to gett next package name
# Add trying to install missing package through package manager
# Allow passing in folders/URLs to find tarballs of dependencies at
# Allow passing prefix

# TEST:
# remove xcb-proto and python-xcbgen
# try to build xcb tarball

