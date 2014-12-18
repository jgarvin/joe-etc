#!/usr/bin/env python

import os
import os.path as op
import argparse
import datetime
import sys
import sh

app_description = ("Backup a given directory with tarsnap.")

parser = argparse.ArgumentParser(description=app_description)
parser.add_argument('-n', '--name', metavar='NAME', type=str, required=True,
                    dest="name", help='Backup name, date will be appended.')
parser.add_argument(metavar='DIR', type=str, dest="dir",
                    help='Directory to backup')
args = parser.parse_args()

curDateStr = datetime.datetime.now().strftime("%Y.%m.%d")

backupName = args.name + "-" + curDateStr 
print "Backup name: " + backupName

backupDateFile = op.join(os.environ["HOME"], ".backup-" + args.name + ".log") 

if op.exists(backupDateFile):
    f = open(backupDateFile)
    contents = f.read()
    if curDateStr in contents:
        print >> sys.stderr, "Already made backup for today."
        sys.exit(0)
    f.close()
    print "No backup for today, making backup."
else:
    print "No previous backup detected, making backup"

f = open(backupDateFile, 'w')
f.write(curDateStr)
f.close()    

backupDir = op.basename(args.dir)

gitIgnorePath = []
tooLarge = set()
executable = set()
for root, dirs, files in os.walk(args.dir):
    for filename in files:
        if filename == ".gitignore":
            gitIgnorePath.append(root)
        path = op.join(root, filename)
        print path
        if op.islink(path):
            # Without this check the stat below will fail
            # when we have a bad symlink, like the .# kind emacs
            # creates deliberately to lock files.
            continue
        filesize = os.stat(path).st_size
        relpath = op.relpath(path, args.dir)
        if filesize > 1024*800: # 800KB
            # PDFs are ok, they maybe big
            ext = op.splitext(path)[-1] 
            if ext != ".pdf" and ext != ".pack":
                tooLarge.add(relpath)
        fileinfo = sh.file(path)
        if "ELF" in fileinfo and "executable" in fileinfo:
            executable.add(relpath)

if len(gitIgnorePath) < 1:
    print >> sys.stderr, "Couldn't find git repo."
    sys.exit(1)
elif len(gitIgnorePath) > 1:
    print >> sys.stderr, "Found more than one .gitignore, using least nested for filtering."
    gitIgnorePath = [min(gitIgnorePath, key=lambda x: op.normpath(x).count(os.sep))]

print ".gitignore path: " + op.join(gitIgnorePath[0], ".gitignore")

# Figure out which files git will ignore
os.chdir(gitIgnorePath[0])
ignored_files = sh.cut(sh.git("ls-files", "--others", "-i", "--exclude-standard"), "-d ", "-f3-").stdout.split("\n")
relative_git_path = op.relpath(gitIgnorePath[0], args.dir)
ignored_set = set()
for f in ignored_files:
    if f: # Usually one empty newline to filter
        ignored_set.add(op.join(relative_git_path, f).lstrip("./"))
print "Ignored files: " + str(ignored_set)
print "Ignored executables: " + str(executable)

print "Too large before: " + str(tooLarge)
tooLarge -= ignored_set
tooLarge -= executable
print "Too large after: " + str(tooLarge)

doExit = False
while tooLarge:
    print "File is too large!: " + tooLarge.pop()
    doExit = True

if doExit:
    sys.exit(1)
 
#tarsnap_args = ["--dry-run"]
tarsnap_args = []
tarsnap_args.append("-c")
tarsnap_args.append("-v")
for f in (ignored_set | executable):
    tarsnap_args.append("--exclude")
    tarsnap_args.append(f)
tarsnap_args.append("-f")
tarsnap_args.append(backupName)
tarsnap_args.append(backupDir)

# Have to move to the directory since tar uses relative paths
os.chdir(op.dirname(args.dir))
sh.tarsnap(*tarsnap_args, _err=sys.stderr, _out=sys.stdout)

