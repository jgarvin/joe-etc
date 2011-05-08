# Originally from: http://stackoverflow.com/questions/1556348/python-run-a-process-with-timeout-and-capture-stdout-stderr-and-exit-status
# Written by: flybywire

import subprocess
import time
import sys

class Timeout(Exception):
    pass

def run(command, timeout=None):
    proc = subprocess.Popen(command, bufsize=0, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    poll_seconds = .250
    if timeout:
        deadline = time.time()+timeout
        while time.time() < deadline and proc.poll() == None:
            time.sleep(poll_seconds)
    else:
        proc.wait()

    if proc.poll() == None:
        if float(sys.version[:3]) >= 2.6:
            proc.terminate()
        raise Timeout()

    stdout, stderr = proc.communicate()
    return stdout, stderr, proc.returncode

if __name__=="__main__":
    print run(["ls", "-l"])
    print run(["find", "/"], timeout=3) #should timeout
