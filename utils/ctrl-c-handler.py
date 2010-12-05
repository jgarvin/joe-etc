#!/usr/bin/env python

# We use this to prevent Ctrl-C from causing a traceback

try:
    import sys
    to_import = sys.argv[1]
    try:
        sys.argv = sys.argv[1:]
        eval ('__import__(\'%s\')' % to_import)
    except ImportError:
        print >>sys.stderr, "Couldn't find " + to_import
except KeyboardInterrupt:
    pass
