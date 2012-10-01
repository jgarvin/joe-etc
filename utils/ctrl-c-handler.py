#!/usr/bin/env python

# We use this to prevent Ctrl-C from causing a traceback

try:
    import sys
    to_import = sys.argv[1]
    try:
        sys.argv = sys.argv[1:]
        eval ('__import__(\'%s\')' % to_import)
    except ImportError as e:
        print >>sys.stderr, "Error importing " + to_import
        print >>sys.stderr, e.message
except KeyboardInterrupt:
    pass
