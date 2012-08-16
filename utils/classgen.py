#!/usr/bin/env python

"""
This script is evil.
"""

examplescript=(
"""
Field,Type,Ops
Seqnum,uint32_t,G:S:I
""")

funcmap = { 'G' : 'get',
            'S' : 'set',
            'I' : 'inc',
            'A' : 'add',
            'GM' : 'get' }

def getTypeSize(t):
    if "[" in t:
        return getTypeSize(t.split("[")[0]) * int(t.split("[")[1][:-1]) 
    elif "int64_t" in t:
        return 8
    elif "int32_t" in t:
        return 4
    elif "int16_t" in t:
        return 2
    elif "int8_t" in t:
        return 1
    elif "char":
        return 1

import sys
import argparse

app_description = ("Code smell generator.")
parser = argparse.ArgumentParser(description=app_description)
parser.add_argument('-n', '--namespace', metavar='NSPACE', type=str,
                    dest="namespace", help='Namespace for class.',
                    default=None)
parser.add_argument('-c', '--class', metavar='NAME', type=str,
                    dest="classname", help='Name for class', required=True)
parser.add_argument('-b', '--base', metavar='NAME', type=str, action='append',
                    dest="base", help='Public base class', default=None)
parser.add_argument('-t', '--template', metavar='PARAMS', type=str,
                    dest="tparams", help='Template parameters')
parser.add_argument('-r', '--ctorparams', metavar='PARAMS', type=str,
                    dest="ctorparams", help='Constructor parameters', default="")
parser.add_argument('-o', '--stdout', default=False, action='store_true',
                    dest="stdout", help='Don\'t save, print to stdout')
parser.add_argument(metavar='FILE', type=str,
                    dest="input_file", help='File to read field defs from.')
args = parser.parse_args()

it = ''
header_header_guard = []
if args.namespace:
    header_header_guard.append(args.namespace.upper())
header_header_guard.append(args.classname.upper())
header_header_guard.append('H')
header_header_guard = '_'.join(header_header_guard)

opening=(
"""#ifndef %s
#define %s

""")    

def genOpening(header_guard):
    return opening % (header_guard, header_guard)

fields=[]
input_file = args.input_file 
for line in open(input_file).readlines()[1:]:
    if len(line) and line[0] == '#': # comment syntax
        continue
    field, ftype, ops = line.split(',')
    fields.append((field, ftype, ops))

#fields.sort(key=lambda x: getTypeSize(x[1]), reverse=True)

header = genOpening(header_header_guard)
header += "#include <cstddef>\n\n"

if args.namespace:
    header += 'namespace %s {\n\n' % args.namespace
    it = '  '

if args.tparams:
    header += it + "template<%s>\n" % args.tparams

def returnType(t):
    if t.startswith("char["):
        return "char*"
    return t

def memberType(t):
    if t.find("[") != -1:
        return t.split("[")[0]
    return t

def fieldName(n):
    return n[0].lower() + n[1:] + "_"
            
header += it + 'class %s' % args.classname
if args.base:
    header += ' : '
    bFirst = True
    for base in args.base:
        if not bFirst:
            header += ", "
        header += 'public %s' % base
        bFirst = False
header += ' {\n'
header += it + 'public:\n'

header += it + '  ' + args.classname + '(' + args.ctorparams + ');\n\n'

lastField = fields[0][0]
for field in fields:
    for op in field[2].split(':'):
        op = op.strip()
        
        if lastField != field[0]:
            header += '\n'
        lastField = field[0]

        returnT = returnType(field[1])

        if op == 'G': # get
            if returnT == "char*":
                returnT = "const char*"
            header += it + '  %s get%s() const;\n' % (returnT, field[0])
        elif op == 'S': # set
            header += it + '  void set%s(%s %s);\n' % (field[0], returnT, fieldName(field[0])[:-1])
        elif op == 'I': # increment
            header += it + '  void inc%s();\n' % field[0]
        elif op == 'A': # add
            header += it + '  void add%s(%s delta);\n' % (field[0], returnT)
        elif op == 'GM': #
            header += it + '  %s get%s();\n' % (returnT, field[0])
        elif op == 'C':
            header += it + '  void clear%s();\n' % field[0]
        else:
            print >>sys.stderr, "Invalid op!: %s" % op
            sys.exit(1)

header += '\n' + it + 'private:\n'
for field in fields:
    typename = memberType(field[1])
    fieldname = fieldName(field[0])
    if field[1].find("[") != -1:
        fieldname += "[" + field[1].split("[")[1][:-1] + "]"
    header += it + '  %s %s;\n' % (typename, fieldname)
header += it + '};\n\n'

if args.namespace:
    header += '}\n\n'
    it = ''

header += '#include <%s/%sINLINES.C>\n\n' % (args.namespace, args.classname) 

header += "#endif\n"

inlines_header_guard = []
if args.namespace:
    inlines_header_guard.append(args.namespace.upper())
inlines_header_guard.append(args.classname.upper() + "INLINES")
inlines_header_guard.append('C')
inlines_header_guard = '_'.join(inlines_header_guard)
inlines = genOpening(inlines_header_guard)

inlines += '#include <%s/%s.H>\n\n' % (args.namespace, args.classname)

inlines += '#include <string.h>\n\n'

if args.namespace:
    inlines += 'namespace %s {\n\n' % args.namespace
    it = '  '

if args.tparams:
    classheader = args.classname + '<' + args.tparams.replace("class ", "") + '>\n'
else:
    classheader = args.classname + '\n' 

if args.tparams:
    inlines += it + 'template<%s>\n' % args.tparams
inlines += it + classheader[:-1] + '\n'
inlines += it + '::' + args.classname
inlines += '(' + args.ctorparams + ')\n'
firstField = True

if args.base:
    inlines += it
    params = [i.split(' ')[-1] for i in args.ctorparams.split(',')]
    params = [i.replace('*', '') for i in params] # depointerify
    params = [i.replace('&', '') for i in params] # dereferenceify
    params = ', '.join(params)
    inlines += ': ' + args.base[0] + '(' + params + ')\n'
    firstField = False

for field in fields:
    if field[1].find("[") != -1:
        continue

    inlines += it
    if firstField:
        inlines += ': '
        firstField = False
    else:
        inlines += ', '
    inlines += fieldName(field[0]) + '(0)'
    inlines += '\n'
inlines += it + '{\n'
for field in fields:
    if field[1].find("[") == -1:
        continue
    memberT = memberType(field[1])
    size = field[1].split("[")[1][:-1] 
    inlines += (it + '  memset(' + fieldName(field[0]) + ', 0, ' + 'sizeof(' +
                memberT + ')*' + size + ');\n')
inlines += it + '}'
inlines += '\n\n'

for field in fields:
    for op in field[2].split(':'):
        op = op.strip()
        
        returnT = returnType(field[1])

        if args.tparams:
            inlines += it + 'template<%s>\n' % args.tparams

        if op == 'G':
            if returnT == "char*":
                returnT = "const char*"
            inlines += it + "inline " + returnT + ' ' + classheader 
            inlines += it + '::get%s() const\n' % (field[0])
            inlines += it + '{\n'
            inlines += it + '  return %s;\n' % fieldName(field[0])
            inlines += it + '}'
        elif op == 'S':
            inlines += it + 'inline void ' + classheader 
            inlines += it + '::set%s(%s %s)\n' % (field[0], returnT, fieldName(field[0])[:-1])
            inlines += it + '{\n'
            inlines += it + '  %s = %s;\n' % (fieldName(field[0]), fieldName(field[0])[:-1])
            inlines += it + '}'
        elif op == 'I':
            inlines += it + 'inline void ' + classheader
            inlines += it + '::inc%s()\n' % field[0]
            inlines += it + '{\n'
            inlines += it + '  ++%s;\n' % (fieldName(field[0]))
            inlines += it + '}'
        elif op == 'A':
            inlines += it + 'inline void ' + classheader
            inlines += it + '::add%s(%s delta)\n' % (field[0], returnT)
            inlines += it + '{\n'
            inlines += it + '  %s += delta;\n' % (fieldName(field[0]))
            inlines += it + '}'
        elif op == 'GM':
            inlines += it + "inline " + returnT + ' ' + classheader
            inlines += it + '::get%s()\n' % (field[0])
            inlines += it + '{\n'
            inlines += it + '  return %s;\n' % fieldName(field[0])
            inlines += it + '}'
        elif op == 'C':
            inlines += it + 'inline void ' + classheader
            inlines += it + '::clear%s()\n' % field[0]
            inlines += it + '{\n'
            inlines += it + '  %s[0] = \'\\0\';\n' % (fieldName(field[0]))
            inlines += it + '}'
        else:
            print >>sys.stderr, "Invalid op!: %s" % op
            sys.exit(1)

        inlines += '\n\n'

if args.namespace:
    inlines += '}\n\n'
    it = ''

inlines += "#endif\n"

if args.stdout:
    print header
    print
    print inlines
else:
    open(args.classname + '.H', 'w').write(header)
    open(args.classname + 'INLINES.C', 'w').write(inlines)
    
