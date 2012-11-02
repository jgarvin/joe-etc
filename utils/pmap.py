#!/usr/bin/env python

"""
Finds the pmap bits in fix fast templates
"""

import sys
import xml.parsers.expat as expat

if "help" in sys.argv or len(sys.argv) == 1:
    print "Usage: pmap templates.xml"
    sys.exit(1)

templatexml = sys.argv[1]

parser = expat.ParserCreate()

class Obj(object): pass

cur_decimal = None
cur_template = None
cur_field = None
cur_stack = []
templates = []
cur_tag = ""

# Maps operator and whether optional to whether pmap bit is needed
pmap_required = { ('none'      , False) : False,
                  ('constant'  , False) : False,
                  ('copy'      , False) : True,
                  ('default'   , False) : True,
                  ('delta'     , False) : False,
                  ('increment' , False) : True,
                  ('none'      , True)  : False,
                  ('constant'  , True)  : True,
                  ('copy'      , True)  : True,
                  ('default'   , True)  : True,
                  ('delta'     , True)  : False,
                  ('increment' , True)  : True }

# Maps operator and whether optional to whether field is nullable
is_nullable = { ('none'      , False) : False,
                ('constant'  , False) : False,
                ('copy'      , False) : False,
                ('default'   , False) : False,
                ('delta'     , False) : False,
                ('increment' , False) : False,
                ('none'      , True)  : True,
                ('constant'  , True)  : False,
                ('copy'      , True)  : True,
                ('default'   , True)  : True,
                ('delta'     , True)  : True,
                ('increment' , True)  : True }

field_names = [ "string", "uInt32", "uInt64", "decimal",
                "int32", "int64", "length", "sequence", "group" ]

operator_names = [ "constant", "copy", "default", "delta", "increment" ]

def build_field(name, attributes):
    new_field = Obj()
    new_field.ftype = name.lower()
    new_field.name = None
    if attributes.has_key("name"):
        new_field.name = attributes["name"]
    new_field.optional = False
    new_field.op_value = None
    new_field.operator = "none"
    if attributes.has_key("presence"):
        if attributes["presence"] == "optional":
            new_field.optional = True
    return new_field

def StartElementHandler(name, attributes):
    global cur_tag, cur_template, cur_field, cur_decimal
    global cur_stack
    if name == "template":
        cur_template = Obj()
        cur_template.id = attributes["id"]
        cur_template.fields = []
        cur_template.name = attributes["name"]
        cur_template.optional = False
        cur_stack.append(cur_template)

        template_id_field = Obj()
        template_id_field.operator = 'copy'
        template_id_field.ftype = 'uint32'
        template_id_field.name = 'TemplateId'
        template_id_field.optional = False
        cur_template.fields.append(template_id_field)        
    elif name in field_names:
        cur_field = build_field(name, attributes)
        cur_stack[-1].fields.append(cur_field)
    elif name in operator_names:
        cur_field.operator = name
        if attributes.has_key("value"):
            cur_field.op_value = attributes["value"]

    if name == "decimal":
        cur_decimal = cur_field
        cur_decimal.mantissa = None
        cur_decimal.exponent = None
    elif name == "exponent":
        cur_field = build_field(name, attributes)
        cur_field.ftype = "int32"
        cur_decimal.exponent = cur_field
    elif name == "mantissa":
        cur_field = build_field(name, attributes)
        cur_field.ftype = "int64"
        cur_decimal.mantissa = cur_field
    elif name == "sequence":
        cur_field.fields = []
        cur_stack.append(cur_field)
    elif name == "group":
        cur_field.fields = []
        cur_stack.append(cur_field)

def EndElementHandler(name):
    global templates, cur_template, cur_stack
    if name == "template":
        templates.append(cur_template)
        cur_stack.pop()
    elif name == "sequence":
        # Sometimes sequences aren't given an explicit length in the
        # XML, but it's present in the bitstream regardless, so we
        # patch it in here.
        foundLengthField = False
        for f in cur_stack[-1].fields:
            if f.ftype == "length":
                foundLengthField = True
        if not foundLengthField:
            # We put an @ sign in to indicate this is a generated name,
            # don't think FIXFAST allows them in names.
            lengthField = build_field("length", { "name" : "@ImplicitLength"})
            cur_stack[-1].fields.insert(0, lengthField)
        cur_stack.pop()
    elif name == "group":
        cur_stack.pop()

parser.StartElementHandler = StartElementHandler
parser.EndElementHandler = EndElementHandler

# We do a read instead of parsing the file directly
# to throw out any unicode characters...
xmlcontents = open(templatexml).read()
parser.Parse(xmlcontents)

max_field_name_width = 0
for t in templates:
    for field in t.fields:
        name_len = len(field.name)
        if field.ftype == "decimal":
            name_len += 3 # for exp/man suffix
        max_field_name_width = max(max_field_name_width, name_len)

max_op_width = 0
for op in operator_names:
    max_op_width = max(max_op_width, len(op))

def traverseFields(container, depth, pmap_counters):
    pmap_counters.append(0)
    for field in container.fields:
        if field.ftype == "sequence" or field.ftype == "group":
            format_str = "%%%ds" % max_field_name_width
            print '\t'*depth,format_str % ("{{{{{{{{{{{{")
            print '\t'*depth,format_str % (field.ftype[0].upper() + ":" + field.name),
            if field.ftype == "group" and field.optional:
                pmap_bit = pmap_counters[-1]
                pmap_counters[-1] += 1
                print " %8s" % ("PMAP_%02d" % pmap_bit)
            else:
                print
            traverseFields(field, depth+1, pmap_counters)
            print '\t'*depth,format_str % ("}}}}}}}}}}}}")
            continue

        print '\t'*depth,

        field_op = field.operator

        if field.ftype == "decimal" and field.exponent:
            # Presence of mantissa depends on the presence of the exponent
            # so what really matters is whether the exponent requires a pmap
            # bit and/or is nullable.
            field_op = field.exponent.operator

        format_str = "%%%ds %%%ds" % (max_field_name_width, max_op_width)
        print format_str % (field.name, field_op),

        field_is_optional = field.optional
        if field.ftype == "length":
            field_is_optional = field_is_optional or container.optional

        if pmap_required[(field_op, field_is_optional)]:
            format_str = " %8s"

            if field.ftype == "length":
                pmap_bit = pmap_counters[-2]
                pmap_counters[-2] += 1
            else:
                pmap_bit = pmap_counters[-1]
                pmap_counters[-1] += 1
                
            print format_str % ("PMAP_%02d" % pmap_bit),
        else:
            print " %8s" % ' ',
        if is_nullable[(field_op, field_is_optional)]:
            print " Nullable",
        else:
            print " %8s" % ' ',
        format_str = " %8s"
        print format_str % field.ftype,
        print
    pmap_counters.pop()
    
for t in templates:
    print "==========================================="
    print "Template: " + t.name + " (" + str(t.id) + ")"
    traverseFields(t, 0, [])
