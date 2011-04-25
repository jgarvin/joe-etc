#!/usr/bin/env python

import re

class parser(object):
    def __init__(self, format_string):
        name_regex = re.compile(r"\$\([^\(\)]+\)")
        matches = name_regex.finditer(format_string)
        self.fixed_strings = []
        self.var_names = []
        idx = 0
        for m in matches:
            self.var_names.append(format_string[m.start()+2:m.end()-1])
            self.fixed_strings.append(format_string[idx:m.start()])
            idx = m.end()

    def parse(self, target):
        result = parse_result()

        idx = 0
        for s, v in zip(self.fixed_strings, [""] + self.var_names):
            end = target.find(s, idx) + len(s)
            print idx, end
            if end == -1:
                return None
            if end - idx != 0 and v:
                val = target[idx:end-1]
                try:
                    val = long(val)
                except ValueError:
                    pass
                setattr(result, v.lower(), val)
            idx = end

        return result

class parse_result(object):
    pass

def test():
    example = "tlapp.icache_wombat.udesktop178-2011-04-23T17:03:22-173-20577.log"
    ex_desc_string = "tlapp.$(APP).$(HOST)-$(YEAR)-$(MONTH)-$(DAY)T$(HOUR):$(MINUTE):$(SECOND)-$(USER)-$(PID).log"

    foo = parser(ex_desc_string)
    print foo.fixed_strings
    print foo.var_names

    bar = foo.parse(example)
    print bar.app, type(bar.app)
    print bar.host, type(bar.host)
    print bar.year, type(bar.year)
    print bar.month, type(bar.month)
    print bar.day, type(bar.day)
    print bar.hour, type(bar.hour)

if __name__ == "__main__":
    test()
