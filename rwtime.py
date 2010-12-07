from datetime import date, datetime
import time

class RWTimePrinter(object):
    def __init__(self, val):
        self.val = val

    def __convert(self, x):
        is_now = ""
        if int(x) == 0:
            d = datetime.now()
            time_t_time = time.mktime(d.timetuple())
            is_now = "(RIGHT NOW)"
        else:
            time_t_time = int(x) - 2177452800
            d = date.fromtimestamp(time_t_time)
        return ("RWTime object. Seconds from Jan 1st 1901: (%d) "
                "Seconds from Jan 1st. 1970: (%d) "
                "Human readable time: %s (%s) " %
                (int(x), time_t_time, is_now, d.ctime()))

    def to_string(self):
        return self.__convert(self.val['sec'])

    def display_hint(self):
        return 'string'

def rwtime_lookup_function(val):
    lookup_tag = val.type.tag
    if lookup_tag == None:
        return None
    if "RWTime" == lookup_tag:
        return RWTimePrinter(val)
    return None

gdb.pretty_printers.append(rwtime_lookup_function)
