#!/usr/bin/env python

import sys
import tempfile
from subprocess import Popen, run
import os
from dataclasses import dataclass, fields
from collections import defaultdict
from pathlib import Path
import matplotlib.pyplot as plt
import glob

LIMIT = 5
REPETITIONS = 1

@dataclass
class ProcessTime:
    user_time: float
    system_time: float
    total_time: float

def time_command(cmd: list[str]) -> float:
    """
    Run a command and return its CPU time usage (user, system, and total).
    Only includes time spent in the process itself, not system-wide time.

    Args:
        cmd: Command and arguments as a list of strings

    Returns:
        ProcessTime containing user, system, and total CPU times in seconds
    """
    print(f"Running: {' '.join(cmd)}")
    process = Popen(cmd)
    _, _, rusage = os.wait4(process.pid, 0)

    # rusage times are in microseconds, convert to seconds
    user_time = rusage.ru_utime
    system_time = rusage.ru_stime
    total_time = user_time + system_time

    return total_time

def min_time_command(cmd):
    return min([time_command(cmd) for i in range(REPETITIONS)])

stdheaders = """
<algorithm>
<any>
<array>
<atomic>
<barrier>
<bit>
<bitset>
<cassert>
<ccomplex>
<cctype>
<cerrno>
<cfenv>
<cfloat>
<charconv>
<chrono>
<cinttypes>
<ciso646>
<climits>
<clocale>
<cmath>
<codecvt>
<compare>
<complex>
<concepts>
<condition_variable>
<coroutine>
<csetjmp>
<csignal>
<cstdarg>
<cstddef>
<cstdint>
<cstdio>
<cstdlib>
<cstring>
<ctgmath>
<ctime>
<cuchar>
<cwchar>
<cwctype>
<deque>
<exception>
<execution>
<expected>
<filesystem>
<format>
<forward_list>
<fstream>
<functional>
<future>
<generator>
<initializer_list>
<iomanip>
<ios>
<iostream>
<iterator>
<latch>
<limits>
<list>
<locale>
<map>
<memory>
<memory_resource>
<mutex>
<new>
<numbers>
<numeric>
<optional>
<print>
<queue>
<random>
<ranges>
<ratio>
<regex>
<scoped_allocator>
<semaphore>
<set>
<shared_mutex>
<source_location>
<span>
<spanstream>
<sstream>
<stack>
<stacktrace>
<stdexcept>
<stdfloat>
<stop_token>
<string>
<string_view>
<strstream>
<syncstream>
<system_error>
<thread>
<tuple>
<typeinfo>
<type_traits>
<unordered_map>
<unordered_set>
<utility>
<valarray>
<variant>
<vector>
<version>
"""

headers = stdheaders.splitlines()
headers = [h for h in headers if h]

# TODO: with and without precompiled headers
@dataclass
class Result:
    title: str = None
    preprocess_time: float = None
    precompile_time: float = None
    compile_time: float = None
    compile_time_with_precompiled_headers: float = None
    compile_time_precompiled_ratio: float = None
    marginal_preprocess_time: float = None
    marginal_precompile_time: float = None
    marginal_compile_time: float = None
    gch_kb: float = None
    header_kb: float = None
    gch_header_ratio: float = None

test_results = {}
# with (
#     tempfile.NamedTemporaryFile(suffix=".hpp") as all_include_file,
#     tempfile.NamedTemporaryFile(suffix=".cpp") as all_compile_precompiled_headers_file,
#     tempfile.NamedTemporaryFile(suffix=".cpp") as all_compile_file
# ):
#     for header in headers:
#         all_include_file.write(("#include " + header + "\n").encode())
#         all_compile_file.write(("#include " + header + "\n").encode())

#     all_include_file.flush()

#     all_compile_file.write("\nint main () { return 0; }".encode())
#     all_compile_file.flush()

#     all_compile_precompiled_heaeders_file.write(("#include <" + all_include_file.name + ">\n").encode())
#     all_compile_precompiled_headers_file.write("\nint main () { return 0; }".encode())
#     all_compile_precompiled_headers_file.flush()

#     preprocess_all_command = [
#         *sys.argv[1:],
#         "-E",
#         "-o",
#         str(Path(all_include_file.name).with_suffix('.ii')),
#         all_include_file.name
#     ]

#     precompile_all_command = [
#         *sys.argv[1:],
#         all_include_file.name
#     ]

#     compile_all_precompiled_command = [
#         *sys.argv[1:],
#         all_compile_precompiled_headers_file.name
#     ]

#     compile_all_command = [
#         *sys.argv[1:],
#         all_compile_file.name
#     ]

#     print("all preprocessing")
#     all_preprocess_time = min_time_command(preprocess_all_command)
#     print("all precompiling")
#     all_precompile_time = min_time_command(precompile_all_command)
#     print("all compiling")
#     all_compile_time = min_time_command(compile_all_command)
#     print("all compiling with precompiled headers")
#     all_compile_time_with_precompiled_headers = min_time_command(compile_all_command)

header_subsets = [headers, *[[h] for h in headers]]

all_results = None
for header_subset in header_subsets[:1+LIMIT]:
    with (
        tempfile.NamedTemporaryFile(suffix=".hpp") as single_include_file,
        tempfile.NamedTemporaryFile(suffix=".hpp") as single_exclude_file,
        tempfile.NamedTemporaryFile(suffix=".cpp") as single_include_compile_file,
        tempfile.NamedTemporaryFile(suffix=".cpp") as single_exclude_compile_file
    ):
        for header in header_subset:
            single_include_file.write(("#include " + header + "\n").encode())

        single_include_compile_file.write(("#include <" + single_include_file.name + ">\n").encode())
        single_include_compile_file.write("\nint main() { return 0; }".encode())

        single_include_file.flush()
        single_include_compile_file.flush()

        for inner_header in headers:
            if inner_header in header_subset:
                continue
            single_exclude_file.write(("#include " + inner_header + "\n").encode())

        single_exclude_file.flush()

        single_exclude_compile_file.write(("#include <" + single_exclude_file.name + ">\n").encode())
        single_exclude_compile_file.write("\nint main() { return 0; }".encode())
        single_exclude_compile_file.flush()

        preprocess_include_command = [
            *sys.argv[1:],
            "-E",
            "-o",
            str(Path(single_include_file.name).with_suffix('.ii')),
            single_include_file.name
        ]

        precompile_include_command = [
            *sys.argv[1:],
            single_include_file.name
        ]

        compile_include_command = [
            *sys.argv[1:],
            single_include_compile_file.name
        ]

        preprocess_exclude_command = [
            *sys.argv[1:],
            "-E",
            "-o",
            str(Path(single_exclude_file.name).with_suffix('.ii')),
            single_exclude_file.name
        ]

        precompile_exclude_command = [
            *sys.argv[1:],
            single_exclude_file.name
        ]

        compile_exclude_command = [
            *sys.argv[1:],
            single_exclude_compile_file.name
        ]

        x = Result()

        x.title = None
        if all_results is None:
            x.title = "All"
        else:
            assert len(header_subset) == 1
            x.title = header_subset[0]

        print(f"{x.title} preprocesssing")
        x.preprocess_time = min_time_command(preprocess_include_command)
        print(f"{x.title} precompiling")
        x.precompile_time = min_time_command(precompile_include_command)
        print(f"{x.title} compiling")
        x.compile_time = min_time_command(compile_include_command) # needs to be after precompile to pick up gch

        if all_results is not None:
            print(f"{x.title} excluding preprocessing")
            x.marginal_preprocess_time = all_results.preprocess_time - min_time_command(preprocess_exclude_command)
            print(f"{x.title} excluding precompliing")
            x.marginal_precompile_time = all_results.precompile_time - min_time_command(precompile_exclude_command)
            print(f"{x.title} excluding compiling")
            x.marginal_compile_time = all_results.compile_time - min_time_command(compile_exclude_command)
        else:
            x.marginal_preprocess_time = x.preprocess_time
            x.marginal_precompile_time = x.precompile_time
            x.marginal_compile_time = x.compile_time

        x.header_kb = os.stat(Path(single_include_file.name).with_suffix('.ii')).st_size / 1000
        x.gch_kb = os.stat(Path(single_include_file.name).with_suffix('.hpp.gch')).st_size / 1000
        x.gch_header_ratio = x.gch_kb / x.header_kb

        if all_results is None:
            # assumption is that the 'all' subset is evaluated first
            all_results = x

        test_results[header] = x

        for garbage in glob.glob(str(Path(single_include_file.name).with_suffix('')) + "*"):
            os.remove(garbage)
        for garbage in glob.glob(str(Path(single_exclude_file.name).with_suffix('')) + "*"):
            os.remove(garbage)

def report_times(times_dict):
    s = list(times_dict.items())

    totals = defaultdict(lambda: 0)
    medians = defaultdict(lambda: 0)
    for field in fields(s[0][1]):
        if field.name == "title":
            continue

        print(f"================== {field.name} ==================")
        s.sort(key=lambda x: getattr(x[1], field.name))

        medians[field.name] = getattr(s[len(s)//2][1], field.name)
        for result in ((x[1] for x in s)):
            totals[field.name] += getattr(result, field.name)

        for header, result in s:
            val = getattr(result, field.name)
            # print(f"{header=} {time=}")
            print(f"{header}: {val:,.6f}")

        print(f"{field.name} average: {totals[field.name]/len(s):,.2f}")
        print(f"{field.name} median: {medians[field.name]:,.2f}")

        headers = []
        values = []
        for header, result in s:
            val = getattr(result, field.name)
            headers.append(header)
            values.append(val if val is not None else 0)
            print(f"{header}: {val:,.6f}" if val is not None else f"{header}: N/A")

        plt.figure(figsize=(6, 20))
        bars = plt.barh(headers, values, color='skyblue')
        plt.ylabel('Headers')
        plt.xlabel(field.name.replace('_', ' ').capitalize())
        plt.title(f'{field.name.replace("_", " ").capitalize()} by Header')
        plt.tight_layout()

        for bar in bars:
            width = bar.get_width()
            plt.annotate(
                f'{width:,.3f}',
                xy=(width, bar.get_y() + bar.get_height() / 2),
                xytext=(3, 0),
                textcoords="offset points",
                ha='left', va='center', fontsize=8
            )

        plt.savefig(f"{field.name}_bar_chart.png")
        plt.close()

    print(f"Ratio of gch to hpp: {totals['gch_bytes']/totals['header_bytes']:,.2f}")

report_times(test_results)