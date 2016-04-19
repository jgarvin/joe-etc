#!/usr/bin/env python
# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
from numpy.random import normal, uniform
import sys

def frange(x, y, jump):
  while x < y:
      yield x
      x += jump

numbers = []
for n in open(sys.argv[1]).readlines():
    numbers.append(float(n))


# numbers = []
# for n in open(sys.argv[1]).readlines():
#     x = float(n)
#     if x < 12:
#         numbers.append(float(n))


# binWidth = 0.5
# bins= list(frange(min(numbers), max(numbers) + binWidth, binWidth))

# print bins

plt.hist(numbers, bins=10, histtype='stepfilled', color='b', label=sys.argv[2])
# plt.hist(numbers, bins=10, histtype='stepfilled', color='b', label='read/write')
# plt.hist(numbers, bins=bins, histtype='stepfilled', color='b', label='JG')
plt.title("Histogram")
plt.xlabel("Seconds taken")
plt.ylabel("Occurrences")
plt.legend()
plt.show()
