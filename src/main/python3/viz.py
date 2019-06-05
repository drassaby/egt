#!/usr/bin/env python3

import csv
from typing import Pattern

import matplotlib.pyplot as plt
import numpy as np
import sys
import argparse
import os
import re

# output_filename = sys.argv[1]
title = "Proportion of (P1, Q1) playing strategies in P and Q arenas"
# labels = [str(x) for x in range(1,10)]
# labels = sys.argv[2:]


def main(args: argparse.Namespace):
    input_location_str: str = args.input
    output_directory_str: str = args.output_directory
    if not os.path.isdir(output_directory_str):
        raise NotADirectoryError(f"{output_directory_str} is not a valid directory, see usage with:\n    ./viz.py --help")

    if os.path.isdir(input_location_str):
        for input_file in filter(lambda file : file.endswith(".out"), os.listdir(input_location_str)):
            egt_result: EGTResult = EGTResult(f"{input_location_str}/{input_file}")


class EGTResult:
    """
    The results indicated in one file of simulation output data.
    """

    def __init__(self, input_file):
        """
        Input Files
        :param input_file:
        """
        problem_type_re: Pattern[str] = re.compile("P1=(\d+\.\d+),"
                                                   " Q1=(\d+\.\d+),"
                                                   " D=(\d+\.\d+), "
                                                   "strategies=Vector\(((?:\d+\.\d+, )+\d+.\d)\), "
                                                   "simulation=(Minimal|Moderate)")
        with open(input_file) as input_file:
            input_data: str = input_file.read()
        description = problem_type_re.search(input_data)
        self.p1 = float(description.group(1))
        self.q1 = float(description.group(2))
        self.d = float(description.group(3))
        self.strategies = [float(strategy) for strategy in description.group(4).strip().split(",")]
        self.simulation = description.group(5)






#
#
#
# with open(output_filename) as csv_fd:
#
#     matrix_lines = [[float(proportion) for proportion in line[:-1].split(" ")]
#                     for line in csv_fd
#                     if line.startswith("0")  or line.startswith("1")]
#
#     matrix_size = len(matrix_lines[0])
#     matrices = [matrix_lines[k:k+matrix_size] for k in range(0, len(matrix_lines), matrix_size)]
#
#     matrix = np.array(matrices[0])
#
#     for matrix_to_add in matrices[1:]:
#         matrix = np.add(matrix, np.array(matrix_to_add))
#
#     matrix /= len(matrices)
#
#     print(matrix)
#
#
# fig, ax = plt.subplots()
# im = ax.imshow(matrix)
#
# ax.set_xticks(np.arange(matrix.shape[0]))
# ax.set_yticks(np.arange(matrix.shape[0]))
#
# ax.set_xticklabels(np.array(labels))
# ax.set_yticklabels(np.array(labels))
#
# ax.xaxis.tick_top()
#
# heatmap = plt.imshow(matrix, cmap='Blues', interpolation='nearest')
#
# # Loop over data dimensions and create text annotations.
# for i in range(len(labels)):
#     for j in range(len(labels)):
#         val = matrix[i, j]
#         if val <= np.max(matrix) / 2:
#             color = "k"
#         else:
#             color="w"
#
#         label = "%.4f" % val
#
#         text = ax.text(j, i, label,
#                        ha="center", va="center", color=color)
#
# plt.title(title)
# plt.colorbar(heatmap)
# plt.show()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Create a heat map from EGT output data.')
    parser.add_argument('input', help='The location we read the data from (directory or file).')
    parser.add_argument('output_directory', help='The directory we write the outputted images to.')
    main(parser.parse_args())
