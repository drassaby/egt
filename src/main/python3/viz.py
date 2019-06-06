#!/usr/bin/env python3

import csv
from typing import Pattern, List

import matplotlib.pyplot as plt
import numpy as np
import sys
import argparse
import os
import re


# output_filename = sys.argv[1]


def main(args: argparse.Namespace):
    input_location: str = args.input
    output_directory: str = args.output_directory
    if not os.path.isdir(output_directory):
        raise NotADirectoryError(f"{output_directory} is not a valid output directory,"
                                 " see usage with:\n    ./viz.py --help")

    if os.path.isdir(input_location):
        input_files = [f"{input_location}/{filename} " for filename in
                       filter(lambda file: file.endswith(".out"), os.listdir(input_location))]
    elif os.path.isfile(input_location):
        input_files = [input_location]
    else:
        raise FileNotFoundError(f"{input_location} is not a valid output directory,"
                                " see usage with:\n    ./viz.py --help")

    for input_file in input_files:
        egt_result: EGTResult = EGTResult(f"{input_file}")
        egt_result.put_heatmap(output_directory)


class EGTResult:
    """
    The results indicated in one file of simulation output data.
    """

    def __init__(self, input_file):
        """
        Input Files
        :param input_file:
        """
        self.input_file = input_file
        problem_type_re: Pattern[str] = re.compile("P1=(\d+\.\d+),"
                                                   " Q1=(\d+\.\d+),"
                                                   " D=(\d+\.\d+), "
                                                   "strategies=Vector\(((?:\d+\.\d+, )+\d+.\d)\), "
                                                   "simulation=(Minimal|Moderate)")
        with open(input_file) as input_fd:
            input_data: str = input_fd.read()
        description = problem_type_re.search(input_data)
        if not description:
            raise LookupError("Could not find problem description in file")


        # Problem description
        self.p1 = float(description.group(1))
        self.q1 = float(description.group(2))
        self.d = float(description.group(3))
        self.strategies = [float(strategy) for strategy in description.group(4).strip().split(",")]
        self.simulation = description.group(5)

        # Results
        self.result_matrix = self.find_parsed_result()

    def find_parsed_result(self) -> np.array:

        line_re: Pattern[str] = re.compile("(?:(\d+\.\d+) )+(\d+\.\d+)\n")
        with open(self.input_file) as input_fd:
            matrix_lines = [[float(proportion) for proportion in line[:-1].split(" ")]
                            for line in input_fd
                            if line_re.fullmatch(line)]
        matrix_size = len(matrix_lines[0])

        matrices = [matrix_lines[k:k + matrix_size] for k in range(0, len(matrix_lines), matrix_size)]

        matrix = np.array(matrices[0])

        for matrix_to_add in matrices[1:]:
            matrix = np.add(matrix, np.array(matrix_to_add))

        matrix /= len(matrices)

        return matrix

    def put_heatmap(self, at: str) -> None:
        """ TODO: Actually write the png files to some location
        :param at: Where to write the png files to
        """
        labels = [str(strategy) for strategy in self.strategies]
        title = "Proportion of (P1, Q1) playing strategies in P and Q arenas with " \
            f"P1={self.p1}, Q1={self.q1}, D={self.d}"

        fig, ax = plt.subplots()
        # im = ax.imshow(self.result_matrix)

        ax.set_xticks(np.arange(self.result_matrix.shape[0]))
        ax.set_yticks(np.arange(self.result_matrix.shape[0]))

        ax.set_xticklabels(np.array(labels))
        ax.set_yticklabels(np.array(labels))

        ax.xaxis.tick_top()

        heatmap = plt.imshow(self.result_matrix, cmap='Blues', interpolation='nearest')

        # Loop over data dimensions and create text annotations.
        for i in range(len(labels)):
            for j in range(len(labels)):
                val = self.result_matrix[i, j]
                if val <= np.max(self.result_matrix) / 2:
                    color = "k"
                else:
                    color = "w"

                label = "%.4f" % val
                ax.text(j, i, label, ha="center", va="center", color=color)

        plt.title(title)
        plt.colorbar(heatmap)
        plt.show()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Create a heat map from EGT output data.')
    parser.add_argument('input', help='The location we read the data from (directory or file).')
    parser.add_argument('output_directory', help='The directory we write the outputted images to.')
    main(parser.parse_args())
