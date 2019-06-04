#!/usr/bin/env python3

import csv
import matplotlib.pyplot as plt
import numpy as np
import sys

output_filename = sys.argv[1]
title = "Proportion of (P1, Q1) playing strategies in P and Q arenas"
# labels = [str(x) for x in range(1,10)]
labels = sys.argv[2:]

with open(output_filename) as csv_fd:

    matrix_lines = [[float(proportion) for proportion in line[:-1].split(" ")]
                    for line in csv_fd
                    if line.startswith("0")  or line.startswith("1")]

    matrix_size = len(matrix_lines[0])
    matrices = [matrix_lines[k:k+matrix_size] for k in range(0, len(matrix_lines), matrix_size)]

    matrix = np.array(matrices[0])

    for matrix_to_add in matrices[1:]:
        matrix = np.add(matrix, np.array(matrix_to_add))

    matrix /= len(matrices)

    print(matrix)


fig, ax = plt.subplots()
im = ax.imshow(matrix)

ax.set_xticks(np.arange(matrix.shape[0]))
ax.set_yticks(np.arange(matrix.shape[0]))

ax.set_xticklabels(np.array(labels))
ax.set_yticklabels(np.array(labels))

ax.xaxis.tick_top()

heatmap = plt.imshow(matrix, cmap='Blues', interpolation='nearest')

# Loop over data dimensions and create text annotations.
for i in range(len(labels)):
    for j in range(len(labels)):
        val = matrix[i, j]
        if val <= np.max(matrix) / 2:
            color = "k"
        else:
            color="w"

        label = "%.4f" % val

        text = ax.text(j, i, label,
                       ha="center", va="center", color=color)

plt.title(title)
plt.colorbar(heatmap)
plt.show()
