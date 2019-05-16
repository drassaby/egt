import csv
import matplotlib.pyplot as plt
import numpy as np

csv_filename = "./456.csv"
title = "Proportion of (P1, Q1) playing strategies in P and Q arenas"
labels = ["4", "5", "6"]


with open(csv_filename) as csv_fd:
    matrix = np.array([np.array([float(x) for x in row])
                       for row in csv.reader(csv_fd, delimiter=' ')])


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
        if val < np.mean(matrix):
            color = "k"
        else:
            color="w"

        label = "%.4f" % val

        text = ax.text(j, i, label,
                       ha="center", va="center", color=color)

plt.title(title)
plt.colorbar(heatmap)
plt.show()