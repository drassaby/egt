import csv
import matplotlib.pyplot as plt
	
with open("../../../data/precedent.csv") as fd:
	data = list(csv.reader(fd))
	skew = [float(d[0]) for d in data]
	p2q2high = [float(d[1]) for d in data]
	p1q2high = [float(d[2]) for d in data]
	p2q1high = [float(d[3]) for d in data]
	p1q1high = [float(d[4]) for d in data]

plt.plot(skew, p1q1high, '#4444ff', label="P1, Q1", linewidth=4)
plt.plot(skew, p1q2high, '#444488', label="P1, Q2 and P2, Q1", linewidth=4)
plt.plot(skew, p2q2high, '#000000', label="P2, Q2", linewidth=4)
plt.title("Effect of Precedent on Majority Advantage with Moderate Intersectionality\n"
	"P1=0.9, Q1=0.9, D=0")

plt.legend()


plt.xlabel("Initial bias toward majority demanding High")
plt.ylabel("Proportion of Simulations resulting in advantage")
plt.show()

