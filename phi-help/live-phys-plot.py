#----
# By Chris Dancy, Dept of Computer Science, Bucknell University
# Python functions to allow simple live graphing of Phys Variables (and perhaps others)
# Assumes file with 2 columns and each row separated by comma
# Used [https://pythonprogramming.net/live-graphs-matplotlib-tutorial/] for initial dev of this functionality
#----

import argparse
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import matplotlib.style as style

class PlotPhi:
	def __init__(self, data_file_name):
		self.data_file = data_file_name


	def main_func(self):
		style.use("fivethirtyeight")
		self._fig = plt.figure()
		self._ax1 = self._fig.add_subplot(1,1,1)
		ani = animation.FuncAnimation(self._fig, self.animate, interval=1000)
		plt.title(self.data_file)
		plt.show()


	def animate(self, i):
		"""
		Used to updaate our plot with new data at every interval
			(interval decided by call to )
		"""
		data_file = open(self.data_file, "r")
		graph_data = data_file.read()
		rows = graph_data.split('\n')
		xs = []
		ys = []

		for row in rows:
			if (len(row) > 1):
				(x, y) = row.split(",")
				xs.append(float(x))
				ys.append(float(y))
		self._ax1.clear()
		self._ax1.plot(xs, ys)


if __name__ == '__main__':
	parser = argparse.ArgumentParser(description='Plot ACT-R/Phi variables')
	parser.add_argument('--data_file', type=str, default="../user-loads/save-phi-vals.txt", help='The name (and location) of the file that holds those data you wish to plot.')
	args = parser.parse_args()
	my_plot = PlotPhi(args.data_file)
	my_plot.main_func()
