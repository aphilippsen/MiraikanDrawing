"""
Interactive plot of Figure 10 of "Quantifying developmental and individual differences ofspontaneous drawing completion in children".

Runing the script opens an interactive figure, showing the individual score of all children (y-axis), computed as the average of the RDM matrix of the highest layer of the CNN, plotted against the completion score acquired in the rating analysis (x-axis).

An interactive figure opens. When selecting points in the figure via mouse click, the child data corresponding to these values is copied to the subfolder `pictureExplorer_current'.

(Note: Some drawings cannot be displayed as the child's parents did not give consent to publish the drawing data online, in this case the folder stays empty.)
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from interactive_plotting import PictureExplorer


# load rating results
rating_data = "results/csv/score-vs-ratings-6.csv"
x = pd.read_csv(rating_data)
compl_data = np.asarray(x["MeanCompl"].tolist())
scribble_data = np.asarray(x["MeanScribble"].tolist())
tracing_data = np.asarray(x["MeanTracing"].tolist())
coloring_data = np.asarray(x["MeanColoring"].tolist())
related_data = np.asarray(x["MeanMeaning"].tolist())

# which is the prominent behavior of each child
rating_data = np.concatenate([compl_data.reshape(-1,1), scribble_data.reshape(-1,1), tracing_data.reshape(-1,1), coloring_data.reshape(-1,1)], axis=1)
child_behavior = np.argmax(rating_data,axis=1)

id_data = np.asarray(x["Child"].tolist())

x = pd.read_csv("results/csv/score-vs-ratings-6.csv")
score_data_6 = np.asarray(x["scoreByMean"].tolist())

# Define which values should be displayed on x any y axis

plot_data = np.concatenate([compl_data.reshape(-1,1),  score_data_6.reshape(-1,1)],axis=1)
axes_names = ["average degree of completion (of maximum rating 100)", "individual differences score on highest network layer"]

explorer = PictureExplorer(plot_data, 'data/all_drawings', id_data, radius=1, copy_images = False)

# colors provided depending on the main drawing style that the child showed, according to the ratings
colors = ['green', 'red', 'blue', 'orange']

plot_fig,ax = plt.subplots(1,1)

for i in range(plot_data.shape[0]):
    ax.plot(plot_data[i,0], plot_data[i,1], 'o', color=colors[child_behavior[i]])
    #plot_fig = sns.scatterplot(plot_data[:,0], plot_data[:,1], colors=colors[child_behavior])

ax.set_xlabel(axes_names[0])
ax.set_ylabel(axes_names[1])

#obtain m (slope) and b(intercept) of linear regression line
m, b = np.polyfit(plot_data[:,0], plot_data[:,1], 1)
#add linear regression line to scatterplot 
plt.plot(plot_data[:,0], m*plot_data[:,0]+b)
plot_fig.canvas.mpl_connect('button_press_event', explorer)
plt.show()

