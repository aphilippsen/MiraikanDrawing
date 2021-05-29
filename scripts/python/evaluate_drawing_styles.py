import pandas as pd
import numpy as np
from similarity_measures import dissimilarity_euclidean, dissimilarity_pearson
import matplotlib.pyplot as plt

# define all stimuli categories and presentation conditions

# sorted by presentation condition
classes = ['class-F', 'class-H', 'class-C', 'class-M', 'class-F', 'class-H', 'class-C', 'class-M', 'class-F', 'class-H', 'class-C', 'class-M']
types = ['type-O', 'type-O', 'type-O', 'type-O', 'type-I',  'type-I',  'type-I',  'type-I',  'type-S', 'type-S', 'type-S', 'type-S']

# sorted by category
#classes = ['class-F', 'class-F', 'class-F', 'class-M', 'class-M', 'class-M', 'class-C', 'class-C', 'class-C', 'class-H', 'class-H', 'class-H']
#types = ['type-O', 'type-I', 'type-S', 'type-O', 'type-I', 'type-S', 'type-O', 'type-I', 'type-S', 'type-O', 'type-I', 'type-S']

num_layers = 7
num_classes = len(classes)

# adult group
adult_indices = np.where(age_AQ_info[:,1]>200)[0]
# child group
child_indices = np.where(age_AQ_info[:,1]<200)[0]

adult_age_AQ_info = np.asarray(age_AQ_info)[adult_indices]
adult_subset = np.asarray(children)[adult_indices]
adult_pic_type = np.asarray(pic_type)[adult_indices]
adult_pic_class = np.asarray(pic_class)[adult_indices]
child_age_AQ_info = np.asarray(age_AQ_info)[child_indices]
children_subset = np.asarray(children)[child_indices]
child_pic_type = np.asarray(pic_type)[child_indices]
child_pic_class = np.asarray(pic_class)[child_indices]

### Pearson distance to adult drawings on different layers for scribbling etc.

#results/python_csv/highest_ranked_coloring.csv
#results/python_csv/highest_ranked_completion.csv
#results/python_csv/highest_ranked_scribbling.csv
#results/python_csv/highest_ranked_tracing.csv

# data structure
# 7 layers x 4 styles x 2 (mean+var)
# => box plot
# layer 1: distance (mean+var) of the 4 styles to the adults... same for all layers
#num_highest = 20
dissimilarity_p_style = np.zeros((num_layers, 4), dtype=object)
# for statistics analysis
style_list = []

m = 70

style_files = ['results/python_csv/highest_ranked_scribbling-' + str(m) + '.csv', 'results/python_csv/highest_ranked_coloring-' + str(m) + '.csv', 'results/python_csv/highest_ranked_tracing-' + str(m) + '.csv', 'results/python_csv/highest_ranked_completion-' + str(m) + '.csv']

for f in range(len(style_files)):
    # load all the drawings with the highest ratings in scribbling
    df = pd.read_csv(style_files[f], delimiter=',')
    df_array = df.values # numpy array
    print("Mean age of " + style_files[f] + ": " + str(np.mean(df_array[:,6])) + "(+/-" + str(np.std(df_array[:,6])) + ")")

    #assert(len(df_array) == num_highest)

    for layer in range(num_layers):
        # go through them one by one
        dissimilarity_p_style[layer, f] = []
        for im in range(len(df_array)):
            # get the index of this picture in the features data set
            index = image_paths.index(df_array[im][5])
            child_pic = features_per_layer[layer][index,:]

            # extract adult drawings of the same class/type
            select_class = [k==df_array[im][4] for k in adult_pic_class]
            select_type = [k==df_array[im][3] for k in adult_pic_type]
            subset_indices = np.asarray(np.where([a and b for a, b in zip(select_type, select_class)])[0],dtype=int)
            adult_feature_subset = features_per_layer[layer][adult_indices][subset_indices]

            dissimilarity_p_style[layer, f].append(dissimilarity_pearson(np.mean(adult_feature_subset,axis=0), child_pic))
            style_list.append([f, layer, dissimilarity_pearson(np.mean(adult_feature_subset,axis=0), child_pic)])

fig = plt.figure('', figsize=(70.0, 10.0))
plt.rcParams.update({'font.size': 40, 'legend.fontsize': 40})

colors = ['blue','red','green','orange']
labels = ['Scribbling', 'Coloring', 'Tracing', 'Completion']
x_poss = [1,2,3,4]

for layer in range(num_layers):
    ax = fig.add_subplot(1, 7, layer+1)
    
    for i in range(4):
        plt.bar(x_poss[i], np.mean(dissimilarity_p_style[layer][i][:]), yerr=
    np.std(dissimilarity_p_style[layer][i][:]), color=colors[i], label=labels[i])
    
    ax.set_ylim([0,0.9])
    ax.set_xlabel('drawing style')
    ax.set_xticks([])
    if layer==0:
        ax.set_ylabel('Distance to adult drawings')
plt.legend()
plt.savefig('results/python_plot/distance-per-drawing-style' + str(m) + '.pdf')
plt.close()
print('Generated plot: results/python_plot/distance-per-drawing-style' + str(m) + '.pdf')

# write csv file for statistical analysis
df_style = pd.DataFrame(data=style_list, columns = ['style', 'layer', 'dist'])
df_style.to_csv('results/python_csv/drawing-style-data-' + str(m) + '.csv')
print('Generated csv: results/python_csv/drawing-style-data-' + str(m) + '.csv')


