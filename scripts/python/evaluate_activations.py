import cv2
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import scipy

from similarity_measures import dissimilarity_euclidean, dissimilarity_pearson


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

############
# Analysis #
############

### Calculate distance to adult for every child and store info

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

# for each layer
all_dists_data = []
for layer in range(num_layers):
    # get adult features for this layer
    feats_adult = features_per_layer[layer][adult_indices]
    feats_kids = features_per_layer[layer][child_indices]
    
    # for all images
    for im in range(len(children_subset)):
    
        # get all related pictures from adults
        current_class = child_pic_class[im]
        current_type = child_pic_type[im]
    
        select_class = [i==current_class for i in adult_pic_class]
        select_type = [i==current_type for i in adult_pic_type]
        subset_indices = np.asarray(np.where([a and b for a, b in zip(select_type, select_class)])[0],dtype=int)
        
        # take the mean
        mean_adult = np.mean(feats_adult[subset_indices,:],axis=0)
        
        dist = dissimilarity_pearson(mean_adult, feats_kids[im,:])
        
        group=0
        if child_age_AQ_info[im,1] <= 40:
            group=1
        elif child_age_AQ_info[im,1] <= 60:
            group=2
        elif child_age_AQ_info[im,1] <= 70:
            group=3
        else:
            group=4
        
        all_dists_data.append([group, np.asarray(image_paths)[child_indices][im], current_class, current_type, layer, child_age_AQ_info[im,1], child_age_AQ_info[im,3], dist])
        
df_all_dists = pd.DataFrame(data=all_dists_data, columns = ['age_group', 'image_url', 'pic_class', 'pic_type', 'layer', 'age', 'aq', 'dist'])
df_all_dists.to_csv('results/python_csv/all-dist-to-adults.csv')
print('Generate results/python_csv/all-dist-to-adults.csv for R script AnalyzeDistToAdults.R')

