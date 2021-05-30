import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import scipy
import seaborn as sns

from similarity_measures import dissimilarity_euclidean, dissimilarity_pearson


os.makedirs('results/python_plot/RDMs_per_child', exist_ok = True)

# sorted by category
classes = ['class-F', 'class-F', 'class-F', 'class-M', 'class-M', 'class-M', 'class-C', 'class-C', 'class-C', 'class-H', 'class-H', 'class-H']
types = ['type-O', 'type-I', 'type-S', 'type-O', 'type-I', 'type-S', 'type-O', 'type-I', 'type-S', 'type-O', 'type-I', 'type-S']
matrix_labels = ['O', 'F I', 'S', 'O', 'M I', 'S', 'O', 'C I', 'S', 'O', 'H I', 'S']
num_classes = len(classes)

# adult group
adult_indices = np.where(age_AQ_info[:,1]>200)[0]
# child group (exclude children which don't have a sufficient number of drawings)
child_indices = np.where([a and b for a, b in zip(age_AQ_info[:,1]<200, age_AQ_info[:,0] != 'P69')])[0]

adult_age_AQ_info = np.asarray(age_AQ_info)[adult_indices]
adult_subset = np.asarray(children)[adult_indices]
adult_pic_type = np.asarray(pic_type)[adult_indices]
adult_pic_class = np.asarray(pic_class)[adult_indices]
child_age_AQ_info = np.asarray(age_AQ_info)[child_indices]
children_subset = np.asarray(children)[child_indices]
child_pic_type = np.asarray(pic_type)[child_indices]
child_pic_class = np.asarray(pic_class)[child_indices]

num_layers = 7

child_ids = np.unique(child_age_AQ_info[:,0]).tolist()
adult_ids = np.unique(adult_age_AQ_info[:,0]).tolist()

adult_RDMs = np.zeros((num_layers, len(adult_ids)-1), dtype=object)
adult_classes_types = np.empty((num_layers, len(adult_ids), 2), dtype=object)

child_RDMs = np.empty((num_layers, len(child_ids)), dtype=object)
child_classes_types = np.empty((len(child_ids), 2), dtype=object)

# get the adult RDM of the mean features
adult_RDM = np.zeros((num_layers, len(classes),len(classes)))
kid1_RDM = np.zeros((num_layers, len(classes),len(classes)))
kid2_RDM = np.zeros((num_layers, len(classes),len(classes)))
kid3_RDM = np.zeros((num_layers, len(classes),len(classes)))
kid4_RDM = np.zeros((num_layers, len(classes),len(classes)))
for layer in range(num_layers):
    feats_adult = features_per_layer[layer][adult_indices]
    # adult matrix
    for i in range(len(classes)):
        for j in range(len(classes)):
            select_class_i = [k==classes[i] for k in adult_pic_class]
            select_type_i = [k==types[i] for k in adult_pic_type]
            subset_indices_i = np.asarray(np.where([a and b for a, b in zip(select_type_i, select_class_i)])[0],dtype=int)
            select_class_j = [k==classes[j] for k in adult_pic_class]
            select_type_j = [k==types[j] for k in adult_pic_type]
            subset_indices_j = np.asarray(np.where([a and b for a, b in zip(select_type_j, select_class_j)])[0],dtype=int)            
            # get mean of class i
            mean_i = np.mean(feats_adult[subset_indices_i],axis=0) 
            # get mean of class j        
            mean_j = np.mean(feats_adult[subset_indices_j],axis=0) 
            adult_RDM[layer,i,j] = dissimilarity_pearson(mean_i, mean_j)


# for all layers
for layer in range(num_layers):
    feats_adult = features_per_layer[layer][adult_indices]
    feats_kids = features_per_layer[layer][child_indices]
    
    # for all adults
    for adult in adult_ids[1:]: # first adult did not draw on scrambled, exclude
        adult_idx = adult_ids.index(adult)

        # get the adult features
        select_adult = adult_age_AQ_info[:,0] == adult
        select_adult_feats = feats_adult[select_adult]
        # store which classes/types adult drew on
        adult_classes_types[layer, adult_idx,0] = adult_pic_class[select_adult]
        adult_classes_types[layer, adult_idx,1] = adult_pic_type[select_adult]

        # generate adult matrix: distance between each pair
        adult_RDMs[layer, adult_idx-1] = np.zeros((len(classes), len(classes)))

        # adults drew on all stimuli, so go through them in order
        for i in range(len(classes)):
            for j in range(len(classes)):
                select_class_i = adult_classes_types[layer][adult_idx][0] == classes[i]
                select_type_i = adult_classes_types[layer][adult_idx][1] == types[i]
                select_class_j = adult_classes_types[layer][adult_idx][0] == classes[j]
                select_type_j = adult_classes_types[layer][adult_idx][1] == types[j]
                subset_indices_i = np.asarray(np.where([a and b for a, b in zip(select_class_i, select_type_i)]))[0]
                subset_indices_j = np.asarray(np.where([a and b for a, b in zip(select_class_j, select_type_j)]))[0]
                
                # get mean of class i
                mean_i = np.mean(select_adult_feats[subset_indices_i,:],axis=0) 
                # get mean of class j        
                mean_j = np.mean(select_adult_feats[subset_indices_j,:],axis=0) 
                adult_RDMs[layer, adult_idx-1][i,j] = dissimilarity_pearson(mean_i, mean_j)
        
        
        # plot the adult RDMs
        min_val = 0
        max_val = 0.8
        fig, ax = plt.subplots(1,1)
        img = ax.imshow(adult_RDMs[layer, adult_idx-1], cmap='viridis', vmin=min_val, vmax=max_val)
        ax.set_xticks(np.arange(num_classes))
        #ax.set_xticklabels(matrix_labels)
        ax.set_yticks(np.arange(num_classes))
        #ax.set_yticklabels(matrix_labels)
        plt.colorbar(img)
        plt.savefig('results/python_plot/RDMs_per_child/RDM_layer-' + str(layer) + '_adult-' + adult + '.pdf')
        plt.close()
        
        
    # for all children
    for child in child_ids:
        child_idx = child_ids.index(child)

        # select all drawings' features of this child
        select_child = child_age_AQ_info[:,0] == child
        select_child_feats = feats_kids[select_child]
        # store which classes/types child drew on
        child_classes_types[child_idx,0] = child_pic_class[select_child]
        child_classes_types[child_idx,1] = child_pic_type[select_child]

        # which are the indices of the class/type on which the child has drawn
        submatrix_idcs = [classes.index(child_classes_types[child_idx][0][x]) + types.index(child_classes_types[child_idx][1][x]) for x in range(len(child_classes_types[child_idx][0]))]
        submatrix_idcs_sorted = np.copy(submatrix_idcs)
        submatrix_idcs_sorted.sort()

        # generate child matrix: distance between each pair
        child_RDMs[layer, child_idx] = np.zeros((len(select_child_feats), len(select_child_feats)))        
        # go through the sorted subindices
        for i in range(len(submatrix_idcs_sorted)):
            for j in range(len(submatrix_idcs_sorted)):
                # get the distance between the corresponding feature vectors
                child_RDMs[layer, child_idx][i,j] = dissimilarity_pearson(select_child_feats[submatrix_idcs.index(submatrix_idcs_sorted[i]),:], select_child_feats[submatrix_idcs.index(submatrix_idcs_sorted[j]),:])


for layer in range(num_layers):
    maxs = [] 
    for i in [layer]:
        for j in range(child_RDMs.shape[1]):
            maxs.append(np.max(child_RDMs[i][j])) 
    min_val = 0
    max_val = np.max(maxs)

    for child_idx in range(len(child_ids)):
        # print the child RDMs
        fig, ax = plt.subplots(1,1)
        img = ax.imshow(child_RDMs[layer, child_idx], cmap='viridis', vmin=min_val, vmax=max_val)
        ax.set_xticks(np.arange(num_classes))
        #ax.set_xticklabels(matrix_labels)
        ax.set_yticks(np.arange(num_classes))
        #ax.set_yticklabels(matrix_labels)
        plt.colorbar(img)
        plt.savefig('results/python_plot/RDMs_per_child/RDM_layer-' + str(layer) + '_child-' + child_ids[child_idx] + '.pdf')
        plt.close()
 
print("Plotted RDMs of all adults and children to: results/python_plot/RDMs_per_child")


# print for each child the mean and std
measure_mean = np.zeros((num_layers, len(child_ids)))
measure_std = np.zeros((num_layers, len(child_ids)))
measure_maxmin = np.zeros((num_layers, len(child_ids)))
 
# for comparisons against age / AQ
unique_idcs = [child_age_AQ_info[:,0].tolist().index(x) for x in child_ids]
child_aqs = child_age_AQ_info[unique_idcs,3]
child_ages = child_age_AQ_info[unique_idcs,1]
child_ids = child_age_AQ_info[unique_idcs,0]

# get the ages and aq values in the same order like child_ids
#child_ages = np.asarray([child_age_AQ_info[child_age_AQ_info[:,0].tolist().index(x),1] for x in child_ids])
#child_aqs = np.asarray([child_age_AQ_info[child_age_AQ_info[:,0].tolist().index(x),3] for x in child_ids])

# analyse the difference in drawing on different stimuli
# get measures such as mean and stddev over the matrix entries (excluding diagonal)
for layer in range(num_layers):
    for child_idx in range(len(child_ids)):
        upper_triangle_values = child_RDMs[layer][child_idx][np.triu_indices(child_RDMs[layer][child_idx].shape[0],k=1)]
        
        measure_mean[layer, child_idx] = np.mean(upper_triangle_values)
        measure_std[layer, child_idx] = np.std(upper_triangle_values)
        measure_maxmin[layer, child_idx] = np.max(upper_triangle_values) - np.min(upper_triangle_values)
        
        #print(child_ids[child_idx] + "\t" + str(measure_mean[layer, child_idx]) + "\t" + str(measure_std[layer, child_idx]) + "\t" + str(measure_maxmin[layer, child_idx]))

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(measure_mean[layer], measure_std[layer], '*')
    ax.set_xlabel('mean')
    ax.set_ylabel('std')
    plt.savefig('results/python_plot/RDMs_per_child/measures_layer-'+str(layer) +'_mean-to-std.png')
    plt.close()

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(measure_mean[layer], measure_maxmin[layer], '*')
    ax.set_xlabel('mean')
    ax.set_ylabel('max - min')
    plt.savefig('results/python_plot/RDMs_per_child/measures_layer-'+str(layer) +'_mean-to-maxmin.png')
    plt.close()

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(measure_std[layer], measure_maxmin[layer], '*')
    ax.set_xlabel('std')
    ax.set_ylabel('max - min')
    plt.savefig('results/python_plot/RDMs_per_child/measures_layer-'+str(layer) +'_std-to-maxmin.png')
    plt.close()

    # get distances of child matrices to the adult matrices
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(child_ages, measure_mean[layer], '*')
    ax.set_xlabel('age')
    ax.set_ylabel('mean')
    plt.savefig('results/python_plot/RDMs_per_child/measures_age-mean.png')
    plt.close()

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(child_ages, measure_std[layer], '*')
    ax.set_xlabel('age')
    ax.set_ylabel('std')
    plt.savefig('results/python_plot/RDMs_per_child/measures_age-std.png')
    plt.close()

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(child_ages, measure_maxmin[layer], '*')
    ax.set_xlabel('age')
    ax.set_ylabel('max - min')
    plt.savefig('results/python_plot/RDMs_per_child/measures_age-maxmin.png')
    plt.close()

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(child_ages, measure_maxmin[layer]+measure_mean[layer]+measure_std[layer], '*')
    ax.set_xlabel('age')
    ax.set_ylabel('all measures (+)')
    plt.savefig('results/python_plot/RDMs_per_child/measures_age-all.png')
    plt.close()

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(child_aqs, measure_mean[layer], '*')
    ax.set_xlabel('AQ')
    ax.set_ylabel('mean')
    plt.savefig('results/python_plot/RDMs_per_child/measures_AQ-mean.png')
    plt.close()

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(child_aqs, measure_std[layer], '*')
    ax.set_xlabel('AQ')
    ax.set_ylabel('std')
    plt.savefig('results/python_plot/RDMs_per_child/measures_AQ-std.png')
    plt.close()

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(child_aqs, measure_maxmin[layer], '*')
    ax.set_xlabel('AQ')
    ax.set_ylabel('max - min')
    plt.savefig('results/python_plot/RDMs_per_child/measures_AQ-maxmin.png')
    plt.close()

    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(child_aqs, measure_maxmin[layer]+measure_mean[layer]+measure_std[layer], '*')
    ax.set_xlabel('AQ')
    ax.set_ylabel('all measures (+)')
    plt.savefig('results/python_plot/RDMs_per_child/measures_AQ-all.png')
    plt.close()


all_data_score = np.empty((num_layers,),dtype=object)
for layer in range(num_layers):
    all_data_score[layer] = np.concatenate([np.zeros((103,1))+layer, np.reshape(child_ids, [-1,1]), np.reshape(child_ages, [-1,1]), np.reshape(child_aqs, [-1,1]), np.reshape(measure_mean[layer,:], [-1,1]), np.reshape(measure_std[layer,:], [-1,1]), np.reshape(measure_maxmin[layer,:], [-1,1])],axis=1)

df_style = pd.DataFrame(data=np.concatenate(all_data_score), columns = ['layer', 'Child', 'age', 'aq', 'scoreByMean', 'scoreByStd', 'scoreByMaxmin'])
df_style.to_csv('results/csv/score-evaluation.csv')
print('Generated csv: results/csv/score-evaluation.csv')

