import scipy
from similarity_measures import dissimilarity_euclidean, dissimilarity_pearson
import matplotlib.pyplot as plt

# sorted by presentation condition
#classes = ['class-F', 'class-M', 'class-C', 'class-H', 'class-F', 'class-M', 'class-C', 'class-H', 'class-F', 'class-M', 'class-C', 'class-H']
#types = ['type-O', 'type-O', 'type-O', 'type-O', 'type-I',  'type-I',  'type-I',  'type-I',  'type-S', 'type-S', 'type-S', 'type-S']
#matrix_labels = ['F', 'M', 'C', 'H', 'F', 'M', 'C', 'H', 'F', 'M', 'C', 'H']

# sorted by category
classes = ['class-F', 'class-F', 'class-F', 'class-M', 'class-M', 'class-M', 'class-C', 'class-C', 'class-C', 'class-H', 'class-H', 'class-H']
types = ['type-O', 'type-I', 'type-S', 'type-O', 'type-I', 'type-S', 'type-O', 'type-I', 'type-S', 'type-O', 'type-I', 'type-S']
matrix_labels = ['O', 'F I', 'S', 'O', 'M I', 'S', 'O', 'C I', 'S', 'O', 'H I', 'S']

store_pic_format = "pdf"

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



# 12x12 matrix of similarity in different categories for each drawing, analogously to [Long et al. 2018]
# use spearmanr to compare the matrices

# splitting children into groups by age such that roughtly the same #drawings is contained in each group
select_age_group1 = (child_age_AQ_info[:,1]<=45)
select_age_group2 = (child_age_AQ_info[:,1]>45) & (child_age_AQ_info[:,1]<=58)
select_age_group3 = (child_age_AQ_info[:,1]>58) & (child_age_AQ_info[:,1]<=67)
select_age_group4 = (child_age_AQ_info[:,1]>67)

### for age groups
num_layers = 7
adult_RDM = np.zeros((num_layers, len(classes),len(classes)))
kid1_RDM = np.zeros((num_layers, len(classes),len(classes)))
kid2_RDM = np.zeros((num_layers, len(classes),len(classes)))
kid3_RDM = np.zeros((num_layers, len(classes),len(classes)))
kid4_RDM = np.zeros((num_layers, len(classes),len(classes)))

num_classes = 12

for layer in range(num_layers):
    feats_adult = features_per_layer[layer][adult_indices]
    feats_kids = features_per_layer[layer][child_indices]

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
            
    # matrix for children in group 1
    for i in range(len(classes)):
        for j in range(len(classes)):
            select_class_i = [k==classes[i] for k in child_pic_class]
            select_type_i = [k==types[i] for k in child_pic_type]
            subset_indices_i = np.asarray(np.where([a and b and c for a, b, c in zip(select_type_i, select_class_i, select_age_group1)])[0],dtype=int)
            select_class_j = [k==classes[j] for k in child_pic_class]
            select_type_j = [k==types[j] for k in child_pic_type]
            subset_indices_j = np.asarray(np.where([a and b and c for a, b, c in zip(select_type_j, select_class_j, select_age_group1)])[0],dtype=int)
            
            # get mean of class i
            mean_i = np.mean(feats_kids[subset_indices_i],axis=0) 
            # get mean of class j        
            mean_j = np.mean(feats_kids[subset_indices_j],axis=0) 
            kid1_RDM[layer,i,j] = dissimilarity_pearson(mean_i, mean_j)


    # matrix for children in group 2
    for i in range(len(classes)):
        for j in range(len(classes)):
            select_class_i = [k==classes[i] for k in child_pic_class]
            select_type_i = [k==types[i] for k in child_pic_type]
            subset_indices_i = np.asarray(np.where([a and b and c for a, b, c in zip(select_type_i, select_class_i, select_age_group2)])[0],dtype=int)
            select_class_j = [k==classes[j] for k in child_pic_class]
            select_type_j = [k==types[j] for k in child_pic_type]
            subset_indices_j = np.asarray(np.where([a and b and c for a, b, c in zip(select_type_j, select_class_j, select_age_group2)])[0],dtype=int)
            
            # get mean of class i
            mean_i = np.mean(feats_kids[subset_indices_i],axis=0) 
            # get mean of class j        
            mean_j = np.mean(feats_kids[subset_indices_j],axis=0) 
            kid2_RDM[layer,i,j] = dissimilarity_pearson(mean_i, mean_j)
            
    # matrix for children in group 3
    for i in range(len(classes)):
        for j in range(len(classes)):
            select_class_i = [k==classes[i] for k in child_pic_class]
            select_type_i = [k==types[i] for k in child_pic_type]
            subset_indices_i = np.asarray(np.where([a and b and c for a, b, c in zip(select_type_i, select_class_i, select_age_group3)])[0],dtype=int)
            select_class_j = [k==classes[j] for k in child_pic_class]
            select_type_j = [k==types[j] for k in child_pic_type]
            subset_indices_j = np.asarray(np.where([a and b and c for a, b, c in zip(select_type_j, select_class_j, select_age_group3)])[0],dtype=int)
            
            # get mean of class i
            mean_i = np.mean(feats_kids[subset_indices_i],axis=0) 
            # get mean of class j        
            mean_j = np.mean(feats_kids[subset_indices_j],axis=0) 
            kid3_RDM[layer,i,j] = dissimilarity_pearson(mean_i, mean_j)

    # matrix for children in group 4
    for i in range(len(classes)):
        for j in range(len(classes)):
            select_class_i = [k==classes[i] for k in child_pic_class]
            select_type_i = [k==types[i] for k in child_pic_type]
            subset_indices_i = np.asarray(np.where([a and b and c for a, b, c in zip(select_type_i, select_class_i, select_age_group4)])[0],dtype=int)
            select_class_j = [k==classes[j] for k in child_pic_class]
            select_type_j = [k==types[j] for k in child_pic_type]
            subset_indices_j = np.asarray(np.where([a and b and c for a, b, c in zip(select_type_j, select_class_j, select_age_group4)])[0],dtype=int)
            
            # get mean of class i
            mean_i = np.mean(feats_kids[subset_indices_i],axis=0) 
            # get mean of class j        
            mean_j = np.mean(feats_kids[subset_indices_j],axis=0) 
            kid4_RDM[layer,i,j] = dissimilarity_pearson(mean_i, mean_j)


    # Plot the RDMs
    fig, axes = plt.subplots(nrows=1, ncols=5, figsize=(30, 5))
    plt.rcParams.update({'font.size': 10})
    min_val = 0
    max_val = np.max([np.max(kid1_RDM[layer]), np.max(kid2_RDM[layer]), np.max(kid3_RDM[layer]), np.max(kid4_RDM[layer]), np.max(adult_RDM[layer])])

    ax = axes.flat[0]
    im = ax.imshow(kid1_RDM[layer], cmap='viridis', vmin=min_val, vmax=max_val)
    ax.set_xticks(np.arange(num_classes))
    ax.set_xticklabels(matrix_labels)
    ax.set_yticks(np.arange(num_classes))
    ax.set_yticklabels(matrix_labels)

    ax = axes.flat[1]
    im = ax.imshow(kid2_RDM[layer], cmap='viridis', vmin=min_val, vmax=max_val)
    ax.set_xticks(np.arange(num_classes))
    ax.set_xticklabels(matrix_labels)
    ax.set_yticks(np.arange(num_classes))
    ax.set_yticklabels(matrix_labels)

    ax = axes.flat[2]
    im = ax.imshow(kid3_RDM[layer], cmap='viridis', vmin=min_val, vmax=max_val)
    ax.set_xticks(np.arange(num_classes))
    ax.set_xticklabels(matrix_labels)
    ax.set_yticks(np.arange(num_classes))
    ax.set_yticklabels(matrix_labels)

    ax = axes.flat[3]
    im = ax.imshow(kid4_RDM[layer], cmap='viridis', vmin=min_val, vmax=max_val)
    ax.set_xticks(np.arange(num_classes))
    ax.set_xticklabels(matrix_labels)
    ax.set_yticks(np.arange(num_classes))
    ax.set_yticklabels(matrix_labels)

    ax = axes.flat[4]
    im = ax.imshow(adult_RDM[layer], cmap='viridis', vmin=min_val, vmax=max_val)
    ax.set_xticks(np.arange(num_classes))
    ax.set_xticklabels(matrix_labels)
    ax.set_yticks(np.arange(num_classes))
    ax.set_yticklabels(matrix_labels)

    #fig.subplots_adjust(bottom=0.1, top=0.9, left=0.1, right=0.8,
    #                    wspace=0.02, hspace=0.02)

    # add an axes, lower left corner in [0.83, 0.1] measured in figure coordinate with axes width 0.02 and height 0.8

    # set the colorbar
    cbar = fig.colorbar(im, ax=axes.ravel().tolist(), shrink=0.95)
    cbar.set_ticks([min_val, max_val])
    cbar.set_ticklabels(['similar', 'dissimilar'])
    plt.savefig('results/python_plot/rdms-with-age-layer-' + str(layer) + '.pdf')
    print('Created plot of RDM matrices: results/python_plot/rdms-with-age-layer-' + str(layer) + '.pdf')

# fixing small errors: distance between same pictures should be 0
for i in range(12):
    kid1_RDM[6][i,i] = 0
    kid2_RDM[6][i,i] = 0
    kid3_RDM[6][i,i] = 0
    kid4_RDM[6][i,i] = 0
    adult_RDM[6][i,i] = 0

# extract unique values and write to csv for analysis
with open('results/csv/RDM_values.csv', 'w') as f:
    f.write("group,dist\n")
    values = scipy.spatial.distance.squareform(kid1_RDM[6])
    for i in range(len(values)):
        f.write("kid1," + str(values[i]) + "\n")    
    values = scipy.spatial.distance.squareform(kid2_RDM[6])
    for i in range(len(values)):
        f.write("kid2," + str(values[i]) + "\n")
    values = scipy.spatial.distance.squareform(kid3_RDM[6])
    for i in range(len(values)):
        f.write("kid3," + str(values[i]) + "\n")
    values = scipy.spatial.distance.squareform(kid4_RDM[6])
    for i in range(len(values)):
        f.write("kid4," + str(values[i]) + "\n")
    values = scipy.spatial.distance.squareform(adult_RDM[6])
    for i in range(len(values)):
        f.write("adult," + str(values[i]) + "\n")

