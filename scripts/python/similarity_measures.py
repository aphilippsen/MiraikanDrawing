import numpy as np
import scipy.stats as stats

def dissimilarity_euclidean(vec1, vec2):
    return np.mean(np.sqrt((vec1 - vec2)**2))

def dissimilarity_pearson(vec1, vec2):
    return 1 - stats.pearsonr(vec1, vec2)[0]

