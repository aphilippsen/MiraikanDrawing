import cv2
import numpy as np
import os
import sys

from image_preparation import show_image, load_images_for_network
from load_info_data import load_participants_info
from vgg19_activations import get_layer_activations, check_classifications

# call with command line argument: Delete (delete black parts of drawing) or Full (do not delete black parts of drawing before processing)

if sys.argv[2] == 'Full':
    delete_black = False # use black parts as well for analysis
elif sys.argv[2] == 'Delete':
    delete_black = True # delete black parts
else:
    print('Please set a valid argument (Full or Delete)')
    sys.exit()
print("delete_black is set to " + str(delete_black))

# load the list of files
file_to_open = 'data/all-drawings.txt'
f = open(file_to_open, 'r')
image_paths = f.readlines()
image_paths = [x[:-1] for x in image_paths]
f.close()

all_pictures_folder = 'data/all_drawings/'

# loading participant information
children, pic_type, pic_class, age_AQ_info = load_participants_info(image_paths, 'data/participants-age-aq-incl-adults.txt')

print("Picture mode: " + str(delete_black))

print("Load features from  network...")
# load or generate the features from different network layers
try:
    features_per_layer = np.load('results/python_data/vgg19_features-' + str(delete_black) + '.npy', allow_pickle = True)        
except:
    print("Load images...")
    imgs_net = load_images_for_network(all_pictures_folder, image_paths, delete_black=delete_black)

    print("Generate activations...")
    features_per_layer = get_layer_activations(imgs_net)
    np.save('results/python_data/vgg19_features-' + str(delete_black) + '.npy', features_per_layer)

