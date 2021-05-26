import keras
from keras import backend as K 
import numpy as np
import cv2
import imutils
import tensorflow as tf

def _flatten(x):
    # perform spatial averaging
    return x.mean(3).mean(2)

def _get_layer_output(model, layer_idx, input_images):
    print('Generate activations for network layer ' + str(layer_idx) + " (" + model.layers[layer_idx].name + ")")
    layer_outputs = np.empty((len(input_images),), dtype=object)
    for i in range(len(input_images)):
        output_fct = K.function([model.layers[0].input], [model.layers[layer_idx].output])
        layer_outputs[i] = output_fct(tf.cast(input_images[i], tf.float32))[0]
        #tf.cast(input_images[i], tf.float32)
        # average across spatial position of image filters
        if model.layers[layer_idx].name.endswith('pool'):
            layer_outputs[i] = _flatten(layer_outputs[i])

    return layer_outputs

def get_layer_activations(input_images):
    
    # define which layers to include: 5 pooling layers and 2 fully-connected layers
    layer_1 = 3
    layer_2 = 6
    layer_3 = 11
    layer_4 = 16
    layer_5 = 21
    layer_6 = 23
    layer_7 = 24
    layer_indices = [layer_1, layer_2, layer_3, layer_4, layer_5, layer_6, layer_7]
            
    # load the network
    vgg19 = keras.applications.VGG19(include_top=True)
    
    # get the output classification for the images
    #output = [vgg19.predict(img) for img in imgs_net]
    #[np.argmax(o) for o in output]
    #classifs = np.asarray([np.argmax(o) for o in output])
    
    # for each layer, load the representations of the input images
    features_per_layer = np.empty((len(layer_indices),), dtype=object)
    l=0
    for layer_idx in layer_indices:
        outputs = _get_layer_output(vgg19, layer_idx, input_images)
        features_per_layer[l] = np.concatenate(outputs)
        l += 1

    return features_per_layer

