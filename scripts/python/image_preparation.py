import cv2
import numpy as np
import os
from keras.applications.vgg19 import preprocess_input
from tensorflow.keras.preprocessing.image import img_to_array
from tensorflow.keras.preprocessing.image import load_img
from tensorflow.keras.applications import imagenet_utils


def _maybe_generate_deleted_black(folder, image_path):
    tmp_directory = 'results/python_data/loaded_pics'
    os.makedirs(tmp_directory, exist_ok = True)

    # load the image
    img = cv2.imread(os.path.join(folder, image_path))
    imgWithoutBlack = cv2.imread(os.path.join(tmp_directory, image_path))
    
    if imgWithoutBlack is None:
        # find all pixel which are NOT blue
        delete_indices = np.where((img[:,:,0]>200) & (img[:,:,1] < 100) & (img[:,:,2] < 100) == False)
        # delete those and store image
        for (i,j) in zip(delete_indices[0], delete_indices[1]):
            img[i,j,:] = [255,255,255]
        cv2.imwrite(os.path.join(tmp_directory, image_path), img)
    return os.path.join(tmp_directory, image_path)


def load_images_for_network(folder, files_list, delete_black = False):
    images = []
    for f, filename in enumerate(files_list):
        
        # load image, with the black part either deleted or not
        if delete_black:
            image = load_img(_maybe_generate_deleted_black(folder, filename), target_size=(224,224))
        else:
            image = load_img(os.path.join(folder, files_list[f]), target_size=(224,224))

        # convert to grayscale
        image = image.convert('L')
        # convert to numpy array
        image = img_to_array(image)
        image = np.expand_dims(image, axis=0)
        image = np.tile(image, (1,1,1,3)) # "fake" RGB
        # preprocess according to network preprocessing
        image = imagenet_utils.preprocess_input(image)

        images.append(image)
    return images


def show_image(image_file):
    cv2.startWindowThread()
    cv2.namedWindow("preview")
    cv2.imshow("preview", image_file)
    cv2.waitKey(3000)
    cv2.destroyAllWindows()

