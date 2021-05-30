import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import os
from shutil import copyfile, copy


class PictureExplorer:
    def __init__(self, data_reduced, folder, images_list, radius = 0.005, copy_images=True):
        self.data_reduced = data_reduced
        self.folder = folder
        self.images_list = images_list
        self.results_folder = "pictureExplorer_current"
        self.radius = radius
        self.copy_images = copy_images
        
    def __call__(self, event):
        print("clicked at: " + str(event.xdata) + ", " + str(event.ydata))
        
        # get closest point
        scale_factor_y = np.max(self.data_reduced[:,0]) / np.max(self.data_reduced[:,1])
        print(scale_factor_y)
        data_reduced_scaled = self.data_reduced * [1,scale_factor_y]
        input_point = [event.xdata, event.ydata*scale_factor_y]
        
        min_idx = np.argmin(np.sum((data_reduced_scaled - np.tile(input_point, (self.data_reduced.shape[0],1)))**2,axis=1))
        idcs = [min_idx]
        
        # create folder
        os.makedirs(self.results_folder, exist_ok = True)
        # potentially delete already existing content
        try:
            filelist = [ f for f in os.listdir(self.results_folder)]
            for f in filelist:
                os.remove(os.path.join(self.results_folder, f))
        except:
            pass
        
        # copy neighboring pictures there
        if self.copy_images:
            for i in range(len(idcs)):
                image_path = os.path.join(self.folder, self.images_list[idcs[i]])
                copyfile(image_path, os.path.join(self.results_folder, self.images_list[idcs[i]]))
        else:
            for i in range(len(idcs)):
                print(self.images_list[idcs[i]])
                import glob

                print(os.path.join(self.results_folder, self.images_list[idcs[i]] + "_*"))
                mylist = [f for f in glob.glob(os.path.join(self.folder, self.images_list[idcs[i]] + "_*"))]
                print(mylist)
                print(len(mylist))
                for j in mylist:
                    print(j)
                    from shutil import copy
                    copy(j, self.results_folder)

