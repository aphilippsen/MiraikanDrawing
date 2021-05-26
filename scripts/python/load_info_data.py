import numpy as np
import pandas as pd

def load_participants_info(image_paths, participants_file):

    children = [x.split('_')[0] for x in image_paths]
    pic_type = [x.split('_')[2] for x in image_paths]
    pic_class = [x.split('_')[3].split('.')[0] for x in image_paths]

    df = pd.read_csv(participants_file, delimiter='\t')
    df_array = df.values # numpy array
    
    participant_info = np.empty((len(children),df_array.shape[1]), dtype=object)
    
    for i in range(len(children)):
        participant_info[i,:] = df_array[np.where(df_array == children[i])[0][0],:]
        
    return children, pic_type, pic_class, participant_info
    
