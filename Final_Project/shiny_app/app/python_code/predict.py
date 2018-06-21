# -*- coding: utf-8 -*-
"""
Created on Wed Jun 20 21:27:15 2018

@author: 羅際禎
"""

import numpy as np
import pandas as pd
import pickle
from sklearn import linear_model, metrics, model_selection, svm, preprocessing, ensemble, tree
import scipy

def lat2y(pts):
    center = 25.048734
    y_ratio = 110768.8  # latitude
    tmp = pts - center
    tmp *= y_ratio
    return tmp

def lon2x(pts):
    center = 121.514231
    x_ratio = 100912.0  # longitude
    tmp = pts - center
    tmp *= x_ratio
    return tmp

def find_closest_dist(data, pts):
    KD = scipy.spatial.KDTree(pts)
    return KD.query(data)[0]    # the nearest distance

def predict_rental():
    model_name = 'random_forest_model.pkl'
    with open(model_name, 'rb') as f:
        model = pickle.load(f)
    with open('label_encoder.pkl', 'rb') as f:
        le = pickle.load(f)
        
    tmp = pd.read_csv('../responses/user_predict.csv').values[-1, :].flatten()
    print(tmp)
    tmp[[7, 8, 9]] = le.transform(tmp[[7, 8, 9]])
    
    
    loc = (tmp[0], tmp[1])
    x = np.load('default_input.npy')
    #x = np.zeros((1,24))
    
    df = pd.read_csv('bus_station_data.csv', sep=' ')
    bus_data = np.zeros((len(df), 2))
    bus_data[:, 1] = lat2y(df.latitude.values)
    bus_data[:, 0] = lon2x(df.longitude.values)
    x[0,20] = find_closest_dist(loc, bus_data)
    
    df = pd.read_csv('MRT_station_data.csv', sep=' ')
    mrt_data = np.zeros((len(df), 2))
    mrt_data[:, 1] = lat2y(df.lng.values)  # note the data notation is wrong
    mrt_data[:, 0] = lon2x(df.lat.values)
    x[0,21] = find_closest_dist(loc, mrt_data)
    
    df = pd.read_csv('park.final.csv', sep=' ')
    park_data = np.zeros((len(df), 2))
    park_data[:, 1] = lat2y(df.Latitude.values)
    park_data[:, 0] = lon2x(df.Longitude.values)
    x[0,22] = find_closest_dist(loc, park_data)
    
    df = pd.read_csv('store_all.csv', sep=' ')
    store_data = np.zeros((len(df), 2))
    store_data[:, 1] = lat2y(df.lat.values)
    store_data[:, 0] = lon2x(df.lng.values)
    x[0,23] = find_closest_dist(loc, store_data)
    
    mapping = { # input => x
            0:8,     # region
            1:12,    # use
            2:3,     # total_size
            3:19,    # house_age
            4:11,    # build_state
            5:16,    # n_room
            6:17,    # n_hall
            7:18,    # n_bath
            8:13,    # is_comp
            9:14,    # is_manage
            10:5,    # is_furn
            11:0,   # lat
            12:1    # lng
            }
    
    for i in range(13):
        x[0, mapping[i]] = tmp[i]
    y = model.predict(x)[0]
    with open('../responses/return.txt', 'w') as f:
        f.write(str(y*tmp[2]))
    return y*tmp[2]

if __name__ == "__main__":
    print(predict_rental())