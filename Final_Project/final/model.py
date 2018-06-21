# -*- coding: utf-8 -*-
"""
Created on Thu May 31 10:49:54 2018

@author: 羅際禎
"""

import numpy as np
import pandas as pd
from sklearn import linear_model, metrics, model_selection, svm, preprocessing, ensemble, tree

df = pd.read_csv('raw_data.csv', encoding='big5')
df.drop(['section', 'address', 'date', 'built_date'], inplace=True, axis=1)
df.dropna(inplace=True)

x = df.drop('unit_PRICE', axis=1).values
y = df.unit_PRICE.values

# categorize data
le = preprocessing.LabelEncoder()
for i in [14, 15, 16]:
    x[:, i] = le.fit_transform(x[:, i])
    
# models
RF = ensemble.RandomForestRegressor(random_state=1111)
result_RF = model_selection.cross_validate(RF, x, y, cv=5, n_jobs=-1, scoring=['neg_mean_squared_error', 'r2'], return_train_score=True)
print('5-fold validation mean R2 score:', result_RF['test_r2'].mean())
print('5-fold validation RMSE:', np.sqrt(-result_RF['test_neg_mean_squared_error'].mean()))
print(result_RF)

import pickle
print(len(x))
RF.fit(x, y)
filename = 'random_forest_model.pkl'
with open(filename, 'wb') as f:
    pickle.dump(RF, f)
    print('Model save to {}'.format(filename))