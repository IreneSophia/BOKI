#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 21 14:25:11 2025

@author: emba
"""

import pandas as pd
from sklearn.model_selection import StratifiedKFold
from scipy import io
import numpy as np

def divide_chunks(l, n):
    # looping till length l
    for i in range(0, len(l), n):
        yield l[i:i + n]
        
def dyad_permutation_stratified(dyad_df,n_fold):
    dyad_simp = dyad_df.drop_duplicates(subset=['dyad'])
    #dyad_simp = dyad_simp.sort_values(by='dyad',ascending=True)
    dyad_id = list(dyad_simp.dyad)
    dyad_label = list(dyad_simp.labelNo)
    dyad_dict = {}
    # map the dyad_id to subject id
    for d in dyad_id:
        dyad_pair = dyad_df.loc[dyad_df.dyad==d]
        dyad_dict[d] = list(dyad_pair.index)
    # define the splitter
    kfold = StratifiedKFold(n_splits=n_fold,shuffle=True)
    # get the dyad id in each fold
    trainInd = []
    testInd = []
    trainLabel = []
    testLabel = []
    for train, test in kfold.split(dyad_id,dyad_label):
        train_ind = []
        test_ind = []
        for tr in train:
            train_ind += dyad_dict[dyad_id[tr]]
        for ts in test:
            test_ind += dyad_dict[dyad_id[ts]]
        train_labels = list(dyad_df.loc[dyad_df.index.isin(train_ind)].labelNo)    
        test_labels = list(dyad_df.loc[dyad_df.index.isin(test_ind)].labelNo)
        one_count = test_labels.count(1)
        if one_count == len(test_labels):
            print (test_labels)
        trainInd.append(train_ind)
        testInd.append(test_ind)
        trainLabel.append(train_labels)
        testLabel.append(test_labels)
    return trainInd,testInd,trainLabel,testLabel

folder = '/media/emba/emba-2/ML_BOKI/ML_data/'

# set index to start with 1 to be compatible with matlab
dyad_df = pd.read_csv(folder + 'BOKI_NM_inputdata.csv', 
                      usecols=['dyad','labelNo'])
dyad_df.index += 1 
dyad_df.head()

cv2 = 10
p2 = 10
cv1 = 9
p1 = 1

cv2_train = []
cv2_test = []
cv2_train_label = []
cv2_test_label = []
cv1_all = []
cv1_labels = []

for p in range(p2):
    train2,test2,train_label2,test_label2 = dyad_permutation_stratified(dyad_df,cv2)
    cv2_train.append(train2)
    cv2_test.append(test2)
    cv2_train_label.append(train_label2)
    cv2_test_label.append(test_label2)
    
    cv1_col = []
    for t in train2:
        df_temp = dyad_df.loc[dyad_df.index.isin(t)]
        df_temp = df_temp.reset_index()
        df_temp.index += 1 
        #display(df_temp.head())
        cv1_dict = {}
        cv1_tr = []
        cv1_ts = []
        cv1_tr_label = []
        cv1_ts_label = []
        
        for p_in in range(p1):
            train1,test1,train_label1,test_label1 = dyad_permutation_stratified(df_temp,cv1)
            cv1_tr.append(train1)
            cv1_ts.append(test1)
            cv1_tr_label.append(train_label1)
            cv1_ts_label.append(test_label1)
        cv1_dict['TrainInd'] = cv1_tr
        cv1_dict['TestInd'] = cv1_ts
        cv1_dict['TrainLabel'] = cv1_tr_label
        cv1_dict['TestLabel'] = cv1_ts_label
        cv1_col.append(cv1_dict)
        
    cv1_all.append(cv1_col)
cv1_all = np.array(cv1_all)
cv2_train = np.array(cv2_train)
cv2_test = np.array(cv2_test)

folder = '/media/emba/emba-2/ML_BOKI/NeuroMiner/'

print ('save cv2')
io.savemat(folder+'TrainInd.mat', {'Train_cv2_raw': cv2_train})
io.savemat(folder+'TestInd.mat', {'Test_cv2_raw': cv2_test})
io.savemat(folder+'TrainLabel.mat', {'Train_cv2_label': cv2_train_label})
io.savemat(folder+'TestLabel.mat', {'Test_cv2_label': cv2_test_label})
print (cv2_train.shape)
print (cv2_test.shape)

print ('save cv1')
print (cv1_all.shape)
io.savemat(folder+'cvin.mat', {'cv1_all': cv1_all})