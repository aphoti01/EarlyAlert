# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 00:03:44 2019

@author: Stefanos Ioannou 
"""
import pandas as pd
import numpy as np
import sklearn.metrics as m
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import accuracy_score
from sklearn.datasets import make_classification
from sklearn.metrics import classification_report

test=pd.read_csv("test.csv")
train=pd.read_csv("train.csv")

xtrain=train.iloc[:,1:15]
ytrain=train['res']

xtest=test.iloc[:,1:15]
ytest=test['res']



