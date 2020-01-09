# -*- coding: utf-8 -*-
"""
Created on Tue Apr  2 16:41:24 2019
Two conditional loops : one on n _estimators
and one for learning rate also on max leaf and min_sample
@author: Stefanos Ioannou
"""

from sklearn.ensemble import GradientBoostingClassifier
from SelectedData import *  
from sklearn.metrics import *
import numpy


max = 0
n_estimators = 0

for x in range(100,1000):
    clf = GradientBoostingClassifier(loss='deviance', learning_rate=0.1, n_estimators=x, subsample=1.0, criterion='friedman_mse',
                                 min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_depth=3, min_impurity_decrease=0.0, min_impurity_split=None, init=None, random_state=None, max_features=None,
                                 verbose=0, max_leaf_nodes=None, warm_start=False, presort='auto', validation_fraction=0.1, n_iter_no_change=None, tol=0.0001)
    print("# Tuning hyper-parameters for n_estimator %s" % x)
    print()
    clf = clf.fit(xtrain, ytrain)
    print("Development Set:") 
    print()  
    ynew =  clf.predict(xtest)
    accuracy = accuracy_score(ynew, ytest)
    
    print(accuracy)
    print(" for ")
    print(x)
    
    if (max<accuracy):
            max = accuracy
            n_estimators = x

print("Estimators are %s" % n_estimators)
print("Accuracy Score is %s" % max)

# Best estimator is 204 with recall approaching 81,468%

# But the best estimator with learning rate 0.01 is 500 is 81,493%
# Trade off between learning rate and n_estimators 
# Setting Learning Rate very low and n_estimators very high

max = 0
learning_rateMax = 0
for x in numpy.arange(0.01, 0.9, 0.01):
    clf = GradientBoostingClassifier(loss='deviance', learning_rate=x, n_estimators=500, subsample=1.0, criterion='friedman_mse',
                                     min_samples_split=2, min_samples_leaf=1, min_weight_fraction_leaf=0.0, max_depth=32, min_impurity_decrease=0.0, min_impurity_split=None, 
                                     init=None, random_state=None, max_features=None,
                                     verbose=0, max_leaf_nodes=100, warm_start=False, presort='auto', validation_fraction=0.1, n_iter_no_change=None, tol=0.0001)
    
    print("# Tuning hyper-parameters for n_estimators %s" % 204)
    print()
    clf = clf.fit(xtrain, ytrain)
    print("Development Set:") 
    print()  
    ynew =  clf.predict(xtest)
    accuracy = accuracy_score(ynew, ytest)
        
    print(accuracy)
    print(" for ")
    print(x)
    if (max<accuracy):
            max = accuracy
            learning_rateMax = x

print("Learning Rate are %s" % learning_rateMax)
print("Accuracy Score is %s" % max)
#Best learning rate is 0.01




