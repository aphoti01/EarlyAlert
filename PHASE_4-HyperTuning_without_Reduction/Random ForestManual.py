# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 00:03:44 2019
Two conditional loops.
One on max depth and the other on n estimators for random forest
@author: Stefanos Ioannou
"""

from SelectedData import * 
from sklearn.ensemble import RandomForestClassifier
from sklearn.datasets import make_classification

max = 0 ;
maxDepth = 0


for x in range(10,1000):
    clf = RandomForestClassifier(n_estimators=100, max_depth=x, random_state=0)
    
    print("# Tuning hyper-parameters for max_depth %s" % x)
    print()
    clf = clf.fit(xtrain, ytrain)
    print("Development Set:") 
    print()  
    ynew =  clf.predict(xtest)
    accuracy = accuracy_score(ynew, ytest)
    print(accuracy)
    print(estimators)
    
    if (max < accuracy):
            max  = accuracy
            maxDepth = x

print("Estimators are %s" % maxDepth)
print("Accuracy Score is %s" % max)

# Max Depth is 17 with recall approaching 81,05%

# Trade off between n_estimators and max depth 
# Setting Learning Rate very low and n_estimators very high

max = 0
maxEstimators = 0

for x in range(10,1000):
    clf = RandomForestClassifier(n_estimators=x, max_depth=17, random_state=0)
    
    print()
    clf = clf.fit(xtrain, ytrain)
    print("Development Set:") 
    print()  
    ynew =  clf.predict(xtest)
    accuracy = accuracy_score(ynew, ytest)

    print(accuracy)
    print(estimators)
    
    if (max < accuracy):
            max = accuracy
            maxEstimators = x
            
print("Accuracy Score is %s" % accuracy)
print("Estimators are %s" % estimators)

#Best value for n_ estimator is 1000 with score of  81,176%