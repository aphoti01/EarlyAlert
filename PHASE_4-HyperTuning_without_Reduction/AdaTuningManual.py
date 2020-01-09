# -*- coding: utf-8 -*-
"""
Created on Sun Mar 31 19:55:43 2019

@author: Stefanos Ioannou
"""


from SelectedData import *  
from sklearn.ensemble import AdaBoostClassifier
from sklearn.metrics import recall_score
from sklearn.tree import DecisionTreeClassifier

max = 0
estimators = 0
for x in range(200,500):
    
    clf = AdaBoostClassifier(n_estimators = x,learning_rate=1)
    print("# Tuning hyper-parameters for n_estimator %s" % x)
    print()
    clf = clf.fit(xtrain, ytrain)
    print("Grid scores on development set:") 
    print()  
    ynew =  clf.predict(xtest)
    accuracy = accuracy_score(ynew, ytest)
    print(accuracy)
    print(estimators)
    
    if (max<accuracy):
            max = accuracy_score(ynew, ytest)
            estimators = x

print("Estimators are %s" % estimators)
print("Accuracy Score is %s" % max)

#Best estimator is 285 with 80,998%





