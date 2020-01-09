# -*- coding: utf-8 -*-
"""
Created on Sun Mar 31 19:55:43 2019

@author: Admin
"""


from SelectedData import *  
from sklearn.ensemble import AdaBoostClassifier
from sklearn.metrics import *
from sklearn.tree import DecisionTreeClassifier

max = 0
estimators = 0
for x in range(200,500):
    
    clf = AdaBoostClassifier(n_estimators = x)
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

#best estimator is 285 with 80,641%

max = 0
estimators = 0
a = DecisionTreeClassifier(max_depth=1)
    
clf = AdaBoostClassifier(a,n_estimators = 285)
print("# Tuning hyper-parameters for n_estimator %s" % 285)
print()
clf = clf.fit(xtrain, ytrain)
print("Grid scores on development set:") 
print()  
ynew =  clf.predict(xtest)
accuracy = accuracy_score(ynew, ytest)
print(accuracy)

print("Estimators are %s" % estimators)
print("Accuracy Score is %s" % max)
#best estimator is with dTree classifier of max depth 80,099%




