# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 00:03:44 2019
This file contains the best parameters with 
GridSearch. This proccess takes much time to run.
MIN_EST AND MAX_EST 
@author: Stefanos Ioannou
"""

from SelectedData import * 
from sklearn.ensemble import RandomForestClassifier
from sklearn.datasets import make_classification

MIN_EST = 100
MAX_EST = 501

MIN_DEPTH = 1
MAX_DEPTH = 18

TUNE_P = [{'n_estimators': range(MIN_EST,MAX_EST)}]

clf = GridSearchCV(RandomForestClassifier(), TUNE_P,cv= 2,scoring = 'recall')   

print("# Tuning hyper-parameters for n_estimator %s to %s and Maximum Depth of the tree - %s to %s" % (MIN_EST, MAX_EST,MIN_DEPTH,MAX_DEPTH))
print()
print("Development Set:") 
print()
clf = clf.fit(xtrain, ytrain)
means = clf.cv_results_['mean_test_score']
stds = clf.cv_results_['std_test_score']

for mean, std, params in zip(means, stds, clf.cv_results_['params']):
    print("%0.6f (+/-%0.6f) for %r" % (mean, std * 2, params))
    print()
    
print("Development Set:") 
print(clf.best_params_)  

pred_prob=(clf.predict_proba(xtest)[:,1]>0.5).astype('int')
accuracy = accuracy_score( ytest,pred_prob)

print ("Accuracy is %s", accuracy)
print(classification_report(pred_prob, ytest,digits = 6))
