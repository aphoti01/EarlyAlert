# -*- coding: utf-8 -*-
"""
Created on Tue Apr  2 16:41:24 2019
GridSearch for Gradient Boost
@author: Stefanos Ioannou
"""

from sklearn.ensemble import GradientBoostingClassifier
from SelectedData import *  
from sklearn.metrics import *
import numpy

MIN_EST = 100
MAX_EST = 501

TUNE_P = [{'n_estimators': range(MIN_EST,MAX_EST), 'loss': ['deviance'],'learning_rate':numpy.arange(0.9, 1, 0.1)}]

clf = GridSearchCV(GradientBoostingClassifier(), TUNE_P,cv= 2,scoring = 'accuracy')   

print("# Tuning hyper-parameters for n_estimator %s TO %s" %(MIN_EST, MAX_EST) )
print()
clf = clf.fit(xtrain, ytrain)
means = clf.cv_results_['mean_test_score']
stds = clf.cv_results_['std_test_score']

for mean, std, params in zip(means, stds, clf.cv_results_['params']):
    print("%0.6f (+/-%0.6f) for %r" % (mean, std * 2, params))
    print()
    
print("Development Set:") 
print(clf.best_params_)  

ynew =  clf.predict(xtest)
accuracy = accuracy_score(ynew, ytest)

print ("Accuracy is %s", accuracy)
print(classification_report(ynew, ytest,digits = 6))





