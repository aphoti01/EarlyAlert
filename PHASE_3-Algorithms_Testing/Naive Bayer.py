# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 00:03:44 2019
Gaussian Naive Bayers
@author: Stefanos Ioannou
"""

from SelectedData import *  
from sklearn.naive_bayes import GaussianNB

clf = GaussianNB()
clf = clf.fit(xtrain, ytrain)
ynew = clf.predict(xtest)
print(accuracy_score(ynew, ytest))

#Result : 0.6885918003565062 %