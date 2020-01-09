# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 00:03:44 2019

@author: Stefanos Ioannou 
"""


from SelectedData import *  
from sklearn import tree

clf = tree.DecisionTreeClassifier()
clf = clf.fit(xtrain, ytrain)
ynew = clf.predict(xtest)
print(accuracy_score(ynew, ytest))

#Result : 0.7324420677361854 %