# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 00:03:44 2019

@author: Stefanos Ioannou
"""

from SelectedData import * 
from sklearn.linear_model import LogisticRegression

clf = LogisticRegression(random_state=0, solver='lbfgs',
                         multi_class='multinomial')
clf = clf.fit(xtrain, ytrain)
ynew = clf.predict(xtest)
print(accuracy_score(ynew, ytest))

#Result : 0.6880570409982175 %