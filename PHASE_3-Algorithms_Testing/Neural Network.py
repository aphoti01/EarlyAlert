# -*- coding: utf-8 -*-
"""
Created on Fri Mar 22 00:03:44 2019

@author: Stefanos Ioannou
"""
from SelectedData import * 
from sklearn.neural_network import MLPClassifier

clf = MLPClassifier(solver='lbfgs', alpha=1e-5, 
                    hidden_layer_sizes=(5, 2), random_state=1)

clf = clf.fit(xtrain, ytrain)
ynew = clf.predict(xtest)
print(accuracy_score(ynew, ytest))

#Result : 0.6880570409982175 %
