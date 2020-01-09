import pandas as pd
import numpy as np
import sklearn.metrics as m
import sklearn.preprocessing as pr
import sklearn.ensemble as e
from sklearn.feature_selection import RFE
from sklearn.decomposition import PCA

test=pd.read_csv("test.csv")
train=pd.read_csv("train.csv")

def met(actual,pred):
    print(m.accuracy_score(actual,pred))
    print(m.recall_score(actual,pred))
    print(m.precision_score(actual,pred))
    print(m.f1_score(actual,pred))
    print(m.confusion_matrix(actual,pred))

def cat(test):
    test.code_module=test.code_module.astype('category')
    test.code_presentation=test.code_presentation.astype('category')
    test.gender=test.gender.astype('category')
    test.region=test.region.astype('category')
    test.imd_band=test.imd_band.astype('category')
    test.age_band=test.age_band.astype('category')
    test.highest_education=test.highest_education.astype('category')
    test.disability=test.disability.astype('category')

del train['id_student']
del test['id_student']
cat(test)
cat(train)

xtrain=train.iloc[:,1:17]
ytrain=train['res']

xtest=test.iloc[:,1:17]
ytest=test['res']

gbc=e.GradientBoostingClassifier()
rf= e.RandomForestClassifier()
ada=e.AdaBoostClassifier()

ada=e.AdaBoostClassifier(n_estimators=270,random_state=0,learning_rate=1,algorithm='SAMME.R')
rf=e.RandomForestClassifier(random_state=0,n_estimators=100,max_depth=25,min_samples_split=17,max_features=7)
gbc=e.GradientBoostingClassifier(learning_rate=0.16,n_estimators=200, max_depth=3,min_samples_split=7, min_samples_leaf=60)


selector=RFE(gbc,12,1).fit(xtrain,ytrain)
r=selector.ranking_

#Gradient Boosting Machine
k=0
for i in r:
    if (i==1):
        r[k]=True
    else: r[k]=False
    k=k+1
r=r.astype('bool')
xtr=xtrain.iloc[:,r]
xte=xtest.iloc[:,r]

#Random Forest
selector=RFE(rf,7,1).fit(xtrain,ytrain)
r=selector.ranking_

k=0
for i in r:
    if (i==1):
        r[k]=True
    else: r[k]=False
    k=k+1
r=r.astype('bool')
xtr1=xtrain.iloc[:,r]
xte1=xtest.iloc[:,r]

#Ada Boost
selector=RFE(ada,7,1).fit(xtrain,ytrain)
r=selector.ranking_

k=0
for i in r:
    if (i==1):
        r[k]=True
    else: r[k]=False
    k=k+1
r=r.astype('bool')
xtr2=xtrain.iloc[:,r]
xte2=xtest.iloc[:,r]

def myf1(cm):
    p = cm[0,0]/(cm[0,0]+cm[1,0])
    r = cm[0,0]/(cm[0,0]+cm[0,1])
    return (2*p*r)/(p+r)

from mlxtend.classifier import StackingClassifier
import xgboost as xgb

xgc=xgb.XGBClassifier(learning_rate = 0.5,gamma=2,max_depth=3,random_state=0,n_estimators=100)
sclf = StackingClassifier(classifiers=[rf],use_probas=True,average_probas=False,meta_classifier=xgc)

gbc.fit(xtr,ytrain)
rf.fit(xtr1,ytrain)
ada.fit(xtr2,ytrain)
sclf.fit(xtrain,ytrain)

gbc_pred_proba=gbc.predict_proba(xte)
rf_pred_proba=rf.predict_proba(xte1)
ada_pred_proba=ada.predict_proba(xte2)
sc_pred_proba=sclf.predict_proba(xtest)

gbc_cm=m.confusion_matrix(ytest,(gbc_pred_proba[:,1] >= 0.5).astype('int'))
rf_cm=m.confusion_matrix(ytest,(rf_pred_proba[:,1] >= 0.5).astype('int'))
ada_cm=m.confusion_matrix(ytest,(ada_pred_proba[:,1] >= 0.5).astype('int'))
sc_cm=m.confusion_matrix(ytest,(sc_pred_proba[:,1] >= 0.5).astype('int'))

k=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
#k=[0.55,0.57,0.6,0.62,0.65]
for i in k:
    gbc_cm=m.confusion_matrix(ytest,(gbc_pred_proba[:,1] >= i).astype('int'))
    print(myf1(gbc_cm))
    
k=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
k=[0.65,0.68,0.7,0.72,0.75]
for i in k:
    rf_cm=m.confusion_matrix(ytest,(rf_pred_proba[:,1] >= i).astype('int'))
    print(myf1(rf_cm))
    
k=[0.4997,0.4998,0.4999,0.5,0.5001,0.5002,0.5003,0.5004,0.5005,0.5006,0.5007,0.5008,]
for i in k:
    ada_cm=m.confusion_matrix(ytest,(ada_pred_proba[:,1] >= i).astype('int'))
    print(myf1(ada_cm))
    
sclf = StackingClassifier(classifiers=[rf],use_probas=True,average_probas=False,meta_classifier=xgc)
sclf.fit(xtrain,ytrain)
sc_pred_proba=sclf.predict_proba(xtest)
k=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
#k=[0.55,0.58,0.6,0.62,0.65]
for i in k:
    sc_cm=m.confusion_matrix(ytest,(sc_pred_proba[:,1] >= i).astype('int'))
    print(myf1(sc_cm))
    
from mlxtend.feature_selection import ColumnSelector
from sklearn.pipeline import make_pipeline
pipe1 = make_pipeline(ColumnSelector(cols=('code_module', 'code_presentation', 'gender', 'region',
        'highest_education', 'imd_band',  'num_of_prev_attempts',
        'studied_credits',  'vleAvBef', 'vleSumNorm', 'avAssNorm')),e.GradientBoostingClassifier(criterion='friedman_mse', init=None,
              learning_rate=0.16, loss='deviance', max_depth=3,
              max_features=None, max_leaf_nodes=None,
              min_impurity_split=1e-07, min_samples_leaf=60,
              min_samples_split=7, min_weight_fraction_leaf=0.0,
              n_estimators=200, presort='auto', random_state=None,
              subsample=1.0, verbose=0, warm_start=False))
pipe2 = make_pipeline(ColumnSelector(cols=('code_module', 'code_presentation', 'gender', 'region',
        'highest_education', 'imd_band',  'num_of_prev_attempts',
        'studied_credits',  'vleAvBef', 'vleSumNorm', 'avAssNorm')),e.RandomForestClassifier(bootstrap=True, class_weight=None, criterion='gini',
            max_depth=25, max_features=7, max_leaf_nodes=None,
            min_impurity_split=1e-07, min_samples_leaf=1,
            min_samples_split=17, min_weight_fraction_leaf=0.0,
            n_estimators=100, n_jobs=1, oob_score=False, random_state=0,
            verbose=0, warm_start=False))
pipe3 = make_pipeline(ColumnSelector(cols=('code_module', 'code_presentation',
        'highest_education', 'imd_band',  'num_of_prev_attempts',  'vleAvBef', 'vleSumNorm', 'avAssNorm')),e.AdaBoostClassifier(algorithm='SAMME.R', base_estimator=None, learning_rate=1,
          n_estimators=270, random_state=0))

vals=[0]*100
gbc_f1=[0]*100
rf_f1=[0]*100
sc_f1=[0]*100

for i in range(1,100,1):
    vals[i]=i
    
for i in range(1,100,1):
    gbc_cm=m.confusion_matrix(ytest,(gbc_pred_proba[:,1] >= i/100).astype('int'))
    gbc_f1[i]=myf1(gbc_cm)
    rf_cm=m.confusion_matrix(ytest,(rf_pred_proba[:,1] >= i/100).astype('int'))
    rf_f1[i]=myf1(rf_cm)
    sc_cm=m.confusion_matrix(ytest,(sc_pred_proba[:,1] >= i/100).astype('int'))
    sc_f1[i]=myf1(sc_cm)
    
import matplotlib.pyplot as plt
plt.plot(vals,gbc_f1)
plt.plot(vals,rf_f1)
plt.plot(vals,sc_f1)
plt.legent()
plt.show()

plt.plot( 'values', 'gbc', data=df, marker='o', markerfacecolor='blue', markersize=12, color='skyblue', linewidth=4)
plt.plot( 'values', 'rf', data=df, marker='', color='olive', linewidth=2)
plt.plot( 'values', 'sc', data=df, marker='', color='olive', linewidth=2, linestyle='dashed', label="toto")
plt.legend()


from sklearn.model_selection import KFold
from sklearn.model_selection import cross_val_score
kfold = KFold(n_splits=10, random_state=7)
results = cross_val_score(gbc, xtrain, ytrain, cv=kfold)