# Written by Fantin Mesny (mesny@mpipz.mpg.de) - October 2020 

import pandas as pd
from sklearn import svm
from sklearn.preprocessing import StandardScaler
from sklearn.feature_selection import RFECV
from sklearn.model_selection import KFold

### PARSING
df0=pd.read_csv('RAs_and_rescue.csv').drop(columns=['genotype']).set_index('names')

### SCALING RELATIVE ABUNDANCES
scaler = StandardScaler()
df = pd.DataFrame(scaler.fit_transform(df0.drop(columns='group')))
df.index=df0.index
df.columns=df0.columns[:-1]
df=df.merge(df0[['group']], left_index=True, right_index=True)

### SVM-RFE
clf = svm.SVC(kernel='linear')
rfe = RFECV(clf, step=1, cv=KFold(n_splits=df.shape[0]),min_features_to_select=1,n_jobs=40)
rfe.fit(df.drop(columns=['group']),df['group'])

# RETURNING RESULTS
print('\nscore= ', sum(rfe.grid_scores_)/len(rfe.grid_scores_))
print('\n')
cols=rfe.get_support(indices=True)
cols =[df.columns[:-1][i] for i in cols]
coeffs=pd.DataFrame()
coeffs['svm_coeff']=list(rfe.estimator_.coef_[0])
coeffs['names']=cols
print(coeffs.set_index('names').sort_values(by='svm_coeff'))
print(len(coeffs),' features kept')

coeffs.to_csv('svm_output.csv')



