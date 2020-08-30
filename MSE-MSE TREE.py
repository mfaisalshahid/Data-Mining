import pandas as pd
import numpy as np
import statsmodels.api as sm 
import matplotlib.pyplot as plt
from sklearn import tree

data= pd.read_csv("/Users/muhammadshahid/Desktop/ASS06_Data.csv")

n = 20
m = 1460
pred_list = []
pred_list_tree=[]
for i in range(n):
    pred_list.append([0] * m)
    pred_list_tree.append([0] * m)

for i in range(0, 20):
    datasample= data.sample(n=1460, replace=True).reset_index(drop=True)
    x = datasample.loc[:,["LotArea","TotalBsmtSF","GarageCars","AGE","TotalArea"]]
    x = sm.add_constant(x) 
    y = datasample["SalePrice"]
    model = sm.OLS(y, x).fit()
    predictions = model.predict(x)
    pred_list[i]=predictions  

bag=sum(pred_list)/20

sum_mse = []
for i in range (0, 20):
    sum_mse.append((pred_list[i]-bag)**2)

mse=sum(sum_mse)/20

df = pd.DataFrame(pred_list)

df=df.T

boxplot = df.boxplot()
plt.show()

# part2

for i in range(0, 20):
    datasample= data.sample(n=1460, replace=True).reset_index(drop=True)
    x = datasample.loc[:,["LotArea","TotalBsmtSF","GarageCars","AGE","TotalArea"]]
    y = datasample["SalePrice"]
    clf = tree.DecisionTreeClassifier()
    clf = clf.fit(x, y)
    predictions=clf.predict(x)
    pred_list_tree[i]=predictions
bag_tree=sum(pred_list_tree)/20
sum_mse_tree = []
for i in range (0, 20):
    sum_mse_tree.append((pred_list_tree[i]-bag_tree)**2)
mse_tree=sum(sum_mse_tree)/20
df_tree = pd.DataFrame(pred_list_tree)
df_tree=df_tree.T
boxplot = df_tree.boxplot()
plt.show()
