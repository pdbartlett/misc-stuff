
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.linear_model import LinearRegression

df = pd.read_csv('Boston.csv')
print(df.describe())
X = df.loc[:, ['lstat']]
Y = df['medv']
lr = LinearRegression()
lr.fit(X, Y)
Y_pred = lr.predict(X)
res = Y - Y_pred
print(lr.coef_, lr.intercept_, lr.score(X, Y))
plt.scatter(X, Y)
plt.plot(X, Y_pred, color='red')
plt.show()
plt.scatter(X, res)
plt.show()
