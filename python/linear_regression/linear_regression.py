import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import warnings
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn import metrics
from sklearn.metrics import r2_score
import pickle
warnings.filterwarnings("ignore")

dataframe = pd.read_csv("C:\\Users\\paulo\\PycharmProjects\\feature-selection\\hour.csv")

def showHist(dataframe):
    plt.rcParams["figure.figsize"] = 15, 18
    dataframe.hist()

showHist(dataframe)

def showPairPlot(dataframe):
    sns.pairplot(dataframe)

showPairPlot(dataframe)

arra = dataframe["dteday"].str.split("-")[0]

def extractColumnsYearMonthDateFromDteday(dataframe):
    dataframe['Year'] = dataframe['dteday'].str.split('-').str[0]
    dataframe['Year'] = dataframe['Year'].astype(int)
    dataframe['Month'] = dataframe['dteday'].str.split('-').str[1]
    dataframe['Month'] = dataframe['Month'].astype(int)
    dataframe['Date'] = dataframe['dteday'].str.split('-').str[2]
    dataframe['Date'] = dataframe['Date'].astype(int)

    dataframe = dataframe.drop(['dteday'], axis = 1)
    dataframe = dataframe.drop(['yr'], axis = 1)
    dataframe = dataframe.drop(['mnth'], axis = 1)
    return(dataframe)

print(dataframe.isna().sum())

dataframe = extractColumnsYearMonthDateFromDteday(dataframe)
DISTANCE_MID_TO_TAIL_IN_BOX_PLOT = 1.5

def outlier(x):
    high=0
    q1 = x.quantile(.25)
    q3 = x.quantile(.75)
    iqr = q3 - q1
    #***************************************************
    #          _________________
    #          |       |        | DISTANCE BETWEEN MID TO LOWER TAIL IS 1.5
    #----------|      mid       |------------
    #          |       |        |
    #          |________________|
    #
    #***************************************************
    low = q1 - DISTANCE_MID_TO_TAIL_IN_BOX_PLOT * iqr
    high += q3 + DISTANCE_MID_TO_TAIL_IN_BOX_PLOT * iqr
    outlier = (x.loc[(x < low) | (x > high)])
    return(outlier)


cont = outlier(dataframe['cnt']).count()

print(cont)

q1 = dataframe['cnt'].quantile(.25)
q3 = dataframe['cnt'].quantile(.75)
iqr = q3-q1
df_new = dataframe[~((dataframe['cnt'] < (q1 - 1.5 * iqr)) | (dataframe['cnt'] > (q3 + 1.5 * iqr)))]

x = df_new.drop(['cnt'], axis = 1)
y = df_new['cnt']
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.30, random_state=40)

lr = LinearRegression(copy_X = True, fit_intercept = True, n_jobs = 0, normalize = False)
LR = lr.fit(x_train, y_train)
LR_Pred = lr.predict(x_test)

print('ROOT EAN ERROR SQUARE:', np.sqrt(metrics.mean_squared_error(y_test, LR_Pred)))

print('R SQUARE:', r2_score(y_test, LR_Pred))

pickle.dump(lr, open('LinearRegression.pkl', 'wb'))
