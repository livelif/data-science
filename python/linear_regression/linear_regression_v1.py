import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import category_encoders as ce
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression
from mlxtend.feature_selection import SequentialFeatureSelector as SFS
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error

train_data = pd.read_csv("C:\\Users\\paulo\\PycharmProjects\\feature-selection\\train_v9rqX0R.csv")

print(train_data.isna().sum())

train_data.Item_Weight.fillna(train_data.Item_Weight.mean(), inplace = True)

train_data.Outlet_Size.fillna(train_data.Outlet_Size.mode(), inplace = True)


# treatment of variables category
OHE = ce.OneHotEncoder(cols = ['Item_Fat_Content',
                             'Item_Type',
                             'Outlet_Identifier',
                             'Outlet_Size',
                             'Outlet_Location_Type',
                             'Outlet_Type'], use_cat_names = True)

train_data = OHE.fit_transform(train_data)

print()

# Scale data

scaler = StandardScaler()

scaler.fit(np.array(train_data.Item_MRP).reshape(-1, 1))

train_data.Item_MRP = scaler.transform(np.array(train_data.Item_MRP).reshape(-1, 1))

train_X = train_data.drop(columns = ['Item_Identifier', 'Item_Outlet_Sales'])
train_Y = train_data['Item_Outlet_Sales']

train_x, test_x, train_y, test_y = train_test_split(train_X, train_Y, test_size = 0.2, random_state = 0)

train_x.shape, test_x.shape, train_y.shape, test_y.shape

model_LR = LinearRegression()

model_LR.fit(train_x, train_y)

predict_train = model_LR.predict(train_x)
predict_test = model_LR.predict(test_x)

print('RMSE on train data: ', mean_squared_error(train_y, predict_train)**(0.5))
print('RMSE on test data: ',  mean_squared_error(test_y, predict_test)**(0.5))