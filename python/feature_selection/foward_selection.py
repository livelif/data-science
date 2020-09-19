# Wrapper methods

from sklearn.datasets import load_boston
import pandas as pd

import statsmodels.api as sm

from mlxtend.feature_selection import SequentialFeatureSelector as SFS
from sklearn.linear_model import LinearRegression

from sklearn.model_selection import train_test_split

boston = load_boston()

# print dataset dimension
print(boston.data.shape)

# feature names

print(boston.feature_names)

# convert boston in dataframe

bostonDataframe = pd.DataFrame(boston.data, columns = boston.feature_names)

# Sequential Forward Selection(sfs)

def forward_selection(data, target, significance_level=0.05):
    initial_features = data.columns.tolist()
    best_features = []
    while (len(initial_features) > 0):
        remaining_features = list(set(initial_features)-set(best_features))
        new_pval = pd.Series(index=remaining_features)
        for new_column in remaining_features:
            model = sm.OLS(target, sm.add_constant(data[best_features+[new_column]])).fit()
            new_pval[new_column] = model.pvalues[new_column]
        min_p_value = new_pval.min()
        if(min_p_value < significance_level):
            best_features.append(new_pval.idxmin())
        else:
            break
    return best_features


bostonDataframe["price"] = boston.target
best_features = forward_selection(bostonDataframe, bostonDataframe["price"])

sfs = SFS(LinearRegression(), k_features=11, forward=True, floating=False, scoring = 'r2', cv = 0)

sfs.fit(bostonDataframe, bostonDataframe["price"])
fNames = sfs.k_feature_names_

print()
