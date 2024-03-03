#Merging Test
import numpy as np
import statsmodels.api as sm
import pandas as pd
from pandas import DataFrame
from scipy.stats import chi2

# Read csv and Print Data
df = pd.read_csv(r"C:\2023 Fall\Advanced Test\Group Projects\Mall_Customers.csv",header=0)
#print(df.dtypes)

#Recode Gender into Binary Variables
dummy_df = pd.get_dummies(df['Gender'], prefix='Gender')
df = pd.concat([df, dummy_df], axis=1)
df: DataFrame = df.drop(columns=["Gender_Female",'Gender'])
df['Gender_Male'] = df['Gender_Male'].astype(int)

#print(df)
#print(X.dtypes)
#print(y.dtype)

# OLS Regression
y = df['Spending Score (1-100)']
X = df[['Age', 'Annual Income (k$)', 'Gender_Male']]
X = sm.add_constant(X)

model = sm.OLS(y, X).fit()
print(model.summary())

#print(df)

#Chi squared test for goodness of fit
#Create bins with inclusive left ends and exclusive right ends
bins = [0, 25, 35, 50, 75, 100, np.inf]

#Align data points according to annual income categories
df['Annual Income (k$)'] = pd.cut(df['Annual Income (k$)'], bins, right=False)
contingency_table = pd.crosstab(df['Annual Income (k$)'], df['Spending Score (1-100)'])
print(contingency_table)

#Summarize data points according to income categories, convert the table to data frame and rename the row headers
income_category = contingency_table.sum(axis=1)
income_category = pd.DataFrame(income_category)

income_headers = ["Under 25,000",
                    "25,000 to 34,999",
                    "35,000 to 49,999",
                    "50,000 to 74,999",
                    "75,000 to 99,999",
                    "100,000 and over"]
column_header = ["Observations"]

income_category.index = income_headers
income_category.columns = column_header

#print(income_category)

#Enter US Census Data and Perform the Chi Squared test
expected_counts = np.array([31.4,
                    15.2,
                    21.2,
                    32.4,
                    24.6,
                    75
                    ])

expected_counts = expected_counts.reshape(-1,1)
#print(expected_counts)

income_category['Expected_Counts'] = expected_counts
#print(income_category)

def chi2_test_stat(obs, exp):
    return(obs-exp)**2/exp

income_category['Squared_Differences'] = chi2_test_stat(income_category['Observations'], income_category['Expected_Counts'])
total = income_category.sum().to_dict()
income_category.loc['Total'] = total
print(income_category)
chi2_test_value = income_category.iloc[6, 2]
print('Test statistic of Chi Squared Test:', chi2_test_value)
degrees_of_freedom = income_category.shape[0]-1
alpha = 0.05
crit_value = chi2.ppf(1-alpha, degrees_of_freedom)
print('The critical value given alpha = 0.05 and df = 5 is:', crit_value)
if chi2_test_value > crit_value:
    print("Statistics suggest significant difference between mall customer income group and US census.")
else:
    print("Statistics fail to suggest significant difference between mall customer income group and US census.")
