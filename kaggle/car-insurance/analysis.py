#!/bin/python3


## LOAD MODULES

# Data frame, arithmetical operation and plotting
import pandas as pd
import numpy as np
import seaborn as sns
from matplotlib import pyplot as plt

# Preprocessing and data splitting
from sklearn.preprocessing import StandardScaler, LabelEncoder, OrdinalEncoder
from sklearn.decomposition import PCA
from sklearn.model_selection import train_test_split
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV
from sklearn.model_selection import cross_val_score

# Imbalanced learning
from imblearn.under_sampling import RandomUnderSampler
from imblearn.over_sampling import RandomOverSampler, SMOTE

# Modelling
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from xgboost import XGBClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import StackingClassifier

# Evaluation metrics
from sklearn.metrics import classification_report, roc_auc_score


## CONFIG

pd.set_option("display.max_columns", None)


## PREP

# Set seed for reference
seed = 42

# Read data frame
data = pd.read_csv("data.csv")

# Check for null
data.isnull().sum()

# Clean columns containing currencies
data_orig        = data.copy() # Preserve the original data
currencies       = ["INCOME", "HOME_VAL", "BLUEBOOK", "OLDCLAIM", "CLM_AMT"]
data[currencies] = data[currencies].replace(
    "[\$,]", "", regex=True
).astype("float") # np.nan does not work with int

# Clean string dtypes
string = ["MSTATUS", "GENDER", "EDUCATION", "CAR_TYPE", "OCCUPATION", "CAR_USE"]
data[string] = data[string].replace(
    "^z_", "", regex=True
).astype("string")

# Convert columns with Yes/No entries into boolean dtypes
boolean       = ["PARENT1", "MSTATUS", "RED_CAR", "REVOKED"]
data[boolean] = data[boolean].apply(
    lambda col: [True if i.lower() == "yes" else False for i in col]
)

# Set `CLAIM_FLAG` as a boolean
data.CLAIM_FLAG = data.CLAIM_FLAG.astype("bool")


## EDA 1

# Get numeric data
numerics = data.select_dtypes(["int", "float"]).columns.tolist()

try:
    numerics.remove("CLM_AMT")
    numerics.remove("ID")
    print("Column removal: success")
except ValueError:
    print("Requested columns are unavailable to remove")

# Descriptive statistics on numeric data
data.describe()

# Density plot on numeric data
nrow    = 3
ncol    = 5
rows    = [0]*ncol + [1]*ncol + [2]*ncol
cols    = [i for i in range(ncol)]*nrow
fig, ax = plt.subplots(nrow, ncol)

for row, col, colname in zip(rows, cols, numerics):
    sns.kdeplot(
        colname, hue="CLAIM_FLAG", fill="CLAIM_FLAG", data=data, ax=ax[row, col]
    )

plt.show()

# Measure correlation on numeric data
sns.heatmap(data[numerics].corr(), annot=True, vmin=-1, vmax=1, center=0)
plt.show()


## FEAT ENG

# Binarize into boolean: `KIDSDRIV`, `HOMEKIDS`
kids           = ["KIDSDRIV", "HOMEKIDS"]
kids_cat       = ["any_{}".format(kid) for kid in kids]
data[kids_cat] = data[kids].apply(
    lambda col: [True if i > 0 else False for i in col]
)

# Categorize based on quartiles: all excepts `CLM_AMT`
num2cat = numerics.copy()
num2cat.remove("KIDSDRIV"); num2cat.remove("HOMEKIDS"); num2cat.remove("CLM_AMT")
for col in num2cat:
    colname = "cat_{}".format(col)
    category = np.nanquantile(data[col], q=[0, 0.7, 0.8, 0.9, 1])
    data[colname] = pd.cut(
        data[col], bins=category, labels=False, include_lowest=True
    )

# Label encode values with two unique values
label = LabelEncoder()
data["lab_GENDER"] = label.fit_transform(data.GENDER)

# Ordinal encoder for ordinal data (education)
edu_rank = {
    "<High School" : 0, "High School" : 1, "Bachelors" : 2, "Masters" : 3, "PhD" : 4
}

data["lab_EDU"] = data.EDUCATION.replace(edu_rank)

# One-hot encoding
data = pd.concat([data, pd.get_dummies(data.OCCUPATION, prefix="job")], axis=1)
data = pd.concat([data, pd.get_dummies(data.CAR_USE, prefix="use")], axis=1)
data = pd.concat([data, pd.get_dummies(data.CAR_TYPE, prefix="type")], axis=1)

# Replace categorical data
cat2num = data.select_dtypes("category").columns
data[cat2num] = data[cat2num].astype("float")

# Manually assign numeric data to group
data["AGE_GROUP"] = pd.cut(
    data.AGE, bins=[0, 24, 40, 56, 80],
    labels=["Gen Z", "Millenial", "Gen X", "Baby boomer"],
    include_lowest=True
)

data = pd.concat(
    [data, pd.get_dummies(data.AGE_GROUP)], axis=1
).drop("AGE_GROUP", axis=1)

# Manually assign categorical data to group
data["is_minivan"] = np.where(data.CAR_TYPE=="Minivan", True, False)


## EDA 2

# Proportional difference on categorical data
categorical = data.select_dtypes(exclude=["int", "float"]).columns.tolist()
categorical.remove("CLAIM_FLAG")

# Looping to create figures on proportional difference
nrow    = 4
ncol    = 8
rows    = [0]*ncol + [1]*ncol + [2]*ncol + [3]*ncol
cols    = [i for i in range(ncol)]*nrow
fig, ax = plt.subplots(nrow, ncol, figsize=[20, 20])

for row, col, cat in zip(rows, cols, categorical):
    # Create a pivot table
    pivot = pd.pivot_table(
        data, values="ID", index=cat, columns="CLAIM_FLAG", aggfunc="count"
    ).reset_index()

    # Calculate the total and percentage
    pivot["total"] = pivot[False] + pivot[True]
    pivot["perc"]  = pivot[True] / pivot["total"] * 100

    # Create a bar plot
    sns.barplot(x=cat, y="perc", data=pivot, ax=ax[row, col])
    ax[row, col].tick_params(labelrotation=45)

fig.suptitle("Percentage of `CLAIM_FLAG`==True")
plt.show()


## NA & SCALING

# Fill NA as 0 due to the exlusivitiy of `pd.cut`
categorized       = ["cat_{}".format(col) for col in num2cat]
data[categorized] = data[categorized].fillna(0)

# Drop NA column
data_nona = data.dropna()
data_nona = data_nona.select_dtypes(exclude="string")

# Split to training and testing dataset
x = data_nona.drop(["CLAIM_FLAG", "CLM_AMT", "ID"], axis=1)
y = data_nona.CLAIM_FLAG

xtrain, xtest, ytrain, ytest = train_test_split(
    x, y, test_size=0.3, random_state=seed
)

# Scale into Z distribution
scaler = StandardScaler()
scaler.fit(xtrain)

xtrain_std = scaler.transform(xtrain)
xtest_std  = scaler.transform(xtest)


## RESAMPLE

# Resolve imbalanced class
sampler = RandomUnderSampler()
xtrain_sample, ytrain_sample = sampler.fit_resample(xtrain_std, ytrain)


## MODEL

# Create dictionary of models
models = {
    "logreg" : LogisticRegression,
    "dt"     : DecisionTreeClassifier,
    "rf"     : RandomForestClassifier,
    "knn"    : KNeighborsClassifier
}

# Looping to fit and evaluate model
sep = "\n=============================================================\n"

for modname, mod in models.items():
    print(sep, modname)
    model = mod()
    model = model.fit(xtrain_sample, ytrain_sample)
    yhat  = model.predict(xtest_std)
    auc   = roc_auc_score(ytest, yhat)
    cv    = cross_val_score(model, xtrain_sample, ytrain_sample, cv=10)
    print(classification_report(ytest, yhat))
    print("CV : ", round(np.mean(cv), 2))
    print("AUC: ", round(auc, 2))

# Hyperparameter tuning on the best performing model
hparams = {
    "tol"          : np.arange(1e-6, 1e-4, 1e-5),
    "C"            : np.arange(0.7, 0.85, 0.02),
    "class_weight" : [{True:i, False:j}
        for i, j in zip(np.arange(1, 1.4, 0.01), np.arange(1.4, 1, -0.01))
    ] + [{True:1, False:1}] # Default
}

logreg = LogisticRegression(solver="lbfgs")
tuned  = RandomizedSearchCV(
    logreg, hparams, cv=10, scoring="accuracy", random_state=seed
)

tuned.fit(xtrain_sample, ytrain_sample)

yhat   = tuned.predict(xtest_std)
auc    = roc_auc_score(ytest, yhat)

print(classification_report(ytest, yhat))
print("AUC: ", round(auc, 2))

# Create an ensemble model
estimator = [
    ("dt",  DecisionTreeClassifier()),
    ("knn", KNeighborsClassifier()),
    ("xgb", XGBClassifier())
]

final_est = LogisticRegression(
    C=0.7, class_weight={True:1.32, False:1.08}, tol=1e-6
)

model     = StackingClassifier(
    estimators=estimator, final_estimator=final_est, cv=10
)

model.fit(xtrain_sample, ytrain_sample)

yhat   = model.predict(xtest_std)
auc    = roc_auc_score(ytest, yhat)

print(classification_report(ytest, yhat))
print("AUC: ", round(auc, 2))

model.score(xtrain_std, ytrain)
model.score(xtest_std, ytest)

# Logistic regression with best fitting tuning parameter
logreg = LogisticRegression(
    C=0.7, class_weight={True:1.32, False:1.08}, tol=1e-6
)

logreg.fit(xtrain_sample, ytrain_sample)

yhat   = logreg.predict(xtest_std)
auc    = roc_auc_score(ytest, yhat)

print(classification_report(ytest, yhat))
print("AUC: ", round(auc, 2))

# Feature importance
var_imp = abs(logreg.coef_[0])
var_imp = 100 * (var_imp / var_imp.max())
col_imp = x.columns[np.argsort(var_imp)]

tbl_imp = pd.DataFrame({
    "Variables"  : col_imp,
    "Importance" : var_imp[np.argsort(var_imp)]
})

sns.barplot(x="Importance", y="Variables", data=tbl_imp)
plt.show()
