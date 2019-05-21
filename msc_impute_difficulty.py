# Program uses original data from http://bit.ly/1oZnEKG research to create
# new imputed data for difficulty values that are missing.
# See the paper for brief description of this process.
#
# Output from this program is number of CSV files that needs to
# be combined for the R usage. Use combining program for that.
#
# Originally following versions were used:
# Python 3.5.6, Pandas 0.23.4, Scipy 1.1.0, Scikit-learn 0.20.0 and
# torch 0.4.1.
import pandas as pd
import numpy as np
from scipy import stats
from sklearn.model_selection import train_test_split, GridSearchCV

# Decision tree, naive Bayes and gradient boosted regression tree
from sklearn import tree
from sklearn.naive_bayes import ComplementNB
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import accuracy_score

# Deep learning
import torch
import torch.nn as nn
import torch.nn.functional as F

# A simple deep learning network
class Net(nn.Module):
    def __init__(self):
        super(Net, self).__init__()
        self.inputSize = 5
        self.hiddenSize1 = 120
        self.hiddenSize2 = 50
        self.outputSize = 5

        self.fc1 = nn.Linear(self.inputSize, self.hiddenSize1)
        self.fc2 = nn.Linear(self.hiddenSize1, self.hiddenSize2)
        self.fc3 = nn.Linear(self.hiddenSize2, self.outputSize)

    def forward(self, X):
        h1 = torch.tanh(self.fc1(X))
        h2 = F.relu(self.fc2(h1))
        return self.fc3(h2)

# NN trainer
def train(mdl, optim, crit, Xt, yt):
    mdl.train()
    data = Xt
    target = yt
    optim.zero_grad()
    output = mdl(data)
    mtarget = target.view(-1, )
    loss = crit(output, mtarget-1)
    loss.backward()
    optim.step()

# NN validator
def validate(mdl, crit, Xt, yt):
    mdl.eval()
    data = Xt
    target = yt
    vloss, correct = 0, 0
    output = mdl(data)
    mtarget = target.view(-1, )
    vloss += crit(output, mtarget-1).data.item()
    pred = output.data.max(1)[1]
    correct += pred.eq(target-1).sum()

    vloss /= Xt.shape[0]

# Utilize various algorithms to create predictions for difficulty
# value. Hyperparameters are based on grid search.
def imputer(Xtrain, ytrain, Xtest, ytest, net_iter = 100):
    mdl_score = 0
    mdl_to_use = None
    mdl_net = False

    # Transform true test values to proper form
    ytv = ytest.values.reshape(-1, )

    # Decision tree
    dtclf = tree.DecisionTreeClassifier(criterion='entropy', splitter='best', max_depth=2).fit(Xtrain.values, ytrain.values)
    xpred = dtclf.predict(Xtest.values)
    mdl_score = accuracy_score(ytv, xpred)
    mdl_to_use = dtclf

    # Naive bayes
    cnbclf = ComplementNB(alpha=0.1,fit_prior=True,norm=False).fit(Xtrain.values, ytrain.values.reshape(-1, ))
    xpred = cnbclf.predict(Xtest.values)
    if accuracy_score(xpred, ytv) > mdl_score:
        mdl_score = accuracy_score(ytv, xpred)
        mdl_to_use = cnbclf

    # Gradient boosted regression tree
    gbdtclf = GradientBoostingClassifier(n_estimators=200, learning_rate=0.7, max_depth=14, criterion='friedman_mse').fit(Xtrain.values, ytrain.values.reshape(-1, ))
    xpred = gbdtclf.predict(Xtest.values)
    if accuracy_score(ytv, xpred) > mdl_score:
        mdl_score = accuracy_score(ytv, xpred)
        mdl_to_use = gbdtclf

    # NN
    # Convert dataframes to tensors
    Xtraint = torch.tensor(Xtrain.values, dtype=torch.float)
    ytraint = torch.tensor(ytrain.values, dtype=torch.long)
    Xtestt = torch.tensor(Xtest.values, dtype=torch.float)
    ytestt = torch.tensor(ytest.values, dtype=torch.long)

    best_score = 0
    best_model = None
    # Train the model few epochs before using it
    epochs = 500
    for _ in range(net_iter):
        model = Net()
        optimizer = torch.optim.Adam(model.parameters(), lr=0.001)
        criterion = nn.CrossEntropyLoss()

        for _ in range(1, epochs + 1):
            train(model, optimizer, criterion, Xtraint, ytraint)
            validate(model, criterion, Xtestt, ytestt)

        tspreds = model(Xtestt)
        _, tsidx = torch.max(tspreds, 1)
        tsidxv = [x + 1 for x in tsidx.tolist()]
        nn_score = accuracy_score(ytv, tsidxv)
        if best_model == None or nn_score > best_score:
            best_model = model
            best_score = nn_score

    if best_score > mdl_score:
        mdl_score = best_score
        mdl_to_use = best_model
        mdl_net = True

    # Return single model info that was selected as the best on this time
    return (mdl_score, mdl_to_use, mdl_net)

#
# Main
#

# Number of iterations per all models
model_iterations = 11
# Number of iterations per NN model within above iterations
nnet_iterations = 13

# Load data and exclude last odd column
all_data = pd.read_csv('sigite2014-difficulty-data.csv', sep=';', index_col=False)
ad_columns = all_data.columns.tolist()
cols_to_use = ad_columns[:len(ad_columns) - 1]
all_data = pd.read_csv('sigite2014-difficulty-data.csv', sep=';', usecols=cols_to_use)

# Output amount of data
print('Rows: ' + str(all_data.shape[0]) + ' Columns: ' + str(all_data.shape[1]))

# Indexes to columns that are to be removed. This refers to those columns
# that exis for every exercise in the dataset omitting general items like name.
dropped_columns = [0,1,5,7,8]

# Process exercises one at the time
# Note that there is no exercise 72 but it consist of three sub-exercises and
# exercise 107 is feedback that does not contain usable data.
all_exercise_indices = list(range(1,72)) + [72.1, 72.2, 72.3] + list(range(73, 107)) + [108]
for exercise_index in all_exercise_indices:
    print('Processing exercise #{0}'.format(exercise_index))

    # Get column indexes for particular exercise
    sel_cols = [col_name for col_name in cols_to_use if str(exercise_index).rjust(3, '0') in col_name]
    # and select rows related to only that exercise
    # Also make deep copy in order to allow writing to sel_data later on when
    # setting imputed values back to dataframe
    sel_data = all_data[sel_cols].copy(True)

    # Remove unnecessary columns i.e. those that are unused as features
    sel_data_dropped = sel_data.drop(sel_data.columns[dropped_columns], axis = 1)

    # Get list of column names except for 'difficulty' and then only that one containing it
    colnames = sel_data_dropped.columns
    cols_wo_difficulty = [s for s in colnames if not 'DIFFICULTY' in s]
    for colname in colnames:
        if 'DIFFICULTY' in colname:
            difficulty_col = colname
            break

    # Select rows that does not contain any NaN values
    sel_data_full = sel_data_dropped.dropna()
    # Split it for features and target
    sel_data_full_x = sel_data_full.drop(columns=difficulty_col)
    sel_data_full_y = sel_data_full[difficulty_col]

    # Select rows that contains NaN values
    sel_data_missing = sel_data_dropped[sel_data_dropped.isnull().any(axis=1)]
    # Drop all rows that contain NaN in any other column except for difficulty column
    sel_data_missing = sel_data_missing.dropna(subset=cols_wo_difficulty)
    # Separate difficulty from other data i.e. features and target
    sel_data_missing_x = sel_data_missing.drop(columns=difficulty_col)
    sel_data_missing_y = sel_data_missing[difficulty_col]

    # Get number of rows in each category
    group_counts = sel_data_full_y.groupby(sel_data_full_y).count()

    # Omit entries with single instance only
    new_y = None
    new_x = None
    drop_index = []
    didx = 0
    for i in range(group_counts.shape[0]):
        if group_counts.iat[i] < 2:
            drop_index.append(sel_data_full_y[sel_data_full_y == group_counts.index[i]].index[0])
            if new_x is None:
                new_y = sel_data_full_y[sel_data_full_y != group_counts.index[i]]
                new_x = sel_data_full_x[sel_data_full_x.index != drop_index[didx]]
            else:
                new_y = new_y[new_y != group_counts.index[i]]
                new_x = new_x[new_x.index != drop_index[didx]]
            didx += 1

    # Remove them by splitting dataframe but store for manual addition,
    # which is explained later
    if new_x is not None:
        single_x = None
        single_y = None
        for i in range(didx):
            if single_x is None:
                single_x = sel_data_full_x[sel_data_full_x.index == drop_index[i]]
                single_y = sel_data_full_y[sel_data_full_y.index == drop_index[i]]
            else:
                single_x = single_x.append(sel_data_full_x[sel_data_full_x.index == drop_index[i]])
                single_y = single_y.append(sel_data_full_y[sel_data_full_y.index == drop_index[i]])

        # Swap datasets
        new_x, sel_data_full_x = sel_data_full_x, new_x
        new_y, sel_data_full_y = sel_data_full_y, new_y

    # As models performance varies between splits try few rounds in order to obtain
    # fair model for imputation
    model_predictions = []
    print('\tSearching model')
    for _ in range(model_iterations):
        # Split data between training and testing set plus ensure that stratification is done
        # to ensure that both sets contain values from all categories i.e. difficulty ratings
        Xtrain, Xtest, ytrain, ytest = train_test_split(sel_data_full_x, sel_data_full_y, stratify=sel_data_full_y, test_size = 0.3)
        # If there are single class entries append them to training dataset
        # Idea is to let model learn this class also
        if new_x is not None:
            Xtrain = Xtrain.append(single_x)
            ytrain = ytrain.append(single_y)

        # Get a model for this round from one of the four candidates
        lscore, lmodel, lnet = imputer(Xtrain, ytrain, Xtest, ytest, nnet_iterations)

        # Create predictions from each round best predictor
        if not lnet:
            ipred = lmodel.predict(sel_data_missing_x)
        else:
            sel_data_missing_x_tensor = torch.tensor(sel_data_missing_x.values, dtype=torch.float)
            npred = lmodel(sel_data_missing_x_tensor)
            _, nidx = torch.max(npred, 1)
            ipred = [x + 1 for x in nidx.tolist()]

        # Store all top model scores and predictions
        model_predictions.append({'score': lscore, 'prediction': ipred})

    # Sort model based on scores
    top_models = sorted(model_predictions, key=lambda k:k['score'], reverse=True)
    # and take top five of them
    top_preds = []
    for idx, mdl in enumerate(top_models):
        if idx > 5: break
        top_preds.append(mdl['prediction'])

    # Get the most common value from each top model prediction to be used as imputed value
    # i.e. majority voting
    ipred = stats.mode(top_preds)[0][0]
    # Update predictions to dataframe that contains all selected week data
    ipdx = 0
    for idx in sel_data_missing_y.index.values:
        sel_data.at[idx, difficulty_col] = ipred[ipdx]
        ipdx +=1
    sel_data.to_csv('imputed_{0}.csv'.format(exercise_index))
    print('\tData stored')
