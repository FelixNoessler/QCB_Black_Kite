from sklearn import model_selection, preprocessing
from imblearn import ensemble
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

plt.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Latin Modern Roman"]})

data = pd.read_csv('milmig.csv')

## observational covariates
observational_covariates = data.loc[:, 'time_observations_started.1':'day_of_year.10']
observational_covariates.loc[:,'site'] = data.site
stubnames = ['time_observations_started.', 'duration_minutes.', 'effort_distance_km.',
             'number_observers.','day_of_year.', 'protocol_type.']
obs_covariates_long = pd.wide_to_long(observational_covariates,
                                      stubnames,
                                      i='site',
                                      j='observation')
obs_covariates_long = obs_covariates_long.rename(
    columns={"time_observations_started.": "daytime",
             "duration_minutes.": "duration",
             "effort_distance_km.": "distance_km",
             "number_observers.": "n_observers",
             "day_of_year.": "day_of_year",
             "protocol_type.": "protocol_type"})
obs_covariates_long['protocol_type'] = preprocessing.LabelEncoder().fit_transform(
    obs_covariates_long['protocol_type'])
sc = preprocessing.StandardScaler(with_mean=False, with_std=False)
obs_covariates_transformed = sc.fit_transform(obs_covariates_long)


## site covariates
site_covariates = data.loc[:, 'bio1':'marshes']
site_covariates_long = site_covariates.loc[
    np.tile(np.arange(0, len(site_covariates)), 10)]



## join covariates to one table
covs = np.concatenate((site_covariates_long.values,
                       obs_covariates_transformed),
                      axis=1)
column_names = np.append(site_covariates_long.columns,
                         obs_covariates_long.columns)


## detection
observed = pd.melt(data.loc[:, 'y.1':'y.10'])

## simulation
n_sim = 500

feature_imp = []
pred_data_all = []
for i in range(n_sim):

    random_numbers = np.random.randint(1, data.n_observations)

    y = observed.iloc[random_numbers, 1]
    x = covs[random_numbers, :]

    x_train, x_test, y_train, y_test = model_selection.train_test_split(
        x, y, test_size=0.0001)
    clf = ensemble.BalancedRandomForestClassifier()

    clf.fit(x_train, y_train)
    feature_imp.append(clf.feature_importances_)

    predict_data_list = []
    for name in column_names:
        mean_x = np.mean(x, axis=0)
        dummy_data = np.tile(mean_x[:, None], 50)
        covariate = np.linspace(-2, 2, 50)
        dummy_data[column_names == name] = covariate
        pred = clf.predict_proba(dummy_data.T)
        predict_data_list.append(pred[:, 1])

    pred_data_all.append(predict_data_list)


feature_imp = np.array(feature_imp)
importance = np.mean(feature_imp.T, axis=1)
importance_sd = np.std(feature_imp.T, axis=1) / np.sqrt(n_sim)

proper_names ={"bio1": "Annual Mean Temperature",
              "bio2": "Mean Diurnal Range",
              "bio3": "Isothermality",
              "bio4": "Temperature Seasonality",
              "bio5": "Max Temperature of Warmest Month",
              "bio7": "Temperature Annual Range",
              "bio9": "Mean Temperature of Driest Quarter",
              "bio10": "Mean Temperature of Warmest Quarter",
              "bio12": "Annual Precipitation",
              "bio17": "Precipitation of Driest Quarter",
              "bio18": "Precipitation of Warmest Quarter",
              "distance_to_water": "Distance to closest river or lake",
              "distance_to_landfill": "Distance to closest landfill",
              "n_observers": "Number of observers",
              "duration": "Duration of observation"}

for key, val in zip(proper_names.keys(), proper_names.values()):
    column_names[key == column_names] = val

filter_index = importance > 0.02
selected_columns = column_names[filter_index]
selected_columns

plt.barh(np.arange(len(selected_columns)), np.sort(importance[filter_index]),
         xerr=importance_sd[filter_index][np.argsort(importance[filter_index])],
         align='center', color='#21918C', alpha=0.7)
plt.yticks(np.arange(len(importance[filter_index])),
           selected_columns[np.argsort(importance[filter_index])], size=12)
plt.xlabel('Importance of covariates', size=12)
plt.tight_layout()

plt.savefig('importance_features.png', dpi=300)
plt.show()



#############
pred_data_all = np.array(pred_data_all)
plt.figure(figsize=(6, 6))

for i, name in enumerate(
        selected_columns[np.argsort(importance[filter_index])][::-1]):
    if i < 4:
        plt.subplot(2,2, i+1)
        data = pred_data_all[:, column_names == name, :]
        y_pred = np.mean(data, axis=1)[1]

        plt.plot(np.linspace(-2, 2, 50), y_pred, '.-k')
        plt.title(name)
plt.tight_layout()
plt.savefig('response.png', dpi=300)
plt.show()
