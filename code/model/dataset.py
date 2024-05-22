import pandas as pd
import numpy as np
import random
import ast

class CountryBasedSplit:
    def __init__(self, data, features, country_column='country', id_column='id', month_column='month', train_ratio=0.6, val_ratio=0.2, test_ratio=0.2):
        self.data = data
        self.features = features
        self.country_column = country_column
        self.id_column = id_column
        self.month_column = month_column
        self.train_ratio = train_ratio
        self.val_ratio = val_ratio
        self.test_ratio = test_ratio

    def split(self, ids=None):
        if ids is None:
            ids = self.data[self.id_column].unique()

        self.data = self.data[self.data[self.id_column].isin(ids)]

        unique_countries = self.data[self.country_column].unique()
        random.shuffle(unique_countries)
        
        num_countries = len(unique_countries)
        train_cutoff = int(num_countries * self.train_ratio)
        val_cutoff = int(num_countries * (self.train_ratio + self.val_ratio))
        
        train_countries = unique_countries[:train_cutoff]
        val_countries = unique_countries[train_cutoff:val_cutoff]
        test_countries = unique_countries[val_cutoff:]
        
        train_data = self.data[self.data[self.country_column].isin(train_countries)]
        val_data = self.data[self.data[self.country_column].isin(val_countries)]
        test_data = self.data[self.data[self.country_column].isin(test_countries)]
        
        return train_data, val_data, test_data

    def adjust_split(self, group, target_size, other_groups):
        current_size = len(group)
        if current_size > target_size:
            excess = current_size - target_size
            to_move = group.sample(excess)
            group = group.drop(to_move.index)
            other_groups[0] = pd.concat([other_groups[0], to_move[:len(to_move)//2]])
            other_groups[1] = pd.concat([other_groups[1], to_move[len(to_move)//2:]])
        return group

    def get_indices(self, short_list, long_list):
        common_elements = set(short_list) & set(long_list)
        return [long_list.index(x) for x in common_elements]
    
    def filter_1darray(self, arr, indices):
        return arr[indices]
    
    def filter_2darray(self, arr, indices):
        return arr[indices, :, :]

    def get_splits(self, id_list):
        train_data, val_data, test_data = self.split(id_list)
        
        total_count = len(self.data)
        train_count = int(total_count * self.train_ratio)
        val_count = int(total_count * self.val_ratio)
        test_count = total_count - train_count - val_count
        
        train_data = self.adjust_split(train_data, train_count, [val_data, test_data])
        val_data = self.adjust_split(val_data, val_count, [train_data, test_data])
        test_data = self.adjust_split(test_data, test_count, [train_data, val_data])

        train_indices = self.get_indices(train_data[self.id_column].tolist(), id_list)
        val_indices = self.get_indices(val_data[self.id_column].tolist(), id_list)
        test_indices = self.get_indices(test_data[self.id_column].tolist(), id_list)
        
        return train_indices, val_indices, test_indices   

    def create_feature_arrays(self, window_size, desired_features, label_column):
        feature_arrays = []
        labels = []
        ids = []

        for _, group in self.data.groupby([self.id_column]):
            start_date = group['date'].iloc[0] - pd.offsets.MonthBegin(window_size + 1)
            end_date = group['date'].iloc[0]
            feature_select = self.features[
                (self.features['country'] == group[self.country_column].iloc[0]) &
                (self.features['measure'].isin(desired_features)) &
                (self.features['date'] > start_date) &
                (self.features['date'] < end_date)
            ]

            if len(feature_select) == window_size * len(desired_features) and not feature_select['value'].isnull().any():
                pivot_df = feature_select.pivot(index='date', columns='measure', values='value')
                feature_arrays.append(pivot_df.values)
                labels.append(group[label_column].iloc[0])
                ids.append(group[self.id_column].iloc[0])
        
        # Convert feature arrays into a 3D numpy array
        feature_array_3d = np.stack(feature_arrays, axis=0)
        feature_array_3d = feature_array_3d.T.reshape(len(labels), len(desired_features), window_size)

        return np.array(feature_array_3d), np.array(labels), np.array(ids)
    
    def process_splits(self, window_size, desired_features, label_column):
        features, labels, ids = self.create_feature_arrays(window_size, desired_features, label_column)
        
        train_indices, val_indices, test_indices = self.get_splits(ids.tolist())

        # select features
        train_features = self.filter_2darray(features, train_indices)
        val_features = self.filter_2darray(features, val_indices)
        test_features = self.filter_2darray(features, test_indices)

        # select labels
        train_labels = self.filter_1darray(labels, train_indices)
        val_labels = self.filter_1darray(labels, val_indices)
        test_labels = self.filter_1darray(labels, test_indices)
        
        # select ids
        train_ids = self.filter_1darray(ids, train_indices)
        val_ids = self.filter_1darray(ids, val_indices)
        test_ids = self.filter_1darray(ids, test_indices)

        return (train_features, train_labels, train_ids), (val_features, val_labels, val_ids), (test_features, test_labels, test_ids)
    