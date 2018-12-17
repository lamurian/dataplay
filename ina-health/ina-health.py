#!/bin/python

import pandas as pd
import numpy as np
from matplotlib import pyplot as plt

faskes01 = pd.read_csv('jumlah-faskes.csv', delimiter=';')
faskes02 = pd.read_csv('jumlah-RS-puskes.csv', delimiter=';')
campak = pd.read_csv('imunisasi-measles.csv', delimiter=';')

for data in [faskes01, faskes02, campak]:
    # Sort values based on provinces' name and year
    data.sort_values(by=['nama_item_vertical_variabel', 'nama_tahun'],
            inplace=True)

    # Set provinces' name as index
    data.set_index(data['nama_item_vertical_variabel'], inplace=True)

    # Drop unneeded data
    data.drop(['nama_item_vertical_variabel', 'nama_variabel', 'nama_turunan_tahun'],
            axis=1, inplace=True)

    # Delete row containing '-' in data_content
    data = data[data['data_content'] != '-']

    # Regularize data_content as float
    data_content = [float(i) for i in list(data['data_content'])]
    data['data_content'] = data_content
    
    # Special case drop
    if data is campak:
        data.drop('nama_variabel_turunan', axis=1, inplace=True) 

provinces = set(faskes01.index)

for province in provinces:
    data = campak.loc[provinces]
    plt.plot(data['nama_tahun'], data['data_content'])

plt.show()
