# -*- coding: utf-8 -*-
"""
Created on Fri Jan 26 12:37:31 2018

@author: oyeda
"""
import pandas as pd
import matplotlib.pyplot as plt

data=pd.read_csv("C:/Users/oyeda/Desktop/Task/BoutiqueShampooLtd.csv", sep=';')
data['Dates']=pd.to_datetime(pd.Series(data['Dates']), format="%d.%m.%Y")
data=data.set_index('Dates', drop=False)
data=data.sort_index(ascending=True)
xc=data.describe()

# =============================================================================
# x=data.index
# y=data['Total Volume Sales']
# plt.plot(x,y)
# plt.title('Sales volume from 10th Feb, 2014 to 5th Dec, 2016 ')
# plt.xlabel('Date')
# plt.ylabel('Sales Volume')
# =============================================================================


def timeSeries(data='',date1='',date2='', y_col='', title='',ylabel='', color=''):
    """
    function for time series analysis
    date: datetime which checks the duration for the analysis.
    y_col: the variable of interest on the y-axis
    """
    if not date2:
        data=data[date1]
    else:
        data=data[date1 : date2]
    x= data['Dates']
    y=data[y_col]
    plt.plot(x, y, color)
    plt.title(title)
    plt.xlabel('Date')
    plt.ylabel(ylabel)
    plt.tight_layout


#now for the entire duration
timeSeries(data=data,date1='2014', date2='2016', y_col='Total Volume Sales', ylabel= 'Sales Volume', title='Sales volume from 2014 to 2016 ')

#for 2014
timeSeries(data=data,date1='2014', y_col='Total Volume Sales', ylabel= 'Sales Volume', title='Sales volume for 2014', color='blue')
timeSeries(data=data,date1='2015', y_col='Total Volume Sales', ylabel= 'Sales Volume', title='Sales volume for 2015', color='red')
timeSeries(data=data,date1='2016', y_col='Total Volume Sales', ylabel= 'Sales Volume(unit)', title='Sales volume 2014-2016', color='green')


#now for the entire duration
timeSeries(data=data,date1='2014', date2='2016', y_col='Total Value Sales', ylabel= 'Sales Volume', title='Total Value Sales from 2014 to 2016 ')
timeSeries(data=data,date1='2014', y_col='Total Value Sales', ylabel= 'Sales Value', title='Sales value for 2014', color='blue')
timeSeries(data=data,date1='2015', y_col='Total Value Sales', ylabel= 'Sales Value', title='Sales value for 2015', color='red')
timeSeries(data=data,date1='2016', y_col='Total Value Sales', ylabel= 'Sales Value(unit)', title='Sales value 2014-2016', color='green')


timeSeries(data=data,date1='2014', y_col='Weighted Average Price', ylabel= 'Sales Val', title='Sales volume for 2014', color='blue')
timeSeries(data=data,date1='2015', y_col='Weighted Average Price', ylabel= 'Sales Volume', title='Sales volume for 2015', color='red')
timeSeries(data=data,date1='2016', y_col='Weighted Average Price', ylabel= 'Sales Val(unit)', title='Sales volume 2014-2016', color='green')




data['Total Volume Sales'].mean()
data['Total Volume Sales'].min()
data['Total Volume Sales'].max()

data_2016= data['2016']

data_2016.loc[data_2016['Total Volume Sales']==data_2016['Total Volume Sales'].max(), :]
kldfg=data_2016.describe()
kldfg.to_csv("C:/Users/oyeda/Desktop/Task/BoutiqueShampooLtd.csv")
data


#ml=data.set_index('Dates').groupby(pd.TimeGrouper('m')).mean()


