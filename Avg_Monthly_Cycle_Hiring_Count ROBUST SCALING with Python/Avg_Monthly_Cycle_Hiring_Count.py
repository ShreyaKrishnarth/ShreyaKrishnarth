#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
import seaborn as sns


# In[8]:


df = pd.read_csv("/Users/shreya/Desktop/Avg_Monthly_Cycle_Hiring_Count.csv")


# In[6]:


df.dtypes


# # Data Preparation

# # Dimensions of Dataset

# In[9]:


df.shape


# In[10]:


df.head()


# In[11]:


df.info()


# # Statistical Summary

# In[13]:


df.describe()


# # Changing datatype of both the columns

# In[14]:


df['Year_Month'] = pd.to_datetime(df['Year_Month'], format = '%Y-%m')


# In[16]:


df.dtypes


# # Observe the data variation

# In[17]:


df.plot.line(x = 'Year_Month', y = 'Cycle_Hiring_Count')
plt.show()


# # Aggregate count of cycle_hiring data by date

# In[19]:


Data = df.groupby('Year_Month')['Cycle_Hiring_Count'].sum().reset_index()


# In[20]:


Data.head()


# In[21]:


Data.info()


# # Indexing with Time Series Data

# In[22]:


Data.set_index('Year_Month', inplace = True)
Data.index


# In[23]:


Data.head()


# # Checking Stationarity

# # Test for testing Stationarity

# In[24]:


y = Data['Cycle_Hiring_Count'].resample('MS').mean()


# In[25]:


y


# In[35]:


from statsmodels.tsa.stattools import adfuller
import pandas as pd
def test_stationarity(timeseries):
    #determine rolling statistics
    rolmean = pd.Series(timeseries).rolling(window=12).mean()
    rolstd = pd.Series(timeseries).rolling(window=12).std()
    #plot rolling statistics
    orig = plt.plot(timeseries,color = 'blue',label='original')
    mean = plt.plot(rolmean,color = 'red',label = 'rolling mean')
    std = plt.plot(rolstd,color = 'black',label = 'rolling std')
    plt.legend(loc = 'best')
    plt.title('rolling mean and standard deviation')
    plt.show(block = False)
 
    print('result of dickey fuller test:')
    dftest = adfuller(timeseries,autolag = 'AIC')
    dfoutput = pd.Series(dftest[0:4],index = ['Test statistics', 'p-value', '#lags used', 'number of observation used'])
    for key,value in dftest[4].items():
        dfoutput['critical value (%s)'%key] = value
    print(dfoutput)


# In[37]:


test_stationary(Data)


# In[ ]:




