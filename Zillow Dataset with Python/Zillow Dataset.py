#!/usr/bin/env python
# coding: utf-8

# In[3]:


import os
import pandas as pd


# In[4]:


zillow=pd.read_csv("/Users/shreya/Desktop/zillow.csv",encoding = 'raw_unicode_escape',sep =',')


# In[7]:


zillow.head()


# In[9]:


df1_1=zillow.loc[0:9,'RegionID':'RegionName']


# In[11]:


df1_1


# In[12]:


df_2=zillow.loc[6:12,'RegionID':'SizeRank']
df_2


# # Joining Data Frames
# # Natural Join

# In[14]:


inner_join=pd.merge(df1_1,df_2,on='RegionName',how='inner')
inner_join


# # Full outer join

# In[15]:


outer_join=pd.merge(df1_1,df_2,on='RegionName',how='outer')
outer_join


# # Left outer Join

# In[16]:


left_join=pd.merge(df1_1,df_2,on='RegionName',how='left')
left_join


# # Right outer Join

# In[17]:


right_join=pd.merge(df1_1,df_2,on='RegionName',how='right')
right_join


# In[20]:


full_outer=pd.merge(df1_1,df_2,on='RegionName',how='outer',indicator='in=')
full_outer  [full_outer["in="] != 'both']


# In[21]:


full_outer=pd.merge(df1_1,df_2,on='RegionName',how='outer',indicator='test')



full_outer  [full_outer["test"] != 'both']


# In[22]:


full_outer=pd.merge(df1_1,df_2,on='RegionName',how='outer',indicator='test')



full_outer  [full_outer["test"] != 'left only']


# In[23]:


full_outer=pd.merge(df1_1,df_2,on='RegionName',how='outer',indicator='test')



full_outer  [full_outer["test"] != 'right only']


# In[ ]:




