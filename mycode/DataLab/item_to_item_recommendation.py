#!/usr/bin/env python
# coding: utf-8

# ### Import Library 

# In[19]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import warnings
from dfply import *
warnings.filterwarnings('ignore')

get_ipython().run_line_magic('matplotlib', 'inline')


# In[20]:


import keras
from IPython.display import SVG
from keras.optimizers import Adam
from keras.layers import merge
from keras.layers import Concatenate
from keras.utils.vis_utils import model_to_dot
import math
from sklearn.model_selection import train_test_split
from sklearn.model_selection import ParameterGrid
from sklearn.model_selection import GridSearchCV
from sklearn.metrics.pairwise import cosine_similarity
from scipy.sparse import csr_matrix
from keras.constraints import non_neg
from scipy.spatial import distance_matrix
from sklearn.metrics.pairwise import cosine_similarity 


# ### Load Data 

# In[21]:


path = '/home/edc8785/'
review2 = pd.read_csv(path + 'total_dat_lunch.csv')

review2.reviewerId = review2.reviewerId.astype("category").cat.codes.values
review2.restaurantId = review2.restaurantId.astype("category").cat.codes.values


# In[22]:


train = (review2 >> mask(X.visitDate < 20170101  ))
test = (review2 >> mask(X.visitDate >= 20170101 ))


# In[23]:


train = train[["restaurantId", "reviewerId", "ratingTotalScore"]]
test = test[["restaurantId", "reviewerId", "ratingTotalScore"]]

review2 = review2[["restaurantId", "reviewerId", "ratingTotalScore"]]


# ### Item to Item recommendation 

# #### nnmf 모델에서 학습시킨 item embedding 값을 이용하여 item to item recommendation에 적용하려 한다. 그런데 이 embedding된 값은 의미를 알 수 없기 때문에 무조건적으로 사용할 순 없다고 판단을 하였고 이 임베딩된 값이 의미가 있음을 증명한 뒤 이를 이용한다. 
# 
# #### 그 방식은 item embedding된 값으로 유사도 매트릭스를 구하여 이를 통해 item based collaborative filtering을 구현하고, 기존의 rest-user matrix를 통해 구현하는 item based cf와 결과값을 비교함으로써 embedding된 값이 의미가 있음을 증명한다.

# #### train set을 이용하여  rest, user rating matrix를 구성한다

# In[24]:


n_users = review2.reviewerId.unique().shape[0]
n_items = review2.restaurantId.unique().shape[0]
    

train_row = []
train_col = []
train_rating = []

for line in train.itertuples():
    i = line[1]
    u = line[2]
        
    train_row.append(u)
    train_col.append(i)
    train_rating.append(line[3])

train_matrix = csr_matrix((train_rating, (train_row, train_col)), shape=(n_users, n_items))


# In[25]:


train_matrix


# ### similarity matrix 

# In[26]:


## version1은 기존의 item based cf와 똑같은 방식으로 구현


# In[37]:


item_based_cf = pd.DataFrame(train_matrix.todense()).transpose()


# In[39]:


similarity = pd.DataFrame(cosine_similarity(item_based_cf))


# In[29]:


## version2은 embedding된 값으로 유사도를 구함


# In[30]:


item_embedding = pd.read_csv("item_embedding.csv")


# In[32]:


new_similarity = pd.DataFrame(cosine_similarity(item_embedding))

## new_corr = item_embedding.corr(method = 'pearson')
## new_corr.head()


# In[40]:


similarity.head()


# In[36]:


new_similarity.head()


# #### 임베딩된 값으로 코사인 유사도를 구할 경우 음수값이 존재한다. 음수값이 있을 경우 뒤에서 가중평균을 하는 과정에서 문제가 생기기 때문에 이를 해결하기 위해 min max scaling을 진행하였다. 

# In[14]:


max_value = new_similarity.max(axis=0).max()
min_value = new_similarity.min(axis=0).min()


# In[15]:


new_similarity = new_similarity - min_value
new_similarity = new_similarity/(max_value - min_value)


# ####  기존의 user based cf 와 embedding된 값으로 유사도를 구한 cf를 구현

# In[27]:


test["result_cf"] = None
test["result_new_cf"] = None

test.head()


# In[28]:


for i in range(0, len(test)):
    item = test["restaurantId"].iloc[i]
    user = test["reviewerId"].iloc[i]
    
    user_vec = np.array(train_matrix.getrow(user).todense()).reshape(-1)
    simil_vec = np.array(similarity.loc[item])
    
    sam1_1 = pd.DataFrame(user_vec, columns=["user"])
    sam1_2 = pd.DataFrame(simil_vec, columns=["simil"])
    
    sam1_3 = pd.concat([sam1_1,sam1_2], axis=1)
    sam1_3 = sam1_3 >> mask(X.user != 0 ) >> mask(X.simil != 1)
    
    if(len(sam1_3) != 0):
        if(sum(sam1_3["simil"]) !=0 ):
            result1 = (sum(sam1_3["user"] * sam1_3["simil"]) / sum(sam1_3["simil"]))
        else:
            result1=0
    else:
        result1 = 0
    
    new_simil_vec = np.array(new_similarity.loc[item])
    
    sam2_1 = pd.DataFrame(user_vec, columns=["user"])
    sam2_2 = pd.DataFrame(new_simil_vec, columns=["simil"])
    
    sam2_3 = pd.concat([sam2_1,sam2_2], axis=1)
    sam2_3 = sam2_3 >> mask(X.user != 0 ) >> mask(X.simil != 1)
    
    if(len(sam2_3) != 0):
        if(sum(sam2_3["simil"]) !=0 ):
            result2 = (sum(sam2_3["user"] * sam2_3["simil"]) / sum(sam2_3["simil"]))
        else:
            result2= 0
    else:
        result2= 0
        
    test["result_cf"].iloc[i] = result1
    test["result_new_cf"].iloc[i] = result2
    
    if(i % 1000 == 0):
        print(i)


# In[30]:


test_sample = test.copy()


# #### result_cf는 기존의 cf를 이용한 방식이고, result_new_cf는 embedding된 값의 유사도를 이용한 cf 방식이다. 

# In[31]:


test_sample.head()


# #### 기존 cf의 경우 유저가 식당 리뷰를 적게하고, 그 식당과 여러 식당의 유사도가 0인 경우는 추천이 불가능하다. 이런 문제는 sparse한 데이터에서 두드러진다. 허나, 이 방식의 경우 embedding된 식당의 값은 전혀 sparse하지 않기 때문에 이런 경우까지 커버가 가능하고 실제로 아래의 값을 보면 기존 cf는 예측은 사용이 불가하지만 embedding 유사도로 구한 cf는 예측이 가능하며 성능 역시 높은 성능을 보임을 알 수 있다.

# In[32]:


test_sample2 = test_sample >> mask(X.result_cf == 0) >> mask(X.result_new_cf != 0)

test_sample2.head()


# #### 기존의 cf가 예측하지 못하는 부분을 제외하고 rmse를 구하여도 새로운 방법의 rmse가 더 낮음을 확인할 수 있다.

# In[33]:


test_sample = test_sample >> mask(X.result_cf != 0) >> mask(X.result_new_cf != 0)


# In[34]:


test_sample.head()


# In[35]:


math.sqrt(sum((test_sample["ratingTotalScore"] - test_sample["result_cf"]) ** 2) / len(test_sample))


# In[36]:


math.sqrt(sum((test_sample["ratingTotalScore"] - test_sample["result_new_cf"]) ** 2) / len(test_sample))


# #### 위의 결과를 바탕으로 embedding된 값의 각각의 차원에 대한 의미는 알 수 없지만 식당의 정보를 나타냄을 알 수 있다. 따라서 이를 이용하여 item to item recommendation을 진행한다.

# In[76]:


path = '/home/edc8785/'
population_user = pd.read_csv(path + 'total_dat_lunch.csv')

population_user["restaurantId_str"] = population_user["restaurantId"]
population_user["reviewerId_str"] = population_user["reviewerId"]

population_user.restaurantId = population_user.restaurantId.astype("category").cat.codes.values
population_user.reviewerId = population_user.reviewerId.astype("category").cat.codes.values


# In[77]:


def search_rest_num_id(rest_id):
    sample = population_user >> mask(X.restaurantId == int(rest_id))
    rest_id = sample["restaurantId_str"].iloc[0]
    
    return(rest_id)

def search_rest_str_id(rest_id):
    sample = population_user >> mask(X.restaurantId_str == rest_id)
    rest_id = sample["restaurantId"].iloc[0]
    
    return(rest_id)


# In[78]:


def recommendation():
    rest_str_id = input("Enter restaurant id: ")
    rest_num_id = search_rest_str_id(rest_str_id)
    
    result = pd.DataFrame(pd.DataFrame(new_similarity.iloc[0]).sort_values([0],  ascending=[False]).iloc[1:11].index)
    
    for i in range(0, len(result)):
        result[0].iloc[i] = search_rest_num_id(result[0].iloc[i])

    
    return(result)


# ### simulation 

# In[80]:


## simulation "/tokyo/A1301/A130102/13017887/"


# In[81]:


recommendation()


# #### 특정 식당을 넣으면 그와 유사한 상위 10개의 식당을 추천해준다 
