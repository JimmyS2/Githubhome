#!/usr/bin/env python
# coding: utf-8

# # User to Item DNN Code
# ## Team4 백우현
# 
# 코드설명 : 
# 
# 
# UsertoItem DNN 코드이며, 하나의 OBS는 i번째 사람(User)이 j번째 식당(Item)에 남기는 리뷰(방문)입니다. 이전에 User-Item Rating Matrix와 각 User와 Item에 대한 Contents(성별, 나이, 식당 내 흡연가능/주차가능/카드가능, 가격대, 식당 카테고리123, 방문 월 등)이 preprocess 되어 있어야 합니다.
# 
#  Input으로는 i번째 User-side Normalized Rating vector, j번째 Item-side Normalized Rating vector, 그리고 Contents vertor입니다. 이에 따른 output은 i번째 user가 j번째 식당에 남긴 평점(Rating)입니다. 따라서 예측평점(expected ratign)과 실제 평점사이의 RMSE(Root mean square error)가 Error Function가 됩니다.
#     
# Neaural Network의 대략적인 스케치는 설명 이미지를 참고 바라며, 간단히 요약하자면 차원이 큰 User, Item side Noramlized Vector를 각각 두 층의 Hidden Layer를 거쳐 작은 차원으로 Embedding 시키고, Contents Features 역시 우선 One-hot Encoding 후 더 적은 차원의 Vector로 Embedding 됩니다. Embedding 된 Vecotors를 Concatenate 시켜서 최종 3층의 Hidden Layer의 Input을 사용합니다. 각 Activation Function은 Tuning 결과 Sigmoid를 사용했고, Adam Optimizer를 사용하였습니다.Dropout, Layer node size, batch size, epochs 등에 관한 Grid Search 결과는 CSV파일을 참고바랍니다.
# 
#  모델링 이후에는 실제 Simulation 코드가 첨부되어있는데, 이는 사용자가 하나의 User를 입력하면 해당 User에 대한 다른 모든 식당들의 값들을 넣어본 후 가장 예측 평점이 높은 식당을 추천해주는 식으로 이루어져 있습니다. 과정에서 사용자가 직접 일부 설정을 Filtering할 수 있는 옵션들도 넣어두었습니다. 

# In[1]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
# from plotnine import *

import tensorflow as tf

from keras.models import Model
from keras.layers import Input, Dense, Concatenate, Reshape, Dropout, Activation
from keras.layers.embeddings import Embedding
from keras.layers.normalization import BatchNormalization
from keras import regularizers
from keras.optimizers import Adam
#f rom keras.wrappers.scikit_learn import KerasClassifier
# from keras import backend as K

from sklearn.model_selection import KFold
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import train_test_split
from sklearn.model_selection import ParameterGrid
from sklearn.model_selection import GridSearchCV
from itertools import product
from dfply  import *
import re
import webbrowser
#from talos.model.early_stopper import early_stopper


# In[ ]:


from os import chdir, getcwd

wd=getcwd()
print(wd)


# ## Load and merge input Data
# ###  미나토 구 만 선택하여 모델링 ( 5개 도시로 모델링할 경우 차원이 너무 커져 메모리 문제 발생)

# In[3]:


path = '/home/woo100100/'
lunch_train_data= pd.read_csv(path +'minato_dinner_train.csv')
lunch_test_data= pd.read_csv(path +'minato_lunch_test.csv')


# ### Contents + User_normalized_rating + Item_normalized_rating 

# In[4]:


# load user_side_normalized rating matrix
path = '/home/woo100100/'
lunch_matrix_user= pd.read_csv(path +'minato_dinner_matrix_user.csv')  # user - normalized matrix


# In[5]:


lunch_matrix_user.rename(columns={'Unnamed: 0':'reviewerId'}, inplace=True)
lunch_train_data_final = pd.merge(lunch_train_data, lunch_matrix_user , how='left', on= 'reviewerId')
lunch_test_data_final = pd.merge(lunch_test_data,lunch_matrix_user, how='left', on= 'reviewerId' )


# In[6]:


del lunch_train_data
del lunch_matrix_user


# In[7]:


# load item_side_normalized rating matrix
lunch_matrix_item= pd.read_csv(path +'minato_dinner_matrix_item.csv') 


# In[8]:


lunch_matrix_item.rename(columns={'Unnamed: 0':'restaurantId'}, inplace=True)
lunch_train_data_final = pd.merge(lunch_train_data_final, lunch_matrix_item , how='left', on= 'restaurantId')
lunch_test_data_final = pd.merge(lunch_test_data_final, lunch_matrix_item, how='left', on= 'restaurantId')


# In[9]:


del lunch_matrix_item
del lunch_test_data


# #### minato
# #### Train : 154280 rows(reviews),  21293 columns      /     Test : 22401 rows, 21293 columns
# #### col : 13개의 Content Columns ,   6094개의 User-normalized columns ,  15186 Item-normalized

# ## Prepare Data For Training 
# ### K-fold를 진행할 경우 Valid를 나누지 않고 lunch_train_data_final전체를 X_train으로 활용하여 아래의 K-fold 과정을 진행하시면 됩니다.

# In[10]:


X_train, X_valid, y_train, y_valid = train_test_split(
        lunch_train_data_final.iloc[:,1:] , lunch_train_data_final.ratingTotalScore, test_size=0.25)


# ### 'Contents' Features

# In[12]:


X_train.columns[2:12]


# In[13]:


# For convenient comparing with y_hat after modeling, divide y with 5 (Because output activation function is a Sigmoid)
# Dividing a constant value(in this case '5') is not a matter on the side of gradient descent 
y_train = y_train / 5
y_valid = y_train /5 
y_test = y_test / 5


# In[28]:


# Prepare for Embedding
levels_of_cols = {c: list(X_train[c].unique()) for c in X_train.columns[2:12]}
embed_cols = []

for c in levels_of_cols:
    if len(levels_of_cols[c])>=2:
        embed_cols.append(c)
        print(c + ': %d levels' % len(levels_of_cols[c])) 


# ### 'user-noramlized rating' & 'item-noramlized rating' Features

# In[15]:


idx_user = np.where(X_train.columns.str.find('tokyo')==1)
idx_item = np.where(X_train.columns.str.find('rvwr')==1)


# In[16]:


user_normalized_cols = X_train.columns[idx_user]
item_normalized_cols = X_train.columns[idx_item]


# In[17]:


user_num = len(user_normalized_cols) 
item_num = len(item_normalized_cols)


# In[18]:


X_train_columns = X_train.columns


# ## Construct Model

# In[2]:


def embedding_dnn():
    
    inputs = []
    embedding_set = []
    
    # Contents Features Embedding 
    
    input_reviewerProfileGender = Input(shape=(1,))
    embedding = Embedding(3, 2, input_length=1)(input_reviewerProfileGender)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_reviewerProfileGender)
    embedding_set.append(embedding)
    
    input_reviewerProfileAge = Input(shape=(1,))
    embedding = Embedding(6, 3, input_length=1)(input_reviewerProfileAge)
    embedding = Reshape(target_shape=(3,))(embedding)
    inputs.append(input_reviewerProfileAge)
    embedding_set.append(embedding)
    
    input_priceValue = Input(shape=(1,))
    embedding = Embedding(13, 6, input_length=1)(input_priceValue)
    embedding = Reshape(target_shape=(6,))(embedding)
    inputs.append(input_priceValue)
    embedding_set.append(embedding)
    
    input_month = Input(shape=(1,))
    embedding = Embedding(12, 6, input_length=1)(input_month)
    embedding = Reshape(target_shape=(6,))(embedding)
    inputs.append(input_month)
    embedding_set.append(embedding)
    
    input_creditCard = Input(shape=(1,))
    embedding = Embedding(3, 2, input_length=1)(input_creditCard)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_creditCard)
    embedding_set.append(embedding)
    
    input_parking = Input(shape=(1,))
    embedding = Embedding(3, 2, input_length=1)(input_parking)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_parking)
    embedding_set.append(embedding)
    
    input_smoking = Input(shape=(1,))
    embedding = Embedding(3, 2, input_length=1)(input_smoking)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_smoking)
    embedding_set.append(embedding)
    
    input_step1 = Input(shape=(1,))
    embedding = Embedding(4, 2, input_length=1)(input_step1)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_step1)
    embedding_set.append(embedding)
    
    input_step2 = Input(shape=(1,))
    embedding = Embedding(16, 6, input_length=1)(input_step2)
    embedding = Reshape(target_shape=(6,))(embedding)
    inputs.append(input_step2)
    embedding_set.append(embedding)
    
    input_step3 = Input(shape=(1,))
    embedding = Embedding(93, 20, input_length=1)(input_step3)
    embedding = Reshape(target_shape=(20,))(embedding)
    inputs.append(input_step3)
    embedding_set.append(embedding)

    
    # User-normalized vector Embedding 
    
    input_numeric_user = Input(shape=( user_num ,))
    embedding_numeric_user = Dense(300)(input_numeric_user)
    hidden_layer_01 = Activation('sigmoid')(embedding_numeric_user)
    drop_layer_01 = Dropout(.2)(hidden_layer_01)
    
    hidden_layer_012 = Dense(100)(drop_layer_01)
    hidden_layer_012 = Activation('sigmoid')(hidden_layer_012 )
    drop_layer_012 = Dropout(.1)(hidden_layer_012)
    
    inputs.append(input_numeric_user)
    embedding_set.append(drop_layer_012)
    
    
    # Item-normalized vector Embedding
    
    input_numeric_item = Input(shape=( item_num ,))
    embedding_numeric_item = Dense(300)(input_numeric_item ) 
    hidden_layer_02 = Activation('sigmoid')(embedding_numeric_item)
    drop_layer_02 = Dropout(.2)(hidden_layer_02)
    
    hidden_layer_022 = Dense(100)(drop_layer_02)
    hidden_layer_022 = Activation('sigmoid')(hidden_layer_022 )
    drop_layer_022 = Dropout(.1)(hidden_layer_022)
    
    inputs.append(input_numeric_item )
    embedding_set.append(drop_layer_022)
    
    
    # Concatenate all Features : Make a final input vector
    merged_x = Concatenate()(embedding_set)
    
    
    # Construct hidden layers
    hidden_layer_1 = Dense(300)(merged_x)
    # hidden_layer_1 = BatchNormalization()(hidden_layer_1) 
    hidden_layer_1 = Activation('sigmoid')(hidden_layer_1)
    drop_layer_1 = Dropout(.2)(hidden_layer_1)
    
    
    hidden_layer_2 = Dense(100)(drop_layer_1)
    # hidden_layer_2= BatchNormalization()(hidden_layer_2)
    hidden_layer_2 = Activation('sigmoid')(hidden_layer_2)
    # drop_layer_2 = Dropout(.2)(hidden_layer_2)
    
    
    hidden_layer_3 = Dense(50)(hidden_layer_2)
    # hidden_layer_3 = BatchNormalization()(hidden_layer_3)
    hidden_layer_3 = Activation('sigmoid')(hidden_layer_3)
    # drop_layer_3 = Dropout(.2)(hidden_layer_3)
    
    
    output = Dense(1)(hidden_layer_3)
    # output = BatchNormalization()(output)
    output = Activation('sigmoid')(output)
    
    model = Model(inputs, output)
    adamopt= Adam(lr=0.01, beta_1=0.9, beta_2=0.999, epsilon=1e-8)
    model.compile(loss='mean_squared_error', optimizer=adamopt)
    
    return model
    
    


# In[ ]:


from keras.models import Sequential
from keras.layers import Dense
from keras.utils.vis_utils import plot_model
inputs = []
embedding_set = []
inputs.append(input_reviewerProfileGender)
embedding_set.append(embedding)
    
input_reviewerProfileAge = Input(shape=(1,))
embedding = Embedding(6, 3, input_length=1)(input_reviewerProfileAge)
embedding = Reshape(target_shape=(3,))(embedding)
inputs.append(input_reviewerProfileAge)
embedding_set.append(embedding)
    
input_priceValue = Input(shape=(1,))
embedding = Embedding(13, 6, input_length=1)(input_priceValue)
embedding = Reshape(target_shape=(6,))(embedding)
inputs.append(input_priceValue)
embedding_set.append(embedding)
    
input_month = Input(shape=(1,))
embedding = Embedding(12, 6, input_length=1)(input_month)
embedding = Reshape(target_shape=(6,))(embedding)
inputs.append(input_month)
embedding_set.append(embedding)
    
input_creditCard = Input(shape=(1,))
embedding = Embedding(3, 2, input_length=1)(input_creditCard)
embedding = Reshape(target_shape=(2,))(embedding)
inputs.append(input_creditCard)
embedding_set.append(embedding)
    
input_parking = Input(shape=(1,))
embedding = Embedding(3, 2, input_length=1)(input_parking)
embedding = Reshape(target_shape=(2,))(embedding)
inputs.append(input_parking)
embedding_set.append(embedding)
    
input_smoking = Input(shape=(1,))
embedding = Embedding(3, 2, input_length=1)(input_smoking)
embedding = Reshape(target_shape=(2,))(embedding)
inputs.append(input_smoking)
embedding_set.append(embedding)
    
input_step1 = Input(shape=(1,))
embedding = Embedding(4, 2, input_length=1)(input_step1)
embedding = Reshape(target_shape=(2,))(embedding)
inputs.append(input_step1)
embedding_set.append(embedding)
    
input_step2 = Input(shape=(1,))
embedding = Embedding(16, 6, input_length=1)(input_step2)
embedding = Reshape(target_shape=(6,))(embedding)
inputs.append(input_step2)
embedding_set.append(embedding)
    
input_step3 = Input(shape=(1,))
embedding = Embedding(93, 20, input_length=1)(input_step3)
embedding = Reshape(target_shape=(20,))(embedding)
inputs.append(input_step3)
embedding_set.append(embedding)

    
    # User-normalized vector Embedding 
    
input_numeric_user = Input(shape=( user_num ,))
embedding_numeric_user = Dense(300)(input_numeric_user)
hidden_layer_01 = Activation('sigmoid')(embedding_numeric_user)
drop_layer_01 = Dropout(.2)(hidden_layer_01)
    
hidden_layer_012 = Dense(100)(drop_layer_01)
hidden_layer_012 = Activation('sigmoid')(hidden_layer_012 )
drop_layer_012 = Dropout(.1)(hidden_layer_012)
    
inputs.append(input_numeric_user)
embedding_set.append(drop_layer_012)
    
    
    # Item-normalized vector Embedding
    
input_numeric_item = Input(shape=( item_num ,))
embedding_numeric_item = Dense(300)(input_numeric_item ) 
hidden_layer_02 = Activation('sigmoid')(embedding_numeric_item)
drop_layer_02 = Dropout(.2)(hidden_layer_02)
    
hidden_layer_022 = Dense(100)(drop_layer_02)
hidden_layer_022 = Activation('sigmoid')(hidden_layer_022 )
drop_layer_022 = Dropout(.1)(hidden_layer_022)
    
inputs.append(input_numeric_item )
embedding_set.append(drop_layer_022)
    
    
    # Concatenate all Features : Make a final input vector
merged_x = Concatenate()(embedding_set)
    
    
    # Construct hidden layers
hidden_layer_1 = Dense(300)(merged_x)
    # hidden_layer_1 = BatchNormalization()(hidden_layer_1) 
hidden_layer_1 = Activation('sigmoid')(hidden_layer_1)
drop_layer_1 = Dropout(.2)(hidden_layer_1)
    
    
hidden_layer_2 = Dense(100)(drop_layer_1)
    # hidden_layer_2= BatchNormalization()(hidden_layer_2)
hidden_layer_2 = Activation('sigmoid')(hidden_layer_2)
    # drop_layer_2 = Dropout(.2)(hidden_layer_2)
    
    
hidden_layer_3 = Dense(50)(hidden_layer_2)
    # hidden_layer_3 = BatchNormalization()(hidden_layer_3)
hidden_layer_3 = Activation('sigmoid')(hidden_layer_3)
    # drop_layer_3 = Dropout(.2)(hidden_layer_3)
    
    
output = Dense(1)(hidden_layer_3)
    # output = BatchNormalization()(output)
output = Activation('sigmoid')(output)
    
model = Model(inputs, output)
# Contents Features Embedding 
    

    
plot_model(model, to_file='model_plot.png', show_shapes=True, show_layer_names=True)


# ### Function for import form ( because of embbeding )
# Embedded Neural Net Model의 Form에 맞추기 위해 Input data를 변수별로 구분되어 있는 List 형태로 바꾸어 줍니다.

# In[ ]:


#converting data to list format to match the network structure
def importform_full(X_train,X_valid, X_test):

    input_list_train = []
    input_list_valid = []
    input_list_test = []
    
    # Contents Features 
    for c in embed_cols:
        raw_vals = np.unique(X_train[c])
        val_map = {}
        for i in range(len(raw_vals)):
            val_map[raw_vals[i]] = i       
        input_list_train.append(X_train[c].map(val_map).fillna(0).values)
        input_list_valid.append(X_valid[c].map(val_map).fillna(0).values)
        input_list_test.append(X_test[c].map(val_map).fillna(0).values)
    
    # Rating features
    user_cols = [c for c in X_train.columns if (c in user_normalized_cols)]
    input_list_train.append(X_train[user_cols ].values)
    input_list_valid.append(X_valid[user_cols ].values)
    input_list_test.append(X_test[user_cols ].values)
    
    item_cols = [c for c in X_train.columns if (c in item_normalized_cols)]
    input_list_train.append(X_train[item_cols].values)
    input_list_valid.append(X_valid[item_cols].values)
    input_list_test.append(X_test[item_cols].values)
    
    return input_list_train, input_list_valid, input_list_test    


# In[20]:


# input form
list_X_train, list_X_valid , list_X_test= inputform_full(X_train, X_valid, X_test)


# ## Model Train 

# In[23]:


dnn_model_lunch = embedding_dnn()
dnn_model_lunch.fit( list_X_train, y_train.values, epochs=15, batch_size=800, verbose=2)


# ## Predict and Error(RMSE)
# ### Validation Predict and Tuning

# In[ ]:


# lunch_valid_rmse
np.sqrt(mean_squared_error(5*y_valid,5*y_preds)  )


# ### Precision / Recall / F-score  (criterion : 3.5 (median of rating) )

# In[ ]:


real_y = y_valid*5
real_y_pre = y_preds*5

idx = real_y>=3.5
real_y[idx] =  1
real_y[-idx] = 0 

idx2 = real_y_pre >=3.5
real_y_pre[idx2] =  1
real_y_pre[~idx2] = 0 


# In[ ]:


confusion_matrix(real_y,real_y_pre)


# In[ ]:


f1_score(real_y,real_y_pre)


# In[ ]:


accuracy_score(real_y,real_y_pre)


# ## Model Save

# In[ ]:


# serialize model to JSON
model_json = dnn_model_lunch.to_json()
with open("usertoitem_minato_dinner_json", "w") as json_file:
    json_file.write(model_json)
# serialize weights to HDF5
dnn_model_lunch.save_weights("usertoitem_minato_dinner.h5")
print("Saved model to disk")
 


# ## Model Load

# In[ ]:


# load json and create model
json_file = open("usertoitem_minato_lunch_json", 'r')
loaded_model_json = json_file.read()
json_file.close()
loaded_model = model_from_json(loaded_model_json)
# load weights into new model
loaded_model.load_weights("usertoitem_minato_lunch.h5")
print("Loaded model from disk")


# ## K-fold CV (K-fold는 위의 과정에서 Train/Valid를 나누지 않고 X_train으로 진행하면 됩니다.)

# In[ ]:


def dnn_kfold(X_train, y_train,X_test,y_test):
    #network training

    K = 4
    runs_per_fold = 1
    n_epochs = 50

    cv_rmse_set = []
    full_val_preds = np.zeros(np.shape(X_train)[0])
    y_preds = np.zeros((np.shape(X_test)[0],K))

    kfold = KFold(n_splits = K, random_state = 1234, shuffle = True)    

    # Split Train / Valid 
    for i, (f_ind, outf_ind) in enumerate(kfold.split(X_train, y_train)):

        X_train_f, X_val_f = X_train.loc[f_ind].copy(), X_train.loc[outf_ind].copy()
        y_train_f, y_val_f = y_train[f_ind], y_train[outf_ind]
    
        X_test_f = X_test.copy()
    
        # input form
        list_X_train_f, list_X_val_f, list_X_test_f = inputform_full(X_train_f, X_val_f, X_test_f)


        # CV prediction
        val_preds = 0
        print(0)
        dnn_model = embedding_dnn()
        dnn_model.fit( list_X_train_f, y_train_f.values, epochs=n_epochs, batch_size=500, verbose=0)
        print(1)
        val_preds += dnn_model.predict(list_X_val_f)[:,0]  / runs_per_fold
        y_preds[:,i] += dnn_model.predict(list_X_test_f)[:,0]  / runs_per_fold
        
        full_val_preds[outf_ind] += val_preds
        
        sigmoid_mse = mean_squared_error(y_val_f.values, val_preds)
        cv_rmse = np.sqrt(sigmoid_mse * 25)
        cv_rmse_set.append(cv_rmse)
        cv_rmse_set_average = np.mean(cv_rmse_set)
        print ('\nFold %i prediction cv rmse: %.5f\n' %(i,cv_rmse))

    print('Mean out of fold rmse: %.5f' % cv_rmse_set_average )
    print('Full validation rmse: %.5f' % np.sqrt(mean_squared_error(y_train.values, full_val_preds)*25 ) )

    y_pred_final = np.mean(y_preds, axis=1)

    test_rmse_final = np.sqrt(mean_squared_error(y_test,y_pred_final) * 25 )

    print('Final test rmse: %.5f' % test_rmse_final)

    return cv_rmse_set_average ,test_rmse_final 


# ## Grid Search
# 하이퍼파라미터 튜닝은 Grid Search를 활용했으며 현재까지 2차 Search를 진행하였다. 1차 결과를 바탕으로 batch_size, epochs, active fun을 고정시키고 2차 Search에서 dropout과 nodesize를 튜닝.

# In[166]:


# 1차 Grid Search
grid = {'batch_size': [300,500,800,1000],
         'epochs': [5,10,20],
         'active': ['sigmoid','tanh','relu'] }

# 1차 결과를 바탕으로 일부 파라미터 고정 후 2차 Grid Search
grid2 = {'batch_size': [800],
        'epochs': [10,15],
        'firstdrop': [0,0.2],
        'seconddrop': [0,0.1],
        'active': ['sigmoid'],
       'firstnode': [300,500,800],
        'secondnode': [100,200],
        'thirdnode': [100,200,300]
       }


params= ParameterGrid(grid)
def expand_grid(dictionary):
   return pd.DataFrame([row for row in product(*dictionary.values())], 
                       columns=dictionary.keys())
expand_frame = expand_grid(grid)
expand_frame['rmse'] = 0
expand_frame['f1score'] = 0
expand_frame['accuracy'] = 0


# In[169]:


def embedding_dnn_grid(param):
    
    inputs = []
    embedding_set = []
    
    # embedding
    input_reviewerProfileGender = Input(shape=(1,))
    embedding = Embedding(3, 2, input_length=1)(input_reviewerProfileGender)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_reviewerProfileGender)
    embedding_set.append(embedding)
    
    input_reviewerProfileAge = Input(shape=(1,))
    embedding = Embedding(6, 3, input_length=1)(input_reviewerProfileAge)
    embedding = Reshape(target_shape=(3,))(embedding)
    inputs.append(input_reviewerProfileAge)
    embedding_set.append(embedding)
    
    input_priceValue = Input(shape=(1,))
    embedding = Embedding(13, 6, input_length=1)(input_priceValue)
    embedding = Reshape(target_shape=(6,))(embedding)
    inputs.append(input_priceValue)
    embedding_set.append(embedding)
    
    input_month = Input(shape=(1,))
    embedding = Embedding(12, 6, input_length=1)(input_month)
    embedding = Reshape(target_shape=(6,))(embedding)
    inputs.append(input_month)
    embedding_set.append(embedding)
    
    input_creditCard = Input(shape=(1,))
    embedding = Embedding(3, 2, input_length=1)(input_creditCard)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_creditCard)
    embedding_set.append(embedding)
    
    input_parking = Input(shape=(1,))
    embedding = Embedding(3, 2, input_length=1)(input_parking)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_parking)
    embedding_set.append(embedding)
    
    input_smoking = Input(shape=(1,))
    embedding = Embedding(3, 2, input_length=1)(input_smoking)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_smoking)
    embedding_set.append(embedding)
    
    input_step1 = Input(shape=(1,))
    embedding = Embedding(4, 2, input_length=1)(input_step1)
    embedding = Reshape(target_shape=(2,))(embedding)
    inputs.append(input_step1)
    embedding_set.append(embedding)
    
    input_step2 = Input(shape=(1,))
    embedding = Embedding(16, 6, input_length=1)(input_step2)
    embedding = Reshape(target_shape=(6,))(embedding)
    inputs.append(input_step2)
    embedding_set.append(embedding)
    
    input_step3 = Input(shape=(1,))
    embedding = Embedding(93, 20, input_length=1)(input_step3)
    embedding = Reshape(target_shape=(20,))(embedding)
    inputs.append(input_step3)
    embedding_set.append(embedding)
    
    
    # User-normalized vector Embedding 
    input_numeric_user = Input(shape=( user_num ,))
    embedding_numeric_user = Dense(param['firstnode'])(input_numeric_user)
    hidden_layer_01 = Activation(param['active'])(embedding_numeric_user)
    drop_layer_01 = Dropout(param['firstdrop'])(hidden_layer_01)
    
    hidden_layer_012 = Dense(param['secondnode'])(drop_layer_01)
    hidden_layer_012 = Activation(param['active'])(hidden_layer_012 )
    drop_layer_012 = Dropout(param['seconddrop'])(hidden_layer_012)
    
    inputs.append(input_numeric_user)
    embedding_set.append(drop_layer_012)
    
    
    # Item-normalized vector Embedding
    input_numeric_item = Input(shape=( item_num ,))
    embedding_numeric_item = Dense(param['firstnode'])(input_numeric_item ) 
    hidden_layer_02 = Activation(param['active'])(embedding_numeric_item)
    drop_layer_02 = Dropout(param['firstdrop'])(hidden_layer_02)
    
    hidden_layer_022 = Dense(param['secondnode'])(drop_layer_02)
    hidden_layer_022 = Activation(param['active'])(hidden_layer_022 )
    drop_layer_022 = Dropout(param['seconddrop'])(hidden_layer_022)
    
    inputs.append(input_numeric_item )
    embedding_set.append(drop_layer_022)
    
    
    # merge all variables 
    merged_x = Concatenate()(embedding_set)
    
    
    # Construct hidden layers
    hidden_layer_1 = Dense(param['thirdnode'])(merged_x)
    # hidden_layer_1 = BatchNormalization()(hidden_layer_1) 
    hidden_layer_1 = Activation(param['active'])(hidden_layer_1)
    drop_layer_1 = Dropout(param['firstdrop'])(hidden_layer_1)
    
    
    hidden_layer_2 = Dense(100)(drop_layer_1)
    # hidden_layer_2= BatchNormalization()(hidden_layer_2)
    hidden_layer_2 = Activation(param['active'])(hidden_layer_2)
    # drop_layer_2 = Dropout(.2)(hidden_layer_2)
    
    
    hidden_layer_3 = Dense(50)(hidden_layer_2)
    # hidden_layer_3 = BatchNormalization()(hidden_layer_3)
    hidden_layer_3 = Activation(param['active'])(hidden_layer_3)
    # drop_layer_3 = Dropout(.2)(hidden_layer_3)
    
    
    output = Dense(1)(hidden_layer_3)
    # output = BatchNormalization()(output)
    output = Activation('sigmoid')(output)
    
    model = Model(inputs, output)
    adamopt= Adam(lr=0.01, beta_1=0.9, beta_2=0.999, epsilon=1e-8)
    model.compile(loss='mean_squared_error', optimizer=adamopt)
    
    return model
    
    


# In[ ]:


params = ParameterGrid(grid)

rmse_grid_set = [None] * len(params)
f1_grid_set = [None] * len(params)
ac_grid_set = [None] * len(params)


# In[ ]:


# grid search

for pn in range(len(params)):
    print(pn)
    p = expand_frame.iloc[pn,:].copy()
    dnn_model_lunch_grid = embedding_dnn_grid(param = p)
    dnn_model_lunch_grid.fit( list_X_train, y_train.values, epochs= p['epochs'], batch_size=p['batch_size'], verbose=2)
    y_preds = dnn_model_lunch_grid.predict(list_X_test)[:,0] 
    rmse_grid = np.sqrt(mean_squared_error(5*y_test,5*y_preds)  )
    print(rmse_grid)
    real_y = y_test*5
    real_y_pre = y_preds*5
    idx = real_y>=3.5
    real_y[idx] =  1
    real_y[-idx] = 0 
    idx2 = real_y_pre >=3.5
    real_y_pre[idx2] =  1
    real_y_pre[~idx2] = 0 
    f1s = f1_score(real_y,real_y_pre)
    acs = accuracy_score(real_y,real_y_pre)
    print(f1s)
    print(acs)
    rmse_grid_set[pn] = rmse_grid
    f1_grid_set[pn] = f1s
    ac_grid_set[pn] = acs
    print(expand_frame.iloc[pn,:].copy())
    
    


# ## After Tuning, Final test set predict 

# In[ ]:


y_preds = loaded_model.predict(list_X_test)[:,0] 


# In[100]:


# lunch_test_rmse
np.sqrt(mean_squared_error(5*y_test,5*y_preds)  )


# ### Precision / Recall / F-score  (criterion : 3.5 (median of rating) )

# In[ ]:


real_y = y_test*5
real_y_pre = y_preds*5

idx = real_y>=3.5
real_y[idx] =  1
real_y[-idx] = 0 

idx2 = real_y_pre >=3.5
real_y_pre[idx2] =  1
real_y_pre[~idx2] = 0 


# In[71]:


confusion_matrix(real_y,real_y_pre)


# In[72]:


f1_score(real_y,real_y_pre)


# In[73]:


accuracy_score(real_y,real_y_pre)


# In[ ]:


d = { 'dnn_pred' : 5*y_preds}
df_preds = pd.DataFrame(d)
lunch_test_data_result = pd.concat([lunch_test_data, df_preds ], axis=1)
# lunch_test_data_result = lunch_test_data_result.iloc[6:,:]
lunch_test_data_result.to_csv('lunch_test_data_result.csv',index = False )y_preds = dnn_model_lunch.predict(list_X_valid)[:,0] 


# # User to User Recommand Simulation
# 하나의 User를 입력했을 때, Restaurants에 대한 예측 평점을 구하고 Top N 개를 추천해주는 Function 입니다.

# In[ ]:


#converting data to list format to match the network structure
def importform_one(X_test):

    input_list_test = []
    
    #the cols to be embedded: rescaling to range [0, # values)
    for c in embed_cols:
        raw_vals = np.unique(X_test[c])
        val_map = {}
        for i in range(len(raw_vals)):
            val_map[raw_vals[i]] = i       
        input_list_test.append(X_test[c].map(val_map).fillna(0).values)
    
    #the rest of the columns
    user_cols = [c for c in X_train_columns if (c in user_normalized_cols)]
    input_list_test.append(X_test[user_cols ].values)
    
    item_cols = [c for c in X_train_columns if (c in item_normalized_cols)]
    input_list_test.append(X_test[item_cols].values)
    
    return input_list_test    


# In[ ]:


# Make Restaurants Set 
full_data = pd.concat([lunch_train_data_final, lunch_test_data_final],sort=False)


# In[ ]:


def randomuser_recommnad(user = None,method = 'random', options = False, log = False , month_now = False ,N=10 ):
    # pick random user
    
    if user != None:
        random_user = full_data >> mask(X.reviewerId == user)
        random_user = random_user.sample(n=1)
        method = None
    
    if method == 'random':
        random_user = full_data.sample(n=1)
        
    if method == 'vip':
        bb=full_data.groupby(by = 'reviewerId').size()
        idx = np.where(bb >= 100)
        name_list=bb.index[np.asarray(idx)]
        vip_id = np.random.choice( name_list[0] )
        vip_data = full_data >> mask(X.reviewerId == vip_id)
        random_user = vip_data.sample(1)

    # random_user = random_user.drop(columns = ['ratingTotalScore'])
    print(random_user.iloc[:,1:4])
    idx = random_user[user_normalized_cols].values[0] != min(random_user[user_normalized_cols].values[0])
    print(random_user[user_normalized_cols].iloc[:,idx])
    
    #random_user[["reviewerProfileGender","reviewerProfileAge","month"]]
    
    if month_now == True :
        selected_data = full_data >> mask(X.month == random_user['month'].values[0])
    if month_now == False :
        selected_data = full_data.copy()

    
    selected_data['reviewerId'] = random_user['reviewerId'].values[0]
    selected_data['reviewerProfileGender'] = random_user['reviewerProfileGender'].values[0]
    selected_data['reviewerProfileAge'] = random_user['reviewerProfileAge'].values[0]
    
    random_user_cols = [c for c in X_train_columns if (c in user_normalized_cols)]
    selected_data[user_normalized_cols] = random_user[random_user_cols].values[0]
    
    if options == True:
        
        smoking = input('흡연이 가능한 곳이어 한다면 yes, 불가능한 곳이어야 한다면 no을 입력해주시고, 상관없다면 skip해주세요. (skip : Enter키)')
        if smoking == 'yes':
            selected_data =  selected_data.iloc[np.where(selected_data['smoking.available'] == 'TRUE')]
        if smoking == 'no':
             selected_data =  selected_data.iloc[np.where(selected_data['smoking.available'] == 'FALSE')]
                
        card = input('신용카드가 가능한 곳이어야 하면 yes를, 상관없다면 skip해주세요.(skip : Enter키) ')
        if card == 'yes':
            selected_data =  selected_data.iloc[np.where(selected_data['creditCard.available'] == 'TRUE')]

        car = input('주차가 가능한 곳이어야 하면 yes를, 상관없다면 skip해주세요. (skip : Enter키)')
        if car== 'yes':
            selected_data =  selected_data.iloc[np.where(selected_data['parking.available'] == 'TRUE')]
                
        step1 = input('식당, 주점, 카페 중 원하는 곳을 입력해주세요. 상관없다면 skip해주세요. (skip : Enter키)' )
        if step1== '식당':
            selected_data =  selected_data.iloc[np.where(selected_data['ko.step1'] == '식당')]
        if step1 == '주점':
            selected_data =  selected_data.iloc[np.where(selected_data['ko.step1'] == '주점')]
        if step1 == '카페':
            selected_data =  selected_data.iloc[np.where(selected_data['ko.step1'] == '카페, 디저트')]
    
    list_selected_data=importform_one(selected_data)
    y_preds_randomuser = loaded_model.predict(list_selected_data)[:,0]  # predict 
    
    dd = {'restaurantId': selected_data["restaurantId"].values, 'Rating': y_preds_randomuser}
    expected_rating_frame = pd.DataFrame(data =dd)
    expected_rating_frame['Rating'] = 5*expected_rating_frame['Rating']
    expected_rating_frame=expected_rating_frame.groupby(by='restaurantId').mean()
    expected_rating_frame = expected_rating_frame.sort_values('Rating',ascending=False)
    
    if log == True:
        return expected_rating_frame[0:N], expected_rating_frame 
    if log == False:
        return expected_rating_frame[0:N]
    


# In[49]:


randomuser_recommnad(user='/rvwr/000314464/', options = True,month_now = True)


# In[158]:


randomuser_recommnad( options = True)


# In[80]:


randomuser_recommnad(method = 'vip', options = True)


# In[1]:


randomuser_recommnad( user = '/rvwr/001831877/',options = True,log= True)


# In[ ]:


urlpath = 'https://tabelog.com'
webbrowser.open_new(urlpath + '/tokyo/A1316/A131602/13194179/')


# ## 100 sample users
# Matrix Factorization Model과의 비교를 위한 100명 샘플 추출. 각 User별로 10개의 식당을 추천받고 두 모델에서 겹치게 추천되는 식당이 몇 개 인지 파악
# 이 때 유저의 리뷰 수 가 예측결과에 영향을 미치는지 파악하기 위해 리뷰 수 별로 층화추출하였다.

# In[69]:


reviewer_list = X_test.groupby(by='reviewerId').size()


# In[81]:


# 리뷰 수를 기준으로 4개 그룹으로 유저 그룹화
group1 = reviewer_list[(reviewer_list == 1)]
group2 = reviewer_list[(reviewer_list > 1) & (reviewer_list <= 10 )]
group3 = reviewer_list[(reviewer_list >  10 ) & (reviewer_list <= 20 )]
group4 = reviewer_list[(reviewer_list > 20) ]


# In[83]:


# 그룹별로 층화추출
group1_sample = group1.sample( 20,replace = False)
group2_sample = group2.sample( 40,replace = False)
group3_sample = group3.sample( 30,replace = False)
group4_sample = group4.sample( 10,replace = False)


# In[90]:


sample_users = group1_sample.append(group2_sample).append(group3_sample).append(group4_sample)
sample_user = pd.DataFrame(sample_users).sort_values([0])
sample_user.to_csv('sample_user.csv',index=True)


# In[ ]:


sample_user = pd.read_csv('sample_user.csv',index=True) 
sample_user_id= sample_user.index
sample_rep = np.repeat(sample_user_id, 10)
sample_rep = pd.DataFrame(sample_rep)
sample_rep['item'] = 'blank'

for i in range(len(sample_user_id)):
    result = randomuser_recommnad(user =  sample_user_id[i] , options = False, log = False  )
    result = result.index
    sample_rep['item'][ (10*i):(10*i+10) ] = result 
    print(sample_rep['item'][ (10*i):(10*i+10) ])


# In[ ]:


sample_rep.to_csv('sample_user_result.csv',index=True)


# In[ ]:




