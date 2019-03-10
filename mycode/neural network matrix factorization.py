#!/usr/bin/env python
# coding: utf-8

# ### Import Library

# In[1]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import warnings
from dfply import *
warnings.filterwarnings('ignore')

get_ipython().run_line_magic('matplotlib', 'inline')


# In[2]:


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
from keras.models import model_from_json


# ### Source 

# In[3]:


## mf 개념 : https://arxiv.org/ftp/arxiv/papers/1503/1503.07475.pdf
## keras embedding 코드 참고 : https://nipunbatra.github.io/blog/2017/neural-collaborative-filtering.html


# ### Data Loading 

# #### 일본 도쿄 중 港区 (미나토 구)를 대상으로 모델링 진행

# In[5]:


path = '/home/edc8785/'
review = pd.read_csv(path + 'total_dat_lunch.csv')


# #### reviewer id와 restaurant id를 고유 숫자로 변경

# In[5]:


review.reviewerId = review.reviewerId.astype("category").cat.codes.values
review.restaurantId = review.restaurantId.astype("category").cat.codes.values


# In[6]:


review.head()


# #### user와 restaurant의 unique 수 확인 

# In[7]:


user_num = len(review.reviewerId.unique())
rest_num = len(review.restaurantId.unique())


# In[8]:


print(user_num, rest_num)


# #### 최근 6개월 데이터를 test셋으로 설정 

# In[9]:


train_review = (review >> mask(X.visitDate < 20170101  ))
test_review = (review >> mask(X.visitDate >= 20170101 ))


# In[10]:


print(len(train_review), len(test_review))


# #### feature selection (불필요한 변수 삭제)

# In[11]:


train_review = train_review[["reviewerId","restaurantId","ratingTotalScore",
                             "creditCard.available","parking.available","smoking.available",
                             "ko.step1","ko.step2","ko.step3",
                             "reviewerProfileGender","reviewerProfileAge","priceValue"]]
train_review = train_review.rename(columns={'reviewerId': 'user_id', 'restaurantId': 'item_id', 'ratingTotalScore': 'rating',
                                            'creditCard.available': 'credit', 'parking.available': 'parking', 'smoking.available': 'smoking',
                                            'ko.step1': 'step1', 'ko.step2': 'step2', 'ko.step3': 'step3'})

test_review = test_review[["reviewerId","restaurantId","ratingTotalScore",
                             "creditCard.available","parking.available","smoking.available",
                             "ko.step1","ko.step2","ko.step3",
                             "reviewerProfileGender","reviewerProfileAge","priceValue"]]
test_review = test_review.rename(columns={'reviewerId': 'user_id', 'restaurantId': 'item_id', 'ratingTotalScore': 'rating',
                                            'creditCard.available': 'credit', 'parking.available': 'parking', 'smoking.available': 'smoking',
                                            'ko.step1': 'step1', 'ko.step2': 'step2', 'ko.step3': 'step3'})


# #### check sparsity 

# In[12]:


train = train_review[["user_id","item_id","rating"]]

df_table = train.set_index(["item_id", "user_id"]).unstack()
plt.imshow(df_table)
plt.grid(False)
plt.xlabel("The Number of User")
plt.ylabel("The Number of Item")
plt.title("Rate Matrix")
plt.show()


# #### 위의 그래프에서 볼 수 있듯이 유저와 식당의 수가 많다보니 굉장히 sparse함을 한 눈에 확인할 수 있다. (대략 0.1%) 이와 같은 경우 기존의 협업 필터링을 그대로 이용하기엔 어려 문제점이 발생되는데 matrix factorization의 경우 이런 문제를 해결이 가능하다.

# #### 1 ~ 5 rating을 5로 나눈 후 모델링 & RMSE 구하는 과정에서 다시 5를 곱해서 계산 

# In[13]:


train_review["rating"] = train_review["rating"]/5
test_review["rating"] = test_review["rating"]/5


# ### Model Structure 

# #### 모델의 원리에 대해 간단하게 설명하자면 일반적인 matrix factorization의 경우 user와 restaurant 매트릭스를 각각 user 매트릭스, restaurant 매트릭스로 구분한 뒤 다시 이 행렬의 곱을 이용하여 평점을 예측하는 방식이다. 즉, 유저 행렬의 i번째 행과 식당 행렬의 j번째 열을 곱함으로써 i번째 유저가 j번째 식당에 남길 평점을 예측하는 것이다. 그리고 행렬의 곱을 하기 위해 유저 행렬의 열의 수와 식당 행렬의 행의 수가 정확히 일치해야만 한다. 
# 
# #### neural network matrix factorization 경우 역시 원리는 비슷한데 다만 이 경우는 행렬의 곱을 이용하지 않는다. i번째유저와 j번째 식당의 정보를 하나의 벡터로 펼친 뒤 target에 평점을 두고 이를 학습시키는 원리이다. 즉, 각각의 사람과 식당을 하나의 카테고리라고 설정을 하고 이를 임베딩 시키는 작업을 통해 각각의 벡터를 생성하여 학습시키는 것이다. 따라서 이 경우 user_factor와 rest_factor의 수를 일치시킬 필요가 없다. 그렇다면 이 factor 수는 어떻게 구하는가? 여러 방법이 있지만 여기서 이용한 방식은 이를 하나의 hyper parameter라고 설정을 한 뒤 이 역시 grid search를 통해 최적의 수를 찾아내는 것이다.

# In[14]:


user_factor = 300
rest_factor = 300


# In[15]:


rest_input = keras.layers.Input(shape=[1], name="Rest")
rest_embedding = keras.layers.Embedding(rest_num, rest_factor, name="Rest-Embedding")(rest_input)
rest_vec = keras.layers.Flatten(name = "FlattenRest")(rest_embedding)


# In[16]:


user_input = keras.layers.Input(shape=[1], name="User")
user_embedding = keras.layers.Embedding(user_num, user_factor, name="User-Embedding")(user_input)
user_vec = keras.layers.Flatten(name = "FlattenUser")(user_embedding)


# In[17]:


## embeddings_constraint=non_neg() -> non-negatvie matrix factorization


# #### 참고: 추천시스템에서 non negative matrix factorization을 주로 이용하는데 이는 매트릭스의 분해과정에서 음수 값이 나오지 않도록 설정함으로써 모델이 더욱 빠르게 학습하는 효과를 가짐. 허나, 이 경우 non negative constraint를 이용할 경우 rmse 결과값이 생각보다 많이 증가하고, 제약 조건을 이용하지 않아도 모델이 학습하는 시간이 그리 오래 걸리지 않아 non negative constraint는 이용하지 않음. 만일 이 제약조건을 이용하려면 keras.layers.Embedding( ~~~~ embedding_constraint = non_neg() ) 내장 함수를 이용하여 적용가능

# #### 기존의 matrix factorization는 유저 식당 평점의 정보만 이용을 할 뿐, 유저의 정보나 식당의 정보는 전혀 사용하지 않는다. 또한, nnmf에 대해 참고한 논문과 자료에서 역시 이런 feature들을 사용하지 않는데 이런 정보를 추가적으로 이용해보았다.

# In[18]:


rest_feature_input1 = keras.layers.Input(shape=(1,), name="rest_feature1")
rest_feature_embedding1 = keras.layers.Embedding(input_dim=3, output_dim=2, input_length=1)(rest_feature_input1)
rest_feature_embedding1 = keras.layers.Flatten(name = "feature1")(rest_feature_embedding1)

rest_feature_input2 = keras.layers.Input(shape=(1,), name="rest_feature2")
rest_feature_embedding2 = keras.layers.Embedding(input_dim=3, output_dim=2, input_length=1)(rest_feature_input2)
rest_feature_embedding2 = keras.layers.Flatten(name = "feature2")(rest_feature_embedding2)

rest_feature_input3 = keras.layers.Input(shape=(1,), name="rest_feature3")
rest_feature_embedding3 = keras.layers.Embedding(input_dim=3, output_dim=2, input_length=1)(rest_feature_input3)
rest_feature_embedding3 = keras.layers.Flatten(name = "feature3")(rest_feature_embedding3)


# In[19]:


rest_feature_input4 = keras.layers.Input(shape=(1,), name="rest_feature4")
rest_feature_embedding4 = keras.layers.Embedding(name="ko.step1",input_dim=4, output_dim=2, input_length=1)(rest_feature_input4)
rest_feature_embedding4 = keras.layers.Flatten(name = "feature4")(rest_feature_embedding4)

rest_feature_input5 = keras.layers.Input(shape=(1,), name="rest_feature5")
rest_feature_embedding5 = keras.layers.Embedding(input_dim=16, output_dim=6, input_length=1)(rest_feature_input5)
rest_feature_embedding5 = keras.layers.Flatten(name = "feature5")(rest_feature_embedding5)

rest_feature_input6 = keras.layers.Input(shape=(1,), name="rest_feature6")
rest_feature_embedding6 = keras.layers.Embedding(input_dim=93, output_dim=20, input_length=1)(rest_feature_input6)
rest_feature_embedding6 = keras.layers.Flatten(name = "feature6")(rest_feature_embedding6)


# In[20]:


rest_feature_input7 = keras.layers.Input(shape=(1,), name="rest_feature7")
rest_feature_embedding7 = keras.layers.Embedding(name="gender", input_dim=3, output_dim=2, input_length=1)(rest_feature_input7)
rest_feature_embedding7 = keras.layers.Flatten(name = "feature7")(rest_feature_embedding7)

rest_feature_input8 = keras.layers.Input(shape=(1,), name="rest_feature8")
rest_feature_embedding8 = keras.layers.Embedding(name = "age", input_dim=6, output_dim=3, input_length=1)(rest_feature_input8)
rest_feature_embedding8 = keras.layers.Flatten(name = "feature8")(rest_feature_embedding8)

rest_feature_input9 = keras.layers.Input(shape=(1,), name="rest_feature9")
rest_feature_embedding9 = keras.layers.Embedding(input_dim=13, output_dim=6, input_length=1)(rest_feature_input9)
rest_feature_embedding9 = keras.layers.Flatten(name = "feature9")(rest_feature_embedding9)


# #### feature_input 1 ~ 9는 순서대로 creditCard.available, parking.available, smoking.available, ko.step1, ko.step2, ko.step3, reviewerProfileGender, reviewerProfileAge, priceValue의 정보가 임베딩되어 들어간다.
# 
# #### 추가적으로, 식당의 정보보단 유저의 정보가 모델의 예측력 개선에 큰 영향을 끼쳤으며 이는 아마 개인이 보통 다양한 식당에 가고, 리뷰를 남기는 반면 성별이나 연령대에 따라 선호하는 식당의 종류가 다른 것에 기인한 것이라 생각된다.

# In[21]:


concat = keras.layers.concatenate([rest_vec, user_vec, rest_feature_embedding1,
                                  rest_feature_embedding2, rest_feature_embedding3,
                                  rest_feature_embedding4, rest_feature_embedding5,
                                  rest_feature_embedding6, rest_feature_embedding7,
                                  rest_feature_embedding8, rest_feature_embedding9,
                                  ], name="Concat")

dense_1 = keras.layers.Dense(250, name = "FullyConnected1", activation="sigmoid")(concat)
dropout_1 = keras.layers.Dropout(0.1, name="Dropout1")(dense_1)
dense_2 = keras.layers.Dense(300, name = "FullyConnected2", activation="sigmoid")(dropout_1)
dropout_2 = keras.layers.Dropout(0, name="Dropout2")(dense_2)
dense_3 = keras.layers.Dense(150, name = "FullyConnected3", activation="sigmoid")(dropout_2)
dropout_3 = keras.layers.Dropout(0, name="Dropout3")(dense_3)

result = keras.layers.Dense(1, activation = None)(dropout_3)


# In[22]:


model = keras.Model([user_input, rest_input, rest_feature_input1,
                    rest_feature_input2, rest_feature_input3,
                    rest_feature_input4, rest_feature_input5,
                    rest_feature_input6, rest_feature_input7,
                    rest_feature_input8, rest_feature_input9,
                    ], result)

model.compile(optimizer=Adam(lr=0.001), loss="mean_squared_error",metrics=['mean_squared_error'])


# ### split train & validation set

# In[23]:


train_data, validation_data = train_test_split(train_review, test_size=0.2)

model_fit = model.fit([train_data.user_id, train_data.item_id, 
                       train_data.credit, train_data.parking,
                       train_data.smoking, train_data.step1,
                       train_data.step2, train_data.step3,
                       train_data.reviewerProfileGender, train_data.reviewerProfileAge,
                       train_data.priceValue
                       ], train_data.rating, epochs=10, batch_size = 700)


# ### validation RMSE 

# In[24]:


validation_predict = model.predict([validation_data.user_id, validation_data.item_id, 
                       validation_data.credit, validation_data.parking,
                       validation_data.smoking, validation_data.step1,
                       validation_data.step2, validation_data.step3,
                       validation_data.reviewerProfileGender, validation_data.reviewerProfileAge,
                       validation_data.priceValue
                          ])[:,0]

validation_real = validation_data.rating


# In[25]:


## 모델링을 진행하기 전 1~5의 평점을 0~1사이로 맞추기 위해 5를 나눠줬으므로 이를 다시 5를 곱해주었다.

validation_predict = validation_predict * 5
validation_real = validation_real *5

math.sqrt(sum((validation_predict-validation_real) **2)/len(validation_real))


# ### test RMSE 

# In[26]:


y_predict = model.predict([test_review.user_id, test_review.item_id, 
                       test_review.credit, test_review.parking,
                       test_review.smoking, test_review.step1,
                       test_review.step2, test_review.step3,
                       test_review.reviewerProfileGender, test_review.reviewerProfileAge,
                       test_review.priceValue
                          ])[:,0]

y_real = test_review.rating


# In[27]:


y_predict = y_predict * 5
y_real = y_real *5


# In[28]:


math.sqrt(sum((y_predict-y_real) **2)/len(y_real))


# ### model save & load 

# In[27]:


model_json = model.to_json()
with open("model.json", "w") as json_file:
    json_file.write(model_json)
    
# serialize weights to HDF5
model.save_weights("save_model")
print("Saved model to disk")


# In[34]:


# load json and create model
json_file = open('model.json', 'r')
loaded_model_json = json_file.read()
json_file.close()
loaded_model = model_from_json(loaded_model_json)

# load weights into new model
loaded_model.load_weights("save_model")
print("Loaded model from disk")

# evaluate loaded model on test data
loaded_model.compile(optimizer=Adam(lr=0.001), loss="mean_squared_error",metrics=['mean_squared_error'])


# #### gender embedding result check!

# In[35]:


def cosine_dist(vector1, vector2):
    numer = sum(vector1 * vector2)
    denom = math.sqrt(sum(vector1**2)) * math.sqrt(sum(vector2**2))
    
    return(numer/denom)


# In[36]:


rest_feature7_embedding_weights = loaded_model.get_layer(name = "gender" ).get_weights()[0]


# In[41]:


gender = pd.DataFrame(rest_feature7_embedding_weights, columns=[["factor1","factor2"]])
gender.index = [["na","female","male"]]
gender
## NA 0, female1, male 2 -> 이를 2차원 값으로 임베딩


# In[42]:


na = np.array(gender.iloc[0])
female = np.array(gender.iloc[1])
male = np.array(gender.iloc[2])


# In[43]:


print(cosine_dist(na,female), cosine_dist(na,male), cosine_dist(female,male))


# #### gender의 embedding 값을 확인한 결과, na값들은 male보단 female과 유사한 것으로 볼 수 있다. 

# #### age embedding result check! 

# In[44]:


rest_feature8_embedding_weights = loaded_model.get_layer(name = "age" ).get_weights()[0]


# In[45]:


age = pd.DataFrame(rest_feature8_embedding_weights)
age


# In[46]:


age0 = np.array(age.iloc[0])
age1 = np.array(age.iloc[1])
age2 = np.array(age.iloc[2])
age3 = np.array(age.iloc[3])
age4 = np.array(age.iloc[4])
age5 = np.array(age.iloc[5])


# In[47]:


print(cosine_dist(age0,age1), cosine_dist(age0,age2), cosine_dist(age0,age3), 
     cosine_dist(age0,age4), cosine_dist(age0,age5))


# ### user to item recommendation 

# #### 유저 아이디를 입력하면 본인이 간 곳은 제외하고 예측 평점이 상위 10개의 식당을 추천하는 시뮬레이션

# In[43]:


path = '/home/edc8785/'
population_user = pd.read_csv(path + 'total_dat_lunch.csv')

population_user["restaurantId_str"] = population_user["restaurantId"]
population_user["reviewerId_str"] = population_user["reviewerId"]

population_user.restaurantId = population_user.restaurantId.astype("category").cat.codes.values
population_user.reviewerId = population_user.reviewerId.astype("category").cat.codes.values


# #### simulation을 위해 함수 생성 (user의 정보 가져오고, 식당의 정보 가져오는 함수) 

# In[144]:


def search_user(user_id):
    sample = population_user >> mask(X.reviewerId_str == user_id)
    sample_id = sample["reviewerId"].iloc[0]
    sample_gender = sample["reviewerProfileGender"].iloc[0]
    sample_age = sample["reviewerProfileAge"].iloc[0]
    
    return(sample_id, sample_gender, sample_age)

def search_rest(rest_id):
    sample = population_user >> mask(X.restaurantId == int(rest_id))
    sample_id = sample["restaurantId_str"].iloc[0]
    sample_credit = sample["creditCard.available"].iloc[0]
    sample_parking = sample["parking.available"].iloc[0]
    sample_smoking = sample["smoking.available"].iloc[0]
    sample_step1 = sample["ko.step1"].iloc[0]
    sample_step2 = sample["ko.step2"].iloc[0]
    sample_step3 = sample["ko.step3"].iloc[0]
    sample_price = sample["priceValue"].iloc[0]
    
    
    return(sample_id, sample_credit, sample_parking, sample_smoking, sample_step1, sample_step2, sample_step3, sample_price)


# #### 시뮬레이션 마다 식당 데이터 셋을 구성해서 진행하면 시간이 오래걸리므로 식당 데이터셋을 미리 구성해놓고 시뮬레이션시 활용 

# In[65]:


def rest_inform():
    dat = pd.DataFrame()
    
    dat["rest_id"] = None
    dat["rest_url"] = None    
    dat["credit"] = None
    dat["parking"] = None
    dat["smoking"] = None
    dat["step1"] = None
    dat["step2"] = None
    dat["step3"] = None
    dat["price"] = None
    
    for i in range(0, rest_num):
        if( i % 1000 == 0 ):
            print(i)
        
        a1,a2,a3,a4,a5,a6,a7,a8 = search_rest(i)

        dat = dat.append({"rest_id": i,
                   "rest_url": a1, "credit":a2, "parking":a3, "smoking":a4,
                    "step1":a5, "step2":a6, "step3":a7, "price":a8}, ignore_index=True)
        
    return(dat)


# In[72]:


rest_information = rest_inform()


# In[142]:


def recommendation():
    sample = rest_information.copy()
    population = population_user.copy()
    
    user_str_id = input("Enter your id: ")
    num_id, gender, age = search_user(user_str_id)
    

    sample["user_id"] = user_str_id
    sample["num_id"] = num_id
    sample["gender"] = gender
    sample["age"] = age
    
    model_predict = model.predict([sample.num_id, sample.rest_id,
                   sample.credit, sample.parking,
                   sample.smoking, sample.step1,
                   sample.step2, sample.step3,
                   sample.gender, sample.age, sample.price
                          ])[:,0]
    
    
    sample["pred_rating"] = model_predict
    sample["rating"]= 0
    
    population = population >> mask(X.reviewerId == num_id)
    for i in range(0, len(population)):
        index = population["restaurantId"].iloc[i]
        sample["rating"].loc[index] = population["ratingTotalScore"].iloc[i]
    
    
    sample = sample >> mask(X.rating == 0 )
    sample = sample.sort_values(["pred_rating"], ascending=[False]).head(10)
    
    sample = sample[["rest_url","user_id"]]
    return(sample)    


# In[140]:


## simulation "/rvwr/jackiejackie/"


# In[143]:


recommendation()


# ### Item to Item recommendation 

# In[53]:


item_embedding = model.get_layer(name="Rest-Embedding").get_weights()[0]
item_embedding = pd.DataFrame(item_embedding)


# In[177]:


item_embedding.to_csv("item_embedding.csv", index=False)


# #### 모델의 item_embedding 값을 이용하여 item_to_item recommendation에 적용
# 
# #### item_to_item_recommendation.ipynb 파일 참고
