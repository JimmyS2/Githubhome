### COSPI30 지수 예측하기 모델링
## 롤링윈도우기법을 사용 하지 않을 때 <- 지금 이대로 진행하기 
## 롤링윈도우기법을 사용할 때 <- 109~114번째 줄에 있는 코드에서의 #을 제거해서 실행하기



import tensorflow as tf
import numpy as np
import datetime
import matplotlib
import matplotlib.pyplot as plt
import os 
import pandas as pd 

from os import chdir
chdir("C:\\Users\\User\\Desktop")
tf.reset_default_graph()

tf.set_random_seed(777)   #하이퍼 파라미터값 수정을 위한 시드값 고정 

if "DISPLAY" not in os.environ:
    # remove Travis CI Error
    matplotlib.use('Agg')


# Min-Max스케일링, 언스케일링 하는 function 정의 하기 
def MinMaxScale(x):
    x_np = np.asarray(x)
    return (x_np - x_np.min()) / (x_np.max() - x_np.min() + 1e-8) 
 
def MinMaxUnScale(org_x, x):
    org_x_np = np.asarray(org_x)
    x_np = np.asarray(x)
    return (x_np * (org_x_np.max() - org_x_np.min() + 1e-8)) + org_x_np.min()
 

# 하이퍼파라미터 (Hyper Parameter)

input_data_dim =16  # 입력데이터의 컬럼 개수(Variable 개수)
output_data_dim = 1  # 결과데이터의 수
 
sequence_length =5# 1개 시퀀스의 길이(시계열데이터 입력 개수)
hidden_dim = 10    # 각 lstm 셀의 은닉층 출력 크기
forget_bias = 1.0           # 망각편향(기본값 1.0)
num_stacked_layers = 1     # stacked LSTM layers 개수
keep_prob = 1       # dropout할 때 keep할 비율 
 
iterations = 600  # 학습반복 횟수
learning_rate = 0.01       #학습률
 #iterations
 
# 데이터를 로딩
raw_data = np.loadtxt('COSPI30_final.csv', delimiter=',' , skiprows=1)
 
 
 
################################################################################################################
# 거시경제 변수들

kospi = raw_data[:,0]
norm_kospi = MinMaxScale(kospi)
hansen = raw_data[:,1]
norm_hansen = MinMaxScale(hansen)
nasdaq = raw_data[:,2]
norm_nasdaq = MinMaxScale(nasdaq)
sanhai = raw_data[:,3]
norm_sanhai = MinMaxScale(sanhai)
nikei = raw_data[:,4]
norm_nikei = MinMaxScale(nikei)
snp500 = raw_data[:,5]
norm_snp500 = MinMaxScale(snp500)
wondollar = raw_data[:,6]
norm_wondollar = MinMaxScale(wondollar)
wonyuan = raw_data[:,7]
norm_wonyuan = MinMaxScale(wonyuan)
gold = raw_data[:,8]
norm_gold = MinMaxScale(gold)

# 뉴스 텍스트마이닝 변수들
topic1  = raw_data[:,9]
norm_topic1 = MinMaxScale(topic1)
topic2  = raw_data[:,10]
norm_topic2 = MinMaxScale(topic2)
topic3  = raw_data[:,11]
norm_topic3 = MinMaxScale(topic3) 
oplex  = raw_data[:,12]
norm_oplex = MinMaxScale(oplex)

# 인플루엔서 변수들
subscribers = raw_data[:,13]
norm_subscribers = MinMaxScale(subscribers)
views = raw_data[:,14]
norm_views = MinMaxScale(views)

#COSPI30
COSPI30 = raw_data[:,15]
norm_COSPI30 = MinMaxScale(COSPI30) 


#모든 설병 변수들 병합하기 
x = np.stack(( norm_kospi, norm_hansen, norm_nasdaq, norm_sanhai,norm_nikei,
              norm_snp500,norm_wondollar,norm_wonyuan,norm_gold,  
              norm_topic1,norm_topic2,norm_topic3,norm_oplex,
              norm_subscribers,norm_views,
              norm_COSPI30 ))
x = np.transpose(x)


### 롤링 윈도우를 실행할 때는 아래의 '#'을 해제해서 각각 코드를 실행하면 된다 

#x=x[0:434,:]        #첫번째 윈도우
#x=x[62:496,:]      #두번째 윈도우
#x=x[124:558, :]    #세번째 윈도우
#x=x[186:620, :]    #네번째 윈도우
#x=x[248:682, :]    #다섯번째 윈도우
#x=x[310:744, :]    #여섯번째 윈도우

y = x[:,[-1]] #반응변수 Y 는 COSPI30  (X 의 마지막 column)

dataX = [] 
dataY = [] 
########################################################################################################################################################################################################
for i in range(0, len(y) - sequence_length):
    _x = x[i : i+sequence_length]
    _y = y[i + sequence_length] 

    dataX.append(_x) # dataX 리스트에 추가
    dataY.append(_y) # dataY 리스트에 추가

    
####train set과 test set을 분류하기 '이전'에 이미 sequence length 길이의 데이터로 그 다음날 데이터를 예측하도록 데이터 분류 
########################################################################################################################################################################################################
 
 
# 학습용/검증용/예측용 데이터를 구분하기
# 전체 데이터를 7등분 한 후 그중 5등분이 학습용, 나머지 1등분씩이 각각 검증용, 예측용 이다.    

train_size = int(len(dataY) * (5/7))
validation_size = int((len(dataY)-train_size)/2)
test_size = int((len(dataY)-train_size)/2)

 
trainX = np.array(dataX[0:train_size])
trainY = np.array(dataY[0:train_size])

validationX = np.array(dataX[train_size:(len(dataX)-validation_size)])
validationY = np.array(dataY[train_size:(len(dataY)-validation_size)])

testX = np.array(dataX[(train_size+validation_size):len(dataX)])
testY = np.array(dataY[(train_size+validation_size):len(dataY)])
########################################################################################################################################################################################################

X = tf.placeholder(tf.float32, [None, sequence_length, input_data_dim])
print("X: ", X)
Y = tf.placeholder(tf.float32, [None, 1])
print("Y: ", Y)
 
# 측정지표를 산출하기 위한 targets, predictions 생성
targets = tf.placeholder(tf.float32, [None, 1])
print("targets: ", targets)
predictions = tf.placeholder(tf.float32, [None, 1])
print("predictions: ", predictions)
##################################################################################################################################

# 모델(LSTM 네트워크) 생성 
def lstm_cell():
    cell = tf.contrib.rnn.BasicLSTMCell(num_units=hidden_dim, 
                                        forget_bias=forget_bias, state_is_tuple=True, activation=tf.nn.softsign) #활성함수로 softsign 사용
    if keep_prob < 1.0:
        cell = tf.contrib.rnn.DropoutWrapper(cell, output_keep_prob=keep_prob)
    return cell

# Stacked RNNs 생성  (우리 모델은 1층이다)
stackedRNNs = [lstm_cell() for _ in range(num_stacked_layers)] #여기서 num_stacked_layers는 1이다 
multi_cells = tf.contrib.rnn.MultiRNNCell(stackedRNNs, state_is_tuple=True) if num_stacked_layers > 1 else lstm_cell()
 

####dynamic_rnn 이라는 드라이브에 multi_cell과 X를 넣어줌으로써 아웃풋을 생성한다 
output, _states = tf.nn.dynamic_rnn(multi_cells, X, dtype=tf.float32)  
 
####위에서 뽑은 output들로 fully_connected를 만들고, 그 output들 중에서 마지막 하나만 사용한다. 
output = tf.contrib.layers.fully_connected(output[:,-1], output_data_dim, activation_fn=tf.identity) # output _data _dim은  fully_connected의 최종 출력값이므로 1 이다. 
#여기서 output는 Y의 예측값

# loss function  : 평균 제곱 오차
loss = tf.reduce_sum(tf.square(output - Y))
# 최적화 함수 : Adam
optimizer = tf.train.AdamOptimizer(learning_rate)

train = optimizer.minimize(loss)   #loss 를 줄이도록 학습한다.
 
# RMSE(Root Mean Square Error)
rmse = tf.sqrt(tf.reduce_mean(tf.squared_difference(targets, predictions)))
 
train_error_summary = [] # 학습용 데이터의 오류를 중간 중간 기록한다
validation_error_summary = [] #검증용 데이터의 오류를 중간 중간 기록한다
test_error_summary = []  # 예측용 데이터의 오류를 중간 중간 기록한다


validation_predict = ''  # 검증용 데이터로예측한결과
test_predict = ''        # 예측용데이터로 예측한 결과
 
sess = tf.Session()
sess.run(tf.global_variables_initializer())
 
####################################################################################################################
####################################################################################################################
#본격 학습시작
 
for epoch in range(iterations):     
    _, _loss = sess.run([train, loss], feed_dict={X: trainX, Y: trainY}) 
    if ((epoch+1) % 2 == 0) or (epoch == iterations-1):
        train_predict = sess.run(output, feed_dict={X: trainX})
        train_error = sess.run(rmse, feed_dict={targets: trainY, predictions: train_predict})
        train_error_summary.append(train_error)
  
        validation_predict = sess.run(output, feed_dict={X: validationX})
        validation_error = sess.run(rmse, feed_dict={targets: validationY, predictions: validation_predict})
        validation_error_summary.append(validation_error)
        

        

 
######################################################################################################################################################################################################################################## 
# 최종 결과 그래프 
        
plt.figure(1)
plt.plot(train_error_summary, 'gold')
plt.plot(validation_error_summary, 'b')
plt.xlabel('학습 수')
plt.ylabel('RMSE')
 
#train plot 
plt.figure(2)
plt.plot(trainY, 'r')              #실제값
plt.plot(train_predict, 'b')       #예측값
plt.xlabel('Time')
plt.ylabel('COSPI30')
plt.show()
 
#validtion plot 
plt.figure(3)
plt.plot(validationY, 'r')         #실제값
plt.plot(validation_predict, 'b')  #예측값
plt.xlabel('Time')
plt.ylabel('COSPI30')
plt.show()


print('input_data_dim:', input_data_dim, end='')
print(',sequence_length:', sequence_length, end='')
print(',hidden_dim:', hidden_dim, end='')
print(',iterations:', iterations, end='')


#Train
#해석의 편의를 위해 Min Max Scale을 다시 Unscale 해준다 

trainY = MinMaxUnScale(COSPI30,trainY)
train_predict = MinMaxUnScale(COSPI30,train_predict)  
train_error = sess.run(rmse, feed_dict={targets: trainY, predictions: train_predict})
print('train_rmse:',train_error)

#Validation

validationY = MinMaxUnScale(COSPI30,validationY)
validation_predict = MinMaxUnScale(COSPI30,validation_predict)
validation_error = sess.run(rmse, feed_dict={targets: validationY, predictions: validation_predict})
print('validation_rmse:',validation_error)


############################################################################################################################
##########################################################################################################################
############################################################################################################################
##########################################################################################################################
# 위의 TrainSet과 ValidationSet 가지고 하이퍼 파라미터 튜닝을 한 후 아래 Test set의 RMSE와 그래프를 최종 산출한다.
#Test part 

for epoch in range(iterations):
    if ((epoch+1) % 2 == 0) or (epoch == iterations-1):      
        test_predict = sess.run(output, feed_dict={X: testX})
        test_error = sess.run(rmse, feed_dict={targets: testY, predictions: test_predict})
        test_error_summary.append(test_error)

#test plot
plt.figure(4)
plt.plot(testY, 'r')           #실제값
plt.plot(test_predict, 'b')    #예측값
plt.xlabel('Time')
plt.ylabel('COSPI30')
plt.show()

###test rmse 를 역정규화해서 구하기 

testY = MinMaxUnScale(COSPI30,testY)
test_predict = MinMaxUnScale(COSPI30,test_predict)
test_error = sess.run(rmse, feed_dict={targets: testY, predictions: test_predict})
print('input_data_dim:', input_data_dim, end='')
print(',sequence_length:', sequence_length, end='')
print(',hidden_dim:', hidden_dim, end='')
print(',iterations:', iterations, end='')
print('test_rmse:',test_error)
####################################################################################################