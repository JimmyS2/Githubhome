

library('xgboost')
library(caret)
library(MASS)
library(leaps)
library(car)
library(LSTS)
library(lmtest)
library(randomForest)
library(dummies)
library(dplyr)
library(gam)
library(RRF)

library(plyr)
library(pander)
library(jsonlite)
library(tidyr)

movies_data<-read.csv('movie_data.csv')
str(movies_data)

colnames(movies_data) # 15개 변수 


# Remove "NA" obs (변화 없음)
library(TestDataImputation)
movies_data<-Listwise(movies_data,Mvalue = "NA") 
str(movies_data) 

## 순수익 y의 분포를 보자. 
idx<-which(movies_data$revenue==0)
movies_data$revenue[idx]<-1
movies_data$y<- log(movies_data$revenue) - log(movies_data$budget)
hist(movies_data$y,breaks=50, main='revenu 0 인 아이들 넣었을 때 y 분포')

## 히익! revenue=0인 친구들을 빼보자
movies_data<-movies_data[-idx,]
movies_data$y<- log(movies_data$revenue) - log(movies_data$budget)
hist(movies_data$y,breaks=50, main='revenu 0 인 아이들 뺐을 때 y 분포')

# 좋아 

######################################################################
################## genres text mining + clustering ###################

# phase 1

movies_data$genres<-as.character(movies_data$genres)

gg<-matrix(NA,length(movies_data$genres),7)
for ( i in 1:length(movies_data$genres)){
  k<-unlist(strsplit(movies_data$genres[i],','))
  m<-length(unlist(strsplit(movies_data$genres[i],',')))
  idx<-c(1:m)%%2==0
  k<-k[idx]
  ggi<-c(1:length(k))
  for(j in ggi){
    gg[i,j]<-k[j]
  }
}
#
for (i in 1:length(movies_data$genres)){
  for (j in 1:7){
    gg[i,j]<-substr(gg[i,j],11,nchar(gg[i,j]))
    
  }
  
}
# 
for (i in 1:length(movies_data$genres)){
  for (j in 1:7){
    gg[i,j]<-substr(gg[i,j],1,3)
    
  }
  
}

real<-matrix(NA,length(movies_data$genres),1)

for ( i in 1:length(movies_data$genres)){
  for(j in 1:7){
    if(is.na(gg[i,j])==1  ) gg[i,j]<-''
  }}

for ( i in 1:length(movies_data$genres)){
  real[i,1]<-paste(gg[i,1],gg[i,2],gg[i,3],gg[i,4],gg[i,5],gg[i,6],gg[i,7])
}


library(tm)
library(stats)  

### 맨 먼저 나오는 영화장르 하나씩만 뽑음 ###

for ( i in c(1:length(real))){
  real[i]<-substr(real[i],1,3)
  
}


real<-as.factor(real)
table(real)

real[which(real=='For')]<-'Doc'
levels(real)[10]<-'Doc'


real[which(real=='His')]<-'War'
levels(real)[10]<-'War'
levels(real)[10]<-'War/His'

real[which(real=='Wes')]<-'Mus'
levels(real)[17]<-'Mus'
levels(real)[12]<-'Mus/Wes'


movies_data$genres<-real






################################################################
######## Spoken language -> Number of spoken language ##########

movies_data$spoken_languages<-as.character(movies_data$spoken_languages)
for( i in 1:length(movies_data$spoken_languages)){
  sl<-unlist(strsplit(movies_data$spoken_languages[i],','))
  movies_data$spoken_languages[i]<-length(grep('iso',sl))

}

movies_data$spoken_languages<-as.numeric(movies_data$spoken_languages)

movies_data$spoken_languages



###############################################################################
######### production_countries-> number of countries ##########################

movies_data$production_countries<-as.character(movies_data$production_countries)

for( i in 1:length(movies_data$production_countries)){
  cl<-unlist(strsplit(movies_data$production_countries[i],','))
  movies_data$production_countries[i]<-length(grep('iso',cl))
  
}

movies_data$production_countries<-as.numeric(movies_data$production_countries)


movies_data$production_countries




#####################################################
############ release date -> month data #############

movies_data$release_date<-as.character(movies_data$release_date)

for ( i in 1:length(movies_data$release_date)){
  movies_data$release_date[i]<-substr(movies_data$release_date[i],6,7)
}


movies_data$release_date<-as.factor(movies_data$release_date)


winter1<-which(movies_data$release_date==c('12'))
winter2<-which(movies_data$release_date==c('01'))
winter3<-which(movies_data$release_date==c('02'))
winter<-c(winter1,winter2,winter3)

spring1<-which(movies_data$release_date==c('03'))
spring2<-which(movies_data$release_date==c('04'))
spring3<-which(movies_data$release_date==c('05'))
spring<-c(spring1,spring2,spring3)

summer1<-which(movies_data$release_date==c('06'))
summer2<-which(movies_data$release_date==c('07'))
summer3<-which(movies_data$release_date==c('08'))
summer<-c(summer1,summer2,summer3)

fall1<-which(movies_data$release_date==c('09'))
fall2<-which(movies_data$release_date==c('10'))
fall3<-which(movies_data$release_date==c('11'))
fall<-c(fall1,fall2,fall3)

length(fall)+length(summer)+length(winter)+length(spring)

movies_data$release_date<-as.character(movies_data$release_date)

movies_data$release_date[fall]<-'fall'
movies_data$release_date[spring]<-'spring'
movies_data$release_date[summer]<-'summer'
movies_data$release_date[winter]<-'winter'

movies_data$release_date<-as.factor(movies_data$release_date)
str(movies_data$release_date)


movies_data$LDA<-as.factor(movies_data$LDA)
movies_data$original_language<-NULL
movies_data$keywords<-NULL
movies_data$title<-NULL






############ Regression #############


fit<- lm(y ~ genres + popularity + production_countries +
          release_date + runtime + spoken_languages + vote_average + vote_count
         + LDA,data=movies_data)

summary(fit)
par(mfrow=c(2,2)); plot(fit)

# remove high leverage point
movies_data<-movies_data[- c(612, 1132, 1632, 1768, 2929, 2937, 3011) ,]

fit<- lm(y ~ genres + original_language + popularity + production_countries +
           release_date + runtime + spoken_languages + vote_average + vote_count
         + LDA,data=movies_data)

summary(fit)
par(mfrow=c(2,2)); plot(fit)
vif(fit)

#train/test data
set.seed(1234)
i<-createDataPartition(y=movies_data$y,p=0.7,list = F)
traindata<-movies_data[i,]
testdata<-movies_data[-i,]

nrow(traindata)
nrow(testdata)
head(traindata)


fit.train<- lm(y ~ genres + original_language + popularity + production_countries +
           release_date + runtime + spoken_languages + vote_average + vote_count
         + LDA,data=traindata)


pre<-predict(fit.train,testdata)

mse<-mean((testdata$y-pre)^2) 
rmse<-sqrt(mse) 
rmse

Regression_RMSE<-sqrt(mean((exp(testdata$y) - exp(pre))^2)) 
Regression_RMSE



# subset selection
null=lm(y~1,data=movies_data)
full=lm(y ~ genres + original_language + popularity + production_countries +
          release_date + runtime + spoken_languages + vote_average + vote_count
        + LDA,data=movies_data)
step(full, scope=list(lower=null, upper=full), direction = "both")


fit2<-lm(formula = y ~ genres + popularity + production_countries + 
           release_date + runtime + vote_average + vote_count, data = movies_data)


summary(fit2)

shapiro.test(fit2$residuals)


########## GAM ############

fit_gam<- gam(y ~ genres  + s(popularity) + s(production_countries) +
           release_date + runtime + spoken_languages + s(vote_average) + s(vote_count)
         + LDA,data=movies_data)

summary(fit_gam)
par(mfrow=c(3,3))
plot(fit_gam)


yhat.gam1<- predict(fit_gam,newdata=testdata)
GAM_RMSE<-sqrt(mean((yhat.gam1-testdata$y)^2))
GAM_RMSE


######################################################################
######### production_countries  Text mining ##########################
 
movies_data$production_countries<-as.character(movies_data$production_countries)

pc<-matrix(NA,length(movies_data$production_countries),12)
for ( i in 1:length(movies_data$production_countries)){
  k<-unlist(strsplit(movies_data$production_countries[i],','))
  m<-length(unlist(strsplit(movies_data$production_countries[i],',')))
  idx<-c(1:m)%%2==1
  k<-k[idx]
  pci<-c(1:length(k))
  for(j in pci){
    pc[i,j]<-k[j]
  }
}

for (i in 1:length(movies_data$production_countries)){
  for (j in 1:12){
    pc[i,j]<-substr(pc[i,j],18,nchar(pc[i,j])-1)
    
  }
  
}



real_pc<-matrix(NA,length(movies_data$production_countries),1)

for ( i in 1:length(movies_data$production_countries)){
  for(j in 1:12){
    if(is.na(pc[i,j])==1  ) pc[i,j]<-''
  }}

for ( i in 1:length(movies_data$production_countries)){
  real_pc[i,1]<-paste(pc[i,1],pc[i,2],pc[i,3],pc[i,4],pc[i,5],pc[i,6],pc[i,7],
                      pc[i,8],pc[i,9],pc[i,10],pc[i,11],pc[i,12])
}



rp<-Corpus(VectorSource(real_pc))

pc_2 <- DocumentTermMatrix(rp,control=list(wordLength=c(2,2) ))
pc_2 ###시발 

inspect(pc_2)

Freq<-colSums(as.matrix(pc_2))

Order_Freq <- order(Freq, decreasing = TRUE)
Freq_term[Order_Freq]

### phase 2 ( Clustering )  안씀!!!!!!!!!!!!#######




rr<-Corpus(VectorSource(real))

genre_3 <- DocumentTermMatrix(rr, control=list(wordLength=c(3,3) ))  
genre_3



Freq<-colSums(as.matrix(genre_3))

Order_Freq <- order(Freq, decreasing = TRUE)
Freq[Order_Freq]






findAssocs(genre_3, "adv", .2)

genre_cluster <-as.matrix(genre_3)


dist<- dist(t(genre_cluster), method="euclidean")
dist

genre_hclust <- hclust(dist, method = "ward.D")
plot(genre_hclust)

# 'k'개 군집으로 나눔
rect.hclust(genre_hclust, k=5, border="red")
genre_hclust_final <- cutree(genre_hclust, k=5) 
genre_hclust_final

sort(genre_hclust_final)



cluster1<-genre_cluster[,c('act','adv')]
head(cluster1)
C1_Sum <- rowSums(cluster1)

################### ################################
cred<-read.csv('tmdb_5000_credits.csv',stringsAsFactors = F)
cast<-as.character(cred$cast)
idx<-which(nchar(cast)<=2)
cred<-cred[-idx,]
js<-lapply(cast,fromJSON)

creditdf=cred %>% filter(nchar(cast)>2) %>% mutate(js=lapply(cast,fromJSON)) %>% unnest(js)
creditdf <- creditdf[creditdf$order<3,]
r<-data.frame(creditdf$title,creditdf$name,creditdf$order)

cast<-creditdf %>% group_by(movie_id) %>% summarise(name = toString(name))












