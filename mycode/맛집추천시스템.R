library(tidyverse)
library(Matrix)
library(recommenderlab)
library(data.table)

# DATA ------------------------------------------------------------------------------------------------------------------------

# CF data Carpentry
cf_data <- fread("cf_data.csv") # nrow = 144544, ID = 8505, Restaurant = 17134

cf_data <- cf_data[complete.cases(cf_data), ] #104448, ID = 6718, Restaurant = 15849
cf_data <-  cf_data %>% group_by(ID,Restaurant) %>% summarize(Rating=max(Rating)) %>% ungroup #104042, ID = 6718, Restaurant = 15849
cf_data <- as.data.frame(cf_data)

# CB data Carpentry
cb_data <- fread("cb_data.csv") # Restaurant = 29352

cb_data <- cb_data %>% group_by(Name) %>% dplyr::slice(which.max(Rating)) # Step1) 중복된 식당 중 평점이 높은 식당만 남김 (27065개)

cb_data <- cb_data %>% filter(Rating != 0) # Step2) 

cb_data <- cb_data %>% filter( nchar(About) >= 100 ) # Step3) About이 100자 이상인 애들 

left_over_Restaurant <- cb_data$Name # 남은 식당의 이름

cf_data_copy <- cf_data[cf_data$Restaurant %in% left_over_Restaurant, ] # CB에서 남은 식당만으로 CF 데이터 구축  

Res_list_for_cb <- cf_data_copy %>% group_by(Restaurant) %>% summarize(count=n()) %>% filter(count > 5) %>% select(Restaurant) # 6번 이상 리뷰를 받은 식당

cf_data_copy2 <- cf_data_copy[cf_data_copy$Restaurant %in% Res_list_for_cb$Restaurant, ]

ID_list_for_cb_eval <- cf_data_copy2 %>% group_by(ID) %>% summarize(count=n()) %>% filter(count > 6) # 6번 이상 리뷰한 애들
Res_list_based_on_ID <- cf_data_copy2[cf_data_copy2$ID %in% ID_list_for_cb_eval$ID, ] %>% select(Restaurant) %>% unique # 6번 이상 리뷰한 애들이 간 식당

cf_data_final <- cf_data_copy2[cf_data_copy2$ID %in% ID_list_for_cb_eval$ID, ]

cb_data <- cb_data[cb_data$Name %in% Res_list_based_on_ID$Restaurant, ] # 6번 이상 리뷰한 애들이 간 식당만 남김

#final data
r <- as(cf_data_final,  'realRatingMatrix')/ 
r <- r[rowCounts(r) > 5]
dim(r)

#making model(split version)

set.seed(9)
traintestset <- evaluationScheme(r, method='split',train=0.8, given=6, goodRating=4, k=3) 
IDlist_CF <- getData(traintestset , "known")@data@Dimnames[[1]]

#UBCF version ------------------------------------------------------------------------------------------------------------------------------------
UBCF <- Recommender(getData(traintestset , "train"), method="UBCF", parameter="Cosine")

UBCF_pred_rating <- predict(UBCF, getData(traintestset , "known"), type="ratings")
UBCF_pred_topNList <- predict(UBCF, getData(traintestset , "known"), type="topNList")

UBCF_E_rating <- calcPredictionAccuracy(UBCF_pred_rating, getData(traintestset , "unknown"), byUser=FALSE)
UBCF_E_topNList <- calcPredictionAccuracy(UBCF_pred_topNList, getData(traintestset , "unknown"), byUser=FALSE, goodRating=4, given=6)

#IBCF version------------------------------------------------------------------------------------------------------------------------------------
IBCF <- Recommender(getData(traintestset , "train"), method="IBCF", parameter="Cosine")

IBCF_pred_rating <- predict(IBCF, getData(traintestset , "known"), type="ratings")
IBCF_pred_topNList <- predict(IBCF, getData(traintestset , "known"), type="topNList")

IBCF_E_rating <- calcPredictionAccuracy(IBCF_pred_rating, getData(traintestset , "unknown"))
IBCF_E_topNList <- calcPredictionAccuracy(IBCF_pred_topNList, getData(traintestset , "unknown"), byUser=FALSE, goodRating=4, given=6)

#IBCF + UBCF------------------------------------------------------------------------------------------------------------------------------------
HCF <- HybridRecommender(UBCF, IBCF)

HCF_pred_topNList <- predict(HCF, getData(traintestset , "known"), type="topNList")

UBCF_rating_for_HCF <- as.matrix(UBCF_pred_rating@data)
IBCF_rating_for_HCF <- as.matrix(IBCF_pred_rating@data)

UBCF_rating_for_HCF[UBCF_rating_for_HCF==0] <- NA
IBCF_rating_for_HCF[IBCF_rating_for_HCF==0] <- NA

HCF_pred_rating <- (UBCF_rating_for_HCF + IBCF_rating_for_HCF) / 2

HCF_E_rating <- calcPredictionAccuracy(as(HCF_pred_rating,"realRatingMatrix"), getData(traintestset, "unknown"))
HCF_E_topNList <- calcPredictionAccuracy(HCF_pred_topNList, getData(traintestset , "unknown"), byUser=FALSE, goodRating=4, given=6)

# CB ---------------------------------------------------------------------------------------------------------------------------------------

cb_data_after_carpentry <- fread("cb_data_after_carpentry.csv")

dist_mat<-fread("dist_mat_full.csv")
dist_mat_full<-as.matrix(dist_mat)

user_n <- length(unique(cf_data_final$ID))
selected_user <- unique(cf_data_final$ID)

TN= length(IDlist_CF)

test_user <- IDlist_CF

idx_real_set<-vector(TN,mode = "list")
idx_given_set<-vector(TN,mode = "list")

idx_eval_set_hat<-vector(TN,mode = "list")  
idx_eval_set_cf<-vector(TN,mode = "list")  # index for caculating error 

eval_frame<-as.data.frame(matrix(NA,TN,2658))  # our final Eval frame! TN x Restaurant matrix

colnames(eval_frame) <-  cb_data_after_carpentry$Name
rownames(eval_frame) <- IDlist_CF

# Let's Start!!!!!!!!

for (i in 1:TN){
  idx_real <- which(cf_data_final$ID == test_user[i])
  
  if ( length(idx_real) != 6){idx_given<-sample(idx_real,6)}  # select 6 given 
  
  idx_eval<-idx_real[which(idx_real %in% idx_given == F)]
  
  
  idx_real_set[[i]] <- which( cb_data_after_carpentry$Name %in% cf_data_final[idx_real,]$Restaurant)
  idx_given_set[[i]] <- which( cb_data_after_carpentry$Name %in% cf_data_final[idx_given,]$Restaurant)
  idx_eval_set_hat[[i]] <- which( cb_data_after_carpentry$Name %in% cf_data_final[idx_eval,]$Restaurant)
  idx_eval_set_cf[[i]] <- idx_eval
  
  Res <- cf_data_final$Restaurant[idx_given]  #  6 given's Res
  Rating <- cf_data_final$Rating[idx_given]   # 6 given's Rating
  
  Res_idx_mat <- matrix(NA,length(Res),2658)
  
  for ( j in 1:length(Res)){
    Res_idx <- which( cb_data$Name == Res[j] )
    Res_idx_mat[j,] <- dist_mat_full[Res_idx,]   # Our Similarity Matrix!!!!!!!!!
    #print(j)
  }
  
  sum_set <- apply(Res_idx_mat,2,sum)
  
  sum_set[which(sum_set==0)] <- 0.00001 # simil sum = 0 case. In this case, sum of expected rating is 0
  
  expected_rating <- (Rating %*% Res_idx_mat)    # matrix multiplication
  
  for( k in 1:length(sum_set)){
    expected_rating[k] <- expected_rating[k]/sum_set[k]
  }
  
  eval_frame[i,] <- expected_rating
  print(i)
}

eval_frame <- as.data.frame(eval_frame)
rownames(eval_frame) <- test_user

CB_E_rating <- calcPredictionAccuracy(as(as.matrix(eval_frame),"realRatingMatrix"), getData(traintestset, "unknown"))

cf_data_cm <- cf_data_final[cf_data_final$ID %in% test_user , ] # leave only test set
cf_data_cm_spread <- cf_data_cm %>% spread(Restaurant, Rating) # spread matrix
id <- cf_data_cm_spread[,1]
cf_data_cm_spread <- cf_data_cm_spread %>% select(-ID)
rownames(cf_data_cm_spread) <- id

cf_data_cm_spread[cf_data_cm_spread <= 3] <- 0
cf_data_cm_spread[cf_data_cm_spread > 3] <- 1
cf_data_cm_spread[is.na(cf_data_cm_spread)] <- 0

eval_frame_copy <- eval_frame

for (i in 1:nrow(eval_frame_copy)) {
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") < 2629] <- 0
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") >= 2629] <- 1
}

eval_frame_final <- eval_frame_copy[,colnames(eval_frame_copy) %in% colnames(cf_data_cm_spread)]

identical(sort(colnames(eval_frame_final)), sort(colnames(cf_data_cm_spread)))

TP <- rowSums(as.matrix(eval_frame_final) * as.matrix(cf_data_cm_spread))
TP_FN <- rowSums(as.matrix(cf_data_cm_spread))
TP_FP <- rowSums(as.matrix(eval_frame_final))
FP <- TP_FP - TP
FN <- TP_FN - TP
TN <-  ncol(cf_data_cm_spread) - 6 - TP - FP - FN

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

TPR <- recall
FPR <- FP / (FP + TN)

CB_E_topNList <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)

CB_E_topNList <- colMeans(CB_E_topNList, na.rm=TRUE)

CB_E_topNList

# CB+UBCF------------------------------------------------------------------------------------------------------------------------------------

CBUBCF_pred <- (eval_frame + UBCF_rating_for_HCF) / 2

CBUBCF_E_rating <- calcPredictionAccuracy(as(as.matrix(CBUBCF_pred),"realRatingMatrix"), getData(traintestset, "unknown"))

cf_data_cm <- cf_data_final[cf_data_final$ID %in% test_user , ] # leave only test set
cf_data_cm_spread <- cf_data_cm %>% spread(Restaurant, Rating) # spread matrix
id <- cf_data_cm_spread[,1]
cf_data_cm_spread <- cf_data_cm_spread %>% select(-ID)
rownames(cf_data_cm_spread) <- id

cf_data_cm_spread[cf_data_cm_spread <= 3] <- 0
cf_data_cm_spread[cf_data_cm_spread > 3] <- 1
cf_data_cm_spread[is.na(cf_data_cm_spread)] <- 0

eval_frame_copy <- (eval_frame + as.matrix(UBCF_pred_rating@data)) / 2

for (i in 1:nrow(eval_frame_copy)) {
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") < 2629] <- 0
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") >= 2629] <- 1
}

eval_frame_final <- eval_frame_copy[,colnames(eval_frame_copy) %in% colnames(cf_data_cm_spread)]

identical(sort(colnames(eval_frame_final)), sort(colnames(cf_data_cm_spread)))

TP <- rowSums(as.matrix(eval_frame_final) * as.matrix(cf_data_cm_spread))
TP_FN <- rowSums(as.matrix(cf_data_cm_spread))
TP_FP <- rowSums(as.matrix(eval_frame_final))
FP <- TP_FP - TP
FN <- TP_FN - TP
TN <-  ncol(cf_data_cm_spread) - 6 - TP - FP - FN

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

TPR <- recall
FPR <- FP / (FP + TN)

CBUBCF_E_topNList <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)

CBUBCF_E_topNList <- colMeans(CBUBCF_E_topNList, na.rm=TRUE)

CBUBCF_E_topNList

# CB+IBCF------------------------------------------------------------------------------------------------------------------------------------

IDlist_CF <- getData(traintestset , "known")@data@Dimnames[[1]]
CBIBCF_pred <- (eval_frame + IBCF_rating_for_HCF) / 2

CBIBCF_E_rating <- calcPredictionAccuracy(as(as.matrix(CBIBCF_pred), "realRatingMatrix"), getData(traintestset, "unknown"))

cf_data_cm <- cf_data_final[cf_data_final$ID %in% test_user , ] # leave only test set
cf_data_cm_spread <- cf_data_cm %>% spread(Restaurant, Rating) # spread matrix
id <- cf_data_cm_spread[,1]
cf_data_cm_spread <- cf_data_cm_spread %>% select(-ID)
rownames(cf_data_cm_spread) <- id

cf_data_cm_spread[cf_data_cm_spread <= 3] <- 0
cf_data_cm_spread[cf_data_cm_spread > 3] <- 1
cf_data_cm_spread[is.na(cf_data_cm_spread)] <- 0

eval_frame_copy <- (eval_frame + as.matrix(IBCF_pred_rating@data)) / 2

for (i in 1:nrow(eval_frame_copy)) {
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") < 2629] <- 0
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") >= 2629] <- 1
}

eval_frame_final <- eval_frame_copy[,colnames(eval_frame_copy) %in% colnames(cf_data_cm_spread)]

identical(sort(colnames(eval_frame_final)), sort(colnames(cf_data_cm_spread)))

TP <- rowSums(as.matrix(eval_frame_final) * as.matrix(cf_data_cm_spread))
TP_FN <- rowSums(as.matrix(cf_data_cm_spread))
TP_FP <- rowSums(as.matrix(eval_frame_final))
FP <- TP_FP - TP
FN <- TP_FN - TP
TN <-  ncol(cf_data_cm_spread) - 6 - TP - FP - FN

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

TPR <- recall
FPR <- FP / (FP + TN)

CBIBCF_E_topNList <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)

CBIBCF_E_topNList <- colMeans(CBIBCF_E_topNList, na.rm=TRUE)

CBIBCF_E_topNList

# IBCF + UBCF + CB------------------------------------------------------------------------------------------------------------------------------------

CBUIBCF_pred <- (eval_frame + UBCF_rating_for_HCF + IBCF_rating_for_HCF) / 3

CBUIBCF_E_rating <- calcPredictionAccuracy(as(as.matrix(CBUIBCF_pred),"realRatingMatrix"), getData(traintestset, "unknown"))

cf_data_cm <- cf_data_final[cf_data_final$ID %in% test_user , ] # leave only test set
cf_data_cm_spread <- cf_data_cm %>% spread(Restaurant, Rating) # spread matrix
id <- cf_data_cm_spread[,1]
cf_data_cm_spread <- cf_data_cm_spread %>% select(-ID)
rownames(cf_data_cm_spread) <- id

cf_data_cm_spread[cf_data_cm_spread <= 3] <- 0
cf_data_cm_spread[cf_data_cm_spread > 3] <- 1
cf_data_cm_spread[is.na(cf_data_cm_spread)] <- 0

eval_frame_copy <- CBUIBCF_pred

for (i in 1:nrow(eval_frame_copy)) {
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") < 2629] <- 0
  eval_frame_copy[i, rank(eval_frame_copy[i,], ties.method="first") >= 2629] <- 1
}

eval_frame_final <- eval_frame_copy[,colnames(eval_frame_copy) %in% colnames(cf_data_cm_spread)]

TP <- rowSums(as.matrix(eval_frame_final) * as.matrix(cf_data_cm_spread))
TP_FN <- rowSums(as.matrix(cf_data_cm_spread))
TP_FP <- rowSums(as.matrix(eval_frame_final))
FP <- TP_FP - TP
FN <- TP_FN - TP
TN <-  ncol(cf_data_cm_spread) - 6 - TP - FP - FN

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

TPR <- recall
FPR <- FP / (FP + TN)

CBUIBCF_E_topNList <- cbind(TP, FP, FN, TN, precision, recall, TPR, FPR)

CBUIBCF_E_topNList <- colMeans(CBUIBCF_E_topNList, na.rm=TRUE)

CBUIBCF_E_topNList

# Summarize Results ----------------------------------------------------------------------------------------------------------------------------

df <- data.frame(MAE = c(UBCF_E_rating[3], IBCF_E_rating[3], CB_E_rating[3], HCF_E_rating[3], CBUBCF_E_rating[3], CBIBCF_E_rating[3], CBUIBCF_E_rating[3]),
                 Precision = c(UBCF_E_topNList[5], IBCF_E_topNList[5], CB_E_topNList[5], HCF_E_topNList[5],
                               CBUBCF_E_topNList[5], CBIBCF_E_topNList[5], CBUIBCF_E_topNList[5]),
                 Recall = c(UBCF_E_topNList[6], IBCF_E_topNList[6], CB_E_topNList[6], HCF_E_topNList[6],
                            CBUBCF_E_topNList[6], CBIBCF_E_topNList[6], CBUIBCF_E_topNList[6]))

rownames(df) <- c("UBCF", "IBCF", "CB", "UBCF+IBCF", "UBCF+CB", "IBCF+CB", "UBCF+IBCF+CB")

df