library(xgboost)
library(dummies)
library(caret)
library(e1071)
library(Ckmeans.1d.dp)
library(dplyr)

PDCD<-read.csv('PDCD.csv')
dat<-PDCD

dat$V1_EDUA<-as.factor(dat$V1_EDUA)
dat$V1_INCOME<-as.factor(dat$V1_INCOME)

## NA Imputation ##

# V1_EDUA #

na_edue<-which(is.na(dat$V1_EDUA)==T)
na_income<-which(is.na(dat$V1_INCOME)==T)


intersect(na_edue,na_income)
# interger (0)

table(dat$V1_INCOME)

for ( i in c(1:nlevels(dat$V1_INCOME))){
  id<-which(dat$V1_INCOME==i & (is.na(dat$V1_EDUA)==F))
  t_dat<-dat[id,]
  idx<-which(dat$V1_INCOME==i & is.na(dat$V1_EDUA)==T)
  for ( j in idx ){
    value<-sample(t_dat$V1_EDUA,1)
    dat$V1_EDUA[j]<-value
  }
  
}


datt<-dat

# V1_PHYACTL , V1_PHYACTM , V1_PHYACTH #
which(is.na(dat$V1_AGE))
#integer(0)
dat$V1_PHYACTL<-as.factor(dat$V1_PHYACTL)
dat$V1_PHYACTM<-as.factor(dat$V1_PHYACTM)
dat$V1_PHYACTH<-as.factor(dat$V1_PHYACTH)


na_L<-which(is.na(dat$V1_PHYACTL)==T)
na_M<-which(is.na(dat$V1_PHYACTM)==T)
na_H<-which(is.na(dat$V1_PHYACTH)==T)


cl<-kmeans(dat$V1_AGE,3)
dat$agecluster<-cl$cluster
dat$agecluster<-as.factor(dat$agecluster)



for ( i in c(1:nlevels(dat$agecluster))){
  id<-which(dat$agecluster==i & (is.na(dat$V1_PHYACTL)==F))
  t_dat<-dat[id,]
  idx<-which(dat$agecluster==i & (is.na(dat$V1_PHYACTL)==T))
  for ( j in idx ){
    value<-sample(t_dat$V1_PHYACTL,1)
    dat$V1_PHYACTL[j]<-value
  }
  
}

for ( i in c(1:nlevels(dat$agecluster))){
  id<-which(dat$agecluster==i & (is.na(dat$V1_PHYACTM)==F))
  t_dat<-dat[id,]
  idx<-which(dat$agecluster==i & (is.na(dat$V1_PHYACTM)==T))
  for ( j in idx ){
    value<-sample(t_dat$V1_PHYACTM,1)
    dat$V1_PHYACTM[j]<-value
  }
  
}


for ( i in c(1:nlevels(dat$agecluster))){
  id<-which(dat$agecluster==i & (is.na(dat$V1_PHYACTH)==F))
  t_dat<-dat[id,]
  idx<-which(dat$agecluster==i & is.na(dat$V1_PHYACTH)==T)
  for ( j in idx ){
    value<-sample(t_dat$V1_PHYACTH,1)
    dat$V1_PHYACTH[j]<-value
  }
  
}

dat$agecluster<-NULL
# write.csv(dat,'PDCD_complete.csv',row.names = F)


PDCD_complete<-read.csv('PDCD_complete.csv')
dat2<-PDCD_complete
group<-kmeans(dat2[,c(3,79,80)],10)
table(group$cluster)
dat2$group<-group$cluster
dat2$group<-as.factor(dat2$group)



for ( p in c(85:105)){
  for ( i in c(1:nlevels(dat2$group))){
    idg<-which(dat2$group==i)
    dat2[idg,p]<-mean(dat2[idg,p] ,na.rm=T)
  }
}

#write.csv(dat2,'PDCD_final.csv',row.names = F)

#######3 mice ##############

dat<-PDCD
colnames(dat)
str(dat[,c(79:105)])

for (i in c(1,2,4:54,62,64:69,107)){
  dat[,i] = as.factor(dat[,i])
}

str(dat)

iid<-which(colSums(is.na(dat))==0)
names(iid)


dat2<-dat[,c(iid)]
dat2$BAID<-NULL
dat2$fuPDCD<-NULL
dat2$V1_PDCD_HIST<-NULL

colnames(dat2)<-substr(colnames(dat2),4,nchar(colnames(dat2)))

dat3<-cbind(dat2,dat[,c(79:105)])
# AGE #
dat3$AGE[which(dat3$AGE<50)]<-1
dat3$AGE[which(dat3$AGE>=50 & dat3$AGE<60)]<-2
dat3$AGE[which(dat3$AGE>=60)]<-3
dat3$AGE<-as.factor(dat3$AGE)



impp<-mice(dat3,maxit =  20)

plot(impp)


impp2<-mice(dat3,method = 'norm.predict',maxit = 25)
plot(impp2)

datt<-dat3

for ( i in c(84:104)){
  pr<-apply(as.data.frame(impp$imp[i]),1,mean)
  rg<-apply(as.data.frame(impp2$imp[i]),1,mean)
  datt[which(is.na(datt[i])),i]<-apply(cbind(pr,rg),1,mean)
}

for ( i in c(84:104)){
  pr<-apply(as.data.frame(impp$imp[i]),1,mean)
  rg<-apply(as.data.frame(impp2$imp[i]),1,mean)
  print(cor(pr,rg))
}



dat[,c(85:105)]<-datt[,c(84:104)]








fit1<-with(data=imp,expr=lm(chl~age))

fit2<-with(data=imp,expr=lm(chl~age+bmi))




stat <- pool.compare(fit2,fit1,method = 'wald')










PDCD_final<-read.csv('PDCD_final.csv')

colnames(PDCD_final[,c(85:105)])
colnames(dat[,c(85:105)])

hist(PDCD_final[,c(105)])

PDCD_final[,c(85:105)]<-dat[,c(85:105)]


PDCD_real_final<-PDCD_final

colSums(is.na(PDCD_real_final))
hist(PDCD_real_final$V1_FVC)

# write.csv(PDCD_real_final,'PDCD_real_final.csv',row.names = F)













#####################


#########unhealthy###########
unhealthy = function(data){
  common = c(1,4,5,6,7,52,53,54,79,80,85:105)
  data = data[data$fuPDCD!=1,]
  data$V1_time =0
  colnames(data)[490:494] = c("V2_time","V3_time","V4_time","V5_time","V6_time")
  
  normal = data.frame()
  normal = rbind(normal, data[1,][c(common,108:183,3,55:61,63,70:78,81:84, 490)])
  normal$V1_time = 0
  
  bad = data.frame()
  bad = rbind(bad, data[1,][c(common,108:183,3,55:61,63,70:78,81:84, 490)])
  bad$V1_time = 0
  
  
  for (position in c(32:107,130)){
    word1 = colnames(normal)[position]
    word2 = colnames(bad)[position]
    colnames(normal)[position] = substr(word1,4,nchar(word1))
    colnames(bad)[position] = substr(word2,4,nchar(word2))
  }
  for (position in c(108:129,131)){
    word1 = colnames(normal)[position]
    word2 = colnames(bad)[position]
    colnames(normal)[position] = paste0(substr(word1,4,nchar(word1)),"before")
    colnames(bad)[position] = paste0(substr(word2,4,nchar(word2)),"before")
  }
  
  rule = c(12,114,190,266,342,418)
  for (i in 1:nrow(data)){
    count = 0
    pos = 0
    for (j in 1:6){
      
      if (!is.na(data[i,][rule[j]]) & data[i,][rule[j]] == 1){
        if(j==1){
          sample = NULL
          count = count + 1
        }
        else if(j==2){
          sample = data[i,][c(common,108:183, 490)]
          count = count + 1
        }
        else if(j==3){
          sample = data[i,][c(common,184:259, 491)]
          count = count + 1
        }
        else if(j==4){
          sample = data[i,][c(common,260:335, 492)]
          count = count + 1
        }
        else if(j==5){
          sample = data[i,][c(common,336:411, 493)]
          count = count + 1
        }
        else if(j==6){
          sample = data[i,][c(common,412:487, 494)]
          count = count + 1
        }
        
        
        if(count != 1){
          if(pos == 1){
            sample = c(sample, data[i,][c(3,55:61,63,70:78,81:84, 495)])
          }
          else if(pos ==2){
            sample = c(sample, data[i,][c(109,154:167,169,176:181, 490)])
          }
          else if(pos ==3){
            sample = c(sample, data[i,][c(185,230:243,245,252:257, 491)])
          }
          else if(pos ==4){
            sample = c(sample, data[i,][c(261,306:316,318,324:328,330:333, 492)])
          }
          else if(pos ==5){
            sample = c(sample, data[i,][c(337,382:396,402,404,406:409, 493)])
          }
          
          
          for (position in 32:108){
            word = names(sample)[position]
            names(sample)[position] = substr(word,4,nchar(word))
          }
          for (position in 109:131){
            word = names(sample)[position]
            names(sample)[position] = paste0(substr(word,4,nchar(word)),"before")
          }
          normal = rbind(normal,sample)
        }
        pos = j
        
      }
      
      else if(!is.na(data[i,][rule[j]]) & data[i,][rule[j]] == 2){
        if(j==1){
          sample = NULL
          count = count + 1
          
        }
        else if(j==2){
          sample = data[i,][c(common,108:183, 490)]
          count = count + 1
        }
        else if(j==3){
          sample = data[i,][c(common,184:259, 491)]
          count = count + 1
        }
        else if(j==4){
          sample = data[i,][c(common,260:335, 492)]
          count = count + 1
        }
        else if(j==5){
          sample = data[i,][c(common,336:411, 493)]
          count = count + 1
        }
        else if(j==6){
          sample = data[i,][c(common,412:487, 494)]
          count = count + 1
        }
        
        if(count !=1){
          
          if(pos ==1){
            sample = c(sample, data[i,][c(3,55:61,63,70:78,81:84, 495)])
          }
          
          else if(pos ==2){
            sample = c(sample, data[i,][c(109,154:167,169,176:181, 490)])
          }
          else if(pos ==3){
            sample = c(sample, data[i,][c(185,230:243,245,252:257, 491)])
          }
          else if(pos ==4){
            sample = c(sample, data[i,][c(261,306:316,318,324:328,330:333, 492)])
          }
          else if(pos ==5){
            sample = c(sample, data[i,][c(337,382:396,402,404,406:409, 493)])
          }
          
          for (position in 32:108){
            word = names(sample)[position]
            names(sample)[position] = substr(word,4,nchar(word))
          }
          
          for(position in 109:131){
            word = names(sample)[position]
            names(sample)[position] = paste0(substr(word,4,nchar(word)),"before")
          }
          
          bad = rbind(bad, sample)
          break
        }
        pos = j
      }
    }
  }
  normal = normal[-1,]
  bad = bad[-1,]
  return (normal)
}

PDCD <- read.csv("PDCD_real_final.csv")
dat<-PDCD
dat$group<-NULL
dat=dat[!is.na(dat$V1_MARRYA),]
dat=dat[!is.na(dat$V1_JOBB),]
dat=dat[!is.na(dat$V1_INCOME),]
colnames(dat)
colSums(is.na(dat))
normal = unhealthy(dat)
bad = unhealthy(dat)

##############################

normal$label<-1
bad$label<-2
normal$BAID<-as.character(normal$BAID)
bad$BAID<-as.character(bad$BAID)


#########healthy###########
colSums(is.na(dat))
healthy = function(data){
  common = c(1,4,5,6,7,52,53,54,79,80,85:105)
  data = data[data$fuPDCD!=2,]
  data$V1_time =0
  colnames(data)[490:494] = c("V2_time","V3_time","V4_time","V5_time","V6_time")
  
  normal = data.frame()
  normal = rbind(normal, data[1,][c(common,108:183,3,55:61,63,70:78,81:84, 490)])
  normal$V1_time = 0
  
  for (position in c(32:107,130)){
    word1 = colnames(normal)[position]
    colnames(normal)[position] = substr(word1,4,nchar(word1))
  }
  for (position in c(108:129,131)){
    word1 = colnames(normal)[position]
    colnames(normal)[position] = paste0(substr(word1,4,nchar(word1)),"before")
  }
  
  rule = c(12,114,190,266,342,418)
  for (i in 1:nrow(data)){
    print(i)
    count = 0
    pos = 0
    for (j in 1:6){
      
      if (!is.na(data[i,][rule[j]]) & data[i,][rule[j]] == 1){
        if(j==1){
          sample = NULL
          count = count + 1
        }
        else if(j==2){
          sample = data[i,][c(common,108:183, 490)]
          count = count + 1
        }
        else if(j==3){
          sample = data[i,][c(common,184:259, 491)]
          count = count + 1
        }
        else if(j==4){
          sample = data[i,][c(common,260:335, 492)]
          count = count + 1
        }
        else if(j==5){
          sample = data[i,][c(common,336:411, 493)]
          count = count + 1
        }
        else if(j==6){
          sample = data[i,][c(common,412:487, 494)]
          count = count + 1
        }
        
        
        if(count != 1){
          if(pos == 1){
            sample = c(sample, data[i,][c(3,55:61,63,70:78,81:84, 495)])
          }
          else if(pos ==2){
            sample = c(sample, data[i,][c(109,154:167,169,176:181, 490)])
          }
          else if(pos ==3){
            sample = c(sample, data[i,][c(185,230:243,245,252:257, 491)])
          }
          else if(pos ==4){
            sample = c(sample, data[i,][c(261,306:316,318,324:328,330:333, 492)])
          }
          else if(pos ==5){
            sample = c(sample, data[i,][c(337,382:396,402,404,406:409, 493)])
          }
          
          
          for (position in 32:108){
            word = names(sample)[position]
            names(sample)[position] = substr(word,4,nchar(word))
          }
          for (position in 109:131){
            word = names(sample)[position]
            names(sample)[position] = paste0(substr(word,4,nchar(word)),"before")
          }
          normal = rbind(normal,sample)
        }
        pos = j
        
      }
    }
  }
  normal = normal[-1,]
  return (normal)
}



person = healthy(dat)
colSums(is.na(person))

write.csv(normal,"normal.csv", row.names = FALSE)
write.csv(bad, "bad.csv", row.names = FALSE)
write.csv(person,"person.csv", row.names = FALSE)

person$label<-0
normal$label<-1
bad$label<-2



## more 

fmhtrel = function(data){
  for(i in 1:nrow(data)){
    if(data$FMHTREL3[i]==1 & data$FMHTREL2[i]==1 & data$FMHTREL1[i]==1){
      data$FMHTREL[i] = 1
    }
    else{
      data$FMHTREL[i] = 2
    }
  }
  
  data$FMHTREL3 = NULL
  data$FMHTREL2 = NULL
  data$FMHTREL1 = NULL
  return(data)
}

fmdmrel = function(data){
  for(i in 1:nrow(data)){
    if(data$FMDMREL3[i]==1 & data$FMDMREL2[i]==1 & data$FMDMREL1[i]==1){
      data$FMDMREL[i] = 1
    }
    else{
      data$FMDMREL[i] = 2
    }
  }
  data$FMDMREL3 = NULL
  data$FMDMREL2 = NULL
  data$FMDMREL1 = NULL
  return(data)
}

fmlprel = function(data){
  for(i in 1:nrow(data)){
    if(data$FMLPREL3[i]==1 & data$FMLPREL2[i]==1 & data$FMLPREL1[i]==1){
      data$FMLPREL[i] = 1
    }
    else{
      data$FMLPREL[i] = 2
    }
  }
  data$FMLPREL3 = NULL
  data$FMLPREL2 = NULL
  data$FMLPREL1 = NULL
  return(data)
}

fmcvarel = function(data){
  for(i in 1:nrow(data)){
    if(data$FMCVAREL3[i]==1 & data$FMCVAREL2[i]==1 & data$FMCVAREL1[i]==1){
      data$FMCVAREL[i] = 1
    }
    else{
      data$FMCVAREL[i] = 2
    }
  }
  data$FMCVAREL3 = NULL
  data$FMCVAREL2 = NULL
  data$FMCVAREL1 = NULL
  return(data)
}

fmcdrel = function(data){
  for(i in 1:nrow(data)){
    if(data$FMCDREL3[i]==1 & data$FMCDREL2[i]==1 & data$FMCDREL1[i]==1){
      data$FMCDREL[i] = 1
    }
    else{
      data$FMCDREL[i] = 2
    }
  }
  data$FMCDREL3 = NULL
  data$FMCDREL2 = NULL
  data$FMCDREL1 = NULL
  return(data)
}

fvc = function(data){
  for(i in 1:nrow(data)){
    data$fvc[i] = data$V1_FVC[i] / data$V1_FEV1[i]
  }
  data$V1_FVC = NULL
  data$V1_FEV1 = NULL
  return(data)
}


#######################

every<-rbind(person,normal,bad)
every$label<-as.factor(every$label)
glimpse(every)
table(every$label)

#write.csv(every,'every.csv',row.names = F)
############## xg

normal<-read.csv('normal.csv')
bad<-read.csv('bad.csv')
person<-read.csv('person.csv')




######### CHOOSE only one 'Normal' per one row ########
for(i in 1:(nrow(normal)-1)){
  if(normal$BAID[i] == normal$BAID[i+1]){
    normal$BAID[i] = NA
  }
}
normal = normal[complete.cases(normal),]


########### Two time function ###########

weight = function(data){
  for ( i in 1:nrow(data)){
    data$V1_WEIGHT[i] = data$BMI[i] * (data$V1_HEIGHT[i]/100)^2
  }  
  return ( data )
}

change = function(data){
  ## change ##
  data$WBCchangerate = data$WBC / data$WBCbefore
  data$RBChangerate = data$RBC / data$RBCbefore
  data$HBchangerate = data$HB / data$HBbefore
  data$HCTchangerate = data$HCT / data$HCTbefore
  data$HBA1Cchangerate = data$HBA1C / data$HBA1Cbefore
  data$PLATchangerate = data$PLAT / data$PLATbefore
  data$UPHchangerate = data$UPH / data$UPHbefore
  data$USGchangerate = data$USG / data$USGbefore
  data$ALTchangerate = data$ALT / data$ALTbefore
  data$ASTchangerate = data$AST / data$ASTbefore
  data$BUNchangerate = data$BUN / data$BUNbefore 
  data$CRchangerate = data$CR / data$CRbefore
  data$GLUchangerate = data$GLU / data$GLUbefore
  data$CHOLchangerate = data$CHOL / data$CHOLbefore
  data$HDLchangerate = data$HDL / data$HDLbefore
  data$TGchangerate = data$TG / data$TGbefore
  data$LDLchangerate = data$LDL / data$LDLbefore 
  data$SBPchangerate = data$SBP / data$SBPbefore
  data$DBPchangerate = data$DBP / data$DBPbefore 
  data$WAISTchangerate = data$WAIST / data$WAISTbefore
  data$BMIchangerate = data$BMI / data$BMIbefore
  
  #########
  data$BPchangerate = (data$SBP - data$DBP)/(data$SBPbefore - data$DBPbefore)
  
  for(i in 1:nrow(data)){
    ## standard deviation ##
    
    ###### change rate #####
    month = data$time[i] - data$timebefore[i]
    
    data$WBCchange_time[i] = (data$WBC[i] - data$WBCbefore[i])/month
    data$RBCchange_time[i] = (data$RBC[i] - data$RBCbefore[i])/month
    data$HBchange_time[i] = (data$HB[i] - data$HBbefore[i])/month
    data$HCTchange_time[i] = (data$HCT[i] - data$HCTbefore[i])/month
    data$HBA1Cchange_time[i] = (data$HBA1C[i] - data$HBA1Cbefore[i])/month
    data$PLATchange_time[i] = (data$PLAT[i] - data$PLATbefore[i])/month 
    data$UPHchange_time[i] = (data$UPH[i] - data$UPHbefore[i])/month
    data$USGchange_time[i] = (data$USG[i] - data$USGbefore[i])/month
    data$ALTchange_time[i] = (data$ALT[i] - data$ALTbefore[i])/month
    data$ASTchange_time[i] = (data$AST[i] - data$ASTbefore[i])/month
    data$BUNchange_time[i] = (data$BUN[i] - data$BUNbefore[i])/month
    data$CRchange_time[i] = (data$CR[i] - data$CRbefore[i])/month
    data$GLUchange_time[i] = (data$GLU[i] - data$GLUbefore[i])/month
    data$CHOLchange_time[i] = (data$CHOL[i] - data$CHOLbefore[i])/month
    data$HDLchange_time[i] = (data$HDL[i] - data$HDLbefore[i])/month
    data$TGchange_time[i] = (data$TG[i] - data$TGbefore[i])/month
    data$LDLchange_time[i] = (data$LDL[i] - data$LDLbefore[i]) /month
    data$SBPchange_time[i] = (data$SBP[i] - data$SBPbefore[i])/month
    data$DBPchange_time[i] = (data$DBP[i] - data$DBPbefore[i]) /month
    data$WAISTchange_time[i] = (data$WAIST[i] - data$WAISTbefore[i])/month 
    data$BMIchange_time[i] = (data$BMI[i] - data$BMIbefore[i])/month
    
    #########
    data$BPchange_time[i] = (data$SBP[i] - data$DBP[i])/month
    
  }
  return(data)
}

delete = function(data){
  data$AGEbefore = NULL
  data$WBCbefore = NULL
  data$RBCbefore = NULL
  data$HBbefore = NULL
  data$HCTbefore = NULL
  data$HBA1Cbefore = NULL
  data$PLATbefore = NULL
  data$UPHbefore = NULL
  data$USGbefore = NULL
  data$ALTbefore = NULL
  data$ASTbefore = NULL
  data$BUNbefore = NULL 
  data$CRbefore= NULL
  data$GLUbefore = NULL
  data$CHOLbefore = NULL
  data$HDLbefore = NULL
  data$TGbefore = NULL
  data$LDLbefore = NULL 
  data$SBPbefore = NULL
  data$DBPbefore = NULL 
  data$WAISTbefore = NULL
  data$BMIbefore = NULL
  data$time = NULL
  data$timebefore = NULL
  
  return(data)
}





##############################################################################
##############################################################################
################## Male :1  / Female :2  바꿔주면서 각각 진행합니다 ##########
##############################################################################
##############################################################################
normal_m<-normal[which(normal$SEX==2),]
bad_m<-bad[which(bad$SEX==2),]
person_m<-person[which(person$SEX==2),]

person_m$label<-0
normal_m$label<-1
bad_m$label<-2

#  real_test : 10%, train : 90%

# make real_test_set
###########################################################33
#########################################################
all_m<-rbind(person_m,normal_m)
write.csv(all_m,'all_m.csv',row.names = F)

all_m$SEX<-NULL
all_m$PDCD_HIST<-NULL

all_m$label<-as.factor(all_m$label)
glimpse(all_m)
i<-createDataPartition(y=all_m$label,p=0.2,list = F) # 20%
real_testdata<-all_m[i,]
table(real_testdata$label)
real_testdata_BAID<-real_testdata$BAID
write.csv(real_testdata,'real_testdata.csv',row.names = F)
write.csv(real_testdata_BAID,'real_testdata_BAID.csv',row.names = F)

new_all_m<-all_m[-i,]
table(new_all_m$label)
nonze<-new_all_m[-which(new_all_m$label==0),]
ze<-new_all_m[which(new_all_m$label==0),]


#############################################################################
########################(start loop)#########################################
#############################################################################

######
for ( w in c(1:10)){
  real_testdata<-read.csv('real_testdata.csv')
  idx<-sample(c(1:nrow(ze)),300)  ## 10번 반복 
  
  c_norm<-ze[idx,]
  
  total<-rbind(c_norm,nonze)
  colnames(total)
  colnames(real_testdata)
  
  #######
  
  for (i in c(1:8,33:75,90,92:97,105,130)){
    total[,i] = as.factor(total[,i])
  }
  
  for (i in c(1:8,33:75,90,92:97,105,130)){
    real_testdata[,i] = as.factor(real_testdata[,i])
  }
  
  total$BAID<-NULL
  real_testdata$BAID<-NULL
  glimpse(total)
  # remaindata<-total
  #########################
  colnames(total)
  
  
  dat<-total
  
  ## make test set ##
  
  table(dat$label)
  
  ########## 2nd
  patientsample = weight(dat)
  patientsample = change(patientsample)
  patientsample = delete(patientsample)
  dat<-patientsample
  
  patientsample = weight(real_testdata)
  patientsample = change(patientsample)
  patientsample = delete(patientsample)
  real_testdata<-patientsample
  
  #dat[,c(105:128)]<-NULL
  #real_testdata[,c(105:128)]<-NULL
  dat = fmhtrel(dat)
  dat = fmdmrel(dat)
  dat = fmlprel(dat)
  dat = fmcvarel(dat)
  dat = fmcdrel(dat)
  dat = fvc(dat)
  dat$FMHTREL<-as.factor(dat$FMHTREL)
  dat$FMDMREL<-as.factor(dat$FMDMREL)
  dat$FMLPREL<-as.factor(dat$FMLPREL)
  dat$FMCVAREL<-as.factor(dat$FMCVAREL)
  dat$FMCDREL<-as.factor(dat$FMCDREL)
  
  
  real_testdata = fmhtrel(real_testdata)
  real_testdata = fmdmrel(real_testdata)
  real_testdata = fmlprel(real_testdata)
  real_testdata = fmcvarel(real_testdata)
  real_testdata = fmcdrel(real_testdata)
  real_testdata = fvc(real_testdata)
  real_testdata$FMHTREL<-as.factor(real_testdata$FMHTREL)
  real_testdata$FMDMREL<-as.factor(real_testdata$FMDMREL)
  real_testdata$FMLPREL<-as.factor(real_testdata$FMLPREL)
  real_testdata$FMCVAREL<-as.factor(real_testdata$FMCVAREL)
  real_testdata$FMCDREL<-as.factor(real_testdata$FMCDREL)
  
  
  ## dummies ##
  
  dat.n<-dummy.data.frame(dat[,-which(colnames(dat)=='label')])
  dat<-cbind(dat.n,dat$label)
  nd<-ncol(dat)
  colnames(dat)
  ncol(dat)
  dat.n2<-dummy.data.frame(real_testdata[,-which(colnames(real_testdata)=='label')])
  real_testdata<-cbind(dat.n2,real_testdata$label)
  ntd<-ncol(real_testdata)
  ncol(real_testdata)
  
  zero<-setdiff(colnames(dat),colnames(real_testdata))
  zero2<-setdiff(colnames(real_testdata),colnames(dat))
  
  zero<-zero[1:length(zero)-1]
  zero2<-zero2[1:length(zero2)-1]
  zero
  zero2
  for ( i in zero){
    real_testdata[,i]<-0}
  
  for ( i in zero2){
    dat[,i]<-0}
  
  ncol(dat)
  ncol(real_testdata)
  
  
  # Make split index
  colnames(dat)[nd]<-'label'
  
  colnames(real_testdata)[ntd]<-'label'
  
  
  ###########################################################
  ################# XGboost modeling ########################
  
  where<-which(colnames(dat)=='label')
  
  
  dat$label<-as.numeric(dat$label)-1
  real_testdata$label<-as.numeric(real_testdata$label)-1 
  
  
  
  train_variables <- as.matrix(dat[,-where]) # index of column 'label'
  train_label <- as.matrix(dat[,"label"])
  
  train_matrix <- xgb.DMatrix(data = train_variables, label = train_label)
  
  
  
  xgb_params <- list(objective = "binary:logistic",
                     subsample = 0.9,
                     colsample_bytree = 0.8,
                     eta = 0.05,
                     max_depth = 7,
                     min_child_weight = 1)
  
  nround    <- 1000 # number of XGBoost rounds
  cv.nfold  <- 3
  
  # Fit cv.nfold * cv.nround XGB models and save OOF predictions
  
  
  cv_model_1 <- xgb.cv(params = xgb_params,
                       data = train_matrix, 
                       nrounds = nround,
                       nfold = cv.nfold,
                       verbose = TRUE,
                       prediction = TRUE,
                       early_stopping_rounds = 100,
                       nthread=2)
  
  n_round<-cv_model_1$best_iteration
  
  
  
  xg_model<-xgb.train(params = xgb_params,
                      data = train_matrix, 
                      nrounds =n_round )
  
  
  ii<-nrow(dat)
  nrow(real_testdata)
  binddata<-rbind(dat,real_testdata)
  real_testdata<-binddata[c( (ii+1):nrow(binddata)),]
  
  where2<-which(colnames(real_testdata)=='label')
  test.label<-as.matrix(real_testdata[,where2])
  
  
  test_matrix = (as.matrix(real_testdata[,-where2]))
  
  
  pred<-predict(xg_model,test_matrix) # result as prob
  
  prob_file = as.data.frame(pred)
  
  
  
  ################################### stop for a moment ######################################
  ####### male : 2m_prob_file, 2m_imp_matrix   //  female : 2f_prob_file, 2f_imp_matrix###########
  ############################################################################################
  #    prob_file 1 : 10 #
  nam<-paste0('2f_prob_file',w , '.csv')
  nam2<-paste0('2f_imp_matrix',w , '.csv')
  write.csv(as.data.frame(prob_file),nam,row.names = F)
  
  imp_matrix <- xgb.importance(model = xg_model)
  write.csv(imp_matrix,nam2,row.names=F)
  
  nrow(real_testdata)
  nrow(prob_file)
  
}
####################################################################################
####################################################################################
############################## End 10 times loop (bootstrap)  ######################
####################################################################################
####################################################################################
####################################################################################






## 10개 prob 파일 앙상블 ##
prob_file1<-read.csv('2f_prob_file1.csv')
prob_file2<-read.csv('2f_prob_file2.csv')
prob_file3<-read.csv('2f_prob_file3.csv')
prob_file4<-read.csv('2f_prob_file4.csv')
prob_file5<-read.csv('2f_prob_file5.csv')
prob_file6<-read.csv('2f_prob_file6.csv')
prob_file7<-read.csv('2f_prob_file7.csv')
prob_file8<-read.csv('2f_prob_file8.csv')
prob_file9<-read.csv('2f_prob_file9.csv')
prob_file10<-read.csv('2f_prob_file10.csv')



prob_file_total<-prob_file1+prob_file2+prob_file3+prob_file4+prob_file5+
  prob_file6+prob_file7+prob_file8+prob_file9+prob_file10
prob_file_total<-prob_file_total / 10
plot(prob_file_total$pred)
oneid<-which(prob_file_total$pred>=0.215)
prob_file_total$pred[oneid]<-1
prob_file_total$pred[-oneid]<-0
prob_file_total$pred<-as.factor(prob_file_total$pred)

#prob_file_total$label <- apply(prob_file_total, 1, which.max)
#prob_file_total$label<-prob_file_total$label-1
#prob_file_total$label<-as.factor(prob_file_total$label)
fe2_CM<-confusionMatrix(prob_file_total$pred,as.factor(real_testdata$label))
fe2_CM
re<-fe2_CM$byClass[6]
pr<-fe2_CM$byClass[5]
f2<-(5*pr*re)/(4*pr+re)
names(f2)<-'F2-Measure'
f2
m2_CM$byClass
## compare ##

oneid<-which(prob_file1$pred>=0.45)
prob_file1$pred[oneid]<-1
prob_file1$pred[-oneid]<-0
prob_file1$pred<-as.factor(prob_file1$pred)
confusionMatrix(prob_file1$pred,as.factor(real_testdata$label))


####### feature ##########

imp_matrix1<-read.csv('2f_imp_matrix1.csv')
imp_matrix2<-read.csv('2f_imp_matrix2.csv')
imp_matrix3<-read.csv('2f_imp_matrix3.csv')
imp_matrix4<-read.csv('2f_imp_matrix4.csv')
imp_matrix5<-read.csv('2f_imp_matrix5.csv')
imp_matrix6<-read.csv('2f_imp_matrix6.csv')
imp_matrix7<-read.csv('2f_imp_matrix7.csv')
imp_matrix8<-read.csv('2f_imp_matrix8.csv')
imp_matrix9<-read.csv('2f_imp_matrix9.csv')
imp_matrix10<-read.csv('2f_imp_matrix10.csv')

importance_frame<-as.data.frame(matrix(NA,20,10))

importance_frame[,1]<-as.character(imp_matrix1$Feature[1:20])
importance_frame[,2]<-as.character(imp_matrix2$Feature[1:20])
importance_frame[,3]<-as.character(imp_matrix3$Feature[1:20])
importance_frame[,4]<-as.character(imp_matrix4$Feature[1:20])
importance_frame[,5]<-as.character(imp_matrix5$Feature[1:20])
importance_frame[,6]<-as.character(imp_matrix6$Feature[1:20])
importance_frame[,7]<-as.character(imp_matrix7$Feature[1:20])
importance_frame[,8]<-as.character(imp_matrix8$Feature[1:20])
importance_frame[,9]<-as.character(imp_matrix9$Feature[1:20])
importance_frame[,10]<-as.character(imp_matrix10$Feature[1:20])


two_male_imp<-importance_frame
two_male_imp_vec<-c(importance_frame[,1],importance_frame[,2],importance_frame[,3],importance_frame[,4],
                    importance_frame[,5],importance_frame[,6],importance_frame[,7],importance_frame[,8],
                    importance_frame[,9],importance_frame[,10])


importance_vector[which(table(two_male_imp_vec)==10)]
sort(table(two_male_imp_vec),decreasing = T)

View(two_male_imp)



