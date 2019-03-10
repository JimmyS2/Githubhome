library(ggdendro)
require("igraph")


bioage<-read.csv('bioage.csv')

bio_data<-bioage

#### na가 30%이상인 변수 삭제 ####
nalist<-c(4,5,21,23,37,38,39,40,41,43,49,51,59,64)

colnames(bio_data)[nalist]
bio_data[,nalist]<-NULL


bio_male<-bio_data[which(bio_data$SEX==1),]
bio_male$CA125 <- NULL


### NA가 존재하는 obs 삭제 ####
bio_male<-bio_male[which(complete.cases(bio_male)==T),]


#### male 변수별 정상 기준으로부터 거리 구하기 Part1 ####

man = function(data){
  for (i in 1:nrow(data)){
    
    if(data$SBP[i] < 120){
      data$SBP[i] = 0
    }
    else{
      data$SBP[i] = data$SBP[i] -120
    }
    
    if(data$DBP[i] < 80){
      data$DBP[i] = 0
    }
    else{
      data$DBP[i] = data$DBP[i] -80
    }
    
    if(data$BMP[i] >= 60){
      data$BMP[i] = 0
    }
    else if(data$BMP[i] < 60){
      data$BMP[i] = abs(data$BMP[i] -60)
    }
    
    if(data$BFP[i] >= 10 & data$BFP[i] < 20){
      data$BFP[i] = 0
    }
    else if(data$BFP[i] < 10){
      data$BFP[i] = abs(data$BFP[i] -10)
    }
    else{
      data$BFP[i] = data$BFP[i] -20
    }
    
    if(data$BMI[i] >= 18.5 & data$BMI[i] < 25){
      data$BMI[i] = 0
    }
    else if(data$BMI[i] < 18.5){
      data$BMI[i] = abs(data$BMI[i] -18.5)
    }
    else{
      data$BMI[i] = data$BMI[i] -25
    }
    
    if(data$FVCPP[i] <80 & data$FEV1_FVC[i] >=0.7){
      data$fvc[i] = 1
    }
    else if(data$FVCPP[i] >=80 & data$FEV1_FVC[i] <0.7){
      data$fvc[i] = 1
    }
    else if(data$FVCPP[i] <80 & data$FEV1_FVC[i] <0.7){
      data$fvc[i] = 2
    }
    else{
      data$fvc[i] = 0
    }
    
    
    if(data$FEV1PP[i] > 40){
      data$FEV1PP[i] = 0
    }
    else if(data$FEV1PP[i] <= 40){
      data$FEV1PP[i] = abs(data$FEV1PP[i] - 40)
    }
    
    if(data$TOTPRO[i] >= 6.6 & data$TOTPRO[i] <= 8.7){
      data$TOTPRO[i] = 0
    }
    else if(data$TOTPRO[i] < 6.6){
      data$TOTPRO[i] = abs(data$TOTPRO[i] - 6.6)
    }
    else if(data$TOTPRO[i] > 8.7){
      data$TOTPRO[i] = data$TOTPRO[i] -8.7
    }
    
    if(data$ALB[i] >= 3.5 & data$ALB[i] <= 5.2){
      data$ALB[i] = 0
    }
    else if(data$ALB[i] < 3.5){
      data$ALB[i] = abs(data$ALB[i] - 3.5)
    }
    else if(data$ALB[i] > 5.2){
      data$ALB[i] = data$ALB[i] - 5.2
    }
    
    if(data$TOTBIL[i] >= 0.1 & data$TOTBIL[i] <= 1.2){
      data$TOTBIL[i] = 0
    }
    else if(data$TOTBIL[i] < 0.1){
      data$TOTBIL[i] = abs(data$TOTBIL[i] - 0.1)
    }
    else if(data$TOTBIL[i] > 1.2){
      data$TOTBIL[i] = data$TOTBIL[i] - 1.2
    }
    
    if(data$ALP[i] >= 40 & data$ALP[i] <= 129){
      data$ALP[i] = 0
    }
    else if(data$ALP[i] < 40){
      data$ALP[i] = abs(data$ALP[i] - 40)
    }
    else if(data$ALP[i] > 129){
      data$ALP[i] = data$ALP[i] - 129
    }
    
    if(data$AST[i] >= 0 & data$AST[i] <= 40){
      data$AST[i] = 0
    }
    else{
      data$AST[i] = data$AST[i] - 40
    }
    
    if(data$ALT[i] >= 0 & data$ALT[i] <= 35){
      data$ALT[i] = 0
    }
    else{
      data$ALT[i] = data$ALT[i] - 35
    }
    
    if(data$GGT[i] >= 11 & data$GGT[i] < 64){
      data$GGT[i] = 0
    }
    else if(data$GGT[i] <11) {
      data$GGT[i] = abs(data$GGT[i] - 11)
    }
    else{
      data$GGT[i] = data$GGT[i] - 64
    }
    
    if(data$LDH[i] >= 240 & data$LDH[i] <= 480){
      data$LDH[i] = 0
    }
    else if(data$LDH[i] <240) {
      data$LDH[i] = abs(data$LDH[i] - 240)
    }
    else{
      data$LDH[i] = data$LDH[i] - 480
    }
    
    if(data$CREA[i] >= 0.5 & data$CREA[i] <= 1.5){
      data$CREA[i] = 0
    }
    else if(data$CREA[i] <0.5) {
      data$CREA[i] = abs(data$CREA[i] - 0.5)
    }
    else{
      data$CREA[i] = data$CREA[i] - 1.5
    }
    
    if(data$BUN[i] >= 6 & data$BUN[i] <= 20){
      data$BUN[i] = 0
    }
    else if(data$BUN[i] <6) {
      data$BUN[i] = abs(data$BUN[i] - 6)
    }
    else{
      data$BUN[i] = data$BUN[i] - 20
    }
    
    if(data$URICACID[i] >= 3.4 & data$URICACID[i] <= 7.0){
      data$URICACID[i] = 0
    }
    else if(data$URICACID[i] <3.4) {
      data$URICACID[i] = abs(data$URICACID[i] - 3.4)
    }
    else{
      data$URICACID[i] = data$URICACID[i] - 7.0
    }
    
    if(data$SODIUM[i] >= 136 & data$SODIUM[i] <= 145){
      data$SODIUM[i] = 0
    }
    else if(data$SODIUM[i] <136) {
      data$SODIUM[i] = abs(data$SODIUM[i] - 136)
    }
    else{
      data$SODIUM[i] = data$SODIUM[i] - 145
    }
    
    if(data$POTASSIUM[i] >= 3.5 & data$POTASSIUM[i] <= 5.5){
      data$POTASSIUM[i] = 0
    }
    else if(data$POTASSIUM[i] < 3.5) {
      data$POTASSIUM[i] = abs(data$POTASSIUM[i] - 3.5)
    }
    else{
      data$POTASSIUM[i] = data$POTASSIUM[i] - 5.5
    }
    
    if(data$CHLORIDE[i] >= 98 & data$CHLORIDE[i] <= 110){
      data$CHLORIDE[i] = 0
    }
    else if(data$CHLORIDE[i] < 98) {
      data$CHLORIDE[i] = abs(data$CHLORIDE[i] - 98)
    }
    else{
      data$CHLORIDE[i] = data$CHLORIDE[i] - 110
    }
    
    if(data$CALCIUM[i] >= 8.6 & data$CALCIUM[i] <= 10.0){
      data$CALCIUM[i] = 0
    }
    else if(data$CALCIUM[i] < 8.6) {
      data$CALCIUM[i] = abs(data$CALCIUM[i] - 8.6)
    }
    else{
      data$CALCIUM[i] = data$CALCIUM[i] - 10.0
    }
    
    if(data$PHOSPHORUS[i] >= 2.7 & data$PHOSPHORUS[i] <= 4.5){
      data$PHOSPHORUS[i] = 0
    }
    else if(data$PHOSPHORUS[i] < 2.7) {
      data$PHOSPHORUS[i] = abs(data$PHOSPHORUS[i] - 2.7)
    }
    else{
      data$PHOSPHORUS[i] = data$PHOSPHORUS[i] - 4.5
    }
    
    if(data$CHOL[i] >= 0 & data$CHOL[i] <= 200){
      data$CHOL[i] = 0
    }
    else{
      data$CHOL[i] = data$CHOL[i] - 200
    }
    
    if(data$HDL[i] >= 60){
      data$HDL[i] = 0
    }
    else{
      data$HDL[i] = abs(data$HDL[i] - 60)
    }
    
    if(data$TG[i] >= 0 & data$TG[i] <= 150){
      data$TG[i] = 0
    }
    else{
      data$TG[i] = data$TG[i] - 150
    }
    
    if(data$LDL[i] >= 0 & data$LDL[i] < 130){
      data$LDL[i] = 0
    }
    else{
      data$LDL[i] = data$LDL[i] - 130
    }
    
    if(data$GLU[i] >= 0 & data$GLU[i] < 100){
      data$GLU[i] = 0
    }
    else{
      data$GLU[i] = data$GLU[i] - 100
    }
    
    if(data$RBC[i] >= 4.1 & data$RBC[i] <= 5.6){
      data$RBC[i] = 0
    }
    else if(data$RBC[i] < 4.1){
      data$RBC[i] = abs(data$RBC[i] - 4.1)
    }
    else{
      data$RBC[i] = data$RBC[i] - 5.6
    }
    
    if(data$HCT[i] >= 40 & data$HCT[i] <= 50){
      data$HCT[i] = 0
    }
    else if(data$HCT[i] < 40){
      data$HCT[i] = abs(data$HCT[i] - 40)
    }
    else{
      data$HCT[i] = data$HCT[i] - 50
    }
    
    if(data$HB[i] >= 13 & data$HB[i] <= 16.5){
      data$HB[i] = 0
    }
    else if(data$HB[i] < 13){
      data$HB[i] = abs(data$HB[i] - 13)
    }
    else{
      data$HB[i] = data$HB[i] - 16.5
    }
    
    
    
  } ## end for
  return (data)
} ## end function

bio_male<-man(bio_male)
bio_male$FVCPP = NULL
bio_male$FEV1_FVC = NULL

#### male 변수별 정상 기준으로부터 거리 구하기 Part2 ####
n<-nrow(bio_male)

for( i in c(1:n)){
  bio_male$MCV[i]<-abs(median(c(bio_male$MCV[i],81,100))-bio_male$MCV[i])}
for( i in c(1:n)){
  bio_male$MCH[i]<-abs(median(c(bio_male$MCH[i],28,32))-bio_male$MCH[i])}
for( i in c(1:n)){
  bio_male$MCHC[i]<-abs(median(c(bio_male$MCHC[i],31,35))-bio_male$MCHC[i])}
for( i in c(1:n)){
  bio_male$ESR[i]<-abs(median(c(bio_male$ESR[i],0,20))-bio_male$ESR[i])}
for( i in c(1:n)){
  bio_male$PLAT[i]<-abs(median(c(bio_male$PLAT[i],150,370))-bio_male$PLAT[i])}
for( i in c(1:n)){
  bio_male$PDW[i]<-abs(median(c(bio_male$PDW[i],8.8,16.5))-bio_male$PDW[i])}
for( i in c(1:n)){
  bio_male$NEUTROPHIL[i]<-abs(median(c(bio_male$NEUTROPHIL[i],50,70))-bio_male$NEUTROPHIL[i])}
for( i in c(1:n)){
  bio_male$LYMPHOCYTE[i]<-abs(median(c(bio_male$LYMPHOCYTE[i],20,50))-bio_male$LYMPHOCYTE[i])}
for( i in c(1:n)){
  bio_male$MONOCYTE[i]<-abs(median(c(bio_male$MONOCYTE[i],4,12))-bio_male$MONOCYTE[i])}
for( i in c(1:n)){
  bio_male$EOSINOPHIL[i]<-abs(median(c(bio_male$EOSINOPHIL[i],0,7))-bio_male$EOSINOPHIL[i])}
for( i in c(1:n)){
  bio_male$BASOPHIL[i]<-abs(median(c(bio_male$BASOPHIL[i],0,1.5))-bio_male$BASOPHIL[i])}

for( i in c(1:n)){
  bio_male$TSH[i]<-abs(median(c(bio_male$TSH[i],0.27,4.20))-bio_male$TSH[i])}
for( i in c(1:n)){
  bio_male$FT4[i]<-abs(median(c(bio_male$FT4[i],0.93,1.7))-bio_male$FT4[i])}
for( i in c(1:n)){
  bio_male$T3[i]<-abs(median(c(bio_male$T3[i],0.8,2.0))-bio_male$T3[i])}
for( i in c(1:n)){
  bio_male$CRP[i]<-abs(median(c(bio_male$CRP[i],0,10))-bio_male$CRP[i])}
for( i in c(1:n)){
  bio_male$CEA[i]<-abs(median(c(bio_male$CEA[i],0,3.8))-bio_male$CEA[i])}
for( i in c(1:n)){
  bio_male$AFP[i]<-abs(median(c(bio_male$AFP[i],0,7))-bio_male$AFP[i])}
for( i in c(1:n)){
  bio_male$CA125[i]<-abs(median(c(bio_male$CA125[i],0,35))-bio_male$CA125[i])}
for( i in c(1:n)){
  bio_male$CA199[i]<-abs(median(c(bio_male$CA199[i],0,34))-bio_male$CA199[i])}
for( i in c(1:n)){
  bio_male$PSA[i]<-abs(median(c(bio_male$PSA[i],0,4))-bio_male$PSA[i])}
for( i in c(1:n)){
  bio_male$UPH[i]<-abs(median(c(bio_male$UPH[i],4.8,7.8))-bio_male$UPH[i])}
for( i in c(1:n)){
  bio_male$USG[i]<-abs(median(c(bio_male$USG[i],1.003,1.030))-bio_male$USG[i])}

bio_male$HBSAG<-as.character(bio_male$HBSAG)
bio_male$HBSAG[which(bio_male$HBSAG=='Negative')]<-0
bio_male$HBSAG[which(bio_male$HBSAG=='WeakPositive')]<-1
bio_male$HBSAG[which(bio_male$HBSAG=='Positive')]<-2
bio_male$HBSAG<-as.numeric(bio_male$HBSAG)

bio_male$HBSAB<-as.character(bio_male$HBSAB)
bio_male$HBSAB[which(bio_male$HBSAB=='Negative')]<-2
bio_male$HBSAB[which(bio_male$HBSAB=='Equivocal')]<-1
bio_male$HBSAB[which(bio_male$HBSAB=='Positive')]<-0
bio_male$HBSAB<-as.numeric(bio_male$HBSAB)

bio_male$RF<-as.character(bio_male$RF)
bio_male$RF[which(bio_male$RF=='Negative')]<-0
bio_male$RF[which(bio_male$RF=='Trace')]<-1
bio_male$RF[which(bio_male$RF=='Positive')]<-2
bio_male$RF<-as.numeric(bio_male$RF)

bio_male$ANTIHCVAB<-as.character(bio_male$ANTIHCVAB)
bio_male$ANTIHCVAB[which(bio_male$ANTIHCVAB=='Negative')]<-0
bio_male$ANTIHCVAB[which(bio_male$ANTIHCVAB=='Equivocal')]<-1
bio_male$ANTIHCVAB[which(bio_male$ANTIHCVAB=='Positive')]<-2
bio_male$ANTIHCVAB<-as.numeric(bio_male$ANTIHCVAB)

bio_male$UNITR<-as.character(bio_male$UNITR)
bio_male$UNITR[which(bio_male$UNITR=='Negative')]<-0
bio_male$UNITR[which(bio_male$UNITR=='Trace')]<-1
bio_male$UNITR[which(bio_male$UNITR=='Positive')]<-2
bio_male$UNITR<-as.numeric(bio_male$UNITR)

bio_male$UPRO<-as.character(bio_male$UPRO)
bio_male$UPRO[which(bio_male$UPRO=='Negative')]<-0
bio_male$UPRO[which(bio_male$UPRO=='Trace')]<-1
bio_male$UPRO[which(bio_male$UPRO=='1Positive')]<-2
bio_male$UPRO[which(bio_male$UPRO=='2Positive')]<-3
bio_male$UPRO[which(bio_male$UPRO=='3Positive')]<-4
bio_male$UPRO[which(bio_male$UPRO=='4Positive')]<-5
bio_male$UPRO<-as.numeric(bio_male$UPRO)

bio_male$UGLU<-as.character(bio_male$UGLU)
bio_male$UGLU[which(bio_male$UGLU=='Negative')]<-0
bio_male$UGLU[which(bio_male$UGLU=='Trace')]<-1
bio_male$UGLU[which(bio_male$UGLU=='1Positive')]<-2
bio_male$UGLU[which(bio_male$UGLU=='2Positive')]<-3
bio_male$UGLU[which(bio_male$UGLU=='3Positive')]<-4
bio_male$UGLU[which(bio_male$UGLU=='4Positive')]<-5
bio_male$UGLU<-as.numeric(bio_male$UGLU)

bio_male$UKET<-as.character(bio_male$UKET)
bio_male$UKET[which(bio_male$UKET=='Negative')]<-0
bio_male$UKET[which(bio_male$UKET=='Trace')]<-1
bio_male$UKET[which(bio_male$UKET=='1Positive')]<-2
bio_male$UKET[which(bio_male$UKET=='2Positive')]<-3
bio_male$UKET[which(bio_male$UKET=='3Positive')]<-4
bio_male$UKET[which(bio_male$UKET=='4Positive')]<-5
bio_male$UKET<-as.numeric(bio_male$UKET)

bio_male$URO<-as.character(bio_male$URO)
bio_male$URO[which(bio_male$URO=='Negative')]<-0
bio_male$URO[which(bio_male$URO=='Trace')]<-1
bio_male$URO[which(bio_male$URO=='Positive')]<-2
bio_male$URO<-as.numeric(bio_male$URO)

bio_male$UBIL<-as.character(bio_male$UBIL)
bio_male$UBIL[which(bio_male$UBIL=='Negative')]<-0
bio_male$UBIL[which(bio_male$UBIL=='Trace')]<-1
bio_male$UBIL[which(bio_male$UBIL=='Positive')]<-2
bio_male$UBIL<-as.numeric(bio_male$UBIL)

bio_male$UBLD<-as.character(bio_male$UBLD)
bio_male$UBLD[which(bio_male$UBLD=='Negative')]<-0
bio_male$UBLD[which(bio_male$UBLD=='Trace')]<-1
bio_male$UBLD[which(bio_male$UBLD=='1Positive')]<-2
bio_male$UBLD[which(bio_male$UBLD=='2Positive')]<-3
bio_male$UBLD[which(bio_male$UBLD=='3Positive')]<-4
bio_male$UBLD[which(bio_male$UBLD=='4Positive')]<-5
bio_male$UBLD<-as.numeric(bio_male$UBLD)



##########outlier 변수별로 확인하는 과정 ##########
dat1 = bio_male

par(mfrow=c(2,3))
hist(dat1$SBP,breaks= 50, ylim=c(0,5))   ## 100이상
which(dat1$SBP>=100)
hist(dat1$DBP, breaks= 50,ylim=c(0,5))  ## 70이상
which(dat1$DBP>=70)
hist(dat1$BMP, breaks= 50,ylim=c(0,5))
hist(dat1$BFP, breaks= 50,ylim=c(0,5))  
hist(dat1$BMI, breaks= 50,ylim=c(0,5))   ## 20 이상
which(dat1$BMI>=20)
hist(dat1$FEV1PP, breaks= 50,ylim=c(0,5))  ## 20이상
which(dat1$FEV1PP>=20)

hist(dat1$TOTPRO,breaks= 50, ylim=c(0,5))   ## 1.5이상
which(dat1$TOTPRO>=1.5)
hist(dat1$ALB, breaks= 50,ylim=c(0,5))    ## 1.2 이상
which(dat1$ALB>=1.2)
hist(dat1$TOTBIL, breaks= 50,ylim=c(0,5))   ## 4 이상
which(dat1$TOTBIL>=4)
hist(dat1$BUN, breaks= 50,ylim=c(0,5))   ##  29이상
which(dat1$BUN>=29)
hist(dat1$URICACID, breaks= 50,ylim=c(0,5)) 
which(dat1$URICACID>=5)
hist(dat1$SODIUM, breaks= 50,ylim=c(0,5)) 
which(dat1$SODIUM>=7)

hist(dat1$POTASSIUM,breaks= 50, ylim=c(0,5))  
which(dat1$POTASSIUM>=1.5)
hist(dat1$CHLORIDE, breaks= 50,ylim=c(0,5))   
hist(dat1$CALCIUM, breaks= 50,ylim=c(0,5))
hist(dat1$PHOSPHORUS, breaks= 50,ylim=c(0,5))
which(dat1$PHOSPHORUS>=6.7)
hist(dat1$CHOL, breaks= 50,ylim=c(0,5))    
hist(dat1$LDL, breaks= 50,ylim=c(0,5))  
which(dat1$LDL>=200)


hist(dat1$GLU,breaks= 50, ylim=c(0,5))
hist(dat1$RBC, breaks= 50,ylim=c(0,5))  ##2.5이상
which(dat1$RBC>=2.5)
hist(dat1$HCT, breaks= 50,ylim=c(0,5))  ##20이상
which(dat1$HCT>=20)
hist(dat1$HB, breaks= 50,ylim=c(0,5))   ## 7이상
which(dat1$HB>=7)
hist(dat1$MCV, breaks= 50,ylim=c(0,5))  ## 20이상
which(dat1$MCV>=20)
hist(dat1$MCH, breaks= 50,ylim=c(0,5))

hist(dat1$MCHC,breaks= 50, ylim=c(0,5))  
hist(dat1$PLAT, breaks= 50,ylim=c(0,5))   ##500이상
which(dat1$PLAT>=500)
hist(dat1$NEUTROPHIL, breaks= 50,ylim=c(0,5))  
hist(dat1$LYMPHOCYTE, breaks= 50,ylim=c(0,5))  ##20이상 
which(dat1$LYMPHOCYTE>=20)
hist(dat1$MONOCYTE, breaks= 50,ylim=c(0,5))
which(dat1$MONOCYTE>=15)
hist(dat1$EOSINOPHIL, breaks= 50,ylim=c(0,5)) 
which(dat1$EOSINOPHIL>=30)

hist(dat1$BASOPHIL,breaks= 50, ylim=c(0,5))
which(dat1$BASOPHIL>=2)
hist(dat1$TSH, breaks= 50,ylim=c(0,5))
which(dat1$TSH>=150)
hist(dat1$FT4, breaks= 50,ylim=c(0,5))  
hist(dat1$CEA, breaks= 50,ylim=c(0,5))  ## 1000이상
which(dat1$CEA>=1000)
hist(dat1$AFP, breaks= 50,ylim=c(0,5))  ## 1000이상
which(dat1$AFP>=1000)
hist(dat1$CA199, breaks= 50,ylim=c(0,5))    ## 1500이상
which(dat1$CA199>=1500)

hist(dat1$PSA,breaks= 50, ylim=c(0,5))   ##100이상
which(dat1$PSA>=100)
hist(dat1$UPH, breaks= 50,ylim=c(0,5))
hist(dat1$USG, breaks= 50,ylim=c(0,5))


#### 위의 히스토그램을 바탕으로 delete outlier obs ####
bio_male = bio_male[-c(4000,4157,7065,6649,5600,1884,3128,2652,7498,8044,1208,4458,2745,407),]

#### minmax scaling ####
maxValue<- apply(bio_male[,-c(1,2,3)], 2, max)
minValue<- apply(bio_male[,-c(1,2,3)], 2, min)
ms<-as.data.frame(scale(bio_male[,-c(1,2,3)], center= minValue, scale=(maxValue-minValue))) 
bio_male_s<-cbind(bio_male[,c(1,2,3)],ms)


###### hierarchical clust ######

dat1 = bio_male_s
dist_data <- dist(t(dat1[,-c(1,2,3)]))


hc_a <- hclust(dist_data, method ="ward.D")

library(ggdendro)
ggdendrogram(hc_a, theme_dendro = FALSE, hang_height = 0.01) +labs(title="Dendrogram in ggplot2")

require("igraph")
fviz_dend(hc_a, k = 1, k_colors = "jco",
          type = "phylogenic", repel = TRUE)


###########################################################################

#### group 1,2,3,4,5의 거리를 구하는 함수 ######

group1 = function(data){
  core = rep(0,17)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$ALT[i], data$AST[i], data$GGT[i], data$CHLORIDE[i],
            data$MONOCYTE[i], data$FT4[i] ,data$UNITR[i] , data$BASOPHIL[i]
            , data$BMP[i] , data$FEV1PP[i] , data$CREA[i] , data$CA199[i], data$TSH[i],
            data$PSA[i], data$CEA[i], data$AFP[i], data$USG[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}

group2 = function(data){
  core = rep(0,11)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$LYMPHOCYTE[i], data$CALCIUM[i], data$UPH[i], data$ALP[i],
            data$LDH[i], data$POTASSIUM[i], data$ALB[i], data$TOTPRO[i], data$PLAT[i], 
            data$BUN[i], data$EOSINOPHIL[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}

group3 = function(data){
  core = rep(0,15)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$BMI[i], data$GLU[i], data$UGLU[i], data$HB[i], data$RBC[i], data$HCT[i],
            data$MCHC[i], data$MCV[i], data$MCH[i], data$UBIL[i], data$ANTIHCVAB[i], data$URICACID[i],
            data$TOTBIL[i], data$SODIUM[i], data$PHOSPHORUS[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}

group4 = function(data){
  core = rep(0,12)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$HBSAG[i], data$TG[i], data$UPRO[i], data$UKET[i],
            data$NEUTROPHIL[i], data$CHOL[i], data$LDL[i], data$BFP[i],
            data$SBP[i], data$DBP[i], data$UBLD[i], data$fvc[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}

group5 = function(data){
  core = rep(0,4)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$RF[i], data$HBSAB[i], data$HDL[i], data$URO[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}


#### score를 변수로 추가해줌 ####
test = bio_male_s
score = group1(test)+ group2(test)+ group3(test)+ group4(test)+ group5(test)

test = cbind(test, score)

## hist(test$score, freq=FALSE, breaks=50, col = "aliceblue",xlab="Sum Of Score", main = "Sum Of Score")


## 상위 69개(1%이하) -> 극단적인 값(이상치) 삭제 
sum(test$score>=3.5)
test = test[test$score<3.5,]


## score를 -10 ~ 10 사이로 스케일링 #####
maxValue = max(test$score)
minValue = min(test$score)
test$score = scale(test$score, center = minValue, scale=(maxValue-minValue))
test$score = test$score*20-10
## hist(test$score, breaks=50, freq=FALSE, col ="lavenderblush", xlab="건강지수", main="건강지수")


### 선형회귀 모델 적용 ###
fit =lm(score~., data= test[,-c(1,2,3)])
both <- step(fit, direction="both", trace= F)


##########################위와 똑같은 방식으로 여성에게도 적용 ######################

bioage<-read.csv('bioage.csv')

bio_data<-bioage

nalist<-c(4,5,21,23,37,38,39,40,41,43,49,51,59,64)

colnames(bio_data)[nalist]
bio_data[,nalist]<-NULL


bio_female<-bio_data[which(bio_data$SEX==2),]
bio_female$PSA<-NULL

bio_female<-bio_female[which(complete.cases(bio_female)==T),]

#### female 변수별 정상 기준으로부터 거리 구하기 Part1 ####

woman = function(data){
  for (i in 1:nrow(data)){
    
    if(data$SBP[i] < 120){
      data$SBP[i] = 0
    }
    else{
      data$SBP[i] = data$SBP[i] -120
    }
    
    if(data$DBP[i] < 80){
      data$DBP[i] = 0
    }
    else{
      data$DBP[i] = data$DBP[i] -80
    }
    
    if(data$BMP[i] >= 50){
      data$BMP[i] = 0
    }
    else if(data$BMP[i] < 50){
      data$BMP[i] = abs(data$BMP[i] -50)
    }
    
    if(data$BFP[i] >= 20 & data$BFP[i] < 30){
      data$BFP[i] = 0
    }
    else if(data$BFP[i] < 20){
      data$BFP[i] = abs(data$BFP[i] -20)
    }
    else if(data$BFP[i] >= 30){
      data$BFP[i] = data$BFP[i] -30
    }
    
    if(data$BMI[i] >= 18.5 & data$BMI[i] < 25){
      data$BMI[i] = 0
    }
    else if(data$BMI[i] < 18.5){
      data$BMI[i] = abs(data$BMI[i] -18.5)
    }
    else if(data$BMI[i] >= 25){
      data$BMI[i] = data$BMI[i] -25
    }
    
    
    if(data$FVCPP[i] <80 & data$FEV1_FVC[i] >=0.7){
      data$fvc[i] = 1
    }
    else if(data$FVCPP[i] >=80 & data$FEV1_FVC[i] <0.7){
      data$fvc[i] = 1
    }
    else if(data$FVCPP[i] <80 & data$FEV1_FVC[i] <0.7){
      data$fvc[i] = 2
    }
    else{
      data$fvc[i] = 0
    }
    
    if(data$FEV1PP[i] > 40){
      data$FEV1PP[i] = 0
    }
    else if(data$FEV1PP[i] <= 40){
      data$FEV1PP[i] = abs(data$FEV1PP[i] - 40)
    }
    
    
    if(data$TOTPRO[i] >= 6.6 & data$TOTPRO[i] <= 8.7){
      data$TOTPRO[i] = 0
    }
    else if(data$TOTPRO[i] < 6.6){
      data$TOTPRO[i] = abs(data$TOTPRO[i] - 6.6)
    }
    else if(data$TOTPRO[i] > 8.7){
      data$TOTPRO[i] = data$TOTPRO[i] -8.7
    }
    
    if(data$ALB[i] >= 3.5 & data$ALB[i] <= 5.2){
      data$ALB[i] = 0
    }
    else if(data$ALB[i] < 3.5){
      data$ALB[i] = abs(data$ALB[i] - 3.5)
    }
    else if(data$ALB[i] > 5.2){
      data$ALB[i] = data$ALB[i] - 5.2
    }
    
    if(data$TOTBIL[i] >= 0.1 & data$TOTBIL[i] <= 1.2){
      data$TOTBIL[i] = 0
    }
    else if(data$TOTBIL[i] < 0.1){
      data$TOTBIL[i] = abs(data$TOTBIL[i] - 0.1)
    }
    else if(data$TOTBIL[i] > 1.2){
      data$TOTBIL[i] = data$TOTBIL[i] - 1.2
    }
    
    if(data$ALP[i] >= 35 & data$ALP[i] <= 104){
      data$ALP[i] = 0
    }
    else if(data$ALP[i] < 35){
      data$ALP[i] = abs(data$ALP[i] - 35)
    }
    else if(data$ALP[i] > 104){
      data$ALP[i] = data$ALP[i] - 104
    }
    
    if(data$AST[i] >= 0 & data$AST[i] <= 40){
      data$AST[i] = 0
    }
    else{
      data$AST[i] = data$AST[i] - 40
    }
    
    if(data$ALT[i] >= 0 & data$ALT[i] <= 35){
      data$ALT[i] = 0
    }
    else{
      data$ALT[i] = data$ALT[i] - 35
    }
    
    if(data$GGT[i] >= 8 & data$GGT[i] < 36){
      data$GGT[i] = 0
    }
    else if(data$GGT[i] <8) {
      data$GGT[i] = abs(data$GGT[i] - 8)
    }
    else{
      data$GGT[i] = data$GGT[i] - 36
    }
    
    if(data$LDH[i] >= 240 & data$LDH[i] <= 480){
      data$LDH[i] = 0
    }
    else if(data$LDH[i] <240) {
      data$LDH[i] = abs(data$LDH[i] - 240)
    }
    else{
      data$LDH[i] = data$LDH[i] - 480
    }
    
    if(data$CREA[i] >= 0.5 & data$CREA[i] <= 1.5){
      data$CREA[i] = 0
    }
    else if(data$CREA[i] <0.5) {
      data$CREA[i] = abs(data$CREA[i] - 0.5)
    }
    else{
      data$CREA[i] = data$CREA[i] - 1.5
    }
    
    if(data$BUN[i] >= 6 & data$BUN[i] <= 20){
      data$BUN[i] = 0
    }
    else if(data$BUN[i] <6) {
      data$BUN[i] = abs(data$BUN[i] - 6)
    }
    else{
      data$BUN[i] = data$BUN[i] - 20
    }
    
    if(data$URICACID[i] >= 2.4 & data$URICACID[i] <= 5.7){
      data$URICACID[i] = 0
    }
    else if(data$URICACID[i] <2.4) {
      data$URICACID[i] = abs(data$URICACID[i] - 2.4)
    }
    else{
      data$URICACID[i] = data$URICACID[i] - 5.7
    }
    
    
    if(data$SODIUM[i] >= 136 & data$SODIUM[i] <= 145){
      data$SODIUM[i] = 0
    }
    else if(data$SODIUM[i] <136) {
      data$SODIUM[i] = abs(data$SODIUM[i] - 136)
    }
    else{
      data$SODIUM[i] = data$SODIUM[i] - 145
    }
    
    if(data$POTASSIUM[i] >= 3.5 & data$POTASSIUM[i] <= 5.5){
      data$POTASSIUM[i] = 0
    }
    else if(data$POTASSIUM[i] < 3.5) {
      data$POTASSIUM[i] = abs(data$POTASSIUM[i] - 3.5)
    }
    else{
      data$POTASSIUM[i] = data$POTASSIUM[i] - 5.5
    }
    
    if(data$CHLORIDE[i] >= 98 & data$CHLORIDE[i] <= 110){
      data$CHLORIDE[i] = 0
    }
    else if(data$CHLORIDE[i] < 98) {
      data$CHLORIDE[i] = abs(data$CHLORIDE[i] - 98)
    }
    else{
      data$CHLORIDE[i] = data$CHLORIDE[i] - 110
    }
    
    if(data$CALCIUM[i] >= 8.6 & data$CALCIUM[i] <= 10.0){
      data$CALCIUM[i] = 0
    }
    else if(data$CALCIUM[i] < 8.6) {
      data$CALCIUM[i] = abs(data$CALCIUM[i] - 8.6)
    }
    else{
      data$CALCIUM[i] = data$CALCIUM[i] - 10.0
    }
    
    if(data$PHOSPHORUS[i] >= 2.7 & data$PHOSPHORUS[i] <= 4.5){
      data$PHOSPHORUS[i] = 0
    }
    else if(data$PHOSPHORUS[i] < 2.7) {
      data$PHOSPHORUS[i] = abs(data$PHOSPHORUS[i] - 2.7)
    }
    else{
      data$PHOSPHORUS[i] = data$PHOSPHORUS[i] - 4.5
    }
    
    if(data$CHOL[i] >= 0 & data$CHOL[i] <= 200){
      data$CHOL[i] = 0
    }
    else{
      data$CHOL[i] = data$CHOL[i] - 200
    }
    
    if(data$HDL[i] >= 60){
      data$HDL[i] = 0
    }
    else{
      data$HDL[i] = abs(data$HDL[i] - 60)
    }
    
    if(data$TG[i] >= 0 & data$TG[i] <= 150){
      data$TG[i] = 0
    }
    else{
      data$TG[i] = data$TG[i] - 150
    }
    
    if(data$LDL[i] >= 0 & data$LDL[i] < 130){
      data$LDL[i] = 0
    }
    else{
      data$LDL[i] = data$LDL[i] - 130
    }
    
    if(data$GLU[i] >= 0 & data$GLU[i] < 100){
      data$GLU[i] = 0
    }
    else{
      data$GLU[i] = data$GLU[i] - 100
    }
    
    if(data$RBC[i] >= 3.7 & data$RBC[i] <= 4.7){
      data$RBC[i] = 0
    }
    else if(data$RBC[i] < 3.7){
      data$RBC[i] = abs(data$RBC[i] - 3.7)
    }
    else{
      data$RBC[i] = data$RBC[i] - 4.7
    }
    
    if(data$HCT[i] >= 33 & data$HCT[i] <= 45){
      data$HCT[i] = 0
    }
    else if(data$HCT[i] < 33){
      data$HCT[i] = abs(data$HCT[i] - 33)
    }
    else{
      data$HCT[i] = data$HCT[i] - 45
    }
    
    if(data$HB[i] >= 12 & data$HB[i] <= 15.5){
      data$HB[i] = 0
    }
    else if(data$HB[i] < 12){
      data$HB[i] = abs(data$HB[i] - 12)
    }
    else{
      data$HB[i] = data$HB[i] - 15.5
    }
  } ## end for
  return (data)
}

bio_female<-woman(bio_female)
bio_female$FVCPP = NULL
bio_female$FEV1_FVC = NULL



#### female 변수별 정상 기준으로부터 거리 구하기 Part2 ####
n<-nrow(bio_female)

for( i in c(1:n)){
  bio_female$MCV[i]<-abs(median(c(bio_female$MCV[i],81,100))-bio_female$MCV[i])}
for( i in c(1:n)){
  bio_female$MCH[i]<-abs(median(c(bio_female$MCH[i],28,32))-bio_female$MCH[i])}
for( i in c(1:n)){
  bio_female$MCHC[i]<-abs(median(c(bio_female$MCHC[i],31,35))-bio_female$MCHC[i])}
for( i in c(1:n)){
  bio_female$ESR[i]<-abs(median(c(bio_female$ESR[i],0,25))-bio_female$ESR[i])}
for( i in c(1:n)){
  bio_female$PLAT[i]<-abs(median(c(bio_female$PLAT[i],150,370))-bio_female$PLAT[i])}
for( i in c(1:n)){
  bio_female$PDW[i]<-abs(median(c(bio_female$PDW[i],8.8,16.5))-bio_female$PDW[i])}
for( i in c(1:n)){
  bio_female$NEUTROPHIL[i]<-abs(median(c(bio_female$NEUTROPHIL[i],50,70))-bio_female$NEUTROPHIL[i])}
for( i in c(1:n)){
  bio_female$LYMPHOCYTE[i]<-abs(median(c(bio_female$LYMPHOCYTE[i],20,50))-bio_female$LYMPHOCYTE[i])}
for( i in c(1:n)){
  bio_female$MONOCYTE[i]<-abs(median(c(bio_female$MONOCYTE[i],4,12))-bio_female$MONOCYTE[i])}
for( i in c(1:n)){
  bio_female$EOSINOPHIL[i]<-abs(median(c(bio_female$EOSINOPHIL[i],0,7))-bio_female$EOSINOPHIL[i])}
for( i in c(1:n)){
  bio_female$BASOPHIL[i]<-abs(median(c(bio_female$BASOPHIL[i],0,1.5))-bio_female$BASOPHIL[i])}
for( i in c(1:n)){
  bio_female$TSH[i]<-abs(median(c(bio_female$TSH[i],0.27,4.20))-bio_female$TSH[i])}
for( i in c(1:n)){
  bio_female$FT4[i]<-abs(median(c(bio_female$FT4[i],0.93,1.7))-bio_female$FT4[i])}
for( i in c(1:n)){
  bio_female$T3[i]<-abs(median(c(bio_female$T3[i],0.8,2.0))-bio_female$T3[i])}
for( i in c(1:n)){
  bio_female$CRP[i]<-abs(median(c(bio_female$CRP[i],0,10))-bio_female$CRP[i])}
for( i in c(1:n)){
  bio_female$CEA[i]<-abs(median(c(bio_female$CEA[i],0,3.8))-bio_female$CEA[i])}
for( i in c(1:n)){
  bio_female$AFP[i]<-abs(median(c(bio_female$AFP[i],0,7))-bio_female$AFP[i])}
for( i in c(1:n)){
  bio_female$CA125[i]<-abs(median(c(bio_female$CA125[i],0,35))-bio_female$CA125[i])}
for( i in c(1:n)){
  bio_female$CA199[i]<-abs(median(c(bio_female$CA199[i],0,34))-bio_female$CA199[i])}
for( i in c(1:n)){
  bio_female$PSA[i]<-abs(median(c(bio_female$PSA[i],0,4))-bio_female$PSA[i])}
for( i in c(1:n)){
  bio_female$UPH[i]<-abs(median(c(bio_female$UPH[i],4.8,7.8))-bio_female$UPH[i])}
for( i in c(1:n)){
  bio_female$USG[i]<-abs(median(c(bio_female$USG[i],1.003,1.030))-bio_female$USG[i])}

bio_female$HBSAG<-as.character(bio_female$HBSAG)
bio_female$HBSAG[which(bio_female$HBSAG=='Negative')]<-0
bio_female$HBSAG[which(bio_female$HBSAG=='WeakPositive')]<-1
bio_female$HBSAG[which(bio_female$HBSAG=='Positive')]<-2
bio_female$HBSAG<-as.numeric(bio_female$HBSAG)

bio_female$HBSAB<-as.character(bio_female$HBSAB)
bio_female$HBSAB[which(bio_female$HBSAB=='Negative')]<-2
bio_female$HBSAB[which(bio_female$HBSAB=='Equivocal')]<-1
bio_female$HBSAB[which(bio_female$HBSAB=='Positive')]<-0
bio_female$HBSAB<-as.numeric(bio_female$HBSAB)

bio_female$RF<-as.character(bio_female$RF)
bio_female$RF[which(bio_female$RF=='Negative')]<-0
bio_female$RF[which(bio_female$RF=='Trace')]<-1
bio_female$RF[which(bio_female$RF=='Positive')]<-2
bio_female$RF<-as.numeric(bio_female$RF)

bio_female$ANTIHCVAB<-as.character(bio_female$ANTIHCVAB)
bio_female$ANTIHCVAB[which(bio_female$ANTIHCVAB=='Negative')]<-0
bio_female$ANTIHCVAB[which(bio_female$ANTIHCVAB=='Equivocal')]<-1
bio_female$ANTIHCVAB[which(bio_female$ANTIHCVAB=='Positive')]<-2
bio_female$ANTIHCVAB<-as.numeric(bio_female$ANTIHCVAB)

bio_female$UNITR<-as.character(bio_female$UNITR)
bio_female$UNITR[which(bio_female$UNITR=='Negative')]<-0
bio_female$UNITR[which(bio_female$UNITR=='Trace')]<-1
bio_female$UNITR[which(bio_female$UNITR=='Positive')]<-2
bio_female$UNITR<-as.numeric(bio_female$UNITR)

bio_female$UPRO<-as.character(bio_female$UPRO)
bio_female$UPRO[which(bio_female$UPRO=='Negative')]<-0
bio_female$UPRO[which(bio_female$UPRO=='Trace')]<-1
bio_female$UPRO[which(bio_female$UPRO=='1Positive')]<-2
bio_female$UPRO[which(bio_female$UPRO=='2Positive')]<-3
bio_female$UPRO[which(bio_female$UPRO=='3Positive')]<-4
bio_female$UPRO[which(bio_female$UPRO=='4Positive')]<-5
bio_female$UPRO<-as.numeric(bio_female$UPRO)

bio_female$UGLU<-as.character(bio_female$UGLU)
bio_female$UGLU[which(bio_female$UGLU=='Negative')]<-0
bio_female$UGLU[which(bio_female$UGLU=='Trace')]<-1
bio_female$UGLU[which(bio_female$UGLU=='1Positive')]<-2
bio_female$UGLU[which(bio_female$UGLU=='2Positive')]<-3
bio_female$UGLU[which(bio_female$UGLU=='3Positive')]<-4
bio_female$UGLU[which(bio_female$UGLU=='4Positive')]<-5
bio_female$UGLU<-as.numeric(bio_female$UGLU)

bio_female$UKET<-as.character(bio_female$UKET)
bio_female$UKET[which(bio_female$UKET=='Negative')]<-0
bio_female$UKET[which(bio_female$UKET=='Trace')]<-1
bio_female$UKET[which(bio_female$UKET=='1Positive')]<-2
bio_female$UKET[which(bio_female$UKET=='2Positive')]<-3
bio_female$UKET[which(bio_female$UKET=='3Positive')]<-4
bio_female$UKET[which(bio_female$UKET=='4Positive')]<-5
bio_female$UKET<-as.numeric(bio_female$UKET)

bio_female$URO<-as.character(bio_female$URO)
bio_female$URO[which(bio_female$URO=='Negative')]<-0
bio_female$URO[which(bio_female$URO=='Trace')]<-1
bio_female$URO[which(bio_female$URO=='Positive')]<-2
bio_female$URO<-as.numeric(bio_female$URO)

bio_female$UBIL<-as.character(bio_female$UBIL)
bio_female$UBIL[which(bio_female$UBIL=='Negative')]<-0
bio_female$UBIL[which(bio_female$UBIL=='Trace')]<-1
bio_female$UBIL[which(bio_female$UBIL=='Positive')]<-2
bio_female$UBIL<-as.numeric(bio_female$UBIL)

bio_female$UBLD<-as.character(bio_female$UBLD)
bio_female$UBLD[which(bio_female$UBLD=='Negative')]<-0
bio_female$UBLD[which(bio_female$UBLD=='Trace')]<-1
bio_female$UBLD[which(bio_female$UBLD=='1Positive')]<-2
bio_female$UBLD[which(bio_female$UBLD=='2Positive')]<-3
bio_female$UBLD[which(bio_female$UBLD=='3Positive')]<-4
bio_female$UBLD[which(bio_female$UBLD=='4Positive')]<-5
bio_female$UBLD<-as.numeric(bio_female$UBLD)



###### 히스토그램을 바탕으로delete outlier obs ####
bio_female = bio_female[-c(3391,20,2503,1916,5471,4148,5049,6165,1557,1154,1282,1079,1525,1641,710,701,1411),]

### minmax scaling #####
maxValue<- apply(bio_female[,-c(1,2,3)], 2, max)
minValue<- apply(bio_female[,-c(1,2,3)], 2, min)
fs<-as.data.frame(scale(bio_female[,-c(1,2,3)], center= minValue, scale=(maxValue-minValue))) 
bio_female_s<-cbind(bio_female[,c(1,2,3)],fs)



###### hierarchical clust ######

dat1 = bio_female_s

dist_data <- dist(t(dat1[,-c(1,2,3)]))
hc_a <- hclust(dist_data, method ="ward.D")


library(ggdendro)
ggdendrogram(hc_a, hang_height = 0.01)
require("igraph")
fviz_dend(hc_a, k = 1, k_colors = "jco",
          type = "phylogenic", repel = TRUE)

#### group 1,2,3,4,5의 거리를 구하는 함수 ####

group1 = function(data){
  core = rep(0,19)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$GGT[i], data$AST[i], data$ALT[i], data$CHLORIDE[i],
            data$TOTPRO[i], data$BASOPHIL[i] ,data$CREA[i] , data$USG[i]
            , data$CEA[i] , data$CA199[i] , data$FEV1PP[i] , data$AFP[i], data$EOSINOPHIL[i],
            data$TSH[i], data$FT4[i], data$ALB[i], data$PLAT[i], data$BMP[i] , data$CA125[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}

group2 = function(data){
  core = rep(0,10)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$UBIL[i], data$CALCIUM[i], data$BUN[i], data$URICACID[i],
            data$GLU[i], data$UGLU[i], data$TOTBIL[i], data$MONOCYTE[i], data$ALP[i], 
            data$UPH[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}


group3 = function(data){
  core = rep(0,8)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$HCT[i], data$MCV[i], data$MCHC[i], data$HB[i], data$MCH[i]
            , data$HBSAG[i], data$HDL[i], data$fvc[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}


group4 = function(data){
  core = rep(0,18)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$UKET[i], data$POTASSIUM[i], data$SODIUM[i], data$PHOSPHORUS[i],
            data$UNITR[i], data$ANTIHCVAB[i], data$UPRO[i], data$RBC[i],
            data$LDH[i], data$LYMPHOCYTE[i] ,data$NEUTROPHIL[i], data$CHOL[i], data$LDL[i],
            data$SBP[i], data$DBP[i], data$TG[i], data$BFP[i],
            data$BMI[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}


group5 = function(data){
  core = rep(0,4)
  result = NULL
  for( i in 1:nrow(data)){
    vec = c(data$RF[i], data$HBSAB[i], data$URO[i], data$UBLD[i])
    distance = dist(rbind(core,vec))
    result = c(result, distance)
  }
  return(result)
}



#### score를 변수로 추가해줌 ####
test = bio_female_s
score = group1(test)+ group2(test)+ group3(test)+ group4(test)+ group5(test)
test = cbind(test, score)


## 상위 49개(1%이하) -> 극단적인 값 (이상치) 삭제
sum(test$score>=3.7)
test = test[test$score<3.7,]


## score를 -10 ~ 10 사이로 스케일링 #####
maxValue = max(test$score)
minValue = min(test$score)
test$score = scale(test$score, center = minValue, scale=(maxValue-minValue))
test$score = test$score*20-10

### 선형회귀 모델 적용 ###
fit =lm(score~., data= test[,-c(1,2,3)])
both <- step(fit, direction="both", trace= F)
summary(both)



