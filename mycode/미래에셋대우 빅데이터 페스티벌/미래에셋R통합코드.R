## Library
library(dplyr)
library('rvest')
library(stringr)
library(tm)
library(TestDataImputation)
library(data.table)
library(topicmodels)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(NLP)
library(SnowballC)
library(stats)

###########################
#### Calculate COSPI30 ####
###########################

#COSPI30 작성코드

stock1<-read.csv('stock01.csv',header=T)
stock2<-read.csv('stock02.csv',header=T)
stock3<-read.csv('stock03.csv',header=T)
stock4<-read.csv('stock04.csv',header=T)

stock<-rbind(stock1,stock2,stock3,stock4)

### 화장품 생산 30개 기업 추출한 후 상장주식수 입력해주기
##날짜, 종목명, 종가만 추출
#각종목별 상장주식수 입력해주기
LG생활건강= stock[which(stock$종목명=='LG생활건강'),]
LG생활건강=as.data.frame( LG생활건강[,c(1,2,6)])
LG생활건강$상장주식수=15618197

동성제약= stock[which(stock$종목명=='동성제약'),]
동성제약=as.data.frame( 동성제약[,c(1,2,6)])
동성제약$상장주식수=24756574

아모레퍼시픽= stock[which(stock$종목명=='아모레퍼시픽 '),]
아모레퍼시픽=as.data.frame( 아모레퍼시픽[,c(1,2,6)])
아모레퍼시픽$상장주식수=58458490

아모레G= stock[which(stock$종목명=='아모레G'),]
아모레G=as.data.frame( 아모레G[,c(1,2,6)])
아모레G$상장주식수=82458180

에이블씨엔씨= stock[which(stock$종목명=='에이블씨엔씨'),]
에이블씨엔씨=as.data.frame( 에이블씨엔씨[,c(1,2,6)])
에이블씨엔씨$상장주식수=24823163

잇츠한불= stock[which(stock$종목명=='잇츠한불'),]
잇츠한불=as.data.frame( 잇츠한불[,c(1,2,6)])
잇츠한불$상장주식수=21929315

제이준코스메틱= stock[which(stock$종목명=='제이준코스메틱'),]
제이준코스메틱=as.data.frame( 제이준코스메틱[,c(1,2,6)])
제이준코스메틱$상장주식수=26520586

코스맥스= stock[which(stock$종목명=='코스맥스'),]
코스맥스=as.data.frame( 코스맥스[,c(1,2,6)])
코스맥스$상장주식수=10049509

코스맥스비티아이= stock[which(stock$종목명=='코스맥스비티아이 주식회사 '),]
코스맥스비티아이=as.data.frame( 코스맥스비티아이[,c(1,2,6)])
코스맥스비티아이$상장주식수=9603921

토니모리= stock[which(stock$종목명=='토니모리'),]
토니모리=as.data.frame( 토니모리[,c(1,2,6)])
토니모리$상장주식수=17640000

한국콜마= stock[which(stock$종목명=='한국콜마'),]
한국콜마=as.data.frame( 한국콜마[,c(1,2,6)])
한국콜마$상장주식수=21104461

한국콜마홀딩스= stock[which(stock$종목명=='한국콜마홀딩스'),]
한국콜마홀딩스=as.data.frame( 한국콜마홀딩스[,c(1,2,6)])
한국콜마홀딩스$상장주식수=17938966

한국화장품= stock[which(stock$종목명=='한국화장품'),]
한국화장품=as.data.frame( 한국화장품[,c(1,2,6)])
한국화장품$상장주식수=16068000


씨에스에이코스믹= stock[which(stock$종목명=='씨에스에이코스믹'),]
씨에스에이코스믹=as.data.frame( 씨에스에이코스믹[,c(1,2,6)])
씨에스에이코스믹$상장주식수=19747641

SK바이오랜드= stock[which(stock$종목명=='SK바이오랜드'),]
SK바이오랜드=as.data.frame( SK바이오랜드[,c(1,2,6)])
SK바이오랜드$상장주식수=15000000

네오팜= stock[which(stock$종목명=='네오팜'),]
네오팜=as.data.frame( 네오팜[,c(1,2,6)])
네오팜$상장주식수=7461237

대봉엘에스= stock[which(stock$종목명=='대봉엘에스'),]
대봉엘에스=as.data.frame( 대봉엘에스[,c(1,2,6)])
대봉엘에스$상장주식수=11086579

리더스코스메틱= stock[which(stock$종목명=='리더스코스메틱'),]
리더스코스메틱=as.data.frame( 리더스코스메틱[,c(1,2,6)])
리더스코스메틱$상장주식수=18007078

에이씨티= stock[which(stock$종목명=='에이씨티'),]
에이씨티=as.data.frame( 에이씨티[,c(1,2,6)])
에이씨티$상장주식수=20822921

연우= stock[which(stock$종목명=='연우'),]
연우=as.data.frame( 연우[,c(1,2,6)])
연우$상장주식수=12398000

제닉= stock[which(stock$종목명=='제닉'),]
제닉=as.data.frame( 제닉[,c(1,2,6)])
제닉$상장주식수=7000000

케어젠= stock[which(stock$종목명=='케어젠'),]
케어젠=as.data.frame( 케어젠[,c(1,2,6)])
케어젠$상장주식수=10743000

케이씨아이= stock[which(stock$종목명=='케이씨아이'),]
케이씨아이=as.data.frame( 케이씨아이[,c(1,2,6)])
케이씨아이$상장주식수=11270000

코리아나화장품= stock[which(stock$종목명=='코리아나화장품'),]
코리아나화장품=as.data.frame( 코리아나화장품[,c(1,2,6)])
코리아나화장품$상장주식수=40000000

코스메카코리아= stock[which(stock$종목명=='코스메카코리아'),]
코스메카코리아=as.data.frame( 코스메카코리아[,c(1,2,6)])
코스메카코리아$상장주식수=10680000

코스온= stock[which(stock$종목명=='코스온'),]
코스온=as.data.frame( 코스온[,c(1,2,6)])
코스온$상장주식수=10680000

클리오= stock[which(stock$종목명=='클리오'),]
클리오=as.data.frame( 클리오[,c(1,2,6)])
클리오$상장주식수=16958895

에스디생명공학= stock[which(stock$종목명=='에스디생명공학'),]
에스디생명공학=as.data.frame( 에스디생명공학[,c(1,2,6)])
에스디생명공학$상장주식수=22415066

아우딘퓨쳐스= stock[which(stock$종목명=='아우딘퓨쳐스'),]
아우딘퓨쳐스= as.data.frame(아우딘퓨쳐스[,c(1,2,6)]) 
아우딘퓨쳐스$상장주식수=10000000 

cosmetic<-rbind(LG생활건강,동성제약,아모레퍼시픽,아모레G,에이블씨엔씨,잇츠한불,
                제이준코스메틱,코스맥스,코스맥스비티아이,토니모리,한국콜마,
                한국콜마홀딩스,한국화장품,씨에스에이코스믹,SK바이오랜드,네오팜,대봉엘에스,
                리더스코스메틱,에이씨티,연우,제닉,케어젠,케이씨아이,코리아나화장품,
                코스메카코리아,코스온,클리오,에스디생명공학,아우딘퓨쳐스)


#이제 cosmetic에는 30개 화장품기업의 종가, 상장주식수가 담겨있다
#시가총액 column을 생성해주자

cosmetic$시가총액 <- 0 
cosmetic$시가총액 <- (cosmetic$종가)*(cosmetic$상장주식수)



###시가총액 계산

### 기준시점 ( 2010년 1월 4일)시가총액 작성 
cosmetic_first<- cosmetic[which(cosmetic$날짜==20100104),]
standard_price<-sum(cosmetic_first$시가총액) 
#standard_price는 기준시점의 시가총액이다



cosmetic_new<-cosmetic[which(cosmetic$날짜>=20150615),]  ##2015년6월15일 이후 주가들 
cosmetic_new$COSPI30 = 0

date<-as.vector(unique(cosmetic_new$날짜))  # 2015년 6월15일부터 2018년 6월29일 까지의 날짜입력

cosmetic_final<- as.data.frame(date)
cosmetic_final$COSPI30 = 0

#날짜별 시가총액을 작성해주는 loop문 작서
for(i in date){
  a<-cosmetic_new[which(cosmetic_new$날짜==i),]
  b<-sum(a$시가총액)  ##b는 그날의 시가총액 
  cosmetic_final[which(cosmetic_final$date==i),]$COSPI30<-(b/standard_price)*100
}


colnames(cosmetic_final) = c('날짜', 'COSPI30')


write.csv(cosmetic_final,'COSPI30.csv',row.names=F)

#########################
### News Web Scraping ###
#########################

## Get url
link <- matrix(NA,880,1) #페이지 수 = 880
link<-as.data.frame(link)
colnames(link)<-'page url'
for(i in c(1:880)){
  a<-'http://www.koreaherald.com/search/index.php?q=china&sort=1&mode=list&np='
  b<-i 
  c<-paste0(a,b)
  link[i,1]<-c
} 
#write.csv(link,'China_link.csv')

url_list<-vector(880,mode='list')

for ( i in c(1:880)){
  url<-link[i,1]
  bugs<-read_html(url, encoding =  'UTF-8')
  title1<-html_nodes( bugs , css = '.mb31')
  title2<-html_nodes( bugs , css = '.mb50')
  Title<-c(as.character(title1),as.character(title2))
  lis<-strsplit(Title,'"')
  
  link_list<-character(15)  # 880개 페이지 하나 당 15개의 뉴스가 있었음 #
  for ( j in c(1:15)){
    l<-paste0('http://www.koreaherald.com',lis[[j]][4])
    link_list[j]<-l
  }
  url_list[[i]]<-link_list
  
}

url_data_frame<-data.frame(url=unlist(url_list)) #; write.csv(url_data_frame,'koreaherald_url.csv')

## Web Scraping

url_data_frame<-read.csv('koreaherald_url.csv',stringsAsFactors = F)

//*[@id="articleText"]
/html/body/div[1]/div[3]/div[3]/div/div[1]/div[3]

/html/body/div[1]/div[3]/div[3]/div/div[1]/div[3]/ul
/html/body/div[1]/div[3]/div[3]/div/div[1]/div[2]/ul
//*[@id="articleText"]

url1<-url_data_frame$url

b<-matrix(NA,length(url1),2)
b<-as.data.frame(b)
colnames(b)<- c('날짜','text')
b$text<-as.character(b$text)
for (i in c(1:length(url1))){
  url<-url1[i]
  bugs<-read_html(url, encoding =  'UTF-8')
  title<-html_nodes( bugs , xpath = '///*[@id="articleText"]')
  title_text<-str_replace_all(title %>% html_text(), '\n', '' )
  b$text[i]<-title_text
  ifelse( length(html_nodes( bugs , xpath = '//html/body/div[1]/div[3]/div[3]/div/div[1]/div[3]/ul'))>0, when<-html_nodes( bugs , xpath = '//html/body/div[1]/div[3]/div[3]/div/div[1]/div[3]/ul'),
          when<-html_nodes( bugs , xpath = '//html/body/div[1]/div[3]/div[3]/div/div[1]/div[2]/ul'))
  when_text<-str_replace_all(when %>% html_text(), '\t', '' )
  b$날짜[i]<-when_text
}
str(url1)


b<-read.csv('koreaherald_text_final.csv')

str(b)
b$category<-rep(0,nrow(b))


for (i in c(1:nrow(b))){
  url<-url1[i]
  bugs<-read_html(url, encoding =  'UTF-8')
  
  if(length(html_nodes( bugs , xpath = '//html/body/div[1]/div[3]/div[3]/div/div[1]/ul[1]'))>0){
    cate<-html_nodes( bugs , xpath = '//html/body/div[1]/div[3]/div[3]/div/div[1]/ul[1]') 
    cate_text<-str_replace_all(cate %>% html_text(), '\t', '' )
  }else(cate_text<-'a')
  
  b$category[i]<-cate_text
}


#write.csv(c,'koreaherald_text2.csv')

koreaherald_text<-read.csv('koreaherald_text.csv')

koreaherald_text$날짜2<-substr(koreaherald_text$날짜[1:nrow(koreaherald_text)],13,25)

koreaherald_text$날짜<-NULL
colnames(koreaherald_text)[2]<-'날짜'

write.csv(koreaherald_text,'koreaherald_text2-3.csv',row.names = F)
koreaherald_text2<-read.csv('koreaherald_text2-3.csv',stringsAsFactors = F)
#colnames(koreaherald_text2)[1]<-'날짜'

idxk<-which(koreaherald_text2$날짜 >20150614 & koreaherald_text2$날짜<=20180629)
koreaherald_text<-koreaherald_text2[idxk,]

koreaherald_text$날짜<-as.factor(koreaherald_text$날짜)

endn<-max(table(koreaherald_text$날짜))

levellist<-levels(koreaherald_text$날짜)

km<-matrix(NA,length(levellist),endn)
km<-as.data.frame(km)
str(km)

for( i in 1:length(levellist)){
  idx<-which(koreaherald_text$날짜==levellist[i])
  
  km[i,][c(1:length(idx))]<-koreaherald_text$text[idx]
  
  
}

km$날짜<- levellist
write.csv(km,'km_final.csv',row.names = F)

########################
### News Text Mining ###
########################

## Read & Organize Data

km<-read.csv('km_final.csv',stringsAsFactors = F)

kor<-matrix(NA,nrow(km),2)
kor<-data.frame(kor)
colnames(kor)<-c('date','text')
for ( i in c(1:nrow(km)) ){
  kor$date[i]<-km$date[i]
  kor$text[i] <-paste(km[i,1],km[i,2],km[i,3])
  
}

##
Amore <- stock[stock$종목명=="아모레퍼시픽 ",]
Amore <- Amore[,c(2,6)]
Amorefin <- Amore[Amore$날짜>20150614,]
Amorefin <- Amorefin[Amorefin$날짜<20180630,]

date1 <- as.data.frame(Amorefin$날짜)
colnames(date1) <- "date"
kordate <- merge(date1, kor, all.x=T, by="date")
kor <- kordate

article <- kor$text

n_article <- length(article)                 #Check the number of app descriptions
n_article

## CTM
######

## Corpusing

M.article <- Corpus(VectorSource(article)) 

## Remove NA
for (j in 1:n_article) M.article[[j]] <- gsub(" NA", "", M.article[[j]]) #NA
for (j in 1:n_article) M.article[[j]] <- gsub("\t", "", M.article[[j]]) #\t

## Lowercase
M.article <- tm_map(M.article, content_transformer(tolower))

## Remove redundant words
for (j in 1:n_article) M.article[[j]] <- gsub("trump", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("korea", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("find", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("china", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("said", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("will", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("percent", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("also", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("chinese", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("year", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("president", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("kim", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("new", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("two", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("seoul", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("last", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("one", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("years", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("including", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("time", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("according", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("park", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("since", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("koreas", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("many", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("now", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("made", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("make", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("take", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("like", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("well", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("north", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("south", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("moon", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("country", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("countries", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("even", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("may", " ", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("hong kong", "hongkong", M.article[[j]])
for (j in 1:n_article) M.article[[j]] <- gsub("lee", " ", M.article[[j]])

## Remove slashes
for (j in 1:n_article) M.article[[j]] <- gsub("\'", "", M.article[[j]])

## Remove special characters
M.article <- tm_map(M.article, removePunctuation)
for (j in 1:n_article) M.article[[j]] <- gsub("['*|&|-|/|\\|()|\\.,!-_?ӡ???????]", " ", M.article[[j]]) 
for (j in 1:n_article) M.article[[j]] <- gsub("¡¯", " ", M.article[[j]]) 
for (j in 1:n_article) M.article[[j]] <- gsub("¡±", " ", M.article[[j]]) 
for (j in 1:n_article) M.article[[j]] <- gsub("¡°", " ", M.article[[j]]) 
for (j in 1:n_article) M.article[[j]] <- gsub("\"", " ", M.article[[j]]) 

## Remove numbers
M.article <- tm_map(M.article, removeNumbers)

## Remove stopwords
for (i in 1:n_article){M.article[[i]]<-removeWords(as.character(M.article[[i]]), stopwords("en"))}
newstopwords <-c("and", "for", "the", "to", "in", "when", "then", "he", "she", "than", "can");
M.article <- tm_map(M.article, removeWords, newstopwords)

## Stemming
M.article <- tm_map(M.article, stemDocument)

## Remove blank spaces
M.article <- tm_map(M.article, stripWhitespace)

##
dtm_M.article <- DocumentTermMatrix(M.article)

Freq_term1 <-colSums(as.matrix(dtm_M.article))
Order_Freq_term1 <- order(Freq_term1, decreasing = TRUE)
head(Freq_term1[Order_Freq_term1])

summary(col_sums(dtm_M.article))

## Calculate tf-idf values
term_tfidf <- tapply(dtm_M.article$v/row_sums(dtm_M.article)[dtm_M.article$i], dtm_M.article$j,mean) * log2(nDocs(dtm_M.article)/col_sums(dtm_M.article>0))
summary(term_tfidf)

term_tfidf

dtm_M.article_left <- dtm_M.article[,term_tfidf>=0.02]
dtm_M.article_left <- dtm_M.article_left[row_sums(dtm_M.article_left) > 0, ]
summary(col_sums(dtm_M.article_left)) 


Freq_term5 <-colSums(as.matrix(dtm_M.article_left))
Order_Freq_term5 <- order(Freq_term5, decreasing = TRUE)
head(Freq_term5[Order_Freq_term5])

## CTM Topic Modeling
ctm.out <- CTM(dtm_M.article_left,control=list(seed=11),k=3)
topicmodels::terms(ctm.out,10)

## Check Probability
posterior_ctm <- topicmodels::posterior(ctm.out)
ctm.topics <- data.frame(posterior_ctm$topics)
round(ctm.topics,3)
CTModel <- round(ctm.topics,3)
write.csv(CTModel,"NEWSTEXT_CTM.csv")

## Opinion Lexicon
##################

## Corpusing
M.article <- Corpus(VectorSource(article)) 

## Remove NA
for (j in 1:n_article) M.article[[j]] <- gsub(" NA", "", M.article[[j]]) #NA
for (j in 1:n_article) M.article[[j]] <- gsub("\t", "", M.article[[j]]) #\t

## Lowercase
M.article <- tm_map(M.article, content_transformer(tolower))

## Remove slashes
for (j in 1:n_article) M.article[[j]] <- gsub("\'", "", M.article[[j]])

## Remove special characters
M.article <- tm_map(M.article, removePunctuation)
for (j in 1:n_article) M.article[[j]] <- gsub("['*|&|-|/|\\|()|\\.,!-_?ӡ???????]", " ", M.article[[j]]) 
for (j in 1:n_article) M.article[[j]] <- gsub("¡¯", " ", M.article[[j]]) 
for (j in 1:n_article) M.article[[j]] <- gsub("¡±", " ", M.article[[j]]) 
for (j in 1:n_article) M.article[[j]] <- gsub("¡°", " ", M.article[[j]]) 
for (j in 1:n_article) M.article[[j]] <- gsub("\"", " ", M.article[[j]]) 

## Remove numbers
M.article <- tm_map(M.article, removeNumbers)

## Remove stopwords
for (i in 1:n_article){M.article[[i]]<-removeWords(as.character(M.article[[i]]), stopwords("en"))}
newstopwords <-c("and", "for", "the", "to", "in", "when", "then", "he", "she", "than", "can");
M.article <- tm_map(M.article, removeWords, newstopwords)

## Remove blank spaces
M.article <- tm_map(M.article, stripWhitespace)

dtm_M.article <- DocumentTermMatrix(M.article)

## Opinion lexicon dictionary
get_sentiments('bing')
oplex <- data.frame(get_sentiments('bing'))
table(oplex$sentiment)

## Set as dataframe
sen_txt <- c(rep(NA), 745)
for(i in 1:745){
  sen_txt[i] <- as.character(M.article[[i]][1])
}

sen_article_txt <- data_frame(id=1:745, doc=sen_txt)
sen_article_txt
str(sen_article_txt)

sen_article_txt_word <- sen_article_txt %>%
  unnest_tokens(word,doc)

## Merge data and dictionary
result.sa <- sen_article_txt_word %>% inner_join(get_sentiments('bing'))
result.sa <- result.sa %>% count(word,id,sentiment)
result.sa <- result.sa %>% spread(sentiment, n, fil=0)

Oplex <- summarise(group_by(result.sa, id),
                   pos.sum = sum(positive),
                   neg.sum = sum(negative),
                   pos.sent = pos.sum-neg.sum)

write.csv(Oplex,"NEWSTEXT_Oplex.csv")


############################
### Youtube Web Scraping ###
############################

## Example with Risabae
RISABAE_url<-read.csv('RISABAE_url.csv',stringsAsFactors = F)
str(RISABAE_url)

url1<-RISABAE_url$Video.URL

a<-matrix(NA,length(url1),2)
a<-as.data.frame(a)
colnames(a)<-c('Number', 'text')
a$Number<-c(1:length(url1))

for (i in c(1:length(url1))){
  url<-url1[i]
  bugs<-read_html(url, encoding =  'UTF-8')
  title<-html_nodes( bugs ,xpath = '///*[@id="content"]')
  title_text<-str_replace_all(title %>% html_text(), '\n', '' )
  a$text[i]<-title_text
}
str(a)

# write.csv(a,'RISABAE make up tutorials.csv',row.names = F)
#a<-read.csv('RISABAE make up tutorials.csv')

## Get overview
b<-matrix(NA,nrow(a),3)
b<-as.data.frame(b)
colnames(b)<-c('날짜','overview', '조회수')

for (i in c(1:length(url1))){
  start_num<-regexpr('게시일', a$text[i] )[1]
  end_num<-regexpr('간략히', a$text[i] )[1]
  b$overview[i]<-substr(a$text[i],start_num,end_num)
  
  start_num2<-regexpr('조회수', a$text[i] )[1]+4
  end_num2<-regexpr('조회수', a$text[i] )[1]+14
  bb<-substr(a$text[i],start_num2,end_num2)
  idxb<-gregexpr('회',bb )[[1]][1]
  b$조회수[i]<-substr(bb,1,idxb-1)
  
  start_num3<-regexpr('게시일', a$text[i] )[1]
  end_num3<-regexpr('게시일', a$text[i] )[1]+18
  dd<-substr(a$text[i],start_num3,end_num3)
  idx<-gregexpr('\\.',dd )[[1]][1]
  idx2<-gregexpr('\\.',dd )[[1]][3]
  b$날짜[i]<-substr(dd,idx-4,idx2)
  
}

## Remove NA rows
b<-Listwise(b,Mvalue = "NA") 

## To lowercase
b$overview<- tolower(b$overview)

#write.csv(b,'RISABAE_overview.csv',row.names = F)
#b<-read.csv('RISABAE_overview.csv')

## Amore Pacific review frequency
#################################

Amore_list<-read.csv('아모레퍼시픽 자회사 list.csv',stringsAsFactors = F, header=F)

Amore_number<-numeric(nrow(b))

for ( i in 1:nrow(b)){
  bla<-NULL
  k<-unlist(strsplit(b$overview[i],' '))
  
  for (j in 1:nrow(Amore_list)){
    le = max(length(grep(Amore_list[j,1],k)),length(grep(Amore_list[j,2],k)))
    if (le>=10) le=10  
    bla <- c(bla,le)
  }
  Amore_number[i]<-sum(bla)
  
}
b$Number <- Amore_number
#write.csv(b,'RISABAE_overview.csv')

## LG review frequency
######################

b<-read.csv('RISABAE_overview.csv',stringsAsFactors = F)
b$Number<-NULL

LG_list<-read.csv('LG생활건강 list.csv',stringsAsFactors = F)

LG_number<-numeric(nrow(b))
for (i in 1:nrow(b)){
  bla<-NULL
  k<-unlist(strsplit(b$overview[i],' '))
  
  for (j in 1:nrow(LG_list)){
    le = max(length(grep(LG_list[j,1],k)),length
             (grep(LG_list[j,2],k)),length(grep(LG_list[j,3],k)),length(grep(LG_list[j,4],k))) 
    if (le>=10) le=10
    bla <- c(bla,le)
  }
  LG_number[i]<-sum(bla)
  
}

b$Number <-LG_number

#write.csv(b,'Risabae_LG_overview.csv',row.names = F)


################################
### Calculate Weighted Views ###
################################

## Get dates
filefile <- read.csv("Youtube_view.csv")
daterow <- as.data.frame(filefile$날짜)
colnames(daterow) = "날짜"
glimpse(daterow)

## Read files
daddoa <- read.csv("Daddoa_LG_overview.csv")
hanbyul <- read.csv("hanbyul_LG_overview.csv")
lamuqe <- read.csv("lamuqe_LG_overview.csv")
ssinnim <- read.csv("ssinnim_LG_overview.csv")
sunny <- read.csv("sunny_LG_overview.csv")
risabae <- read.csv("Risabae_LG_overview.csv")
pony <- read.csv("pony_LG_overview.csv")
yoonjja <- read.csv("yoonjja_LG_overview.csv")
yeondoo <- read.csv("yeondu_LG_overview.csv")

## Sum videos uploaded on the same day
daddoa <- daddoa %>% group_by(날짜) %>% summarize(daddoa=sum(Number))
hanbyul <- hanbyul %>% group_by(날짜) %>% summarize(hanbyul=sum(Number))
lamuqe <- lamuqe %>% group_by(날짜) %>% summarize(lamuqe=sum(Number))
ssinnim <- ssinnim %>% group_by(날짜) %>% summarize(ssinnim=sum(Number))
sunny <- sunny %>% group_by(날짜) %>% summarize(sunny=sum(Number))
risabae <- risabae %>% group_by(날짜) %>% summarize(risabae=sum(Number))
pony <- pony %>% group_by(날짜) %>% summarize(pony=sum(Number))
yoonjja <- yoonjja %>% group_by(날짜) %>% summarize(yoonjja=sum(Number))
yeondoo <- yeondoo %>% group_by(날짜) %>% summarize(yeondoo=sum(Number))

## Merge data
freq <- merge(daterow,daddoa,all.x=T,by="날짜")
freq <- merge(freq,hanbyul,all.x=T,by="날짜")
freq <- merge(freq,lamuqe,all.x=T,by="날짜")
freq <- merge(freq,ssinnim,all.x=T,by="날짜")
freq <- merge(freq,sunny,all.x=T,by="날짜")
freq <- merge(freq,risabae,all.x=T,by="날짜")
freq <- merge(freq,pony,all.x=T,by="날짜")
freq <- merge(freq,yeondoo,all.x=T,by="날짜")
freq <- merge(freq,yoonjja,all.x=T,by="날짜")
freq[is.na(freq)] <- 0
glimpse(freq)

## 7 day roll
# daddoa
freq1 <- data.frame(freq$날짜)
colnames(freq1) <- "날짜"

freq1$daddoa7 <- 0
for (i in 7:1126)
{
  freq1$daddoa7[i] <- freq$daddoa[i]+freq$daddoa[i-1]+freq$daddoa[i-2]+freq$daddoa[i-3]+freq$daddoa[i-4]+freq$daddoa[i-5]+freq$daddoa[i-6]
}

# hanbyul
freq1$hanbyul7 <- 0
for (i in 7:1126)
{
  freq1$hanbyul7[i] <- freq$hanbyul[i]+freq$hanbyul[i-1]+freq$hanbyul[i-2]+freq$hanbyul[i-3]+freq$hanbyul[i-4]+freq$hanbyul[i-5]+freq$hanbyul[i-6]
}

# lamuqe
freq1$lamuqe7 <- 0
for (i in 7:1126)
{
  freq1$lamuqe7[i] <- freq$lamuqe[i]+freq$lamuqe[i-1]+freq$lamuqe[i-2]+freq$lamuqe[i-3]+freq$lamuqe[i-4]+freq$lamuqe[i-5]+freq$lamuqe[i-6]
}

# ssinnim
freq1$ssinnim7 <- 0
for (i in 7:1126)
{
  freq1$ssinnim7[i] <- freq$ssinnim[i]+freq$ssinnim[i-1]+freq$ssinnim[i-2]+freq$ssinnim[i-3]+freq$ssinnim[i-4]+freq$ssinnim[i-5]+freq$ssinnim[i-6]
}

# sunny
freq1$sunny7 <- 0
for (i in 7:1126)
{
  freq1$sunny7[i] <- freq$sunny[i]+freq$sunny[i-1]+freq$sunny[i-2]+freq$sunny[i-3]+freq$sunny[i-4]+freq$sunny[i-5]+freq$sunny[i-6]
}

# risabae
freq1$risabae7 <- 0
for (i in 7:1126)
{
  freq1$risabae7[i] <- freq$risabae[i]+freq$risabae[i-1]+freq$risabae[i-2]+freq$risabae[i-3]+freq$risabae[i-4]+freq$risabae[i-5]+freq$risabae[i-6]
}

# pony
freq1$pony7 <- 0
for (i in 7:1126)
{
  freq1$pony7[i] <- freq$pony[i]+freq$pony[i-1]+freq$pony[i-2]+freq$pony[i-3]+freq$pony[i-4]+freq$pony[i-5]+freq$pony[i-6]
}

# yeondoo
freq1$yeondoo7 <- 0
for (i in 7:1126)
{
  freq1$yeondoo7[i] <- freq$yeondoo[i]+freq$yeondoo[i-1]+freq$yeondoo[i-2]+freq$yeondoo[i-3]+freq$yeondoo[i-4]+freq$yeondoo[i-5]+freq$yeondoo[i-6]
}

# yoonjja
freq1$yoonjja7 <- 0
freq1$yoonjja7[5:6] <- 1
for (i in 7:1126)
{
  freq1$yoonjja7[i] <- freq$yoonjja[i]+freq$yoonjja[i-1]+freq$yoonjja[i-2]+freq$yoonjja[i-3]+freq$yoonjja[i-4]+freq$yoonjja[i-5]+freq$yoonjja[i-6]
}

##
freqplus2 <- cbind(freq1[,1],freq1[,-1] + 2)
colnames(freqplus2)[1] <- "날짜"

views <- read.csv("Youtube_view.csv")
viewsdate <- data.frame(freq$날짜)
colnames(viewsdate) <- "날짜"
views <- merge(viewsdate, views, all.x=T, by="날짜")

scoreboard <- data.frame(freq$날짜)
colnames(scoreboard) <- "날짜"

# outlier imputation (20170116~20170118)
views$Daily.Daddoa.view[581:584] <- mean(views$Daily.Daddoa.view[-c(581,582,583,584)])
views$Daily.Hanbyul.view[581:584] <- mean(views$Daily.Hanbyul.view[-c(581,582,583,584)])
views$Daily.lamuqe.view[581:584] <- mean(views$Daily.lamuqe.view[-c(581,582,583,584)])
views$Daily.Ssinnim.view[581:584] <- mean(views$Daily.Ssinnim.view[-c(581,582,583,584)])
views$Daily.Sunny.view[581:584] <- mean(views$Daily.Sunny.view[-c(581,582,583,584)])
views$Daily.RISABAE.view[581:584] <- mean(views$Daily.RISABAE.view[-c(581,582,583,584)])
views$Daily.PONY.view[581:584] <- mean(views$Daily.PONY.view[-c(581,582,583,584)])
views$Daily.Yunjjami.view[581:584] <- mean(views$Daily.Yunjjami.view[-c(581,582,583,584)])
views$Daily.Yeondookong.view[581:584] <- mean(views$Daily.Yeondookong.view[-c(581,582,583,584)])

## Calculate Weighted Views

scoreboard$daddoa7 <- 0
for (i in 1:1126)
{
  scoreboard$daddoa7[i] <- views$Daily.Daddoa.view[i]*log(freqplus2$daddoa7[i])
}

scoreboard$hanbyul7 <- 0
for (i in 1:1126)
{
  scoreboard$hanbyul7[i] <- views$Daily.Hanbyul.view[i]*log(freqplus2$hanbyul7[i])
}

scoreboard$lamuqe7  <- 0
for (i in 1:1126)
{
  scoreboard$lamuqe7[i] <- views$Daily.lamuqe.view[i]*log(freqplus2$lamuqe7[i])
}

scoreboard$ssinnim7  <- 0
for (i in 1:1126)
{
  scoreboard$ssinnim7[i] <- views$Daily.Ssinnim.view[i]*log(freqplus2$ssinnim7[i])
}

scoreboard$sunny7  <- 0
for (i in 1:1126)
{
  scoreboard$sunny7[i] <- views$Daily.Sunny.view[i]*log(freqplus2$sunny7[i])
}

scoreboard$risabae7  <- 0
for (i in 1:1126)
{
  scoreboard$risabae7[i] <- views$Daily.RISABAE.view[i]*log(freqplus2$risabae7[i])
}

scoreboard$pony7  <- 0
for (i in 1:1126)
{
  scoreboard$pony7[i] <- views$Daily.PONY.view[i]*log(freqplus2$pony7[i])
}

scoreboard$yoonjja7  <- 0
for (i in 1:1126)
{
  scoreboard$yoonjja7[i] <- views$Daily.Yunjjami.view[i]*log(freqplus2$yoonjja7[i])
}

scoreboard$yeondoo7  <- 0
for (i in 1:1126)
{
  scoreboard$yeondoo7[i] <- views$Daily.Yeondookong.view[i]*log(freqplus2$yeondoo7[i])
}

## Add by channel

date1 <- as.data.frame(Amorefin$날짜)
colnames(date1) <- "날짜"
scoreboardfin <- merge(date1, scoreboard, all.x=T, by="날짜")
scoreboardfin$sum7 <- 0
scoreboardfin$sum7 <- apply(scoreboardfin[,2:10],1,sum)

write.csv(scoreboardfin[,c(1,11)],"LG_Weighted_Views.csv")


########################
### Influencer Index ###
########################

wizdeo<-read.csv('Youtube_wizdeo.csv')

str(wizdeo$DailysubsAverage)

wizdeo$rolling7subsAverage<-wizdeo$DailysubsAverage

for (i in 7:nrow(wizdeo)){
  wizdeo$rolling14subsAverage[i]<-mean(c(wizdeo$DailysubsAverage[i],wizdeo$DailysubsAverage[i-1],
                                         wizdeo$DailysubsAverage[i-2],wizdeo$DailysubsAverage[i-3],
                                         wizdeo$DailysubsAverage[i-4],wizdeo$DailysubsAverage[i-5],
                                         wizdeo$DailysubsAverage[i-6]))
  
}

wizdeo$rolling7ViewAverage<-wizdeo$DailyViewAverage

for (i in 7:nrow(wizdeo)){
  wizdeo$rolling14ViewAverage[i]<-mean(c(wizdeo$DailyViewAverage[i],wizdeo$DailyViewAverage[i-1],
                                         wizdeo$DailyViewAverage[i-2],wizdeo$DailyViewAverage[i-3],
                                         wizdeo$DailyViewAverage[i-4],wizdeo$DailyViewAverage[i-5],
                                         wizdeo$DailyViewAverage[i-6]))
  
}

wizdeo$logrolling7subsAverage<-log(wizdeo$rolling7subsAverage)
wizdeo$logrolling7ViewAverage<-log(wizdeo$rolling7ViewAverage)

colnames(wizdeo)[6:7]<-c('Views','Subscribers')


###########################
##### Merge All Data ######
###########################

COSPI30_data <- cbind(cosmetic_final, CTModel, Oplex, wizdeo)
write.csv(COSPI30_data, "COSPI3_final.csv")

##########################
##### Random Forest ######
##########################

## RandomForest COSPI30
COSPI30_data<-read.csv('COSPI3_final.csv')
COSPI30_data$WeightedViews<-NULL
colnames(COSPI30_data)
RF<-data.frame(first=c(0),second=c(0),third=c(0))
COSPI30_data$COSPI<-log(COSPI30_data$COSPI)

## Economy
COSPI_test_data<-COSPI30_data[,c(2:11)]
str(COSPI_test_data)

traindata<-COSPI_test_data[c(1:624),]
testdata<-COSPI_test_data[c(625:744),]
nrow(traindata)
nrow(testdata)

RFTrain = randomForest(COSPI ~ ., data = traindata, ntree=200, mtry =5)
par(mfrow=c(1,1))
plot(RFTrain) # choose best tree numbers

RF_pred=predict(RFTrain,newdata=testdata) # predict

## Economy + News
COSPI_test_data<-COSPI30_data[,c(2:15)]
str(COSPI30_data)

traindata<-COSPI_test_data[c(1:624),]
testdata<-COSPI_test_data[c(625:744),]
nrow(traindata)
nrow(testdata)

RFTrain = randomForest(COSPI ~ ., data = traindata, ntree=200, mtry =5)
par(mfrow=c(1,1))
plot(RFTrain) # choose best tree numbers

RF_pred=predict(RFTrain,newdata=testdata) # predict

## Economy + News + Youtube
COSPI_test_data<-COSPI30_data[,c(2:1)]
str(COSPI30_data)

traindata<-COSPI_test_data[c(1:624),]
testdata<-COSPI_test_data[c(625:744),]
nrow(traindata)
nrow(testdata)

RFTrain = randomForest(COSPI ~ ., data = traindata, ntree=200, mtry =5)
par(mfrow=c(1,1))
plot(RFTrain) # choose best tree numbers

RF_pred=predict(RFTrain,newdata=testdata) # predict

##################
##### Arimax #####
##################

## ARIMAX COSPI30
COSPI30_data<-read.csv('COSPI3_final.csv')
Stock_Price<-(COSPI30_data$COSPI)


COSPI30_data$WeightedViews<-NULL
colnames(COSPI30_data)
ts_train<-Stock_Price[1:624]
ts_train<-ts(ts_train,frequency=1)

ts_test<-Stock_Price[625:744]
ts_test<-ts(ts_test,frequency=1)

xreg_train<-COSPI30_data[1:624,c(3:11)] 
xreg_test<-COSPI30_data[625:744,c(3:11)]
rownames(xreg_test) <- 1:120

fit_arima_regression <- auto.arima(ts_train,xreg=xreg_train,approximation=FALSE)
yhat<-forecast::forecast(fit_arima_regression,xreg=xreg_test)$mean

ts_train<-Stock_Price[1:624]
ts_train<-ts(ts_train,frequency=1)

ts_test<-Stock_Price[625:744]
ts_test<-ts(ts_test,frequency=1)

xreg_train<-COSPI30_data[1:624,c(3:15)] 
xreg_test<-COSPI30_data[625:744,c(3:15)]
rownames(xreg_test) <- 1:120

fit_arima_regression <- auto.arima(ts_train,xreg=xreg_train,approximation=FALSE)
yhat<-forecast::forecast(fit_arima_regression,xreg=xreg_test)$mean

ts_train<-Stock_Price[1:624]
ts_train<-ts(ts_train,frequency=1)

ts_test<-Stock_Price[625:744]
ts_test<-ts(ts_test,frequency=1)

xreg_train<-COSPI30_data[1:624,c(3:17)] 
xreg_test<-COSPI30_data[625:744,c(3:17)]
rownames(xreg_test) <- 1:120

fit_arima_regression <- auto.arima(ts_train,xreg=xreg_train,approximation=FALSE)
yhat<-forecast::forecast(fit_arima_regression,xreg=xreg_test)$mean
