
library(dplyr)
library('rvest')
library(stringr)
library(tm)
library(TestDataImputation)


# Amore Youtube RISABAE example #


RISABAE_url<-read.csv('RISABAE url.csv',stringsAsFactors = F)
str(RISABAE_url)

url1<-RISABAE_url$Video.URL

a<-matrix(NA,nrow(url1),2)
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




### overview 부분만 따오기 + 영상별 조회수 ###
b<-matrix(NA,120,3)
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




# Remove NA rows #
b<-Listwise(b,Mvalue = "NA") 

# tolower #
b$overview<- tolower(b$overview)

#write.csv(b,'RISABAE overview.csv',row.names = F)
b<-read.csv('RISABAE overview.csv')


# 각 overview 별로 아모레 자회사 개수 추출 # 

Amore_list<-read.csv('아모레퍼시픽 자회사 list.csv',stringsAsFactors = F)

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
#write.csv(b,'RISABAE overview14.csv')









start_num3<-regexpr('게시일', a$text[25] )[1]
end_num3<-regexpr('게시일', a$text[25] )[1]+18
dd<-substr(a$text[25],start_num3,end_num3)
idx<-gregexpr('\\.',dd )[[1]][1]
idx2<-gregexpr('\\.',dd )[[1]][3]
substr(dd,idx-3,idx2)

##############################################
##############################################







b<-read.csv('sunny_overview.csv',stringsAsFactors = F)
b$Number<-NULL

# 각 overview 별로 LG생활건강 자회사 개수 추출 # 

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

#write.csv(b,'sunny_LG_overview.csv',row.names = F)







##############################################
##############################################
##############################################
###### NEWS (Koreaherald crawling) ###########
##############################################
##############################################
##############################################



#######url 따오기 ###
library('rvest')
library(stringr)

link<-matrix(NA,880,1) #페이지 수  = 880
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

url_data_frame<-data.frame(url=unlist(url_list))
#write.csv(url_data_frame,'koreaherald_url.csv')

#################
### url 따온걸로 크롤링 하기 ##

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
##################
km<-read.csv('km_final.csv',stringsAsFactors = F)

str(km)

kor<-matrix(NA,nrow(km),2)
kor<-data.frame(kor)
colnames(kor)<-c('날짜','text')
for ( i in c(1:nrow(km)) ){
  kor$날짜[i]<-km$날짜[i]
  kor$text[i] <-paste(km[i,1],km[i,2],km[i,3],km[i,4],km[i,5],km[i,6],km[i,7],km[i,8],km[i,9],km[i,10],km[i,11],km[i,12],km[i,13],
        km[i,14],km[i,15],km[i,16],km[i,17],km[i,18],km[i,19],km[i,20],km[i,21],km[i,22],km[i,23],km[i,24],km[i,25],km[i,26],km[i,27],
        km[i,28],km[i,29],km[i,30],km[i,31],km[i,32],km[i,33],km[i,34],km[i,35],km[i,36],km[i,37],km[i,38],km[i,39],km[i,40],km[i,41],
        km[i,42],km[i,43],km[i,44],km[i,45],km[i,46],km[i,47],km[i,48])
  
}

#write.csv(kor,'koreaherald_final.csv',row.names = F)



str(kor)




















