library(dplyr)
library(data.table)
library(stringr)
library(tidyr)
library(ggplot2)
library(readr)
library(caret)
library(psych) # for factor annalysis 
library(dendextend)
library(FactoMineR)
library(factoextra)
############### 시즌 두 개 합치기 ###############

dat_1718<-fread('2-3. 2017-2018시즌_경기기록데이터.csv')
dat_1617<-fread('2-3. 2016-2017시즌_경기기록데이터.csv')
dat_all<-rbind(dat_1617,dat_1718)


#dat_1718_관중<-fread('1-1. 관중_2017-2018시즌 경기번호.csv')

#dat_1718_관중 <- dat_1718_관중[complete.cases(dat_1718_관중),]





############### 파생변수 만들기 ###############

#필요변수정리
dat_all_plus <-
  dat_all  %>% 
  transmute(season_code = season_code,
            game_no = game_no,
            team_code = team_code,
            player_no = player_no,
            MP = play_min + play_sec/60,
            TWOP = fg,
            TWOPA = fg_a,
            FT = ft,
            FTA = ft_a,
            THREEP = threep,
            THREEPA = threep_a,
            FG = fg + threep,
            FGA = fg_a + threep_a,
            PTS = TWOP*2 + THREEP*3 + FT,
            ORB = o_r,
            DRB = d_r,
            TRB = o_r + d_r,
            AST = a_s,
            STL = s_t,
            BLK = b_s,
            TOV = t_o,
            PF = foul_tot
  )


##team
dat_all_team <-
  dat_all_plus %>% 
  group_by(team_code) %>% 
  summarise_at(vars(5:22), sum) %>% 
  rename_at(vars(-1), funs(stringr::str_c("team_", .))) %>% 
  mutate(team_PPG = team_PTS / 54)

##opp
dat_all_opp<-
  dat_all_plus %>% 
  group_by(game_no, team_code) %>% 
  summarise_at(vars(5:22),sum)


opp_4PER <- dat_all_opp

for(i in 1:nrow(opp_4PER)){
  if(i%%2 == 1){
    opp_4PER[i,3:20] <- dat_all_opp[i+1, 3:20]
  } else {
    opp_4PER[i,3:20] <- dat_all_opp[i-1, 3:20]
  }
}
opp4PER <-
  opp_4PER %>% 
  rename_at(vars(-(1:2)), funs(stringr::str_c("opp_", .))) %>% 
  group_by(team_code) %>%
  select(-1) %>% 
  summarise_all(sum) %>% 
  mutate(opp_PPG = opp_PTS / 54)


dat_all_final<-merge(dat_all_plus,dat_all_team,by='team_code')
dat_all_final <- merge(dat_all_final,opp4PER,by='team_code')



# Make a Plus features  ( http://bookyoon.dothome.co.kr/g5/glossary.php )
dat_all_final<-dat_all_final %>% mutate(
  True_Shooting_percent = PTS / (2 * FGA + 0.44 * FTA),
  Effective_Field_Goal_percent = (FG + 0.5 * 3*THREEP ) / FGA,
  AST_percent = 100 * AST / (((MP / (team_MP / 5)) * team_FG) - FG),
  BLK_percent = 100 * (BLK * (team_MP / 5)) / (MP * (opp_FGA - opp_THREEPA)),
  TOV_percent = 100 * TOV / (FGA + 0.44 * FTA + TOV),
  REB_percent = 100 * (TRB * (team_MP / 5)) / (MP * (team_TRB + opp_TRB))
  )

for ( i in 1:ncol(dat_all_final)){
  idx_nan <- is.nan(dat_all_final[,i])
  dat_all_final[idx_nan,i] <- 0
}

## 여기까지가 경기별 파생변수 만든거임. dat_all_fianl 의 61~66변수 까지가 그것
## 파생된 변수 원래 데이터에 추가 

kbl_final_per_play<-cbind(dat_all, dat_all_final[,61:66])
kbl_final_per_play<-as_tibble(kbl_final_per_play)

# 퍼센트 계산 시 나온 아웃라이어 제거 (0보다 작거나 100보다 큰놈들 30개정도 있는데 없앰)

num_col<-ncol(kbl_final_per_play)

for ( i in (num_col-3):(num_col) ){
  idx_out<-which(kbl_final_per_play[,i]>100 | kbl_final_per_play[,i]<0 )
  if ( length(idx_out)!=0) {kbl_final_per_play<-kbl_final_per_play[-idx_out,]}
}





# backup
# write.csv(kbl_final_per_play,'kbl_final_per_play.csv',row.names = F)
kbl_final_per_play<-fread('kbl_final_per_play.csv')
kbl_final_per_play<-kbl_final_per_play[which(complete.cases(kbl_final_per_play)==T),]
nrow(kbl_final_per_play)
# 12807

### Position  별 클러스터링 시작############

kbl_final_per_play<-kbl_final_per_play %>% select(-c(season_code,game_code,game_no,team_code,home_away,back_num,away_team,
                               start_flag,inout,inout1,fo,inputtime,fb,idf)) %>% 
  filter(pos =="FD")
############################################

# Play time 변수 만들기, sec 으로 통일
## Make "Ratio" variables 

kbl_final_per_play =kbl_final_per_play %>% mutate(play_time = play_min*60 + play_sec) %>%
  select(-play_min, -play_sec)


# 10 번 이하 출전자들 제외 
play_n <- kbl_final_per_play %>% group_by(player_no) %>%
  summarise(N=n())  %>% filter(N>=10)


# 선수별 데이터로 suumarise 
dat_summary<-kbl_final_per_play %>% select(-c(pos)) %>% filter(player_no %in% play_n$player_no) %>% 
  group_by(player_no) %>%
  summarise_all(mean)


# 1분당 스탯으로 전환 ( 이 때 player_no 와 play_time은 제외)
dat_summary[,-c(1,32)] = dat_summary %>% select(-player_no,-play_time) * 60 / dat_summary$play_time

# Ratio 변수 만들기 
dat_summary<- dat_summary %>% mutate(fg_ratio = fg/fg_a, ft_ratio =ft/ft_a, 
         threep_ratio=threep/threep_a)


################
## divide the data by play_time
## stat per min
################

## 경기당 play_time 1분 미만인 친구들 제외
dat_summary = dat_summary %>% filter(play_time>=100)  %>% as.data.frame()

# 분모가 0 인경우 발생하는 NaN -> 0 으로 치환
for ( i in 1:ncol(dat_summary)){
  idx<-which(is.nan(dat_summary[,i])==T)
  dat_summary[idx,i] <- 0
}




## 변수별 정규화
dat_summary_z<-dat_summary %>% select(-player_no)
dat_summary_z <- scale(dat_summary_z) %>% as_tibble()


dat_summary<-cbind(player_no = dat_summary[,1],dat_summary_z) %>% select(-play_time)
dat_summary_final <- as_tibble(dat_summary)

dat_summary_final <- as.data.frame(dat_summary_final)

row.names(dat_summary_final) <- name_data$kname

aa<-dat_summary_final %>% round(3)


# 1번쨰 변수는 player_no
# correatlion 구하기
kbl_per_person_cor <- cor(dat_summary_final[,-1])

#Factor analysis of the data
factors_data <- fa(r = kbl_per_person_cor, nfactors = 7, rotate = "varimax")
#Getting the factor loadings and model analysis
factors_data
# write.csv(factors_data$loadings[1:30,1:7],"fa_c.csv",row.names = F) 
print_data<-factors_data$loadings[,2:7] %>% round(3) 

colnames(print_data) <-c(paste0('F',2:7))
print_data
factors_data
fa.diagram(factors_data$MR1)



fa_frame<-as.data.frame(factors_data$loadings[1:(ncol(dat_summary_final)-1),1:7])

nvariables<-ncol(kbl_per_person_cor)
nfactors<-7
loadings_mat <- as.data.frame(matrix(nrow = nvariables, ncol =nfactors))
loadings_mat<-loadings_mat %>% mutate(Variable = colnames(kbl_per_person_cor))

for (i in 1:nfactors) {
  for (j in 1:nvariables) {
    loadings_mat[j, i] <- factors_data$loadings[j, i]  
  }
}
colnames(loadings_mat) <- c(paste0('F',1:7), "Variable")
loadings_mat<- loadings_mat %>% select_at(-c(1))
loadings_mat %>% round(3)
loadings_mat_gather <- loadings_mat %>% gather("Factor", "Value", 1:(nfactors-1) )

# now start plotting
g1 <- ggplot(loadings_mat_gather, aes(Variable, abs(Value), fill=Value))
g1 <- g1 + facet_wrap(~ Factor, nrow=1)
g1 <- g1 +geom_bar(stat="identity")
g1 <- g1 + coord_flip()
g1 <- g1 + scale_fill_gradient2(name = "Loading", 
                                high = "blue", mid = "white", low = "red", 
                                midpoint=0, guide=F) 
g1 <- g1 + xlab("Variable")  # improve x-axis label
g1 <- g1 + ylab("Factor Loading")  #improve y-axis label
g1 <- g1 + ggtitle("Factors for Guard Position")
g1 <- g1 + theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=12, face="bold"))
g1 <- g1 + theme(plot.title = element_text(size=12))
g1 <- g1 + theme_bw(base_size=12)
g1



dim(dat_summary_final[,-1])
dim(fa_frame)

kbl_fa_7 <- as.matrix(dat_summary_final[,-1]) %*% as.matrix(fa_frame)
kbl_fa_c<-cbind(dat_summary_final[,1], as_tibble(kbl_fa_7) ) 


## Fa 변수별 정규화
kbl_fa_c_z<-kbl_fa_c %>% select(-player_no)
kbl_fa_c_z <- scale(kbl_fa_c_z) %>% as_tibble()


kbl_fa_c<-cbind(player_no =kbl_fa_c[,1],kbl_fa_c_z) 
kbl_fa_c_final <- as_tibble(kbl_fa_c)


## 선수 이름 붙이기 
player_code1<-fread('팀,플레이어 코드1.csv')
player_code1<-player_code1 %>% select_at(2:3)

player_code2<-fread('팀,플레이어 코드2.csv')
player_code2<-player_code2 %>% select_at(2:3)

idx<-player_code1$player_no %in% player_code2$player_no
idx<-which(idx==F)

player_code<-rbind(player_code1[idx,], player_code2)

name_data <- left_join(kbl_fa_c_final, player_code,by='player_no')
#write.csv(name_data,'name_data.csv',row.names = F)
kbl_fa_c_final <- as.data.frame(kbl_fa_c_final)
row.names(kbl_fa_c_final) <- name_data$kname
aa<-kbl_fa_c_final %>% round(3) 



# 군집분석  


#dist_all<-dist(kbl_fa_c_final[,-c(1,2)])

hc<-hclust(dist_all,method = 'complete')

memb <- cutree(hc, k = 2)
table(memb)
centers <- aggregate( . ~ memb, data = kbl_fa_c[,-c(1,2)], FUN = mean) %>% select(-memb)
centers

kk<-kmeans(kbl_fa_c_final[,-c(1,2)],2)
kk_c<-kk$cluster
tdata<-cbind( kbl_fa_c_final,memb)
tpc<-prcomp(tdata[,3:11])$x[,1:2]
ttdata<-cbind(tdata,tpc)
ggplot(ttdata,aes(PC1,PC2,col=factor(memb) )) + geom_point()






centers_sd<-apply(centers,2,sd)
centers_sd
dendo<- hc %>% as.dendrogram() %>% set("branches_k_color", k = 2) %>% 
  set("branches_lwd", c(rep(4,2))) %>%
  color_labels(k=2) %>% set("labels_cex", 1.3)  
plot(dendo, main = 'KBL hierarchical clustering : Center Position Dendrogram')






as.ggdend(dendo) %>% ggplot()
ggdendrogram( hc)


dendo2<-hc %>% as.dendrogram() %>% color_branches(k=2) %>% color_labels %>% 
   set("labels_cex", 1.3)
plot(dendo2,horiz = T)



####################



# visualization
c_for_plot<-gather(kbl_fa_c[,-c(1,2)], Var, Val, 1:6  )
#ggplot(c_for_plot,aes(Val)) + geom_histogram(binwidth = 0.5) + facet_wrap(~Var) 


colnames(kbl_fa_c_final)[3:8] <- paste0('F',1:6)
colnames(kbl_fa_c_final)[2]<-'F0'
Recommend_center <- function(pos_data,n=7){
  original_pos_data<-pos_data
  Center_list <-vector(n,mode = 'list')
  Your_preference  <- NULL
  res.hcpc_list<-vector(n,mode = 'list')
  for ( i in 1:n){
    
    
    dist_all<-dist(pos_data[,-c(1,2)])
    
    hc<-hclust(dist_all,method = 'ward.D')
    
    memb <- cutree(hc, k = 2)
    cluster_data<-cbind(pos_data,K=memb) %>% as_data_frame()
    
    centers <- aggregate( . ~ memb, data = pos_data[,-c(1,2)], FUN = mean) %>% select(-memb)
    
    # 1차 : 가장 값이 큰 값을 해당 군집의 대표 변수로 설정 (정규화를 했기 때문에 단순 비교 가능)
    k1_cri<-which(centers[1,] == max(centers[1,]))
    k2_cri<-which(centers[2,] == max(centers[2,]))
    k1_max_value<-max(centers[1,])
    
    cri1_name<-names(centers)[k1_cri]
    cri2_name<-names(centers)[k2_cri]
    k2_max_value<-max(centers[2,])
    
    # 2차 : 두 군집에서 뽑힌 대표변수가 같을 경우 : 해당 변수의 값이 더 작은 군집에서 2위 변수로 대체
    if( cri1_name == cri2_name){
      if(max(centers[1,]) <= max(centers[2,])){
        k1_cri<-which( centers[1,] == as.numeric(sort(centers[1,],decreasing = T)[2]) ) # 2번째로 큰 값 
        cri1_name<-names(centers)[k1_cri]
        k1_max_value<-as.numeric(sort(centers[1,],decreasing = T)[2])
      } else {
        k2_cri<-which( centers[2,] == as.numeric(sort(centers[2,],decreasing = T)[2]) ) # 2번째로 큰 값 
        cri2_name<-names(centers)[k2_cri]
        k2_max_value<-as.numeric(sort(centers[2,],decreasing = T)[2])
      }
    }
    
    idx_1<- which(cluster_data$K==1)
    idx_2<- which(cluster_data$K==2)
    
    print(c(cri1_name,cri2_name))
    
    K_choose <- readline(prompt = "Please Choose K for 1 or 2 : ") 
    if(K_choose != "1" & K_choose !="2") {stop('You should choose 1 or 2 for K!!')}
    
    Center_list[[i]] <- data.frame(Criteria = c(cri1_name,cri2_name) %>% as.character, 
                                   Value = c(k1_max_value,k2_max_value ))  %>% 
      mutate(Your_Choice = c(as.character(Criteria[as.numeric(K_choose)]),''))
    Your_preference <-c(Your_preference ,Center_list[[i]]$Your_Choice[1] )
    print(Center_list[[i]])
    
    
    # 새롭게 재설정된 pos_data에는 내가 고른 군집속 obs만 들어가며, 동시에 골랐던 변수는 제외됨.
    # ex: 내가 MR4를 골랐으면 MR4가 높은 obs 들 만을 대상으로 MR4는 제외하고 재군집분석.
    if(K_choose=="1"){ pos_data <- cluster_data[idx_1,] %>% select(-c(K,cri1_name))  }
    if(K_choose=="2"){ pos_data <- cluster_data[idx_2,] %>% select(-c(K,cri2_name))  }
    
    name_plus <-left_join(name_data,pos_data,by= 'player_no')
    
    
    if(nrow(pos_data)==1 ){break}
    
  }
  your_player<-original_pos_data %>% filter(player_no==pos_data$player_no) 
  Name<- name_data$kname[which(name_data$player_no==your_player$player_no )]
  return(result_list = list( Choose_log_data = Center_list, 
                             Your_player =  Name,
                             Your_player_stat=your_player, 
                             Your_preference  = Your_preference) )
  
}


Recommend_center(kbl_fa_c_final,7)






Recommend_center <- function(pos_data,n=7){
  original_pos_data<-pos_data
  Center_list <-vector(n,mode = 'list')
  Your_preference  <- NULL
  res.hcpc_list<-vector(n,mode = 'list')
  for ( i in 1:n){
    
    
    dist_all<-dist(pos_data[,-c(1,2)])
    
    hc<-hclust(dist_all,method = 'ward.D')
    
    memb <- cutree(hc, k = 2)
    cluster_data<-cbind(pos_data,K=memb) %>% as_data_frame()
    
    centers <- aggregate( . ~ memb, data = pos_data[,-c(1,2)], FUN = mean) %>% select(-memb)
    
    # 1차 : 가장 값이 큰 값을 해당 군집의 대표 변수로 설정 (정규화를 했기 때문에 단순 비교 가능)
    k1_cri<-which(centers[1,] == max(centers[1,]))
    k2_cri<-which(centers[2,] == max(centers[2,]))
    k1_max_value<-max(centers[1,])
    
    cri1_name<-names(centers)[k1_cri]
    cri2_name<-names(centers)[k2_cri]
    k2_max_value<-max(centers[2,])
    
    # 2차 : 두 군집에서 뽑힌 대표변수가 같을 경우 : 해당 변수의 값이 더 작은 군집에서 2위 변수로 대체
    if( cri1_name == cri2_name){
      if(max(centers[1,]) <= max(centers[2,])){
        k1_cri<-which( centers[1,] == as.numeric(sort(centers[1,],decreasing = T)[2]) ) # 2번째로 큰 값 
        cri1_name<-names(centers)[k1_cri]
        k1_max_value<-as.numeric(sort(centers[1,],decreasing = T)[2])
      } else {
        k2_cri<-which( centers[2,] == as.numeric(sort(centers[2,],decreasing = T)[2]) ) # 2번째로 큰 값 
        cri2_name<-names(centers)[k2_cri]
        k2_max_value<-as.numeric(sort(centers[2,],decreasing = T)[2])
      }
    }
    
    idx_1<- which(cluster_data$K==1)
    idx_2<- which(cluster_data$K==2)
    
    print(c(cri1_name,cri2_name))
    
    K_choose <- readline(prompt = "Please Choose K for 1 or 2 : ") 
    if(K_choose != "1" & K_choose !="2") {stop('You should choose 1 or 2 for K!!')}
    
    Center_list[[i]] <- data.frame(Criteria = c(cri1_name,cri2_name) %>% as.character, 
                                   Value = c(k1_max_value,k2_max_value ))  %>% 
                                     mutate(Your_Choice = c(as.character(Criteria[as.numeric(K_choose)]),''))
    Your_preference <-c(Your_preference ,Center_list[[i]]$Your_Choice[1] )
    print(Center_list[[i]])
    
    #ploting
    tdata<-cluster_data %>%  select(-c(K,player_no,F1))
    res.pca <- PCA(tdata, ncp = 3, graph = FALSE)
    res.hcpc_list[[i]] <- HCPC(res.pca, graph = FALSE,max = 2,min=2,method = 'ward')
    
    
    
    # 새롭게 재설정된 pos_data에는 내가 고른 군집속 obs만 들어가며, 동시에 골랐던 변수는 제외됨.
    # ex: 내가 MR4를 골랐으면 MR4가 높은 obs 들 만을 대상으로 MR4는 제외하고 재군집분석.
    if(K_choose=="1"){ pos_data <- cluster_data[idx_1,] %>% select(-c(K,cri1_name))  }
    if(K_choose=="2"){ pos_data <- cluster_data[idx_2,] %>% select(-c(K,cri2_name))  }
    
    name_plus <-left_join(name_data,pos_data,by= 'player_no')
    
    
    if(nrow(pos_data)==1 ){break}
    
  }
  your_player<-original_pos_data %>% filter(player_no==pos_data$player_no) 
  Name<- name_data$kname[which(name_data$player_no==your_player$player_no )]
  return(result_list = list( Choose_log_data = Center_list, 
               Your_player =  Name,
               Your_player_stat=your_player, 
               Your_preference  = Your_preference,
               res.hcpc_list=res.hcpc_list))
  
}




AAA<-Recommend_center(kbl_fa_c_final,5)

res_list<-AAA$res.hcpc_list

fviz_cluster(res_list[[5]],
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Center Position K=2"
)




tdata<-cbind( kbl_fa_c_final,memb)
tpc<-prcomp(tdata[,2:11])$x[,1:2]
ttdata<-cbind(tdata,tpc)

# Compute PCA with ncp = 3
res.pca <- PCA(tdata[,2:11], ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE,max = 2,min=2,method = 'average')


fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Center Position K=2"
)


tdata2<-res.hcpc$data.clust %>% filter(clust==2)

# Compute PCA with ncp = 3
res.pca <- PCA(tdata2[,1:7], ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE,max = 2,min=2,method = 'average')


fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Center Position K=2"
)


tdata2<-res.hcpc$data.clust %>% filter(clust==1)

# Compute PCA with ncp = 3
res.pca <- PCA(tdata2[,1:7], ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE,max = 2,min=2,method = 'average')


fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Center Position K=2"
)


tdata2<-res.hcpc$data.clust %>% filter(clust==1)

# Compute PCA with ncp = 3
res.pca <- PCA(tdata2[,1:7], ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE,max = 2,min=2,method = 'average')


fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Center Position K=2"
)


tdata2<-res.hcpc$data.clust %>% filter(clust==1)

# Compute PCA with ncp = 3
res.pca <- PCA(tdata2[,1:7], ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE,max = 2,min=2,method = 'average')


fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Center Position K=2"
)

tdata2<-res.hcpc$data.clust %>% filter(clust==1)

# Compute PCA with ncp = 3
res.pca <- PCA(tdata2[,1:7], ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
res.hcpc <- HCPC(res.pca, graph = FALSE,max = 2,min=2,method = 'average')


fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Center Position K=2"
)









fviz_dend(res.hcpc)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)
plot(res.hcpc, choice = "3D.map")
fviz_cluster(res.hcpc, geom = "point", main = "Factor map")


fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)
fviz_dend(res.hcpc, show_labels = FALSE)
# Individuals facor map

dist_all<-dist(pos_data[,-c(1,2)])

hc<-hclust(dist_all,method = 'ward.D')

memb <- cutree(hc, k = 2)
cluster_data<-cbind(pos_data,K=memb) %>% as_data_frame()

centers <- aggregate( . ~ memb, data = kbl_fa_c[,-c(1,2)], FUN = mean) %>% select(-memb)
cri_idx<-which(apply(centers, 2, sd) == max(apply(centers, 2, sd)) ) + 2  # 맨 앞 두개 player_no 랑 MR1
idx_1<- which(cluster_data$K==1)
cri_val_1<-cluster_data[idx_1,cri_idx] %>% apply(2,mean)
idx_2<- which(cluster_data$K==2)
cri_val_2<-cluster_data[idx_2,cri_idx] %>% apply(2,mean)

Center_list[i] <- data.frame(Criteria = names(cri_idx), K1_value = cri_val_1, K2_value = cri_val_2)
print()


cluster_data %>% group_by(K) %>% summarise(criterion = mean( cat(cri_name) ))











dat_norm<-fread('dat_norm.csv')












