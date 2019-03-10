## GAMESAT CODE ##


## Customized F1-Score

customf1 <- function(preds, dtrain) {
  preds <- as.integer(preds)
  labels <- getinfo(dtrain, "label")
  class0rc <- sum(labels==0 & preds==0) / (sum(labels==0 & preds==0) + sum(labels==0 & preds!=0))
  class1rc <- sum(labels==1 & preds==1) / (sum(labels==1 & preds==1) + sum(labels==1 & preds!=1))
  class2rc <- sum(labels==2 & preds==2) / (sum(labels==2 & preds==2) + sum(labels==2 & preds!=2))
  class3rc <- sum(labels==3 & preds==3) / (sum(labels==3 & preds==3) + sum(labels==3 & preds!=3))
  class0pr <- sum(labels==0 & preds==0) / (sum(labels==0 & preds==0) + sum(labels!=0 & preds==0))
  class1pr <- sum(labels==1 & preds==1) / (sum(labels==1 & preds==1) + sum(labels!=1 & preds==1))
  class2pr <- sum(labels==2 & preds==2) / (sum(labels==2 & preds==2) + sum(labels!=2 & preds==2))
  class3pr <- sum(labels==3 & preds==3) / (sum(labels==3 & preds==3) + sum(labels!=3 & preds==3))
  customf1 <- 8/((1/class0rc)+(1/class1rc)+(1/class2rc)+(1/class3rc)+(1/class0pr)+(1/class1pr)+(1/class2pr)+(1/class3pr))
  return(list(metric = "error", value = customf1))
}

#####################
### Data Carpentry###
#####################

library(reshape2)
library(data.table)
library(dplyr)

### ACTIVITy

train_activity <- fread("train_activity.csv")

## cnt_dt
train_activity_cnt_dt <- train_activity[,c(1,2,3)]
cnt_dt <- dcast(train_activity_cnt_dt, acc_id~wk)
colnames(cnt_dt)[2:9] <- paste('week',colnames(cnt_dt)[2:9],'_cnt_dt',sep="")

## play_time
train_activity_play_time <- train_activity[,c(1,2,4)]
play_time <- dcast(train_activity_play_time, acc_id~wk)
colnames(play_time)[2:9] <- paste('week',colnames(play_time)[2:9],'_play_time',sep="")
play_time <- play_time[,-1]

## npc_exp
train_activity_npc_exp <- train_activity[,c(1,2,5)]
npc_exp <- dcast(train_activity_npc_exp, acc_id~wk)
colnames(npc_exp)[2:9] <- paste('week',colnames(npc_exp)[2:9],'_npc_exp',sep="")
npc_exp <- npc_exp[,-1]

## npc_hongmun
train_activity_npc_hongmun <- train_activity[,c(1,2,6)]
npc_hongmun <- dcast(train_activity_npc_hongmun, acc_id~wk)
colnames(npc_hongmun)[2:9] <- paste('week',colnames(npc_hongmun)[2:9],'_npc_hongmun',sep="")
npc_hongmun <- npc_hongmun[,-1]

## quest_exp
train_activity_quest_exp <- train_activity[,c(1,2,7)]
quest_exp <- dcast(train_activity_quest_exp, acc_id~wk)
colnames(quest_exp)[2:9] <- paste('week',colnames(quest_exp)[2:9],'_quest_exp',sep="")
quest_exp <- quest_exp[,-1]

## quest_hongmun
train_activity_quest_hongmun <- train_activity[,c(1,2,8)]
quest_hongmun <- dcast(train_activity_quest_hongmun, acc_id~wk)
colnames(quest_hongmun)[2:9] <- paste('week',colnames(quest_hongmun)[2:9],'_quest_hongmun',sep="")
quest_hongmun <- quest_hongmun[,-1]

## item_hongmun
train_activity_item_hongmun <- train_activity[,c(1,2,9)]
item_hongmun <- dcast(train_activity_item_hongmun, acc_id~wk)
colnames(item_hongmun)[2:9] <- paste('week',colnames(item_hongmun)[2:9],'_item_hongmun',sep="")
item_hongmun <- item_hongmun[,-1]

## game_combat_time
train_activity_game_combat_time <- train_activity[,c(1,2,10)]
game_combat_time <- dcast(train_activity_game_combat_time, acc_id~wk)
colnames(game_combat_time)[2:9] <- paste('week',colnames(game_combat_time)[2:9],'_game_combat_time',sep="")
game_combat_time <- game_combat_time[,-1]

## get_money
train_activity_get_money <- train_activity[,c(1,2,11)]
get_money <- dcast(train_activity_get_money, acc_id~wk)
colnames(get_money)[2:9] <- paste('week',colnames(get_money)[2:9],'_get_money',sep="")
get_money <- get_money[,-1]

## duel_cnt
train_activity_duel_cnt <- train_activity[,c(1,2,12)]
duel_cnt <- dcast(train_activity_duel_cnt, acc_id~wk)
colnames(duel_cnt)[2:9] <- paste('week',colnames(duel_cnt)[2:9],'_duel_cnt',sep="")
duel_cnt <- duel_cnt[,-1]

## duel_win
train_activity_duel_win <- train_activity[,c(1,2,13)]
duel_win <- dcast(train_activity_duel_win, acc_id~wk)
colnames(duel_win)[2:9] <- paste('week',colnames(duel_win)[2:9],'_duel_win',sep="")
duel_win <- duel_win[,-1]

## partybattle_cnt
train_activity_partybattle_cnt <- train_activity[,c(1,2,14)]
partybattle_cnt <- dcast(train_activity_partybattle_cnt, acc_id~wk)
colnames(partybattle_cnt)[2:9] <- paste('week',colnames(partybattle_cnt)[2:9],'_partybattle_cnt',sep="")
partybattle_cnt <- partybattle_cnt[,-1]

## partybattle_win
train_activity_partybattle_win <- train_activity[,c(1,2,15)]
partybattle_win <- dcast(train_activity_partybattle_win, acc_id~wk)
colnames(partybattle_win)[2:9] <- paste('week',colnames(partybattle_win)[2:9],'_partybattle_win',sep="")
partybattle_win <- partybattle_win[,-1]

## cnt_enter_inzone_solo
train_activity_cnt_enter_inzone_solo <- train_activity[,c(1,2,16)]
cnt_enter_inzone_solo <- dcast(train_activity_cnt_enter_inzone_solo, acc_id~wk)
colnames(cnt_enter_inzone_solo)[2:9] <- paste('week',colnames(cnt_enter_inzone_solo)[2:9],'_cnt_enter_inzone_solo',sep="")
cnt_enter_inzone_solo <- cnt_enter_inzone_solo[,-1]

## cnt_enter_inzone_light
train_activity_cnt_enter_inzone_light <- train_activity[,c(1,2,17)]
cnt_enter_inzone_light <- dcast(train_activity_cnt_enter_inzone_light, acc_id~wk)
colnames(cnt_enter_inzone_light)[2:9] <- paste('week',colnames(cnt_enter_inzone_light)[2:9],'_cnt_enter_inzone_light',sep="")
cnt_enter_inzone_light <- cnt_enter_inzone_light[,-1]

## cnt_enter_inzone_skilled
train_activity_cnt_enter_inzone_skilled <- train_activity[,c(1,2,18)]
cnt_enter_inzone_skilled <- dcast(train_activity_cnt_enter_inzone_skilled, acc_id~wk)
colnames(cnt_enter_inzone_skilled)[2:9] <- paste('week',colnames(cnt_enter_inzone_skilled)[2:9],'_cnt_enter_inzone_skilled',sep="")
cnt_enter_inzone_skilled <- cnt_enter_inzone_skilled[,-1]

## cnt_enter_inzone_normal
train_activity_cnt_enter_inzone_normal <- train_activity[,c(1,2,19)]
cnt_enter_inzone_normal <- dcast(train_activity_cnt_enter_inzone_normal, acc_id~wk)
colnames(cnt_enter_inzone_normal)[2:9] <- paste('week',colnames(cnt_enter_inzone_normal)[2:9],'_cnt_enter_inzone_normal',sep="")
cnt_enter_inzone_normal <- cnt_enter_inzone_normal[,-1]

## cnt_enter_raid
train_activity_cnt_enter_raid <- train_activity[,c(1,2,20)]
cnt_enter_raid <- dcast(train_activity_cnt_enter_raid, acc_id~wk)
colnames(cnt_enter_raid)[2:9] <- paste('week',colnames(cnt_enter_raid)[2:9],'_cnt_enter_raid',sep="")
cnt_enter_raid <- cnt_enter_raid[,-1]

## cnt_enter_raid_light
train_activity_cnt_enter_raid_light <- train_activity[,c(1,2,21)]
cnt_enter_raid_light <- dcast(train_activity_cnt_enter_raid_light, acc_id~wk)
colnames(cnt_enter_raid_light)[2:9] <- paste('week',colnames(cnt_enter_raid_light)[2:9],'_cnt_enter_raid_light',sep="")
cnt_enter_raid_light <- cnt_enter_raid_light[,-1]

## cnt_enter_bam
train_activity_cnt_enter_bam <- train_activity[,c(1,2,22)]
cnt_enter_bam <- dcast(train_activity_cnt_enter_bam, acc_id~wk)
colnames(cnt_enter_bam)[2:9] <- paste('week',colnames(cnt_enter_bam)[2:9],'_cnt_enter_bam',sep="")
cnt_enter_bam <- cnt_enter_bam[,-1]

## cnt_clear_inzone_solo
train_activity_cnt_clear_inzone_solo <- train_activity[,c(1,2,23)]
cnt_clear_inzone_solo <- dcast(train_activity_cnt_clear_inzone_solo, acc_id~wk)
colnames(cnt_clear_inzone_solo)[2:9] <- paste('week',colnames(cnt_clear_inzone_solo)[2:9],'_cnt_clear_inzone_solo',sep="")
cnt_clear_inzone_solo <- cnt_clear_inzone_solo[,-1]

## cnt_clear_inzone_light
train_activity_cnt_clear_inzone_light <- train_activity[,c(1,2,24)]
cnt_clear_inzone_light <- dcast(train_activity_cnt_clear_inzone_light, acc_id~wk)
colnames(cnt_clear_inzone_light)[2:9] <- paste('week',colnames(cnt_clear_inzone_light)[2:9],'_cnt_clear_inzone_light',sep="")
cnt_clear_inzone_light <- cnt_clear_inzone_light[,-1]

## cnt_clear_inzone_skilled
train_activity_cnt_clear_inzone_skilled <- train_activity[,c(1,2,25)]
cnt_clear_inzone_skilled <- dcast(train_activity_cnt_clear_inzone_skilled, acc_id~wk)
colnames(cnt_clear_inzone_skilled)[2:9] <- paste('week',colnames(cnt_clear_inzone_skilled)[2:9],'_cnt_clear_inzone_skilled',sep="")
cnt_clear_inzone_skilled <- cnt_clear_inzone_skilled[,-1]

## cnt_clear_inzone_normal
train_activity_cnt_clear_inzone_normal <- train_activity[,c(1,2,26)]
cnt_clear_inzone_normal <- dcast(train_activity_cnt_clear_inzone_normal, acc_id~wk)
colnames(cnt_clear_inzone_normal)[2:9] <- paste('week',colnames(cnt_clear_inzone_normal)[2:9],'_cnt_clear_inzone_normal',sep="")
cnt_clear_inzone_normal <- cnt_clear_inzone_normal[,-1]

## cnt_clear_raid
train_activity_cnt_clear_raid <- train_activity[,c(1,2,27)]
cnt_clear_raid <- dcast(train_activity_cnt_clear_raid, acc_id~wk)
colnames(cnt_clear_raid)[2:9] <- paste('week',colnames(cnt_clear_raid)[2:9],'_cnt_clear_raid',sep="")
cnt_clear_raid <- cnt_clear_raid[,-1]

## cnt_clear_raid_light
train_activity_cnt_clear_raid_light <- train_activity[,c(1,2,28)]
cnt_clear_raid_light <- dcast(train_activity_cnt_clear_raid_light, acc_id~wk)
colnames(cnt_clear_raid_light)[2:9] <- paste('week',colnames(cnt_clear_raid_light)[2:9],'_cnt_clear_raid_light',sep="")
cnt_clear_raid_light <- cnt_clear_raid_light[,-1]

## cnt_clear_bam
train_activity_cnt_clear_bam <- train_activity[,c(1,2,29)]
cnt_clear_bam <- dcast(train_activity_cnt_clear_bam, acc_id~wk)
colnames(cnt_clear_bam)[2:9] <- paste('week',colnames(cnt_clear_bam)[2:9],'_cnt_clear_bam',sep="")
cnt_clear_bam <- cnt_clear_bam[,-1]

## normal_chat
train_activity_normal_chat <- train_activity[,c(1,2,30)]
normal_chat <- dcast(train_activity_normal_chat, acc_id~wk)
colnames(normal_chat)[2:9] <- paste('week',colnames(normal_chat)[2:9],'_normal_chat',sep="")
normal_chat <- normal_chat[,-1]

## whisper_chat
train_activity_whisper_chat <- train_activity[,c(1,2,31)]
whisper_chat <- dcast(train_activity_whisper_chat, acc_id~wk)
colnames(whisper_chat)[2:9] <- paste('week',colnames(whisper_chat)[2:9],'_whisper_chat',sep="")
whisper_chat <- whisper_chat[,-1]

## district_chat
train_activity_district_chat <- train_activity[,c(1,2,32)]
district_chat <- dcast(train_activity_district_chat, acc_id~wk)
colnames(district_chat)[2:9] <- paste('week',colnames(district_chat)[2:9],'_district_chat',sep="")
district_chat <- district_chat[,-1]

## party_chat
train_activity_party_chat <- train_activity[,c(1,2,33)]
party_chat <- dcast(train_activity_party_chat, acc_id~wk)
colnames(party_chat)[2:9] <- paste('week',colnames(party_chat)[2:9],'_party_chat',sep="")
party_chat <- party_chat[,-1]

## guild_chat
train_activity_guild_chat <- train_activity[,c(1,2,34)]
guild_chat <- dcast(train_activity_guild_chat, acc_id~wk)
colnames(guild_chat)[2:9] <- paste('week',colnames(guild_chat)[2:9],'_guild_chat',sep="")
guild_chat <- guild_chat[,-1]

## faction_chat
train_activity_faction_chat <- train_activity[,c(1,2,35)]
faction_chat <- dcast(train_activity_faction_chat, acc_id~wk)
colnames(faction_chat)[2:9] <- paste('week',colnames(faction_chat)[2:9],'_faction_chat',sep="")
faction_chat <- faction_chat[,-1]

## cnt_use_buffitem
train_activity_cnt_use_buffitem <- train_activity[,c(1,2,36)]
cnt_use_buffitem <- dcast(train_activity_cnt_use_buffitem, acc_id~wk)
colnames(cnt_use_buffitem)[2:9] <- paste('week',colnames(cnt_use_buffitem)[2:9],'_cnt_use_buffitem',sep="")
cnt_use_buffitem <- cnt_use_buffitem[,-1]

## gathering_cnt
train_activity_gathering_cnt <- train_activity[,c(1,2,37)]
gathering_cnt <- dcast(train_activity_gathering_cnt, acc_id~wk)
colnames(gathering_cnt)[2:9] <- paste('week',colnames(gathering_cnt)[2:9],'_gathering_cnt',sep="")
gathering_cnt <- gathering_cnt[,-1]

## making_cnt
train_activity_making_cnt <- train_activity[,c(1,2,38)]
making_cnt <- dcast(train_activity_making_cnt, acc_id~wk)
colnames(making_cnt)[2:9] <- paste('week',colnames(making_cnt)[2:9],'_making_cnt',sep="")
making_cnt <- making_cnt[,-1]

train_activity_merged <- cbind(cnt_dt, play_time, npc_exp, npc_hongmun, quest_exp, quest_hongmun, item_hongmun,
                               game_combat_time, get_money, duel_cnt, duel_win, partybattle_cnt, partybattle_win,
                               cnt_enter_inzone_solo, cnt_enter_inzone_light, cnt_enter_inzone_skilled,
                               cnt_enter_inzone_normal,cnt_enter_raid, cnt_enter_raid_light, cnt_enter_bam,
                               cnt_clear_inzone_solo, cnt_clear_inzone_light, cnt_clear_inzone_skilled,
                               cnt_clear_inzone_normal,cnt_clear_raid, cnt_clear_raid_light, cnt_clear_bam,
                               normal_chat, whisper_chat, district_chat, party_chat, guild_chat, faction_chat,
                               cnt_use_buffitem, gathering_cnt, making_cnt)

train_label <- fread("train_label.csv")

train_activity_final <- merge(train_activity_merged, train_label, by="acc_id")

write.csv(train_activity_final, "train_activity_reshaped.csv")

### TRADE

train_trade <- fread("train_trade.csv")

## Unique
train_trade <- unique(train_trade)

## Item Type
train_trade$item_type[train_trade$item_type=="accessory"] <- "etc"
train_trade$item_type[train_trade$item_type=="costume"] <- "etc"
train_trade$item_type[train_trade$item_type=="gem"] <- "etc"
train_trade$item_type[train_trade$item_type=="weapon"] <- "etc"

## Split Data
train_trade_source <- train_trade[,c(1,4,2,6,7)]
train_trade_target <- train_trade[,c(1,5,2,6,7)]

## Source Data
train_trade_source$idx<-c(1:nrow(train_trade_source))
idx_g<-which(train_trade_source$item_type=='grocery')
idx_m<-which(train_trade_source$item_type=='money')
idx_e<-which(train_trade_source$item_type=='etc')

train_trade_source_g<-train_trade_source[idx_g,c(6,5)]
train_trade_source_m<-train_trade_source[idx_m,c(6,5)]
train_trade_source_e<-train_trade_source[idx_e,c(6,5)]
colnames(train_trade_source_g)[2] <- "grocery_amount"
colnames(train_trade_source_m)[2] <- "money_amount"
colnames(train_trade_source_e)[2] <- "etc_amount"

train_trade_source_new<-merge(train_trade_source,train_trade_source_g,'idx',all.x = T)
train_trade_source_new<-merge(train_trade_source_new,train_trade_source_m,'idx',all.x = T)
train_trade_source_new<-merge(train_trade_source_new,train_trade_source_e,'idx',all.x = T)

## Grocery Scaling
index_g <- which(!is.na(train_trade_source_new$grocery_amount))
scaling_g <- train_trade_source_new$grocery_amount[index_g]
maxValue<- max(scaling_g)
minValue<- min(scaling_g)
scaled_g <- ((scaling_g - minValue) / (maxValue-minValue+0.00000000001))*(10-1)+1
train_trade_source_new$grocery_amount[index_g] <- scaled_g
train_trade_source_new$grocery_amount[is.na(train_trade_source_new$grocery_amount)] <- 0

## Money Scaling
index_m <- which(!is.na(train_trade_source_new$money_amount))
scaling_m <- train_trade_source_new$money_amount[index_m]
maxValue<- max(scaling_m)
minValue<- min(scaling_m)
scaled_m <- ((scaling_m - minValue) / (maxValue-minValue+0.00000000001))*(10-1)+1
train_trade_source_new$money_amount[index_m] <- scaled_m
train_trade_source_new$money_amount[is.na(train_trade_source_new$money_amount)] <- 0

## etc Scaling
index_e <- which(!is.na(train_trade_source_new$etc_amount))
scaling_e <- train_trade_source_new$etc_amount[index_e]
maxValue<- max(scaling_e)
minValue<- min(scaling_e)
scaled_e <- ((scaling_e - minValue) / (maxValue-minValue+0.00000000001))*(10-1)+1
train_trade_source_new$etc_amount[index_e] <- scaled_e
train_trade_source_new$etc_amount[is.na(train_trade_source_new$etc_amount)] <- 0

## Finalize
train_trade_source_final <- train_trade_source_new %>% group_by(source_acc_id,trade_week) %>% summarise(s_grocery_times=sum(item_type=="grocery"),s_grocery_amount=sum(grocery_amount),s_money_times=sum(item_type=="money"), s_money_amount=sum(money_amount),s_etc_times=sum(item_type=="etc"), s_etc_amount=sum(etc_amount))

## target Data

train_trade_target$idx<-c(1:nrow(train_trade_target))
idx_g<-which(train_trade_target$item_type=='grocery')
idx_m<-which(train_trade_target$item_type=='money')
idx_e<-which(train_trade_target$item_type=='etc')

train_trade_target_g<-train_trade_target[idx_g,c(6,5)]
train_trade_target_m<-train_trade_target[idx_m,c(6,5)]
train_trade_target_e<-train_trade_target[idx_e,c(6,5)]
colnames(train_trade_target_g)[2] <- "grocery_amount"
colnames(train_trade_target_m)[2] <- "money_amount"
colnames(train_trade_target_e)[2] <- "etc_amount"

train_trade_target_new<-merge(train_trade_target,train_trade_target_g,'idx',all.x = T)
train_trade_target_new<-merge(train_trade_target_new,train_trade_target_m,'idx',all.x = T)
train_trade_target_new<-merge(train_trade_target_new,train_trade_target_e,'idx',all.x = T)

## Grocery Scaling
index_g <- which(!is.na(train_trade_target_new$grocery_amount))
scaling_g <- train_trade_target_new$grocery_amount[index_g]
maxValue<- max(scaling_g)
minValue<- min(scaling_g)
scaled_g <- ((scaling_g - minValue) / (maxValue-minValue+0.00000000001))*(10-1)+1
train_trade_target_new$grocery_amount[index_g] <- scaled_g
train_trade_target_new$grocery_amount[is.na(train_trade_target_new$grocery_amount)] <- 0

## Money Scaling
index_m <- which(!is.na(train_trade_target_new$money_amount))
scaling_m <- train_trade_target_new$money_amount[index_m]
maxValue<- max(scaling_m)
minValue<- min(scaling_m)
scaled_m <- ((scaling_m - minValue) / (maxValue-minValue+0.00000000001))*(10-1)+1
train_trade_target_new$money_amount[index_m] <- scaled_m
train_trade_target_new$money_amount[is.na(train_trade_target_new$money_amount)] <- 0

## etc Scaling
index_e <- which(!is.na(train_trade_target_new$etc_amount))
scaling_e <- train_trade_target_new$etc_amount[index_e]
maxValue<- max(scaling_e)
minValue<- min(scaling_e)
scaled_e <- ((scaling_e - minValue) / (maxValue-minValue+0.00000000001))*(10-1)+1
train_trade_target_new$etc_amount[index_e] <- scaled_e
train_trade_target_new$etc_amount[is.na(train_trade_target_new$etc_amount)] <- 0

## Finalize
train_trade_target_final <- train_trade_target_new %>% group_by(target_acc_id,trade_week) %>% summarise(s_n_grocery=sum(item_type=="grocery"),s_grocery_amount=sum(grocery_amount),s_n_money=sum(item_type=="money"), s_money_amount=sum(money_amount),s_n_etc=sum(item_type=="etc"), s_etc_amount=sum(etc_amount))

## From Long to Wide
train_trade_source_final[1,1]
train_trade_target_final[1,1]

View(train_trade_source_final[1:10,])

## Grocery
## s_grocery_times
train_trade_s_grocery_times <- train_trade_source_final[,c(1,2,3)]
s_grocery_times <- dcast(train_trade_s_grocery_times, source_acc_id~trade_week)
s_grocery_times[s_grocery_times==0] <- NA
colnames(s_grocery_times)[2:9] <- paste('week',colnames(s_grocery_times[2:9]),'_s_grocery_times',sep="")

## s_grocery_amount
train_trade_s_grocery_amount <- train_trade_source_final[,c(1,2,4)]
s_grocery_amount <- dcast(train_trade_s_grocery_amount, source_acc_id~trade_week)
s_grocery_amount[s_grocery_amount==0] <- NA
colnames(s_grocery_amount)[2:9] <- paste('week',colnames(s_grocery_amount[2:9]),'_s_grocery_amount',sep="")
s_grocery_amount <- s_grocery_amount[,-1]

## s_money_times
train_trade_s_money_times <- train_trade_source_final[,c(1,2,5)]
s_money_times <- dcast(train_trade_s_money_times, source_acc_id~trade_week)
s_money_times[s_money_times==0] <- NA
colnames(s_money_times)[2:9] <- paste('week',colnames(s_money_times[2:9]),'_s_money_times',sep="")
s_money_times <- s_money_times[,-1]

## s_money_amount
train_trade_s_money_amount <- train_trade_source_final[,c(1,2,6)]
s_money_amount <- dcast(train_trade_s_money_amount, source_acc_id~trade_week)
s_money_amount[s_money_amount==0] <- NA
colnames(s_money_amount)[2:9] <- paste('week',colnames(s_money_amount[2:9]),'_s_money_amount',sep="")
s_money_amount <- s_money_amount[,-1]

## s_etc_times
train_trade_s_etc_times <- train_trade_source_final[,c(1,2,7)]
s_etc_times <- dcast(train_trade_s_etc_times, source_acc_id~trade_week)
s_etc_times[s_etc_times==0] <- NA
colnames(s_etc_times)[2:9] <- paste('week',colnames(s_etc_times[2:9]),'_s_etc_times',sep="")
s_etc_times <- s_etc_times[,-1]

## s_etc_amount
train_trade_s_etc_amount <- train_trade_source_final[,c(1,2,8)]
s_etc_amount <- dcast(train_trade_s_etc_amount, source_acc_id~trade_week)
s_etc_amount[s_etc_amount==0] <- NA
colnames(s_etc_amount)[2:9] <- paste('week',colnames(s_etc_amount[2:9]),'_s_etc_amount',sep="")
s_etc_amount <- s_etc_amount[,-1]

## Source Merge
train_trade_source_merged <- cbind(s_grocery_times, s_grocery_amount, s_money_times, s_money_amount, s_etc_times, s_etc_amount)
colnames(train_trade_source_merged)[1] <- "acc_id"

train_AP_reshaped <- fread("train_AP_reshaped.csv")
train_APS <- merge(train_AP_reshaped, train_trade_source_merged, by="acc_id", all.x=T)

## Target
## t_grocery_times
train_trade_t_grocery_times <- train_trade_target_final[,c(1,2,3)]
t_grocery_times <- dcast(train_trade_t_grocery_times, target_acc_id~trade_week)
t_grocery_times[t_grocery_times==0] <- NA
colnames(t_grocery_times)[2:9] <- paste('week',colnames(t_grocery_times[2:9]),'_t_grocery_times',sep="")

## t_grocery_amount
train_trade_t_grocery_amount <- train_trade_target_final[,c(1,2,4)]
t_grocery_amount <- dcast(train_trade_t_grocery_amount, target_acc_id~trade_week)
t_grocery_amount[t_grocery_amount==0] <- NA
colnames(t_grocery_amount)[2:9] <- paste('week',colnames(t_grocery_amount[2:9]),'_t_grocery_amount',sep="")
t_grocery_amount <- t_grocery_amount[,-1]

## t_money_times
train_trade_t_money_times <- train_trade_target_final[,c(1,2,5)]
t_money_times <- dcast(train_trade_t_money_times, target_acc_id~trade_week)
t_money_times[t_money_times==0] <- NA
colnames(t_money_times)[2:9] <- paste('week',colnames(t_money_times[2:9]),'_t_money_times',sep="")
t_money_times <- t_money_times[,-1]

## t_money_amount
train_trade_t_money_amount <- train_trade_target_final[,c(1,2,6)]
t_money_amount <- dcast(train_trade_t_money_amount, target_acc_id~trade_week)
t_money_amount[t_money_amount==0] <- NA
colnames(t_money_amount)[2:9] <- paste('week',colnames(t_money_amount[2:9]),'_t_money_amount',sep="")
t_money_amount <- t_money_amount[,-1]

## t_etc_times
train_trade_t_etc_times <- train_trade_target_final[,c(1,2,7)]
t_etc_times <- dcast(train_trade_t_etc_times, target_acc_id~trade_week)
t_etc_times[t_etc_times==0] <- NA
colnames(t_etc_times)[2:9] <- paste('week',colnames(t_etc_times[2:9]),'_t_etc_times',sep="")
t_etc_times <- t_etc_times[,-1]

## t_etc_amount
train_trade_t_etc_amount <- train_trade_target_final[,c(1,2,8)]
t_etc_amount <- dcast(train_trade_t_etc_amount, target_acc_id~trade_week)
t_etc_amount[t_etc_amount==0] <- NA
colnames(t_etc_amount)[2:9] <- paste('week',colnames(t_etc_amount[2:9]),'_t_etc_amount',sep="")
t_etc_amount <- t_etc_amount[,-1]

## Target Merge
train_trade_target_merged <- cbind(t_grocery_times, t_grocery_amount, t_money_times, t_money_amount, t_etc_times, t_etc_amount)
colnames(train_trade_target_merged)[1] <- "acc_id"

train_APST <- merge(train_APS, train_trade_target_merged, by="acc_id", all.x=T)

glimpse(train_APST)

lgwk_lm_dat <- fread("train_agpl_100_t.csv")
lgwk_lm <- lgwk_lm_dat[,c(1,2,39,82)]

train_APSTlm <-  merge(train_APST, lgwk_lm, by="acc_id", all.x=T)

## Remove NA
trainset <- train_APSTlm
trainset <- as.data.frame(trainset)
glimpse(trainset)

colnames(trainset)

activityset <- trainset[,10:201]

for(i in 1:ncol(activityset))
{
  activityset[is.na(activityset[,i]), i] <- min(activityset[,i], na.rm = TRUE)
}

trainset[,10:201] <- activityset

cntset <- trainset[,2:9]
cntset[is.na(cntset)] <- 0
trainset[,2:9] <- cntset

trainset$lm[is.na(trainset$lm)] <- 0

tradeset <- testset[,202:233]
tradeset[is.na(tradeset)] <- 0

partyset <- testset[,244:259]
partyset[is.na(partyset)] <- 0

testset[,202:233] <- tradeset
testset[,244:259] <- partyset

## Maxleap
train_activity <- fread("train_activity.csv")
train_activity <- train_activity[,2:1]
train_activity$cnt <- 1

train_wide <- dcast(train_activity, acc_id~wk)
train_wide_noid <- train_wide[,-1]
train_wide_noid[is.na(train_wide_noid)] <- 0

rle(train_wide_noid[63174,])$values

colnames(rle(train_wide_noid[63174,])$values)[max(which(rle(train_wide_noid[63174,])$values==0))]

colnames(rle(train_wide_noid[63174,])$values)[max(which(rle(train_wide_noid[63174,])$values==1)[-length(which(rle(train_wide_noid[63174,])$values==1))])]

x <- matrix(NA,100000,1)

for (i in 1:nrow(train_wide_noid))
{
  x[i,1] <- as.numeric(colnames(rle(train_wide_noid[i,])$values)[max(which(rle(train_wide_noid[i,])$values==0))]) - as.numeric(colnames(rle(train_wide_noid[i,])$values)[max(which(rle(train_wide_noid[i,])$values==1)[-length(which(rle(train_wide_noid[i,])$values==1))])])
}

y <- matrix(NA,100000,1)

for (i in 1:nrow(train_wide_noid))
{
  y[i,1] <- as.numeric(colnames(rle(train_wide_noid[i,])$values)[min(which(rle(train_wide_noid[i,])$values==0))]) - as.numeric(colnames(rle(train_wide_noid[i,])$values)[min(which(rle(train_wide_noid[i,])$values==1)[-length(which(rle(train_wide_noid[i,])$values==1))])])
}

train <- fread('train_activity.csv')
iw <- train[,c(1,2)]
w1 <- iw
w1$wk <- 1

library(dplyr)

w1 <- w1 %>% group_by(acc_id) %>% summarise_all(sum)
cnt_non_wk <- matrix(data=0, 1, 100000)

notplay <- data.frame(acc_id=w1$acc_id, cnt_non_wk)

id_wk <- iw[iw$acc_id==id[9], c('wk')] 
notplay[1,2] <- 8-w1$wk[9]-(min(id_wk$wk)-1)

w1$acc_id[9] == notplay$acc_id[9]

for (i in 1:100000) {
  id_wk <- iw[w1$acc_id[i]==iw$acc_id, c('wk')]
  notplay[i,2]<- 8 - w1$wk[i] - (min(id_wk$wk)-1)
  print(i)
}

### PARTY
install.packages("readr")
library(readr)
library(dplyr)
library(tidyr)
getwd()
setwd("C:/Users/Administrator/Desktop")
blade_party<-read_csv('train_party.csv')
practice<-blade_party[1:1000,]
glimpse(blade_party)

getwd()
install.packages('data.table')
library(data.table)
train_label<-fread('test_label.csv',header=T)
train_label<-train_label[,c(1,2)]
glimpse(train_label)
head(train_label)
train_label<-as.data.frame(train_label)
colnames(train_label)<-c('id','label')


week2_party_count<-as.data.frame(week2_party_count)
head(week2_party_count)



week1_party <-blade_party[which(blade_party$party_start_week==1),] 
week2_party <-blade_party[which(blade_party$party_start_week==2),]
week3_party <-blade_party[which(blade_party$party_start_week==3),]
week4_party <-blade_party[which(blade_party$party_start_week==4),]
week5_party <-blade_party[which(blade_party$party_start_week==5),]
week6_party <-blade_party[which(blade_party$party_start_week==6),]
week7_party <-blade_party[which(blade_party$party_start_week==7),]
week8_party <-blade_party[which(blade_party$party_start_week==8),]

#####################################################################################################################################################
#week1에서 평일에 시작한 파티들
week1_party_weekday <-rbind(week1_party[which(week1_party$party_start_day==1),],week1_party[which(week1_party$party_start_day==2),]
                            ,week1_party[which(week1_party$party_start_day==3),],week1_party[which(week1_party$party_start_day==6),],
                            week1_party[which(week1_party$party_start_day==7),]
)

#week1에서 주말에 시작한 파티들
week1_party_weekend <-rbind(week1_party[which(week1_party$party_start_day==4),],week1_party[which(week1_party$party_start_day==5),])
#id별로 집계하기
a <- week1_party_weekday %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
head(a)
b <- week1_party_weekend %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
#train_label과 병합하기
week1_weekday_partycnt<-merge(train_label,a,  by='id',all.x=T)
week1_weekday_partycnt<-week1_weekday_partycnt[,-2]
head(week1_weekday_partycnt)

week1_weekend_partycnt<-merge(train_label,b,  by='id',all.x=T)
week1_weekend_partycnt<-week1_weekend_partycnt[,-2]
head(week1_weekend_partycnt)


#####################################################################################################################################################
#week2에서 평일에 시작한 파티들
week2_party_weekday <-rbind(week2_party[which(week2_party$party_start_day==1),],week2_party[which(week2_party$party_start_day==2),]
                            ,week2_party[which(week2_party$party_start_day==3),],week2_party[which(week2_party$party_start_day==6),],
                            week2_party[which(week2_party$party_start_day==7),]
)

#week2에서 주말에 시작한 파티들
week2_party_weekend <-rbind(week2_party[which(week2_party$party_start_day==4),],week2_party[which(week2_party$party_start_day==5),])
#id별로 집계하기
a <- week2_party_weekday %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
head(a)
b <- week2_party_weekend %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
#train_label과 병합하기
week2_weekday_partycnt<-merge(train_label,a,  by='id',all.x=T)
week2_weekday_partycnt<-week2_weekday_partycnt[,-2]
head(week2_weekday_partycnt)

week2_weekend_partycnt<-merge(train_label,b,  by='id',all.x=T)
week2_weekend_partycnt<-week2_weekend_partycnt[,-2]
head(week2_weekend_partycnt)


#####################################################################################################################################################
#week3에서 평일에 시작한 파티들
week3_party_weekday <-rbind(week3_party[which(week3_party$party_start_day==1),],week3_party[which(week3_party$party_start_day==2),]
                            ,week3_party[which(week3_party$party_start_day==3),],week3_party[which(week3_party$party_start_day==6),],
                            week3_party[which(week3_party$party_start_day==7),]
)

#week3에서 주말에 시작한 파티들
week3_party_weekend <-rbind(week3_party[which(week3_party$party_start_day==4),],week3_party[which(week3_party$party_start_day==5),])
#id별로 집계하기
a <- week3_party_weekday %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
head(a)
b <- week3_party_weekend %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
#train_label과 병합하기
week3_weekday_partycnt<-merge(train_label,a,  by='id',all.x=T)
week3_weekday_partycnt<-week3_weekday_partycnt[,-2]
head(week3_weekday_partycnt)

week3_weekend_partycnt<-merge(train_label,b,  by='id',all.x=T)
week3_weekend_partycnt<-week3_weekend_partycnt[,-2]
head(week3_weekend_partycnt)


#####################################################################################################################################################
#week4에서 평일에 시작한 파티들
week4_party_weekday <-rbind(week4_party[which(week4_party$party_start_day==1),],week4_party[which(week4_party$party_start_day==2),]
                            ,week4_party[which(week4_party$party_start_day==3),],week4_party[which(week4_party$party_start_day==6),],
                            week4_party[which(week4_party$party_start_day==7),]
)

#week4에서 주말에 시작한 파티들
week4_party_weekend <-rbind(week4_party[which(week4_party$party_start_day==4),],week4_party[which(week4_party$party_start_day==5),])
#id별로 집계하기
a <- week4_party_weekday %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
head(a)
b <- week4_party_weekend %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
#train_label과 병합하기
week4_weekday_partycnt<-merge(train_label,a,  by='id',all.x=T)
week4_weekday_partycnt<-week4_weekday_partycnt[,-2]
head(week4_weekday_partycnt)

week4_weekend_partycnt<-merge(train_label,b,  by='id',all.x=T)
week4_weekend_partycnt<-week4_weekend_partycnt[,-2]
head(week4_weekend_partycnt)


#####################################################################################################################################################
#week5에서 평일에 시작한 파티들
week5_party_weekday <-rbind(week5_party[which(week5_party$party_start_day==1),],week5_party[which(week5_party$party_start_day==2),]
                            ,week5_party[which(week5_party$party_start_day==3),],week5_party[which(week5_party$party_start_day==6),],
                            week5_party[which(week5_party$party_start_day==7),]
)

#week5에서 주말에 시작한 파티들
week5_party_weekend <-rbind(week5_party[which(week5_party$party_start_day==4),],week5_party[which(week5_party$party_start_day==5),])
#id별로 집계하기
a <- week5_party_weekday %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
head(a)
b <- week5_party_weekend %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
#train_label과 병합하기
week5_weekday_partycnt<-merge(train_label,a,  by='id',all.x=T)
week5_weekday_partycnt<-week5_weekday_partycnt[,-2]
head(week5_weekday_partycnt)

week5_weekend_partycnt<-merge(train_label,b,  by='id',all.x=T)
week5_weekend_partycnt<-week5_weekend_partycnt[,-2]
head(week5_weekend_partycnt)


#####################################################################################################################################################
#week6에서 평일에 시작한 파티들
week6_party_weekday <-rbind(week6_party[which(week6_party$party_start_day==1),],week6_party[which(week6_party$party_start_day==2),]
                            ,week6_party[which(week6_party$party_start_day==3),],week6_party[which(week6_party$party_start_day==6),],
                            week6_party[which(week6_party$party_start_day==7),]
)

#week6에서 주말에 시작한 파티들
week6_party_weekend <-rbind(week6_party[which(week6_party$party_start_day==4),],week6_party[which(week6_party$party_start_day==5),])
#id별로 집계하기
a <- week6_party_weekday %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
head(a)
b <- week6_party_weekend %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
#train_label과 병합하기
week6_weekday_partycnt<-merge(train_label,a,  by='id',all.x=T)
week6_weekday_partycnt<-week6_weekday_partycnt[,-2]
head(week6_weekday_partycnt)

week6_weekend_partycnt<-merge(train_label,b,  by='id',all.x=T)
week6_weekend_partycnt<-week6_weekend_partycnt[,-2]
head(week6_weekend_partycnt)


#####################################################################################################################################################
#week7에서 평일에 시작한 파티들
week7_party_weekday <-rbind(week7_party[which(week7_party$party_start_day==1),],week7_party[which(week7_party$party_start_day==2),]
                            ,week7_party[which(week7_party$party_start_day==3),],week7_party[which(week7_party$party_start_day==6),],
                            week7_party[which(week7_party$party_start_day==7),]
)

#week7에서 주말에 시작한 파티들
week7_party_weekend <-rbind(week7_party[which(week7_party$party_start_day==4),],week7_party[which(week7_party$party_start_day==5),])
#id별로 집계하기
a <- week7_party_weekday %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
head(a)
b <- week7_party_weekend %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
#train_label과 병합하기
week7_weekday_partycnt<-merge(train_label,a,  by='id',all.x=T)
week7_weekday_partycnt<-week7_weekday_partycnt[,-2]
head(week7_weekday_partycnt)

week7_weekend_partycnt<-merge(train_label,b,  by='id',all.x=T)
week7_weekend_partycnt<-week7_weekend_partycnt[,-2]
head(week7_weekend_partycnt)


#####################################################################################################################################################
#week8에서 평일에 시작한 파티들
week8_party_weekday <-rbind(week8_party[which(week8_party$party_start_day==1),],week8_party[which(week8_party$party_start_day==2),]
                            ,week8_party[which(week8_party$party_start_day==3),],week8_party[which(week8_party$party_start_day==6),],
                            week8_party[which(week8_party$party_start_day==7),]
)

#week8에서 주말에 시작한 파티들
week8_party_weekend <-rbind(week8_party[which(week8_party$party_start_day==4),],week8_party[which(week8_party$party_start_day==5),])
#id별로 집계하기
a <- week8_party_weekday %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
head(a)
b <- week8_party_weekend %>% mutate(id=strsplit(as.character(hashed), ",")) %>% unnest(id) %>% group_by(id) %>% summarize(party_cnt=n())
#train_label과 병합하기
week8_weekday_partycnt<-merge(train_label,a,  by='id',all.x=T)
week8_weekday_partycnt<-week8_weekday_partycnt[,-2]
head(week8_weekday_partycnt)

week8_weekend_partycnt<-merge(train_label,b,  by='id',all.x=T)
week8_weekend_partycnt<-week8_weekend_partycnt[,-2]
head(week8_weekend_partycnt)

##1~8주차 합치기
train_party_count<-cbind(week1_weekday_partycnt,week1_weekend_partycnt,week2_weekday_partycnt,week2_weekend_partycnt,week3_weekday_partycnt,week3_weekend_partycnt,
                         week4_weekday_partycnt,week4_weekend_partycnt,week5_weekday_partycnt,week5_weekend_partycnt,week6_weekday_partycnt,week6_weekend_partycnt,
                         week7_weekday_partycnt,week7_weekend_partycnt,week8_weekday_partycnt,week8_weekend_partycnt)

colnames(train_party_count)
train_party_count<-train_party_count[,-c(3,5,7,9,11,13,15,  17,19,21,23,25,27,29,31)]
head(train_party_count)

train_party_count[is.na(train_party_count)]<- 0

colnames(train_party_count)<-c('id','week1_weekday_partycnt','week1_weekend_partycnt','week2_weekday_partycnt','week2_weekend_partycnt',
                               'week3_weekday_partycnt','week3_weekend_partycnt',
                               'week4_weekday_partycnt','week4_weekend_partycnt','week5_weekday_partycnt','week5_weekend_partycnt',
                               'week6_weekday_partycnt','week6_weekend_partycnt',
                               'week7_weekday_partycnt','week7_weekend_partycnt','week8_weekday_partycnt','week8_weekend_partycnt')

write.csv(train_party_count,'train_party_count.csv',row.names = F)

library(data.table)
library(dplyr)
library(reshape2)
library(tidyr)

## Read File
train_party <- fread("train_party.csv")
glimpse(train_party)

## Manage party
train_party <- train_party[,c(1,3,7)]

train_party_unnested <- train_party %>% mutate(acc_id=strsplit(as.character(hashed), ",")) %>% unnest(acc_id)
train_party_unnested$hashed <- NULL

train_activity <- fread("train_activity.csv")
nameslist <- as.data.frame(unique(train_activity$acc_id))
colnames(nameslist) <- "acc_id"
train_party_unnested_use <- merge(nameslist, train_party_unnested, by="acc_id", all.x=T)
train_party_unnested_use <- train_party_unnested_use[com]

## Get Time
train_party_unnested_use$party_hour <- substr(train_party_unnested_use$party_start_time,1,2)
train_party_unnested_use$party_hour <- as.numeric(train_party_unnested_use$party_hour)

train_party_unnested_use$party_hour[train_party_unnested_use$party_hour>=08 & train_party_unnested_use$party_hour<16] <- "daytime"
train_party_unnested_use$party_hour[train_party_unnested_use$party_hour>=16 & train_party_unnested_use$party_hour<24] <- "nighttime"
train_party_unnested_use$party_hour[train_party_unnested_use$party_hour>=0 & train_party_unnested_use$party_hour<08] <- "dawntime"
train_party_unnested_use$party_cnt <- 1

train_party_hour <- train_party_unnested_use %>% group_by(acc_id, party_start_week, party_hour) %>% summarize(party_cnt=sum(party_cnt))

train_party_reshaped <- dcast(train_party_hour, acc_id+party_start_week~party_hour)

## Reshape - dawntime
train_party_dawntime <- train_party_reshaped[,c(1,2,3)]
dawntime <- dcast(train_party_dawntime, acc_id~party_week)
colnames(dawntime)[2:9] <- paste('week',colnames(dawntime)[2:9],'_tr_dawntime',sep="")

## Reshape - daytime
train_party_daytime <- train_party_reshaped[,c(1,2,4)]
daytime <- dcast(train_party_daytime, acc_id~party_week)
colnames(daytime)[2:9] <- paste('week',colnames(daytime)[2:9],'_tr_daytime',sep="")
daytime <- daytime[,-1]

## Reshape - nighttime
train_party_nighttime <- train_party_reshaped[,c(1,2,5)]
nighttime <- dcast(train_party_nighttime, acc_id~party_week)
colnames(nighttime)[2:9] <- paste('week',colnames(nighttime)[2:9],'_tr_nighttime',sep="")
nighttime <- nighttime[,-1]

## Bind
train_party_time <- cbind(dawntime,daytime,nighttime)

glimpse(train_party_time)

## Merge with original data
train_activity <- fread("train_jjinmak.csv")
train_party_time_final <- merge(train_activity, train_party_time, by="acc_id", all.x = T)
write.csv(train_party_time_final, "train_jjinmak_tt.csv", row.names = F)

## Read File
train_trade <- fread("train_trade.csv")

## Unique
train_trade <- unique(train_trade)

## Split Data
train_trade_source <- train_trade[,c(4,1,2)]
train_trade_target <- train_trade[,c(5,1,2)]

## Change column names
colnames(train_trade_source) <- c("acc_id", "party_start_week", "party_start_day")
colnames(train_trade_target) <- c("acc_id", "party_start_week", "party_start_day")

## Rbind data with Party data
train_party_binded <- rbind(train_trade_source, train_trade_target)

## Count Weekend for source
train_party_binded$playamount <- 1
train_party_binded$party_start_day[train_party_binded$party_start_day==1] <- "weekday"
train_party_binded$party_start_day[train_party_binded$party_start_day==2] <- "weekday"
train_party_binded$party_start_day[train_party_binded$party_start_day==3] <- "weekday"
train_party_binded$party_start_day[train_party_binded$party_start_day==6] <- "weekday"
train_party_binded$party_start_day[train_party_binded$party_start_day==7] <- "weekday"
train_party_binded$party_start_day[train_party_binded$party_start_day==4] <- "weekend"
train_party_binded$party_start_day[train_party_binded$party_start_day==5] <- "weekend"

train_party_binded_sum <- train_party_binded %>% group_by(acc_id, party_start_week, party_start_day) %>% summarize(playday=sum(playamount))

train_party_binded_reshaped <- dcast(train_party_binded_sum, acc_id+party_start_week~party_start_day)

## Weekday
train_party_binded_reshaped_weekday <- train_party_binded_reshaped[,c(1,2,3)]
weekday <- dcast(train_party_binded_reshaped_weekday, acc_id~party_start_week)
colnames(weekday)[2:9] <- paste('week',colnames(weekday)[2:9],'_tr_weekday',sep="")

## Weekend
train_party_binded_reshaped_weekend <- train_party_binded_reshaped[,c(1,2,4)]
weekend <- dcast(train_party_binded_reshaped_weekend, acc_id~party_start_week)
colnames(weekend)[2:9] <- paste('week',colnames(weekend)[2:9],'_tr_weekend',sep="")
weekend <- weekend[,-1]
weekdayend <- cbind(weekday,weekend)
train_activity_reshaped <- fread("train_activity_reshaped.csv")
train_activity_reshaped_weekdayend <- merge(train_activity_reshaped,weekdayend, by="acc_id", all.x=T)
write.csv(train_activity_reshaped_weekdayend, "train_jjinmak_tradeday.csv",row.names=F)

library(reshape2)
library(data.table)
library(dplyr)

### Payment

train_payment <- read.csv("train_payment.csv")
colnames(train_payment)[1]<-'wk'

## payment
train_payment_payment_amount <- train_payment[,c(1,2,3)]
payment_amount <- dcast(train_payment_payment_amount, acc_id~wk)
colnames(payment_amount)[2:9] <- paste('week',colnames(payment_amount)[2:9],'_payment_amount',sep="")

# write.csv(payment_amount,'train_payment_reshaped.csv',row.names = F)

###################################################
###################################################
############## MODELING ###########################
###################################################
###################################################

## Bayesian Optimization

#king =read.csv('train_king.csv')
dat = read.csv('r5_prob_grocery.csv')
xgfold<-read.csv('xgfold.csv')
rf<-read.csv('rf_prob.csv')
dat<-cbind(dat,rf)
#dat$label = king$label
dat$label = as.numeric(dat$label)-1

dat = xgb.DMatrix(as.matrix(dat[,-10]),label = dat$label)

xgb_cv_bayes <- function(max.depth, min_child_weight, subsample,colsample) {
  cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.05,
                             max_depth = max.depth,
                             min_child_weight = min_child_weight,
                             subsample = subsample, colsample_bytree = colsample,
                             objective = "multi:softmax",
                             num_class = 4),
               feval = customf1,
               data = dat, nround = 10000,
               folds = xgfold, prediction = TRUE, showsd = TRUE,
               early_stopping_rounds = 200, maximize = TRUE, verbose = 0, nthread = 4) ### Mr.Kwon => nthread= 4
  list(Score = cv$evaluation_log[,max(test_error_mean)],
       Pred = cv$pred)
}

OPT_Res3 <- BayesianOptimization(xgb_cv_bayes,
                                 bounds = list(max.depth = c(5L, 11L),
                                               min_child_weight = c(1L, 10L),
                                               subsample = c(0.5, 1),
                                               colsample = c(0.4, 1)),
                                 init_points = 20, n_iter = 50,
                                 acq = "ucb", kappa = 2.576, eps = 0.0,
                                 verbose = TRUE)

## Level 1 Models

## XGBoost 

xgb_params <- list(objective = "multi:softprob",
                   max.depth = 9, 
                   min_child_weight = 2,
                   subsample = 0.9103, 
                   colsample_bytree = 0.7975,
                   eta = 0.1,
                   num_class = 4)

xg = xgb.train(params = xgb_params, data = dat, nrounds = 468)

## XGBoost (Weight_1.1)

dat_wt = dat
wt = dat_wt[,c(1,478)]
wt[which(dat_wt$label==1),1] = 1.1
wt[which(dat_wt$label!=1),1] = 1
names(wt)[1] = 'weight'

dat_wt = xgb.DMatrix(as.matrix(dat_wt[,-478]),label=dat_wt$label,weight = wt$weight)
xg_wt = xgb.train(params = xgb_params, data = dat_wt, nrounds = 468)

## Random Forest
rf = randomForest(label~.,dat = train, mtry = 26, ntree = 500)

## Extra Trees
et <- extraTrees(train[,-478], train$label, ntree = 500, mtry=25, numRandomCuts = 25)

## K-NN
k2 = knn(train[,-478],test,train[,478],k=2)
k4 = knn(train[,-478],test,train[,478],k=4)
k8 = knn(train[,-478],test,train[,478],k=8)
k16 = knn(train[,-478],test,train[,478],k=16)
k32 = knn(train[,-478],test,train[,478],k=32)
k64 = knn(train[,-478],test,train[,478],k=64)
k128 = knn(train[,-478],test,train[,478],k=128)

## Level 2 Model
## Aggregate Level 1 models and add week8_s_grocery_times

train_level2 = read.csv(choose.files())
test_level2 = read.csv(choose.files())

xgb_params2 <- list(objective = "multi:softprob",
                    subsample = 0.7764,
                    colsample_bytree = 0.7152,
                    eta = 0.01,
                    max_depth = 9,
                    min_child_weight = 7,
                    num_class = 4)


stack_model = xgb.train(params = xgb_params2, data = train_level2, nrounds = 504)

final = predict(stack_model,newdata = xgb.DMatrix(as.matrix(test_level2)))

final = as.data.frame(split(final,1:4))
colnames(final) <- c("2month","month","retained","week")

final_cut = final
final_cut$month = final_cut$month + 0.02
final_cut$label = max.col(final_cut)

final_cut$label[final_cut$label==1] = '2month'
final_cut$label[final_cut$label==2] = 'month'
final_cut$label[final_cut$label==3] = 'retained'
final_cut$label[final_cut$label==4] = 'week'

traindata = read.csv(choose.files()) ## Casted activity file
id =traindata[,1:2]
id$label = final_cut$label
id = id[,c(1,3)]
write.csv(id,'Final.csv',row.names=F)


###############################
######## BAYESIAN 3D###########
###############################
library(rgl)
library(GA)

par(mfrow=c(1,1))
library(plot3D)

library(fields)



bay<-read.csv('bay.csv') # 베이지안 첫 iter 
bay<-bay+0.002
nad<-as.data.frame(matrix(NA,3,18))
colnames(nad)<-colnames(bay)
bays<-rbind(bay,nad)

for ( i in c(1:18)){
  bays[which(is.na(bays[,i])),i]<-0.727
  
}
bays_m<-as.matrix(bays)

x<-c(1:15)
y<-c(1:18)




persp3D(x, y , bays_m, color.palette = heat.colors, phi = 30, theta = 225, 
        box = TRUE, border = NA, shade = .1,expand = 0.65,zlim=c(0.727,0.738)
        ,xlab ='max_dapth', ylab ='min_child_weight', zlab ='F-Score',resfac = 6)



bay2<-read.csv('bay2.csv') # 베이지안 iter 25 % 진행 
bay2<-bay2+0.002
nad<-as.data.frame(matrix(NA,3,18))
colnames(nad)<-colnames(bay2)
bays<-rbind(bay2,nad)

for ( i in c(1:18)){
  bays[which(is.na(bays[,i])),i]<-0.727
  
}
bays_m2<-as.matrix(bays)

persp3D(x, y , bays_m2, color.palette = heat.colors, phi = 30, theta = 225, 
        box = TRUE, border = NA, shade = .1,expand = 0.65,zlim=c(0.727,0.738)
        ,xlab ='max_dapth', ylab ='min_child_weight', zlab ='F-Score',resfac =8)






bay3<-read.csv('bay3.csv')# 베이지안 iter 50 % 진행 
bay3<-bay3+0.002
nad<-as.data.frame(matrix(NA,3,18))
colnames(nad)<-colnames(bay3)
bays<-rbind(bay3,nad)

for ( i in c(1:18)){
  bays[which(is.na(bays[,i])),i]<-0.727
  
}
bays_m3<-as.matrix(bays)

persp3D(x, y , bays_m3, color.palette = heat.colors, phi = 30, theta = 225, 
        box = TRUE, border = NA, shade = .1,expand = 0.65,zlim=c(0.727,0.738)
        ,xlab ='max_dapth', ylab ='min_child_weight', zlab ='F-Score',resfac = 8)



bay4<-read.csv('bay4.csv')# 베이지안 iter 75  % 진행 
bay4<-bay4+0.002
nad<-as.data.frame(matrix(NA,3,18))
colnames(nad)<-colnames(bay4)
bays<-rbind(bay4,nad)

for ( i in c(1:18)){
  bays[which(is.na(bays[,i])),i]<-0.727
  
}
bays_m4<-as.matrix(bays)

persp3D(x, y , bays_m4, color.palette = heat.colors, phi = 30, theta = 225, 
        box = TRUE, border = NA, shade = .1,expand = 0.65,zlim=c(0.727,0.738)
        ,xlab ='max_dapth', ylab ='min_child_weight', zlab ='F-Score',resfac = 8)



bay5<-read.csv('bay5.csv') # 베이지안 iter 100 % 진행 
bay5<-bay5+0.002
nad<-as.data.frame(matrix(NA,3,18))
colnames(nad)<-colnames(bay5)
bays<-rbind(bay5,nad)

for ( i in c(1:18)){
  bays[which(is.na(bays[,i])),i]<-0.727
  
}
bays_m5<-as.matrix(bays)

persp3D(x, y , bays_m5, color.palette = heat.colors, phi = 30, theta = 225, 
        box = TRUE, border = NA, shade = .1,expand = 0.65,zlim=c(0.727,0.738)
        ,xlab ='max_dapth', ylab ='min_child_weight', zlab ='F-Score',resfac = 8)