library(ggplot2);library(dplyr);
df <- read.csv(file = 'Roll_User_Data.csv')
#######################SURVEY STATISTICS#######################
#lane, piegraph
dflane <- as.data.frame(table(df$lane))

ggplot(dflane, aes(x="", y=dflane$Freq, fill=dflane$Var1)) +
         geom_bar(stat="identity", width=1, color="white") +
         coord_polar("y", start=0)+
  labs(title = "Lane Piechart")+
  labs(fill = "Lane")+
  xlab("") +
  ylab("")

#gender
table(df$gender)

#born_year, bargraph
dfborn <- as.data.frame(table(df$born_year))
ggplot(dfborn, aes(x=dfborn$Var1, y=dfborn$Freq))+
  geom_bar(stat = "identity", fill="steelblue")+
  labs(title = "Born Year Bargraph")+
  xlab("Born year") +
  ylab("Freq")

#game_started_year, bargraph
dfgamestart <- as.data.frame(table(df$game_started_year))
ggplot(dfgamestart, aes(x=dfgamestart$Var1, y=dfgamestart$Freq))+
  geom_bar(stat = "identity", fill="steelblue")+
  labs(title = "Game Started Year Bargraph")+
  xlab("Game Started Year") +
  ylab("Freq")

#MBTI, bargraph
dfMBTI <- as.data.frame(table(df$MBTI))
ggplot(dfMBTI, aes(x=dfMBTI$Var1, y=dfMBTI$Freq))+
  geom_bar(stat = "identity", fill="steelblue")+
  labs(title = "MBTI Bargraph")+
  xlab("MBTI") +
  ylab("Freq")

#######################LANE & MBTI#######################
LaneMBTI_df<-df[,c(11,15)]
LaneMBTI <- data.frame()
#delete mbti is NULL, lane is Null
for (row in 1:nrow(LaneMBTI_df)){
  lane <- LaneMBTI_df[row, "lane"]
  mbti <- LaneMBTI_df[row, "MBTI"]
  mbti1 <- substring(LaneMBTI_df[row, "MBTI"],1,1)
  mbti2 <- substring(LaneMBTI_df[row, "MBTI"],2,2)
  mbti3 <- substring(LaneMBTI_df[row, "MBTI"],3,3)
  mbti4 <- substring(LaneMBTI_df[row, "MBTI"],4,4)
  if (mbti != "" & lane != "None"){
    LaneMBTI <- rbind(LaneMBTI, c(lane, mbti1, mbti2, mbti3, mbti4))
  }
}
names(LaneMBTI) <- c("lane", "M", "B", "T", "I")
#table lane, M
tb_lm <- table(LaneMBTI$lane, LaneMBTI$M)
#table lane, B
tb_lb <- table(LaneMBTI$lane, LaneMBTI$B)
#table lane, T
tb_lt <- table(LaneMBTI$lane, LaneMBTI$T)
#table lane, I
tb_li <- table(LaneMBTI$lane, LaneMBTI$I)
#chi-square test
chisq.test(tb_lm)
chisq.test(tb_lb)
chisq.test(tb_lt)
chisq.test(tb_li)


#######################MOST CHAMPION & MBTI#######################
ChampMBTI_df <- df[,c(8,11,15)]

ChampMBTI <- data.frame()
ChampMBTI_h <- data.frame()
for (row in 1:nrow(ChampMBTI_df)){
  champ1 <- ChampMBTI_df[row, "champ1"]
  keyChamp1 <- as.integer(champ1)
  lane <- ChampMBTI_df[row, "lane"]
  mbti <- ChampMBTI_df[row, "MBTI"]
  mbti1 <- substring(ChampMBTI_df[row, "MBTI"],1,1)
  mbti2 <- substring(ChampMBTI_df[row, "MBTI"],2,2)
  mbti3 <- substring(ChampMBTI_df[row, "MBTI"],3,3)
  mbti4 <- substring(ChampMBTI_df[row, "MBTI"],4,4)
  if (mbti != "" & lane != "None"){
    ChampMBTI <- rbind(ChampMBTI, c(champ1, mbti1, mbti2, mbti3, mbti4, kv_df_All[keyChamp1], lane))
    ChampMBTI_h <- rbind(ChampMBTI, c(champ1, mbti1, mbti2, mbti3, mbti4, kv_df_AllH[keyChamp1], lane))
  }
}
names(ChampMBTI) <- c("champ1", "M", "B", "T", "I", "cluster", "lane")
names(ChampMBTI_h) <- c("champ1", "M", "B", "T", "I", "cluster", "lane")

#table cluster, M
tb_cm <- table(ChampMBTI$cluster, ChampMBTI$M)
tb_cm2 <- table(ChampMBTI_h$cluster, ChampMBTI_h$M)
#table cluster, B
tb_cb <- table(ChampMBTI$cluster, ChampMBTI$B)
tb_cb2 <- table(ChampMBTI_h$cluster, ChampMBTI_h$B)
#table cluster, T
tb_ct <- table(ChampMBTI$cluster, ChampMBTI$T)
tb_ct2 <- table(ChampMBTI_h$cluster, ChampMBTI_h$T)
#table cluster, I
tb_ci <- table(ChampMBTI$cluster, ChampMBTI$I)
tb_ci2 <- table(ChampMBTI_h$cluster, ChampMBTI_h$I)
#chi-square test
chisq.test(tb_cm)
chisq.test(tb_cb)
chisq.test(tb_ct)
chisq.test(tb_ci)

chisq.test(tb_cm2)
chisq.test(tb_cb2)
chisq.test(tb_ct2)
chisq.test(tb_ci2)

#######################Champ&line&MBTI ======kmeans=======#######################
ChampMBTI_Top<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character())
ChampMBTI_Jg<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character())
ChampMBTI_Mid<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character())
ChampMBTI_Bot<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character())
ChampMBTI_Sup<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character());

for (row in 1:nrow(ChampMBTI)){
  champ1 <- ChampMBTI[row, "champ1"]
  keyChamp1 <- as.integer(champ1)
  lane <- ChampMBTI[row, "lane"]
  M <- ChampMBTI[row, "M"]
  B <- ChampMBTI[row, "B"]
  t <- ChampMBTI[row, "T"]
  I <- ChampMBTI[row, "I"]

  if (lane == "TOP"){
    if (is.null(kv_df_Top[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_Top)
      ChampMBTI_Top <- rbind(ChampMBTI_Top, df_if)
      }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_Top[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_Top)
      ChampMBTI_Top <- rbind(ChampMBTI_Top, df_if)
    }
  }
  
  if (lane == "JUNGLE"){
    if (is.null(kv_df_Jg[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_Jg)
      ChampMBTI_Jg <- rbind(ChampMBTI_Jg, df_if)
      }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_Jg[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_Jg)
      ChampMBTI_Jg <- rbind(ChampMBTI_Jg, df_if)
      }
  }
  
  if (lane == "MID"){
    if (is.null(kv_df_Mid[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_Mid)
      ChampMBTI_Mid <- rbind(ChampMBTI_Mid, df_if)
      }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_Mid[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_Mid)
      ChampMBTI_Mid <- rbind(ChampMBTI_Mid, df_if)
      }
  }
  
  if (lane == "ADC"){
    if (is.null(kv_df_Bot[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_Bot)
      ChampMBTI_Bot <- rbind(ChampMBTI_Bot, df_if)
      }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_Bot[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_Bot)
      ChampMBTI_Bot <- rbind(ChampMBTI_Bot, df_if)
      }
  }
  
  if (lane == "SUPPORT"){
    if (is.null(kv_df_Sup[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_Sup)
      ChampMBTI_Sup <- rbind(ChampMBTI_Sup, df_if)
      }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_Sup[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_Sup)
      ChampMBTI_Sup <- rbind(ChampMBTI_Sup, df_if)
      }
  }
}

#table Top cluster, M,B,T,I
tb_cTm <- table(ChampMBTI_Top$cluster, ChampMBTI_Top$m);tb_cTb <- table(ChampMBTI_Top$cluster, ChampMBTI_Top$b);tb_cTt <- table(ChampMBTI_Top$cluster, ChampMBTI_Top$t);tb_cTi <- table(ChampMBTI_Top$cluster, ChampMBTI_Top$i);
#table Jg cluster, M,B,T,I
tb_cJm <- table(ChampMBTI_Jg$cluster, ChampMBTI_Jg$m);tb_cJb <- table(ChampMBTI_Jg$cluster, ChampMBTI_Jg$b);tb_cJt <- table(ChampMBTI_Jg$cluster, ChampMBTI_Jg$t);tb_cJi <- table(ChampMBTI_Jg$cluster, ChampMBTI_Jg$i);
#table Mid cluster, M,B,T,I
tb_cMm <- table(ChampMBTI_Mid$cluster, ChampMBTI_Mid$m);tb_cMb <- table(ChampMBTI_Mid$cluster, ChampMBTI_Mid$b);tb_cMt <- table(ChampMBTI_Mid$cluster, ChampMBTI_Mid$t);tb_cMi <- table(ChampMBTI_Mid$cluster, ChampMBTI_Mid$i);
#table Bot cluster, M,B,T,I
tb_cBm <- table(ChampMBTI_Bot$cluster, ChampMBTI_Bot$m);tb_cBb <- table(ChampMBTI_Bot$cluster, ChampMBTI_Bot$b);tb_cBt <- table(ChampMBTI_Bot$cluster, ChampMBTI_Bot$t);tb_cBi <- table(ChampMBTI_Bot$cluster, ChampMBTI_Bot$i);
#table Sup cluster, M,B,T,I
tb_cSm <- table(ChampMBTI_Sup$cluster, ChampMBTI_Sup$m);tb_cSb <- table(ChampMBTI_Sup$cluster, ChampMBTI_Sup$b);tb_cSt <- table(ChampMBTI_Sup$cluster, ChampMBTI_Sup$t);tb_cSi <- table(ChampMBTI_Sup$cluster, ChampMBTI_Sup$i);
#chi-square test
chisq.test(tb_cTm)
chisq.test(tb_cTb)
chisq.test(tb_cTt)
chisq.test(tb_cTi)

chisq.test(tb_cJm)
chisq.test(tb_cJb)
chisq.test(tb_cJt)
chisq.test(tb_cJi)

chisq.test(tb_cMm)
chisq.test(tb_cMb)
chisq.test(tb_cMt)
chisq.test(tb_cMi)

chisq.test(tb_cBm)
chisq.test(tb_cBb)
chisq.test(tb_cBt)
chisq.test(tb_cBi)

chisq.test(tb_cSm)
chisq.test(tb_cSb)
chisq.test(tb_cSt)
chisq.test(tb_cSi)

#######################Champ&line&MBTI=====hierarchical#######################
ChampMBTI_TopH<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character())
ChampMBTI_JgH<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character())
ChampMBTI_MidH<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character())
ChampMBTI_BotH<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character())
ChampMBTI_SupH<-data.frame(champ=integer(),m=character(),b=character(),t=character(),i=character(),cluster=integer(),lane=character());

for (row in 1:nrow(ChampMBTI)){
  champ1 <- ChampMBTI[row, "champ1"]
  keyChamp1 <- as.integer(champ1)
  lane <- ChampMBTI[row, "lane"]
  M <- ChampMBTI[row, "M"]
  B <- ChampMBTI[row, "B"]
  t <- ChampMBTI[row, "T"]
  I <- ChampMBTI[row, "I"]
  
  if (lane == "TOP"){
    if (is.null(kv_df_TopH[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_TopH)
      ChampMBTI_TopH <- rbind(ChampMBTI_TopH, df_if)
    }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_TopH[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_TopH)
      ChampMBTI_TopH <- rbind(ChampMBTI_TopH, df_if)
    }
  }
  
  if (lane == "JUNGLE"){
    if (is.null(kv_df_JgH[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_JgH)
      ChampMBTI_JgH <- rbind(ChampMBTI_JgH, df_if)
    }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_JgH[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_JgH)
      ChampMBTI_JgH <- rbind(ChampMBTI_JgH, df_if)
    }
  }
  
  if (lane == "MID"){
    if (is.null(kv_df_MidH[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_MidH)
      ChampMBTI_MidH <- rbind(ChampMBTI_MidH, df_if)
    }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_MidH[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_MidH)
      ChampMBTI_MidH <- rbind(ChampMBTI_MidH, df_if)
    }
  }
  
  if (lane == "ADC"){
    if (is.null(kv_df_BotH[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_BotH)
      ChampMBTI_BotH <- rbind(ChampMBTI_BotH, df_if)
    }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_BotH[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_BotH)
      ChampMBTI_BotH <- rbind(ChampMBTI_BotH, df_if)
    }
  }
  
  if (lane == "SUPPORT"){
    if (is.null(kv_df_SupH[[keyChamp1]])){
      df_if <- data.frame(champ1,M, B,t,I,0,lane)
      names(df_if) <- names(ChampMBTI_SupH)
      ChampMBTI_SupH <- rbind(ChampMBTI_SupH, df_if)
    }
    else{
      df_if <- data.frame(champ1,M, B,t,I,kv_df_SupH[keyChamp1],lane)
      names(df_if) <- names(ChampMBTI_SupH)
      ChampMBTI_SupH <- rbind(ChampMBTI_SupH, df_if)
    }
  }
}

#table Top cluster, M,B,T,I
tb_cTm <- table(ChampMBTI_TopH$cluster, ChampMBTI_TopH$m);tb_cTb <- table(ChampMBTI_TopH$cluster, ChampMBTI_TopH$b);tb_cTt <- table(ChampMBTI_TopH$cluster, ChampMBTI_TopH$t);tb_cTi <- table(ChampMBTI_TopH$cluster, ChampMBTI_TopH$i);
#table Jg cluster, M,B,T,I
tb_cJm <- table(ChampMBTI_JgH$cluster, ChampMBTI_JgH$m);tb_cJb <- table(ChampMBTI_JgH$cluster, ChampMBTI_JgH$b);tb_cJt <- table(ChampMBTI_JgH$cluster, ChampMBTI_JgH$t);tb_cJi <- table(ChampMBTI_JgH$cluster, ChampMBTI_JgH$i);
#table Mid cluster, M,B,T,I
tb_cMm <- table(ChampMBTI_MidH$cluster, ChampMBTI_MidH$m);tb_cMb <- table(ChampMBTI_MidH$cluster, ChampMBTI_MidH$b);tb_cMt <- table(ChampMBTI_MidH$cluster, ChampMBTI_MidH$t);tb_cMi <- table(ChampMBTI_MidH$cluster, ChampMBTI_MidH$i);
#table Bot cluster, M,B,T,I
tb_cBm <- table(ChampMBTI_BotH$cluster, ChampMBTI_BotH$m);tb_cBb <- table(ChampMBTI_BotH$cluster, ChampMBTI_BotH$b);tb_cBt <- table(ChampMBTI_BotH$cluster, ChampMBTI_BotH$t);tb_cBi <- table(ChampMBTI_BotH$cluster, ChampMBTI_BotH$i);
#table Sup cluster, M,B,T,I
tb_cSm <- table(ChampMBTI_SupH$cluster, ChampMBTI_SupH$m);tb_cSb <- table(ChampMBTI_SupH$cluster, ChampMBTI_SupH$b);tb_cSt <- table(ChampMBTI_SupH$cluster, ChampMBTI_SupH$t);tb_cSi <- table(ChampMBTI_SupH$cluster, ChampMBTI_SupH$i);
#chi-square test
chisq.test(tb_cTm)
chisq.test(tb_cTb)
chisq.test(tb_cTt)
chisq.test(tb_cTi)

chisq.test(tb_cJm)
chisq.test(tb_cJb)
chisq.test(tb_cJt)
chisq.test(tb_cJi)

chisq.test(tb_cMm)
chisq.test(tb_cMb)
chisq.test(tb_cMt)
chisq.test(tb_cMi)

chisq.test(tb_cBm)
chisq.test(tb_cBb)
chisq.test(tb_cBt)
chisq.test(tb_cBi)

chisq.test(tb_cSm)
chisq.test(tb_cSb)
chisq.test(tb_cSt)
chisq.test(tb_cSi)


#######################CORRELATION WITH MOST 1,2,3#######################
Most123 <-  df[,c(8,9,10)]

M123_cluster <- data.frame()
M123_cluster_h <- data.frame()

for (row in 1:nrow(Most123)){
  M1 <- Most123[row, "champ1"]
  M2 <- Most123[row, "champ2"]
  M3 <- Most123[row, "champ3"]
  keyM1 <- as.integer(M1)
  keyM2 <- as.integer(M2)
  keyM3 <- as.integer(M3)

  if (M1 != "None" & M2 != "None" & M3 != "None"){
    M123_cluster <- rbind(M123_cluster, c(kv_df_All[keyM1],kv_df_All[keyM2],kv_df_All[keyM3]))
    M123_cluster_h <- rbind(M123_cluster_h, c(kv_df_AllH[keyM1],kv_df_AllH[keyM2],kv_df_AllH[keyM3]))
  }
}
names(M123_cluster) <- c("m1", "m2", "m3")
names(M123_cluster_h) <- c("m1", "m2", "m3")
three = 0
two = 0
for (row in 1:nrow(M123_cluster)){
  c1 <- M123_cluster[row, "m1"]
  c2 <- M123_cluster[row, "m2"]
  c3 <- M123_cluster[row, "m3"]
  if (c1 == c2 & c2 == c3){
    three = three + 1
  }
  else if (c1 == c2 | c2 == c3 | c1 == c3){
    two = two +1
  }
}
chisq.test(table(M123_cluster$m1,M123_cluster$m1))
chisq.test(table(M123_cluster$m1,M123_cluster$m3))
chisq.test(table(M123_cluster$m2,M123_cluster$m1))

chisq.test(table(M123_cluster_h$m1,M123_cluster_h$m1))
chisq.test(table(M123_cluster_h$m1,M123_cluster_h$m3))
chisq.test(table(M123_cluster_h$m2,M123_cluster_h$m1))

#19/326 , 143/326
###############################bar plot##################
lane_mbti <-as.data.frame(table(df$lane,df$MBTI)[-c(4),])
ggplot(lane_mbti, aes(x=Var2,y=Freq,fill=Var1)) + geom_bar(stat="identity", width=1, color="white",position='dodge')+labs(title = "lane distribution for each MBTI")+labs(fill='lane') + xlab("MBTI") + ylab("Freq")

cluster_mbti1 <-as.data.frame(table(ChampMBTI$lane, ChampMBTI$M))
cluster_mbti1 <- rbind(cluster_mbti1,as.data.frame(table(ChampMBTI$lane, ChampMBTI$B)))
cluster_mbti1 <- rbind(cluster_mbti1,as.data.frame(table(ChampMBTI$lane, ChampMBTI$T)))
cluster_mbti1 <- rbind(cluster_mbti1,as.data.frame(table(ChampMBTI$lane, ChampMBTI$I)))

ggplot(cluster_mbti1, aes(x=Var2,y=Freq,fill=Var1)) + geom_bar(stat="identity", width=1, color="white",position='dodge')+labs(title = "lane distribution for each binary MBTI")+labs(fill='lane') + xlab("MBTI") + ylab("Freq")

cluster_mbti2 <-as.data.frame(table(ChampMBTI_h$lane, ChampMBTI_h$M))
cluster_mbti2 <- rbind(cluster_mbti2,as.data.frame(table(ChampMBTI_h$lane, ChampMBTI_h$B)))
cluster_mbti2 <- rbind(cluster_mbti2,as.data.frame(table(ChampMBTI_h$lane, ChampMBTI_h$T)))
cluster_mbti2 <- rbind(cluster_mbti2,as.data.frame(table(ChampMBTI_h$lane, ChampMBTI_h$I)))

ggplot(cluster_mbti2, aes(x=Var2,y=Freq,fill=Var1)) + geom_bar(stat="identity", width=1, color="white",position='dodge')+labs(title = "lane distribution for each binary MBTI")+labs(fill='lane') + xlab("MBTI") + ylab("Freq")

###########################################Plot for specific lane###################
Mid_cluster_mbti <- as.data.frame(table(ChampMBTI_Mid$m,ChampMBTI_Mid$cluster)[,c(-1)])
Mid_cluster_mbti <- rbind(Mid_cluster_mbti,as.data.frame(table(ChampMBTI_Mid$b,ChampMBTI_Mid$cluster)[,c(-1)]))
Mid_cluster_mbti <- rbind(Mid_cluster_mbti,as.data.frame(table(ChampMBTI_Mid$t,ChampMBTI_Mid$cluster)[,c(-1)]))
Mid_cluster_mbti <- rbind(Mid_cluster_mbti,as.data.frame(table(ChampMBTI_Mid$i,ChampMBTI_Mid$cluster)[,c(-1)]))

ggplot(Mid_cluster_mbti, aes(x=Var2,y=Freq,fill=Var1))+geom_bar(stat="identity", width=1, color="white",position='dodge')+labs(title = "MBTI distribution for each cluster(MID)")+labs(fill='MBTI') + xlab("cluster") + ylab("Freq")

Mid_cluster_mbti <- as.data.frame(table(ChampMBTI_MidH$m,ChampMBTI_MidH$cluster)[,c(-1)])
Mid_cluster_mbti <- rbind(Mid_cluster_mbti,as.data.frame(table(ChampMBTI_MidH$b,ChampMBTI_MidH$cluster)[,c(-1)]))
Mid_cluster_mbti <- rbind(Mid_cluster_mbti,as.data.frame(table(ChampMBTI_MidH$t,ChampMBTI_MidH$cluster)[,c(-1)]))
Mid_cluster_mbti <- rbind(Mid_cluster_mbti,as.data.frame(table(ChampMBTI_MidH$i,ChampMBTI_MidH$cluster)[,c(-1)]))

ggplot(Mid_cluster_mbti, aes(x=Var2,y=Freq,fill=Var1))+geom_bar(stat="identity", width=1, color="white",position='dodge')+labs(title = "MBTI distribution for each cluster(MID)")+labs(fill='MBTI') + xlab("cluster") + ylab("Freq")

sup_cluster_mbti <- as.data.frame(table(ChampMBTI_SupH$m,ChampMBTI_SupH$cluster)[,c(-1)])
sup_cluster_mbti <- rbind(sup_cluster_mbti,as.data.frame(table(ChampMBTI_SupH$b,ChampMBTI_SupH$cluster)[,c(-1)]))
sup_cluster_mbti <- rbind(sup_cluster_mbti,as.data.frame(table(ChampMBTI_SupH$t,ChampMBTI_SupH$cluster)[,c(-1)]))
sup_cluster_mbti <- rbind(sup_cluster_mbti,as.data.frame(table(ChampMBTI_SupH$i,ChampMBTI_SupH$cluster)[,c(-1)]))

ggplot(sup_cluster_mbti, aes(x=Var2,y=Freq,fill=Var1))+geom_bar(stat="identity", width=1, color="white",position='dodge')+labs(title = "MBTI distribution for each cluster(Support)")+labs(fill='MBTI') + xlab("cluster") + ylab("Freq")

##########################################stacked graph#######################################
cluster_mbti1 <-as.data.frame(table(ChampMBTI$M, ChampMBTI$lane)/rowSums(table(ChampMBTI$M, ChampMBTI$lane)))
cluster_mbti1 <- rbind(cluster_mbti1,as.data.frame(table(ChampMBTI$B, ChampMBTI$lane)/rowSums(table(ChampMBTI$B, ChampMBTI$lane))))
cluster_mbti1 <- rbind(cluster_mbti1,as.data.frame(table(ChampMBTI$T, ChampMBTI$lane)/rowSums(table(ChampMBTI$T, ChampMBTI$lane))))
cluster_mbti1 <- rbind(cluster_mbti1,as.data.frame(table(ChampMBTI$I, ChampMBTI$lane)/rowSums(table(ChampMBTI$I, ChampMBTI$lane))))
temp <-as.data.frame(table(ChampMBTI$lane)/sum(table(ChampMBTI$lane)))
colnames(temp) <- c('Var2','Freq')
temp[,'Var1'] <- c('All','All','All','All','All')
cluster_mbti1 <- rbind(cluster_mbti1,temp)

ggplot(cluster_mbti1, aes(x=Freq,y=Var1,fill=Var2)) + geom_bar(stat="identity", width=1, color="white")+coord_flip()+labs(title = "lane distribution for each binary MBTI")+labs(fill='lane') + xlab("MBTI") + ylab("Freq")

