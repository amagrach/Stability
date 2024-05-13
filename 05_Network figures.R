df<- read.csv("Input_data/full.data.csv", T)
head(df)
unique(df$System_var)

df2<-subset(df, df$System_var=="Pollinator interaction frequencies")
head(df2)

library(tidyr)

df3 <- separate(df2, Sps, into = c("Pollinator", "Plant"), sep = " ", extra = "merge")
df3 <- separate(df3, Plant, into = c("Plant", "Extra"), sep = " ", extra = "merge", fill = "right")
head(df3)

df3$Pollinator2<-paste(df3$Pollinator, df3$Plant)
df3$Plant2<-df3$Extra

df4 <- na.omit(df3)
head(df4)

df5<-subset(df4, df4$Abundance>0)
head(df5)




df6<-subset(df5, df5$Site=="Pinar Aznalcazar"&df5$Year==2020)
df6
colnames(df6)

library(bipartite)


temp4<-df6[,c(13, 14, 6, 1)]
colnames(temp4)<-c("higher", "lower", "webID", "freq")

ntw_pl<-frame2webs(temp4)
ntw_pl2<-as.data.frame(ntw_pl)
colnames(ntw_pl2)<-unique(temp4$higher)
p1<- plotweb(ntw_pl2)
 

df6<-subset(df5, df5$Site=="Pinar Aznalcazar"&df5$Year==2021)
df6
colnames(df6)

library(bipartite)


temp4<-df6[,c(13, 14, 6, 1)]
colnames(temp4)<-c("higher", "lower", "webID", "freq")

ntw_pl<-frame2webs(temp4)
ntw_pl2<-as.data.frame(ntw_pl)
colnames(ntw_pl2)<-unique(temp4$higher)
p2<- plotweb(ntw_pl2)




df6<-subset(df5, df5$Site=="Villamanrique Sur"&df5$Year==2020)
df6
colnames(df6)

library(bipartite)


temp4<-df6[,c(13, 14, 6, 1)]
colnames(temp4)<-c("higher", "lower", "webID", "freq")

ntw_pl<-frame2webs(temp4)
ntw_pl2<-as.data.frame(ntw_pl)
colnames(ntw_pl2)<-unique(temp4$higher)
p1<- plotweb(ntw_pl2)


df6<-subset(df5, df5$Site=="Villamanrique Sur"&df5$Year==2021)
df6
colnames(df6)

library(bipartite)


temp4<-df6[,c(13, 14, 6, 1)]
colnames(temp4)<-c("higher", "lower", "webID", "freq")

ntw_pl<-frame2webs(temp4)
ntw_pl2<-as.data.frame(ntw_pl)
colnames(ntw_pl2)<-unique(temp4$higher)
p2<- plotweb(ntw_pl2)



