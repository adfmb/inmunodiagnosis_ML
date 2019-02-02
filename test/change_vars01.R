USIDNET0<-read.csv("data/USIDNET.csv",header=F,sep=",",fileEncoding = "cp932")
names(USIDNET0)
View(head(USIDNET0))
saveRDS(USIDNET0,"data/USIDNET.rds")
USIDNET0<-readRDS("data/USIDNET.rds")

USIDNET<-USIDNET0[-1,]
names(USIDNET)<-as.character(as.matrix(USIDNET0)[1,])
names(USIDNET)<-gsub(" ","_",names(USIDNET))
names(USIDNET)<-gsub("\\(","_",names(USIDNET))
names(USIDNET)<-gsub("\\)","_",names(USIDNET))
names(USIDNET)<-gsub("\\/","_",names(USIDNET))
names(USIDNET)<-gsub("\\%","_",names(USIDNET))
names(USIDNET)<-gsub("\\\n","__",names(USIDNET))
names(USIDNET)<-gsub(",","_",names(USIDNET))
names(USIDNET)<-gsub("-","_",names(USIDNET))
names(USIDNET)<-gsub(":","_",names(USIDNET))
names(USIDNET)<-gsub(";","_",names(USIDNET))

library(dplyr)
df_names<-as.data.frame(table(names(USIDNET)))
df_names<-df_names%>%
  as_data_frame()%>%
  arrange(desc(Freq))

nombres_dup<-df_names$Var1[df_names$Freq>1]
cols_nombres_dup<-names(USIDNET)[names(USIDNET)%in%as.character(nombres_dup)]
length(unique(USIDNET$patient))

length(unique(USIDNET$patient[USIDNET$patient!=""]))
sum(USIDNET$patient!="")

match(1,(names(USIDNET)%in%as.character(nombres_dup)*1))
indices<-c()
for(col in as.character(nombres_dup)){
  # col<-as.character(nombres_dup)[1]
  indices<-c(indices,match(col, names(USIDNET)))
}
sub_usidnet<-USIDNET[names(USIDNET)%in%as.character(nombres_dup)]
View(sub_usidnet)
names(USIDNET)[indices]

indices2<-c(indices,(indices+1))
names(USIDNET)[indices2]
saveRDS(USIDNET,"data/USIDNET_02.rds")

dic_nvasvars<-read.csv("data/Campos_USIDNET.csv",header = T)
dic_nvasvars$Campo.Antiguo
names(bd1)[grep("Infection_Rash",names(bd1))]
dic_nvasvars$Campo.Antiguo[grep("Infection_Rash",dic_nvasvars$Campo.Antiguo)]

names(bd1)[grep("Infection_Pneumonitis",names(bd1))]
dic_nvasvars$Campo.Antiguo[grep("Infection_Pneumonitis",dic_nvasvars$Campo.Antiguo)]


## checar regular_expressions para las comas, los puntos, los espacios, diagonales, signos de porcentajes,etc etc etc