USIDNET0<-read.csv("data/USIDNET.csv",header=F,sep=",",fileEncoding = "cp932")
names(USIDNET)
View(head(USIDNET))
saveRDS(USIDNET,"data/USIDNET.rds")

USIDNET<-USIDNET0
names(USIDNET)<-as.character(as.matrix(USIDNET)[1,])
names(USIDNET)<-gsub(" ","_",names(USIDNET))
names(USIDNET)<-gsub("\\(","_",names(USIDNET))
names(USIDNET)<-gsub("\\)","_",names(USIDNET))
names(USIDNET)<-gsub("\\/","_",names(USIDNET))
names(USIDNET)<-gsub("\\%","_",names(USIDNET))
names(USIDNET)<-gsub("\\\n","__",names(USIDNET))
names(USIDNET)<-gsub(",","_",names(USIDNET))
names(USIDNET)<-gsub("-","_",names(USIDNET))
names(USIDNET)<-gsub(":","_",names(USIDNET))


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
names(USIDNET)[names(USIDNET)%in%as.character(nombres_dup)]
names(USIDNET[names(USIDNET)%in%as.character(nombres_dup)])
sub_usidnet<-USIDNET[names(USIDNET)%in%as.character(nombres_dup)]
for(idcol in c(1,3,5,7,9)){
  # idcol<-1
  idcol_next<-idcol+1
  vars<-names(sub_usidnet)[c(idcol,idcol_next)]
  sub_tmp<-sub_usidnet%>%
    as_data_frame()%>%
    select(one_of(vars))
}
View(sub_usidnet)

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