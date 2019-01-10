USIDNET0<-read.csv("data/USIDNET.csv",header=F,sep=",",fileEncoding = "cp932")
names(USIDNET)
View(head(USIDNET))
saveRDS(USIDNET,"data/USIDNET.rds")

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


df_names<-as.data.frame(table(names(USIDNET)))
df_names<-df_names%>%
  as_data_frame()%>%
  arrange(desc(Freq))

nombres_dup<-df_names$Var1[df_names$Freq>1]
names(USIDNET)[names(USIDNET)%in%as.character(nombres_dup)]
names(USIDNET[names(USIDNET)%in%as.character(nombres_dup)])
sub_usidnet<-USIDNET[names(USIDNET)%in%as.character(nombres_dup)]

idcol<-11
vars<-names(sub_usidnet)[c(idcol:14)]
sub_usidnet_02<-sub_usidnet%>%
  as_data_frame()%>%
  # select(one_of(vars))%>%
  mutate_at(vars,as.character)%>%
  mutate_at(vars,as.numeric)%>%
  rowwise()%>%
  mutate_(.dots= setNames(paste0("max(",paste(vars,collapse=","),",na.rm = T)"),"vartmp"))%>%
  arrange(vartmp)%>%
  filter(!is.infinite(vartmp))%>%
  select(-one_of(vars))%>%
  rename(patient=vartmp)
sub_usidnet_03<-sub_usidnet_02
for(idcol in c(1,3,5,7,9)){
  # idcol<-1
  idcol_next<-idcol+1
  vars<-names(sub_usidnet_02)[c(idcol,idcol_next)]
  print(vars)
  sub_tmp<-sub_usidnet_02%>%
    as_data_frame()%>%
    select(one_of(c("patient",vars)))%>%
    mutate_at(vars,as.character)%>%
    mutate_at(vars,as.numeric)%>%
    rowwise()%>%
    mutate_(.dots= setNames(paste0("max(",paste(vars,collapse=","),",na.rm = T)"),"vartmp"))%>%
    arrange(vartmp)%>%
    select(-one_of(vars))%>%
    rename_(.dots=setNames("vartmp",vars[1]))
  
  sub_usidnet_03<-sub_usidnet_03%>%
    select(-one_of(vars))%>%
    left_join(sub_tmp)
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