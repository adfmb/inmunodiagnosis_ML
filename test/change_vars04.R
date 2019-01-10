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

USIDNET2<-USIDNET[names(USIDNET)%in%names(USIDNET)]
names(USIDNET2)[names(USIDNET2)%in%as.character(nombres_dup)]
names(USIDNET2[names(USIDNET2)%in%as.character(nombres_dup)])
vars<-c("patient","patient.1","patient.2","patient.3")
sub_usidnet_02<-USIDNET2%>%
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


nombres_dup<-df_names$Var1[df_names$Freq>1]
nombres_dup<-nombres_dup[!nombres_dup%in%"patient"]
nombres_dup2<-sort(c(as.character(nombres_dup),paste0(nombres_dup,".1")))
sub_usidnet_03<-sub_usidnet_02[c("patient",as.character(nombres_dup2))]

sub_usidnet_04<-sub_usidnet_03
for(idcol in c(2,4,6,8,10)){
  # idcol<-1
  idcol_next<-idcol+1
  vars<-names(sub_usidnet_03)[c(idcol,idcol_next)]
  print(vars)
  sub_tmp<-sub_usidnet_03%>%
    as_data_frame()%>%
    select(one_of(c("patient",vars)))%>%
    mutate_at(vars,as.character)%>%
    mutate_at(vars,as.numeric)%>%
    rowwise()%>%
    mutate_(.dots= setNames(paste0("max(",paste(vars,collapse=","),",na.rm = T)"),"vartmp"))%>%
    arrange(vartmp)%>%
    select(-one_of(vars))%>%
    rename_(.dots=setNames("vartmp",vars[1]))
  
  sub_usidnet_04<-sub_usidnet_04%>%
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