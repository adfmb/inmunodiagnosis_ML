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
names(USIDNET)[1:10]
as.character(as.matrix(USIDNET0)[1,1:10])

library(dplyr)
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
sub_usidnet_02%>%select(patient)
sum(is.infinite(sub_usidnet_02$patient))
View(names(sub_usidnet_02))

nombres_dup<-df_names$Var1[df_names$Freq>1]
nombres_dup<-nombres_dup[!nombres_dup%in%"patient"]
nombres_dup2<-sort(c(as.character(nombres_dup),paste0(nombres_dup,".1")))
sub_usidnet_03<-sub_usidnet_02[c("patient",as.character(nombres_dup2))]
no_sub_usidnet_03<-sub_usidnet_02[!names(sub_usidnet_02)%in%as.character(nombres_dup2)]
ncol(sub_usidnet_02)
ncol(sub_usidnet_03)
ncol(no_sub_usidnet_03)

nrow(sub_usidnet_02)
nrow(sub_usidnet_03)
nrow(no_sub_usidnet_03)

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


usidnet_univar<-sub_usidnet_04%>%
  left_join(no_sub_usidnet_03)

saveRDS(usidnet_univar,"data/usidnet_univar.rds")
