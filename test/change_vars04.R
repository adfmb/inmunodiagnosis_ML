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

nombres_dup<-df_names$Var1[df_names$Freq>1]

USIDNET2<-USIDNET[names(USIDNET)%in%names(USIDNET)] #con este paso, R pone sufijo .i a los campos repetidos
names(USIDNET2)[names(USIDNET2)%in%as.character(nombres_dup)]
names(USIDNET2[names(USIDNET2)%in%as.character(nombres_dup)])

## Pasamos la información de las diversas variables de 'patient' a una sola variable
## Este proceso lo hacemos separado de las demás variables que están duplicadas porque 
## Es la única variable (patient) que aparece más de 2 veces... 
## Para las demás variables (nombres_dup[!nombres_dup%in%"patient"]) lo hacemos con un loop porque 
## comparten el hecho de que son 2 duplicados por var
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
  select(-one_of(vars))%>% ## Aquì quitamos las variables originales de 'patient' y 
  rename(patient=vartmp) ## mantenemos sòlo la nva vartmp que aquì renombramos con 'patient'
sub_usidnet_02%>%select(patient)
## Confirmamos que todas las entradas de la nueva variable 'patient' no tenga
## ni NA's ni Inf's
sum(is.infinite(sub_usidnet_02$patient))
sum(is.na(sub_usidnet_02$patient))
View(names(sub_usidnet_02))
nrow(sub_usidnet_02)
ncol(sub_usidnet_02)

# nombres_dup<-df_names$Var1[df_names$Freq>1] ## ya se tiene este objeto en lìneas anteriores
## Aquí nos quedamos sólo con las demás variables duplicadas que NO son 'patient'
nombres_dup<-nombres_dup[!nombres_dup%in%"patient"]
## Aquí duplicamos el vector de nombres_dup para incluir, no sólo al nombre original de la var
## sino también al nombre con el sujido .i que, para todos estos casos, es .1"
## por lo que ya se habia comentado de que estas vars sólo tienen 1 duplicado cada una
nombres_dup2<-sort(c(as.character(nombres_dup),paste0(nombres_dup,".1")))
## En este dataframe arastramos la variable 'patient' que será la llave para
## los left_join pues ya tiene los datos correctos de todos sus duplicados 
## y ademàs nos quedamos también con las variables que aparecen 2 veces
sub_usidnet_03<-sub_usidnet_02[c("patient",as.character(nombres_dup2))]
## Por otro lado nos quedamos con todas las variables que no están en el vector nombres_dup2
## por lo tanto, todas las variables que ya no están duplicadas
## provocando que la nueva var 'patient' también esté en este dataframe porque no está
## en el vector nombres_dup2
no_sub_usidnet_03<-sub_usidnet_02[!names(sub_usidnet_02)%in%as.character(nombres_dup2)]
ncol(sub_usidnet_02)
ncol(sub_usidnet_03)
ncol(no_sub_usidnet_03)

nrow(sub_usidnet_02)
nrow(sub_usidnet_03)
nrow(no_sub_usidnet_03)

sub_usidnet_04<-sub_usidnet_03
## Omitimos la intrada i=1 porque pertenece al campo 'patient' y ese ya no tien duplicado
## con sufijo .1
for(idcol in c(2,4,6,8,10)){
  # idcol<-1
  idcol_next<-idcol+1
  vars<-names(sub_usidnet_03)[c(idcol,idcol_next)]
  print(vars)
  sub_tmp<-sub_usidnet_03%>%
    as_data_frame()%>%
    select(one_of(c("patient",vars)))%>% ## jalamos 'patient' para usarla de llave
    mutate_at(vars,as.character)%>%
    mutate_at(vars,as.numeric)%>%
    rowwise()%>%
    mutate_(.dots= setNames(paste0("max(",paste(vars,collapse=","),",na.rm = T)"),"vartmp"))%>%
    arrange(vartmp)%>%
    select(-one_of(vars))%>%
    rename_(.dots=setNames("vartmp",vars[1]))
  
  sub_usidnet_04<-sub_usidnet_04%>%
    select(-one_of(vars))%>% ##quitamos las vars duplicadas de sub_usidnet_04
    left_join(sub_tmp) ## y pegamos la var corregida en sub_tmp by='patient'
}


usidnet_univar<-sub_usidnet_04%>% ## a la tabla sub_usidnet_04 ya corregida de duplicados 
  left_join(no_sub_usidnet_03) ## le pegamos los campos no-duplicados by='patient'

saveRDS(usidnet_univar,"data/usidnet_univar.rds") ## <- Base a utilizar!!!

usidnet_univar<-readRDS("data/usidnet_univar.rds")
df_names_univar<-as.data.frame(table(names(usidnet_univar)))
df_names_univar<-df_names_univar%>%
  as_data_frame()%>%
  arrange(desc(Freq))

