library(dplyr)
n_v<-read.csv("data/nuevas_viejas_prueba01.csv",header = F)
v_n<-read.csv("data/viejas_nuevas_prueba01.csv",header = F)

n_v[1,ncol(n_v)]
v_n[1,ncol(v_n)]

names(n_v)<-as.character(as.matrix(n_v)[1,])
names(v_n)<-as.character(as.matrix(v_n)[1,])

names(n_v)<-gsub(" ","_",names(n_v))
names(v_n)<-gsub(" ","_",names(v_n))

names(n_v)<-gsub("\\(","_",names(n_v))
names(v_n)<-gsub("\\(","_",names(v_n))

names(n_v)<-gsub("\\)","_",names(n_v))
names(v_n)<-gsub("\\)","_",names(v_n))

names(n_v)<-gsub("\\/","_",names(n_v))
names(v_n)<-gsub("\\/","_",names(v_n))

names(n_v)<-gsub("\\%","_",names(n_v))
names(v_n)<-gsub("\\%","_",names(v_n))

names(n_v)<-gsub("\\\n","__",names(n_v))
names(v_n)<-gsub("\\\n","__",names(v_n))

names(n_v)<-gsub(",","_",names(n_v))
names(v_n)<-gsub(",","_",names(v_n))

names(n_v)<-gsub("-","_",names(n_v))
names(v_n)<-gsub("-","_",names(v_n))

names(n_v)<-gsub(":","_",names(n_v))
names(v_n)<-gsub(":","_",names(v_n))

saveRDS(n_v,"data/nuevas_viejas_prueba01.rds")
saveRDS(v_n,"data/viejas_nuevas_prueba01.rds")

View(head(v_n[,199:201],10))

funcion_sentencia_max<-function(nombresvars,nombrecol_nueva){#base,
  # nombresvars<-v_n_tmp$Etiquetas_de_fila
  # base<-tbla_ejemplo
  # nombrecol_nueva<-nombrecol
  
  # base2<-base%>%
  #   rowwise()%>%
  #   mutate_(.dots= setNames(paste0("max(",paste(nombresvars,collapse=","),")"),nombrecol_nueva))
  
  return(setNames(paste0("max(",paste(nombresvars,collapse=","),")"),nombrecol_nueva))
}
lista_setnames<-list()
for(columna in 2:(ncol(v_n)-2)){
  # columna<-2
  print(columna)
  print(names(v_n)[columna])
  
  nombrecol<-names(v_n)[columna]
  
  v_n_tmp<-v_n%>%
    select(one_of(c("Etiquetas_de_fila",nombrecol)))%>%
    mutate_at(nombrecol,as.character)%>%
    mutate_at(nombrecol,as.numeric)%>%
    filter_(.dots = paste0(nombrecol,">=1"))
  

  lista_setnames$tmp<-funcion_sentencia_max(v_n_tmp$Etiquetas_de_fila,nombrecol)
  names(lista_setnames)[length(lista_setnames)]<-nombrecol
  
}



# # # Cambiar de la columna "Etiquetas_de_fila" los nombres quitando los mismos caracteres que se
# # # quitaron en los nombres de las columnas nuevas
# # # Así también quitar esos caracteres raros de los nombres de las columnas de la 
# # # base USIDNET




tbla_ejemplo<-data.frame("vieja1"=c(0,1,0),"vieja2"=c(0,1,1),"vieja3"=c(1,0,0))
v_n02<-data.frame(Etiquetas_de_fila=c("vieja1","vieja2","vieja3"),"varnueva"=c(1,0,1))

nombrecol<-"varnueva"

v_n_tmp<-v_n02%>%
  select(one_of(c("Etiquetas_de_fila",nombrecol)))%>%
  mutate_at(nombrecol,as.character)%>%
  mutate_at(nombrecol,as.numeric)%>%
  filter_(.dots = paste0(nombrecol,">=1"))




tbla_ejemplo02<-tbla_ejemplo%>%
  mutate()