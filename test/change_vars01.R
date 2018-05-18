USIDNET<-read.csv("data/USIDNET.csv",header=F,sep=",",fileEncoding = "cp932")
names(bd1)
View(head(USIDNET))
saveRDS(USIDNET,"data/USIDNET.rds")

dic_nvasvars<-read.csv("data/Campos_USIDNET.csv",header = T)
dic_nvasvars$Campo.Antiguo
names(bd1)[grep("Infection_Rash",names(bd1))]
dic_nvasvars$Campo.Antiguo[grep("Infection_Rash",dic_nvasvars$Campo.Antiguo)]

names(bd1)[grep("Infection_Pneumonitis",names(bd1))]
dic_nvasvars$Campo.Antiguo[grep("Infection_Pneumonitis",dic_nvasvars$Campo.Antiguo)]

## checar regular_expressions para las comas, los puntos, los espacios, diagonales, signos de porcentajes,etc etc etc