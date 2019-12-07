
# ---------------------------------------------------
# Valor dos benefícios ----------------------------------------------------
# ---------------------------------------------------


# install.packages("WriteXLS")
library(xlsx)
setwd("C:/Users/Alice/Dropbox/TCP RAIANE")
fem <- read.csv("Feminino_inativos.csv")
mas <- read.csv("Masculino_inativos.csv")


femi = read.csv("Feminino_inativos.csv", row.names = 1)
View(femi)
masi = read.csv("Masculino_inativos.csv",row.names=1 )


# % do Benefício Feminino -------------------------------------------------
femi$Beneficio <- 0
t = 0.6 + (0.02* (femi$Tempo_cotribuicaof - 15))
if(t < 0){
  t = 0
}
 femi$Beneficio=  t


# Média dos Salários Femininos --------------------------------------------
for(i in 1:nrow(femi)){
  salarios = c(femi[i,3], femi[i,4], femi[i,5], femi[i,6], femi [i,7])
  media = mean(salarios)
  femi$Media_Sal[i] = media
}

# [BENEFÍCIO Com a EC 103/19 ] Valor do Beneficio Feminino ---------------------------------------------
femi$ Valor_BeneficioP = femi$Beneficio*femi$Media_Sal

# [BENEFÍCIO Antes da EC] Média dos 80% maioressalarios Feminino ---------------------------------
for(i in 1:nrow(femi)){
  salarios <- c(femi[i,3],femi[i,4],femi[i,5],femi[i,6],femi[i,7])
  salarios_n <- salarios[(salarios>min(salarios))]
  media <- mean(salarios_n)
  femi$media_Maiores[i] <- media
}


# % do Benefício Masculino ----------------------------------------------
masi$Beneficio <- 0
t = 0.6 + (0.02* (masi$Tempo_cotribuicao1 - 20))
if(t < 0){
  t = 0
}
masi$Beneficio=  t

# Média dos Salários Masculinos------------------------------------------------------
for(i in 1:nrow(masi)){
  salarios = c(masi[i,3], masi[i,4], masi[i,5], masi[i,6], masi [i,7])
  media = mean(salarios)
  masi$Media_Sal[i] = media
}


# [BENEFÍCIO Com a EC 103/19 ] Valor do Beneficio Masculino --------------------------------------------
masi$Valor_BeneficioP = masi$Beneficio*masi$Media_Sal

# [BENEFÍCIO Antes da EC]  Média dos 80% maiores salarios Masculino --------------------------------
for(i in 1:nrow(masi)){
  salarios <- c(masi[i,3],masi[i,4],masi[i,5],masi[i,6],masi[i,7])
  salarios_n <- salarios[(salarios>min(salarios))]
  media <- mean(salarios_n)
  masi$media_Maiores[i] <- media
}


write.csv(femi, "feminino_resultados_inativos.csv", row.names = T)
write.csv(masi, "masculino_resultados_inativos.csv", row.names = T)

write.xlsx(masi,"masculino_resultados_inativos.xlsx", row.names=T)
write.xlsx(femi,"feminino_resultados_inativos.xlsx", row.names=T)

