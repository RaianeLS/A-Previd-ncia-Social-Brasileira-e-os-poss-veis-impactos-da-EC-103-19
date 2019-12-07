## Leitura dos dados (DIRETÓRIO ALICE)
setwd("C:/Users/Alice/Dropbox/TCP RAIANE")
fem <- read.csv("Feminino.csv" , row.names=1)
mas <- read.csv("Masculino.csv", row.names=1)

# EC 103/2019 -----------------------------------------------------------
### FEMININO
fem$PEC_Tempo <-  0
for (i in 1:nrow(fem)){
  t_idade <- 62 - fem$Idade[i]
  t_cont <- 20 - fem$Tempo_cotribuicaof[i] #obrigatorio
  t <- max(t_idade,t_cont)
  if(t < 0){
    t <- 0
  }
  fem$PEC_Tempo[i] <- t
}

### MASCULINO (Raiane)
mas$PEC_Tempo <-  0
for (i in 1:nrow(mas)){
  t_idade <- 65 - mas$Idade[i]
  t_cont <- 20 - mas$Tempo_cotribuicao1[i]
  t <- max(t_idade,t_cont)
  if(t < 0){
    t <- 0
  }
  mas$PEC_Tempo[i] <- t
}


# REGRA ATUAL -------------------------------------------------------------
### FEMININO
fem$Atual_Tempo <- 0
for (i in 1:nrow(fem)){
  t_cont <- 30 - fem$Tempo_cotribuicaof[i]
  t_cont_obg <- 15 - fem$Tempo_cotribuicaof[i]
  t_idade <- 60 - fem$Idade[i]
  if(t_cont <= t_idade){ # aposenta por tempo de contribuição (contribui mais de 30)
    t <- t_cont
  }else{
    if (t_cont_obg <= 0){ # já contribuiu o minimo (15)
      t <- t_idade # aposenta por idade
    }else{
      t <- max(t_idade,t_cont_obg)
    }
  }
  if(t < 0){
    t <- 0
  }
  fem$Atual_Tempo[i] <- t
}
par(mfrow = c(1,1))
par(new=T)
plot(fem$PEC_Tempo, col= "red")
plot(fem$Atual_Tempo, col = "blue")
library(ggplot2)




### MASCULINO
mas$Atual_Tempo <- 0
for (i in 1:nrow(mas)){
  t_cont <- 35 - mas$Tempo_cotribuicao1[i]
  t_cont_obg <- 15 - mas$Tempo_cotribuicao1[i]
  t_idade <- 65 - mas$Idade[i]
  if(t_cont <= t_idade){ 
    t <- t_cont
  }else{
    if (t_cont_obg <= 0){
      t <- t_idade 
    }else{
      t <- max(t_idade,t_cont_obg)
    }
  }
  if(t < 0){
    t <- 0
  }
  mas$Atual_Tempo[i] <- t
}

write.csv(fem, "feminino_resultados_ativos.csv", row.names = T)
write.csv(mas, "masculino_resultados_ativos.csv", row.names = T)
