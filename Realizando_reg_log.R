# Começando a mexer no banco titanic
install.packages("caret")
#Regressao no R

rm(list = ls())
install.packages("ROCR")
install.packages("Epi")
install.packages("SDMTools")
library(nortest) # Testes de normalidade 
library(boot) 


rm(list = ls())

banco <- titanic_train
attach(banco)
names(banco)
# variáveis contidas no banco.
# "X", "Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare","Embarked"

banco[ ,5] <- ceiling(banco[ ,5])
which(banco[, 5] == 0)

# MELHORAR ESSA PARTE.

# Realizando a seleção de variáveis de acordo com o teste t, deixando no 
# modelo apenas as variáveis que tiveram p-valor menor que 5%, o nível de 
# significancia adotado.
# Primeiro realizando a seleção de variáveis já que sendo a regressão 
# logistica, usa-se o modelo de regressão generaizado com distribuição
# binomial e função de ligação "logit", os teste t sendo feito ao nível 
# de significância de 5%.

modelo <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                Fare + Embarked, family = binomial(link = "logit"), data=banco)
summary(modelo)

# Retirando a variável Embarked com p-valor 0.93856
modelo1 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                Fare, family = binomial(link = "logit"), data=banco)
summary(modelo1)

# Retirando a variável Parch com p-valor 0.34329
modelo2 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp +  
                 Fare, family = binomial(link = "logit"), data=banco)
summary(modelo2)

# Retirando a variável Fare com p-valor 0.295925 
modelo3 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp,
               family = binomial(link = "logit"), data=banco)
#Logistic Regression
library(xtable)
xtable(summary(modelo3))

# Seleão de variáveis realizada e o modelo final ficou com 4 variáveis 
# explicativas mais o intercepto.

#Odds Ratio
xtable(as.table(exp(b$coefficients[-1]))) # Cautela ao rodar este comando

# Essa é a razão de chances...

fit=fitted(b) # probabilidades estimadas para cada individuo da amostra

#Curva ROC
#? necess?rio instalar a biblioteca ROCR

library(ROCR)

#Considere em seu modelo o sucesso como Y=1 e o fracasso como Y=0.
pred <- prediction(fitted(b), Survived)
perf <- performance(pred,"tpr","fpr") #Escolha do ponto de corte, TP e FP
area <- performance(pred,"auc") #Calcula a ?rea sob a curva ROC
plot(perf, main = " Cruva ROC") #Constroi o gr?fico da curva ROC

# Olhanco para a curva ROC podemos observar a qualidade do ajuste para o modelo,
# pois é obtido com base nas taxas de erro e acerto do modelo, possibilitando
# pegar o ponto de corte que maximiza o True Positive e minimize o False 
# Positive, podendo plotar uma reta que vai do canto superior esquerdo e acabe 
# no canto inferior direito e ao epgar o ponto em que a reta cruza com a curva é
# uma boa opção de???? HELP ME PLEASE. 
# PENSAR UMA FORMA DE IDENTIFICAR COMO PEGAR ESSE PONTO.

#Outra Opcao da ROC

library(Epi)
ROC(form = Survived ~ Pclass + Sex + Age + SibSp,plot="ROC",MX=FALSE)

# Montando a matriz de contingencia
library(SDMTools)
confusion.matrix(Survived, fit, threshold = 0.5)

# Calcule a sua probabilidade de sobreviver ao naufrágio do Titanic.
# Como carcular isso?

#######################################################################
# Avalaindo o poder preditivo do modelo.
#######################################################################
rmse <- function(y,y_hat){ sqrt(mean((y - y_hat)^2)) }
rae <- function(y, y_hat){ sum(abs(y_hat - y)) / sum(abs(mean(y) - y)) }
rrse <- function(y, y_hat){ sum((y_hat - y)^2) / sum((mean(y) - y)^2) }
mae <- function(y, y_hat){ sum( abs(y - y_hat)) /length(y) }
################################################################
# A minha repetição começa aqui.
################################################################
k <- 10
cv <- 10

# Matrizes que vao receber as méidas das estimativas de rood mean square error
# a quantidade dessas matrizes 
rmse1 <- matrix(0,cv,k)
rae1 <- matrix(0,cv,k)
mae1 <- matrix(0,cv,k)
rrse1 <- matrix(0,cv,k)

####################################################################
# Contador para rodar as repetições
for(j in 1:k){
  
  # Separando os 10 folds.
  require(caret)
  flds <- createDataPartition(Percent, times = cv, p = 0.2, list = TRUE)
  
  ################################################################
  # Lista com os elementos separados para treino.
  ################################################################
  train1 <- banco[-flds[[1]], ]
  train2 <- banco[-flds[[2]], ]
  train3 <- banco[-flds[[3]], ]
  train4 <- banco[-flds[[4]], ]
  train5 <- banco[-flds[[5]], ]
  train6 <- banco[-flds[[6]], ]
  train7 <- banco[-flds[[7]], ]
  train8 <- banco[-flds[[8]], ]
  train9 <- banco[-flds[[9]], ]
  train10 <- banco[-flds[[10]], ]
  mat_treino <- list(train1, train2, train3, train4, train5, train6, train7, train8, train9, train10)
  
  # Lista com os elementos separados para teste.
  teste1 <- banco[flds[[1]], ]
  teste2 <- banco[flds[[2]], ]
  teste3 <- banco[flds[[3]], ]
  teste4 <- banco[flds[[4]], ]
  teste5 <- banco[flds[[5]], ]
  teste6 <- banco[flds[[6]], ]
  teste7 <- banco[flds[[7]], ]
  teste8 <- banco[flds[[8]], ]
  teste9 <- banco[flds[[9]], ]
  teste10 <- banco[flds[[10]], ]
  mat_teste <- list(teste1, teste2, teste3, teste4, teste5, teste6, teste7, teste8, teste9, teste10)

  t1 <- NULL
  
  ################################################################
  # Contador para rodar os folds
  for(i in 1:cv){
    
    m1 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp,
                         family = binomial(link = "logit"), data=banco)
    t1 <- predict(m1, newdata=data.frame(mat_teste[[i]]), type = "response")
    rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
    rae1[i,j] <- rae(t1,mat_teste[[i]][1])
    mae1[i,j] <- mae(t1,mat_teste[[i]][1])
    rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
  }
  
  
}


mermse <- mean(rmse1)
merae <- mean(rae1)
memae <- mean(mae1)
merrse <- mean(rrse1)

mermse
merae
memae
merrse
