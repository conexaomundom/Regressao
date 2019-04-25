# Começando a realizar a validação cruzada, separando o banco para
# o conjunto de treinamento e o conjunto de teste.
install.packages("caret")
install.packages("MLmatrics")
library(MLmatrics)

rm(list = ls())

# Dados
banco <- bodyfat
banco
head(banco)
attach(banco)
names(banco)
typeof(banco)
banco[182, ]
banco$Percent <- banco$Percent + 10
banco


###############################################################
# Modelos de betas originais.
################################################################
# Modelo com gaussiana com função de ligação identidade.
m1f <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Thigh + 
            Forearm + Wrist, family = gaussian(link = "identity"), data=banco)
coef1f <- m1f$coefficients
# Modelo com gaussiana com função de ligação log.
m2f <- glm(formula = Percent ~ Age + Weigth +  Neck + Abdomen + Hip + Thigh +
            Forearm + Wrist, family = gaussian(link = "log"), data=banco)
coef2f <- m2f$coefficients
# Modelo com gaussiana com função de ligação inversa.
m3f <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
            Forearm + Wrist, family = gaussian(link = "inverse"), data=banco)
coef3f <- m3f$coefficients
# Modelo com Gamma com função de ligação inversa.
m4f <- glm(formula = Percent ~ Age + Weigth + Abdomen + Hip + Thigh + Knee + 
            Forearm + Wrist, family = Gamma(link = "inverse"), data=banco)
coef4f <- m4f$coefficients
# Modelo com Gamma com função de ligação identidade.
m5f <- glm(formula = Percent ~ 0 + Age + Neck + Abdomen + Hip + Thigh + 
            Forearm + Wrist, family = Gamma(link = "identity"), data=banco)
coef5f <- m5f$coefficients
# Modelo com inversa gaussiana com função de ligação 1/mu^2.
m6f <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
            Forearm , family = inverse.gaussian(link = "1/mu^2"), data=banco)
coef6f <- m6f$coefficients
# Modelo com Gamma com função de ligação log.
m7f <- glm(formula = Percent ~ 0 + Age + Weigth + Abdomen + Thigh + Knee + 
            Forearm + Wrist, family = Gamma(link = "log"), data=banco)
coef7f <- m7f$coefficients
# Modelo com Gamma com função de ligação log.
m8f <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Hip + Thigh +
            Forearm + Wrist, family = inverse.gaussian(link = "inverse"),
           data=banco)
coef8f <- m8f$coefficients


################################################################
# A minha repetição começa aqui.
################################################################
k <- 10
cv <- 10

# Matrizes que vao receber as méidas das estimativas de rood mean square error
# a quantidade dessas matrizes 
rmse1 <- matrix(0,cv,k)
rmse2 <- matrix(0,cv,k)
rmse3 <- matrix(0,cv,k)
rmse4 <- matrix(0,cv,k)
rmse5 <- matrix(0,cv,k)
rmse6 <- matrix(0,cv,k)
rmse7 <- matrix(0,cv,k)
rmse8 <- matrix(0,cv,k)

rae1 <- matrix(0,cv,k)
rae2 <- matrix(0,cv,k)
rae3 <- matrix(0,cv,k)
rae4 <- matrix(0,cv,k)
rae5 <- matrix(0,cv,k)
rae6 <- matrix(0,cv,k)
rae7 <- matrix(0,cv,k)
rae8 <- matrix(0,cv,k)

mae1 <- matrix(0,cv,k)
mae2 <- matrix(0,cv,k)
mae3 <- matrix(0,cv,k)
mae4 <- matrix(0,cv,k)
mae5 <- matrix(0,cv,k)
mae6 <- matrix(0,cv,k)
mae7 <- matrix(0,cv,k)
mae8 <- matrix(0,cv,k)

rrse1 <- matrix(0,cv,k)
rrse2 <- matrix(0,cv,k)
rrse3 <- matrix(0,cv,k)
rrse4 <- matrix(0,cv,k)
rrse5 <- matrix(0,cv,k)
rrse6 <- matrix(0,cv,k)
rrse7 <- matrix(0,cv,k)
rrse8 <- matrix(0,cv,k)

# Contador para rodar as repetições
for(j in 1:k){

# Separando os 10 folds.
require(caret); 
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



t1 <- NULL; t2 <- NULL
t3 <- NULL; t4 <- NULL
t1 <- NULL; t6 <- NULL
t7 <- NULL; t8 <- NULL
t9 <- NULL; t10 <- NULL

################################################################
# Contador para rodar os folds
 for(i in 1:cv){

m1 <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Thigh + 
          Forearm + Wrist, family = gaussian(link = "identity"), data=mat_treino[[i]])
t1 <- predict(m1, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse1[i,j] <- sqrt(apply(mat_teste[[i]][i] -  t1,2,"mean")^2)


m2 <- glm(formula = Percent ~ Age + Weigth +  Neck + Abdomen + Hip + Thigh +
          Forearm + Wrist, family = gaussian(link = "log"), data=mat_treino[[i]])
t2 <- predict(m2, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse2[i,j] <- sqrt(apply(mat_teste[[i]][i] - t2,2,"mean")^2)


m3 <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm + Wrist, family = gaussian(link = "inverse"), data=mat_treino[[i]])
t3 <- predict(m3, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse3[i,j] <- sqrt(apply(mat_teste[[i]][i] - t3,2,"mean")^2)


m4 <- glm(formula = Percent ~ Age + Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm + Wrist, family = Gamma(link = "inverse"), data=mat_treino[[i]])
t4 <- predict(m4, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse4[i,j] <- sqrt(apply(mat_teste[[i]][i] - t4,2,"mean")^2)


m5 <- glm(formula = Percent ~ 0 + Age + Neck + Abdomen + Hip + Thigh + 
          Forearm + Wrist, family = Gamma(link = "identity"), data=mat_treino[[i]])
t5 <- predict(m5, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse5[i,j] <- sqrt(apply(mat_teste[[i]][i] - t5,2,"mean")^2)


m6 <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm , family = inverse.gaussian(link = "1/mu^2"), data=mat_treino[[i]])
t6 <- predict(m6, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse6[i,j] <- sqrt(apply(mat_teste[[i]][i] - t6,2,"mean")^2)


m7 <- glm(formula = Percent ~ 0 + Age + Weigth + Abdomen + Thigh + Knee + 
          Forearm + Wrist, family = Gamma(link = "log"), data=mat_treino[[i]])
t7 <- predict(m7, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse7[i,j] <- sqrt(apply(mat_teste[[i]][i] - t7,2,"mean")^2)


m8 <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Hip + Thigh +
          Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=mat_treino[[i]])
t8 <- predict(m8, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse8[i,j] <- sqrt(apply(mat_teste[[i]][i] - t8,2,"mean")^2)

 }
medias <- c(mean(rmse1), mean(rmse2), mean(rmse3), mean(rmse4),mean(rmse5),
             mean(rmse7), mean(rmse8))

}
which(medias == min(medias))
# O melhor modelo foi o gaussiana inversa.
library(MLmetrics)
?MLmetrics
