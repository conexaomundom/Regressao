# Começando a realizar a validação cruzada, separando o banco para
# o conjunto de treinamento e o conjunto de teste.
install.packages("caret")
install.packages("MLmatrics")
library(MLmetrics)

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
rmse2 <- matrix(0,cv,k)
rmse3 <- matrix(0,cv,k)
rmse4 <- matrix(0,cv,k)
rmse5 <- matrix(0,cv,k)
rmse6 <- matrix(0,cv,k)
rmse7 <- matrix(0,cv,k)
rmse8 <- matrix(0,cv,k)
m_rmse <- list(c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6,rmse7,rmse8))

rae1 <- matrix(0,cv,k)
rae2 <- matrix(0,cv,k)
rae3 <- matrix(0,cv,k)
rae4 <- matrix(0,cv,k)
rae5 <- matrix(0,cv,k)
rae6 <- matrix(0,cv,k)
rae7 <- matrix(0,cv,k)
rae8 <- matrix(0,cv,k)
m_rae <- list(c(rae1,rae2,rae3,rae4,rae5,rae6,rae7,rae8))


mae1 <- matrix(0,cv,k)
mae2 <- matrix(0,cv,k)
mae3 <- matrix(0,cv,k)
mae4 <- matrix(0,cv,k)
mae5 <- matrix(0,cv,k)
mae6 <- matrix(0,cv,k)
mae7 <- matrix(0,cv,k)
mae8 <- matrix(0,cv,k)
m_mae <- list(c(mae1,mae2,mae3,mae4,mae5,mae6,mae7,mae8))

rrse1 <- matrix(0,cv,k)
rrse2 <- matrix(0,cv,k)
rrse3 <- matrix(0,cv,k)
rrse4 <- matrix(0,cv,k)
rrse5 <- matrix(0,cv,k)
rrse6 <- matrix(0,cv,k)
rrse7 <- matrix(0,cv,k)
rrse8 <- matrix(0,cv,k)
m_rrse <- list(c(rrse1,rrse2,rrse3,rrse4,rrse5,rrse6,rrse7,rrse8))

al <- list(c(m_rmse,m_rae,m_mae,m_rrse))
####################################################################
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
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

m2 <- glm(formula = Percent ~ Age + Weigth +  Neck + Abdomen + Hip + Thigh +
          Forearm + Wrist, family = gaussian(link = "log"), data=mat_treino[[i]])
t2 <- predict(m2, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse2[i,j] <- RMSE(t2,mat_teste[[i]][1])
rae2[i,j] <- rae(t2,mat_teste[[i]][1])
mae2[i,j] <- mae(t2,mat_teste[[i]][1])
rrse2[i,j] <- rrse(t2,mat_teste[[i]][1])


m3 <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm + Wrist, family = gaussian(link = "inverse"), data=mat_treino[[i]])
t3 <- predict(m3, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse3[i,j] <- RMSE(t3,mat_teste[[i]][1])
rae3[i,j] <- rae(t3,mat_teste[[i]][1])
mae3[i,j] <- mae(t3,mat_teste[[i]][1])
rrse3[i,j] <- rrse(t3,mat_teste[[i]][1])


m4 <- glm(formula = Percent ~ Age + Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm + Wrist, family = Gamma(link = "inverse"), data=mat_treino[[i]])
t4 <- predict(m4, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse4[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae4[i,j] <- rae(t4,mat_teste[[i]][1])
mae4[i,j] <- mae(t4,mat_teste[[i]][1])
rrse4[i,j] <- rrse(t4,mat_teste[[i]][1])


m5 <- glm(formula = Percent ~ 0 + Age + Neck + Abdomen + Hip + Thigh + 
          Forearm + Wrist, family = Gamma(link = "identity"), data=mat_treino[[i]])
t5 <- predict(m5, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse5[i,j] <- RMSE(t5,mat_teste[[i]][1])
rae5[i,j] <- rae(t5,mat_teste[[i]][1])
mae5[i,j] <- mae(t5,mat_teste[[i]][1])
rrse5[i,j] <- rrse(t5,mat_teste[[i]][1])

# novamente reolhar quais modelos passaram 
# m6 <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
#           Forearm , family = inverse.gaussian(link = "1/mu^2"), data=mat_treino[[i]])
# t6 <- predict(m6, newdata=data.frame(mat_teste[[i]]), type = "response")
# rmse6[i,j] <- RMSE(t6,mat_teste[[i]][1])
# rae6[i,j] <- rae(t6,mat_teste[[i]][1])
# mae6[i,j] <- mae(t6,mat_teste[[i]][1])
# rrse6[i,j] <- rrse(t6,mat_teste[[i]][1])


m7 <- glm(formula = Percent ~ 0 + Age + Weigth + Abdomen + Thigh + Knee + 
          Forearm + Wrist, family = Gamma(link = "log"), data=mat_treino[[i]])
t7 <- predict(m7, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse7[i,j] <- sqrt(mae(t7,mat_teste[[i]][1]))
rae7[i,j] <- rae(t7,mat_teste[[i]][1])
mae7[i,j] <- mae(t7,mat_teste[[i]][1])
rrse7[i,j] <- rrse(t7,mat_teste[[i]][1])


m8 <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Hip + Thigh +
          Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=mat_treino[[i]])
t8 <- predict(m8, newdata=data.frame(mat_teste[[i]]), type = "response")
rmse8[i,j] <- RMSE(t8,mat_teste[[i]][1])
rae8[i,j] <- rae(t8,mat_teste[[i]][1])
mae8[i,j] <- mae(t8,mat_teste[[i]][1])
rrse8[i,j] <- rrse(t8,mat_teste[[i]][1])

 }
}

m_rmse <- appy(c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6,rmse7,rmse8))
m_rae <- list(c(rae1,rae2,rae3,rae4,rae5,rae6,rae7,rae8))
m_mae <- list(c(mae1,mae2,mae3,mae4,mae5,mae6,mae7,mae8))

# O melhor modelo foi o gaussiana inversa.

