# Começando a realizar a validação cruzada, separando o banco para
# o conjunto de treinamento e o conjunto de teste.
install.packages("caret")


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


########################################################
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



# Separando os 10 folds.
require(caret)
k <- 10
cv <- 10
flds <- createDataPartition(Percent, times = cv, p = 0.2, list = TRUE)
names(flds)
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

################################################################
# Matriz que receberá as estimativas utilizando 
################################################################
mat_t1 <- matrix(0,nrow(teste1), k)
mat_t2 <- matrix(0,nrow(teste2), k)
mat_t3 <- matrix(0,nrow(teste3), k)
mat_t4 <- matrix(0,nrow(teste4), k)
mat_t5 <- matrix(0,nrow(teste5), k)
mat_t6 <- matrix(0,nrow(teste6), k)
mat_t7 <- matrix(0,nrow(teste7), k)
mat_t8 <- matrix(0,nrow(teste8), k)
mat_t9 <- matrix(0,nrow(teste9), k)
mat_t10 <- matrix(0,nrow(teste10), k)









# for(i in 1:1){
i <- 1





################################################################
# Modelo com gaussiana com função de ligação identidade.
m1 <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Thigh + 
          Forearm + Wrist, family = gaussian(link = "identity"), data=mat_treino[[i]])
mat_t1[ ,i] <- predict(m1,newdata=data.frame(mat_teste[[i]]), level = 0.95, interval="prediction") ## valor individual
rmse1 <-  (teste1[1] - mat_t1[ ,i])^2


# Modelo com gaussiana com função de ligação log.
m2 <- glm(formula = Percent ~ Age + Weigth +  Neck + Abdomen + Hip + Thigh +
          Forearm + Wrist, family = gaussian(link = "log"), data=mat_treino[[i]])
mat_t2[ ,i] <- predict(m2,newdata=data.frame(mat_teste[[i]]), level = 0.95, interval="prediction") ## valor individual
coef2 <- m2$coefficients


# Modelo com gaussiana com função de ligação inversa.
m3 <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm + Wrist, family = gaussian(link = "inverse"), data=mat_treino[[i]])
mat_t3[ ,i] <- predict(m3,newdata=data.frame(mat_teste[[i]]), level = 0.95, interval="prediction") ## valor individual
coef3 <- m3$coefficients



# Modelo com Gamma com função de ligação inversa.
m4 <- glm(formula = Percent ~ Age + Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm + Wrist, family = Gamma(link = "inverse"), data=mat_treino[[i]])
mat_t4[ ,i] <- predict(m4,newdata=data.frame(mat_teste[[i]]), level = 0.95, interval="prediction") ## valor individual
coef4 <- m4$coefficients



# Modelo com Gamma com função de ligação identidade.
m5 <- glm(formula = Percent ~ 0 + Age + Neck + Abdomen + Hip + Thigh + 
          Forearm + Wrist, family = Gamma(link = "identity"), data=mat_treino[[i]])
mat_t5[ ,i] <- predict(m5,newdata=data.frame(mat_teste[[i]]), level = 0.95, interval="prediction") ## valor individual
coef5 <- m1$coefficients





# Modelo com inversa gaussiana com função de ligação 1/mu^2.
m6 <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm , family = inverse.gaussian(link = "1/mu^2"), data=mat_treino[[i]])
mat_t6[ ,i] <- predict(m6,newdata=data.frame(mat_teste[[i]]), level = 0.95, interval="prediction") ## valor individual
coef6 <- m6$coefficients




# Modelo com Gamma com função de ligação log.
m7 <- glm(formula = Percent ~ 0 + Age + Weigth + Abdomen + Thigh + Knee + 
          Forearm + Wrist, family = Gamma(link = "log"), data=mat_treino[[i]])
mat_t7[ ,i] <- predict(m7,newdata=data.frame(mat_teste[[i]]), level = 0.95, interval="prediction") ## valor individual
coef7 <- m7$coefficients



# Modelo com Gamma com função de ligação log.
m8 <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Hip + Thigh +
          Forearm + Wrist, family = inverse.gaussian(link = "inverse"),
          data=mat_treino[[i]])
mat_t8[ ,i] <- predict(m8,newdata=data.frame(mat_teste[[i]]), level = 0.95, interval="prediction") ## valor individual
coef8 <- m8$coefficients



# }



