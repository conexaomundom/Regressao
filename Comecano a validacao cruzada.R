# Começando a realizar a validação cruzada, separando o banco para
# o conjunto de treinamento e o conjunto de teste.
install.packages("caret")

# Separando os 10 folds.
require(caret)
flds <- createDataPartition(y, times = 10, p = 0.2, list = TRUE)
names(flds)

# Esses são meus dados pra testar após o primeiro treinamento

flds[[1]]
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


# Pegando os modelos finais e fazendo o teste de aqueção global neles.



cv <- function(banco){

# Modelo com gaussiana com função de ligação identidade.
m1 <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Thigh + 
          Forearm + Wrist, family = gaussian(link = "identity"), data=banco)

# Modelo com gaussiana com função de ligação log.
m2 <- glm(formula = Percent ~ Age + Weigth +  Neck + Abdomen + Hip + Thigh +
          Forearm + Wrist, family = gaussian(link = "log"), data=banco)

# Modelo com gaussiana com função de ligação inversa.
m3 <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm + Wrist, family = gaussian(link = "inverse"), data=banco)

# Modelo com Gamma com função de ligação inversa.
m4 <- glm(formula = Percent ~ Age + Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm + Wrist, family = Gamma(link = "inverse"), data=banco)

# Modelo com Gamma com função de ligação identidade.
m5 <- glm(formula = Percent ~ 0 + Age + Neck + Abdomen + Hip + Thigh + 
          Forearm + Wrist, family = Gamma(link = "identity"), data=banco)

# Modelo com inversa gaussiana com função de ligação 1/mu^2.
m6 <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
          Forearm , family = inverse.gaussian(link = "1/mu^2"), data=banco)

# Modelo com Gamma com função de ligação log.
m7 <- glm(formula = Percent ~ 0 + Age + Weigth + Abdomen + Thigh + Knee + 
          Forearm + Wrist, family = Gamma(link = "log"), data=banco)

# Modelo com Gamma com função de ligação log.
m8 <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Hip + Thigh +
          Forearm + Wrist, family = inverse.gaussian(link = "inverse"),
          data=banco)

}
