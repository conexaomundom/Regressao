# validação cruzada para o modelo de regressão logistica

################################################################
# A minha repetição começa aqui.
################################################################
k <- 100
cv <- 10

# Matrizes que vao receber as méidas das estimativas de rood mean square error
# a quantidade dessas matrizes 
rmse1 <- matrix(0,cv,k)
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
  
  t1 <- NULL
  ################################################################
  # Contador para rodar os folds
  for(i in 1:cv){
    
    m1 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp,family = binomial(link = "logit"), data=mat_treino[i])
    t1 <- predict(m1,newdata=data.frame(mat_teste[[i]]), level = 0.95, interval="prediction")
    rmse1[i,j] <- sqrt(apply(mat_teste[[i]][2] - t1,2,"mean")^2)
    
  }
  media <- min(rmse1)
}
media


