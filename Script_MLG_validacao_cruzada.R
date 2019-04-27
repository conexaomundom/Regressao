
rm(list = ls())

# Dados
banco <- bodPercentfat
banco
head(banco)

attach(banco)
names(banco)
typeof(banco)

# Avaliando a variável y,no caso a variável Percent, que tem uma observação que é 0
# e isso tem bloqueado que diversos outros modelos possam rodar.
# Dado essa situação uma alternativa é somar uma constante. Para não apenas
# sair somando sem fundo nem nexo a ideia e avaliar como se comporta a variável Percent
# olhar sua média, sua mediana e avalar qual dessas medidas de posição seria mais
# adequada pra somar. ou apenas somar um 10.

banco[182, ]
banco$Percent <- banco$Percent + 10
banco

# Avaliando a variável y,no caso a variável Percent, que tem uma observação que é 0
# e isso tem bloqueado que diversos outros modelos possam rodar.
# Dado essa situação uma alternativa é somar uma constante. Para não apenas
# sair somando sem fundo nem nexo a ideia e avaliar como se comporta a variável Percent
# olhar sua média, sua mediana e avalar qual dessas medidas de posição seria mais
# adequada pra somar. ou apenas somar um 10.

# Como a variável resposta é continua e não é contagem, podemos usar 
# as distribuições gaussiana, Gamma, inverse.gaussian
# nao podemos usar binomial, quasibinomial, nem opisson ou quasipoisson
# vamos fazer os modelos com todas as funções de ligação possível.


# Primeiramente com todas as variáveis e em seguinda ir fazendo a seleção 
# de variaveis.

# Rodar modelos MGL's e ver qual eh melhor por validacao cruzada.

# começando a seleção de variáveis ao nível de signiicancia de 10%.
# Modelo gaussiano com função de ligação identidade.
m_gi <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
            Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
            Wrist, family = gaussian(link = "identity"), data=banco)

summary(m_gi)
# retirando a variável Knee com p-valor de 0.94970 no teste t.
m_gi1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Ankle + Biceps + Forearm + 
             Wrist, family = gaussian(link = "identity"), data=banco)
summary(m_gi1)
# retirando a variável Chest com p-valor de 0.80454 no teste t.
m_gi2 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck +  
              Abdomen + Hip + Thigh + Ankle + Biceps + Forearm + 
              Wrist, family = gaussian(link = "identity"), data=banco)
summary(m_gi2)
# retirando a variável Heigth com p-valor de 0.49280 no teste t.
m_gi3 <- glm(formula = Percent ~ Age + Weigth + Neck +  
              Abdomen + Hip + Thigh + Ankle + Biceps + Forearm + 
              Wrist, family = gaussian(link = "identity"), data=banco)
summary(m_gi3)

# retirando a variável Ankle com p-valor de 0.39568 no teste t.
m_gi4 <- glm(formula = Percent ~ Age + Weigth + Neck +  
              Abdomen + Hip + Thigh + Biceps + Forearm + 
              Wrist, family = gaussian(link = "identity"), data=banco)
summary(m_gi4)

# retirando a variável Biceps com p-valor de 0.28878 no teste t.
m_gi5 <- glm(formula = Percent ~ Age + Weigth + Neck +  
              Abdomen + Hip + Thigh + Forearm + 
              Wrist, family = gaussian(link = "identity"), data=banco)
summary(m_gi5)

# retirando a variável Hip com p-valor de 0.15940 no teste t.
m_gif <- glm(formula = Percent ~ Age + Weigth + Neck +  
              Abdomen + Thigh + Forearm + 
              Wrist, family = gaussian(link = "identity"), data=banco)
summary(m_gif)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo gaussiano com função de 
# ligação identidade.











# Modelo gaussiano com função de ligação log.
m_gl <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl)
# retirando a variável Chest +  com p-valor de 0.641041  no teste t.
m_gl1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl1)
# retirando a variável Heigth +  com p-valor de 0.587286  no teste t.
m_gl2 <- glm(formula = Percent ~ Age + Weigth +  Neck + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl2)
# retirando a variável Ankle +com p-valor de 0.449845  no teste t.
m_gl3 <- glm(formula = Percent ~ Age + Weigth +  Neck + 
             Abdomen + Hip + Thigh + Knee +  Biceps + Forearm + 
                Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl3)
# retirando a variável Biceps + com p-valor de 0.271758 no teste t.
m_gl4 <- glm(formula = Percent ~ Age + Weigth +  Neck + 
             Abdomen + Hip + Thigh + Knee +  Forearm + 
                Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl4)
# retirando a variável Knee +  com p-valor de 0.25300 no teste t.
m_glf <- glm(formula = Percent ~ Age + Weigth +  Neck + 
             Abdomen + Hip + Thigh +  Forearm + 
                Wrist, family = gaussian(link = "log"), data=banco)
summary(m_glf)
# E ficou sendo o modelo final passando pelo teste t com 8 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo gaussiano com função de 
# ligação log.















# modelo gaussiano com função de ligação inversa.
m_gin <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin)
# retirando a variável Ankle com p-valor de 0.48380 no teste t.
m_gin1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin1)
# retirando a variável Chest com p-valor de 0.41261 no teste t.
m_gin2 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck +  
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin2)
# retirando a variável Heigth com p-valor de 0.37424 no teste t.
m_gin3 <- glm(formula = Percent ~ Age + Weigth + Neck +  
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin3)
# retirando a variável Heigth com p-valor de 0.37424 no teste t.
m_gin4 <- glm(formula = Percent ~ Age + Weigth + Neck +  
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin4)
# retirando a variávelBiceps +  com p-valor de 0.27592 no teste t.
m_gin5 <- glm(formula = Percent ~ Age + Weigth + Neck +  
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin5)
# retirando a variávelNeck +  com p-valor de 0.27592 no teste t.
m_gin6 <- glm(formula = Percent ~ Age + Weigth + 
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin6)
# retirando a variável Age + com p-valor de 0.27592 no teste t.
m_ginf <- glm(formula = Percent ~ Weigth + 
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_ginf)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo gaussiano com função de 
# ligação inversa.






##
# modelo Gamma com função de ligação inversa.
m_gai <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
              Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai)
# retirando a variável Heigth com p-valor de 0.9354 no teste t.
m_gai1 <- glm(formula = Percent ~ Age + Weigth + Neck + Chest + 
               Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
               Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai1)
# retirando a variável Ankle com p-valor de 0.62681 no teste t.
m_gai2 <- glm(formula = Percent ~ Age + Weigth + Neck + Chest + 
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai2)
# retirando a variável Neck com p-valor de 0.19004 no teste t.
m_gai3 <- glm(formula = Percent ~ Age + Weigth + Neck + Chest + 
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai3)
# retirando a variável Chest com p-valor de 0.61462 no teste t.
m_gai4 <- glm(formula = Percent ~ Age + Weigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai4)
# retirando a variável Age com p-valor de 0.122481 no teste t.
m_gai5 <- glm(formula = Percent ~ Age + Weigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai5)
# retirando a variável Biceps +  com p-valor de 0.20824 no teste t.
m_gai5 <- glm(formula = Percent ~ Age + Weigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai5)
# retirando a variávelNeck +   com p-valor de 0.14161 no teste t.
m_gaif <- glm(formula = Percent ~ Age + Weigth + 
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gaif)
# E ficou sendo o modelo final passando pelo teste t com 8 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo Gamma com função de 
# ligação inversa.















# modelo Gamma com função de ligação identidade.
m_gaid <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
            Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid)
# retirando a variável Chest com p-valor de 0.95040 no teste t.
m_gaid1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + 
            Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid1)
# retirando a variável Heigth com p-valor de 0.885134 no teste t.
m_gaid2 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck + 
            Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid2)
# retirando a variável Ankle + com p-valor de 0.46439 no teste t.
m_gaid3 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck + 
            Abdomen + Hip + Thigh + Knee +  Biceps + Forearm + 
           Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid3)
# retirando a variável knee + com p-valor de 0.51843 no teste t.
m_gaid4 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck + 
            Abdomen + Hip + Thigh +  Biceps + Forearm + 
           Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid4)
# retirando a variável Biceps +  com p-valor de 0.36870 no teste t.
m_gaid5 <- glm(formula = Percent ~ 0 + Age + Neck + 
                 Abdomen + Hip + Thigh + Knee + Forearm + 
                 Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid5)
# retirando a variável Knee com p-valor de 0.275890 no teste t.
m_gaidf <- glm(formula = Percent ~ 0 + Age + Neck + 
                 Abdomen + Hip + Thigh + Forearm + 
                 Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaidf)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# sendo siginificativas para o modelo Gamma com função de 
# ligação inversa.




m_igmu <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
                Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                Wrist, family = inverse.gaussian(link = "1/mu^2"), data=banco)
summary(m_igmu)
# Retirando a variável Heigth +  com p-valor de 0.93452 no teste t.
m_igmu1 <- glm(formula = Percent ~ Age + Weigth + Neck + Chest + 
                Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                Wrist, family = inverse.gaussian(link = "1/mu^2"), data=banco)
summary(m_igmu1)
# Retirando a variável Ankle +  com p-valor de 0.692248 no teste t.
m_igmu2 <- glm(formula = Percent ~ Age + Weigth + Neck + Chest + 
                 Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
                 Wrist, family = inverse.gaussian(link = "1/mu^2"), data=banco)
summary(m_igmu2)
# Retirando a variável Chest +  com p-valor de 0.593119 no teste t.
m_igmu3 <- glm(formula = Percent ~ Age + Weigth + Neck + 
                 Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
                 Wrist, family = inverse.gaussian(link = "1/mu^2"), data=banco)
summary(m_igmu3)
# Retirando a variável Neck +  com p-valor de 0.200119 no teste t.
m_igmu4 <- glm(formula = Percent ~ Age + Weigth + 
                 Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
                 Wrist, family = inverse.gaussian(link = "1/mu^2"), data=banco)
summary(m_igmu4)
# Retirando a variável Biceps +  com p-valor de 0.24827 no teste t.
m_igmu5 <- glm(formula = Percent ~ Age + Weigth + 
                 Abdomen + Hip + Thigh + Knee + Forearm + 
                 Wrist, family = inverse.gaussian(link = "1/mu^2"), data=banco)
summary(m_igmu5)
# Retirando a variável Age + com p-valor de 0.121001 no teste t.
m_igmu6 <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
                 Forearm + Wrist, family = inverse.gaussian(link = "1/mu^2"), data=banco)
summary(m_igmu6)
# Retirando a variável + Wrist com p-valor de 0.286829 no teste t.
m_igmuf <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
                 Forearm , family = inverse.gaussian(link = "1/mu^2"), data=banco)
summary(m_igmuf)
# E ficou sendo o modelo final passando pelo teste t com 6 variáveis explicativas
# sendo siginificativas para o modelo Inversa Gaussiana com função de 
# ligação "1/mu^2".














# modelo Gamma com função de ligação log.
m_gal <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal)
# retirando a variável Heigth  com p-valor de 0.81011 no teste t.
m_gal1 <- glm(formula = Percent ~ Age + Weigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal1)
# retirando a variavel Chest com p-valor de 0.75295 no teste t.
m_gal2 <- glm(formula = Percent ~ 0+ Age + Weigth + Neck + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal2)
# retirando a variável Hip + com p-valor de 0.8846 no teste t.
m_gal3 <- glm(formula = Percent ~ 0+ Age + Weigth + Neck + 
             Abdomen + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal3)
# retirando a variável Neck +  com p-valor de 0.49755 no teste t.
m_gal4 <- glm(formula = Percent ~ 0+ Age + Weigth + 
             Abdomen + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal4)
# retirando a variável Ankle + com p-valor de 0.1861 no teste t.
m_gal4 <- glm(formula = Percent ~ 0+ Age + Weigth + 
             Abdomen + Thigh + Knee + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal4)
# retirando a variável Biceps com p-valor de 0.1626 no teste t.
m_galf <- glm(formula = Percent ~ 0+ Age + Weigth + 
             Abdomen + Thigh + Knee + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_galf)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# sendo siginificativas para o modelo Gamma com função de ligação log.





















# modelo gaussiana  inversa com funcao de ligacao inverse. 
m_ig_inverse <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest +
                      Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse)
# Retirando a variável Chest + com p-valor 0.718193no teste t.
m_ig_inverse1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + 
                      Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse1)
# Retirando a variável Ankle com p-valor 0.58826no teste t.
m_ig_inverse2 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + 
                      Abdomen + Hip + Thigh + Knee +Biceps + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse2)

# Retirando a variável Ankle com p-valor 0.58826no teste t.
m_ig_inverse3 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + 
                       Abdomen + Hip + Thigh + Knee +Biceps + 
                       Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse3)
# Retirando a variável Heigth + com p-valor 0.56512 no teste t.
m_ig_inverse4 <- glm(formula = Percent ~ Age + Weigth + Neck + 
                       Abdomen + Hip + Thigh + Knee +Biceps + 
                       Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse4)
# Retirando a variável Knee + com p-valor 0.56512  no teste t.
m_ig_inverse5 <- glm(formula = Percent ~ Age + Weigth + Neck + 
                       Abdomen + Hip + Thigh + Biceps + 
                       Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse5)
# Retirando a variável Biceps +  com p-valor 0.20463  no teste t.
m_ig_inversef <- glm(formula = Percent ~ Age + Weigth + Neck + 
                       Abdomen + Hip + Thigh + 
                       Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inversef)
# E ficou sendo o modelo final passando pelo teste t com 8 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo Inversa Gaussiana com função de ligação inversa.













# Modelo com inversa gaussiana com função de ligação identidade.
m_ig1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest +
                      Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
                      Forearm + Wrist, family = inverse.gaussian(link = "identity"), data=banco)
summary(m_ig1)
# Retirando a variável Heigth +  com p-valor 0.847767 no teste t.
m_ig2 <- glm(formula = Percent ~ Age + Weigth + Neck + Chest +
               Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
               Forearm + Wrist, family = inverse.gaussian(link = "identity"), data=banco)
summary(m_ig2)
# Retirando a variável Chest + com p-valor 0.712841 no teste t.
m_ig3 <- glm(formula = Percent ~ Age + Weigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
               Forearm + Wrist, family = inverse.gaussian(link = "identity"), data=banco)
summary(m_ig3)
# Retirando o intercepto com p-valor 0.712841 no teste t.
m_ig4 <- glm(formula = Percent ~ 0 +  Age + Weigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
               Forearm + Wrist, family = inverse.gaussian(link = "identity"), data=banco)
summary(m_ig4)
# Retirando a variável Weigth +  com p-valor0.708071 no teste t.
m_ig5 <- glm(formula = Percent ~ 0 +  Age + Neck + 
               Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
               Forearm + Wrist, family = inverse.gaussian(link = "identity"), data=banco)
summary(m_ig5)
# Retirando a variável Ankle +  com p-valor 0.438746 no teste t.
m_ig6 <- glm(formula = Percent ~ 0 +  Age + Neck + 
               Abdomen + Hip + Thigh + Knee + Biceps + 
               Forearm + Wrist, family = inverse.gaussian(link = "identity"), data=banco)
summary(m_ig6)
# Retirando a variável Biceps + com p-valor 0.444659 no teste t.
m_ig7 <- glm(formula = Percent ~ 0 +  Age + Neck + 
               Abdomen + Hip + Thigh + Knee + 
               Forearm + Wrist, family = inverse.gaussian(link = "identity"), data=banco)
summary(m_ig7)
# Retirando a variável Knee +  com p-valor 0.186663 no teste t.
m_ig8 <- glm(formula = Percent ~ 0 +  Age + Neck + Abdomen + Hip + Thigh + 
               Forearm + Wrist, family = inverse.gaussian(link = "identity"), data=banco)
summary(m_ig8)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# sendo siginificativas para o modelo gaussiana inversa com função de ligação log.














# Modelo com inversa gaussiana com função de ligação log.
m_igl <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest +
               Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
               Forearm + Wrist, family = inverse.gaussian(link = "log"), data=banco)
summary(m_igl)
# Retirando a variável Chest + com p-valor 0.93640 no teste t.
m_igl1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
               Forearm + Wrist, family = inverse.gaussian(link = "log"), data=banco)
summary(m_igl1)
# Retirando a variável Knee +  com p-valor 0.70975 no teste t.
m_igl2 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + 
                Abdomen + Hip + Thigh + Ankle + Biceps + 
                Forearm + Wrist, family = inverse.gaussian(link = "log"), data=banco)
summary(m_igl2)
# Retirando a variável Heigth +  com p-valor 0.60186 no teste t.
m_igl3 <- glm(formula = Percent ~ Age + Weigth +Neck + 
                Abdomen + Hip + Thigh + Ankle + Biceps + 
                Forearm + Wrist, family = inverse.gaussian(link = "log"), data=banco)
summary(m_igl3)
# Retirando a variável Ankle +  com p-valor 0.491255 no teste t.
m_igl4 <- glm(formula = Percent ~ Age + Weigth +Neck + 
                Abdomen + Hip + Thigh + Biceps + 
                Forearm + Wrist, family = inverse.gaussian(link = "log"), data=banco)
summary(m_igl4)
# Retirando a variável Biceps +  com p-valor 0.209791 no teste t.
m_igl5 <- glm(formula = Percent ~ Age + Weigth +Neck + 
                Abdomen + Hip + Thigh + 
                Forearm + Wrist, family = inverse.gaussian(link = "log"), data=banco)
summary(m_igl5)
# Retirando a variável Weigth + com p-valor 0.139854  no teste t.
m_iglf <- glm(formula = Percent ~ Age + Neck + Abdomen + Hip + Thigh + 
          Forearm + Wrist, family = inverse.gaussian(link = "log"), data=banco)
summary(m_iglf)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo gaussiana inversa com função de ligação log.
