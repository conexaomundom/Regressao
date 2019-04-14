
rm(list = ls())

# Dados
banco <- bodyfat
banco
head(banco)

attach(banco)
names(banco)
typeof(banco)

teste <- unlist(banco)
as.matrix(banco)
banco <- banco[-182, ]
banco[182,]
which(banco == 0)

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

# retirando a variável Biceps com p-valor de 0.28878 no teste t.
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

# Retirando a variável Ankle com p-valor de 0.51892 no test t.
m_gl1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
              Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
              Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl1)

# Retirando a variável Chest com p-valor de 0.43586 no test t.
m_gl2 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl2)

# Retirando a variável Heigth com p-valor de 0.47240 no test t.
m_gl3 <- glm(formula = Percent ~ Age + Weigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl3)
# Retirando o intercepto com p-valor de 0.50322 no test t. 
m_gl4 <- glm(formula = Percent ~ 0+ Age + Weigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl4)
# Retirando a variável Biceps com p-valor de 0.25539 no test t.
m_gl5 <- glm(formula = Percent ~ 0+ Age + Weigth + Neck + 
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl5)
# Retirando a variável Neck com p-valor de 0.237828 no test t.
m_gl6 <- glm(formula = Percent ~ 0+ Age + Weigth +  
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = gaussian(link = "log"), data=banco)
summary(m_gl6)
# Retirando a variável Age com p-valor de 0.203988 no test t.
m_glf <- glm(formula = Percent ~ 0+  Weigth +  
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = gaussian(link = "log"), data=banco)
summary(m_glf)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# sendo siginificativas para o modelo gaussiano com função de ligação log.
















# modelo gaussiano com função de ligação inversa.
m_gin <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin)
# retirando a variável Ankle com p-valor de 0.46897 no teste t.
m_gin1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
               Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
               Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin1)
# retirando a variável Age com p-valor de 0.41158 no teste t.
m_gin2 <- glm(formula = Percent ~ Weigth + Heigth + Neck + Chest + 
                Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
                Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin2)
# retirando a variável THigh com p-valor de 0.44574 no teste t.
m_gin3 <- glm(formula = Percent ~ Weigth + Heigth + Neck + Chest + 
                Abdomen + Hip + Knee + Biceps + Forearm + 
                Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin3)
# retirando a variável Neck com p-valor de 0.383809 no teste t.
m_gin4 <- glm(formula = Percent ~ Weigth + Heigth + Chest + 
                Abdomen + Hip + Knee + Biceps + Forearm + 
                Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_gin4)
# retirando a variável Biceps com p-valor de 0.296152  no teste t.
m_ginf <- glm(formula = Percent ~ Weigth + Heigth + Chest + 
                Abdomen + Hip + Knee + Forearm + 
                Wrist, family = gaussian(link = "inverse"), data=banco)
summary(m_ginf)
# E ficou sendo o modelo final passando pelo teste t com 8 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo gaussiano com função de 
# ligação inversa.











##
# modelo Gamma com função de ligação inversa.
m_gai <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
              Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai)
# retirando a variável Thigh com p-valor de 0.98643 no teste t.
m_gai1 <- glm(formula = Percent ~ Age + Weigth + Neck + Chest + 
               Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
               Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai1)
# retirando a variável Chest com p-valor de 0.445928 no teste t.
m_gai2 <- glm(formula = Percent ~ Age + Weigth + Neck + 
                Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
                Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai2)
# retirando a variável Neck com p-valor de 0.19004 no teste t.
m_gai3 <- glm(formula = Percent ~ Age + Weigth + 
                Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
                Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai3)
# retirando a variável Biceps com p-valor de 0.231622 no teste t.
m_gai4 <- glm(formula = Percent ~ Age + Weigth + 
                Abdomen + Hip + Thigh + Knee + Forearm + 
                Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai4)
# retirando a variável Age com p-valor de 0.122481 no teste t.
m_gai5 <- glm(formula = Percent ~ Weigth + 
                Abdomen + Hip + Thigh + Knee + Forearm + 
                Wrist, family = Gamma(link = "inverse"), data=banco)
summary(m_gai5)
# retirando a variável 0.22607 com p-valor de 0.22607 no teste t.
m_gaif <- glm(formula = Percent ~ Weigth + 
                Abdomen + Hip + Thigh + Knee + Forearm, family = Gamma(link = "inverse"), data=banco)
summary(m_gaif)
# E ficou sendo o modelo final passando pelo teste t com 8 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo Gamma com função de 
# ligação inversa.















# modelo Gamma com função de ligação identidade.
m_gaid <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
            Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid)
# retirando o intercepto com p-valor de 0.97897 no teste t.
m_gaid1 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck + Chest + 
                Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid1)
# retirando a variável Heigth com p-valor de 0.885134 no teste t.
m_gaid2 <- glm(formula = Percent ~ 0 + Age + Weigth + Neck + Chest + 
                 Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                 Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid2)
# retirando a variável Chest com p-valor de 0.477671 no teste t.
m_gaid3 <- glm(formula = Percent ~ 0 + Age + Weigth + Neck + 
                 Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                 Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid3)
# retirando a variável Ankle com p-valor de 0.442782 no teste t.
m_gaid4 <- glm(formula = Percent ~ 0 + Age + Weigth + Neck + 
                 Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
                 Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid4)
# retirando a variável Weigth com p-valor de 0.290821 no teste t.
m_gaid5 <- glm(formula = Percent ~ 0 + Age + Neck + 
                 Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
                 Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid5)
# retirando a variável Knee com p-valor de 0.275857 no teste t.
m_gaid6 <- glm(formula = Percent ~ 0 + Age + Neck + 
                 Abdomen + Hip + Thigh +Biceps + Forearm + 
                 Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid6)
# Problemmmmmmmmms
# retirando a variável Biceps com p-valor de 0.153545  no teste t.
m_gaid7 <- glm(formula = Percent ~ 0 + Age + Neck + 
                 Abdomen + Hip + Thigh + Forearm + 
                 Wrist, family = Gamma(link = "identity"), data=banco)
summary(m_gaid7)
# Problemmmmmmmmms









# modelo Gamma com função de ligação log.
m_gal <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal)
# retirando a variável Knee com p-valor de 0.7996 no teste t.
m_gal1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal1)
# retirando o intercepto com p-valor de 0.69277 no teste t.
m_gal2 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal2)
# retirando a variável Chest com p-valor de 0.60877 no teste t.
m_gal3 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck +  
             Abdomen + Hip + Thigh + Ankle + Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal3)
# retirando a variável Ankle com p-valor de 0.49755 no teste t.
m_gal4 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck +  
             Abdomen + Hip + Thigh +  Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal4)
# retirando a variável Heigth com p-valor de 0.30067 no teste t.
m_gal5 <- glm(formula = Percent ~ 0 + Age + Weigth + Neck +  
             Abdomen + Hip + Thigh +  Biceps + Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_gal5)
# retirando a variável Biceps com p-valor de 0.12167 no teste t.
m_galf <- glm(formula = Percent ~ 0 + Age + Weigth + Neck +  
             Abdomen + Hip + Thigh +  Forearm + 
           Wrist, family = Gamma(link = "log"), data=banco)
summary(m_galf)
# E ficou sendo o modelo final passando pelo teste t com 8 variáveis explicativas
# sendo siginificativas para o modelo Gamma com função de ligação log.







## Problemmmmms
# modelo gaussiana  inversa com funcao de ligacao 1/mu^2
m_igmu <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = inverse.gaussian(link = "1/mu^2"), data=banco)
summary(m_igmu)
## Problemmmmms






















# modelo gaussiana  inversa com funcao de ligacao inverse. 
m_ig_inverse <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest +
                      Abdomen + Hip + Thigh + Knee + Ankle + Biceps + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse)
# Retirando a variável Ankle com p-valor 0.76086 no teste t.
m_ig_inverse1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest +
                      Abdomen + Hip + Thigh + Knee + Biceps + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse1)
# Retirando a variável Heigth com p-valor 0.53979 no teste t.
m_ig_inverse2 <- glm(formula = Percent ~ Age + Weigth + Neck + Chest +
                      Abdomen + Hip + Thigh + Knee + Biceps + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse2)
# Retirando a variável Heigth com p-valor 0.53979 no teste t.
m_ig_inverse3 <- glm(formula = Percent ~ Age + Weigth + Neck + 
                      Abdomen + Hip + Thigh + Knee + Biceps + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse3)
# Retirando a variável Knee com p-valor 0.165181 no teste t.
m_ig_inverse4 <- glm(formula = Percent ~ Age + Weigth + Neck + 
                      Abdomen + Hip + Thigh + Biceps + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse4)
# Retirando a variável Biceps com p-valor 0.131392 no teste t.
m_ig_inverse5 <- glm(formula = Percent ~ Age + Weigth + Neck + 
                      Abdomen + Hip + Thigh + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inverse5)
# Retirando a variável Weigth com p-valor 0.117300 no teste t.
m_ig_inversef <- glm(formula = Percent ~ Age + Neck + 
                      Abdomen + Hip + Thigh + 
                      Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
summary(m_ig_inversef)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo Inversa Gussiana com função
# de ligação inversa.













## Problemsssssss
# modelo gaussiana  inversa com funcao de ligacao identity. 
m_ig_identity <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
                      Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                      Wrist, family = inverse.gaussian(link = "identity"), data=banco)
## Problemsssssss
# modelo gaussiana  inversa com funcao de ligacao log.
m_ig_inverse <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
                      Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                      Wrist, family = inverse.gaussian(link = "log"), data=banco)
## Problemsssssssssssss 
# modelo quasi com funcao de ligacao logit. 
m_ql <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "logit"), data=banco)
## Problemsssssssssssss
# modelo quasi com funcao de ligacao probit. 
m_qp <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "probit"), data=banco)
## Problemsssssssssssss
# modelo quasi com funcao de ligacao cloglog. 
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "cloglog"), data=banco)
## Problemsssssssssssss
# modelo quasi com funcao de ligacao identity. 
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "identity"), data=banco)

















# modelo quasi com funcao de ligacao inverse. 
m_qi <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
              Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
              Wrist, family = quasi(link = "inverse"), data=banco)
summary(m_qi)
# Retirando a variável Ankle com p-valor 0.46897 no teste t.
m_qi1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
              Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
              Wrist, family = quasi(link = "inverse"), data=banco)
summary(m_qi1)
# Retirando a variável Age com p-valor 0.41158 no teste t.
m_qi2 <- glm(formula = Percent ~ Weigth + Heigth + Neck + Chest + 
              Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
              Wrist, family = quasi(link = "inverse"), data=banco)
summary(m_qi2)
# Retirando a variável Thigh com p-valor 0.44574 no teste t.
m_qi3 <- glm(formula = Percent ~ Weigth + Heigth + Neck + Chest + 
              Abdomen + Hip + Knee + Biceps + Forearm + 
              Wrist, family = quasi(link = "inverse"), data=banco)
summary(m_qi3)
# Retirando a variável Neck com p-valor 0.383809 no teste t.
m_qi4 <- glm(formula = Percent ~ Weigth + Heigth + Chest + 
              Abdomen + Hip + Knee + Biceps + Forearm + 
              Wrist, family = quasi(link = "inverse"), data=banco)
summary(m_qi4)
# Retirando a variável Biceps com p-valor 0.296152 no teste t.
m_qif <- glm(formula = Percent ~ Weigth + Heigth + Chest + 
              Abdomen + Hip + Knee + Forearm + 
              Wrist, family = quasi(link = "inverse"), data=banco)
summary(m_qif)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo quasi com função
# de ligação inversa.






# modelo quasi com funcao de ligacao log. 
m_ql <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "log"), data=banco)
summary(m_ql)
# Retirando a variável Ankle com p- valor de 0.51892.
m_ql1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "log"), data=banco)
summary(m_ql1)
# Retirando a variável Chest com p-valor 0.43586 no teste t.
m_ql2 <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck +
             Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "log"), data=banco)
summary(m_ql2)
# Retirando a variável Heigth com p-valor 0.47240 no teste t.
m_ql3 <- glm(formula = Percent ~ Age + Weigth + Neck +
             Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "log"), data=banco)
summary(m_ql3)
# Retirando o intercepto com p-valor 0.50322 no teste t.
m_ql4 <- glm(formula = Percent ~ 0 + Age + Weigth + Neck +
             Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "log"), data=banco)
summary(m_ql4)
# Retirando a variável Biceps com p-valor 0.25539  no teste t.
m_ql5 <- glm(formula = Percent ~ 0 + Age + Weigth + Neck +
             Abdomen + Hip + Thigh + Knee + Forearm + 
             Wrist, family = quasi(link = "log"), data=banco)
summary(m_ql5)
# Retirando a variável Neck com p-valor 0.237828  no teste t.
m_ql6 <- glm(formula = Percent ~ 0 + Age + Weigth + 
             Abdomen + Hip + Thigh + Knee + Forearm + 
             Wrist, family = quasi(link = "log"), data=banco)
summary(m_ql6)
# Retirando a variável Age com p-valor 0.203988   no teste t.
m_qlf <- glm(formula = Percent ~ 0 + Weigth + 
             Abdomen + Hip + Thigh + Knee + Forearm + 
             Wrist, family = quasi(link = "log"), data=banco)
summary(m_qlf)
# E ficou sendo o modelo final passando pelo teste t com 7 variáveis explicativas
# sendo siginificativas para o modelo quasi com função
# de ligação log.














# modelo quasi com funcao de ligacao 1/mu^2. 
m_qmu <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "1/mu^2"), data=banco)
summary(m_qmu)
# Retirando a variável Neck com p-valor 0.599899 no teste t.
m_qmu1 <- glm(formula = Percent ~ Age + Weigth + Heigth + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "1/mu^2"), data=banco)
summary(m_qmu1)
# Retirando a variável Ankle com p-valor 0.53722 no teste t.
m_qmu2 <- glm(formula = Percent ~ Age + Weigth + Heigth + Chest + 
             Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "1/mu^2"), data=banco)
summary(m_qmu2)
# Retirando a variável Age com p-valor 0.61452 no teste t.
m_qmu3 <- glm(formula = Percent ~ Weigth + Heigth + Chest + 
             Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "1/mu^2"), data=banco)
summary(m_qmu3)
# Retirando a variável Thigh com p-valor 0.557437 no teste t.
m_qmu4 <- glm(formula = Percent ~ Weigth + Heigth + Chest + 
             Abdomen + Hip + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "1/mu^2"), data=banco)
summary(m_qmu4)
# Retirando a variável Biceps com p-valor 0.342895 no teste t.
m_qmu5 <- glm(formula = Percent ~ Weigth + Heigth + Chest + 
             Abdomen + Hip + Knee + Forearm + 
             Wrist, family = quasi(link = "1/mu^2"), data=banco)
summary(m_qmu5)
# Retirando a variável Wrist com p-valor 0.131776 no teste t.
m_qmuf <- glm(formula = Percent ~ Heigth + Chest + 
             Abdomen + Hip + Knee + Forearm, family = quasi(link = "1/mu^2"), data=banco)
summary(m_qmuf)
# E ficou sendo o modelo final passando pelo teste t com 6 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo quasi com função
# de ligação log.















# modelo quasi com funcao de ligacao sqrt. 
m_qs <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "sqrt"), data=banco)
summary(m_qs)
# Retirando o intercepto com p-valor 0.94029 no teste t.
m_qs1 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "sqrt"), data=banco)
summary(m_qs1)
# Retirando a variável Chest com p-valor 0.53765 no teste t.
m_qs2 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "sqrt"), data=banco)
summary(m_qs2)
# Retirando a variável Ankle com p-valor 0.50236 no teste t.
m_qs3 <- glm(formula = Percent ~ 0 + Age + Weigth + Heigth + Neck + 
             Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "sqrt"), data=banco)
summary(m_qs3)
# Retirando a variável Heigth com p-valor 0.41587 no teste t.
m_qs4 <- glm(formula = Percent ~ 0 + Age + Weigth + Neck + 
             Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "sqrt"), data=banco)
summary(m_qs4)
# Retirando a variável Heigth com p-valor 0.41587 no teste t.
m_qs5 <- glm(formula = Percent ~ 0 + Age + Weigth + Neck + 
             Abdomen + Hip + Thigh + Knee + Biceps + Forearm + 
             Wrist, family = quasi(link = "sqrt"), data=banco)
summary(m_qs5)
# Retirando a variável Knee com p-valor 0.47508 no teste t.
m_qs6 <- glm(formula = Percent ~ 0 + Age + Weigth + Neck + 
             Abdomen + Hip + Thigh + Biceps + Forearm + 
             Wrist, family = quasi(link = "sqrt"), data=banco)
summary(m_qs6)
# Retirando a variável Biceps com p-valor 0.26627 no teste t.
m_qsf <- glm(formula = Percent ~ 0 + Age + Weigth + Neck + 
             Abdomen + Hip + Thigh + Forearm + 
             Wrist, family = quasi(link = "sqrt"), data=banco)
summary(m_qsf)
# E ficou sendo o modelo final passando pelo teste t com 8 variáveis explicativas
# mais o intercepto sendo siginificativas para o modelo quasi com função
# de ligação sqrt.