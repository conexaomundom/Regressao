
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

# Pegando os modelos finais e fazendo o teste de aqueção global neles.

m_gif <- glm(formula = Percent ~ Age + Weigth + Neck +  
               Abdomen + Thigh + Forearm + 
               Wrist, family = gaussian(link = "identity"), data=banco)
s1 <- summary(m_gif)

m_glf <- glm(formula = Percent ~ 0+  Weigth +  
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = gaussian(link = "log"), data=banco)
s2 <- summary(m_glf)
m_ginf <- glm(formula = Percent ~ Weigth + Heigth + Chest + 
                Abdomen + Hip + Knee + Forearm + 
                Wrist, family = gaussian(link = "inverse"), data=banco)
s3 <- summary(m_ginf)
m_gaif <- glm(formula = Percent ~ Weigth + 
                Abdomen + Hip + Thigh + Knee + Forearm, family = Gamma(link = "inverse"), data=banco)
s4 <- summary(m_gaif)
m_galf <- glm(formula = Percent ~ 0 + Age + Weigth + Neck +  
                Abdomen + Hip + Thigh +  Forearm + 
                Wrist, family = Gamma(link = "log"), data=banco)
s5 <- summary(m_galf)
m_ig_inversef <- glm(formula = Percent ~ Age + Neck + 
                       Abdomen + Hip + Thigh + 
                       Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
s6 <- summary(m_ig_inversef)
m_qif <- glm(formula = Percent ~ Weigth + Heigth + Chest + 
               Abdomen + Hip + Knee + Forearm + 
               Wrist, family = quasi(link = "inverse"), data=banco)
s7 <- summary(m_qif)
m_qlf <- glm(formula = Percent ~ 0 + Weigth + 
               Abdomen + Hip + Thigh + Knee + Forearm + 
               Wrist, family = quasi(link = "log"), data=banco)
s8 <- summary(m_qlf)
m_qmuf <- glm(formula = Percent ~ Heigth + Chest + 
                Abdomen + Hip + Knee + Forearm, family = quasi(link = "1/mu^2"), data=banco)
s9 <- summary(m_qmuf)
m_qsf <- glm(formula = Percent ~ 0 + Age + Weigth + Neck + 
               Abdomen + Hip + Thigh + Forearm + 
               Wrist, family = quasi(link = "sqrt"), data=banco)
s10 <- summary(m_qsf)