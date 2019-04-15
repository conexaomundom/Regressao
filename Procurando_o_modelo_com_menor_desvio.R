
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

#Desvio escalonado do ajuste para validaÏ„Ï€o do modelo
desvio1 = s1$deviance/s1$dispersion
desvio2 = s2$deviance/s2$dispersion 
desvio3 = s3$deviance/s3$dispersion 
desvio4 = s4$deviance/s4$dispersion 
desvio5 = s5$deviance/s5$dispersion 
desvio6 = s6$deviance/s6$dispersion 
desvio7 = s7$deviance/s7$dispersion 
desvio8 = s8$deviance/s8$dispersion 
desvio9 = s9$deviance/s9$dispersion 
desvio10 = s10$deviance/s10$dispersion 

#q.quadr com n-p graus de liberdade, onde p Î˜ o nÂ·mero de parÎ“metros do modelo

q.quadr1 = qchisq(0.95, s1$df.residual)
q.quadr2 = qchisq(0.95, s2$df.residual)
q.quadr3 = qchisq(0.95, s3$df.residual)
q.quadr4 = qchisq(0.95, s4$df.residual)
q.quadr5 = qchisq(0.95, s5$df.residual)
q.quadr6 = qchisq(0.95, s6$df.residual)
q.quadr7 = qchisq(0.95, s7$df.residual)
q.quadr8 = qchisq(0.95, s8$df.residual)
q.quadr9 = qchisq(0.95, s9$df.residual)
q.quadr10 = qchisq(0.95, s10$df.residual)


# Teste global do ajuste do modelo

desvio1 < q.quadr1 #se Verdadeiro modelo adequado T
desvio2 < q.quadr2 # T
desvio3 < q.quadr3 # T
desvio4 < q.quadr4 # F
desvio5 < q.quadr5 # F
desvio6 < q.quadr6 # F
desvio7 < q.quadr7 # T
desvio8 < q.quadr8 # T
desvio9 < q.quadr9 # T
desvio10 < q.quadr10 # T

desvio1
desvio2
desvio3
desvio7
desvio8
desvio9
desvio10

# O menor desvio foi o modelo com a distribuição Gaussiana com função de
# ligação inversa e a quasi coma  função de ligação log. Mas como que não
# conheço muito e como o desvio foi o mesmo valor então vamos usar o modelo
# de regressão generalizado com distribuição Gaussiana e função de ligação
# inverse e é com esse modelo que vamos realizar a análise de resíduos e
# diagóstico.

modelo <- m_ginf

#Valor ajustado e desvio residual
fit = fitted(modelo)
library(boot)
devres = glm.diag(modelo)$rd


#Grafico de Normalidade
library(nortest)
lillie.test(devres)
shapiro.test(devres)
qqnorm(devres); qqline(devres, col=2)
# Rejeitamos a hipótese de normalidade em ambos os testes realizados,
# de Lilliefors e Shapiro-Wilk, ou seja rejeitamos a hipótese de haver
# normalidade do desvio resídual.

#Verificar a funÏ„Ï€o de variÎ“ncia
plot(fit,devres) 

#Verificar a funÏ„Ï€o de LigaÏ„Ï€o
plot(fit,banco$Percent)

#Indep. Erros
acf(devres)
# De acordo com o ACF do desvio residual 4 lags
# caem foram do intervalo de confiança da 
# autocorrelação, porém a correlação em todos os
# 4 lags são menores que 0.2, o que é uma correlação
# baixa, mesmo passando, estando acima do intervalo
# de confiança. Então pode ser considerada a 
# autocorrelação por não ser um ou dois lags e sim 4,
# ou fica a cargo do pesquisador considerar se existe 
# ou não autocorrelação.

# Vamos realizar um teste de autocorrelação que não
# considera apenas a correlação até o primeiro lag. que 
# ouvi na aula de economegtria, porém não lembro o nome,
# mas irei pesquisar.

# Durbin whatson maybe poderia detectar alguma correlação,
# já que já no segundo lag já cai fora do intervalo de
# confiança.
library(lmtest)
dwtest(modelo)
# Rejeitamos a hipótese nula de que existe autocorrelação
# com p-valor de 0.02984, ao nível de significância de %.
# talves realmente não considerariamos que há autocorrelação
# mesmo sendo 4 lags que cairam fora do intervalo de confiança.

#Pontos aberrantes
plot(devres, ylim = c(3,-3))
abline(h=-2, col = 2); abline(h=2, col=2)
which(abs(devres)>2) #idenifica no grafico as observacoes aberrantes
# Avaliando os pontos aberrantes, os pontos detectados foram 39, 55, 
# 171, 172, 206, 207, 223, 224, em seguida vamos ver se algum desses pontos
# são significativos para o modelo e os que não forem significativos 
# para o modelo poderão ser retirados que afetará em nada.


# Pontos de Alavanca e Influentes
# Visualizacao Grafica

#glm.diag.plots(b)

n=nrow(banco)
p = (s3$df.null - s3$df.residual) # num. de variaveis do modelo (atencao ao Beta_zero))

#Identificacao de pontos de alavanca
plot(glm.diag(modelo)$h)
abline(h=2*(p/n), col = 2)
which(glm.diag(modelo)$h > 2*(p/n))
# Avaliando os pontos de alavanca 5, 36, 39, 40, 41, 42, 59,
# 65, 112, 140, 169, 191, 204, 207, 215, 221, 241, 243, 244,
# 246, 249, 250, 251 são os pontos de alavanca, então estes pontos
# não podem ser retirados do modelo se não o afetaria, mas como
# o ponto 207 é um ponto aberrante e de alavanca, ou seja o ponto
# 207 não poderá ser retirado do modelo.


#Identificacao de pontos de influencia
plot(glm.diag(modelo)$cook)
abline(h=qchisq(0.05,p)/p, col = 2)
which(glm.diag(modelo)$cook > qchisq(0.05,p)/p)
# Avaliando os pontos de influência os pontos de 
# 39 e 42 ou seja são pontos importantes, e o ponto
# 39 que é um ponto aberrante e de influencia não
# pode ser retirado do modelo.

# Concluindo que dos pontos aberrantes identificados 
# os pontos 55, 171, 172, 206, 223, 224 podem ser retirados 
# do modelo sem afeta-lo pois são não pontos nem
# aberrantes nem de influência, em contrapartida os pontos
# 207 e 39 não podem ser retirados por são pontos de alavanca 
# e de influência respectivamentee retiralos afetaria o modelo
# de regressão.