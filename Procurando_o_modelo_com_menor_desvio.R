
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


# Como a variável resposta é continua e não é contagem, podemos usar 
# as distribuições gaussiana, Gamma, inverse.gaussian
# nao podemos usar binomial, quasibinomial, nem opisson ou quasipoisson
# vamos fazer os modelos com todas as funções de ligação possível.


# Primeiramente com todas as variáveis e em seguinda ir fazendo a seleção 
# de variaveis.

# Rodar modelos MGL's e ver qual eh melhor por validacao cruzada.

# Pegando os modelos finais e fazendo o teste de aqueção global neles.

m_gif <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Thigh + Forearm + 
               Wrist, family = gaussian(link = "identity"), data=banco)
s1 <- summary(m_gif)

m_glf <- glm(formula = Percent ~ Age + Weigth +  Neck + Abdomen + Hip + Thigh +  Forearm + 
               Wrist, family = gaussian(link = "log"), data=banco)
s2 <- summary(m_glf)

m_ginf <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + Forearm + 
                Wrist, family = gaussian(link = "inverse"), data=banco)
s3 <- summary(m_ginf)

m_gaif <- glm(formula = Percent ~ Age + Weigth + Abdomen + Hip + Thigh + Knee + Forearm + 
                Wrist, family = Gamma(link = "inverse"), data=banco)
s4 <- summary(m_gaif)

m_gaidf <- glm(formula = Percent ~ 0 + Age + Neck + Abdomen + Hip + Thigh + Forearm + 
                 Wrist, family = Gamma(link = "identity"), data=banco)
s5 <- summary(m_gaidf)

m_igmuf <- glm(formula = Percent ~ Weigth + Abdomen + Hip + Thigh + Knee + 
                 Forearm , family = inverse.gaussian(link = "1/mu^2"), data=banco)
s6 <- summary(m_igmuf)

m_galf <- glm(formula = Percent ~ 0+ Age + Weigth + Abdomen + Thigh + Knee + Forearm + 
                Wrist, family = Gamma(link = "log"), data=banco)
s7 <- summary(m_galf)

m_ig_inversef <- glm(formula = Percent ~ Age + Weigth + Neck + Abdomen + Hip + Thigh + 
                       Forearm + Wrist, family = inverse.gaussian(link = "inverse"), data=banco)
s8 <- summary(m_ig_inversef)

#Desvio escalonado do ajuste para validaÏ„Ï€o do modelo
desvio1 = s1$deviance/s1$dispersion
desvio2 = s2$deviance/s2$dispersion 
desvio3 = s3$deviance/s3$dispersion 
desvio4 = s4$deviance/s4$dispersion 
desvio5 = s5$deviance/s5$dispersion 
desvio6 = s6$deviance/s6$dispersion 
desvio7 = s7$deviance/s7$dispersion 
desvio8 = s8$deviance/s8$dispersion 



#q.quadr com n-p graus de liberdade, onde p Î˜ o nÂ·mero de parÎ“metros do modelo

q.quadr1 = qchisq(0.95, s1$df.residual)
q.quadr2 = qchisq(0.95, s2$df.residual)
q.quadr3 = qchisq(0.95, s3$df.residual)
q.quadr4 = qchisq(0.95, s4$df.residual)
q.quadr5 = qchisq(0.95, s5$df.residual)
q.quadr6 = qchisq(0.95, s6$df.residual)
q.quadr7 = qchisq(0.95, s7$df.residual)
q.quadr8 = qchisq(0.95, s8$df.residual)


# Teste global do ajuste do modelo

desvio1 < q.quadr1 # T
desvio2 < q.quadr2 # T
desvio3 < q.quadr3 # T
desvio4 < q.quadr4 # T
desvio5 < q.quadr5 # T
desvio6 < q.quadr6 # F
desvio7 < q.quadr7 # T
desvio8 < q.quadr8 # T

desvio1
desvio2
desvio3
desvio4
desvio5
desvio7
desvio8

# O menor desvio foi o modelo com a distribuição Gaussiana com função de
# ligação log e com esse modelo que vamos realizar a análise de resíduos e
# diagóstico.

modelo <- m_glf


#Valor ajustado e desvio residual
fit = fitted(modelo)
library(boot)
devres = glm.diag(modelo)$rd


#Grafico de Normalidade
library(nortest)
qqnorm(devres); qqline(devres, col=2)
# Ao analisar o QQ-PLot a normalidade pode ser aceita, já que os pontos 
# plotados estão muito proximos à reta de normalidade, bem acentadas na reta
# concluindo que o  QQ-Plot foi bem feito.
lillie.test(devres)
shapiro.test(devres)

# Não Rejeitamos a hipótese de normalidade em ambos os testes realizados 
# de Lilliefors e Shapiro-Wilk com p-valores 0.253 e 0.4524 respectivamente,
# ou seja não rejeitamos a hipótese de haver normalidade do desvio resídual.

# Verificar a função de variÎ“ncia
plot(fit,devres)
# Avaliando a função de ligação talvez tenha influência dos pontos em fit
# que estão entre 40 e 50, porém o que podemos dizer que tem como identificar
# uma tendencia decrescente da variância, que no início é maior e em seguida.

# Verificar acfunção de LigaÏ„Ï€o
plot(fit,banco$Percent)
# Como o gráfico de função de ligação não apresentou linearização
# uma curva pouco acentuada expŕessaria melhor a relação
# sinalizando que a função de ligação talvez não tenha sido tão adequada.

#Indep. Erros
acf(devres)
# De acordo com o ACF do desvio residual nenhum dos lags
# caem foram do intervalo de confiança da autocorrelação,
# além de todos os lags são menores que 0.2, o que é uma 
# correlação baixa, podendo concluir que não há autocorrelação
# no desvio residual.


# Durbin whatson maybe poderia detectar alguma correlação, ou não né
library(lmtest)
dwtest(modelo)
# Rejeitamos a hipótese nula de que existe autocorrelação
# com p-valor de 0.0389, ao nível de significância de 5%.
# então realmente não considerariamos que há autocorrelação.

#Pontos aberrantes
plot(devres, ylim = c(3,-3))
abline(h=-2, col = 2); abline(h=2, col=2)
which(abs(devres)>2) #idenifica no grafico as observacoes aberrantes
# Avaliando os pontos aberrantes, os pontos detectados foram 39, 81, 82
# 171, 172, 207, 224, 225 e 250 em seguida vamos ver se algum desses pontos
# são significativos para o modelo e os que não forem significativos 
# para o modelo poderão ser retirados que afetará em nada.


# Pontos de Alavanca e Influentes
# Visualizacao Grafica

#glm.diag.plots(b)

n=nrow(banco)
p = (s2$df.null - s2$df.residual) # num. de variaveis do modelo (atencao ao Beta_zero))

#Identificacao de pontos de alavanca
plot(glm.diag(modelo)$h)
abline(h=2*(p/n), col = 2)
which(glm.diag(modelo)$h > 2*(p/n))
# Avaliando os pontos de alavanca 5, 15, 36, 39, 41, 42, 43, 59, 66, 
# 106, 112, 157, 159, 169, 175, 203, 205, 206, 216, 222, 242, 247,
# 250, 252 são os pontos de alavanca, então estes pontos
# não podem ser retirados do modelo se não o afetaria, além de
# os pontos 39 e 250 são pontos aberrante e de alavanca, ou seja os pontos
# 39 e 250 não poderão ser retirados do modelo.


#Identificacao de pontos de influencia
plot(glm.diag(modelo)$cook)
abline(h=qchisq(0.05,p)/p, col = 2)
which(glm.diag(modelo)$cook > qchisq(0.05,p)/p)
# Avaliando os pontos de influência os pontos de 
# 39 ou seja são pontos importantes, e o ponto
# 39 que é um ponto aberrante, de alavanca e de
# influencia não pode ser retirado do modelo.

# Concluindo que dos pontos aberrantes identificados foram
# 81, 82, 171, 172, 207, 224 e 225 podem ser retirados do 
# modelo sem afeta-lo pois são não pontos nem aberrantes nem
# de influência, em contrapartida os pontos 39 e 250 não podem
# ser retirados, pois o ponto 39 além de de ser aberrante é de
# alavanca é de influência também e o ponto 250 é ponto de
# influência por ser pontos importantes e retiralos afetaria o 
# modelo de regressão.