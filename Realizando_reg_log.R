# Começando a mexer no banco titanic
install.packages("caret")
#Regressao no R

rm(list = ls())
install.packages("ROCR")
install.packages("Epi")
install.packages("SDMTools")
library(nortest) # Testes de normalidade 
library(boot) 


rm(list = ls())

banco <- titanic_train
attach(banco)
names(banco)
# variáveis contidas no banco.
# "X", "Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare","Embarked"

banco[ ,5] <- ceiling(banco[ ,5])
which(banco[, 5] == 0)

# MELHORAR ESSA PARTE.

# Realizando a seleção de variáveis de acordo com o teste t, deixando no 
# modelo apenas as variáveis que tiveram p-valor menor que 5%, o nível de 
# significancia adotado.
# Primeiro realizando a seleção de variáveis já que sendo a regressão 
# logistica, usa-se o modelo de regressão generaizado com distribuição
# binomial e função de ligação "logit", os teste t sendo feito ao nível 
# de significância de 5%.

modelo <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                Fare + Embarked, family = binomial(link = "logit"), data=banco)
summary(modelo)

# Retirando a variável Embarked com p-valor 0.93856
modelo1 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                Fare, family = binomial(link = "logit"), data=banco)
summary(modelo1)

# Retirando a variável Parch com p-valor 0.34329
modelo2 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp +  
                 Fare, family = binomial(link = "logit"), data=banco)
summary(modelo2)

# Retirando a variável Fare com p-valor 0.295925 
modelo3 <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp,
               family = binomial(link = "logit"), data=banco)
#Logistic Regression
library(xtable)
xtablesummary((modelo3))

# Seleão de variáveis realizada e o modelo final ficou com 4 variáveis 
# explicativas mais o intercepto.

b <- modelo3

#Overall Goodness of fit
s = summary(b); 
desvio=modelo$deviance
q.quadr=qchisq(0.90,modelo$df.residual)

desvio<q.quadr
# Agora verificando se o modelo é adequado, e sim o modelo dresultou em 
# ser adequado sim.

# Agora fazendo a analise de reíduos e diagnóstico.

#Valor ajustado e desvio residual
fit = fitted(b)
library(boot)
devres = glm.diag(b)$rd

#Grafico de Normalidade
library(nortest)
qqnorm(devres); qqline(devres, col=2)

# A partir do QQ Plot dos resíduos padronizados podemos observar que os 
# pontos estão um pouco distorcidos da reta de normalidade, podendo 
# assim suspeitar de uma não normalidade estar presente nos resíduos
# padronizados, agora vamos fazer o teste de Lilliefors e de Shapiro
# Wilks para ter uma conclusão mais precisa.


lillie.test(devres)
shapiro.test(devres)

# E em ambos os testes de normalidade H0 foi rejeitada, foi rejeitada a 
# hipótese de normalidade nos desvios padronizados com p-valores de 
# < 2.2e-16 e < 2.2e-16 no teste de Lilliefors e do Shapiro-WIlk 
# repspectivamente, podendo afirmar assim que os resíduos padronizados
# não seguem a normalidade.

#Verificar a funÏ„Ï€o de variÎ“ncia
plot(fit,devres, main = "Função de variância") 

# É possível observar tendÊncias decrescentes
# muito claras.
#Verificar a funÏ„Ï€o de LigaÏ„Ï€o
plot(fit,Survived,main = "Função de ligação")

# Perfeito, assuminfo 0 e 1.

#Indep. Erros
acf(devres)

# Analisando o ACF dos desvios padronizados podemos ver que o sétimo lag 
# está fora do intervalo de confiança com relação a autocorrelação dos 
# desvioa padronizados, porém como mesmo passando do limite superior do 
# intervalo de confiança continua a ser uma correlação baixa, próximo de
# 0.3, talvez não sendo muito preocupante, , mas fica a cargo do pesquisador
# decidir se considera como correlacionados ou não enquanto os demais lag's
# estão contidos no intervalo de confiança abaixo de $ \vert 0.2 \vert $.
# Ṕodendo interpretar que os desvios padronizados não são correlacionados.

#Odds Ratio
xtable(as.table(exp(b$coefficients[-1]))) # Cautela ao rodar este comando

# Essa é a razão de chances...

fit=fitted(b) # probabilidades estimadas para cada individuo da amostra

#Curva ROC
#? necess?rio instalar a biblioteca ROCR

library(ROCR)

#Considere em seu modelo o sucesso como Y=1 e o fracasso como Y=0.
pred <- prediction(fitted(b), Survived)
perf <- performance(pred,"tpr","fpr") #Escolha do ponto de corte, TP e FP
area <- performance(pred,"auc") #Calcula a ?rea sob a curva ROC
plot(perf, main = " Cruva ROC") #Constroi o gr?fico da curva ROC

# Olhanco para a curva ROC podemos observar a qualidade do ajuste para o modelo,
# pois é obtido com base nas taxas de erro e acerto do modelo, possibilitando
# pegar o ponto de corte que maximiza o True Positive e minimize o False 
# Positive, podendo plotar uma reta que vai do canto superior esquerdo e acabe 
# no canto inferior direito e ao epgar o ponto em que a reta cruza com a curva é
# uma boa opção de???? HELP ME PLEASE. 
# PENSAR UMA FORMA DE IDENTIFICAR COMO PEGAR ESSE PONTO.

#Outra Opcao da ROC

library(Epi)
ROC(form = Survived ~ Pclass + Sex + Age + SibSp,plot="ROC",MX=FALSE)

# Montando a matriz de contingencia
library(SDMTools)
confusion.matrix(Survived, fit, threshold = 0.5)

# NÃO FUNCIONOU ESSE PACOTE NO MEU PC,ENTÃO NEM SEI A SAÍDA QUANTO MAIS O QUE 
# COMENTAR DELA.
