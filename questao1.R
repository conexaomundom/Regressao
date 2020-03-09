install.packages("ROCR")
install.packages("Epi")
install.packages("SDMTools")

# Questao1 da lista de regressão logistica.

# Um estudo sobre auto-avaliação geral de saúde
# (1 = saúde adequada, 0 = saúde inadequada) em 
# 30 indivíduos com idade variando de 20 a 95 anos.
# O objetivo é estudar a relação entre a auto-avaliação
# de saúde (Y) e as seguintes variáveis explicativas
# idade (em anos) e renda familiar per capita (1 = mais
# de 3 s.m, 0 = até 3 s.m).

idade <- c(21,20,25,26,22,35,36,40,42,46,59,50,60,72,85,59,29,45,39,45,20,25, 
        36,58,95,52,80,85,62,72)
renda <- c(1,1,1,1,0,0,1,1,1,1,1,0,1,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1)
saude <- c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

# a) Ajustar o modelo de regressão logística percorrendo todas
# as etapas até o modelo final. Considerando alpha = 0.10. 

ajuste1 <- glm(saude ~ idade + as.factor(renda), binomial(link = "logit"))
summary(ajuste1)

# Ao nível de siginificancia de 10% o modelo de regressão logistica
# para explicar a auto-avaliação geral de saúde resultou que 
# as variáveis explicativas idade e renda familiar per capita.






# 
s <- summary(ajuste1)
desvio <- ajuste1$deviance
q_quadrado <- qchisq(0.90, ajuste1$df.residual)

desvio < q_quadrado

# O Modelo passou bem no ajuste de adequação global,
# desvio < q_quadrado 18.71078 <  36.74122.
# De acordo com o teste de adequação global
# o modelo está bem ajustado.

###############################################
# Analise de residuos
################################################
fit <- fitted(ajuste1)
devres <- glm.diag(ajuste1)$rd

# Grafico de normalidade dos residuos
library(nortest)
lillie.test(devres)
shapiro.test(devres)
qqnorm(devres); qqline(devres, col=2)
# Não rejeitou a normalidade dos residuos nos dois testes
# de liliiefors e de Shapiro Wilk, com p-valores respectivamente
# o gráfico do qqnorm apresenta bem o resultado do p-valor 
# do teste de lilliefors porque até que segue a linha 
# vermelha no meio, mas nas estremidades nao muito, então
# justifica passar no teste, porém com um p-valor nao muito
# alto.
# Concluindo que mesmo raspando, mas o pressuposto de
# normalidade nos residuos não foi respeitado.

#Verificar a funcao de variancia
plot(fit,devres) 
plot(devres)

# Os desvios residuais estão entre -2 e 2, a grande maioria
#  está entre -1 e 1,ou seja demonstrando um bom modelo,
# porém para umas 6 observações que estão se distanciando
# no canto inferior esquerdo do gráfico e umas 3 observações 
# que também estão se distanciando das demias observações 
# mais proximas do canto superior direito o modelo não 
# nao se ajusta bem, concluindo que pra grande maioria 
# pode ser um bom modelo, mas como há observações que
# não estão sendo bem ajustadas, então não está muito
# bem ajustado não.



#Verificar a funÏ„Ï€o de LigaÏ„Ï€o
plot(fit,saude)
# Graande parte de quem era 1 foi estimado com probabilidade
# acima de 0.80, apenas 3 pontos que foram estimados
# com probabilidades menores, porem ainda um desses 3
# teve probabilidade maior que 0.5 sinalizando que o medelo
# capta bem para estimar a ter uma saúde adequada, porém em 
# 2 pontos que o indivisuo tinha saude adequada o modelo
# ajustou probabilidades menors que 0.5, incluvise 1 menor que
# 0.2.
# Já na parte de quem tinham saúde inadequada o modelo
# também não teve um resultado muito ruim, por mais que
# algumas estimativas passearam entre 0.10 e 0.5, mas usando
# um lambda básico de 0.5 ainda teve um bom desempenho 
# apenas uma única informação que era 0 e o modelo
# estimou com probabilidade acima de 0.80.


#Indep. Erros
acf(devres)
# O acf não foi bom, descartando o primeiro lag, logo em seguida
# 4 lags ficam fora do intervalo de confiança, sinalidando
# que a suposição de não corelação dos erros pode ou deve estar
# sendo violada.

#Odds Ratio
exp(ajuste1$coefficients[2]) # varivel idade
1/exp(ajuste1$coefficients[3]) # variavel renda
# Interpretando os parametros
# idade = 1.14220209, ou seja, a medida que o indiviudo envelhece
# aumenta em 4.22% a chance de y = 1, isto é, a chance de 
# do indivíduo ter uma saúde adequada, em relação ao ano
# anterior.

# renda = 24.02217, o fato do indivíduo ter uma renda familiar
# per capita maior que 3 salários mínimos reduz em 24 vezes 
# a probabilidade de y = 1, reduz em 24 vezes a probabilidade
# do indivíduo ter uma saúde adequada, em relação ao indivíduo
# que tem uma renda familiar per capita em até que 3 salários
# mínimos.

# c)
# Construa a curva ROC. Escolha o melhor ponto de corte. Obtenha as 
# taxas de verdadeiro positivo, falso positivo e AUC. Interprete todos 
# os resultados.

fit=fitted(ajuste1) # probabilidades estimadas para cada individuo da amostra

#Curva ROC
#? necess?rio instalar a biblioteca ROCR
library(ROCR)

#Considere em seu modelo o sucesso como Y=1 e o fracasso como Y=0.
pred <- prediction(fitted(ajuste1), saude)
perf <- performance(pred,"tpr","fpr") #Escolha do ponto de corte, TP e FP
area <- performance(pred,"auc") #Calcula a ?rea sob a curva ROC


area
# Observando a curva ROC também permite verificar quanto do ajuste
# modelo a area abaixo da curva do ajuste1 foi 0.945 uma área de
# cobertura muito boa, muito proximo de 1 que é a área completa 
# do gráfico.


perf
plot(perf) #Constroi o gr?fico da curva ROC
library(Epi)
ROC(form = saude ~ idade + as.factor(renda) ,plot = "ROC", MX=FALSE)
# Uma opção seria a observação 17
# falso poistivo sendo 0.1 e true positivo sendo 0.90, 
# aproximadamente o ponto mais próximo de um quadrado imaginário no canto 
# superior esquerdo do gráfico.


# Montando a matriz de contingencia
library(SDMTools)
confusion.matrix(saude, fit, threshold = 0.59533152)
#       obs
#  pred 0  1
#     0 9  3
#     1 1 17

# Mesmo olhando um bom ponto de corte observado na curva ROC
# sendo 0.59533152, com o intuíto de ter um TRUE POSITIVE DE 0.90
# FALSE POSITIVE de 0.10, porém eu obtenho um TP  de 0.85, e um
# FP de 0.11.

confusion.matrix(saude, fit, threshold = 0.5)
#       obs
#  pred 0  1
#     0 9  2
#     1 1 18

# Observando a matriz de confusão com um lambda = 0.5, ponto de
# corte coringa, básico para avaliação porém eu obtenho um TP
# de 0.90, e um FP de 0.10.

# Talvez seja porque o modelo não está bem ajustado talvez justifique 
# esse resultado, um pouco diferente do esperado.


