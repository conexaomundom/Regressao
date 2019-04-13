
rm(list = ls())

# Dados
banco <- bodyfat
banco
head(banco)

attach(banco)
names(banco)

# Rodar modelos MGL's e ver qual eh melhor por validacao cruzada.

