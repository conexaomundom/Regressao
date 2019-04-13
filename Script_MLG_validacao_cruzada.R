
rm(list = ls())

# Dados
banco <- bodyfat
banco
head(banco)

attach(banco)
names(banco)


# Como a variável resposta é continua e não é contagem, podemos usar 
# as distribuições gaussiana, Gamma, inverse.gaussian
# nao podemos usar binomial, quasibinomial, nem opisson ou quasipoisson
# vamos fazer os modelos com todas as funções de ligação possível.


# Primeiramente com todas as variáveis e em seguinda ir fazendo a seleção 
# de variaveis.

# Rodar modelos MGL's e ver qual eh melhor por validacao cruzada.

# modelo gaussiano com função de ligação identidade.
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
            Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
            Wrist, family = gaussian(link = "identity"), data=banco)

# modelo gaussiano com função de ligação log.
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = gaussian(link = "log"), data=banco)


# modelo gaussiano com função de ligação inversa.
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = gaussian(link = "inverse"), data=banco)


##
# modelo Gamma com função de ligação inversa.
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = Gamma(link = "inverse"), data=banco)

# modelo Gamma com função de ligação identidade.
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = Gamma(link = "identity"), data=banco)

# modelo Gamma com função de ligação log.
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = Gamma(link = "log"), data=banco)

##
# modelo gaussiana  inversa com funcao de ligacao 1/mu^2
m_ig_mu <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = inverse.gaussian(link = "1/mu^2"), data=banco)

# modelo gaussiana  inversa com funcao de ligacao inverse. 
m_ig_inverse <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
              Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
              Wrist, family = inverse.gaussian(link = "inverse"), data=banco)


# modelo gaussiana  inversa com funcao de ligacao identity. 
m_ig_inverse <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
                      Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                      Wrist, family = inverse.gaussian(link = "identity"), data=banco)

# modelo gaussiana  inversa com funcao de ligacao log.
m_ig_inverse <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
                      Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
                      Wrist, family = inverse.gaussian(link = "log"), data=banco)

##
# modelo quasi com funcao de ligacao logit. 
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "logit"), data=banco)

# modelo quasi com funcao de ligacao probit. 
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "probit"), data=banco)

# modelo quasi com funcao de ligacao cloglog. 
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "cloglog"), data=banco)

# modelo quasi com funcao de ligacao identity. 
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "identity"), data=banco)

# modelo quasi com funcao de ligacao inverse. 
vm_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
              Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
              Wrist, family = quasi(link = "inverse"), data=banco)


# modelo quasi com funcao de ligacao log. 
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "log"), data=banco)

# modelo quasi com funcao de ligacao 1/mu^2. 
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "1/mu^2"), data=banco)


# modelo quasi com funcao de ligacao sqrt. 
m_g <- glm(formula = Percent ~ Age + Weigth + Heigth + Neck + Chest + 
             Abdomen + Hip + Thigh + Knee + Ankle + Biceps + Forearm + 
             Wrist, family = quasi(link = "sqrt"), data=banco)
