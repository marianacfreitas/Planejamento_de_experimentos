library(dplyr)

############ QUESTÃO 1

peso <- c(1.3, 1.26, 1.05, 1.52, 1.19, 1.21, 1.00, 1.56, 1.08, 1.19,
          1.05, 1.55)

antib <- rep(c("A0", "A0", "A1", "A1"), 3)
vitam <- rep(c("B0", "B1"), 6)

data <- data.frame(aumento = peso, antibiotico = antib, vitamina = vitam)

## Construindo a tabela Anova
modelo <-  with(data, aov(aumento ~ antibiotico*vitamina))
tabela_anova <- anova(modelo)
tabela_anova
#se o valor p for menos que o \alpha, os níveis do o fator ou interação tem um efeito estatisticamente significativo

## Valor da tabela F para um nível de significância de \alpha = 0.05

#f crítico para o antibiótico
gl_numerador <- tabela_anova$Df[1]  # Graus de liberdade do numerador (fator)
gl_denominador <- tabela_anova$Df[4]  # Graus de liberdade do denominador (resíduos)
qf(0.05, gl_numerador, gl_denominador, lower.tail = FALSE)
# como o F crítico para o antibiótico da tabela anova é maior que o f crítico, rejeitamos
#a hipótese de médias iguais entre diferentes níveis (H_0)

## Teste dos pressupostos

#Normalidadade dos erros

#h_0: resíduos tem distribuição normal
#h_1: resíduos não têm distribuição normal
#W: quanto mais próximo de 1, mais os resíduos se aproximam de uma distribuição normal
#se valor-p > \alpha, não rejeitmaos h_0
(norm=shapiro.test(modelo$residuals))


#QQ plot
library(hnp)
hnp::hnp(modelo, las=1, xlab="Quantis teóricos", pch=16, main="qq-plot dos resíduos")

#Heterogeneidade das variancias - Fator 1
#h_0: variâncias homogêneas
#h_1: variância diferente em pelo menos um grupo
#se p-valor < \alpha, rejeitamos a hipótese nula
with(data, bartlett.test(modelo$residuals~antibiotico))

#Heterogeneidade das variancias - Fator 2
with(data, bartlett.test(modelo$residuals~vitamina))

#Heterogeneidade das variâncias - juntandos os fatores
#considerar cada combinação dos dois níveis como um tratamento diferente:
data <- data |>
  mutate(
    tratamento = case_when(
      antibiotico == "A1" & vitamina == "B1" ~ "t11",
      antibiotico == "A1" & vitamina == "B0" ~ "t10",
      antibiotico == "A0" & vitamina == "B0" ~ "t00",
      antibiotico == "A0" & vitamina == "B1" ~ "t01"
    )
  )
with(data, bartlett.test(modelo$residuals~tratamento))

library(ggplot2)
ggplot(data, aes(x=tratamento, y=aumento, fill=tratamento)) +
  geom_boxplot()

#Independencia dos erros
#h_0: não há autocorrelação significativa entre os resíduos
#p-valor > \alpha indica que não rejeitamos h_0
(ind=lmtest::dwtest(modelo))

#Graficos dos residuos em sequencia
plot(modelo$res, las=1, pch=19, col='red', ylab='Resíduos brutos')
abline(h=0)

## Teste de comparações múltiplas

#Como todos os dois fatores e sua interação foram significativos, faremos o teste três vezes
library(ExpDes.pt)
with(data,fat2.dic(antibiotico,vitamina,aumento, mcomp="tukey"))
# Um valor-p baixo (geralmente < 0,05) indica que a diferença entre os níveis é estatisticamente significativa


############### QUESTÃO 2
library(car)  
library(rsm)

# Criar o conjunto de dados
dados <- data.frame(
  x1 = c(-1, -1, 1, 1, 0, 0, 0, 0, 0),
  x2 = c(-1, 1, -1, 1, 0, 0, 0, 0, 0),
  X1 = c(80,80, 90, 90, rep(85, 5)),
  X2 = c(rep(c(170, 180), 2), rep(175, 5)),
  Y = c(76.5, 77.0, 78.0, 79.5, 79.9, 80.3, 80.0, 79.7, 79.8)
)

dados <- as.coded.data(dados,
                      x1 ~ (X1-85)/5,
                      x2 ~ (X2 - 175)/5)

# Regressão considerando os efeitos principais e as interações entre os dois fatores
ciro <- rsm(Y ~ FO(x1, x2) + TWI(x1, x2), data = dados)
summary(ciro)

#Agora os cálculos sem interação
model <- rsm(Y ~ FO(x1, x2), data = dados)
summary(model)

#Comparando os dois modelos
anova(model, ciro)
#RSS(Residual Sum of Squares): Mede o erro do modelo - o modelo com interação apresenta um erro um pouco menor, mas quase igual
#Sum of Sq (Soma dos Quadrados Explicada pelo Novo Termo): Diferença de erro entre os modelos - apenas 0.25, ou seja, a interação não explicou muita coisa
#F: Mede a melhoria do ajuste ao adicionar a interação - 0.115, um valor muito pequeno
#Valor-p: Mede se a melhoria no ajuste é estatisticamente significativa - 0.7483, um valor muito maior que 0.05 → Não há evidência estatística para manter a interação
#Assim, modelo sem interação escolhido

#Modelo linear inicial
cat(sprintf("Y = %.4f + %.4f*x1 + %.4f*x2\n", model$coefficients[1], model$coefficients[2], model$coefficients[3]))

#Gráficos de contorno

#se o melhor modelo fosse com interação, apenas trocaríamos model por ciro nos modelos
# os gráficos de cotorno com interação apresentam curvas e/ou ondulações, inclinações

contour(model, ~x1+x2,
        image = TRUE,
        xlabs = c("X1", "X2")
        )
points(dados$X1, dados$X2)
#As cores indicam diferentes valores da variável resposta 𝑌
#Y Verde → valores menores
#Y Vermelho/laranja → valores maiores de 𝑌
#As linhas com valores numéricos representam regiões de mesmo valor da resposta 
#Quanto mais próximas essas linhas estão, maior a variação de Y em uma determinada região
#Linhas inclinadas sugerem que ambas as variáveis afetam a resposta

persp(model, ~x1+x2, contours = "colors",
      zlab = "Y",
      xlabs = c("X1", "X2"))


######### QUESTÃO 3
library(FrF2)

planej = FrF2(nfactors = 4,
              
              nruns = 2^4,
              
              factor.names =c("Temperatura" ,"Catalisador", "Reação", "Ph") ,
              
              replications = 1,
              
              randomize = FALSE)

rendimento <- c(54, 85, 49, 62, 64, 94, 56, 70, 52, 87, 49,
                64, 64, 94, 58, 73)
planej <- add.response(planej, rendimento)

modelo <- lm(rendimento ~ Temperatura*Catalisador*`Reação`*Ph, data =
              planej)

anov <- aov(modelo)
summary(anov)
#uma grande soma dos quadrados indica que explica uma grande parte da variabilidade na resposta - sugere que é um fator importante no processo

#retirando interações não significativas - mantendo apenas a interação de tempertaura e catalisador
modelo2 <- lm(rendimento ~ Temperatura*Catalisador + `Reação` +Ph, data =
               planej)
anov2 <- aov(modelo2)
summary(anov2)
#grande soma dos quadrados, baixo valor-p e a alta estatística F um fator importante
#quadrado médio dos resíduos (1,6) é relativamente baixo, o que indica que o modelo explica a maior parte da variabilidade nos dados
#ph tem valor p maior que 0.05 e explica pouca variabilidade do modelo, vamos retirá-lo

#retirando ph
modelo3 <- lm(rendimento ~ Temperatura*Catalisador + `Reação`, data =
                planej)
anov3 <- aov(modelo3)
summary(anov3)

anova(modelo, modelo2, modelo3) #indica que o modelo 2 tem menos RSS, modelo 1 é saturado
# o modelo 2 é  final

# Testando normalidade dos resíduos
shapiro.test(modelo2$residuals)

# Testando homovedasticidade
# Extrair os resíduos do modelo ANOVA
residuos <- residuals(modelo2)

# Realizar o teste de Bartlett
bartlett.test(residuos ~ Temperatura, data = planej)
bartlett.test(residuos ~ Catalisador, data = planej)
bartlett.test(residuos ~ `Reação`, data = planej)
bartlett.test(residuos ~ Ph, data = planej)

# como não temos repetições, cada observação é um nível diferente
tratamentos<-rep(c(paste("T",1:16)))
bartlett.test(residuos ~ tratamentos, data = planej)

# Testando independência dos resíduos
lmtest::dwtest(anov2)

#Teste de comparações
library(ExpDes.pt)
# Se você quiser fazer comparações múltiplas com Tukey
library(multcomp)
# Realizando o teste de Tukey
teste_tukey <- glht(modelo2, linfct = mcp(Temperatura = "Tukey", Catalisador = "Tukey", `Reação` = "Tukey", Ph = "Tukey"))

# Resumo do teste
summary(teste_tukey)
