library(readr)
library(dplyr)
library(janitor)
library(ggplot2)

dados <- read_csv("student_lifestyle_dataset.csv") |> clean_names()

#Criando variáveis de bloco e tratamento
dados <- dados %>%
  mutate(Bloco = as.factor(stress_level),
         Tratamento = cut(social_hours_per_day, breaks = 3, labels = c("Pouco", "Moderado", "Muito")))
#pouco: <=2h, moderado: <= 4h, muitos>4h

#Homocedasticidade

# Boxplots
ggplot(dados, aes(x = Tratamento, y = gpa, fill = Tratamento)) +
  geom_boxplot() + theme_light()

#Teste de Barlett

tratamento1 <- filter(dados, Tratamento == "Pouco")$gpa
tratamento2 <- filter(dados, Tratamento == "Moderado")$gpa
tratamento3 <- filter(dados, Tratamento == "Muito")$gpa

dados_homoc <- list(tratamento1, tratamento2, tratamento3)

resultado <- bartlett.test(dados_homoc)
print(resultado)

#Anova
anova_model <- aov(gpa~ Tratamento + Bloco, data = dados)
anova_table <- anova(anova_model)
print(anova_table)

# Graus de liberdade
k <- length(unique(dados$Tratamento)) # Número de grupos
N <- nrow(dados) # Total de observações
df1 <- k - 1 # Graus de liberdade entre grupos
df2 <- N - k # Graus de liberdade dentro dos grupos

# Nível de significância
alpha <- 0.1

# Valor crítico da tabela F
f_critico <- qf(1 - alpha, df1, df2)

#Valor de F calculado
f_calculado <- anova_table["Tratamento", "F value"]

# Criar uma sequência de valores para a curva F
x <- seq(-2, 11, by = 0.01)
y <- df(x, df1, df2)

# Criar o gráfico
plot(x, y, type = "l", lwd = 2, col = "black",
     main = "Distribuição F com Ponto Crítico",
     xlab = "Valor F", ylab = "Densidade",
     xlim = c(0, 11), ylim = c(0, max(y) * 1.1))

# Criando um polígono para a área a ser destacada
idx <- which(x >= f_critico)
polygon(c(x[idx], rev(x[idx])), c(y[idx], rep(0, length(idx))), col = 'lightblue')

# Adicionar o ponto crítico
abline(v = f_critico, col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = paste("Ponto Crítico F =",round(f_critico, 2)),
       col = "blue", lty = 2, lwd = 2)

# Adicionar o F calculado
abline(v = f_calculado, col = "red", lwd = 2, lty = 2)
legend("topright", legend = paste("F calculado =", round(f_calculado, 2)),
       col = "red", lty = 2, lwd = 2)

#Normalidade
residuos <- anova_model$residuals

#Teste de Shapiro-Wilk
shapiro_wilk <- shapiro.test(residuos)
print(shapiro_wilk)

#QQ-Plot
ggplot(data.frame(residuos), aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQplot dos Resíduos") +
  xlab("Quantis Teóricos (Normal)") +
  ylab("Quantis da Amostra") + theme_light()

# Teste de Scheffé
library(DescTools)
scheffe <- ScheffeTest(anova_model)
print(scheffe)

# Plotando os resultados do teste de Scheffé
plot(scheffe)
