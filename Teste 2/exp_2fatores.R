

############# aula sobre experimento fatorial de 2 fatores
# autor: Gabriel Danilo Shimizu
# https://agronomiar.github.io/livroagro/esquema-fatorial-2-fatores.html



NN=c(339,332,163,230,300,
     163,172,123,083,161,
     171,069,095,046,079,
     335,235,217,174,222,
     284,136,225,098,110,
     082,038,092,053,046,
     196,252,346,468,258,
     032,038,063,048,160)
(Inoculacao=rep(c("IN","NI"),e=20))
(epoca=rep(c("Plantio","V1+15","V3+15","R1+15"),e=5,2))

###################################### dados
F1=as.factor(Inoculacao)
F2=as.factor(epoca)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=NN)
X="";Y="Número de nódulos"


###################################### descritiva
Media = with(dados, mean(resp))
Variancia = with(dados, var(resp))
Desvio = with(dados, sd(resp))
CV = Desvio / Media * 100

desc = cbind(Media, Variancia, Desvio, CV)
desc

###################################### descritiva por inoculacao
MediaA = with(dados, tapply(resp, F1, mean))
VarianciaA = with(dados, tapply(resp, F1, var))
DesvioA = with(dados, tapply(resp, F1, sd))
CVA = DesvioA / MediaA * 100
Desc = cbind(MediaA, VarianciaA, DesvioA, CVA)
Desc


###################################### por epoca de aplicacao
MediaB = with(dados, tapply(resp, F2, mean))
VarianciaB = with(dados, tapply(resp, F2, var))
DesvioB = with(dados, tapply(resp, F2, sd))
CVB = DesvioB / MediaB * 100
Desc = cbind(MediaB, VarianciaB, DesvioB, CVB)
Desc


#graficos analíticos  Fator 1 
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y, main="boxplot - Fator 1"))
mediab=with(dados,tapply(resp, F1, mean))
points(mediab, pch='+', cex=1.5, col='red')


#graficos analíticos  Fator 2
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y, main="boxplot - Fator 2"))
mediab=with(dados,tapply(resp, F2, mean))
points(mediab, pch='+', cex=1.5, col='red')


# fatores juntos
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1*F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y, main="Fatores cruzados"))

#avaliando interacao - Fator 1
with(dados, interaction.plot(F2, F1, resp, las=1, col=1:6, bty='l', 
                             xlab='', ylab='CBM', trace.label="FATOR1"))

#avaliando interacao - Fator 2
with(dados, interaction.plot(F1, F2, resp, las=1, col=1:6, bty='l', 
                             xlab='', ylab='CBM', trace.label="FATOR2"))

#Anova
mod = with(dados, aov(resp~F1*F2))
anova(mod)


#normalidadade dos erros
(norm=shapiro.test(mod$res))

#qq plot
library(hnp)
hnp::hnp(mod, las=1, xlab="Quantis teóricos", pch=16, main="qq-plot dos resíduos")

#heterogeneidade das variancias - Fator 1
with(dados, bartlett.test(mod$residuals~F1))

#heterogeneidade das variancias - Fator 2
with(dados, bartlett.test(mod$residuals~F2))

#juntandos os fatores
tratamentos=rep(c(paste("T",1:8)),e=5)
with(dados, bartlett.test(mod$residuals~tratamentos))

#independencia dos erros
(ind=lmtest::dwtest(mod))

#graficos dos residuos em sequencia
plot(mod$res, las=1, pch=19, col='red', ylab='Resíduos brutos')
abline(h=0)


#teste de comparações
library(ExpDes.pt)
with(dados,fat2.dic(F1,F2,resp, mcomp="tukey"))

