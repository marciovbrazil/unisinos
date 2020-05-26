# Trabalho: O IDH dos municipios brasileiros
# Fonte dos dados: http://atlasbrasil.org.br/2013/
# Autor: Marcio Luis de Oliveira Vieira

#leitura da base atlas completa para município
#install.packages('readxl') 
library(readxl)
atlas2013_dadosbrutos_pt_somente_municipio <- read_excel("~/Downloads/Unisinos/Unisinos/Estatistica/atlas2013_dadosbrutos_pt_somente municipio.xlsx")
atlas_munic <- atlas2013_dadosbrutos_pt_somente_municipio
View(atlas_munic)

#Pegando os dados de 2000 e 2010 para SC
atlas_m2000 <- subset.data.frame(atlas_munic, ANO==2000 & UF == 42)
atlas_m2010 <- subset.data.frame(atlas_munic, ANO==2010 & UF == 42)


#Análise de correlação linear entre duas variáveis quantitativas
plot(atlas_m2000$IDHM,atlas_m2000$RDPC) #Renda per Capita M�dia
cor(atlas_m2000$IDHM,atlas_m2000$RDPC) 

plot(atlas_m2000$IDHM,atlas_m2000$T_AGUA) #Percentual com �gua encanada
cor(atlas_m2000$IDHM,atlas_m2000$T_AGUA) 

plot(atlas_m2000$IDHM,atlas_m2000$T_LUZ) #Percentual com Luz el�trica
cor(atlas_m2000$IDHM,atlas_m2000$T_LUZ) 

plot(atlas_m2000$IDHM,atlas_m2000$PIND) #Propor��o extremamente pobre
cor(atlas_m2000$IDHM,atlas_m2000$PIND) 

plot(atlas_m2000$IDHM,atlas_m2000$PMPOB) #Propor��o de pobres
cor(atlas_m2000$IDHM,atlas_m2000$PMPOB) 


#Limpando o dataset para ficar somente com as variaveis importantes para a an�lise
atlas_m2000 <- atlas_m2000[,c('ANO','UF', 'Munic�pio','RDPC', 'T_AGUA', 'T_LUZ', 'PIND', 'PMPOB', 'IDHM')]
atlas_m2010 <- atlas_m2010[,c('ANO','UF', 'Munic�pio','RDPC', 'T_AGUA', 'T_LUZ', 'PIND', 'PMPOB', 'IDHM')]

class(atlas_m2000)

#inclusão do idh2010 na base do ano 2000
idhm_2010 <- c(atlas_m2010$IDHM)
class(idhm_2010)

atlas_m2000$Y_idhm_2010 <- c(atlas_m2010$IDHM)
str(atlas_m2000)

#Histograma de algumas variáveis
hist(atlas_m2000$RDPC)
hist(atlas_m2000$T_AGUA)
hist(atlas_m2000$T_LUZ)
hist(atlas_m2000$PIND)
hist(atlas_m2000$PMPOB)

summary(atlas_m2000$IDHM)
hist(atlas_m2000$IDHM)

cor(atlas_m2000[c('RDPC', 'T_AGUA', 'T_LUZ', 'PIND', 'PMPOB', 'IDHM')])
pairs(atlas_m2000[c('RDPC', 'T_AGUA', 'T_LUZ', 'PIND', 'PMPOB', 'IDHM')])
install.packages('psych')
library(psych)
pairs.panels(atlas_m2000[c('RDPC', 'T_AGUA', 'T_LUZ', 'PIND', 'PMPOB', 'IDHM')])

#Análise de regressão linear com duas variáveis explicativas quantitativas
reg2=lm(IDHM~ RDPC + T_AGUA + T_LUZ + PIND + PMPOB, data = atlas_m2000  )
summary(reg2)


#intervalos de confiança para os coeficientes da equação
confint(reg2)

#resíduos
plot(fitted(reg2),residuals(reg2),xlab="Valores Ajustados",ylab="Res�duos")
abline(h=0)
plot(atlas_m2000$RDPC,residuals(reg2),xlab="RDPC",ylab="Res�duos")
abline(h=0)
plot(atlas_m2000$T_AGUA,residuals(reg2),xlab="T_AGUA",ylab="Res�duos")
abline(h=0)
plot(atlas_m2000$T_LUZ,residuals(reg2),xlab="T_LUZ",ylab="Res�duos")
abline(h=0)
plot(atlas_m2000$PIND,residuals(reg2),xlab="PIND",ylab="Res�duos")
abline(h=0)
plot(atlas_m2000$PMPOB,residuals(reg2),xlab="PMPOB",ylab="Res�duos")
abline(h=0)

#avaliação da suposição de normalidade dos erros,
qqnorm(residuals(reg2), ylab="Res�duos",xlab="Quantis teóricos",main="")
qqline(residuals(reg2))

#predição da base completa
pred <- predict(reg2, atlas_m2010, interval="prediction", level=0.95)
atlas_m2010 <- data.frame(atlas_m2010,pred)

#Salvando dados com predição em arquivo excel:
#install.packages('xlsx') #Caso não tenha essa biblioteca instalada, descomente
library(xlsx)
write.xlsx(atlas_m2010, file="atlas_m2010_pred.xls") 
