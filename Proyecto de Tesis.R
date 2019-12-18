#Proyecto de Tesis


#Cargar base de datos

install.packages("foreign")
library(foreign)

install.packages("dplyr")
library(dplyr)


install.packages("mFilter")
library(mFilter)

install.packages("e1071")
library(e1071) 

install.packages("lmtest")
library(lmtest)

install.packages("orcutt")
library(orcutt)

install.packages("sandwich")
library(sandwich)

install.packages("tseries")
library(tseries)

install.packages("MASS")
library(MASS)

install.packages("extRemes")
library(extRemes)

install.packages("logspline")
library(logspline)

install.packages("axis")
library(axis)

install.packages("fitdistrplus")

library(fitdistrplus)

install.packages("ismev")
library(ismev)

install.packages("urca")
library(urca)

install.packages("DescTools")


#Obtener factor expansion para cada ENIGH y multiplicarla por el
#tamaño del hogar para luego obtener PIB Real per cápita
#Obteniendo poblacion total

#1984
H_84<-read.dbf("H_84.dbf")
n_84<-sum(H_84$FACTOR*enigh_84$TAM_HOG)

#1989
enigh_89<-read.dbf("CONCE89.dbf")
H_89<-read.dbf("H_89.dbf")
n_89<-sum(H_89$FACTOR*enigh_89$TAM_HOG)

#1992
enigh_92<-read.dbf("CONCE92.dbf")
H_92<-read.dbf("H_92.dbf")
n_92<-sum(H_92$FACTOR*enigh_92$TAM_HOG)

#1994
enigh_94<-read.dbf("CONCE94.dbf")
H_94<-read.dbf("H_94.dbf")
n_94<-sum(H_94$FACTOR*enigh_94$TAM_HOG)


#1996
enigh_96<-read.dbf("CONCE96.dbf")
H_96<-read.dbf("H_96.dbf")
n_96<-sum(H_96$FACTOR*enigh_96$TAM_HOG)

#1998
enigh_98<-read.dbf("CONCE98.dbf")
H_98<-read.dbf("H_98.dbf")
n_98<-sum(H_98$FACTOR*enigh_98$TAM_HOG)

#2000
enigh_00<-read.dbf("CONCE00.dbf")
H_00<-read.dbf("H_00.dbf")
n_00<-sum(H_00$FACTOR*enigh_00$TAM_HOG)

#2002
enigh_02<-read.dbf("CONCE02.dbf")
H_02<-read.dbf("H_02.dbf")
n_02<-sum(H_02$FACTOR*enigh_02$TAM_HOG)

#2004
enigh_04<-read.dbf("CONCE04.dbf")
H_04<-read.dbf("H_04.dbf")
n_04<-sum(H_04$FACTOR*enigh_04$TAM_HOG)

#2005
enigh_05<-read.dbf("CONCE05.dbf")
H_05<-read.dbf("H_05.dbf")
n_05<-as.numeric(sum(H_05$FACTOR*enigh_05$TAM_HOG))

#2006
enigh_06<-read.dbf("CONCE06.dbf")
H_06<-read.dbf("H_06.dbf")
n_06<-as.numeric(sum(H_06$FACTOR*enigh_06$TAM_HOG))

#2008
enigh_08<-read.dbf("CONCE08.dbf")
n_08<-as.numeric(sum(enigh_08$FACTOR*enigh_08$TAM_HOG))

#2010
enigh_10<-read.dbf("CONCE10.dbf")
n_10<-as.numeric(sum(enigh_10$factor*enigh_10$tam_hog))

#2012
enigh_12<-read.dbf("CONCE12.dbf")
n_12<-as.numeric(sum(enigh_12$FACTOR_HOG*enigh_12$TOT_INTEG))

#2014
enigh_14<-read.dbf("CONCE14.dbf")
n_14<-as.numeric(sum(enigh_14$FACTOR_HOG*enigh_14$TOT_INTEG))


#2016
n_16<-as.numeric(sum(enigh_16$factor*enigh_16$tot_integ))


#2018
enigh_18<-read.dbf("CONCE18.dbf")
n_18<-as.numeric(sum(enigh_18$factor*enigh_18$tot_integ))

#reuniendo datos de poblacion
n<-data.frame(n_84,n_89,n_92,n_94,n_96,n_98,n_00,n_02,n_04,n_05,n_06,
              n_08,n_10,n_12,n_14,n_16,n_18)
write.csv(n,"poblacion.csv")

#Examinar pib


#Leyendo datos
p<-read.csv("pib_real_sin_desest.csv")
p_c<-p$log.y.

#Desestacionalizando PIB real
library(forecast)
p_real <- ts(p_c, frequency=4, start = c(1984,1), end= c(2018,4))
decomp <- stl(p_real, s.window="periodic")
deseasonal_p <- seasadj(decomp)
plot(decomp)
deseasonal_p

write.csv(deseasonal_p,"pib_real_deseste.csv")





#Obteniendo ciclo
library(mFilter)

#Hodrick-Prescott
hp_mexico<-hpfilter(deseasonal_p,freq = 1600)
hp_cycle<-hp_mexico$cycle
hp_tend<-hp_mexico$trend

#Christiano-Fitzgerald
cf<-cffilter(deseasonal_p,type="asymmetric",pl=6,pu=32,
               drift=FALSE,root=TRUE,nfix=1,theta=1)

cf_cycle<-cf$cycle
cf_tend<-cf$trend


#Baxter-King 

bk<-bkfilter(deseasonal_p,pl=6,pu=32,type="fixed",drift=FALSE)
bk_cycle<-bk$cycle
bk_tend<-bk$trend
#Regresion trigonometrica

tr<-trfilter(deseasonal_p,pl=6,pu=32,drift=TRUE)

tr_cycle<-tr$cycle
tr_trend<-tr$trend

cycles<-data.frame(hp_cycle,cf_cycle,bk_cycle,tr_cycle)
trends<-data.frame(hp_tend,cf_tend,bk_tend,tr_trend)
#Exportar datos para graficar ciclo
write.csv(cycles,"ciclo_metodologias_log.csv")
write.csv(trends,"tendencias_metodologias.csv")

par(mfrow=c(1,2))
axis(1, seq(-1,1))
hist(ciclo, xlab= "Ciclo Económico Mexicano", ylab= "Frecuencia",
     main= "Histograma de Frecuencias Absolutas" )
plot(density(ciclo), main="Función de Densidad Estimada")

#Analisis de frecuencia
freq<-read.csv("frecuencias.csv")
na.omit(freq)
str(freq)

library(ggplot2)

freq$DIF<-as.numeric(freq$DIF)
freq$BK<-as.numeric(freq$BK)


#Frecuencia para hp
y_range<-range(freq$HP)
x_range<-range(freq$Ciclos.Por.Periodo)

plot(freq$Ciclos.Por.Periodo,freq$HP,xlim=x_range,ylim=y_range,
     col="blue",xlab="Ciclos Por Frecuencia",ylab="Ganancia",
     main="Filtro Hodrick-Prescott (lambda=1600)",type = "line")

#frecuencia para cf
y_range<-range(freq$CF)
plot(freq$Ciclos.Por.Periodo,freq$CF,xlim=x_range,ylim=y_range,
     col="red",xlab="Ciclos por Frecuencia",ylab="Ganancia",
     main="Filtro Christiano-Fitzgerald (6,32)",type="line")

#Frecuencia para bk
bk<-read.csv("bk_f.csv")


y_range<-range(bk$BK)
x_range<-range(bk$Ciclos)


plot(bk$Ciclos,bk$BK, xlim=x_range, ylim=y_range ,
     col="black",xlab="Ciclos por Frecuencia",ylab="Ganancia",
     main="Filtro Baxter-King (6,32)",type="line")

#Summary frecuencias

gain<-read.csv("gains.csv")

hp<-gain[c(1:23),1]
summary(hp)

cf<-gain[c(1:24),2]
summary(cf)

bk<-gain[c(1:12),3]
summary(bk)


rt<-gain[,4]
summary(rt)


dif<-gain[c(1:16),5]
summary(dif)

boxplot.default(gain)

#Verificando caracteristicas del ciclo economico

bk<-read.csv("bk.csv")

#Verificando estacionariedad
#Checar rezagos optimos



install.packages("MARX")
install.packages("aTSA")
library(MARX)
install.packages("urca")
library(urca)
library(aTSA)

#Seleccionando lags optimos
selection.lag(bk$BK, x= NULL, 10)

hqc<-ur.df(bk$BK,type="none",lags=10,selectlags = "Fixed")
summary(hqc)

hist(bk$BK, xlab="Ciclo Económico Real",ylab="Frecuencia Absoluta",
     main="Histograma de Frecuencias")



#Estimando paramatros para distribucion Cauchy
library(EnvStats)
library(fitdistrplus)

fit_c <- fitdist(bk$BK, "cauchy")
fit_d<-fitdist(bk$BK,"norm")



graphics.off()
par(mar=c(0.5,0.5,0.5,0.5))
par(mfrow=c(2,2))
plot.legend <- c("Cauchy", "Normal")
denscomp(list(fit_c, fit_d), legendtext = plot.legend,ylab="Densidad",
         xlab="Ciclo Económico Real",main = "Histograma y Densidades Teóricas")
cdfcomp (list(fit_c,  fit_d), legendtext = plot.legend, ylab="Densidad Acumulada",
         xlab="Ciclo Económico Real", main="Densidad Acumulada Empirica y Teórica")
qqcomp  (list(fit_c,  fit_d), legendtext = plot.legend, xlab="Cuantiles Teóricos",
         ylab="Cuantiles Empirícos", main="Gráfico de Cuantiles")
ppcomp  (list(fit_c,  fit_d), legendtext = plot.legend,xlab="Probabilidades Teóricas",
         ylab="Probabilidades Empirícas", main="Gráfico de Probabilidades")


location<-0.001075558
scale<-0.004430314
n<-1000

c_simulation<-rcauchy(n, location, scale)
c_simulation


ks<-ks.test(bk$BK,c_simulation, alternative ="two.sided", exact = NULL, 
        tol=1e-8, simulate.p.value=TRUE, B=2000)



#Profundidad

library(e1071)
profundidad_bk<-skewness(bk$BK)
profundidad_bk

profundidad_sim<-skewness(c_simulation)
profundidad_sim

#Inclinacion
dif_ciclo_Mexico<-diff(bk$BK)
inclinacion_Mexico<-skewness(dif_ciclo_Mexico)
inclinacion_Mexico

dif_sim<-diff(c_simulation)
inclinacion_sim<-skewness(dif_sim)
inclinacion_sim



#Ciclos de Desigualdad

data<-read.csv("ciclos_indicadores.csv")
dif<-diff(data[2:17,2:4])   #diferencias



  
library(dplyr)
library(tidyr)



#Definiendo variable ingreso por hogar
#Se procede a tomar como variable proxy del ingreso la suma del 
#gasto monetario mas el gasto no monetario que estan registrados
#de forma trimestral en las encuestas del ENIGH. Por lo tanto
#de aqui en adelante se tomara como definición de la variable ingreso
#la suma de los componentes mencionados anteriormente.



library(foreign)


#Subiendo INPC comparable
inpc<-read.csv("INPC_ENIGH.csv")

#Definiendo INPC por año
inpc<-inpc$INPC.Indice.junio.2013.1.

inpc_1984<-inpc[1]
inpc_1989<-inpc[2]
inpc_1992<-inpc[3]
inpc_1994<-inpc[4]
inpc_1996<-inpc[5]
inpc_1998<-inpc[6]
inpc_2000<-inpc[7]
inpc_2002<-inpc[8]
inpc_2004<-inpc[9]
inpc_2005<-inpc[10]
inpc_2006<-inpc[11]
inpc_2008<-inpc[12]
inpc_2010<-inpc[13]
inpc_2012<-inpc[14]
inpc_2014<-inpc[15]
inpc_2016<-inpc[16]
inpc_2018<-inpc[17]

#Obtendremos el ingreso real per capita dividiendo el ingreso real entre
#el número de integrantes del hogar. 

#Enigh 84
enigh_84<-read.dbf("CONCE84.dbf")
enigh_84<-na.omit(enigh_84)
##Quitando filas con ceros
row_sub = apply(enigh_84, 1, function(row) all(row !=0 ))
##Submuestra resultante
enigh_84<-enigh_84[row_sub,]
m_84<-(enigh_84[,7]+enigh_84[,10])/(enigh_84$TAM_HOG)

#Ingreso real 1984
m_real_84<-m_84/inpc_1984
summary(m_real_84)
base_real_84<-data.frame(m_real_84)

#Indicadores de Desigualdad
library(DescTools)

#Atkinson
#Atkinson (0.5)
#Atkinson (0.05)
#Atkinson (0.8)
#Gini

atkinson_84_1<-Atkinson(base_real_84$m_real_84, n = rep(1, length(base_real_84$m_real_84)), parameter = 0.05)
atkinson_84_2<-Atkinson(base_real_84$m_real_84, n = rep(1, length(base_real_84$m_real_84)), parameter = 0.5)
atkinson_84_3<-Atkinson(base_real_84$m_real_84, n = rep(1, length(base_real_84$m_real_84)), parameter = 0.8)
gini_84<-Gini(base_real_84$m_real_84, n = rep(1, length(base_real_84$m_real_84),unbiased=TRUE))

atkinson_84_1
atkinson_84_2
atkinson_84_3
gini_84
#Diviendo en deciles

library(dplyr)

base_real_84 %>% mutate(quintile = ntile(m_real_84, 10)) -> m_84_diez
head(m_84_diez)

# create a list of data frames for each rating value
por_decil_84<- split(m_84_diez, m_84_diez$quintile)


#Promedios de ingreso real
i_84<-as.data.frame(lapply(por_decil_84, sapply, mean))

print(p_84[1:10][10])
#Gini para 1984
library(DescTools)
g_84<-as.data.frame(lapply(por_decil_84, sapply , Gini))

#Atkinson para 1984 (0.5)
library(DescTools)
at_84<-as.data.frame(lapply(por_decil_84,sapply,Atkinson))

library(base)
a_84<-data.frame(i_84,g_84,at_84)
d_84<-data.frame(t(a_84))





#Enigh 89
enigh_89<-read.dbf("CONCE89.dbf")
enigh_89<-na.omit(enigh_89)
##Quitando filas con ceros
row_sub = apply(enigh_89, 1, function(row) all(row !=0 ))
##Submuestra resultante
enigh_89<-enigh_89[row_sub,]
m_89<-(enigh_89[,6]+enigh_89[,9])/(enigh_89$TAM_HOG)


#Ingreso real 1989
m_real_89<-m_89/inpc_1989
summary(m_real_89)

base_real_89<-data.frame(m_real_89)


atkinson_89_1<-Atkinson(base_real_89$m_real_89, n = rep(1, length(base_real_89$m_real_89)), parameter = 0.05)
atkinson_89_2<-Atkinson(base_real_89$m_real_89, n = rep(1, length(base_real_89$m_real_89)), parameter = 0.5)
atkinson_89_3<-Atkinson(base_real_89$m_real_89, n = rep(1, length(base_real_89$m_real_89)), parameter = 0.8)
gini_89<-Gini(base_real_89$m_real_89, n = rep(1, length(base_real_89$m_real_89),unbiased=TRUE))

atkinson_89_1
atkinson_89_2
atkinson_89_3
gini_89





#Diviendo en deciles

library(dplyr)

base_real_89 %>% mutate(quintile = ntile(m_real_89, 10)) -> m_89_diez
head(m_89_diez)

# create a list of data frames for each quintile value
por_decil_89<- split(m_89_diez, m_89_diez$quintile)


#Promedios de ingreso real 1989
i_89<-as.data.frame(lapply(por_decil_89, sapply, mean))

#Gini para 1989
g_89<-as.data.frame(lapply(por_decil_89, sapply , Gini))

#Atkinson para 1989
library(DescTools)
at_89<-as.data.frame(lapply(por_decil_89,sapply,Atkinson))

library(base)
a_89<-data.frame(i_89,g_89,at_89)
d_89<-data.frame(t(a_89))





enigh_92<-read.dbf("CONCE92.dbf")
enigh_92<-na.omit(enigh_92)
##Quitando filas con ceros
row_sub = apply(enigh_92, 1, function(row) all(row !=0 ))
##Submuestra resultante
enigh_92<-enigh_92[row_sub,]
m_92<-(enigh_92[,7]+enigh_92[,10])/(enigh_92$TAM_HOG)

#Ingreso real 1992
m_real_92<-m_92/inpc_1992
summary(m_real_92)

base_real_92<-data.frame(m_real_92)


atkinson_92_1<-Atkinson(base_real_92$m_real_92, n = rep(1, length(base_real_92$m_real_92)), parameter = 0.05)
atkinson_92_2<-Atkinson(base_real_92$m_real_92, n = rep(1, length(base_real_92$m_real_92)), parameter = 0.5)
atkinson_92_3<-Atkinson(base_real_92$m_real_92, n = rep(1, length(base_real_92$m_real_92)), parameter = 0.8)
gini_92<-Gini(base_real_92$m_real_92, n = rep(1, length(base_real_92$m_real_92),unbiased=TRUE))

atkinson_92_1
atkinson_92_2
atkinson_92_3
gini_92


#Diviendo en deciles

library(dplyr)

base_real_92 %>% mutate(quintile = ntile(m_real_92, 10)) -> m_92_diez
head(m_92_diez)

# create a list of data frames for each quintile value
por_decil_92<- split(m_92_diez, m_92_diez$quintile)


#Promedios de ingreso real 1992
i_92<-as.data.frame(lapply(por_decil_92, sapply, mean))

#Gini para 1992
g_92<-as.data.frame(lapply(por_decil_92, sapply , Gini))

#Atkinson para 1992
at_92<-as.data.frame(lapply(por_decil_92,sapply,Atkinson))

a_92<-data.frame(i_92,g_92,at_92)
d_92<-data.frame(t(a_92))



enigh_94<-read.dbf("CONCE94.dbf")
enigh_94<-na.omit(enigh_94)
##Quitando filas con ceros
row_sub = apply(enigh_94, 1, function(row) all(row !=0 ))
##Submuestra resultante
enigh_94<-enigh_94[row_sub,]
m_94<-(enigh_94[,7]+enigh_94[,10])/(enigh_94$TAM_HOG)

#Ingreso real 1994
m_real_94<-m_94/inpc_1994
summary(m_real_94)
base_real_94<-data.frame(m_real_94)


atkinson_94_1<-Atkinson(base_real_94$m_real_94, n = rep(1, length(base_real_94$m_real_94)), parameter = 0.05)
atkinson_94_2<-Atkinson(base_real_94$m_real_94, n = rep(1, length(base_real_94$m_real_94)), parameter = 0.5)
atkinson_94_3<-Atkinson(base_real_94$m_real_94, n = rep(1, length(base_real_94$m_real_94)), parameter = 0.8)
gini_94<-Gini(base_real_94$m_real_94, n = rep(1, length(base_real_94$m_real_94),unbiased=TRUE))

atkinson_94_1
atkinson_94_2
atkinson_94_3
gini_94


#Diviendo en deciles

library(dplyr)

base_real_94 %>% mutate(quintile = ntile(m_real_94, 10)) -> m_94_diez
head(m_94_diez)

# create a list of data frames for each quintile value
por_decil_94<- split(m_94_diez, m_94_diez$quintile)


#Promedios de ingreso real 1992
i_94<-as.data.frame(lapply(por_decil_94, sapply, mean))

#Gini para 1992
g_94<-as.data.frame(lapply(por_decil_94, sapply , Gini))

#Atkinson
at_94<-as.data.frame(lapply(por_decil_94,sapply,Atkinson))


a_94<-data.frame(i_94,g_94,at_94)
d_94<-data.frame(t(a_94))



enigh_96<-read.dbf("CONCE96.dbf")
enigh_96<-na.omit(enigh_96)
##Quitando filas con ceros
row_sub = apply(enigh_96, 1, function(row) all(row !=0 ))
##Submuestra resultante
enigh_96<-enigh_96[row_sub,]
m_96<-(enigh_96[,7]+enigh_96[,10])/(enigh_96$TAM_HOG)

#Ingreso real 1996
m_real_96<-m_96/inpc_1996
summary(m_real_96)

base_real_96<-data.frame(m_real_96)



atkinson_96_1<-Atkinson(base_real_96$m_real_96, n = rep(1, length(base_real_96$m_real_96)), parameter = 0.05)
atkinson_96_2<-Atkinson(base_real_96$m_real_96, n = rep(1, length(base_real_96$m_real_96)), parameter = 0.5)
atkinson_96_3<-Atkinson(base_real_96$m_real_96, n = rep(1, length(base_real_96$m_real_96)), parameter = 0.8)
gini_96<-Gini(base_real_96$m_real_96, n = rep(1, length(base_real_96$m_real_96),unbiased=TRUE))

atkinson_96_1
atkinson_96_2
atkinson_96_3
gini_96




#Diviendo en deciles

library(dplyr)

base_real_96 %>% mutate(quintile = ntile(m_real_96, 10)) -> m_96_diez
head(m_96_diez)

# create a list of data frames for each quintile value
por_decil_96<- split(m_96_diez, m_96_diez$quintile)


#Promedios de ingreso real 1996
i_96<-as.data.frame(lapply(por_decil_96, sapply, mean))

#Gini para 1996
g_96<-as.data.frame(lapply(por_decil_96, sapply , Gini))

#Atkinson para 1996
at_96<-as.data.frame(lapply(por_decil_96,sapply,Atkinson))

a_96<-data.frame(i_96,g_96,at_96)
d_96<-data.frame(t(a_96))




enigh_98<-read.dbf("CONCE98.dbf")
enigh_98<-na.omit(enigh_98)
##Quitando filas con ceros
row_sub = apply(enigh_98, 1, function(row) all(row !=0 ))
##Submuestra resultante
enigh_98<-enigh_98[row_sub,]

m_98<-(enigh_98[,7]+enigh_98[,10])/(enigh_98$TAM_HOG)

#Ingreso real 1998
m_real_98<-m_98/inpc_1998
summary(m_real_98)

base_real_98<-data.frame(m_real_98)


atkinson_98_1<-Atkinson(base_real_98$m_real_98, n = rep(1, length(base_real_98$m_real_98)), parameter = 0.05)
atkinson_98_2<-Atkinson(base_real_98$m_real_98, n = rep(1, length(base_real_98$m_real_98)), parameter = 0.5)
atkinson_98_3<-Atkinson(base_real_98$m_real_98, n = rep(1, length(base_real_98$m_real_98)), parameter = 0.8)
gini_98<-Gini(base_real_98$m_real_98, n = rep(1, length(base_real_98$m_real_98),unbiased=TRUE))

atkinson_98_1
atkinson_98_2
atkinson_98_3
gini_98






#Diviendo en deciles

library(dplyr)

base_real_98 %>% mutate(quintile = ntile(m_real_98, 10)) -> m_98_diez
head(m_98_diez)

# create a list of data frames for each quintile value
por_decil_98<- split(m_98_diez, m_98_diez$quintile)


#Promedios de ingreso real 1998
i_98<-as.data.frame(lapply(por_decil_98, sapply, mean))

#Gini para 1998
g_98<-as.data.frame(lapply(por_decil_98, sapply , Gini))

#Atkinson para 1998
at_98<-as.data.frame(lapply(por_decil_98,sapply,Atkinson))

a_98<-data.frame(i_98,g_98,at_98)
d_98<-data.frame(t(a_98))




enigh_00<-read.dbf("CONCE00.dbf")
enigh_00<-na.omit(enigh_00)
m_00<-(enigh_00$GASMON+enigh_00$GASNOM)/(enigh_00$TAM_HOG)
summary(m_00)

#Ingreso real 2000

m_real_00<-m_00/inpc_2000
summary(m_real_00)


base_real_00<-data.frame(m_real_00)


atkinson_00_1<-Atkinson(base_real_00$m_real_00, n = rep(1, length(base_real_00$m_real_00)), parameter = 0.05)
atkinson_00_2<-Atkinson(base_real_00$m_real_00, n = rep(1, length(base_real_00$m_real_00)), parameter = 0.5)
atkinson_00_3<-Atkinson(base_real_00$m_real_00, n = rep(1, length(base_real_00$m_real_00)), parameter = 0.8)
gini_00<-Gini(base_real_00$m_real_00, n = rep(1, length(base_real_00$m_real_00),unbiased=TRUE))

atkinson_00_1
atkinson_00_2
atkinson_00_3
gini_00


#Diviendo en deciles

library(dplyr)

base_real_00 %>% mutate(quintile = ntile(m_real_00, 10)) -> m_00_diez
head(m_00_diez)

# create a list of data frames for each quintile value
por_decil_00<- split(m_00_diez, m_00_diez$quintile)


#Promedios de ingreso real 2000
i_00<-as.data.frame(lapply(por_decil_00, sapply, mean))

#Gini para 2000
g_00<-as.data.frame(lapply(por_decil_00, sapply , Gini))

#Atkinson para 2000
at_00<-as.data.frame(lapply(por_decil_00,sapply,Atkinson))

a_00<-data.frame(i_00,g_00,at_00)
d_00<-data.frame(t(a_00))




enigh_02<-read.dbf("CONCE02.dbf")
enigh_02<-na.omit(enigh_02)

m_02<-(enigh_02$GASMON+enigh_02$GASNOM)/(enigh_02$TAM_HOG)
m_real_02<-m_02/inpc_2002
summary(m_real_02)

base_real_02<-data.frame(m_real_02)


atkinson_02_1<-Atkinson(base_real_02$m_real_02, n = rep(1, length(base_real_02$m_real_02)), parameter = 0.05)
atkinson_02_2<-Atkinson(base_real_02$m_real_02, n = rep(1, length(base_real_02$m_real_02)), parameter = 0.5)
atkinson_02_3<-Atkinson(base_real_02$m_real_02, n = rep(1, length(base_real_02$m_real_02)), parameter = 0.8)
gini_02<-Gini(base_real_02$m_real_02, n = rep(1, length(base_real_02$m_real_02),unbiased=TRUE))

atkinson_02_1
atkinson_02_2
atkinson_02_3
gini_02






#Diviendo en deciles

library(dplyr)

base_real_02 %>% mutate(quintile = ntile(m_real_02, 10)) -> m_02_diez
head(m_02_diez)

# create a list of data frames for each quintile value
por_decil_02<- split(m_02_diez, m_02_diez$quintile)


#Promedios de ingreso real 2002
i_92<-as.data.frame(lapply(por_decil_02, sapply, mean))

#Gini para 2002
g_02<-as.data.frame(lapply(por_decil_02, sapply , Gini))

#Atkinson para 2002
at_02<-as.data.frame(lapply(por_decil_02,sapply,Atkinson))


a_02<-data.frame(i_92,g_02,at_02)
d_02<-data.frame(t(a_02))




enigh_04<-read.dbf("CONCE04.dbf")
enigh_04<-na.omit(enigh_04)
m_04<-(enigh_04$GASMON+enigh_04$GASNOM)/(enigh_04$TAM_HOG)
m_real_04<-m_04/inpc_2004
summary(m_real_04)
base_real_04<-data.frame(m_real_04)



atkinson_04_1<-Atkinson(base_real_04$m_real_04, n = rep(1, length(base_real_04$m_real_04)), parameter = 0.05)
atkinson_04_2<-Atkinson(base_real_04$m_real_04, n = rep(1, length(base_real_04$m_real_04)), parameter = 0.5)
atkinson_04_3<-Atkinson(base_real_04$m_real_04, n = rep(1, length(base_real_04$m_real_04)), parameter = 0.8)
gini_04<-Gini(base_real_04$m_real_04, n = rep(1, length(base_real_04$m_real_04),unbiased=TRUE))

atkinson_04_1
atkinson_04_2
atkinson_04_3
gini_04

#Diviendo en deciles

library(dplyr)

base_real_04 %>% mutate(quintile = ntile(m_real_04, 10)) -> m_04_diez
head(m_04_diez)

# create a list of data frames for each quintile value
por_decil_04<- split(m_04_diez, m_04_diez$quintile)


#Promedios de ingreso real 2004
i_04<-as.data.frame(lapply(por_decil_04, sapply, mean))

#Gini para 2004
g_04<-as.data.frame(lapply(por_decil_04, sapply , Gini))

#Atkinson para 2004
at_04<-as.data.frame(lapply(por_decil_04,sapply,Atkinson))

a_04<-data.frame(i_04,g_04,at_04)
d_04<-data.frame(t(a_04))






enigh_05<-read.dbf("CONCE05.dbf")
enigh_05<-na.omit(enigh_05)
m_05<-(enigh_05$GASMON+enigh_05$GASNOM)/(enigh_05$TAM_HOG)
m_real_05<-m_05/inpc_2005
summary(m_real_05)
base_real_05<-data.frame(m_real_05)


atkinson_05_1<-Atkinson(base_real_05$m_real_05, n = rep(1, length(base_real_05$m_real_05)), parameter = 0.05)
atkinson_05_2<-Atkinson(base_real_05$m_real_05, n = rep(1, length(base_real_05$m_real_05)), parameter = 0.5)
atkinson_05_3<-Atkinson(base_real_05$m_real_05, n = rep(1, length(base_real_05$m_real_05)), parameter = 0.8)
gini_05<-Gini(base_real_05$m_real_05, n = rep(1, length(base_real_05$m_real_05),unbiased=TRUE))

atkinson_05_1
atkinson_05_2
atkinson_05_3
gini_05




#Diviendo en deciles

library(dplyr)

base_real_05 %>% mutate(quintile = ntile(m_real_05, 10)) -> m_05_diez
head(m_05_diez)

# create a list of data frames for each quintile value
por_decil_05<- split(m_05_diez, m_05_diez$quintile)


#Promedios de ingreso real 2005
i_05<-as.data.frame(lapply(por_decil_05, sapply, mean))

#Gini para 2005
g_05<-as.data.frame(lapply(por_decil_05, sapply , Gini))

#Atkinson para 2005
at_05<-as.data.frame(lapply(por_decil_05,sapply,Atkinson))


a_05<-data.frame(i_05,g_05,at_05)

d_05<-data.frame(t(a_05))




enigh_06<-read.dbf("CONCE06.dbf")
enigh_06<-na.omit(enigh_06)
m_06<-(enigh_06$GASMON+enigh_06$GASNOM)/(enigh_06$TAM_HOG)
m_real_06<-m_06/inpc_2006
summary(m_real_06)

base_real_06<-data.frame(m_real_06)



atkinson_06_1<-Atkinson(base_real_06$m_real_06, n = rep(1, length(base_real_06$m_real_06)), parameter = 0.05)
atkinson_06_2<-Atkinson(base_real_06$m_real_06, n = rep(1, length(base_real_06$m_real_06)), parameter = 0.5)
atkinson_06_3<-Atkinson(base_real_06$m_real_06, n = rep(1, length(base_real_06$m_real_06)), parameter = 0.8)
gini_06<-Gini(base_real_06$m_real_06, n = rep(1, length(base_real_06$m_real_06),unbiased=TRUE))

atkinson_06_1
atkinson_06_2
atkinson_06_3
gini_06




#Diviendo en deciles

library(dplyr)

base_real_06 %>% mutate(quintile = ntile(m_real_06, 10)) -> m_06_diez
head(m_06_diez)

# create a list of data frames for each quintile value
por_decil_06<- split(m_06_diez, m_06_diez$quintile)


#Promedios de ingreso real 2006
i_06<-as.data.frame(lapply(por_decil_06, sapply, mean))

#Gini para 2006
g_06<-as.data.frame(lapply(por_decil_06, sapply , Gini))

#Atkinson para 2006
at_06<-as.data.frame(lapply(por_decil_06,sapply,Atkinson))


a_06<-data.frame(i_06,g_06,at_06)

d_06<-data.frame(t(a_06))




enigh_08<-read.dbf("CONCE08.dbf")
enigh_08<-na.omit(enigh_08)
m_08<-(enigh_08$GASMON+enigh_08$GASNOM)/(enigh_08$TAM_HOG)
m_real_08<-m_08/inpc_2008
summary(m_real_08)

base_real_08<-data.frame(m_real_08)


atkinson_08_1<-Atkinson(base_real_08$m_real_08, n = rep(1, length(base_real_08$m_real_08)), parameter = 0.05)
atkinson_08_2<-Atkinson(base_real_08$m_real_08, n = rep(1, length(base_real_08$m_real_08)), parameter = 0.5)
atkinson_08_3<-Atkinson(base_real_08$m_real_08, n = rep(1, length(base_real_08$m_real_08)), parameter = 0.8)
gini_08<-Gini(base_real_08$m_real_08, n = rep(1, length(base_real_08$m_real_08),unbiased=TRUE))

atkinson_08_1
atkinson_08_2
atkinson_08_3
gini_08


#Diviendo en deciles

library(dplyr)

base_real_08 %>% mutate(quintile = ntile(m_real_08, 10)) -> m_08_diez
head(m_08_diez)

# create a list of data frames for each quintile value
por_decil_08<- split(m_08_diez, m_08_diez$quintile)


#Promedios de ingreso real 2008
i_08<-as.data.frame(lapply(por_decil_08, sapply, mean))

#Gini para 2008
g_08<-as.data.frame(lapply(por_decil_08, sapply , Gini))

#Atkinson para 2008
at_08<-as.data.frame(lapply(por_decil_08,sapply,Atkinson))

a_08<-data.frame(i_08,g_08,at_08)

d_08<-data.frame(t(a_08))





enigh_10<-read.dbf("CONCE10.dbf")
enigh_10<-na.omit(enigh_10)
m_10<-(enigh_10$gasmon+enigh_10$gasnom)/(enigh_10$tam_hog)
m_real_10<-m_10/inpc_2010
summary(m_real_10)


base_real_10<-data.frame(m_real_10)


atkinson_10_1<-Atkinson(base_real_10$m_real_10, n = rep(1, length(base_real_10$m_real_10)), parameter = 0.05)
atkinson_10_2<-Atkinson(base_real_10$m_real_10, n = rep(1, length(base_real_10$m_real_10)), parameter = 0.5)
atkinson_10_3<-Atkinson(base_real_10$m_real_10, n = rep(1, length(base_real_10$m_real_10)), parameter = 0.8)
gini_10<-Gini(base_real_10$m_real_10, n = rep(1, length(base_real_10$m_real_10),unbiased=TRUE))

atkinson_10_1
atkinson_10_2
atkinson_10_3
gini_10


#Diviendo en deciles

library(dplyr)

base_real_10 %>% mutate(quintile = ntile(m_real_10, 10)) -> m_10_diez
head(m_10_diez)

# create a list of data frames for each quintile value
por_decil_10<- split(m_10_diez, m_10_diez$quintile)


#Promedios de ingreso real 2010
i_10<-as.data.frame(lapply(por_decil_10, sapply, mean))

#Gini para 2010
g_10<-as.data.frame(lapply(por_decil_10, sapply , Gini))


#Atkinson para 2010
at_10<-as.data.frame(lapply(por_decil_00,sapply,Atkinson))


a_10<-data.frame(i_10,g_10,at_10)

d_10<-data.frame(t(a_10))










enigh_12<-read.dbf("CONCE12.dbf")
enigh_12<-na.omit(enigh_12)
m_12<-(enigh_12$GASTO_MON+enigh_12$GASTO_NOM)/(enigh_12$TOT_INTEG)
m_real_12<-m_12/inpc_2012
summary(m_real_12)
base_real_12<-data.frame(m_real_12)

atkinson_12_1<-Atkinson(base_real_12$m_real_12, n = rep(1, length(base_real_12$m_real_12)), parameter = 0.05)
atkinson_12_2<-Atkinson(base_real_12$m_real_12, n = rep(1, length(base_real_12$m_real_12)), parameter = 0.5)
atkinson_12_3<-Atkinson(base_real_12$m_real_12, n = rep(1, length(base_real_12$m_real_12)), parameter = 0.8)
gini_12<-Gini(base_real_12$m_real_12, n = rep(1, length(base_real_12$m_real_12),unbiased=TRUE))

atkinson_12_1
atkinson_12_2
atkinson_12_3
gini_12





#Diviendo en deciles

library(dplyr)

base_real_12 %>% mutate(quintile = ntile(m_real_12, 10)) -> m_12_diez
head(m_12_diez)

# create a list of data frames for each quintile value
por_decil_12<- split(m_12_diez, m_12_diez$quintile)


#Promedios de ingreso real 2012
i_12<-as.data.frame(lapply(por_decil_12, sapply, mean))

#Gini para 2012
g_12<-as.data.frame(lapply(por_decil_12, sapply , Gini))

#Atkinson para 2012
at_12<-as.data.frame(lapply(por_decil_12,sapply,Atkinson))


a_12<-data.frame(i_12,g_12,at_12)

d_12<-data.frame(t(a_12))



enigh_14<-read.dbf("CONCE14.dbf")
enigh_14<-na.omit(enigh_14)
m_14<-(enigh_14$GASTO_MON+enigh_14$GASTO_NOM)/(enigh_14$TOT_INTEG)
m_real_14<-m_14/inpc_2014
summary(m_real_14)

base_real_14<-data.frame(m_real_14)

atkinson_14_1<-Atkinson(base_real_14$m_real_14, n = rep(1, length(base_real_14$m_real_14)), parameter = 0.05)
atkinson_14_2<-Atkinson(base_real_14$m_real_14, n = rep(1, length(base_real_14$m_real_14)), parameter = 0.5)
atkinson_14_3<-Atkinson(base_real_14$m_real_14, n = rep(1, length(base_real_14$m_real_14)), parameter = 0.8)
gini_14<-Gini(base_real_14$m_real_14, n = rep(1, length(base_real_14$m_real_14),unbiased=TRUE))

atkinson_14_1
atkinson_14_2
atkinson_14_3
gini_14


#Diviendo en deciles

library(dplyr)

base_real_14 %>% mutate(quintile = ntile(m_real_14, 10)) -> m_14_diez
head(m_14_diez)

# create a list of data frames for each quintile value
por_decil_14<- split(m_14_diez, m_14_diez$quintile)


#Promedios de ingreso real 2014
i_14<-as.data.frame(lapply(por_decil_14, sapply, mean))

#Gini para 2014
g_14<-as.data.frame(lapply(por_decil_14, sapply , Gini))

#Atkinson par 2014
at_14<-as.data.frame(lapply(por_decil_14,sapply,Atkinson))

a_14<-data.frame(i_14,g_14,at_14)

d_14<-data.frame(t(a_14))




library(tidyr)

enigh_16<-read.dbf("CONCE16.dbf")


promedio_gas_mon<-
  gasto<-enigh_16$gasto_mon*enigh_16$factor

sum_gasto_tot<-sum(gasto)/1000
sum_gasto_tot
sum(enigh_16$factor*enigh_16$tot_integ)

enigh_16<-na.omit(enigh_16)
m_16<-(enigh_16$gasto_mon+enigh_16$otros_gas)/(enigh_16$tot_integ)
m_real_16<-m_16/inpc_2016
summary(m_real_16)


base_real_16<-data.frame(m_real_16)



atkinson_16_1<-Atkinson(base_real_16$m_real_16, n = rep(1, length(base_real_16$m_real_16)), parameter = 0.05)
atkinson_16_2<-Atkinson(base_real_16$m_real_16, n = rep(1, length(base_real_16$m_real_16)), parameter = 0.5)
atkinson_16_3<-Atkinson(base_real_16$m_real_16, n = rep(1, length(base_real_16$m_real_16)), parameter = 0.8)
gini_16<-Gini(base_real_16$m_real_16, n = rep(1, length(base_real_16$m_real_16),unbiased=TRUE))

atkinson_16_1
atkinson_16_2
atkinson_16_3
gini_16



#Diviendo en deciles

library(dplyr)

base_real_16 %>% mutate(quintile = ntile(m_real_16, 10)) -> m_16_diez
head(m_16_diez)

# create a list of data frames for each quintile value
por_decil_16<- split(m_16_diez, m_16_diez$quintile)


#Promedios de ingreso real 2016
i_16<-as.data.frame(lapply(por_decil_16, sapply, mean))


#Gini para 2016
g_16<-as.data.frame(lapply(por_decil_16, sapply , Gini))

#Atkinson par 2016
at_16<-as.data.frame(lapply(por_decil_16,sapply,Atkinson))


a_16<-data.frame(i_16,g_16,at_16)

d_16<-data.frame(t(a_16))


enigh_18<-read.dbf("CONCE18.dbf")
enigh_18<-na.omit(enigh_18)
##Quitando filas con ceros
row_sub = apply(enigh_18, 1, function(row) all(row !=0 ))
##Submuestra resultante
enigh_18<-enigh_18[row_sub,]
m_18<-(enigh_18$gasto_mon+enigh_18$otros_gas)/(enigh_18$tot_integ)

m_real_18<-m_18/inpc_2018
summary(m_real_18)

base_real_18<-data.frame(m_real_18)


atkinson_18_1<-Atkinson(base_real_18$m_real_18, n = rep(1, length(base_real_18$m_real_18)), parameter = 0.05)
atkinson_18_2<-Atkinson(base_real_18$m_real_18, n = rep(1, length(base_real_18$m_real_18)), parameter = 0.5)
atkinson_18_3<-Atkinson(base_real_18$m_real_18, n = rep(1, length(base_real_18$m_real_18)), parameter = 0.8)
gini_18<-Gini(base_real_18$m_real_18, n = rep(1, length(base_real_18$m_real_18),unbiased=TRUE))

atkinson_18_1
atkinson_18_2
atkinson_18_3
gini_18




#Diviendo en deciles

library(dplyr)

base_real_18 %>% mutate(quintile = ntile(m_real_18, 10)) -> m_18_diez
head(m_18_diez)

# create a list of data frames for each quintile value
por_decil_18<- split(m_18_diez, m_18_diez$quintile)


#Promedios de ingreso real 2018
i_18<-as.data.frame(lapply(por_decil_18, sapply, mean))

#Gini para 2018
g_18<-as.data.frame(lapply(por_decil_18, sapply , Gini))


#Atkinson para 2018
at_18<-as.data.frame(lapply(por_decil_18,sapply,Atkinson))

a_18<-data.frame(i_18,g_18,at_18)

d_18<-data.frame(t(a_18))

#Base panel 
panel<-data.frame(d_84,d_89,d_92,d_94,d_96,d_98,d_00,
                  d_02,d_04,d_05,d_06,d_08,d_10,d_12,d_14,d_16,d_18)
write.csv(panel,"panel.csv")



#Ciclos para el total per capita

datos<-read.csv("ciclo_desigualdad.csv")[-1]
años<-c("1984","1989","1992","1994","1996","1998","2000","2002",
        "2004","2005","2006","2008","2010","2012","2014","2016","2018")
ciclo<-datos$CER
atkinson_1<-datos$Atkinson.0.05.
atkinson_2<-datos$Atkinson.0.5.
atkinson_3<-datos$Atkinson.0.8.
gini<-datos$Gini
fecha<-as.Date(años,format=c("%Y"))
class(fecha)

indices<-data.frame(fecha,atkinson_1,atkinson_2,atkinson_3,gini)
#Obtener ciclos para indices

#Usando filtro HP

hp_gini<-hpfilter(gini,freq = 100)
cycle_gini_hp<-hp_gini$cycle
hp_atkinson_1<-hpfilter(atkinson_1,freq=100)
cycle_atkinson_1_hp<-hp_atkinson_1$cycle
hp_atkinson_2<-hpfilter(atkinson_2,freq=100)
cycle_atkinson_2_hp<-hp_atkinson_2$cycle
hp_atkinson_3<-hpfilter(atkinson_3,freq=100)
cycle_atkinson_3_hp<-hp_atkinson_3$cycle

ciclos_hp<-data.frame(cycle_gini_hp,cycle_atkinson_1_hp,
                         cycle_atkinson_2_hp,cycle_atkinson_3_hp)
write.csv(ciclos_hp,"ciclos_hp.csv")

#Usando filtro BK

bk_gini<-bkfilter(gini,pl=2,pu=8,type="fixed",drift=FALSE)
bk_at_1<-bkfilter(atkinson_1,pl=2,pu=8,type="fixed",drift=FALSE)
bk_at_2<-bkfilter(atkinson_2,pl=2,pu=8,type="fixed",drift=FALSE)
bk_at_3<-bkfilter(atkinson_3,pl=2,pu=8,type="fixed",drift=FALSE)

bk_gini<-bk_gini$cycle
bk_at_1<-bk_at_1$cycle
bk_at_2<-bk_at_2$cycle
bk_at_3<-bk_at_3$cycle

ciclos_bk<-data.frame(bk_gini,bk_at_1,bk_at_2,bk_at_3)
write.csv(ciclos_bk,"ciclos_bk.csv")



#Usando filtro Christiano-Fitzgerald
cf_gini<-cffilter(gini,type = "asymmetric",pl=2,pu=8,
         drift = TRUE, root = TRUE, nfix = 1,theta = 1)
cf_at_1<-cffilter(atkinson_1,type = "asymmetric",pl=2,pu=8,
                  drift = TRUE, root = TRUE, nfix = 1,theta = 1)
cf_at_2<-cffilter(atkinson_2,type = "asymmetric",pl=2,pu=8,
                  drift = TRUE, root = TRUE, nfix = 1,theta = 1)
cf_at_3<-cffilter(atkinson_3,type = "asymmetric",pl=2,pu=8,
                  drift = TRUE, root = TRUE, nfix = 1,theta = 1)

cf_gini<-cf_gini$cycle
cf_at_1<-cf_at_1$cycle
cf_at_2<-cf_at_2$cycle
cf_at_3<-cf_at_3$cycle

ciclos_cf<-data.frame(cf_gini,cf_at_1,cf_at_2,cf_at_3)
write.csv(ciclos_cf,"ciclos_cf.csv")


#Usando regresion trigonometrica

gini_even<-gini[2:17]
atkinson_1_even<-atkinson_1[2:17]
atkinson_2_even<-atkinson_2[2:17]
atkinson_3_even<-atkinson_3[2:17]
tr_gini<-trfilter(gini_even,pl=2,pu=8,drift=TRUE)
tr_atkinson_1<-trfilter(atkinson_1_even,pl=2,pu=8,drift=TRUE)
tr_atkinson_2<-trfilter(atkinson_2_even,pl=2,pu=8,drift=TRUE)
tr_atkinson_3<-trfilter(atkinson_3_even,pl=2,pu=8,drift=TRUE)

tr_gini<-tr_gini$cycle
tr_atkinson_1<-tr_atkinson_1$cycle
tr_atkinson_2<-tr_atkinson_2$cycle
tr_atkinson_3<-tr_atkinson_3$cycle

tr_ciclo<-data.frame(tr_gini,tr_atkinson_1,tr_atkinson_2,tr_atkinson_3)
write.csv(tr_ciclo,"ciclos_tr.csv")


#Obteniendo correlaciones para ciclos totales

ciclos<-read.csv("ciclos_indicadores.csv")

library(Hmisc)

cor_tot<-rcorr(as.matrix(ciclos,"pearson"))
corr_tot<-as.data.frame(cor_tot[1])
p_tot<-as.data.frame(cor_tot[3])  

write.csv(corr_tot,"correlacion_ciclos.csv")
write.csv(p_tot,"p_values.csv")


#Sin crisis

#1994

ciclos<-ciclos[-5,]
cor_tot<-rcorr(as.matrix(ciclos,"pearson"))
corr_tot<-as.data.frame(cor_tot[1])
p_tot<-as.data.frame(cor_tot[3])  

write.csv(corr_tot,"correlacion_ciclos.csv")
write.csv(p_tot,"p_values.csv")



#2008
ciclos<-ciclos[-13,]
cor_tot<-rcorr(as.matrix(ciclos,"pearson"))
corr_tot<-as.data.frame(cor_tot[1])
p_tot<-as.data.frame(cor_tot[3])  

write.csv(corr_tot,"correlacion_ciclos.csv")
write.csv(p_tot,"p_values.csv")


#2002
ciclos<-ciclos[-8,]
cor_tot<-rcorr(as.matrix(ciclos,"pearson"))
corr_tot<-as.data.frame(cor_tot[1])
p_tot<-as.data.frame(cor_tot[3])  

write.csv(corr_tot,"correlacion_ciclos.csv")
write.csv(p_tot,"p_values.csv")



#Sin crisis
cor_tot<-rcorr(as.matrix(ciclos,"pearson"))
corr_tot<-as.data.frame(cor_tot[1])
p_tot<-as.data.frame(cor_tot[3])  

write.csv(corr_tot,"correlacion_ciclos.csv")
write.csv(p_tot,"p_values.csv")










#Obteniendo ciclo de ingreso y de desigualdad para los deciles...
#Incluir librerias
install.packages("xts")
library(xts)
library(mFilter)

#Leyendo datos
gd<-read.csv("Gini_Capita.csv", header = TRUE)#Coeficientes de Gini por Decil
ipd<-read.csv("ingreso_decil.csv")    #Ingreso Promedio por Decil
at<-read.csv("Atkinson_Capita.csv")   #Atknson Por Decil
cycles<-read.csv("ciclos_indicadores.csv")


#Gini

#Convirtiendo a serie de tiempo
Date<-as.Date(gd[,1])
Date
gini_deciles<-xts(gd[,2:11],order.by =Date)

#Metodologias Ciclo
#Hodrick-Prescott

hpfg_I<-hpfilter(gini_deciles$I,freq=100)
hpfg_II<-hpfilter(gini_deciles$II,freq=100)
hpfg_III<-hpfilter(gini_deciles$III,freq=100)
hpfg_IV<-hpfilter(gini_deciles$IV,freq=100)
hpfg_V<-hpfilter(gini_deciles$V,freq=100)
hpfg_VI<-hpfilter(gini_deciles$VI,freq=100)
hpfg_VII<-hpfilter(gini_deciles$VII,freq=100)
hpfg_VIII<-hpfilter(gini_deciles$VIII,freq=100)
hpfg_IX<-hpfilter(gini_deciles$IX,freq=100)
hpfg_X<-hpfilter(gini_deciles$X,freq=100)

#Recolectando ciclo HP para deciles
hpfg_deciles<-data.frame(hpfg_I$cycle,hpfg_II$cycle,hpfg_III$cycle,
                         hpfg_IV$cycle,hpfg_V$cycle,hpfg_VI$cycle,
                         hpfg_VII$cycle,hpfg_VIII$cycle,
                         hpfg_IX$cycle,hpfg_X$cycle)
write.csv(hpfg_deciles,"hpfg_deciles_ingreso.csv")


#Filtro Baxter King

bkg_I<-bkfilter(gini_deciles$I,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_II<-bkfilter(gini_deciles$II,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_III<-bkfilter(gini_deciles$III,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_IV<-bkfilter(gini_deciles$IV,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_V<-bkfilter(gini_deciles$V,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_VI<-bkfilter(gini_deciles$VI,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_VII<-bkfilter(gini_deciles$VII,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_VIII<-bkfilter(gini_deciles$VIII,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_IX<-bkfilter(gini_deciles$IX,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_X<-bkfilter(gini_deciles$X,pl=2,pu=8,type="fixed",drift=FALSE)


#Recolectando filtro bk para deciles

bkg<-data.frame(bkg_I$cycle,bkg_II$cycle,bkg_III$cycle,bkg_IV$cycle,
                bkg_V$cycle,bkg_VI$cycle,bkg_VII$cycle,bkg_VIII$cycle,
                bkg_IX$cycle,bkg_X$cycle)

bkg_sig<-data.frame(bkg_I$cycle,bkg_II$cycle,bkg_VI$cycle,bkg_VII$cycle,
                    bkg_X$cycle)
write.csv(bkg_sig,"bkg_sig_decil.csv")

#filtro Christiano-Fitzgerald
cf_I<-cffilter(gini_deciles$I,type="asymmetric",pl=2,pu=8,
               drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_II<-cffilter(gini_deciles$II,type="asymmetric",pl=2,pu=8,
                drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_III<-cffilter(gini_deciles$III,type="asymmetric",pl=2,pu=8,
                 drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_IV<-cffilter(gini_deciles$IV,type="asymmetric",pl=2,pu=8,
                drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_V<-cffilter(gini_deciles$V,type="asymmetric",pl=2,pu=8,
               drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_VI<-cffilter(gini_deciles$VI,type="asymmetric",pl=2,pu=8,
                drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_VII<-cffilter(gini_deciles$VII,type="asymmetric",pl=2,pu=8,
                 drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_VIII<-cffilter(gini_deciles$VIII,type="asymmetric",pl=2,pu=8,
                  drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_IX<-cffilter(gini_deciles$IX,type="asymmetric",pl=2,pu=8,
                drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_X<-cffilter(gini_deciles$X,type="asymmetric",pl=2,pu=8,
               drift=TRUE,root=TRUE,nfix=1,theta=1)

#Recolectando ciclos christiano fitzgerald para deciles
cfg<-data.frame(cf_I$cycle,cf_II$cycle,cf_III$cycle,cf_IV$cycle,
                cf_V$cycle,cf_VI$cycle,cf_VII$cycle,cf_VIII$cycle,
                cf_IX$cycle,cf_X$cycle)


#Usando regresion trigonometrica

tr_I<-trfilter(gini_deciles$I[2:17,],pl=2,pu=8,drift=TRUE)
tr_II<-trfilter(gini_deciles$II[2:17,],pl=2,pu=8,drift=TRUE)
tr_III<-trfilter(gini_deciles$III[2:17,],pl=2,pu=8,drift=TRUE)
tr_IV<-trfilter(gini_deciles$IV[2:17,],pl=2,pu=8,drift=TRUE)
tr_V<-trfilter(gini_deciles$V[2:17,],pl=2,pu=8,drift=TRUE)
tr_VI<-trfilter(gini_deciles$VI[2:17,],pl=2,pu=8,drift=TRUE)
tr_VII<-trfilter(gini_deciles$VII[2:17,],pl=2,pu=8,drift=TRUE)
tr_VIII<-trfilter(gini_deciles$VIII[2:17,],pl=2,pu=8,drift=TRUE)
tr_IX<-trfilter(gini_deciles$IX[2:17,],pl=2,pu=8,drift=TRUE)
tr_X<-trfilter(gini_deciles$X[2:17,],pl=2,pu=8,drift=TRUE)


#Recolectando ciclos de regresion trigonometrica para deciles

trg<-data.frame(tr_I$cycle,tr_II$cycle,tr_III$cycle,
                tr_IV$cycle,tr_V$cycle,tr_VI$cycle,tr_VII$cycle,
                tr_VIII$cycle,tr_IX$cycle,tr_X$cycle)


#Correlaciones Gini y Deciles

#Encontrando Correlaciones de deciles con ciclo economico

#Hodrick Prescott y CF
dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf.csv")
write.csv(p_decil,"p_values_hp_cf.csv")

#Bk

bkh<-data.frame(cycles[4:14,26],bkg[4:14,])

cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh.csv")
write.csv(p_decil,"p_values_bkh.csv")

#Tr
trh<-data.frame(cycles[2:17,25],trg[1:16,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh.csv")
write.csv(p_decil,"p_values_trh.csv")


#Diferencias
dif<-diff(gini_deciles)
diferencias<-data.frame(cycles[2:17,22],dif[2:17,1:10])
write.csv(diferencias,"dif_ciclo_gini.csv")


cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif.csv")
write.csv(p_decil,"p_values_dif.csv")


#Efecto de Crisis

#1994
#Encontrando Correlaciones de deciles con ciclo economico


#Hodrick Prescott y CF
cycles<-cycles[-5,]
hpfg_deciles<-hpfg_deciles[-5,]
cfg<-cfg[-5,]

dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf.csv")
write.csv(p_decil,"p_values_hp_cf.csv")

#Bk
bkg<-bkg[-5,]
bkh<-data.frame(cycles[4:14,26],bkg[4:14,])

cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh.csv")
write.csv(p_decil,"p_values_bkh.csv")

#Tr
trg<-trg[-4,]
trh<-data.frame(cycles[2:17,25],trg[1:16,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh.csv")
write.csv(p_decil,"p_values_trh.csv")


#Diferencias
dif<-diff(gini_deciles)
dif<-dif[-4,]
diferencias<-data.frame(cycles[2:16,22],dif[2:16,1:10])
write.csv(diferencias,"dif_ciclo_gini.csv")


cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif.csv")
write.csv(p_decil,"p_values_dif.csv")


#2002

#Hodrick Prescott y CF
cycles<-cycles[-8,]
hpfg_deciles<-hpfg_deciles[-8,]
cfg<-cfg[-8,]
dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf.csv")
write.csv(p_decil,"p_values_hp_cf.csv")

#Bk
bkg<-bkg[-8,]
bkh<-data.frame(cycles[4:14,26],bkg[4:14,])

cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh.csv")
write.csv(p_decil,"p_values_bkh.csv")

#Tr
trg<-trg[-7,]
trh<-data.frame(cycles[2:17,25],trg[1:16,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh.csv")
write.csv(p_decil,"p_values_trh.csv")


#Diferencias
dif<-diff(gini_deciles)
row(dif)
dif<-dif[-8,]
row(dif)
diferencias<-data.frame(cycles[2:16,22],dif)
write.csv(diferencias,"dif_ciclo_gini.csv")


cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif.csv")
write.csv(p_decil,"p_values_dif.csv")

#2010

#Hodrick Prescott y CF
cycles<-cycles[-13,]
hpfg_deciles<-hpfg_deciles[-13,]
cfg<-cfg[-13,]
dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf.csv")
write.csv(p_decil,"p_values_hp_cf.csv")



#Bk
bkg<-bkg[-13,]
bkh<-data.frame(cycles[4:14,26],bkg[4:14,])

cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh.csv")
write.csv(p_decil,"p_values_bkh.csv")


#Tr
trg<-trg[-12,]

trh<-data.frame(cycles[2:16,25],trg[1:15,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh.csv")
write.csv(p_decil,"p_values_trh.csv")

#Diferencias
dif<-diff(gini_deciles)
dif<-dif[-13,]
diferencias<-data.frame(cycles[,22],dif)
write.csv(diferencias,"dif_ciclo_gini.csv")


cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif_10.csv")
write.csv(p_decil,"p_values_dif_10.csv")


#Sin 1996,2002 y 2010

#HP y CF
dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf.csv")
write.csv(p_decil,"p_values_hp_cf.csv")


#BK

bkh<-data.frame(cycles[4:13,26],bkg[4:13,])
cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh.csv")
write.csv(p_decil,"p_values_bkh.csv")


#TG

trh<-data.frame(cycles[2:16,25],trg[1:15,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh.csv")
write.csv(p_decil,"p_values_trh.csv")


#Diferencias
diferencias<-data.frame(cycles[,22],dif)
write.csv(diferencias,"dif_ciclo_gini.csv")


cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif_10.csv")
write.csv(p_decil,"p_values_dif_10.csv")






#Atkinson
#Incluir librerias
install.packages("xts")
library(xts)
library(mFilter)

#Convirtiendo a serie de tiempo
Date<-as.Date(at[,1])
Date
at_deciles<-xts(at[,2:11],order.by =Date)

#Metodologias Ciclo
#Hodrick-Prescott

hpfg_I<-hpfilter(at_deciles$I,freq=100)
hpfg_II<-hpfilter(at_deciles$II,freq=100)
hpfg_III<-hpfilter(at_deciles$III,freq=100)
hpfg_IV<-hpfilter(at_deciles$IV,freq=100)
hpfg_V<-hpfilter(at_deciles$V,freq=100)
hpfg_VI<-hpfilter(at_deciles$VI,freq=100)
hpfg_VII<-hpfilter(at_deciles$VII,freq=100)
hpfg_VIII<-hpfilter(at_deciles$VIII,freq=100)
hpfg_IX<-hpfilter(at_deciles$IX,freq=100)
hpfg_X<-hpfilter(at_deciles$X,freq=100)

#Recolectando ciclo HP para deciles
hpfg_deciles<-data.frame(hpfg_I$cycle,hpfg_II$cycle,hpfg_III$cycle,
                         hpfg_IV$cycle,hpfg_V$cycle,hpfg_VI$cycle,
                         hpfg_VII$cycle,hpfg_VIII$cycle,
                         hpfg_IX$cycle,hpfg_X$cycle)
write.csv(hpfg_deciles,"hpfg_deciles_at.csv")

#Filtro Baxter King

bkg_I<-bkfilter(at_deciles$I,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_II<-bkfilter(at_deciles$II,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_III<-bkfilter(at_deciles$III,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_IV<-bkfilter(at_deciles$IV,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_V<-bkfilter(at_deciles$V,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_VI<-bkfilter(at_deciles$VI,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_VII<-bkfilter(at_deciles$VII,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_VIII<-bkfilter(at_deciles$VIII,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_IX<-bkfilter(at_deciles$IX,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_X<-bkfilter(at_deciles$X,pl=2,pu=8,type="fixed",drift=FALSE)


bkg_at_sig_decil<-data.frame(bkg_II$cycle,bkg_X$cycle)
write.csv(bkg_at_sig_decil,"bkg_at_decil_sig.csv")

#Recolectando filtro bk para deciles

bkg<-data.frame(bkg_I$cycle,bkg_II$cycle,bkg_III$cycle,bkg_IV$cycle,
                bkg_V$cycle,bkg_VI$cycle,bkg_VII$cycle,bkg_VIII$cycle,
                bkg_IX$cycle,bkg_X$cycle)

write.csv(bkg,"bkg_deciles_at.csv")

#filtro Christiano-Fitzgerald
cf_I<-cffilter(at_deciles$I,type="asymmetric",pl=2,pu=8,
               drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_II<-cffilter(at_deciles$II,type="asymmetric",pl=2,pu=8,
                drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_III<-cffilter(at_deciles$III,type="asymmetric",pl=2,pu=8,
                 drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_IV<-cffilter(at_deciles$IV,type="asymmetric",pl=2,pu=8,
                drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_V<-cffilter(at_deciles$V,type="asymmetric",pl=2,pu=8,
               drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_VI<-cffilter(at_deciles$VI,type="asymmetric",pl=2,pu=8,
                drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_VII<-cffilter(at_deciles$VII,type="asymmetric",pl=2,pu=8,
                 drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_VIII<-cffilter(at_deciles$VIII,type="asymmetric",pl=2,pu=8,
                  drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_IX<-cffilter(at_deciles$IX,type="asymmetric",pl=2,pu=8,
                drift=TRUE,root=TRUE,nfix=1,theta=1)

cf_X<-cffilter(at_deciles$X,type="asymmetric",pl=2,pu=8,
               drift=TRUE,root=TRUE,nfix=1,theta=1)

#Recolectando ciclos christiano fitzgerald para deciles
cfg<-data.frame(cf_I$cycle,cf_II$cycle,cf_III$cycle,cf_IV$cycle,
                cf_V$cycle,cf_VI$cycle,cf_VII$cycle,cf_VIII$cycle,
                cf_IX$cycle,cf_X$cycle)
write.csv(cfg,"cfg_deciles_at.csv")


#Usando regresion trigonometrica

tr_I<-trfilter(at_deciles$I[2:17,],pl=2,pu=8,drift=TRUE)
tr_II<-trfilter(at_deciles$II[2:17,],pl=2,pu=8,drift=TRUE)
tr_III<-trfilter(at_deciles$III[2:17,],pl=2,pu=8,drift=TRUE)
tr_IV<-trfilter(at_deciles$IV[2:17,],pl=2,pu=8,drift=TRUE)
tr_V<-trfilter(at_deciles$V[2:17,],pl=2,pu=8,drift=TRUE)
tr_VI<-trfilter(at_deciles$VI[2:17,],pl=2,pu=8,drift=TRUE)
tr_VII<-trfilter(at_deciles$VII[2:17,],pl=2,pu=8,drift=TRUE)
tr_VIII<-trfilter(at_deciles$VIII[2:17,],pl=2,pu=8,drift=TRUE)
tr_IX<-trfilter(at_deciles$IX[2:17,],pl=2,pu=8,drift=TRUE)
tr_X<-trfilter(at_deciles$X[2:17,],pl=2,pu=8,drift=TRUE)


#Recolectando ciclos de regresion trigonometrica para deciles

trg<-data.frame(tr_I$cycle,tr_II$cycle,tr_III$cycle,
                tr_IV$cycle,tr_V$cycle,tr_VI$cycle,tr_VII$cycle,
                tr_VIII$cycle,tr_IX$cycle,tr_X$cycle)
write.csv(trg,"trg_deciles_at.csv")


#Encontrando Correlaciones de deciles con ciclo economico
install.packages("Hmisc")
library(Hmisc)

ciclo<-read.csv("ciclo_capita.csv")

#Hodrick Prescott y CF
dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf_at.csv")
write.csv(p_decil,"p_values_hp_cf_at.csv")

#Bk

bkh<-data.frame(cycles[4:14,26],bkg[4:14,])

cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh_at.csv")
write.csv(p_decil,"p_values_bkh_at.csv")

#Tr
trh<-data.frame(cycles[2:17,25],trg[1:16,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh_at.csv")
write.csv(p_decil,"p_values_trh_at.csv")



#Diferencias
dif<-diff(at_deciles)
diferencias<-data.frame(cycles[2:17,22],dif[2:17,1:10])
write.csv(diferencias,"dif_at_deciles.csv")

cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif_at.csv")
write.csv(p_decil,"p_values_dif_at.csv")


#Sin 1996

#1994
#Encontrando Correlaciones de deciles con ciclo economico


#Hodrick Prescott y CF
cycles<-cycles[-5,]
hpfg_deciles<-hpfg_deciles[-5,]
cfg<-cfg[-5,]

dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf_at.csv")
write.csv(p_decil,"p_values_hp_cf_at.csv")

#Bk
bkg<-bkg[-5,]
bkh<-data.frame(cycles[4:14,26],bkg[4:14,])

cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh_at.csv")
write.csv(p_decil,"p_values_bkh_at.csv")


#Tr
trg<-trg[-4,]
trh<-data.frame(cycles[2:17,25],trg[1:16,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh_at.csv")
write.csv(p_decil,"p_values_trh_at.csv")


#Diferencias
dif<-diff(at_deciles)
dif<-dif[-4,]
diferencias<-data.frame(cycles[2:16,22],dif[2:16,1:10])
write.csv(diferencias,"dif_ciclo_gini.csv")


cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif_at.csv")
write.csv(p_decil,"p_values_dif_at.csv")


#Sin 2002


#2002
cycles<-read.csv("ciclos_indicadores.csv")

#Hodrick Prescott y CF
cycles<-cycles[-8,]
hpfg_deciles<-hpfg_deciles[-8,]
cfg<-cfg[-8,]
dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf_at.csv")
write.csv(p_decil,"p_values_hp_cf_at.csv")

#Bk
bkg<-bkg[-8,]
bkh<-data.frame(cycles[4:14,26],bkg[4:14,])

cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh_at.csv")
write.csv(p_decil,"p_values_bkh_at.csv")

#Tr
trg<-trg[-7,]
trh<-data.frame(cycles[2:17,25],trg[1:16,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh_at.csv")
write.csv(p_decil,"p_values_trh_at.csv")


#Diferencias
dif<-diff(at_deciles)
row(dif)
dif<-dif[-8,]
row(dif)
diferencias<-data.frame(cycles[2:16,22],dif)
write.csv(diferencias,"dif_ciclo_gini_at.csv")


cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif_at.csv")
write.csv(p_decil,"p_values_dif_at.csv")


#Sin 2010

#2010

#Hodrick Prescott y CF
cycles<-cycles[-13,]
hpfg_deciles<-hpfg_deciles[-13,]
cfg<-cfg[-13,]
dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf_at.csv")
write.csv(p_decil,"p_values_hp_cf_at.csv")



#Bk
bkg<-bkg[-13,]
bkh<-data.frame(cycles[4:14,26],bkg[4:14,])

cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh_at.csv")
write.csv(p_decil,"p_values_bkh_at.csv")


#Tr
trg<-trg[-12,]

trh<-data.frame(cycles[2:16,25],trg[1:15,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh_at.csv")
write.csv(p_decil,"p_values_trh_at.csv")

#Diferencias
dif<-diff(at_deciles)
dif<-dif[-13,]
diferencias<-data.frame(cycles[,22],dif)
write.csv(diferencias,"dif_ciclo_gini.csv")


cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif_at.csv")
write.csv(p_decil,"p_values_dif_at.csv")


#Sin 1996,2002 y 2010

#HP y CF
dfh<-data.frame(cycles[,23:24],hpfg_deciles,cfg)

cor_decil<-rcorr(as.matrix(dfh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_hp_cf_at.csv")
write.csv(p_decil,"p_values_hp_cf_at.csv")


#BK

bkh<-data.frame(cycles[4:13,26],bkg[4:13,])
cor_decil<-rcorr(as.matrix(bkh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_bkh_at.csv")
write.csv(p_decil,"p_values_bkh_at.csv")


#TG

trh<-data.frame(cycles[2:16,25],trg[1:15,])

cor_decil<-rcorr(as.matrix(trh,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_trh_at.csv")
write.csv(p_decil,"p_values_trh_at.csv")


#Diferencias
diferencias<-data.frame(cycles[,22],dif)
write.csv(diferencias,"dif_ciclo_gini.csv")


cor_decil<-rcorr(as.matrix(diferencias,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"correlaciones_dif_at.csv")
write.csv(p_decil,"p_values_dif_at.csv")





#Agregado vs. Microdatos
#Obteniendo ciclos para microdatos
micro<-read.csv("microdata.csv")

#Obteniendo ciclos 


#Ingreso Real
ing_hp<-hpfilter(micro$Ingreso,freq=100,drift=FALSE)
ing_bk<-bkfilter(micro$Ingreso,pl=2,pu=8,type="fixed",drift=FALSE)
ing_cf<-cffilter(micro$Ingreso,type="asymmetric",pl=2,pu=8,
                 drift=TRUE, root=TRUE, nfix = 1, theta=1)
ing_tr<-trfilter(micro[2:17,2],pl=2,pu=8, drift=TRUE)
ing_dif<-diff(micro$Ingreso)

#Precios
p_hp<-hpfilter(micro$Precios,freq=100,drift=100)
p_bk<-bkfilter(micro$Precios,pl=2,pu=8,type="fixed",drift = FALSE)
p_cf<-cffilter(micro$Precios,type="asymmetric",pl=2,pu=8,drift=TRUE,
               root=TRUE, nfix=1, theta = 1)
p_tr<-trfilter(micro[2:17,3],pl=2,pu=8, drift = TRUE)
p_dif<-diff(micro$Precios)

#Recolectando ciclos
#HP y cf
ciclos_1<-data.frame(ing_hp$cycle,ing_cf$cycle,
                     p_hp$cycle, p_cf$cycle)
#bk
ciclos_2<-data.frame(ing_bk$cycle,p_bk$cycle)
ciclos_precio<-data.frame(cycles[4:14,26],p_bk$cycle)

bkh<-data.frame(cycles[4:14,26],bkg[4:14,])

#tr y dif
ciclos_3<-data.frame(ing_dif, ing_tr$cycle,p_dif,p_tr$cycle)

write.csv(ciclos_1,"hp_cf_metaingreso.csv")
write.csv(ciclos_2,"bk_metaingreso.csv")
write.csv(ciclos_3,"dif_tr_metaingreso.csv")


#Por decil

m<-read.csv("ingreso_decil.csv")
#Convirtiendo a serie de tiempo
Date<-as.Date(m[,1])
Date
at_deciles<-xts(m[,2:11],order.by =Date)

#Filtro Baxter King

bkg_I<-bkfilter(at_deciles$I,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_II<-bkfilter(at_deciles$II,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_III<-bkfilter(at_deciles$III,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_IV<-bkfilter(at_deciles$IV,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_V<-bkfilter(at_deciles$V,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_VI<-bkfilter(at_deciles$VI,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_VII<-bkfilter(at_deciles$VII,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_VIII<-bkfilter(at_deciles$VIII,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_IX<-bkfilter(at_deciles$IX,pl=2,pu=8,type="fixed",drift=FALSE)
bkg_X<-bkfilter(at_deciles$X,pl=2,pu=8,type="fixed",drift=FALSE)

bkg_decil_ing<-data.frame(bkg_I$cycle,bkg_II$cycle,bkg_III$cycle,bkg_IV$cycle,
                bkg_V$cycle,bkg_VI$cycle,bkg_VII$cycle,bkg_VIII$cycle,
                bkg_IX$cycle,bkg_X$cycle)

write.csv(bkg_decil_ing,"bkg_decil_ing.csv")

bkg_m<-read.csv("bkg_decil_ing.csv")
cor_decil<-rcorr(as.matrix(bkg_m,"pearson"))
corr_decil<-as.data.frame(cor_decil[1])
p_decil<-as.data.frame(cor_decil[3])  

write.csv(corr_decil,"bkg_m_corr.csv")

write.csv(p_decil,"p_decil_m.csv")