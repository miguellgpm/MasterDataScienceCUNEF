
#################################################
###                                           ###
### ASIGNATURA: GESTIÓN DEL RIESGO OPERATIVO  ###
### PRÁCTICA FINAL                            ###
### ALUMNO: MIGUEL LÓPEZ GARRALÓN             ###
### FECHA: 08/04/2020                         ###
### PROFESORA: SONIA DE PAZ COBO              ###
###                                           ###
#################################################


###### LIBRERÍAS

library(MASS)
library(CASdatasets)
library(car)
library(actuar) 
library(fitdistrplus)
library(ggplot2)
library(evmix)
library(dplyr)
library(skimr)
library(moments) #Package para calculo de momentos (asimetr?a, curtosis, ...)
library(fitdistrplus) #Package para ajuste de distribuciones
library(purrr)  #Package para ciertas distribuciones
library(tidyr)
library(sn)
library(readr)
library(lubridate)



datos <- read_csv("danishuni.csv")

# Agrupamos por día, sumando las pérdidas por cada día

datos

agregados <- datos %>% group_by(Date) %>% summarize(loss_total = sum(Loss),
                                                    frec = n())

head(agregados)


agregados3 <- agregados %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(Date) %>% 
  group_by(week) %>% 
  summarize(Loss_sev = sum(loss_total), freq = sum(frec))


        
x <- agregados3$Loss_sev


x2 <- agregados3$freq


###### ANÁLISIS DE FRECUENCIAS

head(x2,10) #vemos los 10 primeros elementos
summary(x2)
table(x2) #tabla de frecuencias

skewness(x2)  
# la asimetría es positiva, por lo que los casos se concentran en los valores bajos

kurtosis(x2)  
# el coeficiente de curtosis es positivoy superior a 3, por lo que se trata de una 
# curva leptocúrtica


# Otros valores estadísticos:

mean(x2) 
var(x2)  
median(x2)
# Se puede ver que la media y la mediana son muy diferentes

quantile(x2,seq(0,1, 0.20))

quantile(x2,probs=c(0.05, 0.95))

quantile(x2,seq(0.9,1, 0.01))

mean(x) 
var(x)  
median(x)

quantile(x,seq(0,1, 0.20))

quantile(x,probs=c(0.05, 0.95))

quantile(x,seq(0.9,1, 0.01))

hist(x2, pch=20, breaks=25, prob=TRUE, main="HISTOGRAMA",
     xlab ="Número de Pérdidas", ylab = "Frecuencia")

# Con la visualización del histograma podemos ver lo que se ha comentado previamente
# con los coeficientes de asimetría y curtosis. La mayoría de los valores se concentran
# en la parte izquierda de la distribución.


############# Ajustar los datos a una distribución de probabilidad

# Las distribuciones elegidas se han llevado a cabo debido a que son distribuciones
# para muestras con valores discretos

# Ajuste por máxima verosimilitud de distribuciones univariables


### Binomial Negativa Máxima Verosimilitud

MLE_bin_neg <- fitdist(x2, "nbinom", method = "mle", discrete = TRUE)

MLE_bin_neg

curve(dnbinom(x, size = MLE_bin_neg$estimate[1], mu = MLE_bin_neg$estimate[2]),
          col="green", lwd=2,
          add=T)

### Poisson Máxima Verosimilitud

MLE_poisson <- fitdist(x2, "pois", method = "mle", discrete = TRUE)

MLE_poisson

curve(dpois(x, MLE_poisson$estimate[1]),
      col="blue", lwd=2,
      add=T)

### Geométrica Máxima Verosimilitud

MLE_geom <- fitdist(x2, "geom", method = "mle", discrete = TRUE)

MLE_geom

curve(dgeom(x, MLE_geom$estimate[1]),
      col="red", lwd=2,
      add=T)


#Ajuste por bondad del ajuste

gofstat(list(MLE_bin_neg, MLE_poisson, MLE_geom), chisqbreaks=c(0:4, 9),
        discrete=TRUE,
        fitnames=c("Binomial Negativa", "Poisson", 'Geométrica'))


cdfcomp(list(MLE_bin_neg, MLE_poisson, MLE_geom),
        xlogscale = FALSE, datapch = ".", 
        datacol = "red", fitlty = 2:5,
        legendtext = c('MLE_bin_neg', 'MLE_poisson', 'MLE_geom'),
        main = "Comparación de ajustes")

# Debido a que los criterios de información entre la binomial negativa y la Poisson son muy
# similares, se ha decidido elegir la Binomial Negativa ya que tiene un p-value de Chi-cuadrado
# superior a la Poisson y se ve como en el gráfico de cdfcomp se ajusta mejor a la muestra
# del caso


##### ANÁLISIS EXPLORATORIO

skim(x)

var(x) 

quantile(x,probs=c(0.05, 0.95))
quantile(x,seq(0,1, 0.20)) 
quantile(x,seq(0.9,1, 0.01))

ggplot(agregados3, aes(Loss_sev)) + geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 1)  +
  xlab("Pérdidas") + ylab("Frecuencia") + ggtitle("Distribución de pérdidas")

skewness(x)  
# Al ser positiva la asimetría, con un valor de 7.9, hace referencia a que la cola a la
# derecha es más larga que a la izquierda

kurtosis(x)
# La curva es leptocúrtica ya que se obtiene un valor de Kurtosis positivo y muy elevado,
# 90. Es por esto que los valores se concentran en torno a la media y las
# colas son menos pesadas.


############# Ajustar los datos de pérdidas a una distribución de probabilidad

# Se han elegido 6 tipos diferentes de distribuciones. Su elección se ha debido a que
# son continuas y tienen asimetría positiva, con valores atípicos principalmente
# en la parte derecha de la cola

# Exponencial
# Lognormal
# Gamma
# Weibull
# Pareto
# Burr

# Se realiza Máxima Verosimilitud

plot(density(x), main="Densidades empíricas",
     lwd=2, xlab="x", ylab="frecuencia")

### Exponencial

MLE_exp <- fitdist(x, "exp", method = "mle")

MLE_exp$estimate

curve(dexp(x, MLE_exp$estimate[1]),
      col="green", lwd=2,
      add=T)

### Pareto

MLE_pareto <- fitdist(x, "pareto", method = "mle")

MLE_pareto$estimate

curve(dpareto(x, MLE_pareto$estimate[1], MLE_pareto$estimate[2]),
      col="blue", lwd=2,
      add=T)


### Gamma

MLE_gamma <- fitdist(x, "gamma", method = "mle")

MLE_gamma$estimate

curve(dgamma(x, MLE_gamma$estimate[1], MLE_gamma$estimate[2]),
      col="orange", lwd=2,
      add=T)

### Lognormal

MLE_lnorm <- fitdist(x, "lnorm", method = "mle")

MLE_lnorm$estimate

curve(dlnorm(x, MLE_lnorm$estimate[1], MLE_lnorm$estimate[2]),
      col="red", lwd=2,
      add=T)


### Weibull

MLE_weibull <- fitdist(x, "weibull", method = "mle")

MLE_weibull$estimate

curve(dweibull(x, MLE_weibull$estimate[1], MLE_weibull$estimate[2]),
      col="yellow", lwd=2,
      add=T)


### Burr

MLE_burr <- fitdist(x, "burr", start = list(shape1 = 2, shape2 = 2, scale = 2),
                       lower = c(0.1,1/2, 0), method = 'mle')

MLE_burr$estimate

curve(dburr(x, MLE_burr$estimate[1], MLE_burr$estimate[2], MLE_burr$estimate[3]),
      col="yellow", lwd=2,
      add=T)


# Visualización de gráficos QQ-Plot

plot(MLE_exp)
plot(MLE_gamma)
plot(MLE_lnorm)
plot(MLE_pareto)
plot(MLE_weibull)
plot(MLE_burr)


# Comparación de los gráficos QQ-Plot de las 5 distribuciones

qqcomp(list(MLE_exp, MLE_gamma, MLE_lnorm, MLE_pareto, MLE_weibull, MLE_burr),
       xlogscale=TRUE, ylogscale=TRUE, 
       ylab = "cuantiles empíricos", xlab="cuantiles teóricos",
       main="QQ-plot sobre pérdidas Distribuciones",
       addlegend = TRUE,
       legendtext=c('Exponencial', 'Gamma', 'LogNormal', 'Pareto', 'Weibull', 'Burr'),
       fitpch=1:4, xlegend = 'topleft')

# PP-Plot de las 5 distribuciones

ppcomp(list(MLE_exp, MLE_gamma, MLE_lnorm, MLE_pareto, MLE_weibull, MLE_burr),
       xlogscale=TRUE, ylogscale=TRUE, 
       ylab = "cuantiles empíricos", xlab="cuantiles teóricos",
       main="PP-plot sobre pérdidas Distribuciones",
       addlegend = TRUE,
       legendtext=c('Exponencial', 'Gamma', 'LogNormal', 'Pareto', 'Weibull', 'MLE_burr'),
       fitpch=1:4, xlegend = 'bottomright')


###Estimación por bondad del ajuste

plot(density(x), main="Densidades empíricas",
     lwd=2, xlab="x", ylab="frecuencia")

### Exponencial

MGE_exp <- fitdist(x, "exp", method = "mge", gof = "CvM")

MGE_exp$estimate

curve(dexp(x, MGE_exp$estimate[1]),
      col="green", lwd=2,
      add=T)

### Pareto

MGE_pareto <- fitdist(x, "pareto", method = "mge")

MGE_pareto$estimate

curve(dpareto(x, MGE_pareto$estimate[1], MGE_pareto$estimate[2]),
      col="blue", lwd=2,
      add=T)


### Gamma

MGE_gamma <- fitdist(x, "gamma", method = "mge")

MGE_gamma$estimate

curve(dgamma(x, MGE_gamma$estimate[1], MGE_gamma$estimate[2]),
      col="orange", lwd=2,
      add=T)

### Lognormal

MGE_lnorm <- fitdist(x, "lnorm", method = "mge")

MGE_lnorm$estimate

curve(dlnorm(x, MGE_lnorm$estimate[1], MGE_lnorm$estimate[2]),
      col="red", lwd=2,
      add=T)


### Weibull

MGE_weibull <- fitdist(x, "weibull", method = "mge")

MGE_weibull$estimate

curve(dweibull(x, MGE_weibull$estimate[1], MGE_weibull$estimate[2]),
      col="yellow", lwd=2,
      add=T)

### Burr

MGE_burr <- fitdist(x, "burr", start = list(shape1 = 2, shape2 = 2, scale = 2),
                    lower = c(0.1,1/2, 0), method = 'mge')


curve(dburr(x, MGE_burr$estimate[1], MGE_burr$estimate[2], MGE_burr$estimate[3]),
      col="yellow", lwd=2,
      add=T)


# Visualización de gráficos QQ-Plot

plot(MGE_exp)
plot(MGE_gamma)
plot(MGE_lnorm)
plot(MGE_pareto)
plot(MGE_weibull)
plot(MGE_burr)


# Comparación de los gráficos QQ-Plot de las 5 distribuciones

qqcomp(list(MGE_exp, MGE_gamma, MGE_lnorm, MGE_pareto, MGE_weibull),
       xlogscale=TRUE, ylogscale=TRUE, 
       ylab = "cuantiles empíricos", xlab="cuantiles teóricos",
       main="QQ-plot sobre pérdidas Distribuciones",
       addlegend = TRUE,
       legendtext=c('Exponencial', 'Gamma', 'LogNormal', 'Pareto', 'Weibull'),
       fitpch=1:4, xlegend = 'topleft')

# PP-Plot de las 5 distribuciones

ppcomp(list(MGE_exp, MGE_gamma, MGE_lnorm, MGE_pareto, MGE_weibull),
       xlogscale=TRUE, ylogscale=TRUE, 
       ylab = "cuantiles empíricos", xlab="cuantiles teóricos",
       main="PP-plot sobre pérdidas Distribuciones",
       addlegend = TRUE,
       legendtext=c('Exponencial', 'Gamma', 'LogNormal', 'Pareto', 'Weibull'),
       fitpch=1:4, xlegend = 'bottomright')





gofstat(list(MGE_exp, MGE_gamma, MGE_lnorm, MGE_pareto, MGE_weibull, MGE_burr,
             MLE_exp, MLE_gamma, MLE_lnorm, MLE_pareto, MLE_weibull, MLE_burr),
        chisqbreaks=c(0:4, 9),
        discrete=FALSE,
        fitnames=c('MGE_exp', 'MGE_gamma', 'MGE_lnorm', 'MGE_pareto', 'MGE_weibull', 'MGE_burr',
                   'MLE_exp', 'MLE_gamma', 'MLE_lnorm', 'MLE_pareto', 'MLE_weibull', 'MLE_burr'))


cdfcomp(list(MLE_lnorm, MLE_exp, MLE_burr, MLE_gamma, MLE_pareto, MLE_weibull),
        xlogscale = TRUE, datapch = ".", 
        datacol = "red", fitlty = 2:5,
        legendtext = c('MLE_lnorm', 'MLE_exp', 'MLE_burr', 'MLE_gamma',  'MLE_pareto', 'MLE_weibull'),
        main = "Comparación de ajustes")



cdfcomp(list(MGE_exp, MGE_gamma, MGE_lnorm, MGE_pareto, MGE_weibull, MGE_burr),
        xlogscale = TRUE, datapch = ".", 
        datacol = "red", fitlty = 2:5,
        legendtext = c('MGE_exp', 'MGE_gamma', 'MGE_lnorm', 'MGE_pareto', 'MGE_weibull', 'MGE_burr'),
        main = "Comparación de ajustes")



# La distribución a la que más se adecúa es a una burr

##### VALORES EXTREMOS


agregados4 <- agregados %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(Date) %>% 
  group_by(week) %>% 
  summarize(Loss_max = max(loss_total), freq = sum(frec))

danish.max <- agregados4$Loss_max

danish.max


##### ANÁLISIS EXPLORATORIO

skim(danish.max)

var(danish.max) 

quantile(danish.max,probs=c(0.05, 0.95))
quantile(danish.max,seq(0,1, 0.20)) 
quantile(danish.max,seq(0.8,1, 0.01))

quantile(agregados3$Loss_sev,seq(0.8,1, 0.01))

summary(danish.max)

q1 <- quantile(danish.max, 0.25)
q3 <- quantile(danish.max, 0.75)

q1
q3

## diferencia entre tercer cuartil y primer cuartil
rq <- q3 - q1

##valores extremos derecha:

dcha <- q3 + 1.5*rq 
izq <- q1 - 1.5*rq  ## no tiene sentido porq son negativos

# Umbral

u <- dcha
u

danish.exc <- danish.max[danish.max > u]

danish.exc

summary(danish.exc)


#Visualizacion de las colas

#La distribucion de Pareto Generalizada podria proporcionar una buena aproximacion


n.u <- length(danish.exc) #n de casos que superan u
n.u

# Establecemos el ranking del orden de ocurrencia 

rank(danish.exc)


summary(danish.exc)

#Determinamos las probabilidades empiricas de la muestra

surv.prob <- 1 - (rank(danish.exc)/(n.u + 1))

#Se obtiene la probabilibada como 1 menos cociente entre nº orden/59.
#El valor 263.25 tiene la prob mas baja de ocurrencia


surv.prob


plot(danish.exc, surv.prob, log = "xy", xlab = "Excesos", 
     ylab = "Probabilidades", ylim=c(0.01, 1))


######### FUNCIÓN DE PÉRDIDAS AGREGADAS

MLE_bin_neg$estimate
MLE_burr$estimate



# Para la frecuencia se sigue una distribución Binomial Negativa de parámetros
# size = 23.46 y mu = 3.9
# y la severidad una Burr de parámetros 1.111754, 1.970112 y 9.562810:

parsev <- MLE_burr$estimate ; parfreq <- MLE_bin_neg$estimate

parsev

xmax <- qburr(1-1e-9, parsev[1], parsev[2], parsev[3]) #Func.cuantialica  



#Calculamos función de distribucion discretizandola (función discretize)

#Discretización por M. recursivo


#Metodo insesgado

fx2 <- discretize(pburr(x, parsev[1], parsev[2], parsev[3]), from = 0,
                  to = xmax, step = 0.5, method = "unbiased",
                  lev = levburr(x, parsev[1], parsev[2], parsev[3])) #unbiased


F2 <- aggregateDist("recursive", model.freq = "negative binomial",
                    model.sev = fx2, size = parfreq[1], mu = parfreq[2],
                    prob = 2/3, x.scale = 0.5, maxit = 2000)


#Metodo d. superior

#fx.u2 <- discretize(pburr(x, parsev[1], parsev[2], parsev[3]), from = 0,
#                    to = xmax, step = 0.5, method = "upper",
#                    lev = levburr(x, parsev[1], parsev[2], parsev[3]))

#F.u2 <- aggregateDist("recursive", model.freq = "negative binomial",
#                      model.sev = fx.u2, size = parfreq[1], mu = parfreq[2],
#                      prob = 1/3, x.scale = 0.5, maxit=2000)

#Metodo d. inferior

#fx.l2 <- discretize(pburr(x, parsev[1], parsev[2], parsev[3]), from = 0,
#                    to = xmax, step = 0.5, method = "lower",
#                    lev = levburr(x, parsev[1], parsev[2], parsev[3]))

#F.l2 <- aggregateDist("recursive", model.freq = "negative binomial",
#                      model.sev = fx.l2, size = parfreq[1], mu = parfreq[2],
#                      prob = 1/3, x.scale = 0.5, maxit=2000)


plot(F2, do.points = F, verticals = T, col = 'red')

#plot(F.u2, do.points = F, verticals = T, add = TRUE, col = 'blue')

#plot(F.l2, do.points = F, verticals = T, add = TRUE, col = 'green')


legend("bottomright", leg=c("Rec.insesgado"),#, "Rec.superior", "Rec.inferior"),
       col = c("red"), #, "red", "blue"),
       lty = 1:3, text.col = "red")



######## SIMULACIÓN DE DISTRIBUCIÓN CON FUNCIÓN DE PÉRDIDAS AGREGADAS

MLE_bin_neg$estimate
MLE_burr$estimate

set.seed(4321)  # For reproducibility of results
m <- 10000      # Number of observations to simulate
size <- MLE_bin_neg$estimate[1]; mu <-  MLE_bin_neg$estimate[2]   # Parameter for frequency distribution N
a <- MLE_burr$estimate[1]
b <- MLE_burr$estimate[2]
c <- MLE_burr$estimate[3]  # Parameters for severity distribution X
S <- rep(NA, m) # Initalize an empty vector to store S observations

n <- rnbinom(m, size = size, mu = mu) # Generate m=10000 observations of N from Poisson
for(j in 1:m){ 
  n_j <- n[j] # Given each n_j (j=1,...,m), generate n_j observations of X from uniform
  x_j <- rburr(n_j, shape1 = a, shape2 = b, scale = c)
  s_j <- sum(x_j) # Calculate the aggregate loss s_j
  S[j] <- s_j # Store s_j in the vector of observations
}
mean(S) 
sd(S)  

## VaR

alpha <- 0.95

VaR <- qnorm(alpha, mean = -mean(S), sd = sd(S))

VaR

## ES

ES <- (-mean(S) + sd(S) * dnorm(qnorm(alpha))/(1 - alpha))

ES


## Representancion

x <- seq(0.9, 0.999, length = 100)

yVaR <- (-mean(S) + sd(S) * qnorm(x))

yES <- (-mean(S) + sd(S) * dnorm(qnorm(x)) / (1 - x)) 

plot(x, yVaR, type = 'l', ylim = range(yVaR, yES), xlab = expression(alpha), ylab = "")

lines(x, yES, lty = 2, col = 2)

legend("topleft", legend = c("VaR", "ES"), col = 1:2, lty = 1:2)


sucesosDF <- agregados4

agregados4 %>% ggplot(aes(x = freq)) + geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 1) + xlab("Frecuencia")

agregados4 %>% ggplot(aes(x = Loss_max)) + geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 1) + xlab("Cuantía pérdidas")

distrib_agregada <- data.frame(S[S > 0])

distrib_agregada %>% ggplot(aes(x = S[S > 0])) +
  geom_density(fill = "#69b3a2", color = "#e9ecef", alpha = 1) +
  xlab("Pérdidas agregadas") +
  ggtitle("Simulación de distribución con función de pérdidas agregadas")




