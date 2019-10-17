# AUTOR: MIGUEL LOPEZ GARRALON
# FECHA: 04/10/2019
# EJERCICIOS DE DATOS DE UNA PULSERA DE ACTIVIDAD


library(readr)
library(markdown)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(TeachingDemos)

# 1. Crear un nuevo proyecto denominado practica 4.

# 2. Mediante la libreria readr, o mediante los menus de RStudio, leer los datasets sleep.csv  y activities.csv
# ambos archivos deben estar previamente en la carpeta del proyecto creado
activities <- read_csv("activities.csv")

# 3.Comprobar el contenido  con View y contar cuantos NAs hay en la columna GPS del dataset activities

View(activities)
valores_na_gps <- sum(is.na(activities$GPS))
cat("Hay",valores_na_gps,"valores NA en la variable GPS")

# 4. Crear un objeto R denominado act_new que contenga solo las variables 
# siguientes: 1,2,5-6

act_new <- select(activities, 1,2,5,6)
View(act_new)

# 5. Renombrar la variable 'Activity type' con el nombre 'tipo' y la variable 'Time zone' como 'ciudad'

act_new <- rename(act_new, tipo = 'Activity type')
act_new <- rename(act_new, ciudad = Timezone)
view(act_new)

# 6. Realizar un recuento de tipo de actividad con summary. Para ello 
# debes transformar previamente la variable tipo a factor con as.factor.
# Crea un grafico de barras con dicha variable para visualizar las frecuencias.
# Haz lo mismo para la variable ciudad

act_new$tipo <- as.factor(act_new$tipo)
ggplot(data = act_new) + geom_col(mapping = aes(x = act_new$tipo, y = frequency(act_new$tipo)))

act_new$ciudad <- as.factor(act_new$ciudad)
plot(act_new$ciudad, main = "Frecuencia deportes en cada ciudad", xlab = "Ciudades", ylab = "Frecuencia")


#7. Filtrar los registros de act_new que correspondan con ciudad Amsterdam en otro objeto
# y lo mismo con Madrid. Con esos nuevos objetos determina los deportes que 
# no se practican en Amsterdam y s? en Madrid y viceversa. Genera graficos para visualizar los resultados

madrid_amsterdam <- subset(act_new, act_new$ciudad != "Europe/Rome")

# PIDIENDO UNA TABLA SE VE DE FORMA SENCILLA Y RAPIDA

table(madrid_amsterdam$tipo,as.character(madrid_amsterdam$ciudad))

# VISUALIZACION DEL GRAFICO, DONDE EL PUNTO IMPLICA QUE SE HA PRACTICADO DICHO DEPORTE

ggplot(data = madrid_amsterdam) + geom_point(mapping = aes(x = madrid_amsterdam$ciudad, y = madrid_amsterdam$tipo))

cat("Los deportes practicados solo en Madrid son Hiking, other, Ski, Swimming y Tennis. El deporte practicado solo en Amsterdam es Dancing.")


#8. Encontrar las fechas en las que se ha practicado bicicleta o pilates en Amsterdam en el a?o 2019

deporte_amsterdam <- filter(act_new, ciudad == "Europe/Amsterdam")
deporte_amsterdam$de <- as.character(deporte_amsterdam$de)

# CREACION DE NUEVA VARIABLE QUE INDICA EL A?O

deporte_amsterdam <- mutate(deporte_amsterdam, year = substr(deporte_amsterdam$de, 1, 4))
View(deporte_amsterdam)

deporte_amsterdam <- filter(deporte_amsterdam, year == '2019' , tipo == 'Pilates' | tipo == 'Cycling')
deporte_amsterdam


#9. Crear una nueva variable dif con los minutos de realizaci?n de cada actividad en Amsterdam
# y realizar una representacion grafica de los resultados con plot y determinar que deporte o deportes
# se han practicado durante dos horas o mas

act_new <- mutate(act_new, dif = a - de)
View(act_new)

# ESTE SERIA UN GRAFICO SIMPLE QUE MOSTRARIA EL TIEMPO DE REALIZACION DE CADA DEPORTE CADA VEZ QUE SE HA REALIZADO

act_new$tipo <- as.factor(act_new$tipo)
ggplot(data = act_new) + geom_point(mapping = aes(x = act_new$tipo, y = act_new$dif))

# AHORA SE HACE UNA SUMA DEL TIEMPO TOTAL DE REALIZACION DE CADA DEPORTE EN LA CIUDAD DE AMSTERDAM

act_new2 <- act_new %>%
  filter(ciudad == "Europe/Amsterdam") %>%
  group_by(tipo) %>%
  summarize(tiempo_total = sum(dif))

act_new2

# AHORA SE MUESTRA EN UN GRAFICO LO MISMO QUE EN LA TABLA ANTERIOR

ggplot(data = act_new2) + geom_col(mapping = aes(x = tipo, y = tiempo_total))

# POR ULTIMO SE REALIZA UN FILTRADO PARA VER UNICAMENTE LOS DEPORTES CON MAS DE 2 HORAS DE REALIZACION TOTAL

act_new2 <- filter(act_new2, tiempo_total >= 120)
act_new2

ggplot(data = act_new2) + geom_col(mapping = aes(x = tipo, y = tiempo_total))

#10. Guardar el nuevo dataset en un archivo llamado  "act_new.csv"

write.csv(act_new, file = "act_new.csv")

#-------------------------------
#-----SEGUNDA PARTE-------------
# 11. Cargar el dataset sleep en un objeto llamado sleep

sleep <- read_csv("sleep.csv")
View(sleep)

#12. crear un nuevo data set llamado sleep_new que contenga solo las variables
#que contengan informacion, que no sean todo cero.

# SE SUMAN LOS VALORES DE LAS COLUMNAS Y SE ELIMINAN LOS NA, Y LAS COLUMNAS QUE SUMEN MAS DE 0 SE SELECCIONAN

sleep_new <- sleep[, colSums(sleep != 0, na.rm = TRUE) > 0]
View(sleep_new)

#13. Renombrar las variables de sleep_new a nombres cortos:

sleep_new <- rename(sleep_new, tiempo_en_dormir = 'Duration to sleep (s)')
sleep_new <- rename(sleep_new, tiempo_en_despertar = 'Duration to wake up (s)')
sleep_new <- rename(sleep_new, ligero = 'ligero (s)')
sleep_new <- rename(sleep_new, profundo = 'profundo (s)')
sleep_new <- rename(sleep_new, despierto = 'despierto (s)')
sleep_new <- rename(sleep_new, snoring = 'Snoring (s)')
View(sleep_new)

#14. Eliminar todas las filas que contengan algun NA

sleep_new <- na.omit(sleep_new)
summary(sleep_new) # PARA COMPROBAR SI HAY NA

# 15. Calcular cuanto tiempo en total se ha dormido cada noche: ligero+profundo

sleep_new <- mutate(sleep_new, tiempo_total_dormido = ligero + profundo)
View(sleep_new)

# 16. Visualizacion de la relacion ligero-profundo-total (SE ENFRENTAN POR PARES)

plot(sleep_new$ligero, sleep_new$profundo, main = "Relacionn sueño ligero y profundo", xlab = "Ligero", ylab = "Profundo")
plot(sleep_new$ligero, sleep_new$ttiempo_total_dormido, main = "Relacion sueño ligero y el total dormido", xlab = "Ligero", ylab = "Total dormido")
plot(sleep_new$profundo, sleep_new$tiempo_total_dormido, main = "Relacion sueño profundo y el total dormido", xlab = "Profundo", ylab = "Total dormido")

# A la vista de los resultados, que tipo de sue?o es mas relevante?
# 17. Realizar un analisis de diferencias entre los dos tipos de sue?o e interpretar los resultados
# usar la funci?n ICalpha o el 'One sample t-test' de TeachingDemos: t.test()

ICalpha <- function(ModeloA, ModeloB, alfa = 0.05)
{
  n <- length(ModeloA)
  diferencias <- ModeloA - ModeloB
  mediad <- mean(diferencias)
  #mediad2<-mean(diferencias^2)
  s <- sqrt(var(diferencias))
  #s<-sqrt(mediad2-mediad^2)
  valort <- qt(alfa / 2, n - 1, lower.tail = F)
  valor <- valort * s / sqrt(n)
  cotaInf <- mediad - valor
  cotaSup <- mediad + valor
  df <- data.frame(cotaInf, cotaSup)
  return(df)
}

ICalpha(sleep_new$ligero, sleep_new$profundo)

# EXISTE DIFERENCIA EN LAS MEDIAS. ADEMAS, AL ENCONTRARSE EL VALOR EN UN INTERVALO NEGATIVO,
# IMPLICA QUE EL SEGUNDO VALOR DE LA FUNCION ES EL QUE TIENE MAS RELEVANCIA.
# POR TANTO, EL 'SLEEP' PROFUNDO ES MAS RELEVANTE

#18. Crear una nueva variable 'ciudad' en sleep_new con la informacion de act_new.

# SE USA "SUBSTR" PARA QUEDARME SOLO CON EL AÑO, MES Y DIA DE CADA BASE DE DATOS

act_new <- mutate(act_new, date = substr(act_new$de, 1, 10))
View(act_new)

sleep_new <- mutate(sleep_new, date = substr(sleep_new$de, 1, 10))
View(sleep_new)
sleep_new

# SE REDUCE LA BASE DE DATOS ACT_NEW A LAS VARIABLES DATE, CIUDAD Y TIEMPO DEPORTE AL DIA

act_new <- act_new %>%
  group_by(date, ciudad) %>%
  summarize(tiempo_dep_day = sum(dif))

View(act_new)

# REDUCCION DE LA BASE DE DATOS SLEEP_NEW A LAS VARIABLES QUE INTERESAN A CONTINUACION

sleep_new <- select(sleep_new,-(1:3), -(5:9))
sleep_new <- mutate(sleep_new, profundo = profundo / 60) # PARA TENERLO EN MINUTOS
sleep_new <- mutate(sleep_new, tiempo_total_dormido = tiempo_total_dormido / 60) # PARA TENERLO EN MINUTOS
View(sleep_new)

# USO DE "INNER_JOIN" PARA JUNTAR LAS DOS BASES DE DATOS SEGUN LA VARIABLE EN COMUN "DATE"

sleep_new <- inner_join(act_new, sleep_new)
View(sleep_new)

#19. Representar la relaci?n totalsleep y profundo usando como facetas el factor ciudad

ggplot(data = sleep_new) + geom_point(mapping = aes(x = profundo, y = tiempo_total_dormido)) + facet_wrap(~ ciudad,nrow = 2)

#20. Guardar el dataset sleep_new en un archivo "sleep_new.csv"

write.csv(sleep_new, file = "sleep_new.csv")

#21. Guardar el proyecto completo. Subir la carpeta del proyecto al campus.