# AUTOR: MIGUEL LOPEZ GARRALON
# FECHA: 24/09/2019
# EJERCICIOS CON LA BASE DE DATOS DE NOMBRES DE BEBES NACIDOS EN ONTARIO

setwd("C:/Users/migue/Desktop/CUNEF/ProgramacionConR/Practica_2")

library(readr)
ontario_top_baby_names_male_1917_2016_english <- read_csv("C:/Users/migue/Desktop/CUNEF/ProgramacionConR/Practica_2_clase_ontario/ontario_top_baby_names_male_1917-2016_english.csv", skip = 1)


babymales <- ontario_top_baby_names_male_1917_2016_english
rm(ontario_top_baby_names_male_1917_2016_english)

# VEMOS LOS PRIMEROS Y ULTIMOS 20 CASOS DE LA BASE DE DATOS

head(babymales, 20)
tail(babymales, 20)



str(babymales)
dim(babymales)

max(babymales$Year)

last_year <- subset(babymales, babymales$Year == max(babymales$Year))

# COMPROBAMOS QUE SOLO HAY CASOS DEL 2016 ()

head(last_year)
tail(last_year)

# SUMA DE TODOS LOS BEBES NACIDOS EN 2016

sum(last_year$Frequency)

last_year <- last_year[, c("Name", "Frequency")]
head(last_year)

last_year <- last_year[order(last_year$Frequency, decreasing = TRUE),]
# SE PIDE LOS PRIMEROS 5 CASOS Y QUE CONVIERTA LA COLUMNA A TIPO CHARACTER
head(as.character(last_year$Name),5)

# SE GUARDA LA NUEVA BASE DE DATOS EN EL DIRECTORIO ASIGNADO

write.csv(last_year, file = "2016_male_popular.csv", row.names = FALSE)

# SE PUEDE CONSULTAR EL NOMBRE QUE SE QUIERA

consulta_nombre <- readline(prompt = "Male baby name? :")

# SE PASA EL NOMBRE A MAYUSCULAS
frecuencia_nombre <- babymales[babymales$Name == toupper(consulta_nombre), c("Year", "Frequency")]
frecuencia_nombre

# DEVUELVE LA SUMA DE LA FRECUENCIA TOTAL DE LOS BEBES CON DICHO NOMBRE

sum(frecuencia_nombre$Frequency)

plot.tittle <- paste("Babies named", toupper(consulta_nombre))
g <- plot(frecuencia_nombre, main = plot.tittle, type = 's')

