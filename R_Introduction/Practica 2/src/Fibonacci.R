# AUTOR: MIGUEL LOPEZ GARRALON
# SELECCION DE N TERNAS PITAGORICAS DENTRO DE LA SECUENCIA DE FIBONACCI 

library(readr)
library(rmarkdown)
library(tidyverse)
library(DescTools)

fibonacci <- Fibonacci(0:30)
fibonacci

nternas <- function(n){
  t <- 0
  for (i in 2:length(fibonacci)) {
    primer_numero = fibonacci[i]
    segundo_numero = fibonacci[i + 1]
    tercer_numero = fibonacci[i + 2]
    cuarto_numero = fibonacci[i + 3]
    cateto_1 = primer_numero * cuarto_numero
    cateto_2 = 2 * segundo_numero * tercer_numero
    hipotenusa = segundo_numero ^ 2 + tercer_numero ^ 2
    if (cateto_1 ^ 2 + cateto_2 ^ 2 == hipotenusa ^ 2) {
      t <- t + 1
      print(c(cateto_1, cateto_2, hipotenusa))
    }
    if (t == n) { break }
  }
}

nternas(3)
nternas(9)

