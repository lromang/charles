#!/usr/bin/Rscript
## Código que checa si las ligas de los servidores están activas.
## Librerías utilizadas
library(httr)
library(plyr)
## Lectura de datos (muestras de 15)
data   <- read.csv( "../../data/MAT.csv", stringsAsFactors = FALSE )
sample <- sample(nrow(data),100)
url    <- data$URL
samp   <- url[sample]
## Prueba
ldply(samp, function( t ){ u <-
                    http_status( GET( t ) )
                    if(u$category != "success"){
                    	print(which(url==t)[1])
                    }
                } )

