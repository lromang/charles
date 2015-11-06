#! /usr/bin/Rscript
## Codigo para obtener todas las ligas de datos.gob
##-----------------------------
## Librerias utilizadas
suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RGoogleAnalytics))
suppressPackageStartupMessages(library(data.table))
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------

##-----------------------------
## Funciones
##-----------------------------

##-----------------------------
## to.date
##-----------------------------
to.date <- function(fecha, hora){
    fecha <- str_split(fecha, " ")[[1]]
    months <- c("Enero","Febrero","Marzo",
                "Abril","Mayo","Junio","Julio",
                "Agosto","Septiembre","Octubre",
                "Noviembre","Diciembre")
    months.number <- c("01","02","03","04",
                       "05","06","07","08",
                       "09","10","11","12")
    month <- fecha[2]
    month <- months.number[months==month]
    date  <- paste(fecha[3],month,fecha[1],sep = "/")
    date_time <- paste(date,str_split(hora, " ")[[1]][1],sep = " ")
    as.POSIXlt(date_time)
}
##-----------------------------
## check.url
##-----------------------------
check.url <- function(url){
    ## Verifica si los datos de la url proporcionada pueden ser descargados.
    ## en caso afirmativo los descarga, de lo contrario manda un mensaje diciendo
    ## que se present un error o una alerta.
    ## IN
    ## url: la url donde estan localizados los datos.
    ## OUT
    ## un mensaje diciendo si hubo un error, una alerta o si se descargaron los datos.
    tmpFile <- tempfile()
    out <- tryCatch(
        {
            download.file(url, destfile = tmpFile, method = "curl")
        },
        error=function(cond) {
            message(paste("La URL", url," no funciona", sep = " "))
            return(NA)
        },
        warning=function(cond) {
            message(paste("La URL", url," despide una alerta", sep = " "))
            return(NA)
        },
        finally={
            message(paste("La URL:", url, "ha sido procesada", sep = " "))
            file.remove(tmpFile)
        }
        )
    return(out)
}
##------------------------------
## link.checker
##------------------------------
link.checker <- function(url){
    ## Las urls que quieres que cheque.
    test   <-
        ldply(url,
              function( t ){ u  <-
                  http_status( GET( t ) ) 
                             if(u$category != 'success'){
                                 index  <- which(url == t)
                                  browseURL(t)
                                 command1 <- paste0('import -window 0x03a00001 ./images/', index,'.jpeg')
                                 system(command1)
                                 ##                                 command2 <- paste0('okular ./images/', t, '.jpeg')
                                 ##                                system(command2)
                             }; u$category })
    data.frame(url = url, estado = test[,1])
}
##------------------------------
## get.links
##------------------------------
get.links <- function(url){
    ## Obtiene todas las ligas dentro de la pagina proporcionada.
    ## IN
    ## url: la direccion de la pagina de donde se quieren obtener las ligas.
    ## OUT
    ## arreglo que contiene todas las ligas de la pagina
    page  <- getURL(url)
    tree  <- htmlParse(page)
    links <- xpathApply(tree,
                        path = "//a",
                        fun = xmlGetAttr, 
                        name = "href")
    links <- unlist(links)
    links
}
##------------------------------
## get.links number
##------------------------------
get.links.number <- function(url, number){
    ## Obtiene todas las ligas de una pagina que es parte de un conjunto enumerado
    ## de paginas
    ## IN
    ## url: la direccion de la pagina de donde se quieren obtener las ligas
    ## number: el número de la pagina.
    ## OUT
    ## arreglo que contiene todas las ligas de la pagina
    base.url <- paste0(url,"?page=",number)
    links    <- get.links(base.url)
    links
}
##------------------------------
## get.links.inst
##------------------------------
get.all.links.inv <- function(url){
    ## Obtiene las ligas a los catalogos de las instituciones
    ## con el número proporcionado.
    ## IN
    ## url: la direccion de la pagina de la institucion
    ## de donde se quieren obtener las ligas
    ## number: el número de la pagina.
    ## OUT
    ## arreglo que contiene todas las ligas que van a instituciones de la pagina
    links        <- get.links.inv(url)
    length.links <- length(links)
    k            <- 1
    while(length.links > 0){
        k               <- k + 1
        new.links       <- get.links.inv(url,k)
        links           <- c(links, new.links)
        length.links    <- length(new.links)
    }
    links
}
##------------------------------
## get.all.links.new
##------------------------------
## Obtiene las ligas a todos los catalogos de las instituciones que los renovaron.
## IN
## url: la direccion de la pagina de la institucion
## de donde se quieren obtener las ligas
## number: el número de la pagina.
## OUT
## data.frame que contiene los nombres y ligas a catalogos de las instituciones
## que los modificaron.
get.all.links.new <- function(inventory, new.inventory){
    ## Vemos que instituciones ya estaban en el inventario
    index.in <- new.inventory$inst %in% inventory$inst
    ## Tomamos las instituciones que actualizaron y las que no estaban
    new.data <- 
        na.omit(rbind(new.inventory[inventory$fecha < new.inventory$fecha[index.in],],
                      new.inventory[!index.in,]))
    ## Obtenemos los datos de estas ligas.
    M        <- ddply(new.data,1,function(t){
                          links  <- get.all.links.inv(t$url)
                          inst   <- rep(t$inst,length(links))
                          data.frame(url = links, inst = inst )
                      })
    M
}
##------------------------------
## get.links.adela
##------------------------------
get.links.adela <- function(url, number){
    ## Obtiene las ligas de las instituciones que se encuentran en la pagina
    ## con el numero proporcionado.
    ## IN
    ## url: la direccion de la pagina de donde se quieren obtener las ligas
    ## number: el numero de la pagina.
    ## OUT
    ## arreglo que contiene todas las ligas que van a instituciones de la pagina
##############################################
    base.url <- paste0(url,"/?page=",number)
    page     <- getURL(base.url)
    tree     <- htmlParse(page)
    links    <- xpathApply(tree,
                        path = "//table[@class='expanded-table organizations-table']//a",
                        fun  = xmlGetAttr,
                        name = "href")
    links <- unlist(links)
    links
##############################################
    links <- paste0(url,links)
    links <- links[str_detect(links,"instituciones")]
    links
}
##------------------------------
## get.link.inv
##------------------------------
get.links.inv <- function(url, number = 1){
    ## Obtiene la liga que va al inventario de la institucion
    ## IN
    ## url: la direccion de la pagina de donde se quieren obtener las ligas
    ## OUT
    ## La liga al inventario donde estan las bases de la institucion
    url   <- paste0(url, "?page=", number)
    links <- get.links(url)
    links <- links[str_detect(links,"csv")]
    links
}
##------------------------------
## get.edit.inv
##------------------------------
get.edit.inv <- function(url){
    ## Obtiene la informacion de la ultima modificacion al inventario de datos
    ## de cada institucion.
    ## IN
    ## url: la direccion de la pagina de donde se quiere obtener la informacion
    ## OUT
    ## data.frame con la informacion de la ultima modificacion al inventario
    ## de datos de cada institucion.
    inst      <- str_split(url,"/")[[1]][5]
    url_cat   <- get.links.inv(url)[1]
    if(!is.na(url_cat)){
        page      <- getURL(url)
        tree      <- htmlParse(page)
        values    <- xpathApply(tree,
                                path = "//table[@class='expanded-table inventories-list']//td",
                                fun  = xmlValue)
        values    <- unlist(values)
        last.mod  <- data.frame(inst      = inst,
                                url       = url,
                                url_cat   = url_cat,
                                fecha     = to.date(values[3],values[4]),
                                conjuntos = values[5],
                                recursos  = values[6])
    }else{
        last.mod  <- data.frame(inst      = inst,
                                url       = url,
                                url_cat   = NA,
                                fecha     = NA,
                                conjuntos = NA,
                                recursos  = NA)
    }
    last.mod
}
##------------------------------
## create MAT
##------------------------------
## Funcion con codigo de Carlos Castro
create.new.MAT<- function(M){
    MAT <- NULL
    if(nrow(M)>0){
        inst <- unique(names(M))
        MAT <- data.frame(matrix(0,1,6))
        MAT <- data.frame(matrix(0,1,6))
        ##cont es el contador para saber la version del catalogo qa la que corresponde
        cont <- matrix(0,length(inst),1)
        indice_csv<-c()
        ##Guarda todas las i que si se pudo leer su csv
        ##(por ejemplo los de la SEP no se pueden)
        for(i in 1:dim(M)[1]){
            ##for(i in 1:2){
            cont[which(inst==M[i,2])]<-cont[which(inst==M[i,2])]+1
            ##T <- read.csv(text = getURL(M[i,1]))
            T<-try(read.csv(text = getURL(M[i,1],.encoding = 'UTF-8')),1)
            if (class(T)=="data.frame"){
                vec<-which(T$rs.title!="")
                ##A<-data.frame(matrix(0,length(vec),6))
                A<-data.frame(matrix("",dim(T)[1],6))
                A[,1]<-M[i,2]
                if(is.null(T$rs.title)==0){
                    A[,2]<-T$rs.title
                }
                if(is.null(T$rs.downloadURL)==0){
                    A[,3]<-T$rs.downloadURL
                }
                if(is.null(T$rs.mediaType)==0){
                    A[,4]<-T$rs.mediaType
                }
                if(is.null(T$rs.byteSize)==0){
                    A[,5]<-T$rs.byteSize
                }
                A[,6]<- matrix(cont[which(inst==M[i,2])],dim(T)[1],1)
                A    <- A[vec,]
                MAT  <- rbind(MAT,A)
                indice_csv <- rbind(indice_csv,i)
            }
        }
        MAT <- data.frame(MAT[2:dim(MAT)[1],],row.names = NULL)
        colnames(MAT) <- c("Institucion","Nombre","URL","Tipo","Bytes","Version")
        MAT$Version <- 1
    }
    MAT
}
##------------------------------
## act.MAT
##------------------------------
## Junta las dos Matrices construidas y actualiza las
## versiones correspondientes.
act.MAT <- function(MAT, new.MAT){
    MAT$Version[paste0(MAT$Nombre,MAT$Institucion)  %in%
                    paste0(new.MAT$Nombre,new.MAT$Institucion)] <- 
                        MAT$Version[paste0(MAT$Nombre,MAT$Institucion)  %in%
                                        paste0(new.MAT$Nombre,new.MAT$Institucion)] + 1
    MAT <- rbind(MAT, new.MAT)
    MAT
}
##------------------------------
## inventory.data
##------------------------------
inventory.data <- function(url="http://adela.datos.gob.mx"){
    ## Recorre e inspecciona todas las ligas de las bases
    ## de datos de las instituciones presentes en Adela
    ## IN
    ## url: la direccion de la pagina de donde se quieren obtener las ligas
    ## OUT
    ## lista que contiene los resultados de todas las pruebas.
    ##------------------------------------------------------------
    ## En esta seccion se obtienen todas las ligas a todas las instituciones
    ## que estan en adela. Y las ligas a los inventarios de las mismas.
    count.adela  <- 1
    links.adela  <- get.links.adela( url, count.adela )
    length.links <- length(links.adela)
    ## En este ciclo se obtienen todas las instituciones que estan
    ## dentro de Adela.
    while( length.links > 0 ){
        count.adela     <- count.adela + 1
        new.links.adela <- get.links.adela( url, count.adela )
        links.adela     <- c(links.adela,  new.links.adela )
        length.links    <- length(new.links.adela)
    }
    ## Datos relevantes de la ultima modificacion que se le hizo a los
    ## inventarios de datos de todas las instituciones.
    edit.inv     <- ldply(links.adela, function(t)t <- get.edit.inv(t))
    ## Todas las versiones de inventarios de todas las instituciones
    edit.inv
}
##------------------------------
## workflow.
##------------------------------
workflow <- function(A){
    ## Calcula MAT tomando en consideracion unicamente aquellas dependencias
    ## que tienen actualizaciones.
    ## IN
    ## la matriz de Análisis.
    ## OUT
    ## Mat actualizada.
    ##------------------------------------------------------------
    ## LECTURA DE DATOS
    ## Obtenemos la version pasada de MAT.
    MAT           <-
        read.csv("./data/MAT.csv",
                 stringsAsFactors = FALSE)
    ## Obtenemos la version pasada del inventario.
    inventory     <-
        read.csv("./data/inventory.csv",
                 stringsAsFactors = FALSE)
    ## ULTIMAS VERSIONES
    ## Obtenemos la version nueva del inventario.
    new.inventory <- inventory.data()
    ## Obtenemos las ligas a los catalogos de las
    ## instituciones que los actualizaron.
    link.cat.act  <- get.all.links.new(inventory, new.inventory)
    ## Creamos la nueva MAT
    new.MAT       <- create.new.MAT(link.cat.act)
    ## INTEGRAR ANALYTICS
    ## Agregamos analytics a MAT
    new.MAT.an    <- mat.analytics(new.MAT, A)
    ## ACTUALIZAR
    ## Actualizamos a MAT
    MAT.act       <- act.MAT(MAT, new.MAT.an)
    ## Agregamos analytics a inventory
    new.inventory <- inventory.MAT(new.inventory, MAT.act)
    ## GUARDAR CAMBIOS
    if(nrow(new.inventory) > 0 & nrow(new.MAT) > 0){
        write.csv(new.inventory,
                  "./data/inventory.csv",
                  row.names = FALSE)
        write.csv(MAT.act,
                  "./data/MAT.csv",
                  row.names = FALSE)
    }
    ## IMPRIMIR RESULTADOS
    resultados <- data.frame(fecha_prueba = date(),
                             catalogos_agregados = nrow(new.inventory) - nrow(inventory),
                             bases_agregadas     = nrow(MAT.act) - nrow(MAT),
                             inst_actualizadas   = length(unique(link.cat.act$inst))
                             )
    print(resultados)
}
##------------------------------
## get.termin
##------------------------------
get.termin <- function(url){
    ## Determina la terminación del archivo dentro de la url
    ## especificada.
    ## IN
    ## url del archivo
    ## OUT
    ## terminación del archivo.
    ##------------------------------------------------------------
    ## Obtenemos la version pasada de MAT.
    type    <- str_replace(str_match(str_sub(url,
                                             str_length(url) - 5,
                                             -1),
                                     "\\.{1}[a-z]*"),".","")
    type
}
##------------------------------
## check.all
##------------------------------
check.all <- function(MAT){
    ## Determina si las ligas de mat funcionan,
    ## si los archivos están comprimidos o no,
    ## el tipo de archivo, el peso y aquí es donde
    ## se hará revisión de calidad de fechas, coordenadas,
    ## entidades federativas, etc.
    ## IN
    ## La matriz MAT
    ## OUT
    ## una lista con todas las características previamente
    ## especificadas.
    ##------------------------------------------------------------
    ## Obtenemos la version pasada de MAT.
    results <- list()
    works   <- link.checker(MAT$URL)$estado
    for(i in 1:nrow(MAT)){
        result  <- list(
            inst       = "",
            url        = "",
            disp       = "", ## si el servidor está funcionando
            tipo.pred  = "", ## tipo de terminación del archivo
            tipo.rel   = "", ## tipo de terminación del archivo
            comprimido = "", ## booleano si el archivo está comprimido
            n.archivos = "", ## numero de archivos
            arch.comp  = "",
            tipo.comp  = "") ## tipo de archivos que estan en el zip
        ## Llenar result
        result$inst          <- MAT$Institucion[i]
        result$url           <- MAT$URL[i]
        result$disp          <- works[i]
        result$tipo.pred     <- get.termin(result$url)[1]
        result$tipo.rel      <- MAT$Tipo[i]
        result$comprimido    <- result$tipo.pred %in% c("rar","zip")
        if(result$disp == 'success'){
            result$n.archivos <- as.numeric(!result$comprimido)
        }
        if(result$comprimido == TRUE & result$disp == 'success'){
            system(paste0('wget -O tmp.tmp ',result$url ))
            if(result$tip.pred == "zip"){
                system('zipinfo -1 tmp.tmp > files.txt')
            }else{
                system('unrar -Z -l tmp.tmp > files.txt')
            }
            archivos.comp        <- readLines('files.txt')
            result$arch.comp     <- archivos.comp
            result$n.archivos    <- length(archivos.comp)
            result$tipo.comp     <- get.termin(archivos.comp)
            system('rm  tmp.tmp files.txt')
        }
        results[[i]] <- result
    }
    results
}
##------------------------------
## mat.analytics
##------------------------------
mat.analytics <- function(MAT, A){
    A$eventLabel  <- ldply(A$eventLabel, function(t)t <- URLdecode(t))[,1]
    MAT$descargas <- ldply(MAT$URL,function(t){
                               if(t %in% A$eventLabel){
                                   t <- sum(A$totalEvents[t == A$eventLabel])
                               }else{
                                   t <- 0
                               }
                               t
                           })[,1]
    no.match <- A[! A$eventLabel %in% MAT$URL, ]
    write.csv(no.match,
              "./data/no_match_mat.csv",
              row.names = FALSE)
    MAT
}
##------------------------------
## inventory.analytics
##------------------------------
inventory.MAT <- function(inventory, MAT){
    inventory$descargas <- NULL
    mat.1 <- filter(MAT, Version == 1)
    mat.1 <- data.table(mat.1)
    setkey(mat.1, Institucion)
    descargas <- mat.1[, sum(descargas), by = Institucion]
    setnames(descargas,names(descargas), c("inst","descargas"))
    merge(descargas, inventory)
}
#############################################################################
################################### PRUEBA ##################################
#############################################################################
## Lectura de datos  y  modificación de formato
MAT   <- read.csv("./data/MAT.csv",
                  stringsAsFactors = FALSE,
                  encoding = "UTF-8")
inventory <- read.csv("./data/inventory.csv",
                      stringsAsFactors = FALSE)
A     <- read.csv("./data/A.csv",
                  stringsAsFactors = FALSE,
                  encoding = "UTF-8")
## La idea es hacer que MAT haga match con A y con inventory.

