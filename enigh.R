
## ETL ENIGH ##
## Alvaro Martinez Barron ##

######################## setting directory ##########################
old_wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste(gsub("\\/code*","",old_wd), "databases", sep = "/"))
#####################################################################


## installing packages ##
requiredpackages <- c('readxl', 'openxlsx', 'foreign', 'dplyr',  
                      'stringr', 'janitor', 'tibble', 'lubridate', 
                      'zoo', 'data.table', 'siebanxicor', 'forecast', 'svMisc')

install_load <- function(packages){
  for (p in packages) {
    if (p %in% rownames(installed.packages())) {
      library(p, character.only=TRUE)
    } else {
      install.packages(p)
      library(p,character.only = TRUE)
    }
  }
}

install_load(requiredpackages)



# Reading files ----

files <- list.files(pattern = ".dbf")


pb <- winProgressBar(title = "progress bar", min = 0,
                     max = length(files), width = 300)


for (i in seq_along(files)) {
  
  assign(files[i] , read.dbf(files[i]))
  
  
  setWinProgressBar(pb, i, title=paste( round(i/length(files)*100, 0),
                                        "% done"))
}

close(pb)


## changing data frame name columns to lower case ##

df_list <- Filter(function(x) is(x, "data.frame"), mget(ls()))


for(i in 1:length(df_list)){
        colnames(df_list[[i]])<- tolower(colnames(df_list[[i]]))
} 


## unlisting list to dfs ##
list2env(df_list, envir = .GlobalEnv)

## working with population characteristics dataset ##

## decoding categories ##

pop_list <- list(POBLA84.dbf= POBLA84.dbf, 
                 POBLA89.dbf= POBLA89.dbf, 
                 POBLA92.dbf= POBLA92.dbf, 
                 POBLA94.dbf= POBLA94.dbf,
                 POBLA96.dbf= POBLA96.dbf, 
                 POBLA98.dbf= POBLA98.dbf, 
                 POBLA00.dbf= POBLA00.dbf,
                 POBLA02.dbf= POBLA02.dbf, 
                 POBLA04.dbf= POBLA04.dbf,
                 POBLA05.dbf= POBLA05.dbf, 
                 POBLA06.dbf= POBLA06.dbf, 
                 POBLA08.dbf= POBLA08.dbf, 
                 POBLA10.dbf= POBLA10.dbf,
                 POBLA12.dbf= POBLA12.dbf, 
                 POBLA14.dbf= POBLA14.dbf, 
                 POBLA16.dbf= POBLA16.dbf, 
                 POBLA18.dbf= POBLA18.dbf
                )


## sex ##

## IFELSE( , IFELSE(, IFELSE,
                         #IFELSE))


sex_function <- function(x){
             if(is.na(x)){
               return(NA)
             } else if(x == "1"){
               return("Masculino")
             } else if(x == "2"){
               return("Femenino")
             } else {
               return("No especificado")
             }
}

#Apply decodification loop for sex column ----

pb <- winProgressBar(title = "progress bar", min = 0,
                     max = length(pop_list), width = 300)

for (i in 1:length(pop_list)) {
  
    pop_list[[i]][["sexo"]] <-  as.factor(do.call( rbind,
                                            lapply(as.factor(pop_list[[i]][["sexo"]]), 
                                               sex_function)))
    
    
    setWinProgressBar(pb, i, title=paste( round(i/length(pop_list)*100, 0),
                                          "% done"))
  
}

close(pb)

## unlist df's into global environment ##

for (i in names(pop_list)) {
  assign(i, pop_list[[i]], .GlobalEnv)
}


## education ##
## 1984 has distinct class fun_ed_1984 is built for 1984 dataset

fun_ed_1984 <- function(x){
  if(is.na(x)){
    return(NA)
  } else if(x== "00"){
    return("Sin Instrucción")
  } else if(x == "01"){
    return("Primaria Incompleta")
  } else if(x == "02"){
    return("Primaria Completa")
  } else if(x == "03"){
    return("Secundaria Completa")
  } else if(x == "04"){
    return("Secundaria Incompleta")
  } else if(x == "05"){
    return("Secundaria Completa")
  } else if(x == "06"){
    return("Preparatoria Incompleta")
  } else if(x == "07"){
    return("Preparatoria Completa")
  } else if(x == "08"){
    return("Universidad Incompleta")
  } else if(x=="09"){
    return("Universidad Completa")
  } else if(x == "10"){
    return("Postgrado")
  } else {
    return("Desconocido")
  }
}



POBLA84.dbf$ed_formal <- as.factor(do.call(rbind, lapply(as.factor(POBLA84.dbf$ed_formal),
                                      fun_ed_1984)))


## excluding 84 as it was already decoded for education

pop_list <- list(POBLA89.dbf= POBLA89.dbf, 
                 POBLA92.dbf= POBLA92.dbf, 
                 POBLA94.dbf= POBLA94.dbf,
                 POBLA96.dbf= POBLA96.dbf, 
                 POBLA98.dbf= POBLA98.dbf, 
                 POBLA00.dbf= POBLA00.dbf,
                 POBLA02.dbf= POBLA02.dbf, 
                 POBLA04.dbf= POBLA04.dbf,
                 POBLA05.dbf= POBLA05.dbf, 
                 POBLA06.dbf= POBLA06.dbf, 
                 POBLA08.dbf= POBLA08.dbf, 
                 POBLA10.dbf= POBLA10.dbf,
                 POBLA12.dbf= POBLA12.dbf, 
                 POBLA14.dbf= POBLA14.dbf, 
                 POBLA16.dbf= POBLA16.dbf, 
                 POBLA18.dbf= POBLA18.dbf
)

## setting same column name for education column ##

pop_list <- lapply(pop_list, function(x) setNames(x, 
                                              gsub("^nivel$", "ed_formal", 
                                                                 names(x))) )

pop_list <- lapply(pop_list, function(x) setNames(x, 
                                                  gsub("^nivelaprob$", "ed_formal", 
                                                                 names(x))) )
## changing folioviv to folio ##
pop_list <- lapply(pop_list, function(x) setNames(x, 
                                                  gsub("^folioviv$", "folio", 
                                                       names(x))) )

fun_ed <- function(x){
             if(is.na(x)){
               return(NA)
             } else if(x== "00" | x== "0"){
               return("Sin Instrucción")
             } else if(x == "01"| x== "1"){
               return("Primaria Incompleta")
             } else if(x == "02"| x== "2"){
               return("Primaria Completa")
             } else if(x == "03"| x == "3"){
               return("Secundaria Incompleta")
             } else if(x == "04"| x == "4"){
               return("Secundaria Completa")
             } else if(x == "05"| x== "5"){
               return("Preparatoria Incompleta")
             } else if(x == "06"| x== "6"){
               return("Preparatoria Completa")
             } else if(x == "07"| x== "7"){
               return("Universidad Incompleta")
             } else if(x == "08"| x=="8"){
               return("Universidad Completa")
             } else if(x == "09"| x== "9"| x=="10"){
               return("Postgrado")
             } else {
               return("Desconocido")
             }
}


pb <- winProgressBar(title = "progress bar", min = 0,
                     max = length(pop_list), width = 300)


for (i in 1:length(pop_list)) {
  
  pop_list[[i]][["ed_formal"]] <- as.factor(do.call(
                                    rbind, 
                                    lapply(as.factor(pop_list[[i]][["ed_formal"]]), 
                                     fun_ed)))
  
  setWinProgressBar(pb, i, title=paste( round(i/length(pop_list)*100, 0),
                                        "% done"))
  
}

close(pb)

## unlist df's into global environment ##

for (i in names(pop_list)) {
  assign(i, pop_list[[i]], .GlobalEnv)
}


# joining expansion factor over 1984-2006 #
# and education level and sex for 1984-2018 datasets ##

conce_list <- list(CONCE84.dbf= CONCE84.dbf, 
                   CONCE89.dbf= CONCE89.dbf, 
                   CONCE92.dbf= CONCE92.dbf, 
                   CONCE94.dbf= CONCE94.dbf,
                   CONCE96.dbf= CONCE96.dbf, 
                   CONCE98.dbf= CONCE98.dbf, 
                   CONCE00.dbf= CONCE00.dbf, 
                   CONCE02.dbf= CONCE02.dbf,
                   CONCE04.dbf= CONCE04.dbf, 
                   CONCE05.dbf= CONCE05.dbf, 
                   CONCE06.dbf= CONCE06.dbf, 
                   CONCE08.dbf= CONCE08.dbf,
                   CONCE10.dbf= CONCE10.dbf, 
                   CONCE12.dbf= CONCE12.dbf, 
                   CONCE14.dbf= CONCE14.dbf, 
                   CONCE16.dbf= CONCE16.dbf,
                   CONCE18.dbf= CONCE18.dbf)

############################### setting keys ################################

conce_list <- lapply(conce_list, function(x) setNames(x, 
                                                  gsub("^folioviv$", "folio", 
                                                       names(x))) )

##############################################################################

## removing duplicates ##

for(i in 1:length(conce_list)){
  conce_list[[i]][["ed_formal"]]<- NULL
  conce_list[[i]][["sexo"]]<- NULL
}


for (i in names(conce_list)) {
  assign(i, conce_list[[i]], .GlobalEnv)
}

#### RUN ONE TIME ###
## WARNING: if more than one then code breaks. ##

POBLA10.dbf[,41]<- NULL
POBLA12.dbf[,41]<- NULL
POBLA14.dbf[,41]<- NULL
POBLA16.dbf[,41]<- NULL
POBLA18.dbf[,41]<- NULL
#####################################
## JOINS ##
## from population datasets to concentrate datasets ##

## dplyr: left_join, right_joins, anti_joins, inner_join

CONCE84.dbf <- CONCE84.dbf %>%
                    left_join(dplyr :: select(POBLA84.dbf, folio, sexo, ed_formal), 
                              by = "folio")

CONCE89.dbf <- CONCE89.dbf %>%
                 left_join(dplyr :: select(POBLA89.dbf, folio, sexo, ed_formal), 
                           by = "folio")

CONCE92.dbf <- CONCE92.dbf %>%
                 left_join(dplyr :: select(POBLA92.dbf, folio, sexo, ed_formal), 
                           by = "folio")

CONCE94.dbf <- CONCE94.dbf %>%
  left_join(dplyr :: select(POBLA94.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE96.dbf <- CONCE96.dbf %>%
  left_join(dplyr :: select(POBLA96.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE98.dbf <- CONCE98.dbf %>%
  left_join(dplyr :: select(POBLA98.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE00.dbf <- CONCE00.dbf %>%
  left_join(dplyr :: select(POBLA00.dbf, folio, sexo, ed_formal), 
            by = "folio")


CONCE02.dbf <- CONCE02.dbf %>%
  left_join(dplyr :: select(POBLA02.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE04.dbf <- CONCE04.dbf %>%
  left_join(dplyr :: select(POBLA04.dbf, folio, sexo, ed_formal), 
            by = "folio")


CONCE05.dbf <- CONCE05.dbf %>%
  left_join(dplyr :: select(POBLA05.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE06.dbf <- CONCE06.dbf %>%
  left_join(dplyr :: select(POBLA06.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE08.dbf <- CONCE08.dbf %>%
  left_join(dplyr :: select(POBLA08.dbf, folio, sexo, ed_formal), 
            by = "folio")


CONCE10.dbf <- CONCE10.dbf %>%
  left_join(dplyr :: select(POBLA10.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE12.dbf <- CONCE12.dbf %>%
  left_join(dplyr :: select(POBLA12.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE14.dbf <- CONCE14.dbf %>%
  left_join(dplyr :: select(POBLA14.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE16.dbf <- CONCE16.dbf %>%
  left_join(dplyr :: select(POBLA16.dbf, folio, sexo, ed_formal), 
            by = "folio")

CONCE18.dbf <- CONCE18.dbf %>%
  left_join(dplyr :: select(POBLA18.dbf, folio, sexo, ed_formal), 
            by = "folio")


## from homes dataset ##

CONCE84.dbf <- CONCE84.dbf %>%
  left_join(dplyr :: select(H84.dbf, folio, factor), by = "folio")

CONCE89.dbf <- CONCE89.dbf %>%
  left_join(dplyr :: select(H89.dbf, folio, factor), by = "folio")

CONCE92.dbf <- CONCE92.dbf %>%
  left_join(dplyr :: select(H92.dbf, folio, factor), by = "folio")

CONCE94.dbf <- CONCE94.dbf %>%
  left_join(dplyr :: select(H94.dbf, folio, factor), by = "folio")

CONCE96.dbf <- CONCE96.dbf %>%
  left_join(dplyr :: select(H96.dbf, folio, factor), by = "folio")

CONCE98.dbf <- CONCE98.dbf %>%
  left_join(dplyr :: select(H98.dbf, folio, factor), by = "folio")

CONCE00.dbf <- CONCE00.dbf %>%
  left_join(dplyr :: select(H00.dbf, folio, factor), by = "folio")

CONCE02.dbf <- CONCE02.dbf %>%
  left_join(dplyr :: select(H02.dbf, folio, factor), by = "folio")

CONCE04.dbf <- CONCE04.dbf %>%
  left_join(dplyr :: select(H04.dbf, folio, factor), by = "folio")

CONCE05.dbf <- CONCE05.dbf %>%
  left_join(dplyr :: select(H05.dbf, folio, factor), by = "folio")

CONCE06.dbf <- CONCE06.dbf %>%
  left_join(dplyr :: select(H06.dbf, folio, factor), by = "folio")



## working with concentrado (mix variables enigh) ##

conce_list <- list(CONCE84.dbf= CONCE84.dbf, 
                   CONCE89.dbf= CONCE89.dbf, 
                   CONCE92.dbf= CONCE92.dbf, 
                   CONCE94.dbf= CONCE94.dbf,
                   CONCE96.dbf= CONCE96.dbf, 
                   CONCE98.dbf= CONCE98.dbf, 
                   CONCE00.dbf= CONCE00.dbf, 
                   CONCE02.dbf= CONCE02.dbf,
                   CONCE04.dbf= CONCE04.dbf, 
                   CONCE05.dbf= CONCE05.dbf, 
                   CONCE06.dbf= CONCE06.dbf, 
                   CONCE08.dbf= CONCE08.dbf,
                   CONCE10.dbf= CONCE10.dbf, 
                   CONCE12.dbf= CONCE12.dbf, 
                   CONCE14.dbf= CONCE14.dbf, 
                   CONCE16.dbf= CONCE16.dbf,
                   CONCE18.dbf= CONCE18.dbf)

## function to extract state function 

state_code_fun <- function(x){
                     if(is.na(x)){
                       return(NA)
                     } else {
                       substr(x, 
                              start = 1,
                              stop  = 2)
                     }
  
}

## extracting state code ##
pb <- winProgressBar(title = "progress bar", min = 0,
                     max = length(conce_list), width = 300)

for (i in 1:length(conce_list)) {
  
  conce_list[[i]][["ubica_geo"]] <- as.factor(
    do.call(
    rbind, 
    lapply(as.factor(conce_list[[i]][["ubica_geo"]]), state_code_fun)))
  
  setWinProgressBar(pb, i, title=paste( round(i/length(conce_list)*100, 0),
                                        "% done"))
  
}

close(pb)

## function to decode state to string ##
state_fun <- function(x){
                 if(is.na(x)){
                   return(NA)
                 } else if(x == '01'){
                   return("Aguascalientes")
                 } else if(x == '02'){
                   return("Baja California")
                 } else if(x == '03'){
                   return("Baja California Sur")
                 } else if(x == '04'){
                   return("Campeche")
                 } else if(x == '05'){
                   return('Coahuila de Zaragoza')
                 } else if(x == '06'){
                   return("Colima")
                 } else if(x == '07'){
                   return("Chiapas")
                 } else if(x == '08'){
                   return("Chihuahua")
                 } else if(x == '09'){
                   return("Distrito Federal")
                 } else if(x == '10'){
                   return("Durango")
                 } else if(x == '11'){
                   return("Guanajuato")
                 } else if(x == '12'){
                   return("Guerrero")
                 } else if(x == '13'){
                   return("Hidalgo")
                 } else if(x == '14'){
                   return("Jalisco")
                 } else if(x == '15'){
                   return("Estado de Mexico")
                 } else if(x == '16'){
                   return("Michoacán de Ocampo")
                 } else if(x == '17'){
                   return("Morelos")
                 } else if(x == '18'){
                   return("Nayarit")
                 } else if(x == '19'){
                   return("Nuevo León")
                 } else if(x == '20'){
                   return("Oaxaca")
                 } else if(x == '21'){
                   return("Puebla")
                 } else if(x == '22'){
                   return("Querétaro de Arteaga")
                 } else if(x == '23'){
                   return("Quintana Roo")
                 } else if(x == '24'){
                   return("San Luis Potosí")
                 } else if(x == '25'){
                   return("Sinaloa")
                 } else if(x == '26'){
                   return("Sonora")
                 } else if(x == '27'){
                   return("Tabasco")
                 } else if(x == '28'){
                   return("Zacatecas")
                 } else if(x == '29'){
                   return("Tlaxcala")
                 } else if(x == '30'){
                   return("Veracruz de Ignacio de la Llave")
                 } else if(x == '31'){
                   return("Yucatán")
                 } else if(x == '32'){
                   return("Zacatecas")
                 } else {
                   return("Otro")
                 }
}

## changing to string value ##
pb <- winProgressBar(title = "progress bar", min = 0,
                     max = length(conce_list), width = 300)

for (i in 1:length(conce_list)) {
  
  conce_list[[i]][["ubica_geo"]] <- as.factor(do.call(
    rbind, 
    lapply(
      as.factor(conce_list[[i]][["ubica_geo"]]), 
      state_fun)))
  
  setWinProgressBar(pb, i, title=paste( round(i/length(conce_list)*100, 0),
                                        "% done"))
  
}

close(pb)

## unlisting list to dfs ##
for (i in names(conce_list)) {
  assign(i, conce_list[[i]], .GlobalEnv)
}


## adding date for each df ##

CONCE84.dbf$año <- "1984"
CONCE89.dbf$año <- "1989"
CONCE92.dbf$año <- "1992"
CONCE94.dbf$año <- "1994"
CONCE96.dbf$año <- "1996"
CONCE98.dbf$año <- "1998"
CONCE00.dbf$año <- "2000"
CONCE02.dbf$año <- "2002"
CONCE04.dbf$año <- "2004"
CONCE05.dbf$año <- "2005"
CONCE06.dbf$año <- "2006"
CONCE08.dbf$año <- "2008"
CONCE10.dbf$año <- "2010"
CONCE12.dbf$año <- "2012"
CONCE14.dbf$año <- "2014"
CONCE16.dbf$año <- "2016"
CONCE18.dbf$año <- "2018"



## selecting relevant columns ##
conce_list <- list(CONCE84.dbf= CONCE84.dbf, 
                   CONCE89.dbf= CONCE89.dbf, 
                   CONCE92.dbf= CONCE92.dbf, 
                   CONCE94.dbf= CONCE94.dbf,
                   CONCE96.dbf= CONCE96.dbf, 
                   CONCE98.dbf= CONCE98.dbf, 
                   CONCE00.dbf= CONCE00.dbf, 
                   CONCE02.dbf= CONCE02.dbf,
                   CONCE04.dbf= CONCE04.dbf, 
                   CONCE05.dbf= CONCE05.dbf, 
                   CONCE06.dbf= CONCE06.dbf, 
                   CONCE08.dbf= CONCE08.dbf,
                   CONCE10.dbf= CONCE10.dbf)


list2env(setNames(lapply(conce_list, `[`, c("año", "ubica_geo" ,"gasnom", "gasmon", "gascor",
                                         "tam_hog", "hombres", "mujeres",
                                         "mayores", "menores", 
                                         "educacion", "salud", "alimentos", "factor",
                                         "ed_formal", "sexo")),
                  c("CONCE84.dbf", "CONCE89.dbf", "CONCE92.dbf", "CONCE94.dbf",
                    "CONCE96.dbf", "CONCE98.dbf", "CONCE00.dbf", "CONCE02.dbf",
                    "CONCE04.dbf", "CONCE05.dbf", "CONCE06.dbf", "CONCE08.dbf",
                    "CONCE10.dbf"
                    )), .GlobalEnv)


### changing col names and selecting them ##
conce_list <- list( CONCE12.dbf= CONCE12.dbf, 
                     CONCE14.dbf= CONCE14.dbf, 
                     CONCE16.dbf= CONCE16.dbf,
                     CONCE18.dbf= CONCE18.dbf)

conce_list <- lapply(conce_list, function(x) setNames(x, gsub("^gasto_mon$", "gasmon", names(x))) )
conce_list <- lapply(conce_list, function(x) setNames(x, gsub("^gasto_nom$", "gasnom", names(x))) )
conce_list <- lapply(conce_list, function(x) setNames(x, gsub("^gasto_cor$", "gascor", names(x))) )
conce_list <- lapply(conce_list, function(x) setNames(x, gsub("^tot_integ$", "tam_hog", names(x))) )
conce_list <- lapply(conce_list, function(x) setNames(x, gsub("^otros_gas$", "gasnom", names(x))) )
conce_list <- lapply(conce_list, function(x) setNames(x, gsub("^ing_cor$",    "gascor", names(x))) )
conce_list <- lapply(conce_list, function(x) setNames(x, gsub("^factor_hog$", "factor", names(x))) )


list2env(setNames(lapply(conce_list, `[`, c("año", "ubica_geo"  ,"gasnom", "gasmon", "gascor",
                                         "tam_hog", "hombres", "mujeres",
                                         "mayores", "menores", 
                                         "educacion", "salud", "alimentos", "factor", "ed_formal",
                                         "sexo")),
                  c("CONCE12.dbf", "CONCE14.dbf", "CONCE16.dbf", "CONCE18.dbf"
                  )), .GlobalEnv)


## adding quantile (10) column ##

conce_list <- list(CONCE84.dbf= CONCE84.dbf, 
                   CONCE89.dbf= CONCE89.dbf, 
                   CONCE92.dbf= CONCE92.dbf, 
                   CONCE94.dbf= CONCE94.dbf,
                   CONCE96.dbf= CONCE96.dbf, 
                   CONCE98.dbf= CONCE98.dbf, 
                   CONCE00.dbf= CONCE00.dbf, 
                   CONCE02.dbf= CONCE02.dbf,
                   CONCE04.dbf= CONCE04.dbf, 
                   CONCE05.dbf= CONCE05.dbf, 
                   CONCE06.dbf= CONCE06.dbf, 
                   CONCE08.dbf= CONCE08.dbf,
                   CONCE10.dbf= CONCE10.dbf)


conce_list <- lapply(conce_list, function(decil) {
  
  mutate(decil,
         decil = ntile(gascor, 10))
  
})

## unlisting list to dfs ##

list2env('names<-'(conce_list, ls(pattern = "CONCE\\d\\d\\.dbf")), envir = .GlobalEnv)


## microdata set ##

global_dataset <- rbind(CONCE84.dbf, CONCE89.dbf, CONCE92.dbf, CONCE94.dbf,
                        CONCE96.dbf, CONCE98.dbf, CONCE00.dbf, CONCE02.dbf,
                        CONCE04.dbf, CONCE05.dbf, CONCE06.dbf, CONCE08.dbf,
                        CONCE10.dbf, CONCE12.dbf, CONCE14.dbf, CONCE16.dbf,
                        CONCE18.dbf)


## query information from Mexican Central Bank ##
## "siebanxicor
#Request token from : 
#https://www.banxico.org.mx/SieAPIRest/service/v1/token

setToken("a2e80d05fd77c8fda51dddddfc462157131fd9188bcd07faf3a994a3011385d9")

## Time Series Description ##
#SP1 : National Price Index (Base = 1st Quarter 2018)
#SR16643: Quarterly Mexican GDP current prices (millions of MXN) (Base 2013=100) (I 1993- IV 2018)
#SR10:Annual Mexican GDP current prices (millions of MXN) (1988-2004)
#SR1: Quarterly Mexican GDP constant prices (millions of MXN) (Base 1993=100) ( I 1980- III 2007)

idSeries <- c("SP1", "SR16643", "SR10", "SR1", 	"SR909")
series <- getSeriesData(idSeries, '1984-01-01','2018-07-12')

## unlisting list to dfs ##
list2env(series, envir = .GlobalEnv)

SP1 <- data.frame(SP1)
SR1 <- data.frame(SR1)
SR10 <- data.frame(SR10)
SR16643 <- data.frame(SR16643)


## Convert raw data into proper calculations  ##
## Target indicator: GDP/National Price Index (1984-2018) ##

## Building Consumer Price Index Time Serie

## Index to june 2013:
# 1. National Price Index / July 2018
#  
             index <- SP1[(
                SP1$date == "2018-07-01"), 2]
             
             SP1$value <- (SP1$value/index)*100
             
             index_1984 <- SP1[(SP1$date == "1984-07-01"), 2]
  
  #2. Index to june 2013 (base june 2013 =100)
              # June 2013 for research purposes
              # base from 100 to 1 as we will be using numeric values for gdp and not 
              # growth rates.
             
             index_june_2013 <- SP1[(
                                  SP1$date == "2013-06-01"
                                            ), 2]
             SP1$value <- (SP1$value / index_june_2013)*100
             
    #3. Extract values for relevant years
             price_index <- data.frame(
                 price_index = SP1[(SP1$date == "1984-06-01" |
                                 SP1$date == "1989-06-01" |
                                 SP1$date == "1992-06-01" |
                                 SP1$date == "1994-06-01" |
                                 SP1$date == "1996-06-01" |
                                 SP1$date == "1998-06-01" |
                                 SP1$date == "2000-06-01" |
                                 SP1$date == "2002-06-01" |
                                 SP1$date == "2004-06-01" |
                                 SP1$date == "2005-06-01" |
                                 SP1$date == "2006-06-01" |
                                 SP1$date == "2008-06-01" |
                                 SP1$date == "2010-06-01" |
                                 SP1$date == "2012-06-01" |
                                 SP1$date == "2014-06-01" |
                                 SP1$date == "2016-06-01" |
                                 SP1$date == "2018-06-01" 
                                   ), 2] )
             
## Building GDP Time Serie
    # 1. Desest SR1, SR16643

        ##SR1
            date_sr1 <- SR1[,1]
                   sr1_ts <- ts(SR1, frequency = 4, end = c(2007, 4))
                   decomp <- stl(sr1_ts[,2], s.window="periodic")
                   deseasonal_p <- seasadj(decomp)
                   deseasonal_p<- data.frame(valor = deseasonal_p)
                   SR1<- data.frame(
                                   date = date_sr1, 
                                   valor = deseasonal_p)
                   
          
          ##SR16443
              date_sr16443 <- SR16643[,1]
                   sr16443_ts <- ts(SR16643, frequency = 4, end = c(2018, 3))
                       decomp <- stl(sr16443_ts[,2], s.window = "periodic")
                    deseasonal_p <- seasadj(decomp)
                    deseasonal_p <- data.frame(valor = deseasonal_p)
                    SR16643 <- data.frame( 
                                          date = date_sr16443,
                                          valor = deseasonal_p)
                 
    # 2. Extract Time Serie
  ##1984: It is necessary to convert to current prices 
      ## From SR1: 
               gdp_1984<- data.frame(
                                gdp = SR1[(SR1$date == "1984-07-01"), 2] / index_1984) 
                
  # 1989-2004 
      ## Use SR10 as it comes: SR10[1989:2004,]
               gdp_89_04 <- data.frame(
                                        gdp = SR10[c(2,5,7,9,11,
                                                     13,15,17),2])
   
  ## 2005-2018:
     ## From SR16643:extract III Quarter from 2005 to 2018 
               gdp_05_18 <- data.frame(
                                        gdp = SR16643[c(51,55,63,71,
                                                        79,87,95,103),2])

    # building data frame 
          years <- data.frame( years = c("1984","1989", "1992", 
                                         "1994", "1996","1998", "2000", "2002",
                                          "2004", "2005", "2006", "2008", "2010", 
                                           "2012", "2014", "2016", "2018"))
               
          gdp <- rbind(gdp_1984, gdp_89_04, gdp_05_18)
          
          aggregate_dataset <- cbind(years, 
                                     gdp, 
                                     price_index)
          ## setting keys ##
          
          colnames(aggregate_dataset)[1]<- "año"
          
## join to global dataset ##
global_dataset <- global_dataset %>%
                              left_join(dplyr :: 
                                           select(aggregate_dataset, año, 
                                                  gdp, price_index),
                                                   by = "año")
## removing zeros ##
global_dataset <- global_dataset[!(global_dataset$gascor== 0),]






