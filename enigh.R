
#####################################################################
#                         ETL ENIGH                                 #
#                                                                   #
#####################################################################


######################## setting directory ##########################

old_wd <- dirname(rstudioapi::getActiveDocumentContext()$path)

setwd("C:/Users/80251882/Documents/data_inegi/databases")

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

df_list <- mget(ls()[sapply(ls(), function(x) is.data.frame(get(x)))])


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



POBLA84.dbf$ed_formal <- as.factor(do.call(rbind, lapply(
                                                    as.factor(POBLA84.dbf$ed_formal),
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
                   CONCE10.dbf= CONCE10.dbf,
                   CONCE12.dbf = CONCE12.dbf,
                   CONCE14.dbf = CONCE14.dbf,
                   CONCE16.dbf = CONCE16.dbf,
                   CONCE18.dbf = CONCE18.dbf)


conce_list <- lapply(conce_list, function(decil) {
  
  mutate(decil,
         decil = ntile(gascor, 10))
  
})

## unlisting list to dfs ##

for (i in names(conce_list)) {
  assign(i, conce_list[[i]], .GlobalEnv)
}


## microdata set ##

global_dataset <- rbind(CONCE84.dbf, 
                        CONCE89.dbf, 
                        CONCE92.dbf, 
                        CONCE94.dbf,
                        CONCE96.dbf, 
                        CONCE98.dbf, 
                        CONCE00.dbf, 
                        CONCE02.dbf,
                        CONCE04.dbf, 
                        CONCE05.dbf, 
                        CONCE06.dbf, 
                        CONCE08.dbf,
                        CONCE10.dbf,
                        CONCE12.dbf, 
                        CONCE14.dbf, 
                        CONCE16.dbf,
                        CONCE18.dbf)


## query information from Mexican Central Bank ##
## "siebanxicor
#Request token from : 
#https://www.banxico.org.mx/SieAPIRest/service/v1/token

setToken("12920dea59240ce4a70683757b5f5de73a97ab2912adfa6e21edb984ed27e6cc")

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

#####################################################################################
#                           Index to june 2013                                      #
#                                                                                   # 
#####################################################################################

## 1. National Price Index / July 2018
#             
 
#              SP1$value <- log(SP1$value, base=10)
# 
#              
# SP1$value <- lag(SP1$value, n=1)
#              index <- SP1[(
#                 SP1$date == "2018-07-01"), 2]
#              
#              SP1$value <- (SP1$value/index)*100
#              
#              index_1984 <- SP1[(SP1$date == "1984-07-01"), 2]
  
  #2. Index to june 2013 (base june 2013 =100)
              # June 2013 for research purposes
              # base from 100 to 1 as we will be using numeric values for gdp and not 
              # growth rates.
             # 
             # index_june_2013 <- SP1[(
             #                      SP1$date == "2013-06-01"
             #                                ), 2]
             # SP1$value <- (SP1$value / index_june_2013)*100
             
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
                                gdp = SR1[(SR1$date == "1984-07-01"), 2] / price_index[1,1]) 
                
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
                                           select(aggregate_dataset, 
                                                  año, 
                                                  gdp, price_index),
                                                   by = "año")
## removing zeros ##
global_dataset <- global_dataset[!(global_dataset$gascor== 0),]

for (i in colnames(global_dataset)){
  print(paste(i, " ", class(global_dataset[[i]])))
  
}




#####################################################################################
#                                                                                   #
#   Conviertiendo la microdata a logaritmo (series de tiempo basadas en hogares)    #
#                                                                                   #
#####################################################################################


##########################################################
#                                                        #
# se calcula el gasto real per capita como el            #  
#log((x= y/p)/(n))== log(x)-log(n)=log(y)-log(p)-log(n)  #
#                                                        #
##########################################################


global_dataset[["price_index_rounded"]] <- round(global_dataset[["price_index"]] / 100, 5)


global_dataset[["ln_gasto_real_per_capita"]] <- log(round(
                                             ((
                                               global_dataset[["gascor"]] / 
                                               global_dataset[["price_index_rounded"]])/
                                               (global_dataset[["tam_hog"]]))
                                             ,2), 
                                             base = 10)



####################################################################################
#                                                                                  #
# Convirtiendo datos agregados en logaritmo (series de tiempo nacionales)          #
#                                                                                  #  
####################################################################################

##########################################################
#                                                        #
# se calcula el pib real per capita como el              #  
#log((x= y/p)/(n))== log(x)-log(n)=log(y)-log(p)-log(n)  #
#                                                        #
##########################################################


############################### logaritmo de pib real ###############################

global_dataset[["ln_pib_real"]] <- log(round((
                                        global_dataset[["gdp"]]/
                                        global_dataset[["price_index_rounded"]]), 2), 
                                        base=10)

real_gdp_time_series <- aggregate(ln_pib_real ~ año, global_dataset, mean)

################################ logaritmo de poblacion #############################

population_time_series <- aggregate(factor ~ año, global_dataset, sum)
population_time_series[["ln_population"]] <- log(population_time_series[["factor"]], base= 10)


################################# agregado macroeconomico ##########################

real_gdp_time_series <- real_gdp_time_series %>%
                                         left_join(dplyr::select(population_time_series,
                                                                 año,
                                                                 ln_population),
                                                                 by= "año")

real_gdp_time_series <- real_gdp_time_series %>% 
                              dplyr::mutate(ln_pib_real_per_capita = 
                                                              (ln_pib_real/
                                                               ln_population)) %>%
                              dplyr::select(-c(ln_pib_real, ln_population))
                                      

############################ construyendo series de tiempo finales #################

decode_decil_fun <- function(x){
     if(is.na(x)){
       return(NA)
     } else if(x == 1){
       return("Decil 1")
     } else if(x == 2){
       return("Decil 2")
     } else if(x == 3){
       return("Decil 3")
     } else if(x == 4){
       return("Decil 4")
     } else if(x == 5){
       return("Decil 5")
     } else if(x==6){
       return("Decil 6")
     } else if(x==7){
       return("Decil 7")
     } else if(x == 8){
       return("Decil 8")
     } else if(x == 9){
       return("Decil 9")
     } else if(x == 10){
       return("Decil 10")
     }
}


global_dataset[["decil"]] <- as.factor(do.call(
                                       rbind, 
                                       lapply(global_dataset[["decil"]], 
                                              decode_decil_fun)))

######################## final time-series data frame ##############################

time_series_df <- aggregate(ln_gasto_real_per_capita ~ año + decil, global_dataset, FUN = mean)


time_series_df <- time_series_df %>%
                             left_join(dplyr::select(real_gdp_time_series,
                                                     año,
                                                     ln_pib_real_per_capita),
                                        by = "año")


time_series_df[["año"]]<- as.Date(paste("01", 
                                        "01", 
                                        time_series_df$año, sep="/"), 
                                  format="%d/%m/%Y")
  

######################## reacomodando de acuerdo al decil ###########################

decil_order <- c("Decil 1", "Decil 2", "Decil 3", 
                 "Decil 4", "Decil 5", "Decil 6", 
                 "Decil 7", "Decil 8", "Decil 9", "Decil 10")

time_series_df <- time_series_df %>%
                      dplyr::mutate(decil =  factor(decil, levels = decil_order)) %>%
                      dplyr::arrange(decil)
                                
#####################################################################################
#                                                                                   #
#                   Graficando distintas series de tiempo                           #
#                                                                                   #
#####################################################################################

 
#####################################################################################
#     Graficando tendencia (ln) del gasto real por decil                            #
#                                                                                   #
#####################################################################################

library(tidyverse)
library(ggplot2)
library(reshape2)
library(scales)
library(ggforce)
library(ggthemes)
library(ggplot2)
library(ggExtra)


df <- time_series_df %>% 
            dplyr::select(-c(ln_pib_real_per_capita))

#library(RColorBrewer)  

ggplot(df,aes(x=año,
              y=ln_gasto_real_per_capita,
              colour=decil,
              group=decil)) + 
              geom_line() +
              scale_x_date(date_breaks = "2 years",date_labels = "%Y")+
              xlab("Año")+
              ylab("Logaritmo Natural del Ingreso Real per Cápita")+
             labs(title = "Tendencia del Poder Adquisitivo en México",
             subtitle = "Por Decil : de 1989 a 2018",
             caption = "Fuente: ENIGH")+
             ggsave(path = "C:/Users/80251882/Documents/data_inegi/plots",
                       filename = "tendencia_deciles.jpg", 
                    width = 10,
                    height = 6)


#####################################################################################
#     Graficando el crecimiento en el poder adquisitivo por decil                   #
#                                                                                   #
#####################################################################################

df <- df %>% 
          group_by(decil) %>% 
              mutate(growth = ((ln_gasto_real_per_capita - lag(ln_gasto_real_per_capita))/
                               lag(ln_gasto_real_per_capita))*100) %>%
            tidyr::drop_na() %>%
            dplyr::select(-c(ln_gasto_real_per_capita)) %>%
  
            ## retirando outliers ##
            dplyr::ungroup() %>% 
            dplyr::nest_by(decil) %>% 
            dplyr::mutate(out_var1 =  list(boxplot.stats(data$growth)$out),
                filtered_df = list(data %>% filter(growth %in% out_var1 %>% `!`))) %>% 
            dplyr::select(decil,filtered_df) %>% 
            tidyr::unnest(filtered_df) %>% 
            dplyr::ungroup() 


################################ defining ggplot object ################################

# p <- ggplot(df,aes(x=año,
#                    y=growth)) + 
#   geom_line(col = "blue", size=7) +
#   scale_x_date(date_breaks = "3 years",date_labels = "%Y") +
#   facet_wrap_paginate(~decil, nrow = 1, ncol = 1, scales = "free") +
#   xlab("Año")+
#   ylab("Crecimiento en el Poder Adquisitivo por Decil") +
#   labs(title = "Evolucion del Poder Adquisitivo en México",
#        subtitle = "De 1989 a 2018",
#        caption = "Fuente: ENIGH")
# 
# 
# required_n_pages <- n_pages(p)
# 
# 
# 
# for(i in 1:required_n_pages){
#   
#    p <- ggplot(df,aes(x=año,
#                      y=growth)) + 
#     geom_line(col = "blue", size= 2) +
#     scale_x_date(date_breaks = "1 year",date_labels = "%Y") +
#     facet_wrap_paginate(~decil, nrow = 1, ncol = 1, scales = "free", page = i) +
#     xlab("Año")+
#     ylab("Crecimiento en el Poder Adquisitivo por Decil (%)") +
#     labs(title = "Evolucion del Poder Adquisitivo en México",
#          subtitle = "De 1989 a 2018",
#          caption = "Fuente: ENIGH")
#     print(p)
#     ggsave(
#       path = "C:/Users/80251882/Documents/data_inegi/plots",
#       filename = paste0(
#       "crecimiento_poder_adquisitivo_decil", i,".jpg"), 
#       width = 10, 
#       height = 6)
# }

#############################

theme_set(theme_bw()) # from ggthemes



ggplot(df, aes(decil, growth))+
  geom_violin()+
  xlab("Decil Poblacional")+
  ylab("Función de Densidad de Probabilidad para el Crecimiento en Poder Adquisitivo")+
  labs(title = "Gráfico de Violin para Crecimiento en Poder Adquisitivo",
       subtitle = "Por Decil/ Función de Densidad de Probabilidad al 95% de Confianza",
       caption = "Elaboración propia con datos de la ENIGH")+
       ggsave(path = "C:/Users/80251882/Documents/data_inegi/plots",
               filename="violin_plot_deciles.jpg", 
               plot=last_plot())


#####################################################################################
#     Graficando el crecimiento en el poder adquisitivo por decil y                 #
#     el crecimiento del pib per capita                                             #
#####################################################################################


growth_biseries_df <- # obteniendo las tasas de crecimiento para el gasto real y el pib 
  
                     time_series_df %>%
                     dplyr::group_by(decil) %>% 
                     dplyr::mutate(growth_gasto_real_per_capita = 
                            ((ln_gasto_real_per_capita - lag(ln_gasto_real_per_capita))/
                              lag(ln_gasto_real_per_capita))*100) %>%
                     dplyr::mutate(growth_pib_real_per_capita = 
                                     ((ln_pib_real_per_capita - lag(ln_pib_real_per_capita))/
                                        lag(ln_pib_real_per_capita))*100) %>%
                     tidyr::drop_na() %>%
                     dplyr::select(-c(ln_gasto_real_per_capita, ln_pib_real_per_capita)) %>%
                     `colnames<-`(c("año", 
                                    "decil", 
                                    "Crecimiento en el Poder Adquisitivo (%)", 
                                    "Crecimiento en el PIB Real per Cápita (%)")) %>%
  
                     ## retirando outliers ##
                      dplyr::ungroup() %>% 
                      dplyr::nest_by(decil) %>% 
                      dplyr::mutate(out_var1 =  list(boxplot.stats(data$`Crecimiento en el Poder Adquisitivo (%)`)$out),
                                    out_var2 =  list(boxplot.stats(data$`Crecimiento en el PIB Real per Cápita (%)`)$out),
                                   filtered_df = list(data %>% filter(`Crecimiento en el Poder Adquisitivo (%)` %in% out_var1 %>% `!`,
                                                   `Crecimiento en el PIB Real per Cápita (%)` %in% out_var2 %>% `!`))) %>% 
                     dplyr::select(decil,filtered_df) %>% 
                     tidyr::unnest(filtered_df) %>% 
                     dplyr::ungroup() %>%
                    
                     ## preparando datos para las graficar ##
                     tidyr::gather(variable, 
                                     value, 
                                   `Crecimiento en el Poder Adquisitivo (%)`, 
                                   `Crecimiento en el PIB Real per Cápita (%)`) 


################################ defining ggplot object ################################
# 
# p <- ggplot(growth_biseries_df, aes(x = año, y = value)) + 
#        geom_line(aes(color = variable), size = 1) +
#        scale_x_date(date_breaks = "3 years",date_labels = "%Y") +
#        facet_wrap_paginate(~decil, nrow = 1, ncol = 1, scales = "free") +
#        scale_color_manual(values = c("blue", "green")) +
#        theme_minimal()+
#        xlab("Año")+
#        ylab("Crecimiento (%)")+
#        labs(title = "Comparación de la Evolucion de la Economia y del Poder Adquisitivo en México",
#             subtitle = "Por Decil: De 1989 a 2018",
#              caption = "Fuente: ENIGH y Banxico")
# 
# 
# required_n_pages <- n_pages(p)
# 
# 
# for(i in 1:required_n_pages){
#   
#   
#   
#   p <- ggplot(growth_biseries_df, aes(x = año, y = value)) + 
#     geom_line(aes(color = variable), size = 1) +
#     scale_x_date(date_breaks = "3 years",date_labels = "%Y") +
#     facet_wrap_paginate(~decil, nrow = 1, ncol = 1, scales = "free", page=i) +
#     scale_color_manual(values = c("blue", "green")) +
#     theme_minimal()+
#     xlab("Año")+
#     ylab("Crecimiento (%)")+
#     labs(title = "Comparación de la Evolucion de la Economia y del Poder Adquisitivo en México",
#          subtitle = "Por Decil: De 1989 a 2018",
#          caption = "Fuente: ENIGH y Banxico")
#   
#   print(p)
#   ggsave(paste0("ppa_growth_decil", i,".png"), width = 15, 
#          height = 4, dpi = 300, units = "in", device='png')
# }




#####################################################################################
#     Graficando el crecimiento en el poder adquisitivo por decil y                 #
#     el crecimiento del pib per capita  (incluyendo histograma y boxplot)          #
#####################################################################################

df1 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 1")
df2 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 2")
df3 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 3")
df4 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 4")
df5 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 5")
df6 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 6")
df7 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 7")
df8 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 8")
df9 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 9")
df10 <- growth_biseries_df %>% dplyr::filter(decil=="Decil 10")


theme_set(theme_bw())  # pre-set the bw theme.

#####################################################################
g1 <- ggplot(tidyr::spread(df1, variable, value), 
            aes(`Crecimiento en el PIB Real per Cápita (%)`,
              `Crecimiento en el Poder Adquisitivo (%)`, 
                )) +
    geom_count(show.legend = F)+
    geom_smooth(method="loess", se=F)+
    geom_text(aes(label= lubridate::year(año)), 
              hjust = 0.5,  vjust = -1)+
    labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
         subtitle = "Decil 1",
         caption = "Elaboración propia con datos de ENIGH y Banxico")


ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil1.jpg",
       ggMarginal(g1, type = "histogram", fill="transparent"),
                 width = 10,
                 height = 6)

########################################################################

g2 <- ggplot(tidyr::spread(df2, variable, value), 
             aes(`Crecimiento en el PIB Real per Cápita (%)`,
                 `Crecimiento en el Poder Adquisitivo (%)`, 
             )) +
  geom_count(show.legend = F)+
  geom_smooth(method="loess", se=F)+
  geom_text(aes(label= lubridate::year(año)), 
            hjust = 0.5,  vjust = -1)+
  labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
       subtitle = "Decil 2",
       caption = "Elaboración propia con datos de ENIGH y Banxico")


invisible(ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil2.jpg",
       ggMarginal(g2, type = "histogram", fill="transparent"),
       width = 10,
       height = 6))

#######################################################################


g3 <- ggplot(tidyr::spread(df3, variable, value), 
             aes(`Crecimiento en el PIB Real per Cápita (%)`,
                 `Crecimiento en el Poder Adquisitivo (%)`, 
             )) +
  geom_count(show.legend = F)+
  geom_smooth(method="loess", se=F)+
  geom_text(aes(label= lubridate::year(año)), 
            hjust = 0.5,  vjust = -1)+
  labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
       subtitle = "Decil 3",
       caption = "Elaboración propia con datos de ENIGH y Banxico")


ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil3.jpg",
       ggMarginal(g3, type = "histogram", fill="transparent"),
       width = 10,
       height = 6)

####################################################################


g4 <- ggplot(tidyr::spread(df4, variable, value), 
             aes(`Crecimiento en el PIB Real per Cápita (%)`,
                 `Crecimiento en el Poder Adquisitivo (%)`, 
             )) +
  geom_count(show.legend = F)+
  geom_smooth(method="loess", se=F)+
  geom_text(aes(label= lubridate::year(año)), 
            hjust = 0.5,  vjust = -1)+
  labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
       subtitle = "Decil 4",
       caption = "Elaboración propia con datos de ENIGH y Banxico")


ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil4.jpg",
       ggMarginal(g4, type = "histogram", fill="transparent"),
       width = 10,
       height = 6)

######################################################################

g5 <- ggplot(tidyr::spread(df5, variable, value), 
             aes(`Crecimiento en el PIB Real per Cápita (%)`,
                 `Crecimiento en el Poder Adquisitivo (%)`, 
             )) +
  geom_count(show.legend = F)+
  geom_smooth(method="loess", se=F)+
  geom_text(aes(label= lubridate::year(año)), 
            hjust = 0.5,  vjust = -1)+
  labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
       subtitle = "Decil 5",
       caption = "Elaboración propia con datos de ENIGH y Banxico")


ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil5.jpg",
       ggMarginal(g5, type = "histogram", fill="transparent"),
       width = 10,
       height = 6)

####################################################################

g6 <- ggplot(tidyr::spread(df6, variable, value), 
             aes(`Crecimiento en el PIB Real per Cápita (%)`,
                 `Crecimiento en el Poder Adquisitivo (%)`, 
             )) +
  geom_count(show.legend = F)+
  geom_smooth(method="loess", se=F)+
  geom_text(aes(label= lubridate::year(año)), 
            hjust = 0.5,  vjust = -1)+
  labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
       subtitle = "Decil 6",
       caption = "Elaboración propia con datos de ENIGH y Banxico")


ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil6.jpg",
       ggMarginal(g6, type = "histogram", fill="transparent"),
       width = 10,
       height = 6)

####################################################################

g7 <- ggplot(tidyr::spread(df7, variable, value), 
             aes(`Crecimiento en el PIB Real per Cápita (%)`,
                 `Crecimiento en el Poder Adquisitivo (%)`, 
             )) +
  geom_count(show.legend = F)+
  geom_smooth(method="loess", se=F)+
  geom_text(aes(label= lubridate::year(año)), 
            hjust = 0.5,  vjust = -1)+
  labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
       subtitle = "Decil 7",
       caption = "Elaboración propia con datos de ENIGH y Banxico")


ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil7.jpg",
       ggMarginal(g7, type = "histogram", fill="transparent"),
       width = 10,
       height = 6)
###################################################################

g8 <- ggplot(tidyr::spread(df8, variable, value), 
             aes(`Crecimiento en el PIB Real per Cápita (%)`,
                 `Crecimiento en el Poder Adquisitivo (%)`, 
             )) +
  geom_count(show.legend = F)+
  geom_smooth(method="loess", se=F)+
  geom_text(aes(label= lubridate::year(año)), 
            hjust = 0.5,  vjust = -1)+
  labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
       subtitle = "Decil 8",
       caption = "Elaboración propia con datos de ENIGH y Banxico")


ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil8.jpg",
       ggMarginal(g8, type = "histogram", fill="transparent"),
       width = 10,
       height = 6)

####################################################################
g9 <- ggplot(tidyr::spread(df9, variable, value), 
             aes(`Crecimiento en el PIB Real per Cápita (%)`,
                 `Crecimiento en el Poder Adquisitivo (%)`, 
             )) +
  geom_count(show.legend = F)+
  geom_smooth(method="loess", se=F)+
  geom_text(aes(label= lubridate::year(año)), 
            hjust = 0.5,  vjust = -1)+
  labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
       subtitle = "Decil 9",
       caption = "Elaboración propia con datos de ENIGH y Banxico")


ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil9.jpg",
       ggMarginal(g9, type = "histogram", fill="transparent"),
       width = 10,
       height = 6)


#####################################################################

g10 <- ggplot(tidyr::spread(df10, variable, value), 
             aes(`Crecimiento en el PIB Real per Cápita (%)`,
                 `Crecimiento en el Poder Adquisitivo (%)`, 
             )) +
  geom_count(show.legend = F)+
  geom_smooth(method="loess", se=F)+
  geom_text(aes(label= lubridate::year(año)), 
            hjust = 0.5,  vjust = -1)+
  labs(title="Crecimiento del Poder Adquisitivo y de la Economía",
       subtitle = "Decil 10",
       caption = "Elaboración propia con datos de ENIGH y Banxico")


ggsave(path="C:/Users/80251882/Documents/data_inegi/plots",
       filename= "relationship_economy_ppa_decil10.jpg",
       ggMarginal(g10, type = "histogram", fill="transparent"),
       width = 10,
       height = 6)




# Conclusion: hay homogeneidad entre deciles
# lo unico que cambia es la magnitud del impacto

#####################################################################################
#     Graficando el ciclo economico real vs el ciclo del poder adquisitivo          #
#                                   por decil                                       #
#####################################################################################
library(dplyr)
library(mFilter)
library(purrr)

economic_cycles_df <- tidyr::spread(growth_biseries_df, variable, value) %>%
                      `colnames<-` (c("decil", 
                            "año", 
                            "Ciclo del Ingreso Real per Cápita", 
                            "Ciclo Económico Real per Cápita")) %>%
              dplyr::group_by(decil) %>%
              dplyr::mutate(año = lubridate::year(año), 
                    `Ciclo del Ingreso Real per Cápita` = fitted(loess(`Ciclo del Ingreso Real per Cápita`~ año)),
                    `Ciclo Económico Real per Cápita` = fitted(loess(`Ciclo Económico Real per Cápita` ~ año))) %>%
             dplyr::ungroup() %>%
              ## preparando datos para las graficar ##
              tidyr::gather(variable, 
                            value, 
                           `Ciclo del Ingreso Real per Cápita`, 
                           `Ciclo Económico Real per Cápita`) 


economic_cycles_df[["año"]]<- as.Date(paste("01", 
                                        "01", 
                                        economic_cycles_df$año,
                                        sep="/"), 
                                        format="%d/%m/%Y")



p <- ggplot(economic_cycles_df, aes(x = año, y = value)) + 
            geom_line(aes(color = variable), size = 1) +
            scale_x_date(date_breaks = "2 years",date_labels = "%Y") +
            facet_wrap_paginate(~decil, nrow = 1, ncol = 1, scales = "free") +
            scale_color_manual(values = c("blue", "green")) +
            theme_minimal()+
            xlab("Año")+
            ylab("Ciclo Real")+
           labs(title = "Comparación de Ciclos Económicos",
                subtitle = "Filtro Baxter-King",
                caption = "Fuente: Elaboración propia con datos del ENIGH y Banxico")


required_n_pages <- n_pages(p)


for(i in 1:required_n_pages){
  
  
  
  
  p <- ggplot(economic_cycles_df, aes(x = año, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    scale_x_date(date_breaks = "2 years",date_labels = "%Y") +
    facet_wrap_paginate(~decil, nrow = 1, ncol = 1, scales = "free", page=i) +
    scale_color_manual(values = c("blue", "green")) +
    theme_minimal()+
    xlab("Año")+
    ylab("Ciclo Real")+
    labs(title = "Comparación de Ciclos Económicos",
         subtitle = "Filtro Baxter-King",
         caption = "Fuente: Elaboración propia con datos del ENIGH y Banxico")
  
  
  print(p)
  ggsave(path = "C:/Users/80251882/Documents/data_inegi/plots",
         filename = paste0("ciclos_por_decil", i,".jpg"), 
         width = 10,
         height = 6)
}


economic_cycles_series_df <- tidyr::spread(economic_cycles_df, 
                                           variable, 
                                           value)

economic_cycles_df[["año"]] <- NULL

library(plyr)

corr_decil <- data.frame(ddply(economic_cycles_series_df, .(decil), 
      summarise, 
      "corr" = cor(`Ciclo del Ingreso Real per Cápita`, 
                   `Ciclo Económico Real per Cápita`, 
                   method = "pearson"))) %>%
        mutate(corr= round(corr, 2))
   
ggplot(data=corr_decil, aes(x=decil, y=corr, group=1)) +
  geom_point() +
  geom_line() +
  labs(x = "Decil", y = "Correlación de Pearson", 
       title = "Grado de Sincronización por Decil")+
  ggsave(filename= "grado_de_sincronizacion_por_decil.jpg",
         path="C:/Users/80251882/Documents/data_inegi/plots",
         width=10,
         height = 6)


##########################################################################
#                      Forecast Regression                               #
#                                                                        #
##########################################################################


######################### calculando las series ##########################
real_gdp_time_series <- real_gdp_time_series %>%
                       dplyr::mutate(año = as.Date(paste("01", 
                                    "01", 
                                    real_gdp_time_series[["año"]], sep="/"), 
                                    format="%d/%m/%Y"))

real_income_time_series <- aggregate(ln_gasto_real_per_capita ~ año, 
                             global_dataset, 
                             FUN = mean) %>%
  dplyr::mutate(año = as.Date(paste("01", 
                                    "01", 
                                    real_income_time_series[["año"]], sep="/"), 
                              format="%d/%m/%Y"))


forecast_input <- cbind(real_gdp_time_series, 
                        ln_gasto_real_per_capita = 
                        real_income_time_series[['ln_gasto_real_per_capita']])

############################### pipeline ################################

forecast_input <- forecast_input %>%
                  # obteniendo las tasas de crecimiento #
                     
                  dplyr::mutate(growth_gasto_real_per_capita = 
                        ((ln_gasto_real_per_capita - lag(ln_gasto_real_per_capita))/
                                  lag(ln_gasto_real_per_capita))) %>%
                  dplyr::mutate(growth_pib_real_per_capita = 
                              ((ln_pib_real_per_capita - lag(ln_pib_real_per_capita))/
                               lag(ln_pib_real_per_capita))) %>%
                  tidyr::drop_na() %>%
                  dplyr::select(-c(ln_gasto_real_per_capita, ln_pib_real_per_capita)) %>%
                  
                  # convirtiendo a ciclos y formateando el año #
                  dplyr::mutate(año = lubridate::year(año), 
                  ciclo_ingreso_real = fitted(loess(growth_gasto_real_per_capita~ año)),
                  ciclo_economico_real = fitted(loess(growth_pib_real_per_capita ~ año))) %>%
                  dplyr::mutate(año = as.Date(paste("01", 
                                                    "01", 
                                                   año, 
                                                   sep="/"), 
                                                   format="%d/%m/%Y")) %>%
                  dplyr::select(-c(growth_gasto_real_per_capita,
                                    growth_pib_real_per_capita))
                  
###############################################################################


requiredpackages <- c('readxl', 'openxlsx', 'foreign', 'dplyr',  
                      'stringr', 'janitor', 'tibble', 'lubridate', 
                      'zoo', 'data.table', 'forecast', 'svMisc', 'tau',
                      'bitops', 'RCurl', 'stringr', 'XML', 'urca', 'fpp',
                      'ggplot2', 'ggfortify', 'TSA', 'stringi')

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

############ realizando el pronostico para el ciclo del ingreso real ############

#################################################################################
# Descripcion: se procede a obtener un pronostico para el año                   #
#              2020 para el ciclo del ingreso real, de esta forma               #
#              lo podremos comparar con el ciclo economico real                 # 
#              del año 2020, y asi poder llegar a tener una nocion              #
#              de cual es el la relacion entre ambos en el año 2020             #
#################################################################################

library(xts)

cir_ts <- xts(forecast_input[['ciclo_ingreso_real']], order.by=forecast_input[,1])


######################## seleccionando el arima optimo #########################

#Criterio BIC

a<-armasubsets(y=cir_ts, nar=2, nma=2, ar.method = 'ols')

par(mfrow=c(1,1))
plot(a)


################################## arima(2,0,1) ################################

#model<-auto.arima(cir_ts, max.d=2, max.q = 1)

model <- Arima(cir_ts, order=c(2,0,1))

Box.test(residuals(model), lag=1, fitdf=1, type="Box-Pierce")

acf(residuals(model),
    main = "Modelo ARIMA(2,0,1) Autocorrelograma")
acf(residuals(model), 
    type="partial",
    main = "Modelo ARIMA(2,0,1) Autocorrelograma Parcial")

############################ pronostico del modelo ############################

forecast(model,h=4)

plot(forecast(model,h=4), 
     main= 'Pronostico ARIMA(2,0,1) con media cero',
     ylab="Ciclo del Ingreso Real",
     xlab="Fecha", 
     col="orange")



### construyendo nuevas series con pronostico y datoS actualizados del Inegi ##

real_gdp_time_series <- real_gdp_time_series %>%
                        ## getting gdp growth ##
  
                       dplyr::mutate(growth_pib_real_per_capita = 
                                    ((ln_pib_real_per_capita - lag(ln_pib_real_per_capita))/
                                     lag(ln_pib_real_per_capita))) %>%
                       dplyr::select(-c(ln_pib_real_per_capita)) %>%
                       tidyr::drop_na() %>%
  
                        ## adding new obs ##
  
                            rbind(list(año = c('2019-01-01', '2020-01-01'), 
                            growth_pib_real_per_capita = c(-0.006, -0.086))) %>%
                        
                        ## obteniendo ciclo ##
                       dplyr::mutate(año = lubridate::year(año), 
                                     ciclo_economico_real = fitted(loess(growth_pib_real_per_capita~ año))) %>%
                       dplyr::mutate(año = as.Date(paste("01", 
                                    "01", 
                                    año, 
                                    sep="/"), 
                              format="%d/%m/%Y")) %>%
                      dplyr::select(-c(growth_pib_real_per_capita))
                         


real_income_time_series <- real_income_time_series %>%
  
                           ## getting income growth ##
  
                          dplyr::mutate(growth_gasto_real_per_capita = 
                                      ((ln_gasto_real_per_capita - lag(ln_gasto_real_per_capita))/
                                        lag(ln_gasto_real_per_capita))) %>%
                         dplyr::select(-c(ln_gasto_real_per_capita)) %>%
                         tidyr::drop_na() %>%
  
                           ## obteniendo ciclo ##
                         dplyr::mutate(año = lubridate::year(año), 
                                       ciclo_ingreso_real = fitted(loess(growth_gasto_real_per_capita~ año))) %>%
                         dplyr::mutate(año = as.Date(paste("01", 
                                                           "01", 
                                                            año, 
                                                            sep="/"), 
                                                            format="%d/%m/%Y")) %>%
                         dplyr::select(-c(growth_gasto_real_per_capita)) %>%
  
                       ## adding new obs ##
  
                         rbind(list(año = c('2019-01-01', '2020-01-01'), 
                                    ciclo_ingreso_real = c(0.006619960, -0.001171934)))


newest_df <- cbind(real_gdp_time_series, ciclo_ingreso_real=
                                         real_income_time_series[['ciclo_ingreso_real']]) %>%
             tidyr::gather(variable, 
                           value, 
                           ciclo_economico_real, 
                           ciclo_ingreso_real) 

newest_df$variable <- factor(newest_df$variable, 
                             levels=c('ciclo_economico_real', 'ciclo_ingreso_real'), 
                             labels=c('Ciclo Económico Real', 'Ciclo de Ingreso Real'))



 
ggplot(newest_df, aes(x=año)) + 
  geom_line(aes(y=value, col=variable), size=2) + 
  labs(title="Ciclo Economico y de Ingreso Real", 
       subtitle="De 1989 a 2020", 
       caption="Elaboración Propia con Datos de la ENIGH y Banxico", 
       y="Ciclo",
       x='Año',
       color=NULL) +  # title and caption
  scale_x_date(date_breaks = "1 year",date_labels = "%Y") +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid 




