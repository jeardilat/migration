### Para construir ecuación

## Extraer datos de hojas de calculo. Se debe pŕocesar la hoja previamente

## Cargar librerias

library(tidyverse)
library(readxl)
Defuncion_1998 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_Mortalidad/Defuncion_1998.xls", sheet = 2)
View(Defuncion_1998)      

leer <- function(archivo, hoja, rango) ## para defunciones
  {ruta <- str_c("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_Mortalidad/",archivo,".xls")
  temp <- read_excel(ruta, sheet = hoja, range = rango)
    View(temp)
    return(temp)
}

ppa <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/proyeccion_poblacion_antioquia.xlsx")
## Uso de funcion para datos
da1998 <- leer("Defuncion_1998",3,"a1:x511")
da1999 <- leer("Defuncion_1999",3,"a1:v623")
da2000 <- leer("Defuncion_2000",3,"a1:w624")
da2001 <- leer("Defuncion_2001",3,"a1:s615")
da2002 <- leer("Defuncion_2002",3,"a1:w612")
da2003 <- leer("Defuncion_2003",2,"a1:w612")
da2004 <- leer("Defuncion_2004",3,"a1:y612")
da2005 <- leer("Defuncion_2005",3,"a1:v593")
da2006 <- leer("Defuncion_2006",3,"a1:s595")
da2007 <- leer("Defuncion_2007",3,"a1:u600")
da2008 <- leer("Defuncion_2008_2018",1,"a1:w612")
da2009 <- leer("Defuncion_2008_2018",2,"a1:w612")
da2010 <- leer("Defuncion_2008_2018",3,"a1:w612")
da2011 <- leer("Defuncion_2008_2018",4,"a1:w612")
da2012 <- leer("Defuncion_2008_2018",5,"a1:w612")
da2013 <- leer("Defuncion_2008_2018",6,"a1:w612")
da2015 <- leer("Defuncion_2008_2018",7,"a1:w612")
da2016 <- leer("Defuncion_2008_2018",8,"a1:w612")
da2017 <- leer("Defuncion_2008_2018",9,"a1:w612")
da2018 <- leer("Defuncion_2008_2018",10,"a1:w612")
da2014 <- leer("Defuncion_2008_2018",11,"a1:w612")

na1998 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_1998.xls", sheet = 1, range = "a27:j608")
## los datos del 1999 estan mal. EStan por lugar de ocurrencia, no de residencia
na1999 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos 1999.XLS", sheet = 1, range = "a25:p152")
na2000 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2000.xls", sheet = 1, range = "a25:p192")
na2001 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2001.xls", range = "a38:p165")
na2002 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos _2002.xls", range = "a37:p164")
na2003 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2003.xls", range = "a27:t154")
na2004 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2004.xls", range = "a25:p152")
na2005 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2005.xls", range = "a23:p150")
na2006 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2006.xls", range = "a25:p152")
na2007 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2007.xls", range = "a25:p152")
na2008 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2008.xls", sheet = 3, range = "a13:l141")
na2009 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2009.xls", sheet = 3, range = "a12:l140")
na2010 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2010.xls", sheet = 3, range = "a12:l141")
na2011 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2011.xls", sheet = 3, range = "a12:l141")
na2012 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2012.xls", sheet = 3, range = "a12:l141")
na2013 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos _2013.xls", sheet = 3, range = "a12:l141")
na2014 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/Nacimientos_2014.xls", sheet = 3, range = "a12:n141")
na2015 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/NACIMIENTOS_2015.xls", sheet = 3, range = "a12:q141")
na2016 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/NACIMIENTOS_2016.xls", sheet = 3, range = "a12:q141")
na2017 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/NACIMIENTOS_2017.xls", sheet = 3, range = "a14:q142")
na2018 <- read_excel("~/Documentos/NIDIA/datos ecuacion poblamacion/DANE_ NACIDOS/NACIMIENTOS_2018 .xls", sheet = 3, range = "a13:q141")






#Nmuni <- for(Defuncion_1998[i,1] in )
  
Nmuni <- function(x)
  {
   i <- length(x[,1])
   a <- i-1
    for(x[i,1] in x) 
      {
    if(is.na(x[i,1])) {xx[i] <- x[a,1]} 
      } }


