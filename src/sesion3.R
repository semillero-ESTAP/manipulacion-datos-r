library(readr)
library(readxl)
library(dplyr)

#limpieza de datos
# manipulacion de caracteres : modificar
transacciones <- read_excel('transacciones.xlsm')

head(transacciones)

mean(transacciones$valor)

sum(transacciones$VALOR)

transacciones$VALOR <- gsub('$', '', transacciones$VALOR, fixed = TRUE)

transacciones$VALOR <- gsub(',', '', transacciones$VALOR, fixed = TRUE)

transacciones$CANAL <- gsub('DEPOSITO ', '', transacciones$CANAL, fixed = TRUE)

View(transacciones)

str(transacciones)

transacciones$VALOR <- as.numeric(transacciones$VALOR)
 
str(transacciones)

mean(transacciones$VALOR)

sum(transacciones$VALOR)

transacciones %>% group_by(CANAL) %>% summarise(media=mean(VALOR))

transacciones %>% group_by(CANAL) %>% summarise(total=sum(VALOR), n = n())

## Modificacion de  caracteres
#modificar valores todo a mayuscula o todo a minuscula
transacciones$CANAL <- tolower(transacciones$CANAL) # se demoran
transacciones$CANAL <- toupper(transacciones$CANAL) # se demoran

transacciones %>% group_by(CANAL) %>% summarise(media=mean(VALOR))

transacciones %>% group_by(CANAL) %>% summarise(total=sum(VALOR))


#manejo de NA

base  <- read_csv('base_na.csv')
#revisar na por columnas
base[is.na(base$VALOR),]

base <- base %>% select(VALOR, CANAL, FECHA)

# ignorando NA
nrow(base_sinNA) <- na.omit(base)
sum(is.na(base_sinNA))

nrow(drop_na(base))
# remplazando NA
#depende si es un valor numerico o un caracater (linea de texto)

base[is.na(base)] <- 0

replace(base$ID.USUARIO, is.na(base$ID.USUARIO), 0)

replace(base$VALOR, is.na(base$VALOR), mean(base$VALOR, na.rm = TRUE))

base %>% group_by(CANAL) %>% summarise(n=n())

base$CANAL <- replace(base$CANAL, is.na(base$CANAL), 'CANAL A')




