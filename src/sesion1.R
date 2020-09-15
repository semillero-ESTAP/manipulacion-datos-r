#orden del dia
#bienvenida
#revision del contenido
#conceptos basicos de Rstudio
#librerias base y externas

#importar archivos
install.packages('readr')
install.packages('readxl')
library(readr)
library(readxl)
library(stringr)

transacciones <-read_excel("Documentos/s1/transacciones.xlsm", sheet = 1)
transacciones <-read_excel("Documentos/s1/transacciones.xlsm", sheet = 1, range = 'B500:C800', col_names = FALSE)

personas <-read_delim('Documentos/s1/people (1).in', delim = "|" , col_names =FALSE)
personas <-read_delim('Documentos/s1/people (1).in', delim = "|" , col_names = columnas)
columnas = c('ID', 'nombre', 'apellido', 'cargo', 'pais', 'industria', 'recomendaciones', 'contactos')

saber11_c = read_csv('Documentos/s1/saber11_coma.csv')
saber11_pc = read_csv2('Documentos/s1/saber11_punto_coma.csv')

#manipulacion basica de dataframes (tibble)

#caracteristicas del dataset
str(transacciones)
nrow(transacciones)
ncol(transacciones)
dim(transacciones)
class(personas)
head(personas)
tail(personas)
View(personas)
#accesar a las columnas especificas y/o sitios/espacios/celdas especificos
transacciones$VALOR
saber11_c$PUNT_FISICA
personas[,1:3]

#operaciones basicas
saber11_c*50
saber11_c$PUNT_FISICA*10
saber11_c$PUNT_FISICA + saber11_c$PUNT_HISTORIA
#crear y quitar columnas
saber11_c$Promedio_Ponderado = (3*saber11_c$PUNT_FISICA+4*saber11_c$PUNT_HISTORIA +3.5*saber11_c$PUNT_QUIMICA)/(3+4+3.5)
saber11_c
saber11_c <- saber11_c[,-7]
## algunas operaciones con valores NA
sum(is.na(personas))
t1 <- personas[is.na(personas$pais),]
personas_na <- na.omit(personas)

###
mean(saber11_c$PUNT_FISICA, na.rm = FALSE)
sd(saber11_c$PUNT_HISTORIA)
summary(saber11_c)


dim(personas_na)
dim(personas)

## algunas operaciones basicas con caracteres

gsub('DEPOSITO CANAL B', '', transacciones$CANAL)
tolower(transacciones$CANAL)
toupper(transacciones$CANAL)
transacciones

#operadores logicos
x=50
y=25
TRUE
FALSE
x>y
x<y
x<=y
x>=y
x==y
x!=y
!x>y #negacion
(x>y & x==y) #codiciones en comparaciones
(x<y | x==y)
# Algunas operaciones basicas con caracteres

ch1 = 'HOLA'
ch2 = 'HOLa'
ch1 > ch2
nchar(ch1)
nchar(ch2)
ch1 == ch2


