library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
#EDA : proceso iterativo
#1.Generar preguntas sobre los datos
#2.Obtener respuestas mediante visualizacion, transformacion y manipulacion de los datos
#3.Refinar preguntas apartir de lo aprendido

#importar el archivo
abandono <- read_csv('Documentos/s5/abandono.csv')
#limpieza de datos
dim(abandono)
str(abandono)
sum(is.na(abandono))
abandono <- na.omit(abandono)
dim(abandono)
names(abandono)
abandono <- abandono[,-1]
tolower(abandono)

summary(abandono)
unique(abandono$gender)
unique(abandono$SeniorCitizen)
unique(abandono$PhoneService)

#analisis inicial

ggplot(abandono, aes(x=MonthlyCharges)) + geom_boxplot()
ggplot(abandono, aes(x=tenure)) + geom_boxplot()

ggplot(abandono, aes(x=tenure)) + geom_histogram(bins = 20)
ggplot(abandono, aes(x=MonthlyCharges)) + geom_histogram(binwidth = 5)

ggplot(abandono, aes(x=SeniorCitizen)) + geom_bar()
ggplot(abandono, aes(x=Partner)) + geom_bar()

abandono %>% group_by(Partner) %>% summarise(n())


ggplot(abandono, aes(x=Churn)) + geom_bar()

churn <- abandono %>% filter(Churn == 'Yes')
no_churn <- abandono %>% filter(Churn == 'No')

ggplot(abandono, aes(x=SeniorCitizen, fill=Churn)) + geom_bar()

ggplot(abandono, aes(x=gender, fill=Churn)) + geom_bar()

ggplot(abandono, aes(x=MonthlyCharges, fill=Churn)) + geom_boxplot()

ggplot(abandono, aes(x=Dependents, fill=Churn)) + geom_bar()

# antiguedad

abandono %>% group_by(Churn) %>% summarise(mean(tenure), n())

ggplot(abandono, aes(x=PhoneService, fill=Churn)) + geom_bar()

ggplot(abandono, aes(x=MultipleLines, fill=Churn)) + geom_bar()

ggplot(abandono, aes(x=InternetService, fill=Churn)) + geom_bar()


ggplot(abandono, aes(x=PaymentMethod, fill=Churn)) + geom_bar()

ggplot(abandono, aes(x=Contract, fill=Churn)) + geom_bar()


