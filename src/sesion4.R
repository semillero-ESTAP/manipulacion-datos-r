library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
#sesion: scatterplot, lineplot, boxplot, histograms, barplots
options(scipen=100000)
#dataset 
transacciones <- read_csv('Documentos/s4/tr_limpia.csv')
summary(transacciones)
# base(datos) + capas(definidas la geometria)
#histogramas
ggplot(transacciones, aes(x=VALOR)) + 
  geom_histogram() + xlim(0,20000) 

ggplot(transacciones, aes(x=VALOR, fill=CANAL)) + 
  geom_histogram() + xlim(0,20000)

transacciones %>% filter(CANAL == 'DEPOSITO CANAL D') %>%
  ggplot(., aes(x=VALOR))+geom_histogram()

transacciones %>% filter(CANAL == 'DEPOSITO CANAL D') %>%
  ggplot(., aes(x=VALOR))+geom_density()

#boxplot
ggplot(transacciones, aes(x=VALOR)) + geom_boxplot()
ggplot(transacciones, aes(x=VALOR)) + geom_boxplot() + xlim(0,25000)
ggplot(transacciones, aes(x=VALOR, fill=CANAL)) + geom_boxplot() + xlim(0,25000)
#genera varias figuras o subplots
ggplot(transacciones, aes(x=VALOR)) + geom_boxplot() + facet_grid(CANAL ~ .) + xlim(1,50000)



median(transacciones$VALOR)
mean(transacciones$VALOR)

transacciones %>% group_by(CANAL) %>% summarise(median(VALOR))
transacciones %>% group_by(CANAL) %>% summarise(mean(VALOR))


ggplot(transacciones, aes(x=CANAL)) + geom_bar()





# numero de transacciones por fecha
#preprocesamiento
as_date(transacciones$FECHA)
transacciones$FECHA <- ymd_hms(transacciones$FECHA)


#numero de transacciones por mes
#puntos
transacciones %>% 
  group_by(month=floor_date(FECHA, "month")) %>%
  summarize(total=sum(VALOR), n= n()) %>% 
  ggplot(., aes(month, n))+geom_point()
#barras
transacciones %>% 
  group_by(month=floor_date(FECHA, "month")) %>%
  summarize(total=sum(VALOR), n= n()) %>% 
  ggplot(., aes(month, n))+geom_bar(stat='identity') 

#linea
transacciones %>% 
  group_by(month=floor_date(FECHA, "week")) %>%
  summarize(total=sum(VALOR), n= n()) %>% 
  ggplot(., aes(month, n))+geom_line()

#puntos + linea 
transacciones %>% 
  group_by(month=floor_date(FECHA, "month")) %>%
  summarize(total=sum(VALOR), n= n()) %>% 
  ggplot(., aes(month, n))+geom_line()+geom_point()

# filtrando los datos para fechas especificas
transacciones %>% 
  group_by(month=floor_date(FECHA, "month")) %>%
  filter(FECHA < '2020-03-01') %>%
  summarize(total=sum(VALOR), n= n()) %>% 
  ggplot(., aes(month, n))+geom_line()+geom_point()

transacciones %>% 
  group_by(month=floor_date(FECHA, "month")) %>%
  filter(FECHA < '2020-03-01') %>%
  summarize(total=sum(VALOR), n= n()) %>% 
  ggplot(., aes(month, n))+geom_line()+geom_point()


canal_a <- transacciones %>% filter(CANAL == 'DEPOSITO CANAL A') %>%
  group_by(month=floor_date(FECHA, "month")) %>%
  summarize(total=sum(VALOR), n= n())

canal_b <- transacciones %>% filter(CANAL == 'DEPOSITO CANAL B') %>%
  group_by(month=floor_date(FECHA, "month")) %>%
  summarize(total=sum(VALOR), n= n())

canal_c <-transacciones %>% filter(CANAL == 'DEPOSITO CANAL C') %>%
  group_by(month=floor_date(FECHA, "month")) %>%
  summarize(total=sum(VALOR), n= n())

canal_d <- transacciones %>% filter(CANAL == 'DEPOSITO CANAL D') %>%
  group_by(month=floor_date(FECHA, "month")) %>%
  summarize(total=sum(VALOR), n= n())

ggplot(canal_a, aes(month, n)) + geom_line() + 
  geom_line(data = canal_b, mapping = aes(month, n), color = 'red') +
  geom_line(data = canal_c, mapping = aes(month, n), color = 'blue') +
  geom_line(data = canal_d, mapping = aes(month, n), color = 'brown')


ggplot() + 
  geom_line(data = canal_b, mapping = aes(month, n, color='B')) +
  geom_line(data = canal_c, mapping = aes(month, n, color = 'C')) + 
  geom_line(data = canal_d, mapping = aes(month, n, color = 'D')) +
  geom_point(data = canal_b, mapping = aes(month, n, color='B')) +
  geom_point(data = canal_c, mapping = aes(month, n, color = 'C')) + 
  geom_point(data = canal_d, mapping = aes(month, n, color = 'D')) +
  xlab('MES') +
  ylab('# DE TRANSACCIONES') +
  ggtitle('TRANSACCIONES POR CANAL')
  



