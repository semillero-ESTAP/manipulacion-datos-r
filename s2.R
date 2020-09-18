# dplyr // manipulacion de datos
install.packages('dplyr') #instalacion
library(dplyr) #cargar 
# select : seleccionar columnas
# filter : filtrar informacion acorde a factores
# mutate : crear columas / modicar
# arrange : organizar / ascendente o descendente
# rename : renombrar columnas
# summarise : resumir datos 
# group_by : agrupar informacion en relacion a factor
stars <- starwars 

# %>% pipeline // concatenar

data1 <- stars %>% select(name, height, eye_color)

ojos_amarillo <- stars %>% filter(eye_color == 'yellow')

stars %>% select(name, birth_year, height, mass) %>% filter(height > 180)

bmi <- stars %>% mutate(BMI = height*mass^2 + birth_year)

bmi <- stars %>% select(1, height, mass) %>% mutate(BMI=height/mass^2)


stars %>% arrange(desc(height))

personas_bajas <- stars %>% select(name, height, mass, eye_color) %>% arrange(desc(mass)) %>%
  filter(height>100 & height < 150 ) # rango 100 - 150

personas_bajas <- stars %>% select(name, height, mass, eye_color) %>% arrange(desc(mass)) %>%
  filter(height<100)


stars <- stars %>% rename(nombre = name, altura = height, masa = mass, color_cabello = hair_color)

stars %>% select(nombre, skin_color, eye_color) %>% rename(color_piel = skin_color, color_ojo = eye_color) %>%
  filter(color_piel == 'gold')

# NA : NOT AVAIBLE -  no disponibles
stars %>% select(nombre, altura, masa) %>% summarise(media_altura = mean(altura, na.rm = TRUE), 
                                                     std_valor = sd(altura, na.rm = TRUE))

mundo_origen <- stars %>% group_by(homeworld) %>% summarise(media = mean(altura, na.rm = TRUE), 
                                            std_altura = sd(altura, na.rm = TRUE))

stars %>% select(nombre, altura, masa, sex) %>% filter(altura >120 & altura < 150 ) %>% group_by(sex) %>% 
  summarise(suma_total=sum(altura, na.rm =TRUE), media = mean(altura, na.rm = TRUE))

stars[10:20, ] %>% select(nombre, altura, masa, sex) %>% group_by(sex) %>% 
  summarise(suma_total=sum(altura, na.rm =TRUE), media = mean(altura, na.rm = TRUE))


stars %>% group_by(sex) %>% summarise(conteo = n(), media_altura = mean(altura, na.rm = TRUE))

planeta_sexo <- stars %>% group_by(homeworld, sex) %>% summarise(conteo = n(), media= mean(masa, na.rm = TRUE))


stars[9:15, ] %>% group_by(homeworld, sex) %>% summarise(conteo = n(), media_masa= mean(masa, na.rm = TRUE))
###
mean(stars$altura, na.rm = TRUE)
help(mean)

summary(stars)




prueba <- stars[10:20,2]
mean(prueba)

colMeans(stars[10:20,2], na.rm = TRUE)
lapply(stars[10:20,2], mean)
sapply(stars[10:20,2], mean)

