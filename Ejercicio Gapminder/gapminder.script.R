#Ejercicio: Analisis econometrico con datos de gapminder

#Cargo datos
library(gapminder)
library(dplyr)
data1<- gapminder
Arg<- filter(data1, country == "Argentina")
inome_arg<- select(Arg,year,gdpPercap)
View(inome_arg)

#______ PARTE 1 ______

#1) Grafico evolucion intertemporal del ingreso per capita

library(ggplot2)
ggplot(data = inome_arg, aes(year, gdpPercap))+
  geom_line(colour = "violet",size=1)+
  theme_minimal() +
  labs(title = "Income Per Person Arg",
       x = "Año",
       y = "Gdp per Capita")

#2) Regresiones del ingreso per capita de los ultimos años

viejo.Arg<- filter(Arg, year< 1997)
nuevo.Arg<- filter(Arg, year>= 1997)

lin<- lm(gdpPercap~ year, viejo.Arg) #linear
pol1<-lm(gdpPercap~ poly(year,2, raw = TRUE), viejo.Arg) #polinomica grado 2
pol2<-lm(gdpPercap~ poly(year,10, raw = TRUE), viejo.Arg) #polinomica de grado 10

#Representacion grafica de los tres modelos
ggplot()+
  geom_point(data=Arg, aes(year, gdpPercap), color= "black")+
  geom_line(data=viejo.Arg, aes(year, predict(lin)),colour = "violet")+
  geom_line(data=viejo.Arg, aes(year, predict(pol1)),colour = "lightblue")+
  geom_line(data=viejo.Arg, aes(year, predict(pol2)),colour = "orange")+
  theme_minimal()+
  labs(title = "Modelos sobre Arg",
       x = "Año",
       y = "Gdp per Capita")

#3)
#a) selecciono 4 paises sudamericanos ademas de Argentina

suda<- c("Argentina","Brasil","Chile","Uruguay","Colombia")
#cálculo matriz de correlaciones entre ingresos
mat.niveles.sud<- gapminder %>%
  filter(country%in% suda) %>%
  select(country, year,gdpPercap)%>%
  pivot_wider(names_from= country, values_from = gdpPercap) %>%
  select(-year)%>%
  cor()

#b) cálculo de matriz de correlaciones entre variaciones porcentuales anuales
mat.crecimiento<-gapminder %>%
  filter(country%in% suda) %>%
  arrange(country %in% suda) %>%
  group_by(country)%>%
  mutate(crecimiento=(gdpPercap-lag(gdpPercap))/lag(gdpPercap))%>%
  ungroup()%>%
  filter(!is.na(crecimiento))%>%
  select(country,year,crecimiento)%>%
  pivot_wider(names_from= country, values_from = crecimiento) %>%
  select(-year)%>%
  cor(use= "complete.obs")

#visualizacion de ambas
print(mat.crecimiento)
print(mat.niveles.sud) 


#_____ PARTE 2 _______

#habiendo importado el dataset desde la ventana derecha, selecciono año 2010

library(tidyverse)
dta.new<- gapminder
dta.new<- select(dta.new,country, year, life_expectancy_female,life_expectancy)
dta.new<- filter(dta.new, year== 2010)

#5) Grafico expectativa de vida general vs mujeres

ggplot(dta.new, aes( life_expectancy_female, life_expectancy))+
  geom_point(colour= "violet")+
  labs(x= "Esperanza de vida mujeres",
       y = "Esperanza de vida general",
       title = "Relacion esperanza de vida mujeres vs total")
#6) Regresion lineal de expectativa de vida general sobre la de las mujeres

regresion.life<- lm(life_expectancy~life_expectancy_female, data= dta.new)
summary(regresion.life)

#7)Test de hipotesis

#H0: expectativa de vida mujeres== expectativa de vida general
#H1: la expectativa de vida de las mugeres es mayor

#lo paso a "numerico" porque sino no me toma los datos
life.exp.fem<- as.numeric(dta.new$life_expectancy_female)
life.exp.tot<- as.numeric(dta.new$life_expectancy)
#test
t.test(life.exp.fem, 
       life.exp.tot,
       paired = TRUE,
       alternative = "greater")

#8)Regresion añadiendo varable "ingreso per capita"

#filtro datos
dta.new2<- gapminder
dta.new2<- select(dta.new2,
                  country,year,life_expectancy_female,life_expectancy, income_per_person)
dta.new2<- filter(dta.new2, year== 2010)

#regresion
regresion.multiple<-lm(life_expectancy~life_expectancy_female+income_per_person,
                       data=dta.new2)
summary(regresion.multiple)

#9) Nueva regresion pero ahora con variables "expectativa de vida hombres", "ingreso per capita" y "region del mundo"

#filtro datos
dta.new3<- gapminder
dta.new3<- select(dta.new3,
                  country,year,life_expectancy, income_per_person, life_expectancy_male,
                  world_region)
dta.new3<- filter(dta.new3, year== 2010)
#nueva regresion
regresion.nueva<-lm(life_expectancy~life_expectancy_male+income_per_person + world_region,
                    data=dta.new3)
summary(regresion.nueva)
summary(regresion.multiple)
