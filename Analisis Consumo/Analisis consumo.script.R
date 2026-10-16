#Analisis de datos: Consumo en Argentina
#cargo datos y pongo el formato "fecha"
data.cons<- datos_cac_r
data.cons$fecha_dt<-as.Date(paste0(data.cons$fecha,"-01"))
data.cons<- subset(data.cons, select = -fecha)

library(ggplot2)

mayo<- subset(data.cons,fecha_dt == "2026-05-01")

#gráfico de la evolucion del consumo e ingreso

cons.in<-ggplot(data.cons,aes(x= fecha_dt))+ 
  geom_line(aes(y= consumo_idx, colour= "consumo"))+
  geom_line(aes(y= ingreso_idx, colour= "ingreso"))+
  geom_vline(xintercept = as.Date("2026-05-01", linetype= "dashdot", colour= "black", alpha= 0.3))+
  geom_point(data = mayo, aes(y=consumo_idx), size = 1.5, colour = "red")+
  geom_point(data = mayo, aes(y=ingreso_idx), size = 1.5, colour = "blue")+
  labs(x="fecha", y="Indice base 2017=100", title = "Evolucion del consumo e ingreso")+
  theme_minimal()
print(cons.in)

#gráfico de la evolucion del consumo e ingreso interanual

cons.in.ia<-ggplot(data.cons,aes(x= fecha_dt))+ 
  geom_line(aes(y= consumo_var_ia, colour= "consumo"))+
  geom_line(aes(y= ingreso_var_ia, colour= "ingreso"))+
  geom_vline(xintercept = as.Date("2026-05-01", linetype= "dashdot", colour= "black", alpha= 0.3))+
  geom_point(data = mayo, aes(y=consumo_var_ia), size = 1.5, colour = "red")+
  geom_point(data = mayo, aes(y=ingreso_var_ia), size = 1.5, colour = "blue")+
  labs(x="fecha", y="Indice base 2017=100", title = "Evolucion del consumo e ingreso inter anual")+
  theme_minimal()
print(cons.in.ia)

#Calculo variaciones mensuales (primeras diferencias y porcentajes)
df_diffs <-data.cons %>%
  mutate(
    d_consumo = c(NA, diff(consumo_idx)),
    d_ingreso = c(NA, diff(ingreso_idx)),
    d_icc     = c(NA, diff(icc_utdt)),
    d_riesgo  = c(NA, diff(riesgo_pais))
  ) %>%
  tidyr::drop_na()

# regresión lineal con las diferencias
modelo_diff <- lm(d_consumo ~ d_ingreso + d_icc + d_riesgo, data = df_diffs)

summary(modelo_diff)