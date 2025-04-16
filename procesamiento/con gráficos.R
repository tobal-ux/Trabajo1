# Trabajo 1
# Integrantes: cristobal astorga, matilde hermosilla, daniela pavez 


# 0.0 Ajustes iniciales: 

# 0.1 Limpiar entorno de trabajo

rm(list=ls())

# 0.2 Desactivar notacion cientifica

options(scipen=999) 

# 1.0 Cargar paquetes:

pacman::p_load(dplyr, 
               sjmisc, 
               car, 
               sjlabelled,
               stargazer, 
               haven, 
               summarytools,
               kableExtra,
               corrplot, 
               sessionInfo,
               ggplot2)

# 2.0 Cargar BBDD

load(url("https://github.com/tobal-ux/Trabajo1/raw/refs/heads/main/input/ELSOC_Long_2016_2023.RData"))

# 2.1 Comprobamos si se puede abrir la base de datos 

View(elsoc_long_2016_2023)

# 2.2 Dimensiones de la base de datos 

dim(elsoc_long_2016_2023) 

# 3.0 Seleccionamos las variables de interes y hacemos una nueva base de datos

proc_data <- elsoc_long_2016_2023 %>% select( m30, # ingresos
                                              m0_sexo, # sexo
                                              c05_01, # confianza en el gobierno
                                              c05_03, # confianza en carabineros
                                              c05_05) # confianza en el poder judicial

# 3.1 Comprobar si resulto el paso anterior 
names(proc_data)

# 4.0 Conocer el nivel de las variables
sjlabelled::get_label(proc_data)

# 5.0 Procesamiento de las variables

# 5.1 Descriptivos

# Ingresos 
frq(proc_data$m30)

# Sexo 
frq(proc_data$m0_sexo)

# Confianza en el gob
frq(proc_data$c05_01)

# Confianza en Carabineros
frq(proc_data$c05_03)

# Confianza en el poder judicial
frq(proc_data$c05_05)

# 5.2 Recodificamos NA ingresos
proc_data$m30 <- recode(proc_data$m30, "c(-888,-999)=NA")
proc_data$m30 <- recode(proc_data$m30, "c(-777,-666)=NA")

# Recodificamos sexo
proc_data$m0_sexo <- car::recode(proc_data$m0_sexo, "1=0;2=1")

# Recodificamos NA confianza en el gobierno
proc_data$c05_01 <- recode(proc_data$c05_01, "c(-888,-999)=NA")
proc_data$c05_01 <- recode(proc_data$c05_01, "c(-777,-666)=NA")

# Recodificamos NA confianza en los carabineros
proc_data$c05_03 <- recode(proc_data$c05_03, "c(-888,-999)=NA")
proc_data$c05_03 <- recode(proc_data$c05_03, "c(-777,-666)=NA")

# Recodificamos NA confianza en el poder judicial
proc_data$c05_05 <- recode(proc_data$c05_05, "c(-888,-999)=NA")
proc_data$c05_05 <- recode(proc_data$c05_05, "c(-777,-666)=NA")

# 5.3 Etiquetado

proc_data <- proc_data %>% rename("ingresos"=m30, # ingresos
                                  "sexo"=m0_sexo, # sexo
                                  "conf_gob"=c05_01, # Confianza en el gobierno
                                  "conf_car"=c05_03, # confianza en los carabineros
                                  "conf_poder_judicial"=c05_05) # Confianza en el poder judicial

# 5.4 Cambiar las etiquetas de las variables

# Variable confianza en el gobierno
proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
get_label(proc_data$conf_gob)

# Variable confianza en carabineros 
proc_data$conf_car  <- set_label(x = proc_data$conf_car, label = "Confianza: Carabineros")
get_label(proc_data$conf_car)

# Variable confianza en el poder judicial 
proc_data$conf_poder_judicial  <- set_label(x = proc_data$conf_poder_judicial, label = "Confianza: Poder Judicial")
get_label(proc_data$conf_poder_judicial)

# Variable sexo 
proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))
get_label(proc_data$sexo)
proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")

# Variable ingresos 
proc_data$ingresos <- set_label(x = proc_data$ingresos,label = "Ingresos: Ingresos")
get_label(proc_data$ingresos)

proc_data <- proc_data %>%
  mutate(
    ingresos= case_when(
      ingresos %in% 1:7 ~ "Menos de $210.000 a 510.000", 
      ingresos %in% 8:14 ~ "Entre $510.01 a $980.000",
      ingresos %in% 15:18 ~ "Entre $980.001 a $1.850.000",
      ingresos %in% 19:20 ~ "$1.850.001 y mas"
    )
  )

# 5.5 Otros ajustes

proc_data$conf_inst <- (proc_data$conf_gob+proc_data$conf_car+proc_data$conf_poder_judicial)
summary(proc_data$conf_inst)

# 5.6 Revision final 

frq(proc_data$conf_gob)

frq(proc_data$conf_car)

frq(proc_data$conf_poder_judicial)

frq(proc_data$sexo)

frq(proc_data$ingresos)

# 5.7 Reetiquetacion de valores

proc_data$conf_gob <- set_labels(proc_data$conf_gob,
                                 labels=c( "Nada"=1,
                                           "Poca"=2,
                                           "Algo"=3,
                                           "Bastante"=4,
                                           "Mucha"=5))


proc_data$conf_car <- set_labels(proc_data$conf_car,
                                 labels=c( "Nada"=1,
                                           "Poca"=2,
                                           "Algo"=3,
                                           "Bastante"=4,
                                           "Mucha"=5))

proc_data$conf_poder_judicial <- set_labels(proc_data$conf_poder_judicial,
                                            labels=c( "Nada"=1,
                                                      "Poca"=2,
                                                      "Algo"=3,
                                                      "Bastante"=4,
                                                      
                                                      "Mucha"=5))

# Revisar 

frq(proc_data$conf_gob)

frq(proc_data$conf_car)

frq(proc_data$conf_poder_judicial)

frq(proc_data$sexo)  

frq(proc_data$ingresos)


# 6.0 Generacion de la base de datos procesada 
proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

# 6.1 Guardar los datos originales
save(proc_data,file = "C:/Users/Mati/OneDrive/Escritorio/Trabajo 1 Astorga, Hermosilla y Pavez/elsoc_long_2016_2023.RData")

# 6.2 Guardar la base de datos procesada
save(proc_data,file = "C:/Users/Mati/OneDrive/Escritorio/Trabajo 1 Astorga, Hermosilla y Pavez/Procesamiento/elsoc_proc.RData")

#7. Visualización de resultados (Gráficos)
#Visualización de la variable confianza en las instituciones

# Convertir a numérico si no lo está
proc_data$conf_inst <- as.numeric(proc_data$conf_inst)

# Reemplazar los valores fuera de rango por NA
proc_data$conf_inst <- ifelse(proc_data$conf_inst >= 1 & proc_data$conf_inst <= 5, 
                              proc_data$conf_inst, NA)
graph1 <- sjPlot::plot_stackfrq(
  dplyr::select(proc_data, conf_gob, conf_car, conf_poder_judicial, conf_inst),
  title = "Confianza en instituciones"
) +
  theme(legend.position = "bottom")

graph1

#Visualización variable ingresos
ggplot()
ggplot(proc_data, aes(x = ingresos))
proc_data %>% ggplot(aes(x = ingresos)) + 
  geom_bar()
proc_data %>% ggplot(aes(x = ingresos)) + 
  geom_bar(fill = "coral")
proc_data %>% ggplot(aes(x = ingresos)) + 
  geom_bar(fill = "coral")+
  labs(title = "Estratificación",
       x = "Ingresos",
       y = "Frecuencia")
# Crear el gráfico usando ggplot2
graph2 <- proc_data %>% ggplot(aes(x = ingresos)) + 
  geom_bar(fill = "coral")+
  labs(title = "Estratificación",
       x = "Ingresos",
       y = "Frecuencia") +
  theme_bw()

graph2

summary(proc_data$ingresos)


