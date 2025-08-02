#carga de paquetes
library(tidyverse)
library(ggplot2)
library(dplyr)
install.packages("treemapify")
library(treemapify)
install.packages("rmarkdown")
library(rmarkdown)
--------------------------------------------------------------------------------
  
#carga de tablas
usu_individual_t416 <-  read.csv("C:/Users/martin/Documents/TABLAS RSTUDIO/01 PROYECCION LABORAL ARGENTINA/usu_individual_t416.csv")
usu_individual_T424 <-  read.csv("C:/Users/martin/Documents/TABLAS RSTUDIO/01 PROYECCION LABORAL ARGENTINA/usu_individual_T424.csv")
usu_hogar_t416 <-  read.csv("C:/Users/martin/Documents/TABLAS RSTUDIO/01 PROYECCION LABORAL ARGENTINA/usu_hogar_t416.csv")
usu_hogar_T424 <-  read.csv("C:/Users/martin/Documents/TABLAS RSTUDIO/01 PROYECCION LABORAL ARGENTINA/usu_hogar_T424.csv")
--------------------------------------------------------------------------------

#elimino filas y columnas no relevantes 
usu_individual_t416 <- usu_individual_t416 %>% select(CODUSU,NIVEL_ED)
usu_individual_t416 <- subset(usu_individual_t416, NIVEL_ED<=7)
--------------------------------------------------------------------------------

#filtrar por maximo nivel educativo alcanzado y renombrar columnas
nivel_estudio_16 <- usu_individual_t416 %>% 
  group_by(CODUSU) %>% 
  filter(NIVEL_ED==max(NIVEL_ED)) %>%                                                                     
  distinct()
colnames(nivel_estudio_16) <- c("CODUSU","NIVEL_ED")
--------------------------------------------------------------------------------

#crear nueva tabla eliminando columna "rn"
nivel_estudio_24 <- usu_individual_T424[-c(3)]
--------------------------------------------------------------------------------

#crear planillas de familia con columnas relevantes
familia_16 <- usu_hogar_t416[c(1,2,66,68)]
familia_24 <- usu_hogar_T424[c(1,2,70,72)]
--------------------------------------------------------------------------------

#LIMPIEZA DE TABLAS FAMILIA (renombre de variables y eliminacion de filas)
colnames(familia_16) <- c("CODUSU","AÑO","NIÑOS","INGRESO")
familia_16 <- subset(familia_16, INGRESO>0)
familia_16 <- subset(familia_16, INGRESO!="ITF")
colnames(familia_24) <- c("CODUSU","AÑO","NIÑOS","INGRESO")
familia_24 <- subset(familia_24, INGRESO>0)
familia_24 <- subset(familia_24, INGRESO!="ITF")
--------------------------------------------------------------------------------

#JUNTAR TABLAS
educacion_familia_16 <- right_join(nivel_estudio_16, familia_16, by="CODUSU")
educacion_familia_24 <- right_join(nivel_estudio_24, familia_24, by="CODUSU")
--------------------------------------------------------------------------------

#limpieza de filas
educacion_familia_16 <- subset(educacion_familia_16, AÑO==2016)
educacion_familia_24 <- subset(educacion_familia_24, AÑO==2024)
--------------------------------------------------------------------------------

#crear columna EDUCACION y clasificar por nivel alcanzado
educacion_familia_16 <- educacion_familia_16 %>% 
  mutate(EDUCACION = case_when(
    NIVEL_ED<=3 ~ "primario",
    NIVEL_ED>=4 & NIVEL_ED<=5 ~ "secundario",
    NIVEL_ED>=6 ~ "universitario",
    TRUE ~ "NULL"
  ))

educacion_familia_24 <- educacion_familia_24 %>% 
  mutate(EDUCACION = case_when(
    NIVEL_ED<=3 ~ "primario",
    NIVEL_ED>=4 & NIVEL_ED<=5 ~ "secundario",
    NIVEL_ED>=6 ~ "universitario",
    TRUE ~ "NULL"
  ))
--------------------------------------------------------------------------------

# AGREGAR CANASTA BASICA FAMILIAR
educacion_familia_16 <- educacion_familia_16 %>% 
  mutate(CBF = case_when(
    NIÑOS < 1 ~ 10450,
    NIÑOS > 1 ~ 13805,
    TRUE  ~ 13126 #SON IGUAL A 1
  ))

educacion_familia_24 <- educacion_familia_24 %>% 
  mutate(CBF = case_when(
    NIÑOS < 1 ~ 797283,
    NIÑOS > 1 ~ 1053322,
    TRUE  ~ 1001466 #SON IGUAL A 1
  ))
--------------------------------------------------------------------------------

#convertir cadena de texto en numeros para luego incorporar linea pobreza con 
#formula
educacion_familia_16$INGRESO <- as.numeric(educacion_familia_16$INGRESO)

educacion_familia_16 <- educacion_familia_16 %>% 
  mutate(LINEA_POBREZA = case_when(
    INGRESO-CBF<0 ~ "debajo",
    INGRESO-CBF>=0 ~ "sobre",
    TRUE ~ "NULL"
  ))

educacion_familia_24$INGRESO <- as.numeric(educacion_familia_24$INGRESO)

educacion_familia_24 <- educacion_familia_24 %>% 
  mutate(LINEA_POBREZA = case_when(
    INGRESO-CBF<0 ~ "debajo",
    INGRESO-CBF>=0 ~ "sobre",
    TRUE ~ "NULL"
  ))
--------------------------------------------------------------------------------

#juntar tablas de manera vertical 
educacion_familia_final <- rbind(educacion_familia_16,educacion_familia_24) 
--------------------------------------------------------------------------------
  

#resumen agrupado por categorias  
educacion_familia_final <-  educacion_familia_final %>% 
  group_by(AÑO, NIÑOS, EDUCACION, LINEA_POBREZA) %>% 
  summarise(n=n())
--------------------------------------------------------------------------------
  
# limpieza de filas  
educacion_familia_final <- subset(educacion_familia_final, NIÑOS<=5)
educacion_familia_final <- subset(educacion_familia_final, EDUCACION!="NULL")
educacion_familia_final <- na.omit(educacion_familia_final) #elimina cualquier 
#fila con valor null
--------------------------------------------------------------------------------

#GRAFICO NRO 1  
  
# creacion de marco de datos para graficar la evolucion educativa   
grafico_educacion <- educacion_familia_final
  
grafico_educacion$total_niños <- grafico_educacion$NIÑOS*grafico_educacion$n

grafico_educacion <- subset(grafico_educacion, total_niños>0)

grafico_educacion <- grafico_educacion %>% 
  group_by(AÑO, EDUCACION) %>% 
  summarize(
    niños_educ = sum(total_niños, na.rm = TRUE)
  )
  
# grafico educacion
ggplot(data = grafico_educacion)+
  geom_line(mapping = aes(x=AÑO, y=niños_educ, group = EDUCACION, colour = EDUCACION))+
  theme_bw()+
  labs(title = "Niños según el maximo nivel educativo alcanzado por los Padres")
--------------------------------------------------------------------------------

  #GRAFICO NRO2  
  
#preparando tablas para graficar niños por familia
grafico_niños_hogar <- educacion_familia_final %>% 
  group_by(AÑO, NIÑOS) %>% 
  summarize(
    hijos_por_familia = sum(n, na.rm = TRUE)
  )

grafico_niños_hogar16 <- subset(grafico_niños_hogar, AÑO=="2016")
grafico_niños_hogar16$porcentaje <- grafico_niños_hogar16$hijos_por_familia/sum(grafico_niños_hogar16$hijos_por_familia)*100
grafico_niños_hogar16$porcentaje <- round(grafico_niños_hogar16$porcentaje, digits = 2)
grafico_niños_hogar24 <- subset(grafico_niños_hogar, AÑO=="2024")
grafico_niños_hogar24$porcentaje <- grafico_niños_hogar24$hijos_por_familia/sum(grafico_niños_hogar24$hijos_por_familia)*100
grafico_niños_hogar24$porcentaje <- round(grafico_niños_hogar24$porcentaje, digits = 2)

#graficos de cuadros
ggplot(grafico_niños_hogar16, aes(area=hijos_por_familia, fill = NIÑOS, label=porcentaje))+
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "centre")+
  scale_fill_viridis_c()+
  labs(title = "Cantidad de niños por familia en 2016")
  
ggplot(grafico_niños_hogar24, aes(area=hijos_por_familia, fill = NIÑOS, label=porcentaje))+
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "centre")+
  scale_fill_viridis_c()+
  labs(title = "Cantidad de niños por familia en 2024")
--------------------------------------------------------------------------------

#GRAFICO NRO 3

  #preparando tablas para graficar  
grafico_linea_pobreza <- educacion_familia_final %>% 
  group_by(EDUCACION, LINEA_POBREZA) %>% 
  summarize(
    educacion_pobreza = sum(n, na.rm = TRUE)
  )

grafico_linea_pobreza$porcentaje <- grafico_linea_pobreza$educacion_pobreza/sum(grafico_linea_pobreza$educacion_pobreza)*100
grafico_linea_pobreza$porcentaje <- round(grafico_linea_pobreza$porcentaje, digits = 2)

#grafico de batrras
ggplot(grafico_linea_pobreza, aes(x=EDUCACION, y=porcentaje, fill = LINEA_POBREZA))+
  geom_bar(stat = "identity")+
  labs(title = "Familias debajo/sobre linea de pobreza segun estudios alcanzados")
--------------------------------------------------------------------------------

tirando magia como para ver que onda 
