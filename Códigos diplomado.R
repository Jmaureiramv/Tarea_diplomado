#Evaluacion diplomado con analisis de base de datos. 

#install.packages("dplyr")
library(dplyr)
library(readxl)
#install.packages("tidyr")
library(tidyr)
library(ggplot2)

library(gridExtra)

# 1° Importar base de datos

B_D_CAT <- read_excel("B_D_CAT.xlsx", col_types = c("text", 
                                                    "date", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric"))
View(B_D_CAT)

# 2° Identificar datos atípicos, duplicados o faltantes (NA) con summary

summary(B_D_CAT)


# 3° Omitir/quitar datos faltantes na.omit()
B_D_CAT_new <- na.omit(B_D_CAT)

# 4° Ver la dimensión de la nueva base de datos con datos faltantes
dim(B_D_CAT)

# 5° Ver la dimensión de la nueva base de datos sin datos faltantes 
dim(B_D_CAT_new)

# Transformar variables tipo chr a factor

salmon$Ploidy <- as.factor(salmon$Ploidy)
salmon$Family <- as.factor(salmon$Family)
salmon$Tank <- as.factor(salmon$Tank)

# Dar formato a la fecha en la nueva base de datos

B_D_CAT_new$Fecha <- as.Date(B_D_CAT_new$Fecha,format = "%d/%m/%Y")


#resumen de nueva base de datos B_D_CAT_new
summary(B_D_CAT_new)

B_D_CAT_new$Fecha <- as.Date(B_D_CAT_new$Fecha,format = "%d/%m/%Y")


BDC_categoria <- B_D_CAT_new %>% 
  mutate(cat_catarata=case_when(IDC==0~"normal",
                                IDC > 0 & IDC <= 8 ~ "leve",
                                IDC > 8 & IDC <=16 ~ "moderado",
                                IDC >16 & IDC <=24 ~ "severo")) %>% 
  mutate(cat_catarata= as.factor(cat_catarata)) %>%
  mutate(Jaula= as.factor(Jaula)) %>%
  mutate(Centro= as.factor(Centro)) %>% 
  mutate (IDC = as.factor(IDC))

summary(BDC_categoria)

# Grafica de barras 

BDC_categoria$cat_catarata <- factor(BDC_categoria$cat_catarata, levels = c("normal", "leve", "moderado", "severo"))
barplot(table(BDC_categoria$cat_catarata))



BDC_categoria %>%
  group_by(cat_catarata) %>%
  count() %>%
ggplot(aes(x = cat_catarata, y = n)) +
  geom_col(fill = "blue") +
  labs(title = "Gráfico de Barras",
       x = "Categoria",
       y = "Frecuencia de datos")



table(BDC_categoria$cat_catarata)
# 
#boxplot peso vs cAT_catarata
ggplot(BDC_categoria,aes(cat_catarata,Peso))+
  geom_boxplot()

#boxplot
ggplot(BDC_categoria,aes(cat_catarata,K))+
  geom_boxplot()

ggplot(BDC_categoria,aes(cat_catarata,K,
                         fill=Jaula))+
geom_boxplot()

ggplot(BDC_categoria,aes(Jaula,K,
                         fill=cat_catarata))+
  geom_boxplot()

#Respuesta 4

#tabla frecuencia
table(BDC_categoria$cat_catarata)

#tabla frecuencia Jaula
table(BDC_categoria$Jaula)

#tabla frecuencia Centro 
table(BDC_categoria$Centro)

#grafico de dispersion 

#respuesta pregunta 3

hist(BDC_categoria$Peso)

hist(BDC_categoria$K)

barplot(frecuencia, names.arg = grados_cataratas, 
        main = "Grados de Cataratas",
        xlab = "Grado",
        ylab = "Frecuencia",
        col = "blue",
        border = "white")

#Cataratas_resumen <- table(datos_all$Nivel_cataratas)
#barplot(Cataratas_resumen)

#datos_all$Nivel_cataratas <- factor(datos_all$Nivel_cataratas, levels = c("Bajo", "Medio", "Alto"))
#barplot(table(datos_all$Nivel_cataratas))

BDC_categoria$cat_catarata <- factor(BDC_categoria$cat_catarata, levels = c("Normal","Leve", "Moderado", "Severo"))
barplot(table(BDC_categoria$cat_catarata))


colnames(BDC_categoria)

plot(ecdf(BDC_categoria)

#tamaño de los efectos 

plot.design(BDC_categoria $Peso ~ BDC_categoria$cat_catarata + BDC_categoria $Jaula)


