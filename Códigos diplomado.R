#Evaluacion diplomado con analisis de base de datos. 

#install.packages("dplyr")
library(dplyr)
library(readxl)
#install.packages("tidyr")
library(tidyr)
library(ggplot2)

library(gridExtra)

# Importar base de datos

B_D_CAT_new$Fecha <- as.Date(B_D_CAT_new$Fecha,format = "%d/%m/%Y")


# Transformar variables tipo chr a factor
salmon$Sample <- as.factor(salmon$Sample)
salmon$Ploidy <- as.factor(salmon$Ploidy)
salmon$Family <- as.factor(salmon$Family)
salmon$Tank <- as.factor(salmon$Tank)

# Identificar datos atípicos, duplicados o faltantes (NA) con summary
summary(B_D_CAT)

# Ver la dimensión de la nueva base de datos con datos faltantes
dim(B_D_CAT)

# Omitir/quitar datos faltantes na.omit()
B_D_CAT_new <- na.omit(B_D_CAT)

# Ver la dimensión de la nueva base de datos sin datos faltantes 
dim(B_D_CAT_new)

#resumen de nueva base de datos B_D_CAT_new
summary(B_D_CAT_new)

B_D_CAT_new$Fecha <- as.Date(B_D_CAT_new$Fecha,format = "%d/%m/%Y")

class(B_D_CAT_$Fecha)

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

hist(BDC_categoria$IDC)

colnames(BDC_categoria)

plot(ecdf(BDC_categoria$IDC))

#tamaño de los efectos 

plot.design(BDC_categoria $Peso ~ BDC_categoria$cat_catarata + BDC_categoria $Jaula)


