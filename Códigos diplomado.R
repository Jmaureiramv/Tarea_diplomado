#Evaluacion diplomado con analisis de base de datos. 

#install.packages("dplyr")
library(dplyr)
library(readxl)
#install.packages("tidyr")
library(tidyr)
library(ggplot2)
library(gridExtra)
#install.packages("knitr")
library(knitr)

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
  mutate (IDC = as.factor(IDC)) %>% 
  filter(K < 2.5)
# Quitar datos no logicos y extremos

summary(BDC_categoria)

# Orden logico de las categorias 

BDC_categoria$cat_catarata <- factor(BDC_categoria$cat_catarata, levels = c("normal", "leve", "moderado", "severo"))
barplot(table(BDC_categoria$cat_catarata))


# Histograma de categoria catartas 
BDC_categoria %>%
  group_by(cat_catarata) %>%
  count() %>%
ggplot(aes(x = cat_catarata, y = n)) +
  geom_col(fill = "blue") +
  labs(title = "Gráfico de Barras",
       x = "Categoria",
       y = "Frecuencia de datos")

# Histograma de K
ggplot(BDC_categoria, aes(x = K)) + 
  geom_histogram(col='black', fill='green', alpha=0.4)
#Historama de peso

ggplot(BDC_categoria, aes(x = Peso)) + 
  geom_histogram(col='black', fill='green', alpha=0.4)

#Graficos de distribucion empirica 

plot(ecdf(BDC_categoria$K),main="Distribucion empirica K",xlab="K")

plot(ecdf(BDC_categoria$Peso),main="Distribucion empirica peso",xlab="Peso")


#boxplot
ggplot(BDC_categoria,aes(cat_catarata,K))+
  geom_boxplot(fill="blue", alpha= 0.4)

#boxplot peso vs cAT_catarata
ggplot(BDC_categoria,aes(cat_catarata,Peso))+
  geom_boxplot(fill="blue", alpha= 0.4)


# Boxplot jaua categoria K
ggplot(BDC_categoria,aes(cat_catarata,K,
                         fill=Jaula))+
geom_boxplot()

# Boxplot 
ggplot(BDC_categoria,aes(Jaula,K,
                         fill=cat_catarata))+
  geom_boxplot()

#Respuesta 4

# Resumen de tabla estadistica descriptiva 
BDC_categoria %>% group_by(cat_catarata) %>%  
  summarise(n=n(),promedio=mean(K),desv=sd(K),
            mediana=median(K),valor_maximo=max(K), 
            valor_minimo=min(K)) %>% kable(align="c",digits = 2)

BDC_categoria %>% group_by(Jaula) %>%  
  summarise(n=n(),promedio=mean(K),desv=sd(K),
            mediana=median(K),valor_maximo=max(K), 
            valor_minimo=min(K)) %>% kable(align="c",digits = 2)

#tabla frecuencia
table(BDC_categoria$cat_catarata) %>% 
  kable(align="c",digits = 2)

#tabla frecuencia Jaula
table(BDC_categoria$Jaula) %>% 
  kable(align="c",digits = 2)

#tabla frecuencia Centro 
table(BDC_categoria$Centro)

#grafico de dispersion 

#respuesta pregunta 3


hist(BDC_categoria$K)

hist(BDC_categoria$Peso)

ggplot(BDC_categoria, aes(x = K)) + 
  geom_histogram(col='black', fill='green', alpha=0.4)

ggplot(BDC_categoria, aes(x = Peso)) + 
  geom_histogram(col='black', fill='green', alpha=0.4)

#tamaño de los efectos 

plot.design(BDC_categoria $Peso ~ BDC_categoria$cat_catarata + BDC_categoria $Jaula)

colnames(BDC_categoria)







