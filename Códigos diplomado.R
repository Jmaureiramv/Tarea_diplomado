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
#install.packages("lubridate")
library(lubridate)

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


# Histograma de categoria catartas (grafico de barras) 

G1 <- BDC_categoria %>%
  group_by(IDC) %>%
  count() %>%
  ggplot(aes(x = IDC, y = n)) +
  geom_col(fill = "blue") +
  labs(title = "Indice de desarrollo de cataratas",
       x = "Puntuación del IDC",
       y = "Frecuencia de datos")

G2<- BDC_categoria %>%
  group_by(cat_catarata) %>%
  count() %>%
ggplot(aes(x = cat_catarata, y = n)) +
  geom_col(fill = "blue") +
  labs(title = "Categorias del Indice de desarrollo de cataratas",
       x = "Categoria",
       y = "Frecuencia de datos")

grid.arrange(G1,G2,ncol=2)

# Histograma de K
ggplot(BDC_categoria, aes(x = K)) + 
  geom_histogram(col='black', fill='green', alpha=0.4)
#Historama de peso

ggplot(BDC_categoria, aes(x = Peso)) + 
  geom_histogram(col='black', fill='green', alpha=0.4)

#Graficos de distribucion empirica 

plot(ecdf(BDC_categoria$IDC),main="Distribucion empirica IDC",xlab="IDC")

plot(ecdf(BDC_categoria$K),main="Distribucion empirica K",xlab="K")

plot(ecdf(BDC_categoria$Peso),main="Distribucion empirica peso",xlab="Peso")

grid.arrange(G1,G2,G3,ncol=3)



#boxplot
Y1 <- ggplot(BDC_categoria,aes(cat_catarata,K))+
  geom_boxplot(fill="blue", alpha= 0.4)+
  labs(x = "Categoria Catartas",title = "Indice condicion v/s Categoria cataratas")+
  theme(plot.title= element_text(size = 17),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

#boxplot peso vs cAT_catarata
Y2 <- ggplot(BDC_categoria,aes(cat_catarata,Peso))+
  geom_boxplot(fill="blue", alpha= 0.4)+
  theme(plot.title= element_text(size = 17),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

grid.arrange(Y1,Y2,ncol=2)


# Boxplot jaua categoria K
T1 <- ggplot(BDC_categoria,aes(Jaula,K,
                         fill=Jaula))+
geom_boxplot()+facet_wrap(~cat_catarata)+
  theme(legend.position = "none")

# Boxplot 
T2 <- ggplot(BDC_categoria,aes(Jaula,K,
                         fill=cat_catarata))+
  geom_boxplot()

grid.arrange(T1,T2,ncol=1)

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

categoria <- BDC_categoria $cat_catarata
Jaula <- BDC_categoria $Jaula
plot.design(BDC_categoria $K ~ categoria + Jaula, 
xlab = "Factores", ylab = "Indice condicion (K)")



colnames(BDC_categoria)


BDC2 <- B_D_CAT %>% 
  mutate(cat_catarata=case_when(IDC==0~"normal",
                                IDC > 0 & IDC <= 8 ~ "leve",
                                IDC > 8 & IDC <=16 ~ "moderado",
                                IDC >16 & IDC <=24 ~ "severo")) %>% 
  mutate(cat_catarata= as.factor(cat_catarata)) %>%
  mutate(Jaula= as.factor(Jaula)) %>%
  mutate(Centro= as.factor(Centro)) %>% 
  mutate (IDC = as.factor(IDC)) %>% 
  BDC2$Fecha <- as.Date(BDC2$Fecha,format = "%d/%m/%Y")

 BDC2$mes_anio <- format (BDC2$Fecha, "%Y-%m") 
 

 BDC2 <- BDC2 %>%
   mutate (mes_anio = as.factor(mes_anio))

summary(BDC2)

BDC2 %>%
  group_by(mes_anio) %>%
  count(cat_catarata) %>%
  ggplot(aes(x = mes_anio, y = n)) +
  geom_col(fill = "blue") +
  labs(title = "",
       x = "Mes de muestreo",
       y = "Frecuencia de datos")


ggplot(BDC2,aes(mes_anio,cat_catarata))+
  geom_boxplot(fill="blue", alpha= 0.4)+
  labs(x = "Categoria Catartas",title = "Indice condicion v/s Categoria cataratas")+
  theme(plot.title= element_text(size = 17),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14))

datos_acumulados <- BDC2 %>%
  group_by(mes_anio,cat_catarata) %>%
  summarise(recuento = n()) %>%
  group_by(mes_anio) %>%
  mutate(acumulado = cumsum(recuento))

ggplot(datos_acumulados, aes(x = mes_anio, y = acumulado, fill =  cat_catarata)) +
  geom_area(position = "stack") +
  labs(x = "Mes", y = "Recuento acumulado", title = "Indice desarrollo de cataratas Loncochalgua", fill = "Categoría") +
  scale_x_discrete(labels = function(x) format(as.Date(paste0(x, "-01")), "%b %Y"))

datos_acumulados <- left_join(datos_acumulados, BDC2, by = "mes")





barplot(table(BDC_categoria$cat_catarata))

datos_acumulados <- na.omit(datos_acumulados)

datos_acumulados <- datos_acumulados %>%
  mutate(porcentaje = acumulado / sum(acumulado) * 100)
# Orden logico de las categorias 


datos_acumulados$cat_catarata <- factor(datos_acumulados$cat_catarata, levels = c("severo", "moderado","leve","normal"))



ggplot(datos_acumulados, aes(x = mes_anio, y = porcentaje, fill = cat_catarata, label = porcentaje)) +
  geom_bar(stat = "identity") +
  labs(x = "Mes", y = "Recuento acumulado", fill = "Categoría") +
  scale_x_discrete(labels = function(x) format(as.Date(paste0(x, "-01")), "%b %Y")) +
  geom_text(position = position_stack(vjust = 0.5), size = 3)

ggplot(datos_acumulados, aes(x = mes_anio, y = porcentaje, fill = cat_catarata)) +
  geom_bar(stat = "identity") +
  labs(x = "Mes", y = "Recuento acumulado", fill = "Categoría") +
  scale_x_discrete(labels = function(x) format(as.Date(paste0(x, "-01")), "%b %Y"))

ggplot(datos_acumulados, aes(x = mes_anio, y = porcentaje, fill = cat_catarata)) +
  geom_bar(stat = "identity") +
  labs(x = "Mes", y = "Recuento acumulado", fill = "Categoría", title = "Gráfico de barras acumuladas") +
  scale_x_discrete(labels = function(x) format(as.Date(paste0(x, "-01")), "%b %Y")) +
  scale_fill_manual(values = c("normal" = "blue", "leve" = "green", "moderado" = "yellow", "severo" = "red"))

ggplot(datos_acumulados, aes(x = mes_anio, y = porcentaje, fill = cat_catarata)) +
  geom_bar(stat = "identity") +
  labs(x = "Mes", y = "Recuento acumulado en %", fill = "Categoría catarata", title = "Indice desarrollo cataratas Loncochalgua") +
  scale_x_discrete(labels = function(x) format(as.Date(paste0(x, "-01")), "%b %Y"),
                   breaks = unique(datos_acumulados$mes_anio)) +
  scale_fill_manual(values = c("normal" = alpha("blue", 0.6),
                               "leve" = alpha("green", 0.6),
                               "moderado" = alpha("yellow", 0.6),
                               "severo" = alpha("red", 0.6)))




summary(datos_acumulados)                
                   
                   
                   
                   
                   