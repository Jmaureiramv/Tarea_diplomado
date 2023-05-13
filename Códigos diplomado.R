#Evaluacion diplomado con analisis de base de datos. 

install.packages("dplyr")
library(dplyr)
library(readxl)
install.packages("tidyr")
library(tidyr)
library(ggplot2)

library(gridExtra)

# Habilita paquetes
library(readxl) # Para importar datos a R

library(dplyr) # Para manipular datos

library(tidyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos

library(gridExtra)

# Importar base de datos
salmon <- read_excel("salmon.xlsx", na = "NA")
B_D_CAT_new$Fecha <- as.Date(B_D_CAT_new$Fecha,format = "%d/%m/%Y")
# Ordenar la variable Sample de menor a mayor usando arrange
salmon <- salmon%>% 
  arrange(Sample)

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






