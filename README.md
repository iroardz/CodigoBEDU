# CodigoBEDU
Proyecto final


#Librería para leer la base de datos.
library("readr")
library("scales")
install.packages("ggplot2")
library(ggplot2)
install.packages("vctrs")
#Establecer directorio de trabajo.
setwd("~/BEDU/proyecto")

#Importar la base de datos.

BaseDatos <- read_excel("ratios.xlsx", range = "B2:AB122")

#Media de las variables.
apply(X = BaseDatos, MARGIN = 2, FUN = mean)

#Varianza de las variables.
apply(X = BaseDatos, MARGIN = 2, FUN = var)

#Estandarizar (Reescalar) las variables.
Base_Datos <- data.frame(lapply(BaseDatos,function(x) rescale(x)))

#Análisis de Componentes Principales
PCA <- prcomp(Base_Datos)
PCA2 <- princomp(Base_Datos)

#Nuevas Variables
PCA_Base <- PCA$rotation
write.csv(PCA_Base, file="PCA.csv")

#Gráfica Bidimensional
biplot(x=PCA,scale=0,cex=0.6,col=c("blue","brown"))

#Varianza Explicada por el PCA
Pr_Varianza <- PCA$sdev^2 / sum(PCA$sdev^2)
Pr_Varianza

Pr_Varianza_Acum <- cumsum(Pr_Varianza)
Pr_Varianza_Acum

#Gráfica de Varianza Explicada
ggplot(data = data.frame(Pr_Varianza_Acum, pc = 1:24),
       aes(x = pc, y = Pr_Varianza_Acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")
plot(PCA)
plot(PCA$sdev)
summary(PCA2)
PCA2$loadings


#Librería para leer la base de datos.
install.packages("ggplot")
library("ggplot2")
install.packages("factoextra")
library("factoextra")
library("cluster")
library("igraph")
library("tidygraph")
library("ggraph")
library("scales")
library("mclust")
library("dbscan")
library("fpc")
library("clustertend")
library("tidyverse")  # data manipulation



####################################################################codigo2

#Establecer directorio de trabajo.
setwd("~/BEDU/proyecto")


#Importar la base de datos.
BaseDatos <- read_excel("ratios.xlsx", range = "B2:AB122")
BaseDatos <- BaseDatos[,2:7]

#############################################################################

#Valuación del Clústering
set.seed(321)
hopkins(data = BaseDatos, n = nrow(BaseDatos) - 1)

#############################################################################

#Definir el número de clústers.
fviz_nbclust(x = BaseDatos, FUNcluster = cluster::pam, method = "wss", k.max = 10,
             diss = dist(BaseDatos, method = "manhattan"))


#Clústering K-Medoids
set.seed(123)
pam_clusters <- pam(x = BaseDatos, k = 4, metric = "manhattan")
pam_clusters


#Gráfica de Clústering K-Medoids
fviz_cluster(object = pam_clusters, data = BaseDatos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Análisis de Conglomerados (K-Medoids)") +
  theme(legend.position = "none")

write.csv(pam_clusters$clustering, file="Clústers.csv")

