#Ejercicio 3:

#Primero graficamos las observaciones:
x1=c(1,1,0,5,6,4)
x2=c(4,3,4,1,2,0)
plot(x1,x2,xlim=c(-4,8), ylim=c(-4,8))

#Generamos las etiquetas:
labels=sample(1:2,6,replace=T)

#Asignamos las etiquetas a las observaciones:
data=data.frame(x1,x2,labels)

#Graficamos para ver la asignacio:
plot(x1, x2, col = (labels + 1), pch = 20, cex = 2,xlim=c(-4,8), ylim=c(-4,8))

#Mostramos la tabla de observaciones y etiquetas:
View(data)

#Calculamos los centroides:

centroid=function(data){
  #Centroide del primer cluster:
  #Calculamos la cordenada x:
  c1x=mean(data$x1[data$labels==1])
  
  #Calculamos la cordenada y:
  c1y=mean(data$x2[data$labels==1])

  #Centroide del segundo cluster:
  #Calculamos la cordenada x:
  c2x=mean(data$x1[data$labels==2])

  #Calculamos la cordenada y:
  c2y=mean(data$x2[data$labels==2])

  #Hacemos un data frame para los centroides
  cx=c(c1x,c2x)
  cy=c(c1y,c2y)
  c=data.frame(cx,cy)
  return(c)
}
c = centroid(data)
c

#Graficamos las observaciones y los centroides
plot(x1, x2, col = (labels + 1), pch = 20, cex = 2,xlim=c(-4,8), ylim=c(-4,8))
points(c,col=c(2,3),pch=4)

#Definimos una funcion para calcular distancia euclideana:
euclidean = function(x1,x2,y1,y2) sqrt((y2-y1)^2 +(x2-x1)^2 )

#Calculamos las distancias euclidianas y comprobamos las
#observaciones mas cercanas:
calculate_distances=function(c){
  list1=c()
  list2=c()
  cluster1=c()
  cluster2=c()
  i=1
  while(i<=length(data[,1])){
    list1=c(list1,euclidean(c[1,1],data[i,1],c[1,2],data[i,2]))
    list2=c(list2,euclidean(c[2,1],data[i,1],c[2,2],data[i,2]))
    if(list1[i]<list2[i]){
      cluster1=c(cluster1,i)
    } else{
      cluster2=c(cluster2,i)
    }
    i=i+1
  }

    result=list("c1"=cluster1, "c2"=cluster2)
  return (result)
}
distances = calculate_distances(c)

#Mostramos las observaciones mas cercanas al centroide 1:
cluster1 = distances$c1
cluster1

#Mostramos las observaciones mas cercanas al centroide 2:
cluster2 = distances$c2
cluster2

#Definimos la funcion actualizar etiquetas
update_labels = function(cluster1,cluster2){
  i=1
  while(i<=length(cluster1)){
    data$labels[cluster1[i]]=1
    i=i+1
  }
  i=1
  while(i<=length(cluster2)){
    data$labels[cluster2[i]]=2
    i=i+1
  } 
  return (data)
} 

data = update_labels(cluster1,cluster2)
data$labels
data
#Coloreamos de rojo las observaciones que pertenecen al primer cluster y de 
#verde las que pertenecen al segundo cluster:
plot(data$x1,data$x2,col=data$labels+1,xlim=c(-4,8), ylim=c(-4,8),pch=20)
points(c,col=c(2,3),pch=4)

#Repetimos los pasos anteriores hasta que pare de actualizar las 
#etiquetas:
while (TRUE) {
  current_labels=data$labels
  c = centroid(data)
  distances = calculate_distances(c)
  cluster1 = distances$c1
  cluster2 = distances$c2
  data = update_labels(cluster1,cluster2)
  new_labels=data$labels
  if(sum(current_labels==new_labels)==6){
    break
  }
}
data

#Coloreamos de rojo las observaciones que pertenecen al primer cluster y de 
#verde las que pertenecen al segundo cluster:
plot(data$x1,data$x2,col=data$labels+1,xlim=c(-4,8), ylim=c(-4,8),pch=20)
points(c,col=c(2,3),pch=4)


#Agregamos leyenda
plot(data$x1,data$x2,col=data$labels+1,xlim=c(-4,8), ylim=c(-4,8),pch=20)
legend("bottomleft", 
       legend = c("Cluster 1", "Cluster 2"), 
       col = c(2, 3), 
       pch = c(20,20), 
       bty = "n",
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F ) 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Ejercicio 9:
library(tidyverse) 
library(cluster)    
library(h2o)
library(factoextra)

#Cargamos la base de datos y guardamos la informacion dentro
#de la variable estados:
estados=USArrests 

#Veamos la base de datos:
View(estados)

#Usando agrupamiento jerarquico con enlace completo, vamos a agrupar los 
#los estados:
hclust_data = hclust( dist(estados, method = "euclidean"), method="complete" )

#Graficamos
plot( hclust_data, xlab="", sub="", cex=0.8 )

#Cortamos ahora en la altura que determine 3 agrupamientos:
cut1 = cutree( hclust_data, k=3 ) 

#Revisamos que altura tomar para el corte:
sort(hclust_data$height)

#Comprobamos que en efecto se hace bien el corte:
plot(hclust_data, xlab="", sub="", cex=0.8)
rect.hclust(hclust_data , k = 3, border = 2:6)
abline(h = 150, col = 'red')

#Imprimimos los estados segun la etiqueta:
for( k in 1:3 ){
  print(k)
  print( rownames( estados )[ cut1 == k ] )
}

#Veamoslo graficamente:
suppressPackageStartupMessages(library(dendextend))
data_dend1= as.dendrogram(hclust_data)
data_col_dend1 = color_branches(data_dend1, h = 150)
plot(data_col_dend1, xlab="", sub="", cex=0.9)

#Ahora vamos a escalar las variables y hacemos agrupamietno jerarquico:
hclust_data_scale = hclust( dist(scale(estados),method = "euclidean"), method="complete" )

#Graficamos
plot( hclust_data_scale, xlab="", sub="", cex=0.9 )

#Cortamos ahora en la altura que determine 3 agrupamientos:
cut2 = cutree( hclust_data_scale, k=3 )

#Revisamos que altura tomar para el corte:
sort(hclust_data_scale$height)

#Comprobamos que en efecto se hace bien el corte:
plot(hclust_data_scale, xlab="", sub="", cex=0.8)
rect.hclust(hclust_data_scale , k = 3, border = 2:5)
abline(h = 4.42, col = 'red')

#Imprimimos los estados segun la etiqueta:
for( k in 1:3 ){
  print(k)
  print( rownames( estados )[ cut2 == k ] )
}

#Imprimimos la tabla de contingencia:
table(cutree(hclust_data, 3), cutree(hclust_data_scale, 3))

#Veamoslo graficamente:
data_dend2 = as.dendrogram(hclust_data_scale)
data_col_dend2 = color_branches(data_dend2, h = 4.42)
plot(data_col_dend2, xlab="", sub="", cex=0.9)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Ejercicio 10:

K = 3  # the number of classes
n = 20 # the number of samples per class
p = 50 # the number of variables 

#Generamos el dataset simulado:
# Create data for class 1: 
x_1 = matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){
  x_1[row,] = x_1[row,] + rep( 1, p ) 
}

# Create data for class 2: 
x_2 = matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){
  x_2[row,] = x_2[row,] + rep( -1, p ) 
}

# Create data for class 3: 
x_3 = matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){
  x_3[row,] = x_3[row,] + c( rep( +1, p/2 ), rep( -1, p/2 ) ) 
}

x= rbind( x_1, x_2, x_3 )

#Asignamos la etiquetas:
labels = c( rep(1,n), rep(2,n), rep(3,n) )

#Realizamos pca con el comando prcomp:
pca = prcomp(x) 

#Graficamos para observar los agrupamientos:
plot( pca$x[,1], pca$x[,2], col=labels, pch=19,sub = "")

#Hacemos kmean con k=3:
kmean3 = kmeans( x, centers=3, nstart=50 )

#Realizamos la tabla de contingencia:
table( kmean3$cluster, labels )

#Graficamos:
fviz_cluster(kmean3, data = x,
             palette = c("#2E9FDF", "#BF0609", "#E7B800"), 
             geom = "point",
             ellipse.type = FALSE, 
             ggtheme = theme_bw(),xlab = FALSE,ylab = FALSE
)

#Hacemos kmean con k=2:
kmean2 = kmeans( x, centers=2, nstart=50 )

#Realizamos la tabla de contingencia:
table( kmean2$cluster, labels )

#Graficamos:
fviz_cluster(kmean2, data = x,
             palette = c("#2E9FDF", "#BF0609"), 
             geom = "point",
             ellipse.type =FALSE, 
             ggtheme = theme_bw(),xlab = FALSE,ylab = FALSE
)

#Hacemos kmean con k=4:
kmean4 = kmeans( x, centers=4, nstart=50 )

#Realizamos la tabla de contingencia:
table( kmean4$cluster, labels )

#Graficamos:
fviz_cluster(kmean4, data = x,
             palette = c("#2E9FDF", "#BF0609", "#E7B800","#0A0909"), 
             geom = "point",
             ellipse.type = FALSE, 
             ggtheme = theme_bw(),xlab = FALSE,ylab = FALSE
)

#Hacemos kmean con k=3 a las 2 primeras componentes del pca:
kmeanpca = kmeans( pca$x[,c(1,2)], centers=3, nstart=50 )

#Realizamos la tabla de contingencia:
table( kmeanpca$cluster, labels )

#Graficamos:
fviz_cluster(kmeanpca, data = pca$x[,c(1,2)],
             palette = c("#2E9FDF", "#BF0609", "#E7B800"), 
             geom = "point",
             ellipse.type = FALSE, 
             ggtheme = theme_bw(),xlab = FALSE,ylab = FALSE
)

##Hacemos kmean con k=3 a los datos escalados:
x_scaled = scale( x ) 
kmean_scaled = kmeans( x_scaled, 3, nstart=50 )

#Realizamos la tabla de contingencia:
table( kmean_scaled$cluster, labels )

#Graficamos:
fviz_cluster(kmean_scaled, data = x_scaled,
             palette = c("#2E9FDF", "#BF0609", "#E7B800"), 
             geom = "point",
             ellipse.type = FALSE, 
             ggtheme = theme_bw(),xlab = FALSE,ylab = FALSE
)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Ejercicio 11

#Cargamos la base de datos:
fp<-file.path("C:/Users/Jose Fuentes/Desktop/Posgrado/Tercer Semestre/Aprendizaje estadístico automatizado/Tareas/Tarea 2/Ch10Ex11.csv")
BD<-read.csv(fp,header=FALSE)

#Hacemos la transpuesta de la base de datos, de forma que los elementos de las 
#filas tengan relacion:
BD=t(BD)
View(BD)

#Comprobamos para distintos linkage:
hclust_complete = hclust(get_dist(BD,method = "pearson"), method="complete" )
hclust_single = hclust(get_dist(BD,method = "pearson"), method="single" )
hclust_average = hclust(get_dist(BD,method = "pearson"), method="average" )

#Graficamos cada uno:
plot( hclust_complete, xlab="", sub="", cex=0.9 )
plot( hclust_single, xlab="", sub="", cex=0.9 )
plot( hclust_average, xlab="", sub="", cex=0.9 )

#Realizamos pca a los datos:
pr_out <- prcomp(BD)
head(pr_out$rotation)

#Comprobamos los genes que difieren mas:
total_load <- apply(pr_out$rotation, 1, sum)
index <- order(abs(total_load), decreasing = TRUE)
index[1:6]
#La variable index nos dice de mayot a menor los genes que mas difieren, siendo
#estos 865, 68, 911, 428, 624,  11.
