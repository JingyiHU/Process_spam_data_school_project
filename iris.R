##########################################
###      2. Application - Iris         ###
##########################################

######### Librairies necessaires ########

source("kmeans_adp.R")
library(mclust)
library(cluster)
library(plyr)

############################Classification par K-means avec distance adaptative###############################

data(iris)

test = iris
dim(test) # 150 4
iris_quant = iris[,-5] # A data.frame
ktest = as.matrix(iris_quant) # Turn into a matrix
resu = Kmeans.adapt(ktest, 1)

species_colors = mapvalues(iris$Species, from = c("setosa", "versicolor", "virginica"), to = c("red", "orange", "blue"))

clusplot(iris_quant, resu$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "K = 3")

legend(2.75,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.4,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(resu$cluster)], cex=.8)


############################Classification par K-means classique###############################

#Iris On souhaite effectuer une classification des donnees Iris que l'on pourra charger au moyen du
#code suivant :
iris_quant <- iris[,1:4]
z <- iris[,5]
#la partition en trois classes avec inertie minimale


# Determiner le nombre de classes optimal
#(a) effectuer N = 100 classifications en prenant K = 2, 3, ...,10 classes
#creer une matrice pour mettre des valeurs des inerties intra-classes
iris_matrix <- matrix(0, nrow = 100, ncol = 9)
rownames(iris_matrix) <- rownames(iris_matrix, do.NULL = FALSE, prefix = "N")
colnames(iris_matrix) <- c("K=2","K=3","K=4","K=5","K=6","K=7","K=8","K=9","K=10")
iris_matrix
for(k in 2:10) {
  for (N in 1:100) {
    iris_kmeans <- kmeans(iris_quant,k)
    iris_matrix[N,k-1] <- iris_kmeans$tot.withinss / dim(iris_quant)[1] #dtot/n
  }
}

# Pour chaque valeur de K, calculer l'inertie intra-classe mininale
iris_matrix_min <- apply(iris_matrix, 2, min)
iris_matrix_min

# etudier la decroissance du critere (inertie totale) en fonction du nombre de classes
# et choisir le nombre de classes avant le premier saut significatif
plot(iris_matrix_min, type ='o', xaxt='n', xlab = "Nombe K de classes", ylab = "Inertie intra-classe minimale")
axis(side = 1, at = seq(1,9,1),labels = c(2:10))

iris_kmeans_3 <- kmeans(iris_quant, 3)
iris_matrix_min <- apply(iris_matrix, 2, min)
while((iris_kmeans_3$tot.withinss / dim(iris_quant)[1]) != iris_matrix_min[2]) {
  iris_kmeans_3 <- kmeans(iris_quant, 3)
}
iris_kmeans_3$tot.withinss

table(iris$Species, iris_kmeans_3$cluster)

#Centre mobile en 2-3 clusters pour kmeans
species_colors = mapvalues(iris$Species, from = c("setosa", "versicolor", "virginica"), to = c("red", "orange", "blue"))

iris_kmeans_2 <- kmeans(iris_quant, 2)
clusplot(iris_quant,iris_kmeans_4$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile en 2 clusters")
legend(2.5,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.3,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(iris_kmeans_2$cluster)], cex=.8)

iris_kmeans_3 <- kmeans(iris_quant, 3)
clusplot(iris_quant,iris_kmeans_5$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile en 3 clusters")
legend(2.5,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.3,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(iris_kmeans_3$cluster)], cex=.8)

#Centre mobile en 2-3 clusters pour kmeans a distance adaptative

species_colors = mapvalues(iris$Species, from = c("setosa", "versicolor", "virginica"), to = c("red", "orange", "blue"))

iris_kmeans_adapt_2 <- Kmeans.adapt(iris_quant, 2)
clusplot(iris_quant,iris_kmeans_adapt_2$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile a distance adaptative en 2 clusters")
legend(2.5,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.3,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(iris_kmeans_adapt_2$cluster)], cex=.8)

iris_kmeans_adapt_3 <- Kmeans.adapt(iris_quant, 3)
clusplot(iris_quant,iris_kmeans_adapt_3$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile a distance adaptative en 3 clusters")
legend(2.5,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.3,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(iris_kmeans_adapt_3$cluster)], cex=.8)
