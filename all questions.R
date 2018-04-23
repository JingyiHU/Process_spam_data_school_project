recettesPays = read.csv("donnees/recettes-pays.data", header=T, row.names = 1)

###########1. Faire une analyse exploratoire de ce jeu de donn??es.
dim(recettesPays) #26 50 first colonne origin isn't numeric

# the number of the record in the dataframe
d1 = dim(recettesPays)[1] #26

class(recettesPays) #data.frame

summary(recettesPays)

###########2. Faire une analyse en composantes principales de ce jeu de donn??es. Interpr??tez les r??sultats obtenus.

# faire PCA, affichage dans le premier plan factoriel
# can't use the princomp, because we don't have enough records, but more features

recettesPays_quant = recettesPays[, -1] # with only the value numeric
acp_recettesPays = prcomp(recettesPays_quant) 
acp_recettesPays  # PC1 to PC26

install.packages("ggfortify")
library(ggfortify)

plot(acp_recettesPays$x, asp = 1)  # this 2 has the same result..but without color, I don't have ideas.
autoplot(acp_recettesPays, data = recettesPays)

autoplot(acp_recettesPays, data = recettesPays, 
         #colour = "recettesPays[0]", 
         loadings = TRUE, 
         loadings.label = TRUE, x = 2, y = 3)

# according to the pricture, we note that we have too much va qualitatif.

###########3. Faire une analyse ascendante hi??rarchique avec la distance de Manhattan 
#et un crit??re d???aggr??gation de votre choix.

###part1
#faire le tableau de dissimilarite

#not work
#recettesPays_dist = as.dist(recettesPays, diag = T, upper = T) # warning message: non-square matrix
#recettesPays_hclust <- hclust(recettesPays_dist, method = "manhattan") # error: invalid clustering method manhattan

dist.recettesPays = dist(recettesPays,method = "manhattan")
# faire un cluster pour effectuer la classification hierarchie assendente
hclust.recettesPays = hclust(dist.recettesPays)
?hclust
# faire heatmap pour voire globalement le coupe de donnees
heatmap(as.matrix(dist.recettesPays), labRow = F, labCol = F)
# here we find that we have 4 parts of different colors, so we devide the data into 3 groupes

# display the dendrogram
plot(hclust.recettesPays)

# show the dendrogram with rectangular frames
rect.hclust(hclust.recettesPays, k = 3) #3 is more reasonable or 4

# devide in 3 groups
cutree.recettesPays = cutree(hclust.recettesPays, k = 3)
t(t(cutree.recettesPays)) # the hight?

###part2
# classification with different agregation criteria
#min
hclust_recettesPays = hclust(dist.recettesPays, method = "single")
plot(hclust.recettesPays, hang = -1, main = "Critere d'agregation \"Min\"", xlab = "Pays", ylab = "Valeur d'indice", sub = "")
rect.hclust(hclust.recettesPays, k = 3)

#max
hclust_recettesPays = hclust(dist.recettesPays, method = "complete")
plot(hclust.recettesPays, hang = -1, main = "Critere d'agregation \"Max\"", xlab = "Pays", ylab = "Valeur d'indice", sub = "")
rect.hclust(hclust.recettesPays, k = 3)

#moyen
hclust_recettesPays = hclust(dist.recettesPays, method = "average")
plot(hclust.recettesPays, hang = -1, main = "Critere d'agregation \"Moyenne\"", xlab = "Pays", ylab = "Valeur d'indice", sub = "")
rect.hclust(hclust.recettesPays, k = 3)

#moyenne ponderee
hclust_recettesPays = hclust(dist.recettesPays, method = "mcquitty")
plot(hclust.recettesPays, hang = -1, main = "Critere d'agregation \"Moyenne ponderee\"", xlab = "Pays", ylab = "Valeur d'indice", sub = "")
rect.hclust(hclust.recettesPays, k = 3)

#distance eucilienne
hclust_recettesPays = hclust(dist.recettesPays, method = "centroid")
plot(hclust.recettesPays, hang = -1, main = "Critere d'agregation \"UPGMC : Distance euclidienne \n entre les centres de gravite des classes\"", xlab = "Pays", ylab = "Valeur d'indice", sub = "")
rect.hclust(hclust.recettesPays, k = 3)

#median
hclust_recettesPays = hclust(dist.recettesPays, method = "median")
plot(hclust.recettesPays, hang = -1, main = "Critere d'agregation \"WPGMC : UPGMC pondere\"", xlab = "Pays", ylab = "Valeur d'indice", sub = "")
rect.hclust(hclust.recettesPays, k = 3)


hclust_recettesPays = hclust(dist.recettesPays, method = "ward.D2")
plot(hclust.recettesPays, hang = -1, main = "Critere d'agregation \"ward\"", xlab = "Pays", ylab = "Valeur d'indice", sub = "")
rect.hclust(hclust.recettesPays, k = 3)

##########4. Grouper les origines des recettes ?? l???aide de l???algorithme des K-means.

# k = 3 classes

aftd_recettesPays_5 <- cmdscale(dist.recettesPays, k = 5, eig = TRUE, x.ret = TRUE)
aftd_recettesPays_5$points
aftd_recettesPays_5$x

kmeans3.recettesPays = kmeans(aftd_recettesPays_5$points, 3) #---???pca????????????????????????essai 

plot(aftd_recettesPays_5$points, col = c("green", "blue", "red")[kmeans3.recettesPays$cluster])
# seems wired..

library(cluster)
clusplot(aftd_recettesPays_5$points, kmeans3.recettesPays$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 0, main = "Centre mobile en 3 clusters")

kmeans3.recettesPays$tot.withinss
?kmeans

#5. Comparer les classifications obtenues avec une classification g??ographique des origines.

# Il s'agit de calculer la representation euclidienne des donnees par AFTD (analyse factorielle 
# de tableau de distance) en k dimensions.

# Diagrammes de shepard pour k allant de 2 e 5 dimensions
##### k = 2 dimensions
aftd_recettesPays_2 = cmdscale(mut, k = 2, eig = TRUE, x.ret = TRUE)
dist(aftd_recettesPays_2$points,diag = T, upper = T) # fonction pour transformer la representation des points sur les axes en une matrice de distance
plot(mut, dist(aftd_recettesPays_2$points, diag = T, upper = T), xlab = "Dissimilarites originelles", ylab = "Dissimilarites espace factoriel a 2 dimensions")
abline(1,1)
##### k = 3 dimensions
aftd_recettesPays_3 = cmdscale(mut, k = 3, eig = TRUE, x.ret = TRUE)
plot(mut, dist(aftd_recettesPays_3$points, diag = T, upper = T), xlab = "Dissimilarites originelles", ylab = "Dissimilarites espace factoriel a 3 dimensions")
abline(1,1)
##### k = 4 dimensions ######
aftd_recettesPays_4 <- cmdscale(mut, k = 4, eig = TRUE, x.ret = TRUE)
plot(mut, dist(aftd_recettesPays_4$points, diag = T, upper = T), xlab = "Dissimilarites originelles", ylab = "Dissimilarites espace factoriel a 4 dimensions")
abline(1,1)
##### k = 5 dimensions ######
aftd_recettesPays_5 <- cmdscale(mut, k = 5, eig = TRUE, x.ret = TRUE)
plot(mut, dist(aftd_recettesPays_5$points, diag = T, upper = T), xlab = "Dissimilarites originelles", ylab = "Dissimilarites espace factoriel a 5 dimensions")
abline(1,1)
# with the k increases, the ponits are closer to x-y = 1

#Le jeu de donn??es pr??c??dent provient en fait d???un jeu de donn??es plus gros 
#qui d??crit plusieurs recettes en sp??cifiant leur origine et la composition en ingr??dients. 
#Un extrait de ce jeu de donn??es est pr??sent dans le fichier recettes-echant.data.

#6. Faites une analyse descriptive rapide de ce jeu de donn??es.
recettesEchant = read.csv("donnees/recettes-echant.data", header=T) #, row.names = 1)

recettesEchant = read.csv("donnees/recettes-echant.data", header=T, row.names = 1) 
# error: duplicate 'row.names' are not allowed

dim(recettesEchant) #2000 50
colnames(recettesEchant)
colnames(recettesPays) # the names not same in the 2 data sets

class(recettesEchant) # data.frame
summary(recettesEchant)
#barplot(summary(recettesEchant)) 50 warnings..
#pie(summary(recettesEchant)) error
boxplot(recettesEchant)

#7. On cherche ?? grouper les diff??rents ingr??dients. 
#Transformez les donn??es en tableau individuvariable portant sur les ingr??dients 
#et proposez une m??thode pour calculer une matrice de similarit?? ou de dissimilarit?? entre les ingr??dients.
#Justifiez vos choix.


#8. Effectuer une classification ascendante hi??rarchique ?? l???aide de la dissimilarit?? obtenue pr??c??dem- ment. 
#Combien de classes d???ingr??dients semblent se distinguer ?


#9. En utilisant la fonction pam de la biblioth??que cluster, appliquez l???algorithme des K-m??do??des 
#et proposer des ingr??dients repr??sentant chaque classe.

####################################
###      2.2 Application        ####
####################################

# synthese

############################Classification par K-means avec distance adaptative###############################
Synth1 <- read.csv("donnees/Synth1.csv", header=T, row.names=1)
z1 <- X[,3]
Synth1 <- X[,-3]

Synth2 <- read.csv("donnees/Synth2.csv", header=T, row.names=1)
z2 <- X[,3]
Synth2 <- X[,-3]

Synth3 <- read.csv("donnees/Synth3.csv", header=T, row.names=1)
z3 <- X[,3]
Synth3 <- X[,-3]

source("kmeans_adp.R")
library(cluster)
# For the methods map values
library(plyr)
# tester utilisant kmeans_adp.R

#synth1
resuSynth1 = Kmeans.adapt(Synth1, 2)
synth_colors1 = mapvalues(z1, from = c("1","2"), to = c("red", "orange"))
clusplot(Synth1, resuSynth1$cluster, color = TRUE, col.p = as.character(synth_colors), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("red", "orange"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(resuSynth1$cluster)], cex=.8)
#synth2
resuSynth2 = Kmeans.adapt(Synth2, 2)
synth_colors2 = mapvalues(z2, from = c("1","2"), to = c("blue", "orange"))
clusplot(Synth2, resuSynth2$cluster, color = TRUE, col.p = as.character(synth_colors2), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("blue", "orange"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(resuSynth1$cluster)], cex=.8)
#synth3
resuSynth3 = Kmeans.adapt(Synth3, 2)
synth_colors3 = mapvalues(z3, from = c("1","2"), to = c("deeppink", "darkviolet"))
clusplot(Synth3, resuSynth3$cluster, color = TRUE, col.p = as.character(synth_colors3), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("deeppink", "darkviolet"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(resuSynth1$cluster)], cex=.8)



####pas encore faire...TODODODOD
#On com- parera les r??sultats obtenus et on interpr??tera en fonction des donn??es. 
#On peut utiliser la fonction adjustedRandIndex (biblioth??que mclust) 
#pour calculer l???ad??quation de la partition trouv??e ?? la par- tition r??elle.
############################Classification par K-means classique###############################
synth1_kmeans <- kmeans(Synth1, 2)
synth_colors1 = mapvalues(z1, from = c("1","2"), to = c("red", "orange"))
clusplot(Synth1, synth1_kmeans$cluster, color = TRUE, col.p = as.character(synth_colors), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("red", "orange"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(synth1_kmeans$cluster)], cex=.8)



synth2_kmeans <- kmeans(Synth2, 2)
resuSynth2 = Kmeans.adapt(Synth2, 2)
synth_colors2 = mapvalues(z2, from = c("1","2"), to = c("blue", "orange"))
clusplot(Synth2, synth2_kmeans$cluster, color = TRUE, col.p = as.character(synth_colors2), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("blue", "orange"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(synth2_kmeans$cluster)], cex=.8)



synth3_kmeans <- kmeans(Synth3, 2)
synth_colors3 = mapvalues(z3, from = c("1","2"), to = c("deeppink", "darkviolet"))
clusplot(Synth3, synth3_kmeans$cluster, color = TRUE, col.p = as.character(synth_colors3), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("deeppink", "darkviolet"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(synth3_kmeans$cluster)], cex=.8)



############################Classification par K-means avec distance adaptative###############################
#The K means algorithm that uses the Euclidean distance above:

source("kmeans_adp.R")
library(cluster)
# For the methods map values
library(plyr)
# tester utilisant kmeans_adp.R
data(iris)  
test1 = iris
dim(test) # 150 4 
iris_quant = iris[,-5] # A data.frame
ktest = as.matrix(iris_quant) # Turn into a matrix
resu = Kmeans.adapt(ktest, 3)

species_colors = mapvalues(iris$Species, from = c("setosa", "versicolor", "virginica"), to = c("red", "orange", "blue"))

clusplot(iris_quant, resu$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "K = 3")

legend(2.75,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.4,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(resu$cluster)], cex=.8)

#points(resu[["centres"]],col=c("gold1", "deeppink", "forestgreen"),pch=16,cex=2)
#j'essai d'afficher les centres, mais fonctinne pas...

############################Classification par K-means classique###############################
#Donn??es r??elles
#Iris On souhaite effectuer une classification des donn??es Iris que l???on pourra charger au moyen du
#code suivant :
data(iris)
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

#Centre mobile en 3 clusters
species_colors = mapvalues(iris$Species, from = c("setosa", "versicolor", "virginica"), to = c("red", "orange", "blue"))
clusplot(iris_quant,iris_kmeans_3$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile en 3 clusters")
legend(2.5,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.3,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(iris_kmeans_3$cluster)], cex=.8)

iris_kmeans_4 <- kmeans(iris_quant, 4)
clusplot(iris_quant,iris_kmeans_4$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile en 3 clusters")
legend(2.5,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.3,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(iris_kmeans_3$cluster)], cex=.8)

iris_kmeans_5 <- kmeans(iris_quant, 5)
clusplot(iris_quant,iris_kmeans_5$cluster, color = TRUE, col.p = as.character(species_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile en 3 clusters")
legend(2.5,2.9, legend=c("setosa", "versicolor", "virginica"), col=c("red", "orange", "blue"), pch=16, cex=.8)
legend(1.3,2.9, legend = c("Cluster 1","Cluster 2", "Cluster 3"), pch = c(1,2,3)[unique(iris_kmeans_3$cluster)], cex=.8)

#####################TO DO with adapdative

Spam <- read.csv("donnees/spam.csv", header=T, row.names=1)
X <- Spam[,-58] 
z <- Spam[,58] #11111..22222..

source("spam_fin.R")


