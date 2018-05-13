########################################
#              PARTIE 1                #
########################################


######### Librairies necessaires ########

install.packages("ggfortify")
library(ggfortify)
library(cluster)

#########################################

recettesPays = read.csv("data/recettes-pays.data", header=T, row.names = 1)

###########1. Faire une analyse exploratoire de ce jeu de donnees.

dim(recettesPays) #26 50 first colon origin isn't numeric

# the number of the record in the dataframe
d1 = dim(recettesPays)[1] #26

class(recettesPays) #data.frame

summary(recettesPays)

###########2. Faire une analyse en composantes principales de ce jeu de donnees. Interpretez les resultats obtenus.

# faire PCA, affichage dans le premier plan factoriel
# on ne peut pas utiliser princomp parce qu'il n'y a pas assez de lignes

recettesPays_quant = recettesPays # with only the value numeric
acp_recettesPays = prcomp(recettesPays_quant)
acp_recettesPays  # PC1 to PC26

autoplot(acp_recettesPays, data = recettesPays) #Figure 1

autoplot(acp_recettesPays, data = recettesPays,
         #colour = "recettesPays[0]",
         loadings = TRUE,
         loadings.label = TRUE, x = 2, y = 3) #Figure 2

# Selon le graphique, on voit qu'on a beaucoup de variables qualitatives.

###########3. Faire une analyse ascendante hierarchique avec la distance de Manhattan
#et un critere d'aggregation de votre choix.

###part1
#faire le tableau de dissimilarite

dist.recettesPays = dist(recettesPays,method = "manhattan")
# faire un cluster pour effectuer la classification hierarchie assendente
hclust.recettesPays = hclust(dist.recettesPays)
# faire heatmap pour voire globalement le coupe de donnees
heatmap(as.matrix(dist.recettesPays), labRow = F, labCol = F) #Figure 3

# afficher le dendrogramme
plot(hclust.recettesPays)

# show the dendrogram with rectangular frames
rect.hclust(hclust.recettesPays, k = 3) #3 is more reasonable or 4

# devide in 3 groups
cutree.recettesPays = cutree(hclust.recettesPays, k = 3)

#distance eucilienne
hclust_recettesPays = hclust(dist.recettesPays, method = "centroid")
plot(hclust.recettesPays, hang = -1, main = "Critere d'agregation \"UPGMC : Distance euclidienne \n entre les centres de gravite des classes\"", xlab = "Pays", ylab = "Valeur d'indice", sub = "")
rect.hclust(hclust.recettesPays, k = 3) #Figure 4

#median
hclust_recettesPays = hclust(dist.recettesPays, method = "median")
plot(hclust.recettesPays, hang = -1, main = "Critere d'agregation \"WPGMC : UPGMC pondere\"", xlab = "Pays", ylab = "Valeur d'indice", sub = "")
rect.hclust(hclust.recettesPays, k = 3) #Figure 4


##########4. Grouper les origines des recettes e l'aide de l'algorithme des K-means.


aftd_recettesPays_5 <- cmdscale(dist.recettesPays, k = 5, eig = TRUE, x.ret = TRUE)
aftd_recettesPays_5$points
points2 = aftd_recettesPays_5$points[,c(1,3)]
aftd_recettesPays_5$x

kmeans3.recettesPays = kmeans(aftd_recettesPays_5$points, 3)

plot(aftd_recettesPays_5$points, col = c("green", "blue", "red")[kmeans3.recettesPays$cluster])

clusplot(aftd_recettesPays_5$points, kmeans3.recettesPays$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 3, main = "Centre mobile en 3 clusters", xlab = "Composante 1", ylab="Composante 2") #Figure 5

clusplot(points2, kmeans3.recettesPays$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 3, main = "Centre mobile en 3 clusters", xlab = "Composante 1", ylab="Composante 3") #Figure 6

kmeans3.recettesPays$tot.withinss

#5. Comparer les classifications obtenues avec une classification geographique des origines.


#Le jeu de donnees precedent provient en fait d"un jeu de donnees plus gros
#qui decrit plusieurs recettes en specifiant leur origine et la composition en ingredients.
#Un extrait de ce jeu de donnees est present dans le fichier recettes-echant.data.

#6. Faites une analyse descriptive rapide de ce jeu de donnees.
recettesEchant = read.csv("data/recettes-echant.data", header=T) #, row.names = 1)

dim(recettesEchant) #2000 50
colnames(recettesEchant)
colnames(recettesPays) # the names not same in the 2 data sets

class(recettesEchant) # data.frame
summary(recettesEchant)

boxplot(recettesEchant)

#7. On cherche a grouper les differents ingredients.
#Transformez les donnees en tableau individuvariable portant sur les ingredients
#et proposez une methode pour calculer une matrice de similarite ou de dissimilarite entre les ingredients.
#Justifiez vos choix.

ingredientsPays = cov(recettesPays)
ingredientsPays2 = t(recettesPays)

#8. Effectuer une classification ascendante hierarchique a l'aide de la dissimilarite obtenue precedemment.
#Combien de classes d'ingredients semblent se distinguer ?
dist.ingredientsPays = dist(ingredientsPays,method = "manhattan")
# faire un cluster pour effectuer la classification hierarchique assendente
hclust.ingredientsPays = hclust(dist.ingredientsPays)

# faire heatmap pour voire globalement le coupe de donnees
heatmap(as.matrix(dist.ingredientsPays), labRow = F, labCol = F) #Figure 7

plot(hclust.ingredientsPays) #Figure 8

rect.hclust(hclust.ingredientsPays, k = 5)

cutree.ingredientsPays = cutree(hclust.ingredientsPays, k = 5)


#9. En utilisant la fonction pam de la bibliotheque cluster, appliquez le?algorithme des K-medoedes
#et proposer des ingredients representant chaque classe.

kmedoids = pam(ingredientsPays, k=5)
clusplot(ingredientsPays, kmedoids$cluster,color=TRUE, shade = FALSE, lines = FALSE, labels = 3, main = "kmedoids en 5 clusters") #Figure 9

