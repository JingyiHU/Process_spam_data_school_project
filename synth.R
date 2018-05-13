###########################################
###      2. Application - Synth         ###
###########################################

######### Librairies necessaires ########

install.packages("mculst")
source("kmeans_adp.R")
source("distXY.r")
library(cluster)
library(mclust)
library(plyr)

####################################
# Données synthétiques

Synth1 <- read.csv("data/Synth1.csv", header=T, row.names=1)
z1 <- Synth1[,3]
Synth1 <- Synth1[,-3]

Synth2 <- read.csv("data/Synth2.csv", header=T, row.names=1)
z2 <- Synth2[,3]
Synth2 <- Synth2[,-3]

Synth3 <- read.csv("data/Synth3.csv", header=T, row.names=1)
z3 <- Synth3[,3]
Synth3 <- Synth3[,-3]


# Test de la fonction des kmeans adaptatifs sur les données

#synth1
resuSynth1 = Kmeans.adapt(Synth1, 2)
synth_colors1 = mapvalues(z1, from = c("1","2"), to = c("red", "orange"))
clusplot(Synth1, resuSynth1$cluster, color = TRUE, col.p = as.character(synth_colors1), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("red", "orange"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(resuSynth1$cluster)], cex=.8)
#synth2
resuSynth2 = Kmeans.adapt(Synth2, 2)
synth_colors2 = mapvalues(z2, from = c("1","2"), to = c("blue", "orange"))
clusplot(Synth2, resuSynth2$cluster, color = TRUE, col.p = as.character(synth_colors2), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("blue", "orange"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(resuSynth2$cluster)], cex=.8)
#synth3
resuSynth3 = Kmeans.adapt(Synth3, 2)
synth_colors3 = mapvalues(z3, from = c("1","2"), to = c("deeppink", "darkviolet"))
clusplot(Synth3, resuSynth3$cluster, color = TRUE, col.p = as.character(synth_colors3), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("deeppink", "darkviolet"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(resuSynth3$cluster)], cex=.8)


# Test de la fonction kmeans

#synth1
synth1_kmeans <- kmeans(Synth1, 2)
synth_colors1 = mapvalues(z1, from = c("1","2"), to = c("red", "orange"))
clusplot(Synth1, synth1_kmeans$cluster, color = TRUE, col.p = as.character(synth_colors1), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("red", "orange"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(synth1_kmeans$cluster)], cex=.8)

#synth2

synth2_kmeans <- kmeans(Synth2, 2)
synth_colors2 = mapvalues(z2, from = c("1","2"), to = c("blue", "orange"))
clusplot(Synth2, synth2_kmeans$cluster, color = TRUE, col.p = as.character(synth_colors2), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("blue", "orange"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(synth2_kmeans$cluster)], cex=.8)

#synth3

synth3_kmeans <- kmeans(Synth3, 2)
synth_colors3 = mapvalues(z3, from = c("1","2"), to = c("deeppink", "darkviolet"))
clusplot(Synth3, synth3_kmeans$cluster, color = TRUE, col.p = as.character(synth_colors3), shade = F, labels = 0, lines = FALSE, main = "K = 2")
legend(3,4, legend=c("1", "2"), col=c("deeppink", "darkviolet"), pch=16, cex=.8)
legend(1,4, legend = c("Cluster 1","Cluster 2"), pch = c(1,2)[unique(synth3_kmeans$cluster)], cex=.8)

# Test de l'adéquation

adjustedRandIndex(synth1_kmeans$cluster,z1)
adjustedRandIndex(resuSynth1$cluster,z1)

adjustedRandIndex(synth2_kmeans$cluster,z2)
adjustedRandIndex(resuSynth2$cluster,z2)

adjustedRandIndex(synth3_kmeans$cluster,z3)
adjustedRandIndex(resuSynth3$cluster,z3)







