##########################################
###      2. Application - Spam         ###
##########################################

######### Librairies necessaires ########
source("distXY.R")
source("kmeans_adp.R")
library(mclust)
library(cluster)
library(plyr)

####### Chargement et binarisation des donnees #####
Spam <- read.csv("data/spam.csv", header=T, row.names=1)
X <- Spam[,-58]
z <- Spam[,58] #11111..22222..
K = 2
X2 = X[,1:55]
spam2 = scale(X2)
spam2[spam2 > 0] = 1
spam2 = as.matrix(spam2)
spam_colors = mapvalues(z, from = c(1,2), to = c("blue", "red"))


####### ACP #####

comp = princomp(spam2)
screeplot(comp, type="lines")
#biplot(comp)
plot(comp$scores[,1],comp$scores[,2], col= z)
postacp_variables = comp$scores


####### Kmeans & PCA #####


km = kmeans(spam2,2)
adjustedRandIndex(km$cluster,z) # 0.498512

clusplot(as.matrix(spam2),km$cluster, color = TRUE, col.p = as.character(spam_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile en 2 clusters")


kmacp = kmeans(postacp_variables[,1:2],2)
adjustedRandIndex(kmacp$cluster,z) # 0.4773322

####### Pam & PCA ######

pam = pam(spam2,k=2)
adjustedRandIndex(pam$clustering,z) # 0.2563789

pam.acp = pam(postacp_variables[,1:2],k=2)
adjustedRandIndex(pam.acp$clustering,z) # 0.4737757

####### Kmeans adaptive & PCA #####
spam.kmeans.adaptative = Kmeans.adapt(postacp_variables[,1:2],2) # les 2 premieres composantes
adjustedRandIndex(spam.kmeans.adaptative$cluster,z) #0.3941267

spam.kmeans.adaptative = Kmeans.adapt(postacp_variables[,c(1,4)],2) # les 2 premieres composantes
adjustedRandIndex(spam.kmeans.adaptative$cluster,z) #0.5318719
clusplot(as.matrix(spam2),spam.kmeans.adaptative$cluster, color = TRUE, col.p = as.character(spam_colors), shade = F, labels = 0, lines = FALSE, main = "Centre mobile en 2 clusters")

