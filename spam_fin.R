library("MASS")
source("src/fonctions_tp4/separ1.R") #split data into test and training data.
source("src/fonctions_tp4/mvdnorm.r") #normalisation of the data.
source("src/fonctions_tp4/anadisc.R") #proportion, centre de gravite, matrice de covariance,
source("src/fonctions_tp4/logistic.R")
source("src/fonctions_tp4/logistic_quadratic.R")
source("src/fonctions_tp4/tree.rpart.R")
source("src/fonctions_tp4/functions.util.R")
source("src/fonctions_tp4/prob.ad.R")
source("src/fonctions_tp4/prob.log.R")
source("src/fonctions_tp4/prob.log2.R")
source("src/fonctions_tp4/Fonctions_Voisins.R")
source("src/fonctions_tp4/separ2.R")


qdaName = "Quadratic Discriminat Analysis"
ldaName = "Linear Discriminat Analysis"
nbaName = "Naive Bayes classifier"
logName = "Logistic Regression"
logQuadName = "Quadratic Logistic Regression"
decisionTreeName = "Decision Tree"
kppvName = "KPPV"


data <- read.csv("donnees/spam.csv")

data = data[,-1] # 1,2,3...enlever les index
zIndex = dim(data)[2] #58 ligne
X <- data[,1:zIndex-1]
Z <- data[,zIndex] #111111...2222....

######### Analyse des variances des donnees #########
X_high_variance = X[,55:57]
X_low_variance = X[,1:54] #o....
X_high_variance_scaled = X_high_variance / apply(X_high_variance, 2, sd)

diag(var(X_high_variance)) #extracts the diagonal
barplot(diag(var(X_high_variance)), ylab = "Variance")
diag(var(X_high_variance_scaled))
barplot(diag(var(X_low_variance)), ylab = "Variance")
diag(var(X_low_variance))

######### Classifieurs sur les donnees brutes #########
#spamErrorFunction(X, Z, qdaName, 20, printBeforeError = T)
spamErrorFunction(X[,-c(31,32,41)], Z, qdaName, 20, printBeforeError = F) #0.1679  Error in chol.default(Sigma) at mvdnorm.r#8 : le mineur dominant d'ordre c(31,32,41) n'est pas defini positif 
spamErrorFunction(X, Z, ldaName, 20) # 0.1149
#spamErrorFunction(X, Z, nbaName, 20, printBeforeError = T)
spamErrorFunction(X[,-c(31,32,41)], Z, nbaName, 20, printBeforeError = F)# 0.1819 #  Error in chol.default(Sigma) at mvdnorm.r#8 : le mineur dominant d'ordre c(31,32,41) n'est pas defini positif 
spamErrorFunction(X, Z, logName, 20)# 0.072 
spamErrorFunction(X, Z, decisionTreeName, 20)# 0.0937
displayDecisionTree(X,Z) #Error rate =  0.08936725 
spamErrorFunction(X, Z, kppvName, 20)


######### ACP avec scale et center #########
ACP = prcomp(X, center = T, scale = T)
plot(summary(ACP)$importance[3,], ylab = "Inertie expliquee cummulee", xlab = "Nombre de composantes principales considerees", pch=1)
plot(ACP$x[,1:2], col = c("black", "red")[Z])
ACP_X = ACP$x[,1:50]  #much faster
spamErrorFunction(ACP_X, Z, qdaName, 20)#0.1694
spamErrorFunction(ACP_X, Z, ldaName, 20)#0.1128
spamErrorFunction(ACP_X, Z, nbaName, 20)#0.1523
spamErrorFunction(ACP_X, Z, logName, 20)# error:infinite or missing values in 'x' 
spamErrorFunction(as.data.frame(ACP_X), Z, decisionTreeName, 20) #0.1099
displayDecisionTree(as.data.frame(ACP_X),Z)