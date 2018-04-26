Kmeans.adapt = function(X, K, rou=rep(1,K), nIterMax=100, nEss=10, Epsilon=10^-5) {
  
  library(MASS)
  
  X = as.matrix(X)
  dOpt = 100000
  
  n=dim(X)[1] # nb. de observation
  p=dim(X)[2] # nb. de features
  d=c()
  
  for (i in 1 : nEss) {
    
    # initialisation de la matrice de covarience
    Vk = array(0,c(p,p,K))
    for (k in 1 : K) {
      Vk[,,k] = diag(p) * rou[k] ^ (-1/p)
    }
    center = X[sample(nrow(X),K), ] # choix au hasard des centres
    nIter = 0
    delta = 100000                           
    
    
    if (K == 1){
      distMahalanobis = matrix(0, nrow=n, ncol=K)
      mtxCov = solve(Vk[,,1]) # solve le vk quand k=1
      distMahalanobis[,1] = distXY(X,center, mtxCov)
      clusters = apply(distMahalanobis, 1, which.min) # trouver le minimum
      
      centerPrec = center # entregister les centres precedents
      dTot = 0
      center = apply(t(t(X)[,clusters==1]), 2, mean) 
      tmp = t(t(X)[,clusters==1] - center) 
      if (dim(tmp)[1] == 1) 
      {
        Vk[,,1] = diag(p) 
      }
      else { 
        Vk[,,1] = cov(tmp) * (rou[1] * det(cov(tmp)))^(-1/p) 
      }
      
      d[1] = sum(distXY(X[clusters==1,],center,solve(Vk[,,1])))
      dTot = sum(distXY(X[clusters==1,],center,solve(Vk[,,1])))
    }
    else{
      while(!is.na(delta) & Epsilon<delta & nIter<nIterMax ){
        nIter = nIter + 1 
        # mise a jour les partitions
        distMahalanobis = matrix(0,nrow=n,ncol=K) # ici pour construire la matrice de distMahalanobis (n*K) on peut aussi utiliser rbind
        for (k in 1 : K) {
          #if(sum(is.na(Vk[,,k]))>0) return(clusters) #a enlever, pour tester
          mtxCov = solve(Vk[,,k])
          distMahalanobis[,k] = distXY(X,center[k,], mtxCov) # remplacer solve(Vk[,,k]) par diag(p) pour retrouver la distance euclidienne
          #return (distMahalanobis[,k])
        } 
        
        clusters = apply(distMahalanobis,1,which.min)
        
        # entregister les centres precedents
        centerPrec = center
        
        dTot = 0
        for (k in 1 : K) {
          # mise a jour le centre de la classe k
          center[k,] = apply( t(t(X)[,clusters==k]), 2, mean)
          
          # mise a jour la matrice de cov normalise de la classe k
          tmp = t(t(X)[,clusters==k]-center[k,]) 
          if (dim(tmp)[1]==1) 
          {
            Vk[,,k]=diag(p) 
          }
          else { 
            Vk[,,k] = cov(tmp) * (rou[k] * det(cov(tmp)))^(-1/p) 
          }
          
          d[k] = sum(distXY(X[clusters==k,],center[k,],solve(Vk[,,k])))
          # mise a jour dTot
          dTot = dTot+ sum(distXY(X[clusters==k,],center[k,],solve(Vk[,,k]))) 
          # remplacer diag par solve(Vk[,,k]) pour retrouver la distance de m
        }
        
        # mise a jour delta
        tmp.d = (center-centerPrec)
        delta = sum(diag(tmp.d %*% t(tmp.d)))
      }
    }	
    # mise a jour l'opt
    if (dTot < dOpt) {
      dOpt=dTot
      nIterOpt=nIter
      clustersOpt=clusters
      centerOpt=center
      VkOpt=Vk
      
    }
    
  }
  nombre = c()
  for(k in 1:K)
  {
    nombre[k] = length(which(clustersOpt==k))
  }
  
  
  result = NULL # initialisation de resultat
  result$dist.withinss = d
  result$dist.tot = dOpt
  result$nb.iterations = nIterOpt
  result$cluster = clustersOpt
  result$nombre.donnees.classe = nombre
  result$centres = centerOpt
  result$matrices.covariance.classe = VkOpt
  return(result)
}















#K_means <- function(x, centers, distFun, nItter) {
  #clusterHistory <- vector(nItter, mode="list")
 # centerHistory <- vector(nItter, mode="list")
  
 # for(i in 1:nItter) {
 #   distsToCenters <- distFun(x, centers)
 #   print(distsToCenters)
 #   clusters <- apply(distsToCenters, 1, which.min)
 #   centers <- apply(x, 2, tapply, clusters, mean)
 #   # Saving history
 #   clusterHistory[[i]] <- clusters
  #  centerHistory[[i]] <- centers
 # }  
 # list(clusters=clusterHistory, centers=centerHistory)
 # plot(clusters, col = c('green','blue','yellow'))
#}
#centers <- ktest[sample(nrow(ktest), 3),] # Sample some centers, 3 for example
#res <- K_means(ktest, centers, distXY, 10)
#res






