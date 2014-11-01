source('~/Documents/Signal/Projet/usefullTools.r')
library("nnet")



data <- loadAll("/home/khahenash/Documents/Signal/Projet/data_Digits_HMM/Data7X5/Train_compute_symbol_7_5Digit")
dataObs <- data$obs
dataCl <- data$cl

test <- loadAll("/home/khahenash/Documents/Signal/Projet/data_Digits_HMM/Data7X5/Test_compute_symbol_7_5Digit")
testObs <- test$obs
testCl <- test$cl


nr = 7
nc = 5

digits <- matrix(data=NA, nrow=dim(dataObs)[1], ncol=(3*(nr+nc+1)) )
digitsTest <- matrix(data=NA, nrow=dim(testObs)[1], ncol=(3*(nr+nc+1)) )

# compute images & features
for (j in 1:dim(dataObs)[1]){
  img <- compute_img(dataObs[j,],nr,nc)
  profile <- compute_profiles(img, nr, nc)
  centroid <- compute_centroid(img, nr, nc)
  projection <- compute_projections(img, nr, nc)
  digits[j,] <- c(dataCl[j], profile$left, profile$right, profile$top, profile$bottom, projection$col, projection$row, centroid$row, centroid$col)
}

for (j in 1:dim(testObs)[1]){
  img <- compute_img(testObs[j,],nr,nc)
  profile <- compute_profiles(img, nr, nc)
  centroid <- compute_centroid(img, nr, nc)
  projection <- compute_projections(img, nr, nc)
  digitsTest[j,] <- c(testCl[j], profile$left, profile$right, profile$top, profile$bottom, projection$col, projection$row, centroid$row, centroid$col)
}


getNbN <- function(digits, nbLoop, fold){
  nbNLst <- NULL
  meanMseTLst <- NULL
  meanMseVLst <- NULL
  for(nbN in 8:20){
    cv <- cross_val(digits[,2:dim(digits)[2]], class.ind(digits[,1]), nbN, nbLoop, fold, 0.2)
    nbNLst <- c(nbNLst, nbN)
    meanMseTLst <- c(meanMseTLst,mean(cv$mseT))
    meanMseVLst <- c(meanMseVLst,mean(cv$mseV))
  }
  
  
  par(fg = "black")
  plot(nbNLst, meanMseTLst , type = "l")
  par(fg = "red")
  lines(nbNLst, meanMseVLst , type = "l")

  return(nbNLst[match(c(min(meanMseTLst)), meanMseTLst)])
}

getDecay <- function(digits, nbN, nbLoop, fold){
  nbDLst <- NULL
  meanMseTLst <- NULL
  meanMseVLst <- NULL
  for(i in c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.40, 0.45, 0.5)){
    cv <- cross_val(digits[,2:dim(digits)[2]], class.ind(digits[,1]), nbN, nbLoop, fold, i)
    nbDLst <- c(nbDLst, i)
    meanMseTLst <- c(meanMseTLst,mean(cv$mseT))
    meanMseVLst <- c(meanMseVLst,mean(cv$mseV))
  }
  
  
  par(fg = "black")
  plot(nbDLst, meanMseTLst , type = "l")
  par(fg = "red")
  lines(nbDLst, meanMseVLst , type = "l")
  
  return(nbDLst[match(c(min(meanMseTLst)), meanMseTLst)])
}


nb <- getNbN(digits, 20, 5)
# nb 12

#dec <- getDecay(digits, 12, 20, 5)
# 0.2

res <- cross_val(digitsTest[,2:dim(digitsTest)[2]], class.ind(digitsTest[,1]), 12, 20, 5, 0.2)
bestnn <- res$best_nn

cat("Reco rate (Test)  = ",test.reco(class.ind(digitsTest[,1]), predict(bestnn,digitsTest[,2:dim(digitsTest)[2]])),"/",dim(class.ind(digitsTest[,1]))[1],"\n")

#save(maVar, file="maVar.RData")
# load("maVar.RData")

save(bestnn, file="nn85-9.RData")
