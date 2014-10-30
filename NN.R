source('~/Documents/Signal/Projet/usefullTools.r')
library("nnet")



data <- loadAll("/home/khahenash/Documents/Signal/Projet/data_Digits_HMM/Data7X5/Train_compute_symbol_7_5Digit")
dataObs <- data$obs
dataCl <- data$cl
nr = 7
nc = 5
digits <- matrix(data=NA, nrow=dim(dataObs)[1], ncol=(3*(nr+nc+1)) )

for (j in 1:dim(dataObs)[1]){
  img <- compute_img(dataObs[j,],nr,nc)
  profile <- compute_profiles(img, nr, nc)
  centroid <- compute_centroid(img, nr, nc)
  projection <- compute_projections(img, nr, nc)
  digits[j,] <- c(dataCl[j], profile$left, profile$right, profile$top, profile$bottom, projection$col, projection$row, centroid$row, centroid$col)
}


getNbN <- function(digits, nbLoop, fold){
  nbNLst <- NULL
  meanMseTLst <- NULL
  meanMseVLst <- NULL
  for(nbN in 1:20){
    cv <- cross_val(digits[,2:dim(digits)[2]], class.ind(digits[,1]), nbN, nbLoop, fold)
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

nb <- getNbN(digits, 10, 5)

# save(maVar, file="maVar.RData")
# load("maVar.RData")

