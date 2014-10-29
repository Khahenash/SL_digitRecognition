source('~/Documents/Signal/Projet/usefullTools.r')
library("nnet")



data <- loadAll("/home/khahenash/Documents/Signal/Projet/data_Digits_HMM/Data5X3/Train_compute_symbol_5_3Digit")
dataObs <- data$obs
dataCl <- data$cl

digits <- matrix(data=NA, nrow=dim(dataObs)[1], ncol=13 )

for (j in 1:dim(dataObs)[1]){
  img <- compute_img(dataObs[j,],5,3)
  profile <- compute_profiles(img, 5, 3)
  centroid <- compute_centroid(img, 5, 3)
  digits[j,] <- c(dataCl[j], profile$left, profile$right, centroid$row, centroid$col)
}


getNbN <- function(digits){
  nbNLst <- NULL
  meanMseTLst <- NULL
  meanMseVLst <- NULL
  for(nbN in 1:20){
    cv <- cross_val(digits[,2:dim(digits)[2]], class.ind(digits[,1]), nbN, 10, 5)
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

nb <- getNbN(digits)

# save(maVar, file="mavar.RData")
# load("maVar.RData")
