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

cross_val(digits[,2:dim(digits)[2]], class.ind(digits[,1]), 5, 10, 5)