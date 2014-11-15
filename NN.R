
# fonctions
source('~/Documents/Signal/Projet/usefullTools.r')


library("nnet")

# chemin vers les données 
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




# nb <- getNbN(digits, 20, 5)
# 15

# dec <- getDecay(digits, 15, 20, 5)
# 0.08




# mseVlst = NULL

# for (i in c(160, 180, 200, 220, 240, 260)){
#   res <- cross_val(digits[,2:dim(digits)[2]], class.ind(digits[,1]), 15, i, 5, 0.08)
#   mseVlst <- c(mseVlst, res$mseV)
# }
# par(fg = "black")
# mseVlst = c(mean(c(mseVlst[1], mseVlst[2], mseVlst[3], mseVlst[4], mseVlst[5])), mean(c(mseVlst[6], mseVlst[7], mseVlst[8], mseVlst[9], mseVlst[10])), mean(c(mseVlst[11], mseVlst[12], mseVlst[13], mseVlst[14], mseVlst[15])), mean(c(mseVlst[16], mseVlst[17], mseVlst[18], mseVlst[19], mseVlst[20])), mean(c(mseVlst[21], mseVlst[22], mseVlst[23], mseVlst[24], mseVlst[25])), mean(c(mseVlst[26], mseVlst[27], mseVlst[28], mseVlst[29], mseVlst[30])))
# plot(c(160, 180, 200, 220, 240, 260), mseVlst, type = "l")




res <- cross_val(digits[,2:dim(digits)[2]], class.ind(digits[,1]), 15, 200, 5, 0.08)
bestnn <- res$best_nn

cat("Reco rate (Train + Val)  = ",test.reco(class.ind(digits[,1]), predict(bestnn,digits[,2:dim(digits)[2]])),"/",dim(class.ind(digits[,1]))[1],"\n")
#test.cl(class.ind(digits[,1]), predict(bestnn,digits[,2:dim(digits)[2]]))

cat("Reco rate (Test)  = ",test.reco(class.ind(digitsTest[,1]), predict(bestnn,digitsTest[,2:dim(digitsTest)[2]])),"/",dim(class.ind(digitsTest[,1]))[1],"\n")



#save(bestnn, file="nnfixe.RData")
