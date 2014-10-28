# confusion matrix
test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  return (table(true, cres))
}

#recognition rate
test.reco <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  return (as.numeric(sum(true == cres)))
}

#compute the MSE
test.mse <- function(true, pred) {
  diff <- true - pred
  sqred <- diff * diff
  return (sum(sqred) / length(sqred))
}

# convert a vector of classes in a matrix of 0 and 1 (objectif value matrix)
class.ind <- function (cl) 
{
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)))
  x[(1L:n) + n * (unclass(cl) - 1L)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}

#generate the matrix of observation from a text file => usefull for training
Load_Obs <- function(file)
{
  obs <- read.table(file)
  cat(dim(obs)[1], " examples loaded of size ",dim(obs)[2],"\n")
  return(matrix(t(obs),ncol=dim(obs)[2],byrow=T))
}

#load all file => usefull for test
loadAll <- function(rootName){
  cl <- NULL
  allobs <- NULL
  for(i in 0:9){
    obs <- Load_Obs(paste(rootName, i, ".txt", sep=""))
    allobs <- rbind(allobs, obs)
    cl <- c(cl, rep(i,dim(obs)[1]))
  }
  return (list(obs=allobs, cl=cl))
}

learnVal <- function (dataFt, dataTarg,trainId, valId, nbN, nbLoop){
  # init a new random MLP
  # nbN : nombre de neurones dans la couche cachée
  # pas d'iter pour avec un nn aléatoire
  
  new_nn <- nnet(dataFt[trainId,], dataTarg[trainId,], size=nbN, maxit=0, decay=1e-4, rang = 1)
  best_nn = new_nn
  curr_w <- new_nn$wts
  # compute initial rates / mse and save them
  currTrRate <- test.reco(dataTarg[trainId,], predict(new_nn,dataFt[trainId,]))
  currTrRateVal <- test.reco(dataTarg[valId,], predict(new_nn,dataFt[valId,]))
  currTrMSE <- test.mse(dataTarg[trainId,], predict(new_nn,dataFt[trainId,]))
  currTrMSEVal <- test.mse(dataTarg[valId,], predict(new_nn,dataFt[valId,]))
  
  scoresT <- c(currTrRate/length(trainId))
  scoresV <- c(currTrRateVal/length(valId))
  mseT <- c(currTrMSE)
  mseV <- c(currTrMSEVal)
  iterations <- c(0)
  bestRate <- 0  
  bestIt <- 0
  cat("Starting Reco rate = ",currTrRate,"\n")
  for(i in 1:nbLoop){
    cat(i," / ", nbLoop, "\n")
    #continue the training
    new_nn <- nnet(dataFt[trainId,], dataTarg[trainId,], size=nbN, maxit=50, decay=1e-4, Wts=curr_w)
    curr_w <- new_nn$wts
    #compute the rates/MSE
    currTrRate <- test.reco(dataTarg[trainId,], predict(new_nn,dataFt[trainId,]))
    currTrRateVal <- test.reco(dataTarg[valId,], predict(new_nn,dataFt[valId,]))
    currTrMSE <- test.mse(dataTarg[trainId,], predict(new_nn,dataFt[trainId,]))
    currTrMSEVal <- test.mse(dataTarg[valId,], predict(new_nn,dataFt[valId,]))
    
    #save values to plot
    scoresT <- c(scoresT,currTrRate/length(trainId))
    scoresV <- c(scoresV,currTrRateVal/length(valId))
    mseT <- c(mseT,currTrMSE)
    mseV <- c(mseV,currTrMSEVal)
    iterations <- c(iterations, i * 50)
    #save if best
    if(currTrRateVal > bestRate){
      bestRate <- currTrRateVal
      best_nn <- new_nn
      bestIt <- 50 * i
    }
  }
  return (list(nn = best_nn, nbIt=bestIt, scoreTrain = scoresT, scoreVal = scoresV, it=iterations, mseT=mseT,mseV=mseV))
}

cross_val <- function (dataFt, dataTarg, nbN, nbLoop, fold){
  # mélange des indices
  ind <- sample(1:dim(dataFt)[1],dim(dataFt)[1])
  score <- NULL
  
  for(i in 1:fold){
    cat("==================== ", i, " ====================\n")
    valId <- ind[((i-1)*floor(dim(dataFt)[1]/fold)+1):(i*floor(dim(dataFt)[1]/fold))]
    res <- learnVal(dataFt, dataTarg, -valId, valId, nbN, nbLoop)
    score <- c(score, res$scoreTrain)
    
    par(fg = "black")
    plot(res$it, res$mseT, type = "l")
    par(fg = "red")
    lines(res$it, res$mseV, type = "l")
  }
}


compute_profiles <- function (symbol, nr=5, nc=3){
  left_profile <- matrix(data=0, nrow = 1, ncol = nr)
  right_profile <- matrix(data=0, nrow = 1, ncol = nr)
  top_profile <- NULL
  bottom_profile <- NULL
  
  for(row in nr:1){
    found_left <- F
    best_right_elem <- 0
    
    col <- 1
    while(col <= nc){
      if(symbol[(row-1)*nc+col] == 1 && !found_left){
        found_left <- T
        left_profile[row] <- ((row-1)*nc+col)
      }
      if(symbol[(row-1)*nc+col] == 1){
        best_right_elem <- ((row-1)*nc+col)
      }
      col <- col +1
    }
    right_profile[row] <- best_right_elem
  }
  
  return (list(left=left_profile, right=right_profile))
}

compute_centroid <- function (symbol, nr=5, nc=3){
  nb_pts <- 0
  sum_col <- 0
  sum_row <- 0
  
  for(row in 1:nr){
    for(col in 1:nc){
      if(symbol[(row-1)*nc+col] == 1){
        sum_row <- sum_row + row
        sum_col <- sum_col + col
        nb_pts <- nb_pts +1
      }
    }
  }
  
  return (list(row=round(sum_row/nb_pts), col=round(sum_col/nb_pts)))
}


compute_img <- function(symbol, nr, nc){
  img <- NULL
  for(i in 1:(nr*nc)){
    if(i %in% symbol){
      img <- c(img, 1)
    }
    else{
      img <- c(img, 0)
    }
  }
  
  return (img)
}