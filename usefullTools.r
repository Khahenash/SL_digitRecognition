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

learnVal <- function (dataFt, dataTarg,trainId, valId, nbN, nbLoop, Nbdecay=1e-4){
  # init a new random MLP
  # nbN : nombre de neurones dans la couche cachée
  # pas d'iter pour avec un nn aléatoire
  
  new_nn <- nnet(dataFt[trainId,], dataTarg[trainId,], size=nbN, maxit=0, decay=Nbdecay, rang = 1)
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
  for(i in 1:nbLoop){
    #continue the training
    new_nn <- nnet(dataFt[trainId,], dataTarg[trainId,], size=nbN, maxit=200, decay=Nbdecay, Wts=curr_w)
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

cross_val <- function (dataFt, dataTarg, nbN, nbLoop, fold, Nbdecay){
  # mélange des indices
  ind <- sample(1:dim(dataFt)[1],dim(dataFt)[1])
  score <- NULL
  lstT <- NULL
  lstV <- NULL
  bestNN <- NULL
  bestScoreV <- 0
  
  matT <- matrix(data = 0, nrow = fold, ncol = nbLoop+1)
  for(i in 1:fold){
    cat("==================== nbN: ", nbN, " decay: ", Nbdecay, "nbLoop: ", nbLoop, " : ", i, "/", fold, " ====================\n")
    valId <- ind[((i-1)*floor(dim(dataFt)[1]/fold)+1):(i*floor(dim(dataFt)[1]/fold))]
    res <- learnVal(dataFt, dataTarg, -valId, valId, nbN, nbLoop, Nbdecay)
    
    if(i==1 || max(res$scoreV)>bestScoreV){
      bestScoreV <- max(res$scoreV)
      bestNN <- res$nn
    }
    score <- c(score, res$scoreTrain)
    
    lstT <- c(lstT,min(res$mseT))
    lstV <- c(lstV, min(res$mseV))
  }
  
  return(list(mseT = lstT, mseV = lstV, best_nn = bestNN))
}


compute_profiles <- function (symbol, nr=5, nc=3){
  left_profile <- matrix(data=0, nrow = 1, ncol = nr)
  right_profile <- matrix(data=0, nrow = 1, ncol = nr)
  top_profile <- matrix(data=0, nrow = 1, ncol = nc)
  bottom_profile <- matrix(data=0, nrow = 1, ncol = nc)
  
  for(row in nr:1){
    found_left <- F
    best_right_elem <- 0
    
    for(col in 1:nc){
      if(symbol[(row-1)*nc+col] == 1 && !found_left){
        found_left <- T
        left_profile[row] <- ((row-1)*nc+col)
      }
      if(symbol[(row-1)*nc+col] == 1){
        best_right_elem <- ((row-1)*nc+col)
      }
    }
    right_profile[row] <- best_right_elem
  }
  
  for(col in 1:nc){
    found_top <- F
    best_bottom_elem <- 0
    
    for(row in nr:1){
      if(symbol[(row-1)*nc+col] == 1 && !found_top){
        found_top <- T
        top_profile[col] <- ((row-1)*nc+col)
      }
      if(symbol[(row-1)*nc+col] == 1){
        best_bottom_elem <- ((row-1)*nc+col)
      }
    }
    bottom_profile[col] <- best_bottom_elem
  }
  
  return (list(left=left_profile, right=right_profile, top=top_profile, bottom=bottom_profile))
}


compute_projections <- function(symbol, nr=5, nc=3){
  row_proj = matrix(0, 1, nr)
  col_proj = matrix(0, 1, nc)
  
  for(col in 1:nc){
    for(row in 1:nr){
      if(symbol[(row-1)*nc+col] == 1){
        row_proj[row] = row_proj[row]+1
        col_proj[col] = col_proj[col]+1
      }
    }
  }
  
  return(list(row=row_proj, col=col_proj))
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