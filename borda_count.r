
normalize <- function (m){
  for(r in 1:dim(m)[1]){
    sum <- 0
    
    for(c in 1:dim(m)[2]){
      sum <- sum + m[r,c]
    }
    
    for(c in 1:dim(m)[2]){
      m[r,c] <- m[r,c]/sum
    }
  }
  
  return(m)
}



borda_classify <- function (m1, m2, coef1=0.5 , coef2=0.5){
  
  # normalisation
  m1 <- normalize(m=m1)
  m2 <- normalize(m=m2)
  c1c2 = (coef1+coef2)
  coef1 = coef1/c1c2
  coef2 = coef2/c1c2
  
  if(dim(m1)[1] == dim(m2)[1] && dim(m1)[2] == dim(m2)[2]){
    result <- matrix(data=0, nrow=dim(m1)[1], ncol = dim(m1)[2])
    
    for(r in 1:dim(m1)[1]){
      for(c in 1:dim(m1)[2]){
        result[r,c] = m1[r,c]*coef1 + m2[r,c]*coef2 
      }
    }
    return (result)
  }
  else{
    return (NULL)
  }
}


# initialisation pseudo-aléatoire de deux matrices
m1 = matrix(data=sample(0:100, 1000, T), nrow = 10, ncol = 5)
m2 = matrix(data=sample(0:100, 1000, T), nrow = 10, ncol = 5)

res = borda_classify(m1, m2, 20, 80)
print(res)
