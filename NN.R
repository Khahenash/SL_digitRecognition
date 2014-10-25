source('~/Documents/Signal/Projet/usefullTools.r')

compute_profile <- function (symbol, nr=5, nc=3){
  left_profile <- NULL
  right_profile <- NULL
  top_profile <- NULL
  bottom_profile <- NULL
  
  for(row in nr:1){
    found_left <- F
    best_right_elem <- NULL
    
    col <- 1
    while(col <= nc){
      if(symbol[(row-1)*nc+col] == 1 && !found_left){
        found_left <- T
        left_profile <- c(left_profile, ((row-1)*nc+col))
      }
      if(symbol[(row-1)*nc+col] == 1){
        best_right_elem <- ((row-1)*nc+col)
      }
      col <- col +1
    }
    right_profile <- c(right_profile, best_right_elem)
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

symb <- Load_Obs("/home/khahenash/Documents/Signal/Projet/data_Digits_HMM/Data5X3/Train_compute_symbol_5_3Digit1.txt")[1,]
img = compute_img(symb,5,3)
m = matrix(img, 5, 3, byrow=T)
compute_profile(img, 5, 3)
compute_centroid(img, 5, 3)

