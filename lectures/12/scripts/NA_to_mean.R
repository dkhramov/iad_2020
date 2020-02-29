train$x61[which(is.na(train$x61))] = mean(train$x61, na.rm = T)

sum(is.na(train$x61))
sum(is.na(train[, c(61,62)]))

NA.to.MEAN = function(col){
  col[is.na(col)] = mean(col, na.rm = T)
  return(col)
}

apply(train[, c(9,24:62)], 2,NA.to.MEAN)

sum(is.na(NA.to.MEAN(train$x61)))
sum(is.na(apply(train[, c(9,24:62)], 2,NA.to.MEAN)))