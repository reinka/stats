sqrt_dev <- function(list, means=0, within=TRUE){
  res = 0
  count = 1
  n = sapply(list,length)
  if (means == 0 & within){
    means = sapply(list, mean) 
    count = 1
    for (i in list){
      res = res + sum((i - means[count])^2)
      count = count + 1
    }
  }
  else if (!within){
    temp = sapply(list, mean)
    means = mean(temp)
    index = 1:length(temp)
    for (i in index){
      res = res + n[i]*(temp[i] - means)^2
    }
  }
  else{
    for (i in list){
       res = res + sum((i - means)^2)
    }
  }
  
   unclass(res)
}