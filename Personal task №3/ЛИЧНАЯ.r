x <- c(1, 1, 1, 1, 1, 1)
FP <- function(x){
  if (length(x)==0 || length(x)==1){
    print("Сортировка не может быть выполнена")
  }
  sum_k <- x[1] 
  maxK <- x[1]
  for (i in 2:length(x)){
    sum_k <- sum_k+x[i]
  }
  if (sum_k==0){ #случай, массив, состоящий из нулей
    print("Сортировка не может быть выполнена")
  }
  for (i in 1:(length(x)-1)){
    for (j in 1:(length(x)-i)){
      if (x[j]<x[j+1]){
        k <- x[j]
        x[j] <- x[j+1]
        x[j+1] <- k
        
      }
    }
  }
  return(x)
}

f <- function(x){
  x <- FP(x)
  kucha1 <- c(x[1])
  kucha2 <- c(x[2])
  x <- x[-c(1,2)]
  
  while (length(x)>0){
    if (sum(kucha1) > sum (kucha2)){
      kucha2 <- c(kucha2,x[1])
      x<- x[-1]
    }else{
      kucha1 <- c(kucha1,x[1])
      x<- x[-1]
    }
    
    # проверка на правдивость
    if (sum(kucha1)/sum(kucha2)<=1.5 & sum(kucha1)/sum (kucha2)>=1/1.5){
      print('куча один')
      print(kucha1)
      print('куча два')
      print(kucha2)
    }else{
    print("Сортировка не может быть выполнена")
    
  }
}}
print(f(x))
