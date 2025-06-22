A <- c(99, -2, 7, 21, -34, 82, 50, 45, -1, 4)
FP <- function(A){
  for (i in 1:(length(A)-1)){
    for (j in 1:(length(A)-i)){
      if (A[j]>A[j+1]){
        k <- A[j]
        A[j] <- A[j+1]
        A[j+1] <- k
        print(A) #необязательно ввел, но это чтобы посмотреть наглядно 
      }
    }
  }
  return(A)
}
FP(A)

