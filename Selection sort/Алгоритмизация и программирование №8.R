#Сортировка выбором
A <- c(99, -2, 7, 21, -34, 82, 50, 45, -1, 4) 
FV <- function(A){
  for (i in length(A):1){
    max <- 1
    for (j in 1:i){
      if (A[j]>A[max]){ 
        max <- j
      } else { 
        if (max<i) {
          
        B <- A[max]
        A[max] <- A[i]
        A[i] <- B
        print(A)
        }
      }
      }
  }
  return(FV)  
}
FV(A)
