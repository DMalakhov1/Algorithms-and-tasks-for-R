#Сортировка вставками
A <- c(99, -2, 7, 21, -34, 82, 50, 45, -1, 4)
FV <- function(A){
for (i in 2:(length(A))){
K <- A[i]
J <- i-1
while(K<A[J] && J>0){
A[J+1]=A[J]
J=J-1 
A[J+1]=K
}
}
return(A)
}
FV(A)