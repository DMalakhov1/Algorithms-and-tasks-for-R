vector <- c(-94, 44, 7, 15, 23, 68, -77, 99, -1, 17)
print(vector)
max <- vector[1]
min <- vector[1]
nmax <- vector[1]
nmin <- vector[1]
sum_n <- vector[1]
for (i in 2:length(vector)){
  sum_n <- sum_n+vector[i]
  
  if (vector[i]>max){
    
  max <- vector[i]
  nmax <- i
  if (vector[i]<min){
    
    min <- vector[i]
    nmin <- i
  }
  }
}

print(min)
print(max)
print(sum_n)
