#Лабораторная работа 

#Сортировки 

#Пузырек 
FP <- function(A){
  for (i in 1:(length(A)-1)){
    for (j in 1:(length(A)-i)){
      if (A[j]<A[j+1]){
        k <- A[j]
        A[j] <- A[j+1]
        A[j+1] <- k
      }
    }
  }
  return(A)
}



#Сортировка выбором
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
        }
      }
    }
  }
  return(FV)  
}

#Сортировка вставками
FVS <- function(A){
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


#Cлияние 
MSORT <- function(S){   
  if (length(S)<2){      
    return(S)
  } else { 
    k <- floor(length(S)/2)
    PART1 <- MSORT(S[1:k])
    PART2 <- MSORT(S[(k+1):length(S)])
    res <- FSORT(PART1, PART2)
    return(res)
  }
}
FSORT <- function(PART1, PART2){  
  k <- c()                      
  if (PART1[length(PART1)]<=PART2[1]){  
    k <- c(PART1, PART2)
  } else if (PART2[length(PART2)]<=PART1[1]){ 
    k <- c(PART2, PART1)
  } else {
    while(length(PART1)>0 & length(PART2)>0){
      if (PART1[1]<=PART2[1]){ 
        k <- c(k, PART1[1])
        PART1 <- PART1[-1] 
      } else {
        k <- c(k, PART2[1]) 
        PART2 <- PART2[-1]
      } 
    }
    while(length(PART1)>0){ 
      k <- c(k, PART1[1])
      PART1 <- PART1[-1]
    }
    while(length(PART2)>0){
      k <- c(k, PART2[1])
      PART2 <- PART2[-1]
    }
  }
  return(k)
}

#Лабораторная работа


Q <- as.numeric(as.vector(as.matrix(read.csv2("/Users/dmitry/Downloads/1000.csv",sep=";",header=FALSE)))) 
W <- as.numeric(as.vector(as.matrix(read.csv2("/Users/dmitry/Downloads/3000.csv",sep=";",header=FALSE)))) 
E <- as.numeric(as.vector(as.matrix(read.csv2("/Users/dmitry/Downloads/6000.csv",sep=";",header=FALSE)))) 
R <- as.numeric(as.vector(as.matrix(read.csv2("/Users/dmitry/Downloads/9000.csv",sep=";",header=FALSE)))) 

VREMIA1 <- c(system.time(FP(Q))[3], system.time(FP(W))[3], system.time(FP(E))[3], system.time(FP(R))[3]) 
VREMIA2 <- c(system.time(FV(Q))[3], system.time(FV(W))[3], system.time(FV(E))[3], system.time(FV(R))[3])
VREMIA3 <- c(system.time(FVS(Q))[3], system.time(FVS(W))[3], system.time(FVS(E))[3], system.time(FVS(R))[3])
VREMIA4 <- c(system.time(MSORT(Q))[3], system.time(MSORT(W))[3], system.time(MSORT(E))[3], system.time(MSORT(R))[3])


KOLVO_EL_MASSIVA <- c(1000, 3000, 6000, 9000)


plot(KOLVO_EL_MASSIVA, VREMIA1)
lines(KOLVO_EL_MASSIVA, VREMIA1, col = "#0099CC")
lines(KOLVO_EL_MASSIVA, VREMIA2, col = "#FF6600")
lines(KOLVO_EL_MASSIVA, VREMIA3, col = "#99FF33")
lines(KOLVO_EL_MASSIVA, VREMIA4, col = "#00FFFF")

