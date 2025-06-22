#вспомогательные данные 
a <- 2 
b <- 5
t <- 15
Pogreshnost <- 0.1
x <- seq(1, 10, by=0.1)
fun <- function(x){(x*cos(x)^2) + (x*sin(x))}
y <- fun(x)

#Принципы золотого сечения 
# -делит интервал на неравные части 
# -отношение интервалов - const
# -является предельной формой поиска методом Фибоначчи

#Золотое сечение 
golden_ratio <- function(a, b, Pogreshnost){
  GOLD <- ((1+sqrt(5))/2)
  x1 <- b - ((b-a)/GOLD)
  x2 <- a + ((b-a)/GOLD)
  while (abs(b-a)>Pogreshnost){
    if (fun(x1)<=fun(x2)){
      b <- x2
      x2 <-  x1
      x1 <-  b - (x2-a)
    } else {
      a <- x1
      x1 <-  x2
      x2 <-  a + (b-x1)
    }
  }
  REC <- (x1+x2)/2 #Точка экстремума 
  cat("Значение REC", REC)
  return(REC)
}

#Фибоначчи 
FIB <- function(t){
  if (t<0){
    return("число должно быть больше нуля и целым")
  } else if (t==1){
    return(0)
  } else if (t==2){
    return(1)
  } else {
    return(FIB(t-1)+FIB(t-2))
  }
}

Chisla_FIB <- function(a, b, Pogreshnost, t){
  x1 <- a + (b-a)*(FIB(t-2)/FIB(t))
  x2 <- a + (b-a)*(FIB(t-1)/FIB(t))
  for (i in 1:(t-3)){
    if (fun(x1)<fun(x2)){
      b <- x2
      x2 <- x1
      x1 <- b - (x2-a)
    } else {
      a <- x1
      x1 <- x2
      x2 <- a + (b-x1)
    }
  }
  REC2 <- (x1+x2)/2 #Точка экстремума 
  cat("Значение REC2", REC2)
  return(REC2)
}

golden_ratio(a, b, Pogreshnost) # Проверка для метода Золотого сечения
Chisla_FIB(a, b, Pogreshnost, t) # Проверка для метода Фибоначчи