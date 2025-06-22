#Входные данные 
FUN <- function(x, y){(10*sin(sqrt(x^2+y^2))*cos(sqrt(x^2+y^2))^2)/(sqrt(x^2+y^2))}
FUN2 <- function(a){(10*sin(sqrt(a[1]^2+a[2]^2))*cos(sqrt(a[1]^2+a[2]^2))^2)/(sqrt(a[1]^2+a[2]^2))}

a <- c(5, 5) #начальный вектор 
h <- 10 #начальное приращение 
d <- 0.01 #погрешность поиска d 
ex <- c(1, 0) #базисный вектор 
ey <- c(0, 1) #базисный вектор

x <- seq(-10, 10, by=0.2)
y <- seq(-10, 10, by=0.2)
z <- outer(x, y, FUN)

#График
mt <- persp(x, y, z) #трехмерный 
xy <- trans3d(a[1], a[2], a[3], mt)
points(xy$x, xy$y, col="red")

#алгоритм Хука-Дживса
Hooke_Jeeves <- function(a){
  a1 <- a
  while(h>d){
    if(FUN2(a1+h*ex)>FUN2(a1)){
      a1 <- a+h*ex
    } else {
      if(FUN2(a1-h*ex)>FUN2(a1)){
        a1 <- a-h*ex
      } else{
        if(FUN2(a1+h*ey)>FUN2(a1)){
          a1 <- a+h*ey
        } else {
          if(FUN2(a1-h*ey)>FUN2(a1)){
            a1 <- a-h*ey
          }
        }
      }
    }
     if (all(a1==a)){
      h <- h/2
    } else {
      while(FUN2(2*a1-a) > FUN2(a1)){
         a <- a1
        a1 <- 2*a1-a
      }
      a <- a1
    }
  }
  
  ax <- a[1]
  ay <- a[2]
  Q <- FUN2(a)
  return(Q)
}
Q <- Hooke_Jeeves(a)




