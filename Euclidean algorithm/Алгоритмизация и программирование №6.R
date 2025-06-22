F <- function(a, b){ 
  if (abs(a==0) & abs(b!=0)){ 
    return(abs(b)) 
  } 
  if (abs(a!=0) & abs(b==0)){ 
    return(abs(a))
  } 
  if (abs(a==0) & abs(b==0)){ 
    return("Неопределенный")
  }
  if (abs(a!=0) & abs(b!=0)){
    while(abs(a)!=abs(b)) { 
      if (abs(a)>abs(b)){ 
        a <-abs(a)-abs(b)
      } 
      else { 
        b <-abs(b)-abs(a)} 
    } 
  }
    return(abs(a))
  } 

F(-5, -10)


