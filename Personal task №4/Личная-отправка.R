MinR <- function(R, A){ 
  min <- Inf 
  m <- "ХиХиХаХа" 
  for (i in 1:length(A)){ 
    if ((R[i] < min) & (A[i] == 0)){ 
      min <- R[i]
      m <- i 
    }
  }
  return(m) 
}

Dijkstra <- function(M, v1, v2){
  
  if(!is.matrix(M) || nrow(M) != ncol(M)){ 
    stop("Введите квадратную матрицу")
  }
  
  n <- ncol(M)
  if (!(v1 %in% (1:n))){
    stop("такая вершина не существует")
  }
  if (!(is.numeric(v1))){ 
    stop("вершина должна задаваться числом")
  }
  if (!(v2 %in% (1:n))){ 
    stop("такая вершина не существует")
  }
  if (!(is.numeric(v2))){ 
    stop("вершина должна задаваться числом")
  }
  if (v1 == v2) { 
    stop("вводи разные вершины")
  }
  R <- M[v1, ] #массив демонстрирует смежные вершины их достижения 
  A <-  rep(0, n) #заполняем массив A нулями n раз
  A[v1] <- 1 #массив демонстрирует то, какие вершины мы уже прошли p.s. сначала стартовую вершину
  Predki <-  rep(0, n) #заполняем массив Predki нулями n раз
  Predki[(M[v1, ] != Inf)] <- v1 #добавляем в массив предков v1
  
  while (sum(A) != length(A)){ #пока массив A не будет полностью состоять из единиц
    Vertex <- MinR(R, A) #результат функции MinR (т.е. m => новая вершина с которой дальше работаем)
    if (Vertex == "ХиХиХаХа") {break()} #если не выполнено одно из условий: (R[i] < min) & (A[i] == 0)
    
    for (i in (1:n)) { 
      if (R[i] > (R[Vertex] + M[Vertex, i])){ 
        R[i] <- R[Vertex] + M[Vertex, i] 
        Predki[i] <- Vertex #это готовый массив предков
      }
      A[Vertex] <- 1
    }
  }
  
  if (Predki[v2] != 0){ #если предок конечной вершины не равен нулю, значит мы можем распутать массив
    path <- v2 #значит путь заканчивается v2 (конечной вершиной)
    if (Predki[v2] == v1){ #если предок конечной вершины равен начальной вершине, то путь просто от начальной до конечной
      path <- c(v1, v2) #вот собственно и этот путь
    }else{ #если путь более сложен, то попадаем в следующий цикл
      while (Predki[v2] != v1){ #пока предок v2 не будет равен вершине v1
        Predki[v2] <- Predki[path[1]] #Предок конечной вершины будет
        path <- c(Predki[v2], path) 
      }
    }
  }else{
    path <- Inf #если в массиве предков на индексе конечной вершины все же стоит 0 то путь равен inf
  }
  length <- R[v2]
  return(list(length,path))
  
}

M <- matrix(0, nrow = 6, ncol = 6) #через матрицу смежности задаем граф
M[1,] <- c(Inf, Inf, 1, 5, 2, 1)
M[2,] <- c(Inf, Inf, 1, 1, 4, 2)
M[3,] <- c(1, 1, Inf, 3, 8, 3)
M[4,] <- c(5, 1, 3, Inf, 4, 4)
M[5,] <- c(2, 4, 8, 4, Inf,5)
M[6,] <- c(1, 2, 3, 4, 5, Inf)

library(igraph) #подключаем библиотеку
par(bg = "#ffe9f4")
set.seed(1) #устанавливаем зерно, чтобы с каждым новым запуском кода, граф имел единую форму
g <- M #обозначаем матрицу смежности за g для простоты дальнейшего использования
g[g == Inf] <- 0 #если все ячейки матрицы смежности будут заполнены inf то ни одна вершина не связана с другой
a <- graph.adjacency(g, mode = "directed", weighted = T) #задаем тип графа (от матрицы M, ориентированный, взвешенный)
plot.igraph(a, edge.label = c(t(g)[t(g) != 0]), 
            edge.arrow.size = 0.5, layout = layout_in_circle)
plot.igraph(a, edge.label = c(t(g)[t(g) != 0]), 
            edge.arrow.size = 0.5)

#Не пересекающийся по вершинам 
bez_v <- function(M, len, v1, v2){
  TheEndAnswer <- "нулевая итерация" #ну и конечный
  while(len != 0){
    answerV <- Dijkstra(M, v1, v2)
    RD <- unlist(answerV)
    len <- RD[1]
    path <- RD[2:length(RD)]
    cat(' len :  ',len)
    cat(' пас : ',path)
    TheEndAnswer <- c(TheEndAnswer, list(path))
    k <- unlist(TheEndAnswer)
    if(k[length(k)] == Inf){
      break()
    }
    
    if (length(path) == 2) {       #т е нач и кон верш имеют 1 ребро и оно самое короткое
      M <- Delite_rebro(M, v1, v2)
    }else{
      cat('  ав:  ',RD)
      cat('  ав123:  ',RD[1],RD[2],RD[3])
      b <- length(RD)
      pathin <- RD[3:(b-1)]
      cat('  pathin ;    ',pathin)
      M <- Delite_vershina(M, pathin)
    }
  }
  SuperAnswer <- TheEndAnswer[1:length(TheEndAnswer)-1]
  return(SuperAnswer)
}

#удаление ребра из матрицы в случае когда на одной из итераций цикла без вершин появляется путь v1 - v2
Delite_rebro <- function(M, v1, v2) {
  M[v1, v2] <- Inf
  M[v2, v1] <- Inf
  return(M)
}

#удаление вершины из матрицы
Delite_vershina <- function(M, pathin) {
  for(i in pathin[1]:pathin[length(pathin)]){
    M[i, ] <- Inf
    M[, i] <- Inf
  }
  return(M)
}

#не пересекающийся по ребрам 
bez_r <- function(M, len, v1, v2){
  TheEndAnswer <- "нулевая итерация"
  while(len != 0){
    answerR <- Dijkstra(M, v1, v2)
    RD <- unlist(answerR)
    len <- RD[1]
    path <- RD[2:length(RD)]
    TheEndAnswer <- c(TheEndAnswer, list(path))
    k <- unlist(TheEndAnswer)
    if(k[length(k)] == Inf){
      break()
    }
    M <- Delite_rebro_v_bez_r(M, path)
  }
  SuperAnswer <- TheEndAnswer[1:length(TheEndAnswer)-1]
  return(SuperAnswer)
}


#удаление ребра из матрицы в случае когда на одной из итераций цикла без вершин появляется путь v1 - v2
Delite_rebro_v_bez_r <- function(M, path) {
  for(i in 1:length(path)-1){
    M[path[i], path[i+1]] <- Inf
    M[path[i+1], path[i]] <- Inf
  }
  return(M)
}

#главная функция 
main <- function(ask, v1, v2){
  answer <- Dijkstra(M, v1, v2) #состоит из 2-ух частей 
  ans <- unlist(answer)
  len <- ans[1]
  print("1 - непересекающиеся по вершинам, 2 - непересекающиеся оп рёбрам")
  if (ask == 1) { #ну тут и просходит выбор (см низ, где про выбор)
    otvet <- bez_v(M, len, v1, v2)
  }else{
    otvet <- bez_r(M, len, v1, v2)
  }
  return(otvet)
}
main(1, 1, 5) # 1 место - вершины, остальные цифры - ребра
Dijkstra(M,1,5)
