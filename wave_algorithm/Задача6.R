#подготовительные данные для основного алгоритма 
library(igraph)

#Необходимые мне функции из задачи №4

#Создание матрицы смежности графа 
NEW_GRAPH <- function(x){
  G <- matrix(data = 0, nrow = x, ncol = x)
  return(G)
} 
TEST <- NEW_GRAPH(10)

#Добавление ребра заданного веса 
DOP_REBRO <- function(matrix, i, j, weight){
  matrix[i, j] <- weight
  matrix[j, i] <- weight
  return(matrix)
}

#Создаю граф из методички, путем добавления ребер
TEST <- DOP_REBRO(TEST, 1, 2, 1)
TEST <- DOP_REBRO(TEST, 2, 3, 1)
TEST <- DOP_REBRO(TEST, 1, 5, 1)
TEST <- DOP_REBRO(TEST, 2, 5, 1)
TEST <- DOP_REBRO(TEST, 2, 6, 1)
TEST <- DOP_REBRO(TEST, 3, 6, 1)
TEST <- DOP_REBRO(TEST, 5, 6, 1)
TEST <- DOP_REBRO(TEST, 3, 7, 1)
TEST <- DOP_REBRO(TEST, 4, 7, 1)
TEST <- DOP_REBRO(TEST, 4, 8, 1)
TEST <- DOP_REBRO(TEST, 7, 8, 1)
TEST <- DOP_REBRO(TEST, 3, 9, 1)
TEST <- DOP_REBRO(TEST, 5, 9, 1)
TEST <- DOP_REBRO(TEST, 6, 9, 1)
TEST <- DOP_REBRO(TEST, 7, 9, 1)
TEST <- DOP_REBRO(TEST, 8, 9, 1)
TEST <- DOP_REBRO(TEST, 1, 10, 1)
TEST <- DOP_REBRO(TEST, 5, 10, 1)
TEST <- DOP_REBRO(TEST, 8, 10, 1)
TEST <- DOP_REBRO(TEST, 9, 10, 1)
print(TEST)

relatives <- function(graph, NACHALO_PUTI){ #Под NACHALO_PUTI вводим номер вершины для которой хотим построить родословную 
  n <- nrow(graph) #определяем кол-во вершин в графе 
  vector <- numeric(n) #создаем вектор размером n 
  vector[NACHALO_PUTI] <- 1 #наш нулевой уровень 
  for (L in 1:(n-1)) { # Проходим по уровням графа 
    parents <- which(vector == L) #список вершииииин
    for (parents in parents) { #пройдемся по всем родительским вершинам вот тут я не очень понял как это можно записать, но с семинара помню, что должно быть 3 цикла for
      spv <- which(graph[parents,] > 0) # Получаем список смежных вершин для текущей родительской вершины 
      for (spv in spv) { #проходимся по всем смежным вершинам, вот тут я не очень понял как это можно записать, но с семинара помню, что должно быть 3 цикла for
        if (spv != NACHALO_PUTI & vector[spv] == 0) {#проверяем, не была ли вершина на более раннем этапе 
          vector[spv] <- L + 1 # устанавливаем уровень потомка 
        }
      }
    }
  }
  
  return(vector-1) #Поправили :)
}

relatives(TEST, 4)

#Раскрутка 

wave_algorithm <- function(TEST, NACHALO_PUTI, END){
  n <- nrow(TEST) #кол-во вершин 
  LEVELS <- relatives(TEST, NACHALO_PUTI) #тут лежит массив уровней
  LAST_TOP <- LEVELS[END] #уровень последней вершины 
  MY_WAY <- c(NACHALO_PUTI, rep(NA, (LAST_TOP-1)), END) #наш невероятный и захватывающий воображение путь 
  while(LAST_TOP>1){
    for(i in 1:n){
      if ((LEVELS[i] == (LAST_TOP-1)) & (TEST[END, i] == 1)){ #чек на то, что у вершины предыдущий уровень + что есть связь 
        MY_WAY[LAST_TOP] <- i #переназначение конечной вершины и опять раскрутка...
        END <- i
        LAST_TOP <- LEVELS[END]
      }
    }
  }
  return(MY_WAY)
}
wave_algorithm(TEST, 1, 4)
