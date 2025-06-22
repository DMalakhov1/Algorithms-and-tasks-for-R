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

#Визуализируем граф 

GG <- graph_from_adjacency_matrix(TEST, weighted = NULL, mode =c("undirected"))
plot.igraph(GG)


labirint <- function(TEST, VHOD, EXIT, vis= rep(0, length(TEST[1,]))){
  vis[VHOD] = 1
  if (VHOD == EXIT){
    res <- c(EXIT)
    return(res)
  }
  AL <- which(TEST[VHOD,] != 0)
  res <- c()
  for (i in AL){
    if (vis[i] == 0){
      R <- labirint(TEST, i, EXIT, vis)
      if (length(R) != 0){
        res <- c(VHOD, R)
        return(res)
      }
    }
  }
  return(NULL) #ввел навсякий случай, вдруг граф окажеться несвязным...Методички на седьмую задачу нет, условие уточнить не могу
}
print(labirint(TEST, 1, 7))