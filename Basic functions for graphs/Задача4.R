library(igraph)

#Создание матрицы смежности графа 
NEW_GRAPH <- function(x){
G <- matrix(data = 0, nrow = x, ncol = x)
return(G)
} 

TEST <- NEW_GRAPH(10)
print(TEST)

#Добавление ребра заданного веса 
DOP_REBRO <- function(matrix, i, j, weight){
matrix[i, j] <- weight
matrix[j, i] <- weight
return(matrix)
}

TEST <- DOP_REBRO(TEST, 1, 5, 40)
print(TEST)

#Удаление ребра между вершинами v1 и v2
Delite_REBRO <- function(matrix, v1, v2){
matrix[v1, v2] <- 0
matrix[v2, v1] <- 0
return(matrix)
}

TEST <- Delite_REBRO(TEST, 1, 5)
print(TEST)

#Добавление вершины
DOP_Vershina <- function(matrix, vershina){
#Добавляем новую строку и столбец в матрицу
new_size <- nrow(matrix) + 1
new_matrix <- matrix(0, nrow = new_size, ncol = new_size)
new_matrix[1:nrow(matrix), 1:ncol(matrix)] <- matrix
#Добавляем вершину в последнюю строку и столбец матрицы
new_matrix[new_size, ] <- 0
new_matrix[, new_size] <- 0
return(new_matrix)
}

TEST <- DOP_Vershina(TEST, 11)
print(TEST)

#удаление заданной вершины 
Delite_Vershina <- function(matrix, vershina){
matrix <- matrix[-vershina, ]
matrix <- matrix[ , -vershina]
}
TEST <- Delite_Vershina(TEST, 10)
print(TEST)

#Получение списка вершин, смежной с заданной 
SPISOK_Vershin <- function(matrix, vershina){
  Smeshnaia_vershina <- which(matrix[vershina,] != 0)
  return(Smeshnaia_vershina)
}

TEST <- SPISOK_Vershin(TEST, 3)
print(TEST)











  




