#Алгоритм Дэйкстры 
#Задача №8

#Необходимые мне функции из задачи №4

#Создание матрицы смежности графа 
NEW_GRAPH <- function(x){
  G <- matrix(data = 0, nrow = x, ncol = x)
  return(G)
} 
TEST <- NEW_GRAPH(12)

#Добавление ребра заданного веса 
DOP_REBRO <- function(matrix, i, j, weight){
  matrix[i, j] <- weight
  matrix[j, i] <- weight
  return(matrix)
}


#Создаю граф из методички, путем добавления ребер
TEST <- DOP_REBRO(TEST, 1, 2, 5)
TEST <- DOP_REBRO(TEST, 1, 5, 2)
TEST <- DOP_REBRO(TEST, 2, 6, 1)
TEST <- DOP_REBRO(TEST, 2, 3, 7)
TEST <- DOP_REBRO(TEST, 5, 10, 3)
TEST <- DOP_REBRO(TEST, 5, 4, 6)
TEST <- DOP_REBRO(TEST, 5, 9, 9)
TEST <- DOP_REBRO(TEST, 9, 6, 7)
TEST <- DOP_REBRO(TEST, 1, 7, 10)
TEST <- DOP_REBRO(TEST, 7, 10, 4)
TEST <- DOP_REBRO(TEST, 8, 10, 15)
TEST <- DOP_REBRO(TEST, 4, 8, 11)
TEST <- DOP_REBRO(TEST, 8, 6, 3)
TEST <- DOP_REBRO(TEST, 8, 11, 4)
TEST <- DOP_REBRO(TEST, 6, 11, 7)
TEST <- DOP_REBRO(TEST, 11, 12, 6)
TEST <- DOP_REBRO(TEST, 3, 12, 5)
TEST <- DOP_REBRO(TEST, 7, 12, 8)
print(TEST)

#Все написаное выше, помогает нам создать граф, с которым мы будем дальше работать) 

dijkstra_algorithm <- function(TEST, START_VERTEX, END){
  #Функция принимает на вход 3 аргумента: TEST (граф), START_VERTEX (начальная вершина), END (конечная вершина)
  
  # Многочисленные проверки на существование "правильной" матрицы 
  if (!(START_VERTEX %in% (1:nrow(TEST)))){ #Проверяем на то, присутствует ли START_VERTEX в нашем графе
    stop("START_VERTEX не принадлежит графу")
  }
  if (!(is.numeric(START_VERTEX))){ #Проверяем, задана ли START_VERTEX числом 
    stop("START_VERTEX не задана числом")
  }
  if (!(END %in% (1:nrow(TEST)))){ #Проверяем на то, присутствует ли END в нашем графе
    stop("END не принадлежит графу")
  }
  if (!(is.numeric(END))){ #Проверяем, задана ли END числом
    stop("END не задана числом")
  }
  if(!is.matrix(TEST) || nrow(TEST) != ncol(TEST)){ #Проверяем, квадратная ли наша матрица? 
    stop("Введите квадратную матрицу")
  }
  
  distances <- rep(Inf, nrow(TEST)) #массив для хранения расстояния от начальной вершины до всех остальных 
  distances[START_VERTEX] <- 0 # Длина пути из этой вершины в нее саму (на семенаре обсуждали)
  predki <- rep(0, nrow(TEST)) #массив предшественников каждой вершины 
  visited <- c() #массив, просмотренных вершин
  
  # Основной цикл 
  while(length(visited) < nrow(TEST)){ #Цикл продолжается до тех пор, пока не будут посещены все вершины графа
    #Найти вершину с наименьшим расстоянием от начальной вершины
    DIS_SUB <- distances 
    DIS_SUB[visited] <- Inf
    smegnie <- which.min(DIS_SUB)
    
    #В общем и целом, этот блок кода позволяет нам найти следующую вершину, которую мы посетим в процессе поиска кратчайшего пути в графе.
    
    #Теперь мы должны ходить по смежным вершинам текущей вершины
    for (sosed in which(TEST[smegnie,] > 0)){
      if (!(sosed %in% visited)){
        MB_distance <- distances[smegnie] + TEST[smegnie,sosed]
        if (MB_distance < distances[sosed]) {
          distances[sosed] <- MB_distance
          predki[sosed] <- smegnie
        }
      }
    }
    
    # Помечаем выбранную вершину как просмотренную
    visited <- c(visited, smegnie)
    
    if (smegnie == END) {
      break #завершаем алгоритм досрочно, если достигнута конечная вершина 
    }
  }
  
  if (distances[END] == Inf) {
    stop("Нет пути") 
  }
  
  #строим путь от начальной вершины до конечной 
  path <- END
  predki_path <- predki[END]
  while (predki_path != START_VERTEX) {
    path <- c(predki_path, path)
    predki_path <- predki[predki_path]
  }
  path <- c(START_VERTEX, path)
  
  return(list(distances = distances[END], path = path))
}


dijkstra_algorithm(TEST, 1, 12)