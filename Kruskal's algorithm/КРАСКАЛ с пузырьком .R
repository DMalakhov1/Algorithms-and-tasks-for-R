library(igraph)

edges <- c(1,2, 
           2,3, 
           3,4,
           2,5,
           3,5,
           4,5,
           1,5
)
g<-graph(edges, n=max(edges), directed=FALSE)
E(g)$weight <- c(3,5,2,4,6,7,1)

Bubble_Sort <- function(VES, edges){
  n <- length(VES)
  for (i in 1:(n-1)){
    for (j in 1:(n-i)){
      k <- (j-1)*2+1
      if (VES[j]>VES[j+1]){
        element <- VES[j] 
        VES[j] <- VES[j+1]
        VES[j+1] <- element 
        
        Chast <- c(edges[k], edges[k+1])
        edges[k] <- edges[k+2] 
        edges[k+1] <- edges[k+3] 
        edges[k+2] <- Chast[1] 
        edges[k+3] <- Chast[2] 
      } 
    } 
  } 
  return(edges) 
} 

PROSORT <- Bubble_Sort(E(g)$weight, edges)
#Что произошло, мы отсортировали веса ребер с помощью функции Bubble_Sort и сохраняем отсортированный список в PROSORT

#SPISOCHEK нужен для разбиения графа на непересекающиеся множества вершин
SPISOCHEK <- function(g){ 
  spisok <- list() 
  for (i in 1:vcount(g)){ #Внутри цикла for берем номер каждой вершины и помещяем ее в список spisok
    
    spisok[[i]] <- i
  } 
  return(spisok) 
} 

SVIAZ <- SPISOCHEK(g) #Получаем SVIAZ как набор всех вершинных множеств графа g, используя функцию SPISOCHEK

kruskal_algorithm <- function(PROSORT, SVIAZ){ 
  n <- length(PROSORT)
  MST <- c() 
  for (i in 1:n){ 
    n <- i * 2 - 1 #вычисляется номер ребра в списке PROSORT
    a <- 0 
    b <- 0 
    for (j in 1:length(SVIAZ)){ #перебираем все списки вершин-компонент связности из списка componentы
      if (PROSORT[n] %in% SVIAZ[[j]]){
        a <- j 
        #Если вершина PROSORT[n] находится в текущей компоненте связности, то номер этой компоненты присваивается переменной a.
      } 
      if (PROSORT[n+1] %in% SVIAZ[[j]]){ 
        b <- j 
        # Тут аналогично с a
      } 
    } 
    if (a != b) { 
      SVIAZ[[a]] <- c(SVIAZ[[a]], SVIAZ[[b]]) 
      SVIAZ <- SVIAZ[-b] 
      MST <- c(MST, PROSORT[n], PROSORT[n+1]) 
    } 
    #Если переменные a и b не равны, то компонента связности a объединяется с компонентой связности b,
    #а компонента b удаляется из списка компонент связности SVIAZ
    #Также добавляются два ребра с номерами n и n+1 из списка PROSORT в список MST
    if (length(SVIAZ) == 1){ 
      break 
      #Если количество компонент связности становится равным 1, то цикл завершается
    } 
  } 
  return(MST) 
} 

otvet <- kruskal_algorithm(PROSORT,SVIAZ) 

#тк по цифрам ничего не поймнешь и как-то лень, то делаем рисунок

WAY <- get.edge.ids(g,otvet) #это взял с сайта https://cran.r-project.org/web/packages/igraph/vignettes/igraph.html , 
#как все красить с https://stackoverflow.com/questions/58626708/how-do-i-color-edges-and-vertices-in-igraph
E(g)$color <- "#7FFFD4" #Если интересно выбор цветов находиться здесь https://r-charts.com/colors/ ну я поигрался в любом случае 
E(g)[WAY]$color <- "violetred"
plot(g,edge.label= E(g)$weight)