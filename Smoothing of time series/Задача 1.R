#Блок "загрузка данных"
setwd("/Users/dmitry/Downloads/specdata") #путь к файлу
N <- 2#ВВЕДИТЕ НОМЕР ФАЙЛА
FN <- sprintf("%0.3i.%s", N, "csv") 
print(FN)
DS <- read.csv(file = FN, sep = ",", header = TRUE)
WithoutNA <- subset(DS,(!(is.na(sulfate+nitrate))))
print(WithoutNA)

Date <- as.Date(WithoutNA$Date)
Sulfate <- c(WithoutNA$sulfate)
Nitrate <- c(WithoutNA$nitrate)

#Сульфаты
plot(Date, Sulfate, xlab = "Date", ylab = "Value", type = "l", col="black") 

#Метод скользящего среднего для сульфатов 
mediana <- 11
VEC <- as.numeric(length(Sulfate))
m1 <- ((mediana-1)/2)
vv <- c(Sulfate)
for (i in (m1+1):(length(Sulfate)-m1)){
  vv[i] <- (sum(vv[(i-m1):(i+m1)]))/mediana
  Sulfate <- append(Sulfate, vv[i])
}
  lines(Date, vv, col="red", type = "l")

#Нитраты 
lines(Date, Nitrate, xlab = "Date", ylab = "Value", type = "l", col="blue") 

#Метод скользящего среднего для нитратов 
mediana2 <- 11
VEC2 <- as.numeric(length(Nitrate))
m2 <- ((mediana2-1)/2)
vv2 <- c(Nitrate)
for (i in (m2+1):(length(Nitrate)-m2)){
vv2[i] <- (sum(vv2[(i-m2):(i+m2)]))/mediana2
Nitrate <- append(Nitrate, vv2[i])
}
lines(Date, vv2, col="green", type = "l")

#Корреляция
PIRSON <- function(Sulfate, Nitrate){
  COR <- sum((Sulfate - mean(Sulfate))*(Nitrate - mean(Nitrate)))/ 
    (sqrt(sum((Sulfate - mean(Sulfate))^2))*
       sqrt(sum((Nitrate - mean(Nitrate))^2)))
  return(COR)
} 

#Блок проверки корреляции с помощью внутренней функции
print(cor.test(Sulfate, Nitrate))

  
  
