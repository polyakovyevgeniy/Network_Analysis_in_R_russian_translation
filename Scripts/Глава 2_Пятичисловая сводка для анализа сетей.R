# Подготовка
install.packages("statnet")
install.packages("devtools")
library(devtools)
install_github("DougLuke/UserNetR")

library(statnet)
library(UserNetR)
data(Moreno)

# Простая визуализация

gender <- Moreno %v% "gender"
plot(Moreno, vertex.col = gender + 2, vertex.cex = 1.2)

# Размер сети

network.size(Moreno)
summary(Moreno,print.adj=FALSE)

# Плотность сети

den_hand <- 2*46/(33*32)
den_hand
gden(Moreno)

# Компоненты сети

components(Moreno)

# Диаметр сети

lgc <- component.largest(Moreno,result="graph")
gd <- geodist(lgc)
max(gd$gdist)

# Коэффициент кластеризации сети
gtrans(Moreno,mode="graph")
