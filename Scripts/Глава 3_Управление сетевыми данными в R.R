# Создание объекта-сети в statnet

library(UserNetR)
library(statnet)
netmat1 <- rbind(c(0,1,1,0,0),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))
rownames(netmat1) <- c("A","B","C","D","E")
colnames(netmat1) <- c("A","B","C","D","E")
net1 <- network(netmat1,matrix.type="adjacency")
class(net1)
summary(net1)
gplot(net1, vertex.col = 2, displaylabels = TRUE)
netmat2 <- rbind(c(1,2),
                 c(1,3),
                 c(2,3),
                 c(2,4),
                 c(3,2),
                 c(5,3))
net2 <- network(netmat2,matrix.type="edgelist")
network.vertex.names(net2) <- c("A","B","C","D","E")
summary(net2)
as.sociomatrix(net1)
class(as.sociomatrix(net1))
all(as.matrix(net1) == as.sociomatrix(net1))
as.matrix(net1,matrix.type = "edgelist")

# Атрибуты узлов

set.vertex.attribute(net1, "gender", c("F", "F", "M", "F", "M"))
net1 %v% "alldeg" <- degree(net1)
list.vertex.attributes(net1)
summary(net1)
get.vertex.attribute(net1, "gender")
net1 %v% "alldeg"

# Атрибуты связей

list.edge.attributes(net1)
set.edge.attribute(net1,"rndval",
                   runif(network.size(net1),0,1))
list.edge.attributes(net1)
summary(net1 %e% "rndval")
summary(get.edge.attribute(net1,"rndval"))
netval1 <- rbind(c(0,2,3,0,0),
                 c(0,0,3,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,2,0,0))
netval1 <- network(netval1,matrix.type="adjacency",
                   ignore.eval=FALSE,names.eval="like")
network.vertex.names(netval1) <- c("A","B","C","D","E")
list.edge.attributes(netval1)
get.edge.attribute(netval1, "like")
as.sociomatrix(netval1)
as.sociomatrix(netval1,"like")

# Создание объекта-сети в igraph

detach(package:statnet)
library(igraph)
inet1 <- graph.adjacency(netmat1)
class(inet1)
summary(inet1)
str(inet1)
inet2 <- graph.edgelist(netmat2)
summary(inet2)
V(inet2)$name <- c("A","B","C","D","E")
E(inet2)$val <- c(1:6)
summary(inet2)
str(inet2)
library(intergraph)
class(net1)
net1igraph <- asIgraph(net1)
class(net1igraph)
str(net1igraph)

# Импорт сетевых данных

detach("package:igraph", unload=TRUE)
library(statnet)
netmat3 <- rbind(c("A","B"),
                 c("A","C"),
                 c("B","C"),
                 c("B","D"),
                 c("C","B"),
                 c("E","C"))
net.df <- data.frame(netmat3)
net.df
write.csv(net.df, file = "MyData.csv", row.names = FALSE)
net.edge <- read.csv(file="MyData.csv")
net_import <- network(net.edge, matrix.type="edgelist")
summary(net_import)
gden(net_import)
n1F <- get.inducedSubgraph(net1,which(net1 %v% "gender" == "F"))
n1F[,]
gplot(n1F,displaylabels=TRUE)
deg <- net1 %v% "alldeg"
n2 <- net1 %s% which(deg > 1)
gplot(n2,displaylabels=TRUE)
data(ICTS_G10)
gden(ICTS_G10)
length(isolates(ICTS_G10))
n3 <- ICTS_G10
delete.vertices(n3,isolates(n3))
gden(n3)
length(isolates(n3))

# Фильтрация на основе значений ребер

data(DHHS)
d <- DHHS
gden(d)
op <- par(mar = rep(0, 4))
gplot(d,gmode="graph",edge.lwd=d %e% 'collab', 
      edge.col="grey50",vertex.col="lightblue",
      vertex.cex=1.0,vertex.sides=20)
par(op)
as.sociomatrix(d)[1:6,1:6]
list.edge.attributes(d)
as.sociomatrix(d,attrname="collab")[1:6,1:6]
table(d %e%"collab")
d.val <- as.sociomatrix(d,attrname="collab")
d.val[d.val < 3] <- 0
d.filt <- as.network(d.val, directed=FALSE, 
                     matrix.type="a",ignore.eval=FALSE,
                     names.eval="collab")
summary(d.filt,print.adj=FALSE)
gden(d.filt)
op <- par(mar = rep(0, 4))
gplot(d.filt,gmode="graph",displaylabels=TRUE,
      vertex.col="lightblue",vertex.cex=1.3,
      label.cex=0.4,label.pos=5,
      displayisolates=FALSE)
par(op)
op <- par(mar = rep(0, 4))
d.val <- as.sociomatrix(d,attrname="collab")
gplot(d.val,gmode="graph",thresh=2,
      vertex.col="lightblue",vertex.cex=1.3,
      label.cex=0.4,label.pos=5,
      displayisolates=FALSE)
par(op)

# Преобразование направленной сети в ненаправленную

net1mat <- symmetrize(net1,rule="weak")
net1mat
net1symm <- network(net1mat,matrix.type="adjacency")
network.vertex.names(net1symm) <- c("A","B","C","D","E")
summary(net1symm)
