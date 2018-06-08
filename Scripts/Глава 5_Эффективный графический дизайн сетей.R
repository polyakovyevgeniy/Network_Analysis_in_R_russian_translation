# Цвет узла

library(statnet)
library(UserNetR)
data(Bali)
gplot(Bali,vertex.col="slateblue2",gmode="graph")
col2rgb('slateblue2')
gplot(Bali,vertex.col=rgb(122,103,238,
                          maxColorValue=255),gmode="graph")
gplot(Bali,vertex.col="#7A67EE",gmode="graph")
ndum <- rgraph(300,tprob=0.025,mode="graph")
op <- par(mar = c(0,0,2,0),mfrow=c(1,2))
gplot(ndum,gmode="graph",vertex.cex=2,
      vertex.col=rgb(0,0,139,maxColorValue=255),
      edge.col="grey80",edge.lwd=0.5,
      main="Полностью непрозрачный")
gplot(ndum,gmode="graph",vertex.cex=2,
      vertex.col=rgb(0,0,139,alpha=80,maxColorValue=255),
      edge.col="grey80",edge.lwd=0.5,
      main="Частично прозрачный")
par(op)
rolelab <- get.vertex.attribute(Bali,"role")
op <- par(mar=c(0,0,0,0))
plot(Bali,usearrows=FALSE,vertex.cex=1.5,label=rolelab,
     displaylabels=T,vertex.col="role")
par(op)
palette()
library(RColorBrewer)
display.brewer.pal(5, "Dark2")
my_pal <- brewer.pal(5,"Dark2")
rolecat <- as.factor(get.vertex.attribute(Bali,"role"))
plot(Bali,vertex.cex=1.5,label=rolelab,
     displaylabels=T,vertex.col=my_pal[rolecat])

# Форма узла

op <- par(mar=c(0,0,0,0))
sidenum <- 3:7
plot(Bali,usearrows=FALSE,vertex.cex=4,
     displaylabels=F,vertex.sides=sidenum[rolecat])
par(op)

# Размер узла

op <- par(mar = c(0,0,2,0),mfrow=c(1,3))
plot(Bali,vertex.cex=0.5,main="Слишком маленький")
plot(Bali,vertex.cex=2,main="В самый раз")
plot(Bali,vertex.cex=6,main="Слишком большой")
par(op)
deg <- degree(Bali,gmode="graph")
deg
cls <- closeness(Bali,gmode="graph")
cls
bet <- betweenness(Bali,gmode="graph")
bet
op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
plot(Bali,usearrows=T,vertex.cex=deg,main="Исходный")
plot(Bali,usearrows=FALSE,vertex.cex=log(deg),
     main="Скорректированный")
par(op)
op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
plot(Bali,usearrows=T,vertex.cex=cls,main="Исходный")
plot(Bali,usearrows=FALSE,vertex.cex=4*cls,
     main="Скорректированный")
par(op)
op <- par(mar = c(0,0,2,1),mfrow=c(1,2))
plot(Bali,usearrows=T,vertex.cex=bet,main="Исходный")
plot(Bali,usearrows=FALSE,vertex.cex=sqrt(bet+1),
     main="Скорректированный")
par(op)
rescale <- function(nchar,low,high) {
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}
plot(Bali,vertex.cex=rescale(deg,1,6),
     main="Размеры узлов, скорректированные
     с помощью функции rescale.")

# Метка узла

get.vertex.attribute(Bali,"vertex.names")
op <- par(mar = c(0,0,0,0))
plot(Bali,displaylabels=TRUE,label.cex=0.8,
     pad=0.4,label.col="darkblue")
par(op)
rolelab <- get.vertex.attribute(Bali,"role")
plot(Bali,usearrows=FALSE,label=rolelab,
     displaylabels=T,label.col="darkblue")

# Ширина ребра

op <- par(mar = c(0,0,0,0))
IClevel <- Bali %e% "IC"
plot(Bali,vertex.cex=1.5,
     edge.lwd=1.5*IClevel)
par(op)

# Цвет ребра

n_edge <- network.edgecount(Bali)
edge_cat <- sample(1:3,n_edge,replace=T)
linecol_pal <- c("blue","red","green")
plot(Bali,vertex.cex=1.5,vertex.col="grey25",
     edge.col=linecol_pal[edge_cat],edge.lwd=2)

# Тип ребра

n_edge <- network.edgecount(Bali)
edge_cat <- sample(1:3,n_edge,replace=T)
line_pal <- c(2,3,4)
gplot(Bali,vertex.cex=0.8,gmode="graph",
      vertex.col="gray50",edge.lwd=1.5,
      edge.lty=line_pal[edge_cat])

# Легенды

my_pal <- brewer.pal(5,"Dark2")
rolecat <- as.factor(get.vertex.attribute(Bali,"role"))
plot(Bali,vertex.cex=rescale(deg,1,5),
     vertex.col=my_pal[rolecat])
legend("bottomleft",legend=c("BM","CT","OA","SB","TL"),
       col=my_pal,pch=19,pt.cex=1.5,bty="n",
       title="Роль террориста")
