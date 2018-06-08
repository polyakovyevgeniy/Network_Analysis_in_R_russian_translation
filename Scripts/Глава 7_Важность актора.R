# Центральность по степени

library(statnet)
net_mat <- rbind(c(0,1,1,0,0,0,0,0,0,0),
                 c(1,0,1,0,0,0,0,0,0,0),
                 c(1,1,0,1,1,0,1,0,0,0),
                 c(0,0,1,0,1,0,0,0,0,0),
                 c(0,0,1,1,0,1,0,0,0,0),
                 c(0,0,0,0,1,0,1,0,0,0),
                 c(0,0,1,0,0,1,0,1,0,0),
                 c(0,0,0,0,0,0,1,0,1,1),
                 c(0,0,0,0,0,0,0,1,0,0),
                 c(0,0,0,0,0,0,0,1,0,0))
rownames(net_mat) <- c("a","b","c","d","e","f","g","h","i","j")
colnames(net_mat) <- c("a","b","c","d","e","f","g","h","i","j")
net <- network(net_mat,matrix.type="adjacency")
net %v% 'vertex.names'
degree(net, gmode="graph")

# Центральность по близости

closeness(net, gmode="graph")

# Центральность по посредничеству

betweenness(net, gmode="graph")

# Меры центральности в R

library(UserNetR)
data(DHHS)
df.prom <- data.frame(
  deg = degree(DHHS),
  cls = closeness(DHHS),
  btw = betweenness(DHHS),
  evc = evcent(DHHS),
  inf = infocent(DHHS),
  flb = flowbet(DHHS)
)
cor(df.prom)

# Централизация или вычисление индексов центральности для сети в целом

library(RColorBrewer)
dum1 <- rbind(c(1,2),c(1,3),c(1,4),c(1,5))
star_net <- network(dum1,directed=FALSE)
dum2 <- rbind(c(1,2),c(2,3),c(3,4),c(4,5),c(5,1))
circle_net <- network(dum2,directed=FALSE)
par(mar=c(4,4,.1,.1))
my_pal <- brewer.pal(5,"Set2")
gplot(star_net,usearrows=FALSE,displaylabels=FALSE,
      vertex.cex=2,
      vertex.col=my_pal[1],
      edge.lwd=0,edge.col="grey50",xlab="Звездчатый граф")
gplot(circle_net,usearrows=FALSE,displaylabels=FALSE,
      vertex.cex=2,
      vertex.col=my_pal[3],
      edge.lwd=0,edge.col="grey50",xlab="Круговой граф")
closeness(circle_net)
centralization(circle_net,closeness)
closeness(star_net)
centralization(star_net,closeness)

# Создание отчетов по центральности

data(Bali)
str(degree(Bali))
summary(degree(Bali))
data(Bali)
my_pal <- brewer.pal(5,"Set2")
rolecat <- Bali %v% "role"
gplot(Bali,usearrows=FALSE,displaylabels=TRUE,
      vertex.col=my_pal[as.factor(rolecat)],
      edge.lwd=0,edge.col="grey25")
legend("topright",legend=c("BM","CT","OA","SB",
                           "TL"),col=my_pal,pch=19,pt.cex=2)

data(Bali)
df.prom2 <- data.frame(
  degree = degree(Bali),
  closeness = closeness(Bali),
  betweenness = betweenness(Bali)
)
row.names(df.prom2) <- Bali %v% "vertex.names"
df.promsort <- df.prom2[order(-df.prom2$degree),]
cd <- centralization(Bali,degree)
cc <- centralization(Bali,closeness)
cb <- centralization(Bali,betweenness)
df.promsort <- rbind(df.promsort,c(cd,cc,cb))
row.names(df.promsort)[18] <- "\\emph{Centralization}"
deg <- degree(Bali,rescale=TRUE)
op <- par(mfrow=c(1,2))
gplot(Bali,usearrows=FALSE,displaylabels=FALSE,
      vertex.cex=deg,
      vertex.col=my_pal[as.factor(rolecat)],
      edge.lwd=0,edge.col="grey25",
      main="Слишком маленькие")
gplot(Bali,usearrows=FALSE,displaylabels=FALSE,
      vertex.cex=deg*20,
      vertex.col=my_pal[as.factor(rolecat)],
      edge.lwd=0,edge.col="grey25",
      main="Немного лучше")
par(op)
deg <- degree(Bali,rescale=TRUE)
gplot(Bali,usearrows=FALSE,displaylabels=TRUE,
      vertex.cex=deg*12,
      vertex.col=my_pal[as.factor(rolecat)],
      edge.lwd=0.5,edge.col="grey75")
legend("topright",legend=c("BM","CT","OA","SB","TL"),
       col=my_pal,pch=19,pt.cex=2)

# Точки сочленения и мосты

cpnet <- cutpoints(net,mode="graph",
                   return.indicator=TRUE)
mycoord <- gplot(net,gmode="graph",
                 vertex.cex=1.5)
gplot(net,gmode="graph",vertex.col=cpnet+2,
      coord=mycoord,
      jitter=FALSE,displaylabels=TRUE)

net2 <- net
components(net2)
delete.vertices(net2,7)
components(net2)
gplot(net2,gmode="graph",vertex.col=2,
      coord=mycoord [-7,],
      jitter=FALSE,displaylabels=TRUE)

bridges <- function(dat,mode="graph",
                    connected=c("strong", "weak")) {
  e_cnt <- network.edgecount(dat)
  if (mode == "graph") {
    cmp_cnt <- components(dat)
    b_vec <- rep(FALSE,e_cnt)
    for(i in 1:e_cnt){
      dat2 <- dat
      delete.edges(dat2,i)
      b_vec[i] <- (components(dat2) != cmp_cnt)
    }
  }
  else {
    cmp_cnt <- components(dat,connected=connected)
    b_vec <- rep(FALSE,e_cnt)
    for(i in 1:e_cnt){
      dat2 <- dat
      delete.edges(dat2,i)
      b_vec[i] <- (components(dat2,connected=connected)
                   != cmp_cnt)
    }
  }
  return(b_vec)
}
bridges(net)
brnet <- bridges(net)
gplot(net,gmode="graph",vertex.col="red",
      edge.col=brnet+2, coord=mycoord,
      jitter=FALSE,displaylabels=TRUE)