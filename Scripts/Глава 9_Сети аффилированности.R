# Аффилированность в виде бимодальных сетей

C1 <- c(1,1,1,0,0,0)
C2 <- c(0,1,1,1,0,0)
C3 <- c(0,0,1,1,1,0)
C4 <- c(0,0,0,0,1,1)
aff.df <- data.frame(C1,C2,C3,C4)
row.names(aff.df) <- c("S1","S2","S3","S4","S5","S6")

# Двудольные графы (биграфы)

library(igraph)
bn <- graph.incidence(aff.df)
plt.x <- c(rep(2,6),rep(4,4))
plt.y <- c(7:2,6:3)
lay <- as.matrix(cbind(plt.x,plt.y))
shapes <- c("circle","square")
colors <- c("blue","red")
plot(bn,vertex.color=colors[V(bn)$type+1],
     vertex.shape=shapes[V(bn)$type+1],
     vertex.size=10,vertex.label.degree=-pi/2,
     vertex.label.dist=1.2,vertex.label.cex=0.9,
     layout=lay)

# Создание сетей аффилированности из матриц инцидентности

bn <- graph.incidence(aff.df)
bn
get.incidence(bn)
V(bn)$type
V(bn)$name

# Создание сетей аффилированности из списков ребер

el.df <- data.frame(rbind(c("S1","C1"),
                          c("S2","C1"),
                          c("S2","C2"),
                          c("S3","C1"),
                          c("S3","C2"),
                          c("S3","C3"),
                          c("S4","C2"),
                          c("S4","C3"),
                          c("S5","C3"),
                          c("S5","C4"),
                          c("S6","C4")))
el.df

bn2 <- graph.data.frame(el.df,directed=FALSE)
bn2
V(bn2)$type <- V(bn2)$name %in% el.df[,1]
bn2
graph.density(bn)==graph.density(bn2)

# Графическое представление сетей аффилированности

shapes <- c("circle","square")
colors <- c("blue","red")
plot(bn,vertex.color=colors[V(bn)$type+1],
     vertex.shape=shapes[V(bn)$type+1],
     vertex.size=10,vertex.label.degree=-pi/2,
     vertex.label.dist=1.2,vertex.label.cex=0.9)

# Проекции

bn.pr <- bipartite.projection(bn)
bn.pr
graph.density(bn.pr$proj1)
bn.student <- bn.pr$proj1
bn.class <- bn.pr$proj2
graph.density(bn.student)
get.adjacency(bn.student,sparse=FALSE,attr="weight")
get.adjacency(bn.class,sparse=FALSE,attr="weight")
shapes <- c("circle","square")
colors <- c("blue","red")
op <- par(mfrow=c(1,2))
plot(bn.student,vertex.color="blue",
     vertex.shape="circle",main="Студенты",
     edge.width=E(bn.student)$weight*2,
     vertex.size=15,vertex.label.degree=-pi/2,
     vertex.label.dist=1.2,vertex.label.cex=1)
plot(bn.class,vertex.color="red",
     vertex.shape="square",main="Классы",
     edge.width=E(bn.student)$weight*2,
     vertex.size=15,vertex.label.degree=-pi/2,
     vertex.label.dist=1.2,vertex.label.cex=1)
par(op)

# Анализ полной сети аффилированности актеров Голливуда

data(hwd)
h1 <- hwd
h1
V(h1)$name[1:10]
V(h1)$type[1:10]
V(h1)$IMDBrating[1:10]
V(h1)$name[155:165]
V(h1)$shape <- ifelse(V(h1)$type==TRUE,
                      "square","circle")
V(h1)$shape[1:10]
V(h1)$color <- ifelse(V(h1)$type==TRUE,
                      "red","lightblue")
graph.density(h1)
table(degree(h1,v=V(h1)[type==FALSE]))
mean(degree(h1,v=V(h1)[type==FALSE]))
V(h1)$deg <- degree(h1)
V(h1)[type==FALSE & deg > 4]$name

busy_actor <- data.frame(cbind(
  Actor = V(h1)[type==FALSE & deg > 4]$name,
  Movies = V(h1)[type==FALSE & deg > 4]$deg
))
busy_actor[order(busy_actor$Movies,decreasing=TRUE),]
for (i in 161:1365) {
  V(h1)[i]$totrating <- sum(V(h1)[nei(i)]$IMDBrating)
}
max(V(h1)$totrating,na.rm=TRUE)
pop_actor <- data.frame(cbind(
  Actor = V(h1)[type==FALSE & totrating > 40]$name,
  Popularity = V(h1)[type==FALSE &
                       totrating > 40]$totrating))
pop_actor[order(pop_actor$Popularity,decreasing=TRUE),]
for (i in 161:1365) {
  V(h1)[i]$avgrating <- mean(V(h1)[nei(i)]$IMDBrating)
}
num <- V(h1)[type==FALSE]$deg
avgpop <- V(h1)[type==FALSE]$avgrating
summary(lm(avgpop ~ num))
scatter.smooth(num,avgpop,col="lightblue",
               ylim=c(2,10),span=.8,
               xlab="Занятость актера",
               ylab="Средняя популярность фильма")

# Анализ проекций актеров и фильмов

h1.pr <- bipartite.projection(h1)
h1.act <- h1.pr$proj1
h1.mov <- h1.pr$proj2
h1.act
h1.mov
op <- par(mar = rep(0, 4))
plot(h1.mov,vertex.color="red",
     vertex.shape="circle",
     vertex.size=(V(h1.mov)$IMDBrating)-3,
     vertex.label=NA)
par(op)
graph.density(h1.mov)
no.clusters(h1.mov)
clusters(h1.mov)$csize
table(E(h1.mov)$weight)
h2.mov <- induced.subgraph(h1.mov,
                           vids=clusters(h1.mov)$membership==1)
plot(h2.mov,vertex.color="red",
     edge.width=sqrt(E(h1.mov)$weight),
     vertex.shape="circle",
     vertex.size=(V(h2.mov)$IMDBrating)-3,
     vertex.label=NA)
table(graph.coreness(h2.mov))
h3.mov <- induced.subgraph(h2.mov,
                           vids=graph.coreness(h2.mov)>4)
h3.mov
plot(h3.mov,vertex.color="red",
     vertex.shape="circle",
     edge.width=sqrt(E(h1.mov)$weight),
     vertex.label.cex=0.7,vertex.label.color="darkgreen",
     vertex.label.dist=0.3,
     vertex.size=(V(h3.mov)$IMDBrating)-3)