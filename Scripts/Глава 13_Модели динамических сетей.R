# Создание сети для имитационного моделирования

library(igraph)
N <- 25
netdum <- erdos.renyi.game(N, p=0.10)
graph.density(netdum)
mean(degree(netdum))
Bh <- runif(N,0,1)
BhCat <- cut(Bh, breaks=5, labels = FALSE)
V(netdum)$Bh <- Bh
V(netdum)$BhCat <- BhCat
table(V(netdum)$BhCat)
library(RColorBrewer)
my_pal <- brewer.pal(5, "PiYG")
V(netdum)$color <- my_pal[V(netdum)$BhCat]
crd_save <- layout.auto(netdum)
plot(netdum, layout = crd_save)

# Создание функции изменения

g <- netdum
get.adjlist(g)[24]
g[[24,]]
V(g)[24]$Bh
V_adj <- unlist(get.adjlist(g)[24])
V(g)[V_adj]$Bh
BhDiff <- abs(V(g)[V_adj]$Bh - V(g)[24]$Bh)
BhDiff
gdum <- g
gdum[24,9] <- FALSE
get.adjlist(gdum)[24]
gdum <- g
V_sel <- V_adj[BhDiff == max(BhDiff)]
gdum[24,V_sel] <- FALSE
get.adjlist(gdum)[24]
plot(gdum, layout = crd_save)
gdum <- g
V_sel <- sample(V_adj,1,prob=BhDiff)
gdum[24,V_sel] <- FALSE
get.adjlist(gdum)[24]
smplCheck <- sample(V_adj,500,replace=TRUE,prob=BhDiff)
table(smplCheck)
vtx <- 24
nodes <- 1:vcount(g)
V_nonadj <- nodes[-c(vtx,V_adj)]
V_nonadj
BhDiff2 <- 1-abs(V(g)[V_nonadj]$Bh - V(g)[vtx]$Bh)
BhDiff2
Sel_V <- sample(V_nonadj,1,prob=BhDiff2)
gnew <- g
gnew[vtx,Sel_V] <- TRUE
get.adjlist(gnew)[vtx]
E(gnew)$color <- "grey"
E(gnew, P = c(vtx, Sel_V))$color <- "darkred"
plot(gnew, layout = crd_save)
Sel_update <- function(g,vtx){
  V_adj <- neighbors(g,vtx)
  if(length(V_adj)==0) return(g)
  BhDiff1 <- abs(V(g)[V_adj]$Bh - V(g)[vtx]$Bh)
  Sel_V <- sample(V_adj,1,prob=BhDiff1)
  g[vtx,Sel_V] <- FALSE
  nodes <- 1:vcount(g)
  V_nonadj <- nodes[-c(vtx,V_adj)]
  BhDiff2 <- 1-abs(V(g)[V_nonadj]$Bh - V(g)[vtx]$Bh)
  Sel_V <- sample(V_nonadj,1,prob=BhDiff2)
  g[vtx,Sel_V] <- TRUE
  g
}
gtst <- g
node <- 24
gnew <- Sel_update(g,node)
neighbors(gtst,node)
neighbors(gnew,node)

# Построение простой имитационной модели социальной селекции

Sel_sim <- function(g,upd){
  g_lst <- lapply(1:(upd+1), function(i) i)
  g_lst[[1]] <- g
  for (i in 1:upd) {
    gnew <- g_lst[[i]]
    node <- sample(1:vcount(g),1)
    gupd <- Sel_update(gnew,node)
    g_lst[[i+1]] <- gupd
  }
  g_lst
}
N <- 100
netdum <- erdos.renyi.game(N, p=0.10)
graph.density(netdum)
mean(degree(netdum))
Bh <- runif(N,0,1)
BhCat <- cut(Bh, breaks=5, labels = FALSE)
V(netdum)$Bh <- Bh
V(netdum)$BhCat <- BhCat
table(V(netdum)$BhCat)
set.seed(999)
g_lst <- Sel_sim(netdum,500)
length(g_lst)
summary(g_lst[[1]])

# Интерпретация результатов имитационного моделирования

graph.density(g_lst[[1]])
graph.density(g_lst[[501]])
neighbors(g_lst[[1]],1)
neighbors(g_lst[[501]],1)
modularity(g_lst[[1]],BhCat)
modularity(g_lst[[501]],BhCat)
sim_stat <- unlist(lapply(g_lst, function(u)
  modularity(u,V(u)$BhCat)))
op <- par(mfrow=(c(1,2)))
plot(density(sim_stat),main="",xlab="Modularity")
plot(0:500,sim_stat,type="l",
     xlab="Simulation Step",ylab="Modularity")
par(op)

# Имитационное моделирование социального влияния

N <- 25
netdum <- erdos.renyi.game(N, p=0.10)
Bh <- runif(N,0,1)
BhCat <- cut(Bh, breaks=5, labels = FALSE)
V(netdum)$Bh <- Bh
V(netdum)$BhCat <- BhCat
V(netdum)$Tl <- 0.20
V(netdum)$Tl[1:10]

# Создание функции изменения

g <- netdum
V_adj <- neighbors(g,24)
V_adj
V(g)[24]$Bh
V(g)[V_adj]$Bh
newval <- .5*(V(g)$Bh[24] + mean(V(g)[V_adj]$Bh))
newval
V_Bh <- V(g)[24]$Bh
V_Bh
N_Bh <- V(g)[V_adj]$Bh
N_Bh
N_Bh[abs(N_Bh-V_Bh) < .20]
newval2 <- .5*(V(g)$Bh[24] +
                 mean(N_Bh[abs(N_Bh-V_Bh) < .20]))
newval2
Inf_update <- function(g,vtx){
  TL <- V(g)[vtx]$Tl
  V_adj <- neighbors(g,vtx)
  V_Bh <- V(g)[vtx]$Bh
  N_Bh <- V(g)[V_adj]$Bh
  ifelse(length(N_Bh[abs(N_Bh-V_Bh)<TL]) > 0,
         new_Bh <- .5*(V_Bh +
                         mean(N_Bh[abs(N_Bh-V_Bh) < TL])),
         new_Bh <- V_Bh
  )
  new_Bh
}
newval3 <- Inf_update(g,24)
newval3

# Построение имитационной модели социального влияния

Inf_sim <- function(g,runs){
  g_lst <- lapply(1:(runs+1), function(i) i)
  g_lst[[1]] <- g
  for (i in 1:runs) {
    gnew <- g_lst[[i]]
    for (j in 1:length(V(g))) {
      V(gnew)[j]$Bh <- Inf_update(g=gnew,vtx=j)
    }
    g_lst[[i+1]] <- gnew
  }
  g_lst
} 
N <- 100
netdum <- erdos.renyi.game(N, p=0.10)
Bh <- runif(N,0,1)
V(netdum)$Tl <- .20
V(netdum)$Bh <- Bh
V(netdum)$BhCat <- BhCat
set.seed(999)
g_lst <- Inf_sim(netdum,50)

# Интерпретация результатов имитационного моделирования

op <- par(mfrow=(c(3,2)))
plot(density(V(g_lst[[1]])$Bh),xlim=c(-.2,1.2),
     main="Первоначальная сеть")
plot(density(V(g_lst[[6]])$Bh),xlim=c(-.2,1.2),
     main='После 5 прогонов')
plot(density(V(g_lst[[11]])$Bh),xlim=c(-.2,1.2),
     main='После 10 прогонов')
plot(density(V(g_lst[[16]])$Bh),xlim=c(-.2,1.2),
     main='После 15 прогонов')
plot(density(V(g_lst[[26]])$Bh),xlim=c(-.2,1.2),
     main='После 25 прогонов')
plot(density(V(g_lst[[51]])$Bh),xlim=c(-.2,1.2),
     main='После 50 прогонов')
par(op)
V(g_lst[[1]])$BhCat <- cut(V(g_lst[[1]])$Bh,
                           breaks=c(0,.2,.4,.6,.8,1), labels = FALSE)
V(g_lst[[26]])$BhCat <- cut(V(g_lst[[26]])$Bh,
                            breaks=c(0,.2,.4,.6,.8,1), labels = FALSE)
V(g_lst[[51]])$BhCat <- cut(V(g_lst[[51]])$Bh,
                            breaks=c(0,.2,.4,.6,.8,1), labels = FALSE)
V(g_lst[[1]])$color <- my_pal[V(g_lst[[1]])$BhCat]
V(g_lst[[26]])$color <- my_pal[V(g_lst[[26]])$BhCat]
op <- par(mfrow=c(1,2),mar=c(0,0,2,0))
plot(g_lst[[1]],vertex.label=NA,
     main="Первоначальная сеть")
plot(g_lst[[26]],vertex.label=NA,
     main="Сеть после 25-го прогона")
par(op)
