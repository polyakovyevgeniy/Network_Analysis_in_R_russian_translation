# Клики

library(igraph)
clqexmp <- graph.formula(A:B:C:D--A:B:C:D,D-E,E-F-G-E)
clique.number(clqexmp)
cliques(clqexmp, min=3)
maximal.cliques(clqexmp,min=3)
largest.cliques(clqexmp)
V(clqexmp)[unlist(largest.cliques(clqexmp))]
g25 <- erdos.renyi.game(25, 75, type="gnm")
g50 <- erdos.renyi.game(50, 150, type="gnm")
g100 <- erdos.renyi.game(100, 300, type="gnm")
g500 <- erdos.renyi.game(500, 1500, type="gnm")
nodes <- c(25,50,100,500)
lrgclq <- c(clique.number(g25),clique.number(g50),
            clique.number(g100),clique.number(g500))
numclq <- c(length(cliques(g25,min=3)),
            length(cliques(g50,min=3)),
            length(cliques(g100,min=3)),
            length(cliques(g500,min=3)))
clqinfo <- data.frame(Nodes=nodes,Largest=lrgclq,Number=numclq)
clqinfo

# k-ядра

data(DHHS)
library(intergraph)
iDHHS <- asIgraph(DHHS)
graph.density(iDHHS)
iDHHS <- subgraph.edges(iDHHS,E(iDHHS)[collab > 2])
graph.density(iDHHS)
coreness <- graph.coreness(iDHHS)
table(coreness)
maxCoreness <- max(coreness)
maxCoreness
Vname <- get.vertex.attribute(iDHHS,name='vertex.names',
                              index=V(iDHHS))
V(iDHHS)$name <- Vname
V(iDHHS)$color <- coreness + 1
op <- par(mar = rep(0, 4))
plot(iDHHS,vertex.label.cex=0.8)
par(op)
colors <- rainbow(maxCoreness)
op <- par(mar = rep(0, 4))
plot(iDHHS,vertex.label=coreness,
     vertex.color=colors[coreness])
par(op)
V(iDHHS)$name <- coreness
V(iDHHS)$color <- colors[coreness]
iDHHS1_6 <- iDHHS
iDHHS2_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 1))
iDHHS3_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 2))
iDHHS4_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 3))
iDHHS5_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 4))
iDHHS6_6 <- induced.subgraph(iDHHS,
                             vids=which(coreness > 5))
lay <- layout.fruchterman.reingold(iDHHS)
op <- par(mfrow=c(3,2),mar = c(3,0,2,0))
plot(iDHHS1_6,layout=lay,main="Все k-ядра")
plot(iDHHS2_6,layout=lay[which(coreness > 1),],
     main="k-ядра 2-6")
plot(iDHHS3_6,layout=lay[which(coreness > 2),],
     main="k-ядра 3-6")
plot(iDHHS4_6,layout=lay[which(coreness > 3),],
     main="k-ядра 4-6")
plot(iDHHS5_6,layout=lay[which(coreness > 4),],
     main="k-ядра 5-6")
plot(iDHHS6_6,layout=lay[which(coreness > 5),],
     main="k-ядра 6-6")
par(op)

# Модулярность

g1 <- graph.formula(A-B-C-A,D-E-F-D,G-H-I-G,A-D-G-A)
V(g1)$grp_good <- c(1,1,1,2,2,2,3,3,3)
V(g1)$grp_bad <- c(1,2,3,2,3,1,3,1,2)
op <- par(mfrow=c(1,2))
plot(g1,vertex.color=(V(g1)$grp_good),
     vertex.size=20,
     main="Хорошая группировка")
plot(g1,vertex.color=(V(g1)$grp_bad),
     vertex.size=20,
     main="Плохая группировка")
par(op)
modularity(g1,V(g1)$grp_good)
modularity(g1,V(g1)$grp_bad)
library(intergraph)
data(DHHS)
iDHHS <- asIgraph(DHHS)
table(V(iDHHS)$agency)
V(iDHHS)[1:10]$agency
modularity(iDHHS,(V(iDHHS)$agency+1))
data(Moreno)
iMoreno <- asIgraph(Moreno)
table(V(iMoreno)$gender)
modularity(iMoreno,V(iMoreno)$gender)
data(Facebook)
levels(factor(V(Facebook)$group))
grp_num <- as.numeric(factor(V(Facebook)$group))
modularity(Facebook,grp_num)

# Алгоритмы обнаружения сообществ

cw <- cluster_walktrap(iMoreno)
membership(cw)
modularity(cw)
plot(cw, iMoreno)
cw <- cluster_walktrap(iDHHS)
modularity(cw)
membership(cw)
table(V(iDHHS)$agency,membership(cw))
data(Bali)
iBali <- asIgraph(Bali)
data(Bali)
iBali <- asIgraph(Bali)
cw <- cluster_walktrap(iBali)
modularity(cw)
membership(cw)
ceb <- cluster_edge_betweenness(iBali)
modularity(ceb)
membership(ceb)
cs <- cluster_spinglass(iBali)
modularity(cs)
membership(cs)
cfg <- cluster_fast_greedy(iBali)
modularity(cfg)
membership(cfg)
clp <- cluster_label_prop(iBali)
modularity(clp)
membership(clp)
cle <- cluster_leading_eigen(iBali)
modularity(cle)
membership(cle)
cl <- cluster_louvain(iBali)
modularity(cl)
membership(cl)
co <- cluster_optimal(iBali)
modularity(co)
membership(co)
table(V(iBali)$role,membership(cw))
compare(as.numeric(factor(V(iBali)$role)),cw,
        method="adjusted.rand")
compare(cw,ceb,method="adjusted.rand")
compare(cw,cs,method="adjusted.rand")
compare(cw,cfg,method="adjusted.rand")
op <- par(mfrow=c(3,2),mar=c(3,0,2,0))
plot(ceb, iBali,vertex.label=V(iBali)$role,
     main="Edge Betweenness")
plot(cfg, iBali,vertex.label=V(iBali)$role,
     main="Fastgreedy")
plot(clp, iBali,vertex.label=V(iBali)$role,
     main="Label Propagation")
plot(cle, iBali,vertex.label=V(iBali)$role,
     main="Leading Eigenvector")
plot(cs, iBali,vertex.label=V(iBali)$role,
     main="Spinglass")
plot(cw, iBali,vertex.label=V(iBali)$role,
     main="Walktrap")
par(op)