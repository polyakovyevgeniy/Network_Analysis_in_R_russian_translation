# Подготовка данных

library(igraph)
library(UserNetR)
data(Coevolve)
fr_w1 <- Coevolve$fr_w1
fr_w2 <- Coevolve$fr_w2
fr_w3 <- Coevolve$fr_w3
fr_w4 <- Coevolve$fr_w4
colors <- c("darkgreen","SkyBlue2")
shapes <- c("circle","square")
coord <- layout.kamada.kawai(fr_w1)
op <- par(mfrow=c(2,2),mar=c(1,1,2,1))
plot(fr_w1,vertex.color=colors[V(fr_w1)$smoke+1],
     vertex.shape=shapes[V(fr_w1)$gender],
     vertex.size=10,main="Волна 1",vertex.label=NA,
     edge.arrow.size=0.5,layout=coord)
plot(fr_w2,vertex.color=colors[V(fr_w2)$smoke+1],
     vertex.shape=shapes[V(fr_w2)$gender],
     vertex.size=10,main="Волна 2",vertex.label=NA,
     edge.arrow.size=0.5,layout=coord)
plot(fr_w3,vertex.color=colors[V(fr_w3)$smoke+1],
     vertex.shape=shapes[V(fr_w3)$gender],
     vertex.size=10,main="Волна 3",vertex.label=NA,
     edge.arrow.size=0.5,layout=coord)
plot(fr_w4,vertex.color=colors[V(fr_w4)$smoke+1],
     vertex.shape=shapes[V(fr_w4)$gender],
     vertex.size=10,main="Волна 4",vertex.label=NA,
     edge.arrow.size=0.5,layout=coord)
par(op)
library(RSienaTest)
matw1 <- as.matrix(get.adjacency(fr_w1))
matw2 <- as.matrix(get.adjacency(fr_w2))
matw3 <- as.matrix(get.adjacency(fr_w3))
matw4 <- as.matrix(get.adjacency(fr_w4))
matw1[1:8,1:8]
fr4wav<-sienaDependent(array(c(matw1,matw2,matw3,matw4),
                             dim=c(37,37,4)),sparse=FALSE)
class(fr4wav)
fr4wav
library(Matrix)
w1 <- cbind(get.edgelist(fr_w1), 1)
w2 <- cbind(get.edgelist(fr_w2), 1)
w3 <- cbind(get.edgelist(fr_w3), 1)
w4 <- cbind(get.edgelist(fr_w4), 1)
w1s <- spMatrix(37, 37, w1[,1], w1[,2], w1[,3])
w2s <- spMatrix(37, 37, w2[,1], w2[,2], w2[,3])
w3s <- spMatrix(37, 37, w3[,1], w3[,2], w3[,3])
w4s <- spMatrix(37, 37, w4[,1], w4[,2], w4[,3])
fr4wav2 <- sienaDependent(list(w1s,w2s,w3s,w4s))
fr4wav2
gender_vect <- V(fr_w1)$gender
table(gender_vect)
gender <- coCovar(gender_vect)
gender
smoke <- array(c(V(fr_w1)$smoke,V(fr_w2)$smoke,
                 V(fr_w3)$smoke,V(fr_w4)$smoke),dim=c(37,4))
smokebeh <- sienaDependent(smoke,type = "behavior")
smokebeh
friend <- sienaDataCreate(fr4wav,smokebeh,gender)
friend
print01Report(friend,modelname = 'Coevolve Example' )

# Спецификация модели

frndeff <- getEffects( friend )
frndeff
effectsDocumentation(frndeff)
frndeff <- getEffects( friend )
frndeff <- includeEffects(frndeff,sameX,
                          interaction1="gender",name="fr4wav")
frndeff <- includeEffects(frndeff,egoX,
                          interaction1="smokebeh",name="fr4wav")
frndeff <- includeEffects(frndeff,altX,
                          interaction1="smokebeh",name="fr4wav")
frndeff <- includeEffects(frndeff,sameX,
                          interaction1="smokebeh",name="fr4wav")
frndeff <- includeEffects(frndeff,avSim,
                          interaction1="fr4wav",name="smokebeh")
frndeff <- includeEffects(frndeff,totSim,
                          interaction1="fr4wav",name="smokebeh")
frndeff <- includeEffects(frndeff,recip,transTrip,
                          name="fr4wav")
frndeff

# Оценивание модели

myalgorithm <- sienaAlgorithmCreate(projname='coevolve')
set.seed(999)
RSmod1 <- siena07( myalgorithm, data = friend,
                   effects = frndeff,batch=TRUE,
                   verbose=FALSE,useCluster=TRUE,
                   initC=TRUE,nbrNodes=3)

# Интерпретация модели

summary(RSmod1)
frndeff2 <- includeEffects(frndeff,totSim,
                           interaction1="fr4wav",
                           name="smokebeh",
                           include=FALSE)
frndeff2 <- includeEffects(frndeff2,transTrip,
                           name="fr4wav",
                           include=FALSE)
frndeff2
myalgorithm <- sienaAlgorithmCreate(projname='coevol2')
set.seed(999)
RSmod2 <- siena07(myalgorithm,data = friend,
                  effects = frndeff2,
                  prevAns=RSmod1,batch=TRUE,
                  verbose=FALSE,useCluster=TRUE,
                  initC=TRUE,nbrNodes=3,
                  returnDeps=TRUE)
summary(RSmod2)

# Качество подгонки

table(degree(fr_w4,mode="in"))
gofi <- sienaGOF(RSmod2, IndegreeDistribution,
                 levls=1:10,verbose=FALSE, join=TRUE,
                 varName="fr4wav")
plot(gofi)
TriadCensus <- function(i,data,sims,wave,
                        groupName,varName,levls=1:16){
  unloadNamespace("igraph") # чтобы избежать конфликта пакетов
  require(sna)
  require(network)
  x <- networkExtraction(i,data,sims,wave,
                         groupName,varName)
  if (network.edgecount(x) <= 0){x <- symmetrize(x)}
  # иначе вызов triad.census(x) вернет ошибку
  tc <- sna::triad.census(x)[1,levls]
  # имена передаются автоматически
  tc
}
goftc <- sienaGOF(RSmod2, TriadCensus,
                  varName="fr4wav",
                  verbose=FALSE, join=TRUE)
descriptives.sienaGOF(goftc)
plot(goftc, center=TRUE, scale=TRUE)

# Имитационное моделирование

str(RSmod2$sims[[500]])
RSmod2$sims[[500]][[1]][[1]][[3]][1:25,]
RSmod2$sims[[500]][[1]][[2]][[3]]
library(igraph)
el <- RSmod2$sims[[500]][[1]][[1]][[3]]
sb <- RSmod2$sims[[500]][[1]][[2]][[3]]
fr_w4_sim <- graph.data.frame(el,directed = TRUE)
V(fr_w4_sim)$smoke <- sb
V(fr_w4_sim)$gender <- V(fr_w4)$gender
fr_w4_sim
modularity(fr_w4_sim,membership = V(fr_w4_sim)$smoke+1)
modularity(fr_w4,membership = V(fr_w4)$smoke+1)
colors <- c("darkgreen","SkyBlue2")
coord <- layout.kamada.kawai(fr_w4)
op <- par(mfrow=c(1,2),mar=c(1,1,2,1))
plot(fr_w4,vertex.color=colors[V(fr_w4)$smoke+1],
     vertex.shape=shapes[V(fr_w4)$gender],
     vertex.size=10,main="Реальная сеть - Волна 4",
     vertex.label=NA,
     edge.arrow.size=0.5,layout=coord)
plot(fr_w4_sim,
     vertex.color=colors[V(fr_w4_sim)$smoke+1],
     vertex.shape=shapes[V(fr_w4_sim)$gender],
     vertex.size=10,main="Имитированная сеть - Волна 4",
     vertex.label=NA,
     edge.arrow.size=0.5,layout=coord)
par(op)