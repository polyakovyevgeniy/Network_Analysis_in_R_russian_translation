# Простые интерактивные сети в igraph

library(intergraph)
library(igraph)
data(Bali)
iBali <- asIgraph(Bali)
Coord <- tkplot(iBali, vertex.size=3,
                vertex.label=V(iBali)$role,
                vertex.color="darkgreen")
# Редактируем график в графическом окне Tk, прежде чем
# запустить последующие две команды.
MCoords <- tkplot.getcoords(Coord)
plot(iBali, layout=MCoords, vertex.size=5,
     vertex.label=NA, vertex.color="lightblue")

# Публикация интерактивных веб-диаграмм сетей

library(networkD3)
src <- c("A","A","B","B","C","E")
target <- c("B","C","C","D","B","C")
net_edge <- data.frame(src, target)
simpleNetwork(net_edge)
net_D3 <- simpleNetwork(net_edge)
saveNetwork(net_D3,file = 'Net_test1.html',
            selfcontained=TRUE)
iBali_edge <- get.edgelist(iBali)
iBali_edge <- iBali_edge - 1
iBali_edge <- data.frame(iBali_edge)
iBali_nodes <- data.frame(NodeID=as.numeric(V(iBali)-1),
                          Group=V(iBali)$role,
                          Nodesize=(degree(iBali)))
forceNetwork(Links = iBali_edge, Nodes = iBali_nodes,
             Source = "X1", Target = "X2",
             NodeID = "NodeID",Nodesize = "Nodesize",
             radiusCalculation="Math.sqrt(d.nodesize)*3",
             Group = "Group", opacity = 0.8,
             legend=TRUE)
net_D3 <- forceNetwork(Links = iBali_edge,
                       Nodes = iBali_nodes,
                       Source = "X1", Target = "X2",
                       NodeID = "NodeID",Nodesize = "Nodesize",
                       radiusCalculation="Math.sqrt(d.nodesize)*3",
                       Group = "Group", opacity = 0.8,
                       legend=TRUE)
saveNetwork(net_D3,file = 'Net_test2.html',
            selfcontained=TRUE)
library(visNetwork)
iBali_edge <- get.edgelist(iBali)
iBali_edge <- data.frame(from = iBali_edge[,1],
                         to = iBali_edge[,2])
iBali_nodes <- data.frame(id = as.numeric(V(iBali)))
visNetwork(iBali_nodes, iBali_edge, width = "100%")
iBali_nodes$group <- V(iBali)$role
iBali_nodes$value <- degree(iBali)
net <- visNetwork(iBali_nodes, iBali_edge,
                  width = "100%",legend=TRUE)
visOptions(net,highlightNearest = TRUE)
net <- visNetwork(iBali_nodes, iBali_edge,
                  width = "100%",legend=TRUE)
net <- visOptions(net,highlightNearest = TRUE)
net <- visInteraction(net,navigationButtons = TRUE)
library(htmlwidgets)
saveWidget(net, "Net_test3.html")

# Statnet Web: интерактивный statnet c помощью shiny

library(statnetWeb)
run_sw()

# Дуговые диаграммы

library(devtools)
install_github("gastonstat/arcdiagram")
library(arcdiagram)
library(igraph)
library(intergraph)
data(Simpsons)
iSimp <- asIgraph(Simpsons)
simp_edge <- get.edgelist(iSimp)
arcplot(simp_edge)
s_grp <- V(iSimp)$group
s_col = c("#a6611a", "#dfc27d","#80cdc1","#018571")
cols = s_col[s_grp]
node_deg <- degree(iSimp)
arcplot(simp_edge, lwd.arcs=2, cex.nodes=node_deg/2,
        labels=V(iSimp)$vertex.names,
        col.labels="darkgreen",font=1,
        pch.nodes=21,line=1,col.nodes = cols,
        bg.nodes = cols, show.nodes = TRUE)

# Хордовые диаграммы

library(statnet)
library(circlize)
data(FIFA_Nether)
FIFAm <- as.sociomatrix(FIFA_Nether,attrname='passes')
names <- c("GK1","DF3","DF4","DF5","MF6",
           "FW7","FW9","MF10","FW11","DF2","MF8")
rownames(FIFAm) = names
colnames(FIFAm) = names
FIFAm
FIFAm[FIFAm < 10] <- 0
FIFAm
chordDiagram(FIFAm)
grid.col <- c("#AA3939",rep("#AA6C39",4),
              rep("#2D882D",3),rep("#226666",3))
chordDiagram(FIFAm,directional = TRUE,
             grid.col = grid.col,
             order=c("GK1","DF2","DF3","DF4","DF5",
                     "MF6","MF8","MF10","FW7",
                     "FW9","FW11"))

# Теплокарты для сетевых данных

data(FIFA_Nether)
FIFAm <- as.sociomatrix(FIFA_Nether,attrname='passes')
colnames(FIFAm) <- c("GK1","DF3","DF4","DF5",
                     "MF6","FW7","FW9","MF10",
                     "FW11","DF2","MF8")
rownames(FIFAm) <- c("GK1","DF3","DF4","DF5",
                     "MF6","FW7","FW9","MF10",
                     "FW11","DF2","MF8")
palf <- colorRampPalette(c("#669999", "#003333"))
heatmap(FIFAm[,11:1],Rowv = NA,Colv = NA,col = palf(60),
        scale="none", margins=c(11,11) )
