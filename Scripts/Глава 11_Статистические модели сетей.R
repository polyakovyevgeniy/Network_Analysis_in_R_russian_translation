# Построение экспоненциальных моделей случайных графов

library(UserNetR)
library(statnet)
data(TCnetworks)
TCcnt <- TCnetworks$TCcnt
TCcoll <- TCnetworks$TCcoll
TCdiss <- TCnetworks$TCdiss
TCdist <- TCnetworks$TCdist
summary(TCdiss,print.adj=FALSE)
components(TCdiss)
gden(TCdiss)
centralization(TCdiss,betweenness,mode='graph')
deg <- degree(TCdiss,gmode='graph')
lvl <- TCdiss %v% 'agency_lvl'
plot(TCdiss,usearrows=FALSE,displaylabels=TRUE,
     vertex.cex=log(deg),
     vertex.col=lvl+1,
     label.pos=3,label.cex=.7,
     edge.lwd=0.5,edge.col="grey75")
legend("bottomleft",legend=c("Местные","На уровне штата","Федеральные"),
       col=2:4,pch=19,pt.cex=1.5)

# Построение нулевой модели

library(ergm)
DSmod0 <- ergm(TCdiss ~ edges,
               control=control.ergm(seed=40))
class(DSmod0)
summary(DSmod0)
plogis(coef(DSmod0))

# Включение предикторов узлов

scatter.smooth(TCdiss %v% 'tob_yrs',
               degree(TCdiss,gmode='graph'),
               xlab='Опыт участия в годах',
               ylab='Степень')
DSmod1 <- ergm(TCdiss ~ edges +
                 nodefactor('lead_agency') +
                 nodecov('tob_yrs') ,
               control=control.ergm(seed=40))
summary(DSmod1)
p_edg <- coef(DSmod1)[1]
p_yrs <- coef(DSmod1)[3]
plogis(p_edg + 5*p_yrs + 10*p_yrs)

# Включение предикторов диад

mixingmatrix(TCdiss,'agency_lvl')
mixingmatrix(TCdiss,'agency_cat')
DSmod2a <- ergm(TCdiss ~ edges +
                  nodecov('tob_yrs') +
                  nodematch('agency_lvl'),
                control=control.ergm(seed=40))
summary(DSmod2a)
DSmod2b <- ergm(TCdiss ~ edges +
                  nodecov('tob_yrs') +
                  nodematch('agency_lvl',diff=TRUE),
                control=control.ergm(seed=40))
summary(DSmod2b)
DSmod2c <- ergm(TCdiss ~ edges +
                  nodecov('tob_yrs') +
                  nodemix('agency_lvl',base=1),
                control=control.ergm(seed=40))
summary(DSmod2c)

# Включение предикторов ребер

as.sociomatrix(TCdist,attrname = 'distance')[1:5,1:5]
as.sociomatrix(TCcnt,attrname = 'contact')[1:5,1:5]
DSmod3 <- ergm(TCdiss ~ edges +
                 nodecov('tob_yrs') +
                 nodematch('agency_lvl',diff=TRUE) +
                 edgecov(TCdist,attr='distance') +
                 edgecov(TCcnt,attr='contact'),
               control=control.ergm(seed=40))
summary(DSmod3)

# Включение предикторов локальных структур

DSmod4 <- ergm(TCdiss ~ edges +
                 nodecov('tob_yrs') +
                 nodematch('agency_lvl',diff=TRUE) +
                 edgecov(TCdist,attr='distance') +
                 edgecov(TCcnt,attr="contact") +
                 gwesp(0.7, fixed=TRUE),
               control=control.ergm(seed=40))
summary(DSmod4)

# Интерпретация модели

prd_prob1 <- plogis(-6.31 + 2*1*.099 + 1.52 +
                      4*1.042 + .858*(.50^4))
prd_prob1
prd_prob2 <- plogis(-6.31 + 2*5*.099 +
                      1*1.042 + .858*(.50^4))
prd_prob2

# Подгонка модели

DSmod.fit <- gof(DSmod4,
                 GOF = ~distance + espartners +
                   degree + triadcensus,
                 burnin=1e+5, interval = 1e+5)
summary(DSmod.fit)
op <- par(mfrow=c(2,2))
plot(DSmod.fit,cex.axis=1.6,cex.label=1.6)
par(op)

# Диагностика модели

mcmc.diagnostics(DSmod4)

# Имитационное моделирование сетей на основе оцененной модели

sim4 <- simulate(DSmod4, nsim=1, seed=569)
summary(sim4,print.adj=FALSE)
op <- par(mfrow=c(1,2),mar=c(0,0,2,0))
lvlobs <- TCdiss %v% 'agency_lvl'
plot(TCdiss,usearrows=FALSE,
     vertex.col=lvl+1,
     edge.lwd=0.5,edge.col="grey75",
     main="Реальная сеть")
lvl4 <- sim4 %v% 'agency_lvl'
plot(sim4,usearrows=FALSE,
     vertex.col=lvl4+1,
     edge.lwd=0.5,edge.col="grey75",
     main="Имитированная сеть - модель 4")
par(op)