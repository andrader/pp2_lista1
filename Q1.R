##MAE327-2020-Lista01
##Q1

library(MANOVA.RM)
dat.long<-o2cons
#dat.long
names(dat.long)
names(dat.long)<-c("O2","Staph","Time","Grup","Subj")
#dat.long

library(reshape2)
dat.wide <- dcast(dat.long, Subj + Grup + Staph~ Time, value.var="O2")
#dat.wide

dat<-dat.wide
attach(dat)
names(dat)
dat<-cbind(dat,c(rep(1:2,12),rep(3:4,12)))
names(dat)<-c("Subj","Trat","Virus","T6","T12","T18","Grup")
dat<-dat[,2:7]
attach(dat)
str(dat)
dat[1:10,]
dat[38:48,]

tapply(T6,Grup,mean)
tapply(T12,Grup,mean)
tapply(T18,Grup,mean)
table(Grup)

# fazer para t6 t12 e t18
fit6<-aov(T6~Trat*factor(Virus))
summary(fit6)
interaction.plot(Trat, Virus, T6,main="Interação para o Tratamento vs Virus (resposta T6)")
pg <- par()
par(mfrow=c(2,2))
plot(fit6)
par(pg)


#Simulando dados
#Experimento DCA Fatorial 2x2 com efeito de interação
#Modelo estrutural: y=mi+tau1+tau2+tau12+e
#set.seed()
mi<- 4
tau1.1<- (-1.8)
tau2.1<- (-2)
tau12.11<- 6
sigma<-1
mi00<-mi
mi01<-mi+tau2.1
mi10<-mi+tau1.1
mi11<-mi+tau1.1+tau2.1+tau12.11
r<-12
#Ef Interacao Nulo: mi00-mi01 = mi10-mi11
y.T00<-rnorm(r,mi00,sigma)
y.T01<-rnorm(r,mi01,sigma)
y.T10<-rnorm(r,mi10,sigma)
y.T11<-rnorm(r,mi11,sigma)
TSim<-c(round(y.T00,2),round(y.T01,2),round(y.T10,2),round(y.T11,2))
TSim
f1<-rep(c(0,1),each=2*r)
f1
f2<-rep(rep(c(0,1),each=r),2)
f2
f12<-rep(c(1,2,3,4),each=r)
f12
datSim<-cbind(f1,f2,TSim,f12)
datSim<-data.frame(datSim)
datSim
attach(datSim)
str(datSim)


tapply(TSim,f12,mean)


fitSim<-aov(TSim~factor(f1)*factor(f2))
#Para fatorial 2x2 é equivalente usar "aov(TSim~factor(f1)*factor(f2))"
summary(fitSim)
fitSim$coefficients

with(data.frame(datSim), interaction.plot(factor(f1),factor(f2), TSim,main="Gráfico de interação"))

par(mfrow=c(2,2))
plot(fitSim)



