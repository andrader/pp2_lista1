##MAE327-2020-Lista01
##Q1
library(MANOVA.RM)
dat.long<-o2cons
dat.long
names(dat.long)
names(dat.long)<-c("O2","Staph","Time","Grup","Subj")
dat.long

library(reshape2)
dat.wide <- dcast(dat.long, Subj + Grup + Staph~ Time, value.var="O2")
dat.wide

dat<-dat.wide
attach(dat)
names(dat)
dat<-cbind(dat,c(rep(1:2,12),rep(3:4,12)))
names(dat)<-c("Subj","Trat","Virus","T6","T12","T18","Grup")
str(dat)
dat
dat<-dat[,2:7]
attach(dat)
dat

#write.csv(dat, file="C://Users//jpsol//OneDrive//Documents//Julia//MAE327//Aula3-DABC//O2cons.csv", row.names=F, col.names=T)

#write.table(dat, file="C://Users//jpsol//OneDrive//Documents//Julia//MAE327//Aula3-DABC//O2cons",sep="\t", row.names=F, col.names=T)


##Q1
o2<-read.table("C://Users//jpsol//OneDrive//Documents//Julia//MAE327//Aula3-DABC//O2cons.txt", header = TRUE)
o2
attach(o2)
tapply(T6,Grup,mean)
tapply(T12,Grup,mean)
tapply(T18,Grup,mean)
table(Grup)

fit6<-aov(T6~Trat*factor(Virus))
summary(fit6)
par(mfrow=c(2,2))
plot(fit6)
with(o2, interaction.plot(Trat, Virus, T6,main="Gráfico de interação"))
#Gráfico de Perfis de Médias dos fatores

fit12<-aov(T12~Trat*factor(Virus))
summary(fit12)
par(mfrow=c(2,2))
plot(fit12)
with(dat, interaction.plot(Trat, Virus, T12,main="Gráfico de interação"))

fit18<-aov(T18~Trat*factor(Virus))
summary(fit18)
par(mfrow=c(2,2))
plot(fit18)
with(dat, interaction.plot(Trat, Virus, T18,main="Gráfico de interação"))

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

with(data.frame(datSim), interaction.plot(factor(f1),factor(f2), TSim,main="Gráfico de interação"))
tapply(TSim,f12,mean)

fitSim<-aov(TSim~factor(f1)*factor(f2))
#Para fatorial 2x2 é equivalente usar "aov(TSim~factor(f1)*factor(f2))"
summary(fitSim)
fitSim$coefficients
par(mfrow=c(2,2))
plot(fitSim)


##Q2
resp<-c(0.02,-0.01,-0.03,0.04,0.15,0.27,0.14,0.22,0.18,0.24,0.11,0.18,0.45,0.58,0.35,0.48)
resp
x<-rep(rep(c(0,1),each=4),2)
y<-rep(c(0,1),each=8)
fx<-rep(c(1,2,3,4),4)
trat<-rep(c(1,2,3,4),each=4)
dados<-cbind(x,y,fx,trat,resp)
dados
str(dados)
attach(dados)
fitxy<-aov(resp~factor(fx)+x*y)
summary(fitxy)

#Efeito de interação entre Tratamento e Fx (Bloco) 
fitxy$coefficients
with(data.frame(dados), interaction.plot(factor(trat),factor(fx),resp,main="Gráfico de interação"))
fxi<-rep(c(0,fitxy$coefficients[2:4]),4)
fxi
xyi<-rep(c(0,fitxy$coefficients[5:7]),each=4)
xyi
fxyi<-fxi*xyi
fxyi
fititb<-aov(resp~factor(fx)+x*y+fxyi)
summary(fititb)
#Não há efeito significante de interação entre Tratamentos e Fx (Bloco)

#Gráfico de Perfis de Médias de Tratamentos (X e Y)
with(data.frame(dados), interaction.plot(factor(x),factor(y),resp,main="Gráfico de interação"))
#Como o modelo é aditivo (entre Trat e Fx) o gráfico de interação é o mesmo com
#ou sem o ajuste por FX
#Contudo, somente no modelo ajustado por Fx é que o efeito de interação é significante
fitxy$coefficients
respi<-model.matrix(fitxy)%*%fitxy$coefficients[1:7]
respi
dadosi<-cbind(dados[,1],dados[,2],respi)
dadosi
with(data.frame(dadosi), interaction.plot(factor(dadosi[,1]),factor(dadosi[,2]),respi,main="Gráfico de interação ajustado"))
par(mfrow=c(1,2))
with(data.frame(dados), interaction.plot(factor(x),factor(y),resp,main="Gráfico de interação"))
with(data.frame(dadosi), interaction.plot(factor(dadosi[,1]),factor(dadosi[,2]),respi,main="Gráfico de interação ajustado"))
plot(resp,respi)
cbind(resp,respi)
tapply(resp,trat,mean)
tapply(respi,trat,mean)
