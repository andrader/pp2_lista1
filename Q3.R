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



library(lmPerm)

anova(lmp(T6~Trat*factor(Virus), data = dat))

anova(lmp(T12~Trat*factor(Virus), data = dat))

anova(lmp(T18~Trat*factor(Virus), data = dat))







