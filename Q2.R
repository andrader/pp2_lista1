library(dplyr)
library(tidyr)
library(ggplot2)

df = data.frame(
  
  taxa_cresc = c(0.02, 0.27, 0.11, 0.48, 0.15, 0.24, 0.35, 0.04, 0.45, -0.01, 0.14, 0.18, 0.18, 0.58, -0.03, 0.22),
  
  faixa_etaria = c(rep(c(1, 2, 3, 4),times=4)), 
  
  medicamento = c("A", "B", "C", "D", "B", "C", "D", "A", "D", "A", "B", "C", "C", "D", "A", "B")
  
  
)

df$faixa_etaria <- as.factor(df$faixa_etaria)
df$medicamento <- as.factor(df$medicamento)

df


#a

ggplot(df, aes(x=faixa_etaria, y=taxa_cresc, group=medicamento, colour = medicamento)) + 
  geom_line() +
  geom_point() +
  labs(x="Faixa Etária", y="Taxa de Crescimento")


ggplot(df, aes(x=medicamento, y=taxa_cresc, group=faixa_etaria, colour = faixa_etaria)) + 
  geom_line() +
  geom_point() +
  labs(x="Medicamento", y="Taxa de Crescimento")


#c

tapply(df$taxa_cresc,df$faixa_etaria,mean)

d <- df %>% 
  filter(medicamento != 'A')


fit1<-aov(taxa_cresc ~ medicamento*faixa_etaria, data = d)
summary(fit1)


fit2<-aov(taxa_cresc ~ medicamento + faixa_etaria, data = d)
summary(fit2)

par(mfrow=c(2,2))
plot(fit2)

fit3<-aov(taxa_cresc ~ medicamento, data = d)
summary(fit3)


par(mfrow=c(2,2))
plot(fit3)


#c

fit_c <-aov(taxa_cresc ~ medicamento + faixa_etaria, data = d)
summary(fit_c)


levels(d$medicamento)

c1 <- c(.5, -.5, .5, -.5) 
c2 <- c(1, 0, -1, 0) 
c3 <- c(0, 1, 0, -1)

mat <- cbind(c1,c2,c3)

contrasts(d$medicamento) <- mat

model1 <- aov(taxa_cresc ~ medicamento, data = d)

# can just look at the omnibus ANOVA (same as above)
summary(model1)



#d

fit3<-aov(taxa_cresc ~ faixa_etaria/medicamento, data = df)
summary(fit3)


TukeyHSD(fit3, "faixa_etaria")
#f




fit2<-aov(taxa_cresc ~ medicamento + faixa_etaria, data = df)
summary(fit2)


#rascunho


library(agricolae)
LSD.test (fit2, "medicamento", p.adj= "bon", console= TRUE)
HSD.test(fit2, "medicamento", group=TRUE, console=TRUE)

summary(glht(fit2, linfct=Xc))

