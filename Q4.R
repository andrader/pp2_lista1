df = data.frame(
  
  taxa_cresc = c(0.02, 0.27, 0.11, 0.48, 0.15, 0.24, 0.35, 0.04, 0.45, -0.01, 0.14, 0.18, 0.18, 0.58, -0.03, 0.22),
  
  faixa_etaria = c(rep(c(1, 2, 3, 4),times=4)), 
  
  medicamento = c("A", "B", "C", "D", "B", "C", "D", "A", "D", "A", "B", "C", "C", "D", "A", "B")
  
  
)

df$faixa_etaria <- as.factor(df$faixa_etaria)
df$medicamento <- as.factor(df$medicamento)



library(lmPerm)

anova(lmp(taxa_cresc ~ medicamento + faixa_etaria, data = df))
