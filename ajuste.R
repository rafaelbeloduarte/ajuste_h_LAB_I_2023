library(tikzDevice)

setwd("~/DriveUEMEncrypt/casa/uem/Doutorado/Disciplinas/Estágio/Aula_Coeficiente_de_transferência_de_calor_em_corpos_submersos/ajuste_h_LAB_I_2023")

data <- read.csv("psi_vs_eta.csv", stringsAsFactors=TRUE)

#tikz("~/DriveUEMEncrypt/casa/uem/Doutorado/Disciplinas/Estágio/Aula_Coeficiente_de_transferência_de_calor_em_corpos_submersos/figures/psi_vs_eta.tex",
#     width = 4, height = 4)
#plot(data$eta, data$psi, xlab = "eta", ylab = "ln psi")
#dev.off()

# tikz("~/DriveUEMEncrypt/casa/uem/Doutorado/Disciplinas/Estágio/Aula_Coeficiente_de_transferência_de_calor_em_corpos_submersos/figures/reamostragem.tex",
#      width = 4, height = 4)
# amostra_simulada <- data[sample(nrow(data), nrow(data), replace=TRUE), ]
# par(mfrow = c(1, 1))
# plot(amostra_simulada$eta, amostra_simulada$psi, ylim = c(-4,0), xlim = c(-0.007, 0), xlab = "eta", ylab = "ln psi")
# ajuste_simulado <- lm(amostra_simulada$psi ~ amostra_simulada$eta - 1)
# abline(ajuste_simulado)
# dev.off()
# print(ajuste_simulado$coefficients)

h_dist <- c()
for (i in 1:100) {
  amostra_simulada <- data[sample(nrow(data), nrow(data), replace=TRUE), ]
  par(mfrow = c(1, 2))
  plot(amostra_simulada$eta, amostra_simulada$psi, ylim = c(-4,0), xlim = c(-0.007, 0))
  ajuste_simulado <- lm(amostra_simulada$psi ~ amostra_simulada$eta - 1)
  abline(ajuste_simulado)
  print(ajuste_simulado$coefficients)
  h_dist <- append(h_dist, ajuste_simulado$coefficients)
  hist(h_dist, main = "")
  Sys.sleep(0.1)
}

for (i in 1:1000) {
  amostra_simulada <- data[sample(nrow(data), nrow(data), replace=TRUE), ]
  #par(mfrow = c(1, 2))
  #plot(amostra_simulada$eta, amostra_simulada$psi, ylim = c(-4,0), xlim = c(-0.007, 0))
  ajuste_simulado <- lm(amostra_simulada$psi ~ amostra_simulada$eta - 1)
  #abline(ajuste_simulado)
  #print(ajuste_simulado$coefficients)
  h_dist <- append(h_dist, ajuste_simulado$coefficients)
  #hist(h_dist, main = "")
  #Sys.sleep(0.1)
}

plot(amostra_simulada$eta, amostra_simulada$psi, ylim = c(-4,0), xlim = c(-0.007, 0))
abline(ajuste_simulado)

#tikz("~/DriveUEMEncrypt/casa/uem/Doutorado/Disciplinas/Estágio/Aula_Coeficiente_de_transferência_de_calor_em_corpos_submersos/figures/h_dist.tex",
#     width = 4, height = 4)
histograma <- hist(h_dist, main = "", ylab = "Frequência", xlab = "h")                                   # Draw histogram
abline(v = mean(h_dist),                       # Add line for mean
       col = "red",
       lwd = 3)
text(x = mean(h_dist)*1.01,                   # Add text for mean
     y = mean(histograma$counts)*1.5,
     paste(round(mean(h_dist), digits = 0)),
     col = "red",
     cex = 1)
IC <- quantile(h_dist, probs = c(0.025,0.975))

abline(v = IC[1],                       # Add line for ICinf
       col = "blue",
       lwd = 3, lty = "dashed")

abline(v = IC[2],                       # Add line for ICsup
       col = "blue",
       lwd = 3, , lty = "dashed")

text(x = IC[1]*1.01,                   # Add text for mean
     y = mean(histograma$counts)*0.9,
     paste(round(IC[1], digits = 0)),
     col = "blue",
     cex = 1)
text(x = IC[2]*0.99,                   # Add text for mean
     y = mean(histograma$counts)*0.9,
     paste(round(IC[2], digits = 0)),
     col = "blue",
     cex = 1)
#dev.off()

