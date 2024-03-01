## Función para las prob. betas
betas <- function(mu, c_i) 1 - pnorm(c_i, mean = mu, sd = sqrt(12))
#ci calculados teóricamente
c_i <- c(121.1669, 123.82, 127.3375, 123.29)
#medias 
mu <- seq(121, 130, by = 0.01)
#obtener betas
betas_prop <- sapply(c_i, function(ci) betas(mu, ci))
#Grafica
plot(mu, betas_prop[,1], type = "l", col = c("blue", "red", "lightblue", "orange"),
     ylim = c(0, 1), xlab = 'Valores de mu', ylab = expression(beta[i](mu)),
     main = "Tasa de crecimiento de las probabilidades tipo II")
colors <- c("blue", "red", "lightblue",'orange')

for (i in 1:4) {
  lines(mu, betas_prop[, i], col = colors[i - 1])
}

legend("bottomright", legend = c(expression(beta[1]), expression(beta[2]),
                                expression(beta[3]), expression(beta[4])),
      col = c(colors), lty = 1)



legend("center", legend = expression(beta[1:4]),
       col = c("blue", colors), lty = 1)
