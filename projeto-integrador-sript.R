rm(list = ls())

df <- read.table("data/youtube.txt", header = TRUE)

# Separa os dados por canal
canais <- split(df, df$CANAL)
df_canal1 <- canais[[1]]
df_canal2 <- canais[[2]]

# Exibe as primeiras linhas de cada data frame
head(df_canal1)
head(df_canal2)

# Calcula o número acumulado Y de inscritos para cada canal
df_canal1$INSCRITOS <- df_canal1$INSCRITOS / 100000
df_canal1$Y <- cumsum(df_canal1$INSCRITOS)

df_canal2$INSCRITOS <- df_canal2$INSCRITOS / 100000
df_canal2$Y <- cumsum(df_canal2$INSCRITOS)

# Função para o modelo logistico
f_log <- function(dias, l, beta, beta0) {
     out <- l / (1 + exp(-beta * (dias - beta0)))
     return(out)
}

# Função para calcular a perda quadrática
fc_perda <- function(par, y, dias) {
     mu <- f_log(dias = dias, l = par[1], beta = par[2], beta0 = par[3])
     out <- sum((y - mu)^2)
     return(out)
}

# Mostra os gráficos lado a lado
par(mfrow = c(1, 2), mar = c(2.6, 3, 1.2, 0.5), mgp = c(1.6, 0.6, 0))

# Gráfico do número acumulado de inscritos por dia para o canal 1
plot(df_canal1$Y ~ df_canal1$DIAS,
     xlim = c(0, 1215),
     ylim = c(0, 25),
     ylab = "Número de inscritos*100000",
     main = "Canal 1",
     xlab = "DIAS",
     type = "o",
     cex = 0.1
)

# Aplica linha de corte no dia 850
abline(v = 850)

# Gráfico do número acumulado de inscritos por dia para o canal 2
plot(df_canal2$Y ~ df_canal2$DIAS,
     ylab = "Número de inscritos*100000",
     main = "Canal 2",
     xlab = "DIAS",
     ylim = c(0, 50),
     xlim = c(0, 980),
     type = "p",
     cex = 0.1
)

# Aplica linha de corte no dia 607
abline(v = 607)


##### CANAL 1 #####


# Calcula a perda variando os parâmetros para encontrar o melhor,
# mais próximo encontrado em c(24, 0.007, 600)
fc_perda(par = c(24, 0.007, 600), y = df_canal1$Y, dias = df_canal1$DIAS)


# Otimiza o modelo utilizando os parâmetros encontrados manualmente
tmp <- optim(
     par = c(24, 0.007, 600),
     fn = fc_perda,
     y = df_canal1$Y,
     dias = df_canal1$DIAS,
     method = "Nelder-Mead"
)

# round(tmp$par, 3)

# Plota o gráfico com o predito
plot(df_canal1$Y ~ df_canal1$DIAS,
     xlim = c(0, 1215),
     ylim = c(0, 25),
     ylab = "Número de inscritos*100000",
     main = "Canal 1",
     xlab = "DIAS",
     type = "o",
     cex = 0.1
)
abline(v = 850)
dias <- 1:c(850 + 365)
predito <- f_log(
     dias = dias,
     l = tmp$par[1],
     beta = tmp$par[2],
     beta0 = tmp$par[3]
)
lines(dias, predito, col = "green")


##### CANAL 2 #####

fc_perda(par = c(27, 0.008, 620), y = df_canal2$Y, dias = df_canal2$DIAS)

tmp <- optim(
     par = c(27, 0.008, 620),
     fn = fc_perda,
     y = df_canal2$Y,
     dias = df_canal2$DIAS,
     method = "Nelder-Mead"
)

# round(tmp$par, 3)

plot(df_canal2$Y ~ df_canal2$DIAS,
     ylab = "Número de inscritos*100000",
     main = "Canal 2",
     xlab = "DIAS",
     ylim = c(0, 50),
     xlim = c(0, 980),
     type = "p",
     cex = 0.1
)
abline(v = 607)
dias <- 1:c(850 + 365)
predito <- f_log(
     dias = dias,
     l = tmp$par[1],
     beta = tmp$par[2],
     beta0 = tmp$par[3]
)
lines(dias, predito, col = "blue")
