rm(list = ls())

df <- read.table("data/youtube.txt", header = TRUE)

# Separa os dados por canal
canais <- split(df, df$CANAL)
df.CANAL_1 <- canais[[1]]
df.CANAL_2 <- canais[[2]]

# Exibe as primeiras linhas de cada data frame
head(df.CANAL_1)
head(df.2)

# Calcula o número acumulado de inscritos para cada canal
df.CANAL_1$INSCRITOS <- df.CANAL_1$INSCRITOS/100000
df.CANAL_1$Y <- cumsum(df.CANAL_1$INSCRITOS)

df.CANAL_2$INSCRITOS <- df.CANAL_2$INSCRITOS/100000
df.CANAL_2$Y <- cumsum(df.CANAL_2$INSCRITOS)

# Função para o modelo logistico
f_log <- function(DIAS, L, beta, beta0) {
  out <- L/(1+ exp(-beta*(DIAS - beta0)))
  return(out)
}

# Função para calcular a perda quadrática
fc_perda <- function(par, y, DIAS) {
  mu <- f_log(DIAS = DIAS, L = par[1], beta = par[2], beta0 = par[3])
  out <- sum((y - mu)^2)
  return(out)
}

# Mostra os gráficos lado a lado
par(mfrow = c(1,2), mar=c(2.6, 3, 1.2, 0.5), mgp = c(1.6, 0.6, 0))

# Gráfico do número acumulado de inscritos por dia para o canal 1
plot(df.CANAL_1$Y ~ df.CANAL_1$DIAS, 
     xlim = c(0, 1215), 
     ylim = c(0, 25),
     ylab = "Número de inscritos*100000", 
     main = "Canal 1",
     xlab = "Dias", 
     type = "o", 
     cex = 0.1)

# Aplica linha de corte no dia 850
abline(v = 850)

# Gráfico do número acumulado de inscritos por dia para o canal 2
plot(df.CANAL_2$Y ~ df.CANAL_2$DIAS, 
     ylab = "Número de inscritos*100000", 
     main = "Canal 2", 
     xlab = "Dias", 
     ylim = c(0, 50), 
     xlim = c(0, 980), 
     type = "p", 
     cex = 0.1)

# Aplica linha de corte no dia 850
abline(v = 607)

# Mostra os gráficos lado a lado
par(mfrow = c(1,2), mar=c(2.6, 3, 1.2, 0.5), mgp = c(1.6, 0.6, 0))


##### CANAL 1 ##### 


# Calcula a perda variando os parâmetros para encontrar o melhor, mais próximo do real
fc_perda(par = c(24, 0.007, 600), y = df.CANAL_1$Y, DIAS = df.CANAL_1$DIAS)


# Otimiza o modelo com utilizando os parâmetros encontrados manualmente
tmp <- optim(par = c(24, 0.007, 600), 
             fn = fc_perda, 
             y = df.CANAL_1$Y, 
             DIAS = df.CANAL_1$DIAS, 
             method = 'Nelder-Mead')

round(tmp$par, 3)

plot(df.CANAL_1$Y ~ df.CANAL_1$DIAS, 
     xlim = c(0, 1215), 
     ylim = c(0, 25),
     ylab = "Número de inscritos*100000", 
     main = "Canal 1",
     xlab = "Dias", 
     type = "o", 
     cex = 0.1)

abline(v = 850)

DIAS <- 1:c(850 + 365)

predito <- f_log(DIAS = DIAS, 
                 L = tmp$par[1], 
                 beta = tmp$par[2], 
                 beta0 = tmp$par[3])

lines(DIAS, predito, col = 'green')


##### CANAL 2 ##### 


# Calcula a perda variando os parâmetros para encontrar o melhor, mais próximo do real
fc_perda(par = c(27, 0.008, 620), y = df.CANAL_2$Y, DIAS = df.CANAL_2$DIAS)


# Otimiza o modelo com utilizando os parâmetros encontrados manualmente
tmp <- optim(par = c(27, 0.008, 620), 
             fn = fc_perda, 
             y = df.CANAL_2$Y, 
             DIAS = df.CANAL_2$DIAS, 
             method = 'Nelder-Mead')

round(tmp$par, 3)

plot(df.CANAL_2$Y ~ df.CANAL_2$DIAS, 
     ylab = "Número de inscritos*100000", 
     main = "Canal 2", 
     xlab = "Dias", 
     ylim = c(0, 50), 
     xlim = c(0, 980), 
     type = "p", 
     cex = 0.1)

# Aplica linha de corte no dia 850
abline(v = 607)

DIAS <- 1:c(850 + 365)

predito <- f_log(DIAS = DIAS, 
                 L = tmp$par[1], 
                 beta = tmp$par[2], 
                 beta0 = tmp$par[3])

lines(DIAS, predito, col = 'blue')
