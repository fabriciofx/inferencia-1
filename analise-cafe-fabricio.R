# ----------------------------
# SCRIPT — ANÁLISE DE INTERVALOS DE CONFIANÇA
# ----------------------------

# 1. Instalar e carregar os pacotes necessários
if (!require(DescTools)) install.packages("DescTools")
library(DescTools)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(patchwork)) install.packages("patchwork")
library(patchwork)

# 2. Leitura dos dados
dados <- read.table("cafe_dados.csv", header = TRUE, sep = ",")

# 3. Separar os dados por marca
marca_A <- subset(dados, modelo == "A")$cafeina_mg
marca_B <- subset(dados, modelo == "B")$cafeina_mg

# 3.1 Tamanho das amostras - Marcas A e B
n_A <- length(marca_A)
n_B <- length(marca_B)
cat("Tamanho da amostra da Marca A: ", n_A, "\n")
cat("Tamanho da amostra da Marca B: ", n_B, "\n")

# 3.2 Criar data frames para cada conjunto de dados
df_A <- data.frame(marca_A)
df_B <- data.frame(marca_B)

# 3.3 Média das amostras - Marcas A e B
media_A <- mean(marca_A)
media_B <- mean(marca_B)

# 4. IC da MÉDIA — Marcas A e B
cat("\n--- IC da MÉDIA — Marca A ---\n")
ic_A_90 <- MeanCI(marca_A, conf.level = 0.90)
ic_A_95 <- MeanCI(marca_A, conf.level = 0.95)
ic_A_99 <- MeanCI(marca_A, conf.level = 0.99)
print(ic_A_90)
print(ic_A_95)
print(ic_A_99)

cat("\n--- IC da MÉDIA — Marca B ---\n")
ic_B_90 <- MeanCI(marca_B, conf.level = 0.90)
ic_B_95 <- MeanCI(marca_B, conf.level = 0.95)
ic_B_99 <- MeanCI(marca_B, conf.level = 0.99)
print(ic_B_90)
print(ic_B_95)
print(ic_B_99)

# 5.1 Margem de erro e amplitude — Marca A
erro_A_90 <- (ic_A_90["upr.ci"] - ic_A_90["lwr.ci"]) / 2
ampl_A_90 <- ic_A_90["upr.ci"] - ic_A_90["lwr.ci"]
erro_A_95 <- (ic_A_95["upr.ci"] - ic_A_95["lwr.ci"]) / 2
ampl_A_95 <- ic_A_95["upr.ci"] - ic_A_95["lwr.ci"]
erro_A_99 <- (ic_A_99["upr.ci"] - ic_A_99["lwr.ci"]) / 2
ampl_A_99 <- ic_A_99["upr.ci"] - ic_A_99["lwr.ci"]
cat("\n--- MARGEM DE ERRO E AMPLITUDE — Marca A ---\n")
cat("IC 90%: Margem de erro =", erro_A_90, " | Amplitude =", ampl_A_90, "\n")
cat("IC 95%: Margem de erro =", erro_A_95, " | Amplitude =", ampl_A_95, "\n")
cat("IC 99%: Margem de erro =", erro_A_99, " | Amplitude =", ampl_A_99, "\n")

# 5.2 Margem de erro e amplitude — Marca B
erro_B_90 <- (ic_B_90["upr.ci"] - ic_B_90["lwr.ci"]) / 2
ampl_B_90 <- ic_B_90["upr.ci"] - ic_B_90["lwr.ci"]
erro_B_95 <- (ic_B_95["upr.ci"] - ic_B_95["lwr.ci"]) / 2
ampl_B_95 <- ic_B_95["upr.ci"] - ic_B_95["lwr.ci"]
erro_B_99 <- (ic_B_99["upr.ci"] - ic_B_99["lwr.ci"]) / 2
ampl_B_99 <- ic_B_99["upr.ci"] - ic_B_99["lwr.ci"]
cat("\n--- MARGEM DE ERRO E AMPLITUDE — Marca B ---\n")
cat("IC 90%: Margem de erro =", erro_B_90, " | Amplitude =", ampl_B_90, "\n")
cat("IC 95%: Margem de erro =", erro_B_95, " | Amplitude =", ampl_B_95, "\n")
cat("IC 99%: Margem de erro =", erro_B_99, " | Amplitude =", ampl_B_99, "\n")

# 6.1 Tamanho da amostra necessário para margem de erro = 0.01 (1%) - Marca A
margem_desejada <- 0.01
desvio_A <- sd(marca_A)
z_90 <- qnorm(1 - (1 - 0.90)/2)
z_95 <- qnorm(1 - (1 - 0.95)/2)
z_99 <- qnorm(1 - (1 - 0.99)/2)
n_90 <- (z_90 * desvio_A / margem_desejada)^2
n_95 <- (z_95 * desvio_A / margem_desejada)^2
n_99 <- (z_99 * desvio_A / margem_desejada)^2
cat("\n--- TAMANHO DA AMOSTRA NECESSÁRIO (Margem = 0.01) - Marca A ---\n")
cat("Confiança 90%:", ceiling(n_90), "observações\n")
cat("Confiança 95%:", ceiling(n_95), "observações\n")
cat("Confiança 99%:", ceiling(n_99), "observações\n")

# 6.2 Tamanho da amostra necessário para margem de erro = 0.01 (1%) 0 Marca B
margem_desejada <- 0.01
desvio_B <- sd(marca_B)
z_90 <- qnorm(1 - (1 - 0.90)/2)
z_95 <- qnorm(1 - (1 - 0.95)/2)
z_99 <- qnorm(1 - (1 - 0.99)/2)
n_90 <- (z_90 * desvio_B / margem_desejada)^2
n_95 <- (z_95 * desvio_B / margem_desejada)^2
n_99 <- (z_99 * desvio_B / margem_desejada)^2
cat("\n--- TAMANHO DA AMOSTRA NECESSÁRIO (Margem = 0.01) - Marca B ---\n")
cat("Confiança 90%:", ceiling(n_90), "observações\n")
cat("Confiança 95%:", ceiling(n_95), "observações\n")
cat("Confiança 99%:", ceiling(n_99), "observações\n")

# 7. IC da DIFERENÇA DE MÉDIAS (Marca A - Marca B)
cat("\n--- IC da DIFERENÇA DE MÉDIAS (A - B) ---\n")
ic_diff_90 <- MeanDiffCI(marca_A, marca_B, conf.level = 0.90)
ic_diff_95 <- MeanDiffCI(marca_A, marca_B, conf.level = 0.95)
ic_diff_99 <- MeanDiffCI(marca_A, marca_B, conf.level = 0.99)
print(ic_diff_90)
print(ic_diff_95)
print(ic_diff_99)

# 8. IC da VARIÂNCIA de cada marca
cat("\n--- IC da VARIÂNCIA — Marca A ---\n")
print(VarCI(marca_A, conf.level = 0.95))
cat("\n--- IC da VARIÂNCIA — Marca B ---\n")
print(VarCI(marca_B, conf.level = 0.95))

# 9. IC da RAZÃO DE VARIÂNCIAS (A / B)
cat("\n--- IC da RAZÃO DE VARIÂNCIAS (A / B) ---\n")
print(VarTest(marca_A, marca_B, conf.level = 0.90)$conf.int)
print(VarTest(marca_A, marca_B, conf.level = 0.95)$conf.int)
print(VarTest(marca_A, marca_B, conf.level = 0.99)$conf.int)

# 10. Compara os histogramas das Marcas A e B
hist_A <- ggplot(df_A, aes(x = marca_A)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "lightblue",
    color = "black"
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = media_A, sd = desvio_A),
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Marca A - Histograma",
    x = "Teor de cafeína (mg/xícara)",
    y = "Densidade"
  ) +
  theme_minimal()
hist_B <- ggplot(df_B, aes(x = marca_B)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "lightpink",
    color = "black"
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = media_B, sd = desvio_B),
    color = "red",
    linewidth = 1.2
  ) +
  labs(
    title = "Marca B - Histograma",
    x = "Teor de cafeína (mg/xícara)",
    y = "Densidade"
  ) +
  theme_minimal()
hist_AB <- hist_A / hist_B
print(hist_AB)

# 11. Compara os boxplots das Marcas A e B
bplot_AB <- ggplot(dados, aes(x = modelo, y = cafeina_mg)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "red") +
  labs(
    title = "Teor de cafeína por marca",
    x = "Marca",
    y = "Teor de cafeína (mg/xícara)"
  ) +
  theme_minimal()
print(bplot_AB)

# 12. Exporta as figuras
if (!dir.exists("plots")) dir.create("plots")
ggsave("plots/histograma_cafeina_AB.pdf", plot = hist_AB, width = 7, height = 5)
ggsave("plots/boxplot_cafeina_AB.pdf", plot = bplot_AB, width = 7, height = 5)

