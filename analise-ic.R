# ----------------------------
# SCRIPT — ANÁLISE DE INTERVALOS DE CONFIANÇA
# ----------------------------

# 1. Instalar e carregar o pacote necessário
if (!require(DescTools)) install.packages("DescTools")
library(DescTools)

# 2. Leitura dos dados
dados <- read.table("dados_fones.csv", header = TRUE, sep = ",")

# 3. Separar os dados por modelo
modelo_A <- subset(dados, modelo == "A")$tempo
modelo_B <- subset(dados, modelo == "B")$tempo

# 4. IC da MÉDIA — Modelos A e B
cat("\n--- IC da MÉDIA — MODELO A ---\n")
ic_A_90 <- MeanCI(modelo_A, conf.level = 0.90)
ic_A_95 <- MeanCI(modelo_A, conf.level = 0.95)
ic_A_99 <- MeanCI(modelo_A, conf.level = 0.99)
print(ic_A_90)
print(ic_A_95)
print(ic_A_99)
cat("\n--- IC da MÉDIA — MODELO B ---\n")
ic_B_90 <- MeanCI(modelo_B, conf.level = 0.90)
ic_B_95 <- MeanCI(modelo_B, conf.level = 0.95)
ic_B_99 <- MeanCI(modelo_B, conf.level = 0.99)
print(ic_B_90)
print(ic_B_95)
print(ic_B_99)

# 5. Margem de erro e amplitude — Modelo A
erro_A_90 <- (ic_A_90["upr.ci"] - ic_A_90["lwr.ci"]) / 2
ampl_A_90 <- ic_A_90["upr.ci"] - ic_A_90["lwr.ci"]
erro_A_95 <- (ic_A_95["upr.ci"] - ic_A_95["lwr.ci"]) / 2
ampl_A_95 <- ic_A_95["upr.ci"] - ic_A_95["lwr.ci"]
erro_A_99 <- (ic_A_99["upr.ci"] - ic_A_99["lwr.ci"]) / 2
ampl_A_99 <- ic_A_99["upr.ci"] - ic_A_99["lwr.ci"]
cat("\n--- MARGEM DE ERRO E AMPLITUDE — MODELO A ---\n")
cat("IC 90%: Margem de erro =", erro_A_90, " | Amplitude =", ampl_A_90, "\n")
cat("IC 95%: Margem de erro =", erro_A_95, " | Amplitude =", ampl_A_95, "\n")
cat("IC 99%: Margem de erro =", erro_A_99, " | Amplitude =", ampl_A_99, "\n")

# 6. Tamanho da amostra necessário para margem de erro = 0.01s
margem_desejada <- 0.01
desvio_A <- sd(modelo_A)
z_90 <- qnorm(1 - (1 - 0.90)/2)
z_95 <- qnorm(1 - (1 - 0.95)/2)
z_99 <- qnorm(1 - (1 - 0.99)/2)
n_90 <- (z_90 * desvio_A / margem_desejada)^2
n_95 <- (z_95 * desvio_A / margem_desejada)^2
n_99 <- (z_99 * desvio_A / margem_desejada)^2
cat("\n--- TAMANHO DA AMOSTRA NECESSÁRIO (Margem = 0.01s) ---\n")
cat("Confiança 90%:", ceiling(n_90), "observações\n")
cat("Confiança 95%:", ceiling(n_95), "observações\n")
cat("Confiança 99%:", ceiling(n_99), "observações\n")

# 7. IC da DIFERENÇA DE MÉDIAS (Modelo A - Modelo B)
cat("\n--- IC da DIFERENÇA DE MÉDIAS (A - B) ---\n")
ic_diff_90 <- MeanDiffCI(modelo_A, modelo_B, conf.level = 0.90)
ic_diff_95 <- MeanDiffCI(modelo_A, modelo_B, conf.level = 0.95)
ic_diff_99 <- MeanDiffCI(modelo_A, modelo_B, conf.level = 0.99)
print(ic_diff_90)
print(ic_diff_95)
print(ic_diff_99)

# 8. IC da VARIÂNCIA de cada modelo
cat("\n--- IC da VARIÂNCIA — MODELO A ---\n")
print(VarCI(modelo_A, conf.level = 0.95))
cat("\n--- IC da VARIÂNCIA — MODELO B ---\n")
print(VarCI(modelo_B, conf.level = 0.95))

# 9. IC da RAZÃO DE VARIÂNCIAS (A / B)
cat("\n--- IC da RAZÃO DE VARIÂNCIAS (A / B) ---\n")
print(VarTest(modelo_A, modelo_B, conf.level = 0.90)$conf.int)
print(VarTest(modelo_A, modelo_B, conf.level = 0.95)$conf.int)
print(VarTest(modelo_A, modelo_B, conf.level = 0.99)$conf.int)
