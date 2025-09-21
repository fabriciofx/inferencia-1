# ----------------------------
# SCRIPT — ANÁLISE DE ACEITACAÇÃO DE CAFÉ
# ----------------------------
# 1. Instalar e carregar o pacote necessário
install.packages("DescTools")
if (!require(DescTools)) install.packages("DescTools")
library(DescTools)

# 2. Leitura dos dados
#"D:\Curso Estatística_2025_1º Sem\Inferência_Estatística I\3ª Prova_Parte_1_23_09_2025\cafe_dados.csv"
setwd("C:/Users/Public/Meu_Projeto")
dados <- read.table("cafe_dados.csv", header = TRUE, sep = ",")
summary(dados)

# 3. Separar os dados por modelo
modelo_A <- subset(dados, modelo == "A")
modelo_A
modelo_B <- subset(dados, modelo == "B")
modelo_B
# 3.1 Sumarização por modelo por modelo
summary(modelo_A)
summary(modelo_B)

# 4. IC da MÉDIA — Modelos A e B
cat("\n--- IC da MÉDIA — MODELO A ---\n")
ic_A_90 <- MeanCI(modelo_A$cafeina_mg, conf.level = 0.90)
ic_A_95 <- MeanCI(modelo_A$cafeina_mg, conf.level = 0.95)
ic_A_99 <- MeanCI(modelo_A$cafeina_mg, conf.level = 0.99)
print(ic_A_90)
print(ic_A_95)
print(ic_A_99)
cat("\n--- IC da MÉDIA — MODELO B ---\n")
ic_B_90 <- MeanCI(modelo_B$cafeina_mg, conf.level = 0.90)
ic_B_95 <- MeanCI(modelo_B$cafeina_mg, conf.level = 0.95)
ic_B_99 <- MeanCI(modelo_B$cafeina_mg, conf.level = 0.99)
print(ic_B_90)
print(ic_B_95)
print(ic_B_99)

# 5.1 Margem de erro e amplitude — Modelo A
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

# 5.2 Margem de erro e amplitude — Modelo B
erro_B_90 <- (ic_B_90["upr.ci"] - ic_B_90["lwr.ci"]) / 2
ampl_B_90 <- ic_B_90["upr.ci"] - ic_B_90["lwr.ci"]
erro_B_95 <- (ic_B_95["upr.ci"] - ic_B_95["lwr.ci"]) / 2
ampl_B_95 <- ic_B_95["upr.ci"] - ic_B_95["lwr.ci"]
erro_B_99 <- (ic_B_99["upr.ci"] - ic_B_99["lwr.ci"]) / 2
ampl_B_99 <- ic_B_99["upr.ci"] - ic_B_99["lwr.ci"]
cat("\n--- MARGEM DE ERRO E AMPLITUDE — MODELO B ---\n")
cat("IC 90%: Margem de erro =", erro_B_90, " | Amplitude =", ampl_B_90, "\n")
cat("IC 95%: Margem de erro =", erro_B_95, " | Amplitude =", ampl_B_95, "\n")
cat("IC 99%: Margem de erro =", erro_B_99, " | Amplitude =", ampl_B_99, "\n")


# 6. Tamanho da amostra necessário para margem de erro = 0.01s
margem_desejada <- 0.01
desvio_A <- sd(modelo_A$cafeina_mg)
z_90 <- qnorm(1 - (1 - 0.90)/2)
z_95 <- qnorm(1 - (1 - 0.95)/2)
z_99 <- qnorm(1 - (1 - 0.99)/2)
n_90 <- (z_90 * desvio_A / margem_desejada)^2
n_95 <- (z_95 * desvio_A / margem_desejada)^2
n_99 <- (z_99 * desvio_A / margem_desejada)^2
cat("\n--- TAMANHO DA AMOSTRA NECESSÁRIO (Margem = 0.01s) ---\n")
cat("Confiança 90%:", ceiling(n_90), "observações\n")

# 7. IC da DIFERENÇA DE MÉDIAS (Modelo A - Modelo B)
cat("\n--- IC da DIFERENÇA DE MÉDIAS (A - B) ---\n")
ic_diff_90 <- MeanDiffCI(modelo_A$cafeina_mg, modelo_B$cafeina_mg, conf.level = 0.90)
ic_diff_95 <- MeanDiffCI(modelo_A$cafeina_mg, modelo_B$cafeina_mg, conf.level = 0.95)
ic_diff_99 <- MeanDiffCI(modelo_A$cafeina_mg, modelo_B$cafeina_mg, conf.level = 0.99)
print(ic_diff_90)
print(ic_diff_95)
print(ic_diff_99)

# 8. IC da VARIÂNCIA de cada modelo
cat("\n--- IC da VARIÂNCIA — MODELO A ---\n")
print(VarCI(modelo_A$cafeina_mg, conf.level = 0.95))
cat("\n--- IC da VARIÂNCIA — MODELO B ---\n")
print(VarCI(modelo_B$cafeina_mg, conf.level = 0.95))

# 9. IC da RAZÃO DE VARIÂNCIAS (A / B)
cat("\n--- IC da RAZÃO DE VARIÂNCIAS (A / B) ---\n")
print(VarTest(modelo_A, modelo_B, conf.level = 0.90)$conf.int)
print(VarTest(modelo_A, modelo_B, conf.level = 0.95)$conf.int)
print(VarTest(modelo_A, modelo_B, conf.level = 0.99)$conf.int)

# 10. Representação Gráfica dos dados associados aos dois tipos de Café.
## ---------------------------
## Boxplot cafeína: modelos A vs B
## ---------------------------

## (a) Pacotes ----
pkgs <- c("readr", "dplyr", "ggplot2")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
lapply(pkgs, library, character.only = TRUE)

## (b) Caminho do arquivo ----
## Opção A: escolha interativa
# arq <- file.choose()

## Opção B: informe o caminho diretamente (ajuste conforme sua máquina)
arq <- "C:/Users/Public/Meu_Projeto/cafe_dados_3.csv"

stopifnot(file.exists(arq))

## (c) Leitura (tenta BR ; e internacional ,) ----
suppressMessages({
  df <- tryCatch(
    readr::read_csv2(arq, show_col_types = FALSE), # ; e vírgula decimal
    error = function(e) readr::read_csv(arq, show_col_types = FALSE) # , e ponto decimal
  )
})

## (d) Conferência de colunas ----
if (!all(c("modelo","cafeina_mg") %in% names(df))) {
  stop("O CSV deve conter as colunas: 'modelo' e 'cafeina_mg'. Encontradas: ",
       paste(names(df), collapse=", "))
}

## (e) Garantir tipos corretos ----
df <- df |>
  mutate(
    modelo = as.character(modelo),
    cafeina_mg = if (is.character(cafeina_mg))
      as.numeric(sub(",", ".", cafeina_mg, fixed = TRUE))
    else as.numeric(cafeina_mg)
  ) |>
  filter(modelo %in% c("A","B"))

stopifnot(sum(!is.na(df$cafeina_mg)) >= 2)

## (f) Estatísticas de apoio (opcional) ----
print(
  df |>
    group_by(modelo) |>
    summarise(
      n = sum(!is.na(cafeina_mg)),
      media = mean(cafeina_mg, na.rm = TRUE),
      mediana = median(cafeina_mg, na.rm = TRUE),
      dp = sd(cafeina_mg, na.rm = TRUE),
      q1 = quantile(cafeina_mg, 0.25, na.rm = TRUE),
      q3 = quantile(cafeina_mg, 0.75, na.rm = TRUE),
      .groups = "drop"
    ),
  n = Inf
)

## (g) Boxplot ----
p <- ggplot(df, aes(x = modelo, y = cafeina_mg)) +
  geom_boxplot(width = 0.55, outlier.shape = NA) +                  # boxplot
  geom_jitter(width = 0.12, alpha = 0.6, size = 2) +                # pontos individuais
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,    # marca a média (losango)
               fill = "white") +
  labs(title = "Teor de cafeína por modelo (A vs B)",
       x = "Modelo", y = "Cafeína (mg)") +
  theme_minimal(base_size = 12)

print(p)

## (h) Exportar figuras ----
dir.create("plots", showWarnings = FALSE)
ggsave("plots/boxplot_cafeina_AB.png", plot = p, width = 7, height = 5, dpi = 300)
ggsave("plots/boxplot_cafeina_AB.pdf", plot = p, width = 7, height = 5)
