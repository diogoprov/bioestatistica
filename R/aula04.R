# Aula 04: medidas de tendência central e de dispersão. Figuras com os
# dados das atividades do original ou com dados simulados; cada função fixa
# a própria semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))

vermelho <- "#c0392b"
num_br <- function(v, d = 1) format(round(v, d), nsmall = d, decimal.mark = ",")

# Dados das atividades do original
capivaras <- tibble::tibble(
  grupo = rep(c("Grupo A (ração enriquecida)", "Grupo B (ração padrão)"), each = 5),
  peso = c(50, 51, 52, 58, 59, 35, 52, 53, 65, 65))
ninhos <- c(4, 5, 5, 6, 7, 7, 7)
remedios <- tibble::tibble(
  grupo = factor(rep(c("Placebo", "Droga X", "Droga Y"), each = 7),
                 c("Placebo", "Droga X", "Droga Y")),
  horas = c(1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7, 15, 6, 6, 7, 7, 7, 8, 8))
# Pressão diastólica de 35 enfermeiros (exercício de histograma da Aula 3)
pressao <- c(81, 89, 91, 81, 79, 82, 96, 70, 80, 92, 64, 73, 86, 80, 87, 74, 72, 75,
             90, 96, 82, 83, 79, 82, 82, 78, 85, 86, 77, 83, 85, 87, 88, 80, 85)

# ---------------------------------------------------------------------------
# Capivaras: mesma média, dispersões diferentes.
# ---------------------------------------------------------------------------
fig_capivaras <- function() {
  r <- capivaras |> group_by(grupo) |> summarise(m = mean(peso), s = sd(peso))
  ggplot(capivaras, aes(peso, grupo, colour = grupo)) +
    geom_vline(xintercept = 54, linetype = "22", colour = cores[["neutro"]]) +
    geom_errorbar(data = r, aes(y = grupo, xmin = m - s, xmax = m + s, colour = grupo),
                  orientation = "y", width = .18, linewidth = 1.1, inherit.aes = FALSE) +
    geom_point(size = 5, alpha = .85) +
    geom_text(data = r, aes(x = 72, y = grupo,
                            label = sprintf("média = %s kg\ns = %s kg", num_br(m, 0), num_br(s, 1))),
              hjust = 0, size = 5, lineheight = .95, inherit.aes = FALSE) +
    scale_colour_manual(values = c(cores[["verde"]], cores[["ferrugem"]]), guide = "none") +
    coord_cartesian(xlim = c(33, 82)) +
    labs(x = "Peso (kg)", y = NULL,
         caption = "Linha tracejada: a média dos dois grupos (54 kg). Barras: média ± desvio-padrão.") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Histograma com moda, mediana e média (comprimento de espinhos simulado).
# ---------------------------------------------------------------------------
fig_tendencia_histograma <- function(seed = 9, n = 80) {
  set.seed(seed)
  # assimetria à direita, para moda < mediana < média ficarem visíveis
  y <- round(.12 + rgamma(n, 2, 1 / .05), 3)
  br <- seq(.1, ceiling(max(y) / .025) * .025, by = .025)
  h <- hist(y, breaks = br, plot = FALSE)
  moda <- h$mids[which.max(h$counts)]
  topo <- max(h$counts)
  m <- tibble::tibble(rot = c("Moda", "Mediana", "Média"),
                      v = c(moda, median(y), mean(y)),
                      yy = topo + c(3.4, 2.1, .8),
                      hj = c(1.05, .5, -.05))
  ggplot(tibble::tibble(y = y), aes(y)) +
    geom_histogram(breaks = br, fill = "#c5e0d6", colour = "white") +
    geom_segment(data = m, aes(x = v, xend = v, y = yy - .45, yend = 0),
                 colour = cores[["texto"]], linewidth = .6,
                 arrow = arrow(length = unit(.2, "cm"))) +
    geom_label(data = m, aes(v, yy, label = sprintf("%s = %s", rot, num_br(v, 3)), hjust = hj),
               size = 4.4, linewidth = 0, fill = cores[["papel"]]) +
    labs(x = "Comprimento do espinho (mm)", y = "Frequência",
         caption = sprintf("%d espinhos (dados simulados).", n)) +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Mediana numa reta e moda num gráfico de frequências.
# ---------------------------------------------------------------------------
fig_mediana_reta <- function(x = c(3.5, 2.8, 3.2, 2.2, 3.7)) {
  ggplot(tibble::tibble(x = x), aes(x, 0)) +
    geom_hline(yintercept = 0, colour = cores[["neutro"]]) +
    geom_point(size = 5, colour = cores[["accent"]]) +
    annotate("segment", x = median(x), xend = median(x), y = -.35, yend = -.08,
             colour = vermelho, linewidth = 1, arrow = arrow(length = unit(.25, "cm"))) +
    annotate("label", x = median(x), y = -.45, label = sprintf("Mediana = %s", num_br(median(x), 1)),
             colour = vermelho, fontface = "bold", size = 5, linewidth = 0, fill = cores[["papel"]]) +
    scale_x_continuous(breaks = seq(2, 4, .25)) +
    scale_y_continuous(NULL, breaks = NULL, limits = c(-.6, .25)) +
    labs(x = "Comprimento rostro-cloacal (cm)") +
    tema_aula(15) + theme(panel.grid = element_blank())
}

fig_moda_barras <- function(x = c(0, 0, 2, 5, 3, 7, 4, 7, 8, 7, 9, 6, 7)) {
  d <- tibble::tibble(x = x) |> count(x) |> mutate(moda = n == max(n))
  ggplot(d, aes(x, n, fill = moda)) +
    geom_col(width = .6) +
    scale_fill_manual(values = c(`FALSE` = cores[["neutro"]], `TRUE` = vermelho), guide = "none") +
    scale_x_continuous(breaks = 0:10) +
    labs(x = "Número de girinos por armadilha", y = "Frequência") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Atividade dos ninhos: histograma sem e com o ninho "mutante".
# ---------------------------------------------------------------------------
fig_ninhos <- function(mutante = FALSE, destaca_moda = FALSE, linhas = FALSE) {
  x <- if (mutante) c(ninhos, 15) else ninhos
  d <- tibble::tibble(x = x) |> count(x) |>
    mutate(moda = destaca_moda & n == max(n))
  g <- ggplot(d, aes(x, n)) +
    geom_col(aes(fill = moda), width = .8) +
    scale_fill_manual(values = c(`FALSE` = "#bdb6a8", `TRUE` = vermelho), guide = "none") +
    scale_x_continuous(breaks = 3:16, limits = c(3, 16)) +
    scale_y_continuous(breaks = 0:3) +
    labs(x = "Número de ovos por ninho", y = "Número de ninhos") +
    tema_aula(15)
  if (linhas) {
    m <- tibble::tibble(rot = c("Média", "Mediana"), v = c(mean(x), median(x)),
                        cor = c(cores[["ferrugem"]], cores[["accent"]]))
    g <- g + geom_vline(data = m, aes(xintercept = v, colour = cor), linewidth = 1.2) +
      geom_label(data = m, aes(v, 3.3, label = sprintf("%s = %s", rot, num_br(v, 2)), colour = cor),
                 size = 4.6, fill = cores[["papel"]], linewidth = 0, hjust = c(-.05, 1.05)) +
      scale_colour_identity()
  }
  g
}

# ---------------------------------------------------------------------------
# Cinco grupos com a mesma média (exemplo de Bussab & Morettin).
# ---------------------------------------------------------------------------
fig_grupos_media5 <- function() {
  d <- tibble::tibble(
    grupo = rep(c("A (X)", "B (Y)", "C (Z)", "D (W)", "E (V)"), c(5, 5, 5, 4, 5)),
    x = c(3, 4, 5, 6, 7, 1, 3, 5, 7, 9, 5, 5, 5, 5, 5, 3, 5, 5, 7, 3, 5, 5, 6, 6)) |>
    mutate(grupo = factor(grupo, rev(unique(grupo))))
  ggplot(d, aes(x, grupo)) +
    geom_vline(xintercept = 5, linetype = "22", colour = cores[["neutro"]]) +
    geom_dotplot(binaxis = "x", stackdir = "center", binwidth = .35, dotsize = .9,
                 fill = cores[["accent"]], colour = NA) +
    scale_x_continuous(breaks = 0:10) +
    labs(x = "Valor", y = "Grupo", caption = "Todas as médias são iguais a 5 (linha tracejada).") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Quartis da pressão diastólica dos 35 enfermeiros, em três etapas.
# ---------------------------------------------------------------------------
fig_quartis_pressao <- function(etapa = 1) {
  q <- quantile(pressao, c(.25, .5, .75))
  g <- ggplot(tibble::tibble(x = pressao), aes(x)) +
    geom_histogram(bins = 6, fill = "#5f5a52", colour = "white") +
    labs(x = "Pressão diastólica sanguínea (mmHg)", y = "Frequência") +
    tema_aula(15)
  if (etapa >= 2) {
    ql <- tibble::tibble(v = q, rot = c("Q1 (25%)", "Mediana (50%)", "Q3 (75%)"),
                         cor = c(vermelho, cores[["accent"]], cores[["verde"]]))
    g <- g + geom_vline(data = ql, aes(xintercept = v, colour = cor), linetype = "22",
                        linewidth = 1.1) +
      geom_label(data = ql, aes(v, 15.5 - c(0, 1.6, 0), label = sprintf("%s = %s", rot, num_br(v, 1)),
                                colour = cor), size = 4.2, fill = cores[["papel"]], linewidth = 0) +
      scale_colour_identity() + coord_cartesian(ylim = c(0, 16.5))
  }
  if (etapa >= 3)
    g <- g + annotate("rect", xmin = q[[1]], xmax = q[[3]], ymin = 4, ymax = 6.5,
                      fill = "white", colour = "#d98a3d", linewidth = 1.2) +
      annotate("segment", x = q[[2]], xend = q[[2]], y = 4, yend = 6.5, colour = "#8e44ad",
               linewidth = 1.4) +
      annotate("text", x = mean(q[c(1, 3)]), y = 7.2, size = 4.4,
               label = sprintf("Intervalo interquartil = %s", num_br(q[[3]] - q[[1]], 1)))
  g
}

# ---------------------------------------------------------------------------
# Desvios em relação à média: idades de cinco crianças (3, 6, 5, 7, 9).
# ---------------------------------------------------------------------------
fig_desvios <- function(x = c(3, 6, 5, 7, 9)) {
  d <- tibble::tibble(i = seq_along(x), x = x, dev = x - mean(x))
  ggplot(d, aes(i, x)) +
    geom_hline(yintercept = mean(x), colour = cores[["texto"]], linewidth = .9) +
    geom_segment(aes(xend = i, yend = mean(x), colour = dev > 0), linewidth = 1.6) +
    geom_point(size = 5, colour = cores[["texto"]]) +
    geom_text(aes(label = ifelse(dev > 0, paste0("+", dev), dev), colour = dev > 0),
              nudge_x = .22, size = 5.2, fontface = "bold") +
    annotate("text", x = .55, y = mean(x) + .35, label = "média = 6", hjust = 0, size = 4.8) +
    scale_colour_manual(values = c(`TRUE` = cores[["accent"]], `FALSE` = cores[["ferrugem"]]),
                        guide = "none") +
    scale_x_continuous(breaks = 1:5, labels = paste("Criança", 1:5)) +
    labs(x = NULL, y = "Idade (anos)") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Posição e dispersão são propriedades independentes (Krzywinski & Altman
# 2013): curvas normais que mudam só a média ou só o desvio-padrão.
# ---------------------------------------------------------------------------
fig_posicao_dispersao <- function() {
  x <- seq(-6, 6, by = .05)
  pos <- purrr::map_dfr(c(-2, 0, 2), \(m) tibble::tibble(x = x, y = dnorm(x, m, 1), k = paste0("μ = ", m))) |>
    mutate(painel = "Muda a posição (média)")
  dis <- purrr::map_dfr(c(.6, 1, 1.8), \(s) tibble::tibble(x = x, y = dnorm(x, 0, s), k = paste0("σ = ", num_br(s, 1)))) |>
    mutate(painel = "Muda a dispersão (desvio-padrão)")
  ggplot(bind_rows(pos, dis) |> mutate(painel = factor(painel, c("Muda a posição (média)",
                                                                  "Muda a dispersão (desvio-padrão)"))),
         aes(x, y, colour = k)) +
    geom_line(linewidth = 1.3) +
    facet_wrap(~painel) +
    scale_colour_manual(values = c("μ = -2" = cores[["accent"]], "μ = 0" = cores[["accent_dark"]],
                                   "μ = 2" = "#7fb0c2", "σ = 0,6" = cores[["ferrugem"]],
                                   "σ = 1,0" = "#d98a3d", "σ = 1,8" = "#e6c35c"), name = NULL) +
    labs(x = NULL, y = "Densidade") +
    tema_aula(15) + theme(axis.text.y = element_blank())
}

# ---------------------------------------------------------------------------
# Formas de distribuição com média, mediana e moda (redesenho da figura de
# Zar). `destaque` realça um par de grupos, como nos três slides originais.
# ---------------------------------------------------------------------------
fig_formas <- function(destaque = c("nenhum", "simetria", "assimetria", "modalidade")) {
  destaque <- match.arg(destaque)
  set.seed(1)
  x <- seq(0, 10, by = .01)
  dens <- list(
    "Simétrica unimodal"   = dnorm(x, 5, 1.2),
    "Simétrica bimodal"    = .5 * dnorm(x, 3.6, .8) + .5 * dnorm(x, 6.4, .8),
    "Assimétrica positiva" = dgamma(x, 3, 1),
    "Assimétrica negativa" = dgamma(10 - x, 3, 1))
  amostras <- list(rnorm(1e5, 5, 1.2),
                   c(rnorm(5e4, 3.6, .8), rnorm(5e4, 6.4, .8)),
                   rgamma(1e5, 3, 1), 10 - rgamma(1e5, 3, 1))
  d <- purrr::imap_dfr(dens, \(y, nm) tibble::tibble(painel = nm, x = x, y = y))
  medidas <- purrr::map2_dfr(names(dens), amostras, function(nm, a) {
    yy <- dens[[nm]]
    modas <- if (nm == "Simétrica bimodal") c(3.6, 6.4) else x[which.max(yy)]
    tibble::tibble(painel = nm,
                   rot = c("Média", "Mediana", rep("Moda", length(modas))),
                   v = c(mean(a), median(a), modas))
  })
  grupo <- c("Simétrica unimodal" = "simetria", "Simétrica bimodal" = "simetria",
             "Assimétrica positiva" = "assimetria", "Assimétrica negativa" = "assimetria")
  d <- mutate(d, painel = factor(painel, names(dens)),
              realce = case_when(destaque == "nenhum" ~ TRUE,
                                 destaque == "modalidade" ~ painel != "Simétrica bimodal",
                                 destaque == "simetria" ~ grupo[as.character(painel)] == "simetria",
                                 TRUE ~ grupo[as.character(painel)] == "assimetria"))
  medidas <- mutate(medidas, painel = factor(painel, names(dens)))
  cor_m <- c(Média = cores[["ferrugem"]], Mediana = cores[["accent"]], Moda = cores[["verde"]])
  ggplot(d, aes(x, y)) +
    geom_area(aes(alpha = realce), fill = "#d8d2c4") +
    geom_line(aes(alpha = realce), linewidth = 1.1) +
    geom_vline(data = medidas, aes(xintercept = v, colour = rot), linewidth = .9) +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = .25), guide = "none") +
    scale_colour_manual(values = cor_m, name = NULL) +
    facet_wrap(~painel, ncol = 4) +
    labs(x = NULL, y = NULL) +
    tema_aula(14) + theme(axis.text = element_blank(), panel.grid = element_blank())
}

# ---------------------------------------------------------------------------
# Boxplots da atividade "Qual remédio é melhor?".
# ---------------------------------------------------------------------------
fig_remedios <- function() {
  ggplot(remedios, aes(grupo, horas, fill = grupo)) +
    geom_boxplot(width = .5, outlier.shape = NA, alpha = .8) +
    geom_jitter(aes(colour = horas > 10), width = .08, height = 0, size = 2.4, alpha = .8) +
    scale_colour_manual(values = c(`FALSE` = cores[["texto"]], `TRUE` = vermelho), guide = "none") +
    scale_fill_manual(values = c(cores[["neutro_cl"]], "#e6c35c", cores[["verde"]]), guide = "none") +
    scale_y_continuous(breaks = seq(0, 16, 2)) +
    labs(x = NULL, y = "Duração do efeito analgésico (h)") +
    tema_aula(15)
}
