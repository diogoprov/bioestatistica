# Aula 09: correlação e regressão linear. Os dados de Galton (1886) são
# reais e de domínio público (data/galton.csv); os demais são simulados, e
# cada função que sorteia fixa a própria semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/aula02_pergunta.R"))   # fig_diagrama()

vermelho <- "#c0392b"
azul_ponto <- "#3b6db3"
num_br <- function(v, d = 2) format(round(v, d), nsmall = d, decimal.mark = ",")
fundo_papel <- plot_annotation(theme = theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA)))

# Altura (polegadas) de 928 filhos adultos e a média da altura dos pais.
galton <- read.csv(here::here("data/galton.csv"))

# ---------------------------------------------------------------------------
# Galton: os filhos "regridem" para a média.
# ---------------------------------------------------------------------------
fig_galton <- function(d = galton) {
  m <- lm(child ~ parent, data = d)
  ggplot(d, aes(parent, child)) +
    geom_point(position = position_jitter(width = .25, height = .25, seed = 1),
               alpha = .35, size = 1.6, colour = cores[["texto"]]) +
    geom_abline(intercept = 0, slope = 1, linetype = "22", colour = cores[["neutro"]], linewidth = 1) +
    geom_abline(intercept = coef(m)[1], slope = coef(m)[2], colour = vermelho, linewidth = 1.3) +
    annotate("label", x = 74.4, y = 74.3, hjust = 1, label = "filho = pais", colour = cores[["neutro"]],
             size = 4.6, fill = cores[["papel"]], linewidth = 0) +
    annotate("label", x = 74.4, y = 71.2, hjust = 1, label = "reta de regressão", colour = vermelho,
             size = 4.6, fill = cores[["papel"]], linewidth = 0) +
    coord_equal(xlim = c(63.5, 74.5), ylim = c(61, 74.5)) +
    labs(x = "Média da altura dos pais (polegadas)", y = "Altura do filho (polegadas)") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Correlação: quatro padrões com r diferentes.
# ---------------------------------------------------------------------------
fig_correlacoes <- function(seed = 4, n = 40) {
  set.seed(seed)
  x <- rnorm(n)
  ruido <- rnorm(n)
  d <- bind_rows(
    tibble::tibble(painel = "a", x = x, y = .9 * x + sqrt(1 - .81) * ruido),
    tibble::tibble(painel = "b", x = x, y = -.7 * x + sqrt(1 - .49) * ruido),
    tibble::tibble(painel = "c", x = x, y = .1 * x + ruido),
    tibble::tibble(painel = "d", x = x, y = 1 - x^2 + .3 * ruido)
  )
  rotulos <- d |> group_by(painel) |> summarise(r = cor(x, y)) |>
    mutate(nome = c("Relação linear positiva", "Relação linear negativa",
                    "Sem relação", "Relação não linear"),
           rotulo = sprintf("%s\nr = %s", nome, num_br(r, 2)))
  d <- left_join(d, select(rotulos, painel, rotulo), by = "painel") |>
    mutate(rotulo = factor(rotulo, levels = rotulos$rotulo))
  ggplot(d, aes(x, y)) +
    geom_point(colour = azul_ponto, size = 2) +
    facet_wrap(~ rotulo, nrow = 1, scales = "free_y") +
    labs(x = "X", y = "Y") +
    tema_aula(13, conceitual = TRUE)
}

# ---------------------------------------------------------------------------
# Quarteto de Anscombe (dados do R base).
# ---------------------------------------------------------------------------
fig_anscombe <- function() {
  d <- purrr::map_dfr(1:4, \(i) tibble::tibble(
    conjunto = sprintf("Conjunto %d", i),
    x = anscombe[[paste0("x", i)]], y = anscombe[[paste0("y", i)]]))
  ggplot(d, aes(x, y)) +
    geom_smooth(method = "lm", se = FALSE, formula = y ~ x, colour = vermelho, fullrange = TRUE) +
    geom_point(size = 2.6, colour = azul_ponto) +
    facet_wrap(~ conjunto, nrow = 1) +
    tema_aula(14)
}

# ---------------------------------------------------------------------------
# Correlação espúria: chocolate e prêmios Nobel (dados fictícios).
# ---------------------------------------------------------------------------
fig_chocolate <- function(seed = 2, n = 23) {
  set.seed(seed)
  d <- tibble::tibble(chocolate = runif(n, 0, 12)) |>
    mutate(nobel = pmax(0, 2.4 * chocolate - 2 + rnorm(n, 0, 5)))
  r <- cor(d$chocolate, d$nobel)
  ggplot(d, aes(chocolate, nobel)) +
    geom_point(size = 3, colour = cores[["ferrugem"]]) +
    annotate("text", x = .3, y = max(d$nobel), hjust = 0, size = 5,
             label = sprintf("r = %s", num_br(r, 2))) +
    labs(x = "Consumo de chocolate (kg/ano/pessoa)",
         y = "Prêmios Nobel por\n10 milhões de habitantes",
         subtitle = "Cada ponto é um país (dados fictícios)") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Gráfico de dispersão passo a passo, as hipóteses sobre o slope e os
# resíduos, todos com os mesmos 12 pontos simulados.
# ---------------------------------------------------------------------------
pontos_dispersao <- function(seed = 21, n = 12) {
  set.seed(seed)
  tibble::tibble(x = sort(runif(n, 1, 9))) |> mutate(y = 1.5 + .6 * x + rnorm(n, 0, .9))
}

eixos_conceituais <- function(g, rotulo_y = "Variável resposta (Y)") {
  g + labs(x = "Variável preditora (X)", y = rotulo_y) +
    scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 9), expand = c(0, 0)) +
    tema_aula(16, conceitual = TRUE)
}

fig_dispersao <- function(etapa = 3, d = pontos_dispersao()) {
  g <- ggplot(d, aes(x, y))
  if (etapa >= 2) g <- g + geom_point(colour = azul_ponto, size = 3)
  if (etapa >= 3) {
    m <- lm(y ~ x, d)
    g <- g + geom_abline(intercept = coef(m)[1], slope = coef(m)[2], colour = vermelho, linewidth = 1.3) +
      annotate("text", x = 8.3, y = coef(m)[1] + coef(m)[2] * 8.3 - .8, label = "reta de\nregressão",
               colour = vermelho, size = 5, lineheight = .9)
  }
  eixos_conceituais(g)
}

fig_hipotese_slope <- function(tipo = "nula") {
  a <- switch(tipo, nula = 4.5, positiva = 1.5, negativa = 7.5)
  b <- switch(tipo, nula = 0, positiva = .6, negativa = -.55)
  titulo <- switch(tipo, nula = "Hipótese nula: β = 0",
                   positiva = "Hipótese alternativa: β > 0",
                   negativa = "Hipótese alternativa: β < 0")
  g <- ggplot() +
    geom_abline(intercept = a, slope = b, colour = vermelho, linewidth = 1.4) +
    annotate("text", x = .25, y = a + .45, label = "α", size = 7, fontface = "bold", hjust = 0) +
    labs(title = titulo) + theme(plot.title = element_text(hjust = .5))
  eixos_conceituais(g) + theme(plot.title = element_text(hjust = .5, size = 18))
}

fig_hipoteses_slope <- function() {
  fig_hipotese_slope("nula") + fig_hipotese_slope("positiva") + fig_hipotese_slope("negativa") +
    fundo_papel
}

# A reta de mínimos quadrados passa pelo ponto (média de X, média de Y).
fig_media_ponto <- function(d = pontos_dispersao()) {
  m <- lm(y ~ x, d)
  xb <- mean(d$x); yb <- mean(d$y)
  i <- which.max(resid(m))
  pred <- fitted(m)[i]
  g <- ggplot(d, aes(x, y)) +
    annotate("segment", x = xb, xend = xb, y = 0, yend = yb, linetype = "22", colour = cores[["neutro"]]) +
    annotate("segment", x = 0, xend = xb, y = yb, yend = yb, linetype = "22", colour = cores[["neutro"]]) +
    geom_abline(intercept = coef(m)[1], slope = coef(m)[2], colour = vermelho, linewidth = 1.3) +
    annotate("point", x = xb, y = yb, size = 5, shape = 21, fill = "white", colour = vermelho, stroke = 1.5) +
    annotate("segment", x = d$x[i] - .15, xend = d$x[i] - .15, y = pred, yend = d$y[i],
             colour = cores[["verde"]], linewidth = 1.2,
             arrow = arrow(ends = "both", length = unit(.15, "cm"))) +
    annotate("text", x = d$x[i] - .3, y = (pred + d$y[i]) / 2 + .25, hjust = 1, size = 4.6,
             label = "resíduo:", colour = cores[["verde"]]) +
    annotate("text", x = d$x[i] - .3, y = (pred + d$y[i]) / 2 - .25, hjust = 1, size = 4.6,
             label = "y[i] - hat(y)[i]", parse = TRUE, colour = cores[["verde"]]) +
    geom_point(colour = azul_ponto, size = 3) +
    annotate("label", x = xb + .2, y = yb - .7, hjust = 0, size = 4.6, fill = cores[["papel"]],
             linewidth = 0, label = "(média de X, média de Y)")
  eixos_conceituais(g)
}

# Resíduo = observado − predito, para um ponto de cada vez.
fig_residuo_obs <- function(qual = 0, d = pontos_dispersao()) {
  m <- lm(y ~ x, d)
  d$pred <- fitted(m)
  ids <- c(11, 6, 2)   # pontos destacados, um por etapa
  g <- ggplot(d, aes(x, y)) +
    geom_abline(intercept = coef(m)[1], slope = coef(m)[2], colour = vermelho, linewidth = 1.3) +
    geom_point(colour = azul_ponto, size = 3)
  if (qual == 0) {
    i <- ids[1]
    g <- g + annotate("point", x = d$x[i], y = d$y[i], size = 6, shape = 21, colour = cores[["verde"]], stroke = 1.5) +
      annotate("text", x = d$x[i] + .3, y = d$y[i] + .5, label = "Observação", hjust = 0,
               colour = cores[["verde"]], size = 5)
  } else {
    i <- ids[qual]
    g <- g + annotate("segment", x = d$x[i], xend = d$x[i], y = d$pred[i], yend = d$y[i],
                      colour = cores[["verde"]], linewidth = 1.4, linetype = "22") +
      annotate("label", x = d$x[i] + ifelse(d$x[i] > 5, -.25, .25), y = (d$y[i] + d$pred[i]) / 2,
               hjust = ifelse(d$x[i] > 5, 1, 0), size = 4.6,
               label = "resíduo = observado − predito", fill = cores[["papel"]], linewidth = 0,
               colour = cores[["verde"]])
  }
  eixos_conceituais(g)
}

# Os quadrados dos resíduos, desenhados como quadrados de verdade: uma reta
# qualquer e a reta de mínimos quadrados.
fig_quadrados <- function(d = pontos_dispersao()) {
  m <- lm(y ~ x, d)
  painel <- function(a, b, titulo) {
    q <- d |> mutate(pred = a + b * x, e = y - pred)
    soma <- sum(q$e^2)
    ggplot(q, aes(x, y)) +
      geom_rect(aes(xmin = x, xmax = x + abs(e), ymin = pmin(y, pred), ymax = pmax(y, pred)),
                fill = vermelho, alpha = .25, colour = vermelho, linewidth = .3) +
      geom_abline(intercept = a, slope = b, colour = cores[["texto"]], linewidth = 1.2) +
      geom_point(colour = azul_ponto, size = 2.6) +
      coord_equal(xlim = c(0, 11.5), ylim = c(0, 9), expand = FALSE) +
      labs(title = titulo, subtitle = sprintf("Soma dos quadrados dos resíduos = %s", num_br(soma, 1)),
           x = "X", y = "Y") +
      tema_aula(14, conceitual = TRUE) +
      theme(plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5))
  }
  painel(5.5, .1, "Uma reta qualquer") +
    painel(coef(m)[1], coef(m)[2], "Reta de mínimos quadrados") + fundo_papel
}

# Distâncias minimizadas por três tipos de regressão.
fig_ols_ma_rma <- function() {
  b <- .7
  p <- c(3, 6.5)                      # um ponto acima da reta y = b x
  py <- b * p[1]                      # predito na vertical (OLS)
  px <- p[2] / b                      # ponto da reta na horizontal
  # projeção perpendicular (MA)
  t <- (p[1] + b * p[2]) / (1 + b^2)
  ggplot() +
    geom_abline(intercept = 0, slope = b, linewidth = 1.2, colour = cores[["texto"]]) +
    annotate("polygon", x = c(p[1], p[1], px), y = c(p[2], py, p[2]), fill = "grey70", alpha = .6) +
    annotate("segment", x = p[1], xend = p[1], y = p[2], yend = py, colour = vermelho, linewidth = 1.3) +
    annotate("segment", x = p[1], xend = t, y = p[2], yend = b * t, colour = cores[["accent"]], linewidth = 1.3) +
    annotate("point", x = p[1], y = p[2], size = 4) +
    annotate("text", x = p[1] - .2, y = (p[2] + py) / 2, label = "1. OLS", hjust = 1, colour = vermelho, size = 5) +
    annotate("text", x = (p[1] + t) / 2 + .15, y = (p[2] + b * t) / 2 + .35, label = "2. MA", hjust = 0,
             colour = cores[["accent"]], size = 5) +
    annotate("text", x = (p[1] + px) / 2 + .9, y = p[2] + .35, label = "3. RMA (área)", size = 5) +
    coord_equal(xlim = c(0, 11), ylim = c(0, 8.5), expand = FALSE) +
    labs(x = "X", y = "Y") +
    tema_aula(15, conceitual = TRUE)
}

# ---------------------------------------------------------------------------
# A população de Y para cada valor de X é normal, com a mesma variância.
# ---------------------------------------------------------------------------
fig_normal_x <- function(a = 1.5, b = .55, dp = .9) {
  xs <- c(2.5, 5, 7.5)
  curvas <- purrr::map_dfr(xs, \(x0) {
    yy <- seq(-3, 3, by = .05) * dp
    tibble::tibble(x0 = x0, y = a + b * x0 + yy, x = x0 + 1.6 * dnorm(yy, 0, dp) / dnorm(0, 0, dp))
  })
  ggplot() +
    geom_abline(intercept = a, slope = b, colour = vermelho, linewidth = 1.3) +
    geom_segment(data = tibble::tibble(x0 = xs), aes(x = x0, xend = x0, y = 0, yend = 8.4),
                 colour = cores[["neutro"]], linetype = "22") +
    geom_path(data = curvas, aes(x, y, group = x0), linewidth = 1.1) +
    geom_point(data = tibble::tibble(x = xs, y = a + b * xs), aes(x, y), size = 3) +
    annotate("text", x = xs[3] + .1, y = a + b * xs[3] - .35, hjust = 0, size = 4.6,
             label = "μ de Y para este X") +
    annotate("text", x = 1, y = 7.8, hjust = 0, size = 5, label = "mu[Y*\"|\"*X] == beta[0] + beta[1]*X", parse = TRUE, colour = vermelho) +
    scale_x_continuous(breaks = xs, labels = expression(x[1], x[2], x[3])) +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 8.5), expand = FALSE) +
    labs(x = "X", y = "Y") +
    tema_aula(16) +
    theme(panel.grid = element_blank(), axis.text.y = element_blank(),
          axis.line = element_line(colour = cores[["texto"]]))
}

# ---------------------------------------------------------------------------
# A estatística t do slope de Galton, longe da distribuição nula.
# ---------------------------------------------------------------------------
fig_t_galton <- function(d = galton) {
  tobs <- summary(lm(child ~ parent, data = d))$coefficients["parent", "t value"]
  gl <- nrow(d) - 2
  x <- seq(-4, 17, by = .02)
  dd <- tibble::tibble(x = x, y = dt(x, gl))
  crit <- qt(.975, gl)
  ggplot(dd, aes(x, y)) +
    geom_ribbon(data = filter(dd, x >= crit), aes(ymin = 0, ymax = y), fill = "#7fb7d6") +
    geom_ribbon(data = filter(dd, x <= -crit), aes(ymin = 0, ymax = y), fill = "#7fb7d6") +
    geom_line(linewidth = 1.1) +
    annotate("segment", x = tobs, xend = tobs, y = .22, yend = 0, colour = vermelho, linewidth = 1.4,
             arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
    annotate("label", x = tobs, y = .25, label = sprintf("t = %s", num_br(tobs, 3)),
             colour = vermelho, fill = cores[["papel"]], fontface = "bold", size = 5) +
    annotate("text", x = 5.5, y = .3, hjust = 0, size = 4.6,
             label = sprintf("Distribuição t com %d gl\n(valores críticos: ±%s)", gl, num_br(crit, 2))) +
    labs(x = "t", y = NULL) +
    tema_aula(15) + theme(axis.text.y = element_blank(), panel.grid = element_blank())
}

# R² como fatia da variação total.
fig_r2_pizza <- function(d = galton) {
  r2 <- summary(lm(child ~ parent, data = d))$r.squared
  p <- tibble::tibble(parte = factor(c("Explicada pela altura dos pais", "Não explicada"),
                                     levels = c("Explicada pela altura dos pais", "Não explicada")),
                      valor = c(r2, 1 - r2))
  ggplot(p, aes(x = 1, y = valor, fill = parte)) +
    geom_col(width = 1, colour = "white") +
    geom_text(aes(label = sprintf("%s%%", num_br(100 * valor, 1))), position = position_stack(vjust = .5),
              size = 6, colour = c("white", cores[["texto"]])) +
    coord_polar(theta = "y", direction = -1) +
    scale_fill_manual(values = c(cores[["verde"]], cores[["neutro_cl"]]), name = NULL) +
    theme_void(base_size = 15) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          legend.position = "right")
}

# ---------------------------------------------------------------------------
# Padrões de resíduos contra valores ajustados.
# ---------------------------------------------------------------------------
fig_padroes_residuos <- function(seed = 5, n = 40) {
  set.seed(seed)
  ajust <- sort(runif(n, 0, 10))
  d <- bind_rows(
    tibble::tibble(painel = "A. Normal", ajust = ajust, e = rnorm(n)),
    tibble::tibble(painel = "B. Não linear", ajust = ajust, e = 1.2 - .012 * ajust^3 + rnorm(n, 0, .4)),
    tibble::tibble(painel = "C. Quadrático", ajust = ajust, e = .18 * (ajust - 5)^2 - 1.5 + rnorm(n, 0, .5)),
    tibble::tibble(painel = "D. Heterogeneidade de variância", ajust = ajust, e = rnorm(n, 0, .1 + .25 * ajust))
  )
  ggplot(d, aes(ajust, e)) +
    geom_hline(yintercept = 0, linetype = "22", colour = cores[["accent"]]) +
    geom_point(size = 2, colour = cores[["texto"]]) +
    facet_wrap(~ painel, nrow = 1, scales = "free_y") +
    labs(x = expression(paste("Valores ajustados (", hat(y), ")")), y = "Resíduos") +
    tema_aula(13, conceitual = TRUE)
}

# ---------------------------------------------------------------------------
# Regressão polinomial (quadrática) contra a linear.
# ---------------------------------------------------------------------------
fig_polinomial <- function(seed = 3, n = 14) {
  set.seed(seed)
  d <- tibble::tibble(x = sort(runif(n, 5, 240))) |>
    mutate(y = pmax(2, .0025 * x^2 + .25 * x + rnorm(n, 0, 9)))
  ggplot(d, aes(x, y)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = cores[["texto"]], linewidth = 1) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, colour = vermelho,
                linetype = "42", linewidth = 1) +
    geom_point(size = 3, shape = 21, fill = "white", colour = cores[["texto"]]) +
    annotate("text", x = 15, y = max(d$y) * .95, hjust = 0, size = 4.6,
             label = "linha contínua: linear\ntracejada: quadrática", lineheight = .9) +
    labs(x = "Riqueza regional de espécies", y = "Riqueza local de espécies") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Árvore de regressão e análise de rota (diagramas com valores fictícios).
# ---------------------------------------------------------------------------
fig_arvore <- function() {
  folha <- cores[["verde"]]; no <- cores[["accent"]]
  nos <- tibble::tribble(
    ~id, ~rotulo,                             ~x,   ~y,  ~cor,
    "R", "Pastoreio < 4,5?",                   4,    3,   no,
    "A", "log10 área < 1,15?",                 2,    1.8, no,
    "B", "Abundância média\n5,6 (n = 13)",      6.2,  1.8, folha,
    "C", "Abundância média\n15,2 (n = 18)",     .8,   .5,  folha,
    "D", "Abundância média\n29,8 (n = 19)",     3.2,  .5,  folha
  )
  arestas <- tibble::tribble(
    ~de, ~para, ~rotulo,
    "R", "A",   "sim",
    "R", "B",   "não",
    "A", "C",   "sim",
    "A", "D",   "não"
  )
  fig_diagrama(nos, arestas, larg = 2.1, alt = .75, base = 4.8, xlim = c(-.4, 7.4), ylim = c(0, 3.5))
}

fig_rota <- function() {
  pred <- cores[["accent"]]
  nos <- tibble::tribble(
    ~id, ~rotulo,                         ~x,  ~y,  ~cor,
    "A", "log10 área\ndo fragmento",       0,   2.6, pred,
    "P", "Pastoreio",                      0,   1.3, pred,
    "I", "Anos desde\no isolamento",       0,   0,   pred,
    "Y", "Abundância de\naves florestais", 3.6, 1.3, cores[["verde"]],
    "U", "U (não explicado)",              3.6, 2.9, cores[["neutro"]]
  )
  arestas <- tibble::tribble(
    ~de, ~para, ~rotulo, ~tracejada, ~curva, ~lado,   ~lx,  ~ly,
    "A", "Y",   NA,      FALSE,      0,      "baixo", NA,   NA,
    "P", "Y",   NA,      FALSE,      0,      "baixo", NA,   NA,
    "I", "Y",   NA,      FALSE,      0,      "baixo", NA,   NA,
    "U", "Y",   NA,      FALSE,      0,      "baixo", NA,   NA
  )
  g <- fig_diagrama(nos, arestas, larg = 1.75, alt = .75, base = 4.6, xlim = c(-1.6, 4.6), ylim = c(-.5, 3.4))
  # correlações entre os preditores: arcos de duas pontas à esquerda
  arcos <- tibble::tibble(y0 = c(2.6, 1.3, 2.6), y1 = c(1.3, 0, 0), curv = c(.5, .5, .6))
  for (i in seq_len(nrow(arcos)))
    g <- g + annotate("curve", x = -.9, xend = -.9, y = arcos$y0[i], yend = arcos$y1[i],
                      curvature = arcos$curv[i], colour = cores[["neutro"]], linewidth = .9,
                      arrow = arrow(ends = "both", length = unit(.22, "cm"), type = "closed"))
  g
}
