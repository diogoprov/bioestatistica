# Aula 05: probabilidade, variáveis aleatórias e distribuições. Esquemas
# redesenhados do original e distribuições calculadas em R; cada função que
# sorteia fixa a própria semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))

vermelho <- "#c0392b"
num_br <- function(v, d = 1) format(round(v, d), nsmall = d, decimal.mark = ",")
cor_discreta <- cores[["accent"]]
cor_continua <- cores[["ferrugem"]]

fundo <- theme_void(base_size = 14) +
  theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
        plot.title = element_text(face = "bold", hjust = .5, margin = margin(b = 4)))

# ---------------------------------------------------------------------------
# Diagramas de Venn: união, interseção, complemento e mutuamente excludentes.
# ---------------------------------------------------------------------------
venn <- function(tipo) {
  g <- expand.grid(x = seq(0, 10, by = .05), y = seq(0, 6, by = .05))
  sep <- tipo == "Mutuamente excludentes"
  ca <- c(if (sep) 3 else 4, 3); cb <- c(if (sep) 7 else 6, 3); r <- 1.9
  inA <- (g$x - ca[1])^2 + (g$y - ca[2])^2 <= r^2
  inB <- (g$x - cb[1])^2 + (g$y - cb[2])^2 <= r^2
  g$fill <- switch(tipo,
                   "União" = inA | inB,
                   "Interseção" = inA & inB,
                   "Complemento" = !inA,
                   "Mutuamente excludentes" = FALSE)
  t <- seq(0, 2 * pi, length.out = 200)
  circ <- bind_rows(tibble::tibble(c = "A", x = ca[1] + r * cos(t), y = ca[2] + r * sin(t)),
                    if (tipo != "Complemento") tibble::tibble(c = "B", x = cb[1] + r * cos(t), y = cb[2] + r * sin(t)))
  rot <- tibble::tibble(lab = c("A", if (tipo != "Complemento") "B"),
                        x = c(ca[1] - 1.5, if (tipo != "Complemento") cb[1] + 1.5), y = 4.9)
  ggplot() +
    geom_raster(data = g, aes(x, y, fill = fill)) +
    scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = "#b9c9d3"), guide = "none") +
    geom_path(data = circ, aes(x, y, group = c), colour = cores[["texto"]], linewidth = .8) +
    annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 6, fill = NA, colour = cores[["texto"]]) +
    geom_text(data = rot, aes(x, y, label = lab), fontface = "italic", size = 5.5) +
    annotate("text", x = 9.6, y = .4, label = "S", fontface = "italic", size = 5) +
    labs(title = tipo) + coord_equal() + fundo
}

fig_venn <- function() {
  (venn("União") | venn("Interseção")) / (venn("Complemento") | venn("Mutuamente excludentes"))
}

# Probabilidade condicional: dado B, o espaço amostral encolhe para B.
fig_condicional <- function() {
  g <- expand.grid(x = seq(0, 10, by = .04), y = seq(0, 6, by = .04))
  r <- 2
  inA <- (g$x - 4)^2 + (g$y - 3)^2 <= r^2
  inB <- (g$x - 6)^2 + (g$y - 3)^2 <= r^2
  t <- seq(0, 2 * pi, length.out = 200)
  circ <- function(cx) tibble::tibble(x = cx + r * cos(t), y = 3 + r * sin(t))
  a <- ggplot() +
    geom_raster(data = mutate(g, f = inA & inB), aes(x, y, fill = f)) +
    scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = "#c7b3dd"), guide = "none") +
    geom_path(data = circ(4), aes(x, y), colour = "#2b4fd8", linewidth = 1.1) +
    geom_path(data = circ(6), aes(x, y), colour = vermelho, linewidth = 1.1) +
    annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 6, fill = NA, colour = cores[["texto"]]) +
    annotate("text", x = c(2.4, 7.6, 5, 9.6), y = c(4.9, 4.9, 3, 5.6),
             label = c("A", "B", "A∩B", "Ω"), colour = c("#2b4fd8", vermelho, "#6b4c8a", cores[["texto"]]),
             fontface = "bold.italic", size = c(6, 6, 5, 6)) +
    labs(title = "Espaço amostral completo") + coord_equal() + fundo
  gb <- filter(g, (x - 6)^2 + (y - 3)^2 <= r^2)
  b <- ggplot() +
    geom_raster(data = mutate(gb, f = (x - 4)^2 + (y - 3)^2 <= r^2), aes(x, y, fill = f)) +
    scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = "#c7b3dd"), guide = "none") +
    geom_path(data = circ(6), aes(x, y), colour = vermelho, linewidth = 1.1) +
    annotate("text", x = c(7.6, 5), y = c(4.9, 3), label = c("B", "A∩B"),
             colour = c(vermelho, "#6b4c8a"), fontface = "bold.italic", size = c(6, 5)) +
    labs(title = "Dado que B ocorreu: P(A|B) = P(A∩B) / P(B)") +
    coord_equal(xlim = c(0, 10), ylim = c(0, 6)) + fundo
  a + b
}

# ---------------------------------------------------------------------------
# Rede de relações entre distribuições, com miniaturas. `destaque` realça um
# grupo ("discretas", "continuas") ou um nó, como na sequência original.
# ---------------------------------------------------------------------------
fig_relacoes <- function(destaque = NULL) {
  nos <- tibble::tribble(
    ~id,                 ~x,   ~y,  ~tipo,
    "Uniforme",          1.2,  6,   "d",
    "Bernoulli",         3.6,  6,   "d",
    "Hipergeométrica",   0,    4.8, "d",
    "Binomial",          2.2,  4.8, "d",
    "Geométrica",        4.6,  4.8, "d",
    "Binomial negativa", 6.8,  4.5, "d",
    "Poisson",           2.2,  3.6, "d",
    "Exponencial",       4.6,  3.6, "c",
    "Log-normal",        0,    2.4, "c",
    "Normal",            2.2,  2.4, "c",
    "Qui-quadrado",      4.1,  2.4, "c",
    "Weibull",           6.4,  2.7, "c",
    "t de Student",      0,    1.2, "c",
    "Gama",              4.6,  1.2, "c",
    "Beta",              4.6,  0,   "c") |>
    mutate(x = x * 1.75)   # espalha na horizontal: o slide é largo
  arestas <- tibble::tribble(
    ~de, ~para,
    "Uniforme", "Bernoulli", "Bernoulli", "Binomial", "Bernoulli", "Geométrica",
    "Hipergeométrica", "Binomial", "Binomial", "Geométrica", "Geométrica", "Binomial negativa",
    "Binomial", "Poisson", "Binomial", "Normal", "Poisson", "Normal",
    "Geométrica", "Exponencial", "Exponencial", "Weibull", "Exponencial", "Gama",
    "Normal", "Log-normal", "Normal", "t de Student", "Normal", "Qui-quadrado",
    "Qui-quadrado", "Gama", "Gama", "Beta") |>
    left_join(rename(nos, x0 = x, y0 = y), by = c(de = "id")) |>
    left_join(rename(nos, x1 = x, y1 = y), by = c(para = "id"))
  w <- 1.6; h <- .66
  # miniatura de cada distribuição, desenhada no canto direito da caixa
  forma <- function(id) {
    u <- seq(0, 1, length.out = 40)
    switch(id,
      "Uniforme" = list(d = TRUE, y = rep(.6, 6)),
      "Bernoulli" = list(d = TRUE, y = c(.35, .8)),
      "Hipergeométrica" = list(d = TRUE, y = dbinom(0:6, 6, .5) * 3),
      "Binomial" = list(d = TRUE, y = dbinom(0:6, 6, .4) * 3),
      "Geométrica" = list(d = TRUE, y = dgeom(0:6, .45) * 2),
      "Binomial negativa" = list(d = TRUE, y = dnbinom(0:7, 3, .5) * 4),
      "Poisson" = list(d = TRUE, y = dpois(0:7, 2.5) * 3.5),
      "Exponencial" = list(d = FALSE, y = dexp(u * 4) * .9),
      "Log-normal" = list(d = FALSE, y = dlnorm(u * 3, 0, .5) * 1.1),
      "Normal" = list(d = FALSE, y = dnorm(u * 6 - 3) * 2.2),
      "Qui-quadrado" = list(d = FALSE, y = dchisq(u * 10, 3) * 3.5),
      "Weibull" = list(d = FALSE, y = dweibull(u * 2.5, 2) * 1),
      "t de Student" = list(d = FALSE, y = dt(u * 8 - 4, 2) * 2.4),
      "Gama" = list(d = FALSE, y = dgamma(u * 8, 2) * 2.3),
      "Beta" = list(d = FALSE, y = dbeta(u, 2, 4) * .4))
  }
  barras <- list(); curvas <- list()
  for (i in seq_len(nrow(nos))) {
    # nome no alto da caixa, miniatura embaixo, na largura toda
    f <- forma(nos$id[i]); x0 <- nos$x[i] - w / 2 + .15; x1 <- nos$x[i] + w / 2 - .15
    y0 <- nos$y[i] - h / 2 + .04
    yy <- pmin(f$y, 1) * (h - .3)
    if (f$d) {
      k <- length(yy); xs <- seq(x0, x1, length.out = k); bw <- (x1 - x0) / k * .7
      barras[[i]] <- tibble::tibble(xmin = xs - bw / 2, xmax = xs + bw / 2, ymin = y0, ymax = y0 + yy)
    } else {
      curvas[[i]] <- tibble::tibble(id = nos$id[i], x = seq(x0, x1, length.out = length(yy)), y = y0 + yy)
    }
  }
  # destaque: NULL, "discretas", "continuas" ou um ou mais nomes de distribuições
  grupo <- length(destaque) == 1 && destaque %in% c("discretas", "continuas")
  realce <- if (is.null(destaque)) character(0) else
    if (grupo && destaque == "discretas") nos$id[nos$tipo == "d"] else
      if (grupo) nos$id[nos$tipo == "c"] else destaque
  rot_grupo <- if (grupo)
    c(discretas = "Variáveis aleatórias discretas", continuas = "Variáveis aleatórias contínuas")[[destaque]]
  nos <- mutate(nos, realce = id %in% realce)
  g <- ggplot() +
    geom_segment(data = arestas, aes(x0, y0, xend = x1, yend = y1), colour = "#b8b0a0", linewidth = .6) +
    geom_tile(data = nos, aes(x, y, colour = realce), width = w, height = h,
              fill = "#eef4f7", linewidth = ifelse(nos$realce, 1.4, .4)) +
    scale_colour_manual(values = c(`TRUE` = vermelho, `FALSE` = "#c9d6dd"), guide = "none") +
    geom_text(data = nos, aes(x, y + h / 2 - .12, label = id),
              size = 4.2, fontface = "bold", colour = cores[["texto"]]) +
    geom_rect(data = bind_rows(barras), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = cor_discreta) +
    geom_line(data = bind_rows(curvas), aes(x, y, group = id), colour = cor_continua, linewidth = .8)
  if (!is.null(rot_grupo))
    g <- g + annotate("text", x = 12.6, y = .2, label = rot_grupo, colour = vermelho,
                      fontface = "bold", size = 5, hjust = 1)
  if (identical(destaque, "Normal"))
    g <- g + annotate("segment", x = 2.6, y = .55, xend = 3.6, yend = 2.02, colour = vermelho,
                      linewidth = .9, arrow = arrow(length = unit(.25, "cm"), type = "closed")) +
      annotate("text", x = -.8, y = .3, label = "A distribuição mais importante para\ndescrever variáveis aleatórias contínuas",
               colour = vermelho, size = 4.6, hjust = 0, lineheight = .9)
  g + coord_equal(xlim = c(-.85, 12.8), ylim = c(-.3, 6.3)) + fundo
}

# ---------------------------------------------------------------------------
# Bernoulli: função de probabilidade e função de distribuição acumulada.
# ---------------------------------------------------------------------------
fig_bernoulli <- function(p = .7) {
  a <- ggplot(tibble::tibble(x = 0:1, y = c(1 - p, p)), aes(x, y)) +
    geom_segment(aes(xend = x, yend = 0), colour = cor_discreta, linewidth = 1.4) +
    geom_point(size = 4, colour = cor_discreta) +
    scale_x_continuous(breaks = 0:1, labels = c("0\nfracasso", "1\nsucesso"), limits = c(-.5, 1.5)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 1 - p, p, 1), labels = c("0", "1 − p", "p", "1")) +
    labs(x = NULL, y = "P(X = x)", title = "Função de probabilidade") + tema_aula(14)
  st <- tibble::tibble(x0 = c(-.5, 0, 1), x1 = c(0, 1, 1.5), y = c(0, 1 - p, 1))
  b <- ggplot(st) +
    geom_segment(aes(x = x0, xend = x1, y = y, yend = y), colour = cor_discreta, linewidth = 1.4) +
    geom_point(aes(x0, y), data = st[-1, ], size = 3, colour = cor_discreta) +
    scale_x_continuous(breaks = 0:1, limits = c(-.5, 1.5)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 1 - p, 1), labels = c("0", "1 − p", "1")) +
    labs(x = NULL, y = "F(x) = P(X ≤ x)", title = "Função de distribuição acumulada") + tema_aula(14)
  a + b + plot_annotation(caption = sprintf("Exemplo com p = %s: um organismo sobrevive (1) ou não (0) ao tratamento.", num_br(p, 1)))
}

# ---------------------------------------------------------------------------
# Binomial e Poisson: barras da função de probabilidade, com os valores.
# ---------------------------------------------------------------------------
barras_prob <- function(d, cor, xlab, titulo = NULL, destaque = NULL, rotulos = TRUE) {
  d <- mutate(d, dest = !is.null(destaque) & k %in% destaque)
  g <- ggplot(d, aes(k, p, fill = dest)) +
    geom_col(width = .7) +
    scale_fill_manual(values = c(`FALSE` = cor, `TRUE` = vermelho), guide = "none") +
    scale_x_continuous(breaks = d$k) +
    scale_y_continuous(expand = expansion(mult = c(0, .12))) +
    labs(x = xlab, y = "Probabilidade", title = titulo) +
    tema_aula(15)
  if (rotulos) g <- g + geom_text(aes(label = num_br(p, 3)), vjust = -.4, size = 4.2)
  g
}

fig_binomial <- function(n, p, xlab, titulo = NULL, destaque = NULL) {
  barras_prob(tibble::tibble(k = 0:n, p = dbinom(0:n, n, p)), cor_discreta, xlab, titulo, destaque)
}

fig_poisson <- function(lambda, kmax, xlab, titulo = NULL, destaque = NULL, rotulos = TRUE) {
  barras_prob(tibble::tibble(k = 0:kmax, p = dpois(0:kmax, lambda)), "#d98a3d", xlab, titulo,
              destaque, rotulos)
}

# Binomial com n grande e p pequeno se parece com a Poisson de λ = np.
fig_binom_poisson <- function(n = 100, prob = .03) {
  # argumentos com nomes diferentes das colunas (k, p) do tibble
  k <- 0:10
  rot_b <- sprintf("Binomial (n = %d, p = %s)", n, num_br(prob, 2))
  rot_p <- sprintf("Poisson (λ = np = %s)", num_br(n * prob, 0))
  d <- bind_rows(tibble::tibble(k = k, p = dbinom(k, n, prob), dist = rot_b),
                 tibble::tibble(k = k, p = dpois(k, n * prob), dist = rot_p))
  ggplot(d, aes(k, p, fill = dist)) +
    geom_col(position = position_dodge(.8), width = .75) +
    scale_fill_manual(values = c(cor_discreta, "#d98a3d"), name = NULL) +
    scale_x_continuous(breaks = k) +
    labs(x = "Número de eventos (k)", y = "Probabilidade") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Distribuição normal.
# ---------------------------------------------------------------------------
fig_normal_empirica <- function(mu = 30, s = 4, xlab = "Comprimento do girino (mm)",
                                quem = "dos girinos", un = "mm") {
  x <- seq(mu - 4 * s, mu + 4 * s, length.out = 400)
  d <- tibble::tibble(x = x, y = dnorm(x, mu, s))
  faixa <- function(k, rot) geom_ribbon(data = filter(d, abs(x - mu) <= k * s) |> mutate(f = rot),
                                        aes(ymin = 0, ymax = y, fill = f))
  # camadas da mais larga para a mais estreita, para nenhuma cobrir as outras
  ggplot(d, aes(x, y)) +
    faixa(3, "±3σ (99,7%)") + faixa(2, "±2σ (95%)") + faixa(1, "±1σ (68%)") +
    geom_line(colour = vermelho, linewidth = 1) +
    scale_fill_manual(values = c("±1σ (68%)" = "#5b9bd5", "±2σ (95%)" = "#9fc5e8",
                                 "±3σ (99,7%)" = "#f4b6c2"),
                      breaks = c("±1σ (68%)", "±2σ (95%)", "±3σ (99,7%)"), name = NULL) +
    scale_x_continuous(breaks = seq(mu - 3 * s, mu + 3 * s, by = s)) +
    labs(x = xlab, y = NULL,
         caption = sprintf("μ = %d %s, σ = %d %s: 68%% %s têm entre %d e %d %s.",
                           mu, un, s, un, quem, mu - s, mu + s, un)) +
    tema_aula(15) + theme(axis.text.y = element_blank(), legend.position = "top")
}

curvas_normais <- function(pars, rotulo) {
  x <- seq(-5.5, 5.5, by = .02)
  purrr::pmap_dfr(pars, \(m, s) tibble::tibble(x = x, y = dnorm(x, m, s), k = rotulo(m, s)))
}

fig_normal_media <- function() {
  d <- curvas_normais(tibble::tibble(m = 0:2, s = 1), \(m, s) paste0("μ = ", m))
  ggplot(d, aes(x, y, colour = k)) + geom_line(linewidth = 1.3) +
    scale_colour_manual(values = c(cores[["accent_dark"]], cores[["accent"]], "#7fb0c2"), name = NULL) +
    scale_x_continuous(breaks = -5:5) + labs(x = "X", y = "Y") +
    tema_aula(15) + theme(axis.text.y = element_blank(), legend.position = "top")
}

fig_normal_variancia <- function() {
  d <- curvas_normais(tibble::tibble(m = 0, s = c(1, 1.5, 2)), \(m, s) paste0("σ = ", num_br(s, 1)))
  ggplot(d, aes(x, y, colour = k)) + geom_line(linewidth = 1.3) +
    scale_colour_manual(values = c(cores[["ferrugem"]], "#d98a3d", "#e6c35c"), name = NULL) +
    scale_x_continuous(breaks = -5:5) + labs(x = "X", y = "Y") +
    tema_aula(15) + theme(axis.text.y = element_blank(), legend.position = "top")
}

fig_t_normal <- function() {
  x <- seq(-5, 5, by = .02)
  d <- bind_rows(tibble::tibble(x = x, y = dnorm(x), k = "Normal padrão N(0, 1)"),
                 purrr::map_dfr(c(2, 5), \(v) tibble::tibble(x = x, y = dt(x, v), k = paste0("t com ", v, " graus de liberdade"))))
  ggplot(d, aes(x, y, colour = k, linetype = k)) + geom_line(linewidth = 1.2) +
    scale_colour_manual(values = c(cores[["texto"]], cores[["ferrugem"]], "#d98a3d"), name = NULL) +
    scale_linetype_manual(values = c("22", "solid", "solid"), name = NULL) +
    labs(x = NULL, y = NULL) +
    tema_aula(15) + theme(axis.text.y = element_blank(), legend.position = "top",
                          legend.direction = "vertical")
}

# Metade da área de cada lado da média.
fig_normal_simetria <- function() {
  x <- seq(-3.5, 3.5, by = .01)
  d <- tibble::tibble(x = x, y = dnorm(x), lado = x > 0)
  ggplot(d, aes(x, y)) +
    geom_ribbon(aes(ymin = 0, ymax = y, fill = lado, group = lado)) +
    geom_line(linewidth = 1) +
    scale_fill_manual(values = c("#d8d2c4", "#a9a194"), guide = "none") +
    annotate("text", x = c(-.6, .6), y = .12, label = "0,5", size = 6) +
    annotate("text", x = 0, y = .43, label = "média = mediana = moda", size = 4.6) +
    labs(x = NULL, y = NULL) +
    tema_aula(15) + theme(axis.text = element_blank(), panel.grid = element_blank())
}

fig_regra_empirica <- function() {
  x <- seq(-3.6, 3.6, by = .01)
  d <- tibble::tibble(x = x, y = dnorm(x), banda = pmin(floor(abs(x)), 3))
  pct <- tibble::tibble(x = c(-3.3, -2.5, -1.5, -.5, .5, 1.5, 2.5, 3.3),
                        lab = c("0,1%", "2,1%", "13,6%", "34,1%", "34,1%", "13,6%", "2,1%", "0,1%"),
                        y = c(.03, .04, .06, .18, .18, .06, .04, .03))
  ggplot(d, aes(x, y)) +
    geom_ribbon(aes(ymin = 0, ymax = y, fill = factor(banda), group = interaction(banda, x > 0))) +
    geom_line(linewidth = .9) +
    scale_fill_manual(values = c("#144f64", "#1c6e8c", "#7fb0c2", "#c8dde5"), guide = "none") +
    geom_label(data = pct, aes(x, y, label = lab), size = 4, linewidth = 0, fill = cores[["papel"]],
               colour = cores[["texto"]]) +
    scale_x_continuous(breaks = -3:3, labels = c("−3σ", "−2σ", "−1σ", "μ", "1σ", "2σ", "3σ")) +
    labs(x = NULL, y = NULL) +
    tema_aula(15) + theme(axis.text.y = element_blank(), panel.grid = element_blank())
}

fig_altura_z <- function(mu = 30, s = 4, corte = 34, xlab = "Comprimento do girino (mm)") {
  x <- seq(mu - 4 * s, mu + 4 * s, length.out = 400)
  d <- tibble::tibble(x = x, y = dnorm(x, mu, s))
  ggplot(d, aes(x, y)) +
    geom_ribbon(data = filter(d, x >= corte), aes(ymin = 0, ymax = y), fill = vermelho, alpha = .7) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = mu, linetype = "22", colour = cores[["neutro"]]) +
    annotate("text", x = corte + .6 * s, y = dnorm(corte, mu, s) * .55,
             label = sprintf("P(X > %d) ≈ %s%%", corte, num_br(100 * pnorm(corte, mu, s, lower.tail = FALSE), 1)),
             hjust = 0, size = 5, colour = vermelho) +
    scale_x_continuous(breaks = mu + (-3:3) * s,
                       labels = paste0(mu + (-3:3) * s, "\n(Z = ", -3:3, ")")) +
    labs(x = xlab, y = NULL) +
    tema_aula(15) + theme(axis.text.y = element_blank())
}

# Os mesmos indivíduos na escala original e na escala Z.
fig_escore_z <- function(seed = 5, mu = 120, s = 10) {
  set.seed(seed)
  x <- round(rnorm(15, mu, s))
  x[1] <- 100
  d <- bind_rows(tibble::tibble(v = x, escala = "Pressão arterial (mmHg)", dest = seq_along(x) == 1),
                 tibble::tibble(v = (x - mean(x)) / sd(x), escala = "Escore-Z", dest = seq_along(x) == 1)) |>
    mutate(escala = factor(escala, c("Pressão arterial (mmHg)", "Escore-Z")))
  ref <- tibble::tibble(escala = factor(levels(d$escala), levels(d$escala)), v = c(mean(x), 0))
  z1 <- (x[1] - mean(x)) / sd(x)
  ggplot(d, aes(v, 0, colour = dest)) +
    geom_vline(data = ref, aes(xintercept = v), colour = vermelho, linewidth = 1) +
    geom_jitter(width = 0, height = .25, size = 4, alpha = .85) +
    scale_colour_manual(values = c(`FALSE` = cores[["accent"]], `TRUE` = vermelho), guide = "none") +
    facet_wrap(~escala, ncol = 1, scales = "free_x") +
    scale_y_continuous(NULL, breaks = NULL, limits = c(-.5, .5)) +
    labs(x = NULL, caption = sprintf("Média = %s mmHg; desvio-padrão = %s mmHg. O ponto vermelho (100 mmHg) tem Z = (100 − %s) / %s = %s.",
                                     num_br(mean(x), 1), num_br(sd(x), 1), num_br(mean(x), 1), num_br(sd(x), 1), num_br(z1, 2))) +
    tema_aula(15)
}

fig_tres_intervalos <- function() {
  x <- seq(-3.8, 3.8, by = .01)
  d <- purrr::map_dfr(1:3, \(k) tibble::tibble(x = x, y = dnorm(x), k = k,
                                              painel = c("68%", "95%", "99,7%")[k]))
  d$painel <- factor(d$painel, c("68%", "95%", "99,7%"))
  ggplot(d, aes(x, y)) +
    geom_ribbon(data = filter(d, abs(x) <= k), aes(ymin = 0, ymax = y), fill = "#a9a194") +
    geom_line(linewidth = .9) +
    facet_wrap(~painel) +
    scale_x_continuous(breaks = -3:3, labels = c("−3σ", "−2σ", "−1σ", "μ", "1σ", "2σ", "3σ")) +
    labs(x = NULL, y = NULL) +
    tema_aula(14) + theme(axis.text.y = element_blank(), panel.grid = element_blank())
}

# Comparar valores vindos de populações diferentes pelo escore-Z.
fig_comparar_z <- function() {
  pops <- tibble::tibble(pop = c("Lagoa A: μ = 70 mm, σ = 10 mm", "Lagoa B: μ = 140 mm, σ = 20 mm"),
                         mu = c(70, 140), s = c(10, 20), obs = c(80, 160))
  d <- purrr::pmap_dfr(pops, \(pop, mu, s, obs) tibble::tibble(pop = pop, x = seq(mu - 4 * s, mu + 4 * s, length.out = 300)) |>
                         mutate(y = dnorm(x, mu, s)))
  ggplot(d, aes(x, y)) +
    geom_line(linewidth = 1, colour = cores[["texto"]]) +
    geom_vline(data = pops, aes(xintercept = obs), colour = vermelho, linewidth = 1) +
    geom_label(data = pops, aes(obs, dnorm(obs, mu, s) * 1.6,
                                label = sprintf("%d mm\nZ = (%d − %d) / %d = 1", obs, obs, mu, s)),
               colour = vermelho, size = 4.2, linewidth = 0, fill = cores[["papel"]], hjust = -.05) +
    facet_wrap(~pop, scales = "free") +
    labs(x = "Comprimento do girino (mm)", y = NULL) +
    tema_aula(14) + theme(axis.text.y = element_blank())
}

# ---------------------------------------------------------------------------
# Guia de decisão: qual distribuição usar? Com miniaturas nas folhas.
# ---------------------------------------------------------------------------
fig_guia_distribuicao <- function() {
  caixa <- function(x, y, lab, w = 3.4, h = .8, cor = "#d98a3d", fill = "white", txt = cores[["texto"]], tam = 4.3)
    list(annotate("tile", x = x, y = y, width = w, height = h, fill = fill, colour = cor, linewidth = .8),
         annotate("text", x = x, y = y, label = lab, size = tam, colour = txt, lineheight = .95))
  seta <- function(x0, y0, x1, y1) annotate("segment", x = x0, y = y0, xend = x1, yend = y1,
                                             colour = cores[["neutro"]], linewidth = .8,
                                             arrow = arrow(length = unit(.2, "cm"), type = "closed"))
  folha <- function(x, y, lab, cor, xs, ys, discreta) {
    bx <- x + seq(-.9, .9, length.out = length(ys)); escala <- ys / max(ys) * .75
    c(list(annotate("tile", x = x, y = y, width = 2.6, height = 1.7, fill = cor, colour = NA),
           annotate("text", x = x, y = y + .6, label = lab, colour = "white", fontface = "bold", size = 4.4)),
      if (discreta) list(annotate("rect", xmin = bx - .08, xmax = bx + .08, ymin = y - .7, ymax = y - .7 + escala,
                                  fill = "white"))
      else list(annotate("path", x = x + xs, y = y - .7 + escala, colour = "white", linewidth = 1.2)))
  }
  u <- seq(-1.1, 1.1, length.out = 60)
  ggplot() +
    caixa(4.5, 6, "Qual o tipo da sua variável aleatória?", w = 5, cor = cores[["texto"]], tam = 4.8) +
    seta(4.5, 5.6, 2.2, 4.9) + seta(4.5, 5.6, 7.4, 4.9) +
    caixa(2.2, 4.5, "Discreta (contagem)", cor = cor_discreta, txt = cor_discreta) +
    caixa(7.4, 4.5, "Contínua (medição)", cor = cor_continua, txt = cor_continua) +
    seta(2.2, 4.1, 2.2, 3.5) + seta(7.4, 4.1, 7.4, 3.5) +
    caixa(2.2, 3.05, "Número fixo de tentativas, cada\numa com dois resultados?", w = 4.2, h = .85) +
    caixa(7.4, 3.05, "Simétrica, com a maioria dos\nvalores perto da média?", w = 4.2, h = .85) +
    seta(1.4, 2.6, .8, 1.95) + seta(3, 2.6, 3.6, 1.95) + seta(7.4, 2.6, 7.4, 1.95) +
    annotate("text", x = c(.7, 3.7, 7.75), y = 2.35, label = c("sim", "não", "sim"), size = 4.2) +
    folha(.8, 1.05, "Binomial", cor_discreta, NULL, dbinom(0:6, 6, .4), TRUE) +
    folha(3.6, 1.05, "Poisson", "#d98a3d", NULL, dpois(0:7, 2), TRUE) +
    annotate("text", x = 3.6, y = -.05, label = "eventos num intervalo\nde tempo ou espaço", size = 3.6, lineheight = .9) +
    folha(7.4, 1.05, "Normal", "#c9a227", u, dnorm(u * 2.7), FALSE) +
    coord_equal(xlim = c(-.8, 9.9), ylim = c(-.5, 6.5)) + fundo
}

# ---------------------------------------------------------------------------
# Gancho: o acaso de verdade parece agrupado. Uma sequência real de 100
# lançamentos ao lado de uma "inventada", que alterna demais.
# ---------------------------------------------------------------------------
maior_sequencia <- function(v) max(rle(v)$lengths)

fig_moeda_sequencias <- function(seed = 12, n = 100) {
  set.seed(seed)
  real <- sample(c("Ca", "Co"), n, replace = TRUE)
  # inventada: quem inventa evita repetir; aqui, troca de face com prob. 0,75
  # e nunca deixa passar de 3 iguais seguidas
  inv <- character(n); inv[1] <- "Ca"; rep_atual <- 1
  for (i in 2:n) {
    troca <- rep_atual >= 3 || runif(1) < .75
    inv[i] <- if (troca) setdiff(c("Ca", "Co"), inv[i - 1]) else inv[i - 1]
    rep_atual <- if (troca) 1 else rep_atual + 1
  }
  d <- bind_rows(tibble::tibble(i = seq_len(n), face = real, seq = "A"),
                 tibble::tibble(i = seq_len(n), face = inv, seq = "B")) |>
    mutate(linha = (i - 1) %/% 25, col = (i - 1) %% 25)
  rot <- tibble::tibble(seq = c("A", "B"),
                        lab = sprintf("Sequência %s: maior repetição = %d",
                                      c("A", "B"), c(maior_sequencia(real), maior_sequencia(inv))))
  ggplot(d, aes(col, -linha, fill = face)) +
    geom_tile(width = .9, height = .9) +
    scale_fill_manual(values = c(Ca = cores[["accent"]], Co = "#e6e0d3"),
                      labels = c(Ca = "cara", Co = "coroa"), name = NULL) +
    facet_wrap(~seq, ncol = 1, labeller = as_labeller(setNames(rot$lab, rot$seq))) +
    coord_equal() +
    theme_void(base_size = 15) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          strip.text = element_text(face = "bold", margin = margin(b = 4, t = 6)),
          legend.position = "bottom")
}

# A frequência relativa estabiliza: três sequências de lançamentos.
fig_convergencia <- function(seed = 3, lances = 1000) {
  set.seed(seed)
  d <- purrr::map_dfr(1:3, \(k) tibble::tibble(n = seq_len(lances), serie = paste("Sequência", k),
                                              p = cumsum(rbinom(lances, 1, .5)) / seq_len(lances)))
  ggplot(d, aes(n, p, colour = serie)) +
    geom_hline(yintercept = .5, linetype = "22", colour = cores[["texto"]]) +
    geom_line(linewidth = .8) +
    scale_x_log10(breaks = c(1, 10, 100, 1000)) +
    scale_colour_manual(values = c(cores[["accent"]], cores[["ferrugem"]], cores[["verde"]]), name = NULL) +
    labs(x = "Número de lançamentos (escala log)", y = "Proporção de caras") +
    tema_aula(15)
}

# O problema do aniversário.
fig_aniversario <- function(n_turma = 30) {
  d <- tibble::tibble(n = 2:60) |> mutate(p = purrr::map_dbl(n, \(k) pbirthday(k)))
  pt <- filter(d, n == n_turma)
  ggplot(d, aes(n, p)) +
    geom_hline(yintercept = .5, linetype = "22", colour = cores[["neutro"]]) +
    geom_line(colour = cores[["accent"]], linewidth = 1.3) +
    geom_point(data = pt, colour = vermelho, size = 4) +
    annotate("label", x = pt$n + 2, y = pt$p - .08, hjust = 0, size = 5, colour = vermelho,
             linewidth = 0, fill = cores[["papel"]],
             label = sprintf("%d pessoas: %s%%", pt$n, num_br(100 * pt$p, 0))) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
    labs(x = "Número de pessoas na sala",
         y = "P(pelo menos dois\nno mesmo dia)") +
    tema_aula(15)
}

# "O teste que mente": árvore de frequências naturais com 1000 sapos.
fig_arvore_teste <- function(n = 1000, prev = .02, sens = .9, fp = .1) {
  inf <- n * prev; sad <- n - inf
  vp <- inf * sens; fn <- inf - vp; fpos <- sad * fp; vn <- sad - fpos
  nos <- tibble::tibble(
    x = c(5, 2.5, 7.5, 1.3, 3.7, 6.3, 8.7),
    y = c(3, 1.8, 1.8, .5, .5, .5, .5),
    lab = c(sprintf("%d sapos", n),
            sprintf("%g infectados\n(%g%%)", inf, 100 * prev), sprintf("%g sadios", sad),
            sprintf("%g positivos", vp), sprintf("%g negativos", fn),
            sprintf("%g positivos", fpos), sprintf("%g negativos", vn)),
    cor = c(cores[["texto"]], cores[["ferrugem"]], cores[["verde"]],
            vermelho, "#8a8172", vermelho, "#8a8172"))
  lig <- tibble::tibble(x0 = c(5, 5, 2.5, 2.5, 7.5, 7.5), y0 = c(3, 3, 1.8, 1.8, 1.8, 1.8),
                        x1 = c(2.5, 7.5, 1.3, 3.7, 6.3, 8.7), y1 = c(1.8, 1.8, .5, .5, .5, .5),
                        lab = c("2%", "98%", "detecta 90%", "10%", "falso-positivo 10%", "90%"))
  ggplot() +
    geom_segment(data = lig, aes(x0, y0 - .3, xend = x1, yend = y1 + .3), colour = cores[["neutro"]],
                 linewidth = .9) +
    geom_label(data = lig, aes((x0 + x1) / 2, (y0 + y1) / 2, label = lab), size = 4.6,
               linewidth = 0, fill = cores[["papel"]], colour = "#4a5568") +
    geom_label(data = nos, aes(x, y, label = lab, fill = cor), colour = "white", fontface = "bold",
               size = 4.6, lineheight = .9, label.padding = unit(.45, "lines"), linewidth = 0) +
    scale_fill_identity() +
    annotate("text", x = 5, y = -.45, size = 5.4, colour = vermelho, fontface = "bold",
             label = sprintf("Positivos: %g + %g = %g.   P(infectado | positivo) = %g / %g ≈ %s%%",
                             vp, fpos, vp + fpos, vp, vp + fpos, num_br(100 * vp / (vp + fpos), 0))) +
    coord_cartesian(xlim = c(0, 10), ylim = c(-.7, 3.4), clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# Binomial construída por simulação: muitas poças com 6 girinos cada.
fig_binomial_simulada <- function(seed = 4, n = 6, prob = .3, pocas = c(10, 100, 10000)) {
  set.seed(seed)
  d <- purrr::map_dfr(pocas, \(m) tibble::tibble(k = rbinom(m, n, prob), m = m)) |>
    count(m, k) |> group_by(m) |> mutate(f = n / sum(n)) |> ungroup() |>
    mutate(painel = factor(sprintf("%s poças simuladas", format(m, big.mark = ".")),
                           sprintf("%s poças simuladas", format(pocas, big.mark = "."))))
  teo <- tidyr::crossing(painel = levels(d$painel), k = 0:n) |>
    mutate(painel = factor(painel, levels(d$painel)), p = dbinom(k, n, prob))
  ggplot(d, aes(k, f)) +
    geom_col(fill = cor_discreta, width = .7) +
    geom_point(data = teo, aes(k, p), colour = vermelho, size = 2.6) +
    facet_wrap(~painel) +
    scale_x_continuous(breaks = 0:n) +
    labs(x = "Girinos predados por poça (de 6)", y = "Proporção das poças",
         caption = "Barras: simulação. Pontos vermelhos: a fórmula da binomial (n = 6, p = 0,3).") +
    tema_aula(14)
}
