# Aula 08: teste t e introdução aos modelos lineares. Todos os dados são
# simulados; cada função que sorteia fixa a própria semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/aula02_pergunta.R"))   # fig_diagrama()

vermelho <- "#c0392b"
num_br <- function(v, d = 2) format(round(v, d), nsmall = d, decimal.mark = ",")
fundo_papel <- plot_annotation(theme = theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA)))

# Amostra normal com média e desvio-padrão exatamente iguais aos pedidos.
rnorm_exata <- function(n, media, dp) {
  x <- rnorm(n)
  media + dp * (x - mean(x)) / sd(x)
}

# ---------------------------------------------------------------------------
# A distribuição t para vários graus de liberdade, comparada à normal.
# ---------------------------------------------------------------------------
fig_t_gl <- function(gls = c(1, 3, 10)) {
  x <- seq(-4.5, 4.5, by = .02)
  d <- bind_rows(
    purrr::map_dfr(gls, \(g) tibble::tibble(x = x, y = dt(x, g), curva = sprintf("t, %d gl", g))),
    tibble::tibble(x = x, y = dnorm(x), curva = "Normal (t com ∞ gl)")
  ) |> mutate(curva = factor(curva, levels = c(sprintf("t, %d gl", gls), "Normal (t com ∞ gl)")))
  ggplot(d, aes(x, y, colour = curva, linetype = curva)) +
    geom_line(linewidth = 1.1) +
    scale_colour_manual(values = c(cores_niveis, cores[["texto"]]), name = NULL) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "22"), name = NULL) +
    labs(x = "t", y = "Densidade") +
    tema_aula(15) + theme(legend.position = "right")
}

# ---------------------------------------------------------------------------
# Lagartos-de-chifre: comprimento do chifre de mortos (por picanços) e vivos.
# Médias, desvios e tamanhos do exemplo 12.3 de Whitlock & Schluter.
# ---------------------------------------------------------------------------
lagartos_resumo <- tibble::tibble(
  grupo = c("vivos", "mortos"), media = c(24.28, 21.99), dp = c(2.63, 2.71), n = c(154, 30)
)

dados_lagartos <- function(seed = 5) {
  set.seed(seed)
  purrr::pmap_dfr(lagartos_resumo, \(grupo, media, dp, n)
    tibble::tibble(grupo = grupo, chifre = rnorm_exata(n, media, dp))) |>
    mutate(grupo = factor(grupo, levels = c("vivos", "mortos")))
}

fig_lagartos <- function(d = dados_lagartos()) {
  ggplot(d, aes(chifre)) +
    geom_histogram(binwidth = 2, boundary = 0, fill = cores[["ocre"]], colour = "white") +
    facet_wrap(~ grupo, ncol = 1, scales = "free_y",
               labeller = as_labeller(c(vivos = "Vivos (n = 154)", mortos = "Mortos (n = 30)"))) +
    labs(x = "Comprimento do chifre (mm)", y = "Frequência") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Amostras independentes vs. pareadas: cada quadrado é uma unidade amostral,
# cada cor um tratamento.
# ---------------------------------------------------------------------------
fig_independente_pareado <- function(seed = 2) {
  set.seed(seed)
  # posições sorteadas numa grade, para os quadrados não se sobreporem
  grade <- tidyr::expand_grid(x = seq(.5, 9.5, by = 1.8), y = seq(.5, 9.5, by = 1.8))
  ind <- grade[sample(nrow(grade), 16), ] |>
    mutate(x = x + runif(16, -.4, .4), y = y + runif(16, -.4, .4), trat = rep(c("A", "B"), 8))
  par <- grade[sample(nrow(grade), 8), ] |>
    mutate(x = x + runif(8, -.3, .3), y = y + runif(8, -.4, .4))
  par2 <- bind_rows(mutate(par, trat = "A", x = x - .3), mutate(par, trat = "B", x = x + .3))
  painel <- function(d, tit, sub) {
    ggplot(d, aes(x, y, fill = trat)) +
      geom_tile(width = .55, height = .55, colour = cores[["texto"]]) +
      scale_fill_manual(values = c(A = "#e6c35c", B = vermelho), name = "Tratamento") +
      coord_equal(xlim = c(-.5, 10.5), ylim = c(-.5, 10.5)) +
      labs(title = tit, subtitle = sub) +
      theme_void(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = .5),
            plot.subtitle = element_text(hjust = .5, colour = "#4a5568", size = 12),
            panel.border = element_rect(fill = NA, colour = cores[["neutro"]]),
            plot.background = element_rect(fill = cores[["papel"]], colour = NA),
            legend.position = "bottom")
  }
  painel(ind, "Independentes", "cada unidade recebe um tratamento;\ncompara-se a média dos dois grupos") +
    painel(par2, "Pareadas", "cada unidade recebe os dois tratamentos;\nmede-se a diferença dentro de cada unidade") +
    plot_layout(guides = "collect") + fundo_papel & theme(legend.position = "bottom")
}

# ---------------------------------------------------------------------------
# Melros: produção de anticorpos antes e depois do implante de testosterona
# (13 machos; desenho do exemplo 12.2 de Whitlock & Schluter).
# ---------------------------------------------------------------------------
dados_melros <- function(seed = 1) {
  set.seed(seed)
  antes <- rnorm(13, 4.6, .2)
  tibble::tibble(ave = 1:13, antes = round(antes, 2),
                 depois = round(antes + rnorm(13, .05, .2), 2))
}

fig_melros <- function(d = dados_melros()) {
  longo <- pivot_longer(d, c(antes, depois), names_to = "momento", values_to = "anticorpos") |>
    mutate(momento = factor(momento, levels = c("antes", "depois"), labels = c("Antes", "Depois")))
  p1 <- ggplot(longo, aes(momento, anticorpos, group = ave)) +
    geom_line(colour = cores[["neutro"]]) +
    geom_point(size = 2.6, colour = vermelho) +
    labs(x = "Implante de testosterona", y = "Produção de anticorpos\n(ln mOD/min)") +
    tema_aula(14)
  p2 <- ggplot(d, aes(depois - antes)) +
    geom_histogram(binwidth = .1, boundary = 0, fill = vermelho, colour = "white") +
    geom_vline(xintercept = 0, linetype = "22") +
    labs(x = "Diferença (depois − antes)", y = "Frequência") +
    tema_aula(14)
  p1 + p2 + fundo_papel
}

# ---------------------------------------------------------------------------
# Qual teste t usar?
# ---------------------------------------------------------------------------
fig_qual_teste_t <- function() {
  pergunta <- cores[["accent"]]; resposta <- cores[["verde"]]
  nos <- tibble::tribble(
    ~id,  ~rotulo,                                     ~x,   ~y,   ~cor,
    "Q1", "Estou comparando uma\nmédia com um valor?",  4.2,  3,    pergunta,
    "R1", "Teste t para\numa amostra",                  7.6,  3,    resposta,
    "Q2", "Estou comparando\nduas médias?",              .6,  3,    pergunta,
    "Q3", "São dados\npareados?",                        .6,  1.5,  pergunta,
    "R2", "Teste t\npareado",                            .6,  0,    resposta,
    "Q4", "As variâncias são\nhomogêneas?",              4.2,  1.5,  pergunta,
    "R3", "Teste t com\nvariância combinada",            4.2,  0,    resposta,
    "R4", "Teste t de Welch\n(variâncias separadas)",    7.6,  1.5,  resposta
  )
  arestas <- tibble::tribble(
    ~de,  ~para, ~rotulo,
    "Q1", "R1",  "sim",
    "Q1", "Q2",  "não",
    "Q2", "Q3",  "sim",
    "Q3", "R2",  "sim",
    "Q3", "Q4",  "não",
    "Q4", "R3",  "sim",
    "Q4", "R4",  "não"
  )
  fig_diagrama(nos, arestas, larg = 2.5, alt = .9, base = 4.8,
               xlim = c(-.8, 9), ylim = c(-.6, 3.6))
}

# ---------------------------------------------------------------------------
# Equação da reta: intercepto e inclinação.
# ---------------------------------------------------------------------------
fig_reta_parametros <- function(a = 2, b = .6) {
  x0 <- 3
  ggplot() +
    geom_abline(intercept = a, slope = b, colour = cores[["verde"]], linewidth = 1.3) +
    annotate("segment", x = x0, xend = x0 + 1, y = a + b * x0, yend = a + b * x0,
             linetype = "22", colour = cores[["texto"]]) +
    annotate("segment", x = x0 + 1, xend = x0 + 1, y = a + b * x0, yend = a + b * (x0 + 1),
             linetype = "22", colour = cores[["texto"]]) +
    annotate("text", x = x0 + .5, y = a + b * x0 - .25, label = "1 unidade de X", size = 4.6) +
    annotate("text", x = x0 + 1.15, y = a + b * (x0 + .5), label = "β", hjust = 0, size = 7,
             fontface = "bold", colour = vermelho) +
    annotate("segment", x = .25, xend = .25, y = 0, yend = a,
             arrow = arrow(ends = "both", length = unit(.2, "cm")), colour = cores[["accent"]]) +
    annotate("text", x = .45, y = a / 2, label = "α", hjust = 0, size = 7, fontface = "bold",
             colour = cores[["accent"]]) +
    annotate("point", x = 0, y = a, size = 3, colour = cores[["accent"]]) +
    annotate("text", x = 6.5, y = a + b * 6.5 + .45, label = "y = α + βx", size = 6) +
    scale_x_continuous(limits = c(0, 8), breaks = 0, labels = "0", expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 7.5), breaks = NULL, expand = c(0, 0)) +
    labs(x = "X (variável preditora)", y = "Y (variável resposta)") +
    tema_aula(15) +
    theme(panel.grid = element_blank(), axis.line = element_line(colour = cores[["texto"]]))
}

# Dados = modelo + erro: os resíduos são as distâncias verticais até a reta.
fig_residuos <- function(seed = 12, n = 15) {
  set.seed(seed)
  d <- tibble::tibble(x = runif(n, 0, 10), y = 2 + .6 * x + rnorm(n, 0, 1))
  m <- lm(y ~ x, data = d)
  d$ajustado <- fitted(m)
  ggplot(d, aes(x, y)) +
    geom_segment(aes(xend = x, yend = ajustado), colour = vermelho, linewidth = .8) +
    geom_abline(intercept = coef(m)[1], slope = coef(m)[2], colour = cores[["verde"]], linewidth = 1.2) +
    geom_point(size = 3, colour = cores[["texto"]]) +
    annotate("label", x = 8.6, y = 2.4, label = "erro (resíduo) =\ndado − modelo", size = 4.6,
             colour = vermelho, fill = cores[["papel"]], linewidth = 0) +
    labs(x = "X", y = "Y") +
    tema_aula(15, conceitual = TRUE)
}

# ---------------------------------------------------------------------------
# Um fator de dois níveis como dummy: a "reta" liga as duas médias.
# ---------------------------------------------------------------------------
fig_dummy <- function(seed = 6, n = 15) {
  set.seed(seed)
  d <- tibble::tibble(x = rep(0:1, each = n), y = c(rnorm(n, 3.3, 1.3), rnorm(n, 7.5, 1.3)))
  m <- tapply(d$y, d$x, mean)
  ggplot(d, aes(x, y)) +
    geom_hline(yintercept = m, colour = cores[["accent"]], linetype = "22") +
    geom_point(position = position_jitter(width = .04, height = 0, seed = 1), size = 2.4,
               alpha = .7, colour = cores[["texto"]]) +
    annotate("segment", x = 0, xend = 1, y = m[1], yend = m[2], colour = vermelho, linewidth = 1.4) +
    annotate("label", x = c(-.12, 1.12), y = m, hjust = c(1, 0), size = 4.6,
             fill = cores[["papel"]], linewidth = 0, colour = cores[["accent"]],
             label = c(sprintf("α = média do grupo 0\n(%s)", num_br(m[1], 1)),
                       sprintf("α + β = média do grupo 1\n(%s)", num_br(m[2], 1)))) +
    annotate("label", x = .72, y = m[1] + .4, size = 4.6, fill = cores[["papel"]], linewidth = 0,
             colour = vermelho,
             label = sprintf("β = inclinação = diferença\nentre as médias (%s)", num_br(diff(m), 1))) +
    scale_x_continuous(breaks = 0:1, labels = c("0\n(referência)", "1"), limits = c(-.75, 1.75)) +
    labs(x = "Variável dummy", y = "Y") +
    tema_aula(15) + theme(panel.grid.minor = element_blank())
}

# ---------------------------------------------------------------------------
# Teor de sódio de duas marcas de sopa (mg por porção).
# ---------------------------------------------------------------------------
dados_sopa <- function(seed = 9) {
  set.seed(seed)
  tibble::tibble(marca = factor(rep(c("A", "B"), each = 10)),
                 sodio = round(c(rnorm(10, 980, 60), rnorm(10, 900, 60))))
}
