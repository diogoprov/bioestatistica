# Aula 01: figuras da introdução. Esquemas redesenhados dos slides originais
# e dados simulados; cada função fixa a própria semente.

library(ggplot2)
library(dplyr)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/aula02_pergunta.R"))   # fig_diagrama()

# ---------------------------------------------------------------------------
# Ciclo do método científico. `extra = "teoria"` acrescenta a teoria
# científica alimentando observação e hipótese; `extra = "estatistica"`, a
# estatística alimentando experimento e análise (slides originais 23 e 24).
# ---------------------------------------------------------------------------
fig_metodo_cientifico <- function(extra = c("nenhum", "teoria", "estatistica")) {
  extra <- match.arg(extra)
  etapas <- tibble::tibble(
    id  = c("Observação", "Hipótese", "Experimento", "Análise", "Conclusão"),
    cor = c("#d98a3d", "#8a8172", "#e6c35c", cores[["accent"]], cores[["verde"]]),
    ang = seq(90, 90 - 4 * 72, by = -72) * pi / 180
  ) |>
    mutate(x = 1.25 * cos(ang), y = sin(ang))
  r_no <- .36
  # círculos como polígonos, como em ppdac_plot(), para não depender de ggforce
  t <- seq(0, 2 * pi, length.out = 121)
  circulos <- purrr::pmap_dfr(list(etapas$id, etapas$x, etapas$y), function(id, x, y)
    tibble::tibble(id = id, cx = x + r_no * cos(t), cy = y + r_no * sin(t)))
  n <- nrow(etapas)
  setas <- tibble::tibble(x0 = etapas$x, y0 = etapas$y,
                          x1 = etapas$x[c(2:n, 1)], y1 = etapas$y[c(2:n, 1)]) |>
    mutate(dx = x1 - x0, dy = y1 - y0, L = sqrt(dx^2 + dy^2),
           xa = x0 + dx / L * (r_no + .05), ya = y0 + dy / L * (r_no + .05),
           xb = x1 - dx / L * (r_no + .1), yb = y1 - dy / L * (r_no + .1))

  g <- ggplot() +
    geom_segment(data = setas, aes(xa, ya, xend = xb, yend = yb),
                 linewidth = 1.6, colour = cores[["neutro"]],
                 arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
    geom_polygon(data = circulos, aes(cx, cy, group = id, fill = id),
                 colour = "white", linewidth = 1) +
    scale_fill_manual(values = setNames(etapas$cor, etapas$id), guide = "none") +
    geom_text(data = etapas, aes(x, y, label = id), colour = "white",
              fontface = "bold", size = 4) +
    annotate("label", x = 0, y = .05, label = "Método\ncientífico",
             fill = cores[["texto"]], colour = "white", fontface = "bold",
             size = 5.4, label.padding = unit(.6, "lines"), linewidth = 0)

  # cx, cy: posição da nuvem (nomes diferentes das colunas x, y de `etapas`)
  nuvem <- function(g, cx, cy, rotulo, cor, alvos) {
    alvo <- filter(etapas, id %in% alvos) |>
      mutate(dx = cx - x, dy = cy - y, L = sqrt(dx^2 + dy^2),
             xa = cx - dx / L * .45, ya = cy - dy / L * .3,
             xb = x + dx / L * (r_no + .1), yb = y + dy / L * (r_no + .1))
    g +
      geom_segment(data = alvo, aes(x = xa, y = ya, xend = xb, yend = yb),
                   inherit.aes = FALSE, linewidth = 1.6, colour = cor,
                   arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
      annotate("label", x = cx, y = cy, label = rotulo, fill = cor,
               colour = "white", fontface = "bold", size = 4.6,
               label.padding = unit(.55, "lines"), linewidth = 0)
  }
  if (extra == "teoria")
    g <- nuvem(g, 2.35, 1.05, "Teoria\ncientífica", "#c0392b",
               c("Observação", "Hipótese"))
  if (extra == "estatistica")
    g <- nuvem(g, 0, -1.75, "Estatística", "#8e44ad",
               c("Análise", "Experimento"))

  g +
    coord_equal(xlim = c(-2.9, 2.9), ylim = c(-2, 1.45), clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# "Como realmente é": uma previsão traz junto a sua incerteza. Série e
# cenários simulados, só para ilustrar a forma de uma projeção; não são
# dados de clima.
# ---------------------------------------------------------------------------
fig_previsao <- function(seed = 3) {
  set.seed(seed)
  obs <- tibble::tibble(ano = 1950:2024) |>
    mutate(y = .00018 * (ano - 1950)^2 + as.numeric(arima.sim(list(ar = .5), length(ano), sd = .08)))
  ult <- tail(obs$y, 1)
  cen <- tidyr::expand_grid(
    cenario = factor(c("Emissões altas", "Emissões médias", "Emissões baixas"),
                     c("Emissões altas", "Emissões médias", "Emissões baixas")),
    ano = 2024:2100) |>
    mutate(t = ano - 2024,
           taxa = c(.045, .022, .006)[as.integer(cenario)],
           y = ult + taxa * t,
           larg = .08 + c(.012, .009, .006)[as.integer(cenario)] * t)
  cor_c <- c(`Emissões altas` = cores[["ferrugem"]], `Emissões médias` = "#d98a3d",
             `Emissões baixas` = cores[["accent"]])
  ggplot() +
    geom_ribbon(data = cen, aes(ano, ymin = y - larg, ymax = y + larg,
                                fill = cenario), alpha = .25) +
    geom_line(data = cen, aes(ano, y, colour = cenario), linewidth = 1.2) +
    geom_line(data = obs, aes(ano, y), colour = cores[["texto"]], linewidth = .7) +
    scale_colour_manual(values = cor_c, name = NULL) +
    scale_fill_manual(values = cor_c, name = NULL) +
    labs(x = "Ano", y = "Aumento de temperatura (°C)",
         caption = "Dados simulados, só para mostrar a forma de uma projeção.") +
    tema_aula(14)
}

# ---------------------------------------------------------------------------
# Dois modelos para a mesma população: um ignora a incerteza, o outro a
# incorpora e comunica. Redesenho com dados simulados da Figura I de
# Simmonds et al. (2024).
# ---------------------------------------------------------------------------
fig_incerteza_modelos <- function(seed = 8) {
  set.seed(seed)
  anos <- 0:25
  m1 <- tibble::tibble(ano = anos, y = 2.1 + as.numeric(arima.sim(list(ar = .3), length(anos), sd = .22)),
                       modelo = "Modelo 1")
  m2 <- tibble::tibble(ano = anos, y = 1 + as.numeric(arima.sim(list(ar = .3), length(anos), sd = .12)),
                       modelo = "Modelo 2") |>
    mutate(li = y - .35 - .01 * ano, ls = y + .35 + .01 * ano)
  cor_m <- c(`Modelo 1` = "#2b4fd8", `Modelo 2` = "#8e44ad")
  ggplot() +
    geom_ribbon(data = m2, aes(ano, ymin = li, ymax = ls), fill = cor_m[["Modelo 2"]],
                alpha = .3) +
    geom_hline(yintercept = 1, linetype = "22", colour = cores[["texto"]]) +
    geom_line(data = bind_rows(m1, select(m2, ano, y, modelo)),
              aes(ano, y, colour = modelo), linewidth = 1.6) +
    annotate("point", x = 22, y = 1.45, colour = "#d98a3d", size = 4) +
    annotate("text", x = 25.6, y = c(2.1, 1), hjust = 0, size = 4.6,
             lineheight = .95, colour = cores[["texto"]],
             label = c("Não incorpora\nincerteza", "Incorpora e\ncomunica incerteza")) +
    scale_colour_manual(values = cor_m, name = NULL) +
    scale_x_continuous(expand = expansion(mult = c(.02, .02))) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 3), clip = "off") +
    labs(x = "Ano", y = "Taxa de crescimento\npopulacional",
         caption = "Tracejado: a taxa média verdadeira, igual nos dois modelos. Ponto laranja: uma observação.") +
    tema_aula(14) +
    theme(plot.margin = margin(5, 150, 5, 5), legend.position = "top")
}

# ---------------------------------------------------------------------------
# População e amostra: amostragem num sentido, inferência no outro.
# ---------------------------------------------------------------------------
fig_populacao_amostra <- function(seed = 4, n_pop = 400, n_am = 15) {
  set.seed(seed)
  pop <- tibble::tibble(t = runif(n_pop, 0, 2 * pi), r = sqrt(runif(n_pop))) |>
    mutate(x = -1.8 + 1.15 * r * cos(t), y = .35 + .75 * r * sin(t),
           tamanho = rnorm(n_pop, 32, 6))
  am <- slice_sample(pop, n = n_am) |>
    mutate(x2 = 1.8 + runif(n_am, -.55, .55), y2 = -.55 + runif(n_am, -.32, .32))
  num <- function(v) format(round(v, 1), nsmall = 1, decimal.mark = ",")
  lab_pop <- sprintf("População\nμ = %s mm   σ = %s mm",
                     num(mean(pop$tamanho)), num(sd(pop$tamanho)))
  lab_am <- sprintf("Amostra (n = %d)\nx̄ = %s mm   s = %s mm",
                    n_am, num(mean(am$tamanho)), num(sd(am$tamanho)))
  ggplot() +
    geom_point(data = pop, aes(x, y), colour = cores[["accent"]], alpha = .35, size = 1.6) +
    geom_point(data = am, aes(x, y), colour = cores[["ferrugem"]], size = 2.4) +
    annotate("rect", xmin = 1.15, xmax = 2.45, ymin = -.97, ymax = -.13,
             fill = "#f3e3d3", colour = NA) +
    geom_point(data = am, aes(x2, y2), colour = cores[["ferrugem"]], size = 2.6) +
    annotate("curve", x = -1.2, y = -.55, xend = 1.05, yend = -.62,
             curvature = .25, linewidth = 1.4, colour = cores[["texto"]],
             arrow = arrow(length = unit(.35, "cm"), type = "closed")) +
    annotate("curve", x = 1.8, y = .02, xend = -.55, yend = .75,
             curvature = .25, linewidth = 1.4, colour = cores[["texto"]],
             arrow = arrow(length = unit(.35, "cm"), type = "closed")) +
    annotate("text", x = -.05, y = -1.12, label = "Processo de amostragem",
             fontface = "bold", size = 5.2) +
    annotate("text", x = .75, y = 1.12, label = "Processo de inferência",
             fontface = "bold", size = 5.2) +
    annotate("text", x = -1.8, y = 1.32, label = lab_pop, size = 4.8,
             lineheight = 1, colour = cores[["accent_dark"]], fontface = "bold") +
    annotate("text", x = 1.8, y = -1.35, label = lab_am, size = 4.8,
             lineheight = 1, colour = cores[["ferrugem"]], fontface = "bold") +
    coord_equal(xlim = c(-3.1, 2.6), ylim = c(-1.6, 1.6), clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# Tipos de variáveis, em três etapas como nos slides originais 39 a 41.
# ---------------------------------------------------------------------------
fig_tipos_variaveis <- function(etapa = 3) {
  nos <- tibble::tribble(
    ~id,  ~rotulo,                                 ~x,  ~y,    ~cor,                 ~etapa,
    "QT", "Quantitativas\n(numéricas)",              0,  1.6,   cores[["accent"]],    1,
    "QL", "Qualitativas\n(categóricas)",             0, -1.6,   cores[["accent"]],    1,
    "D",  "Discreta\n(contagem)",                  2.6,  2.3,   cores[["verde"]],     2,
    "C",  "Contínua\n(medidas)",                   2.6,  0.9,   "#d98a3d",            2,
    "N",  "Nominal\n(não têm ordem)",              2.6, -0.9,   cores[["verde"]],     3,
    "O",  "Ordinal\n(seguem ordem\nde grandeza)",  2.6, -2.3,   "#d98a3d",            3
  ) |> filter(etapa <= !!etapa)
  arestas <- tibble::tribble(
    ~de,  ~para, ~rotulo,
    "QT", "D",   NA,
    "QT", "C",   NA,
    "QL", "N",   NA,
    "QL", "O",   NA
  ) |> filter(para %in% nos$id)
  exemplos <- tibble::tribble(
    ~id, ~texto,
    "D", "Números inteiros\nEx.: riqueza de espécies, abundância,\nnúmero de patos numa lagoa",
    "C", "Números \"com vírgula\"\nEx.: área, densidade, tamanho, altura",
    "N", "Ex.: gênero, grupo sanguíneo",
    "O", "Ex.: qualidade (bom, mais ou menos, ruim);\naltura (alto, médio, baixo)"
  ) |> inner_join(select(nos, id, x, y), by = "id")
  fig_diagrama(nos, arestas, larg = 1.75, alt = .95, base = 4.6,
               xlim = c(-1, 7.6), ylim = c(-2.85, 2.85)) +
    geom_text(data = exemplos, aes(x + 1.05, y, label = texto), hjust = 0,
              size = 4.4, lineheight = .95, colour = cores[["texto"]])
}

# ---------------------------------------------------------------------------
# Variabilidade: 60 sapos da mesma espécie e população, 60 tamanhos.
# Substitui a foto dos morfos de cor do original (sem licença aberta).
# ---------------------------------------------------------------------------
fig_variabilidade <- function(n = 60, seed = 6) {
  set.seed(seed)
  d <- tibble::tibble(crc = round(rnorm(n, 22, 2.4), 1))
  ggplot(d, aes(crc)) +
    geom_dotplot(binwidth = .4, dotsize = .8, stackratio = 1.1,
                 fill = cores[["ferrugem"]], colour = cores[["papel"]]) +
    scale_y_continuous(NULL, breaks = NULL) +
    scale_x_continuous(breaks = seq(14, 30, 2)) +
    labs(x = "Comprimento rostro-cloacal (mm)",
         title = sprintf("%d sapos da mesma espécie, da mesma lagoa", n)) +
    tema_aula(15)
}
