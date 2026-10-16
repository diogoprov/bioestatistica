# Aula 02, partes 5 e 6: estudos observacionais vs. experimentais e os
# delineamentos clássicos. Dados simulados; cada função fixa a semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/aula02_pergunta.R"))   # fig_diagrama()

niveis_n <- c("Controle", "Pouco N", "Muito N")

# ---------------------------------------------------------------------------
# Confundimento: a água no solo afeta o nitrogênio e o crescimento.
# ---------------------------------------------------------------------------
fig_confundimento_dag <- function(mostrar_agua = TRUE) {
  nos <- tibble::tribble(
    ~id, ~rotulo,                          ~x,   ~y,   ~cor,
    "C", "Taxa de crescimento\nper capita", 0,    0,    cores[["accent"]],
    "N", "Nitrogênio\nno solo",             2.8,  0,    cores[["verde"]],
    "A", "Água\nno solo",                   1.4, -1.25, cores[["ferrugem"]]
  )
  arestas <- tibble::tribble(
    ~de, ~para, ~rotulo, ~tracejada,
    "N", "C",   "?",     FALSE,
    "A", "C",   "+",     FALSE,
    "A", "N",   "+",     FALSE
  )
  if (!mostrar_agua) {
    nos <- filter(nos, id != "A")
    arestas <- filter(arestas, de != "A") |> mutate(rotulo = "+")
  }
  fig_diagrama(nos, arestas, larg = 1.5, alt = .62, ylim = c(-1.6, .4))
}

dados_observacional <- function(n = 60, seed = 12) {
  set.seed(seed)
  tibble::tibble(agua = runif(n, 5, 35)) |>
    mutate(nitrogenio = 1 + .08 * agua + rnorm(n, 0, .35),
           # o nitrogênio NÃO tem efeito; só a água
           crescimento = .5 + .05 * agua + rnorm(n, 0, .18),
           classe_agua = cut(agua, c(5, 15, 25, 35), include.lowest = TRUE,
                             labels = c("seco", "intermediário", "úmido")))
}

fig_confundimento_dados <- function(d = dados_observacional()) {
  cor_agua <- c(seco = "#d98a3d", intermediário = "#8a8172",
                úmido = cores[["accent"]])
  a <- ggplot(d, aes(nitrogenio, crescimento)) +
    geom_smooth(method = "lm", se = FALSE, colour = cores[["texto"]],
                linewidth = 1) +
    geom_point(size = 2.8, colour = cores[["neutro"]]) +
    labs(x = "Nitrogênio no solo (mg/g)", y = "Crescimento per capita",
         title = "Todas as parcelas juntas") +
    tema_aula(14)
  b <- ggplot(d, aes(nitrogenio, crescimento, colour = classe_agua)) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    geom_point(size = 2.8) +
    scale_colour_manual(values = cor_agua, name = "Água no solo") +
    labs(x = "Nitrogênio no solo (mg/g)", y = NULL,
         title = "Separando por umidade") +
    tema_aula(14)
  a + b
}

# ---------------------------------------------------------------------------
# O mesmo problema como experimento: 30 parcelas sorteadas entre 3 níveis.
# ---------------------------------------------------------------------------
dados_experimento <- function(seed = 15) {
  set.seed(seed)
  expand.grid(col = 1:10, lin = 1:3) |>
    mutate(agua = 8 + 2.6 * col + rnorm(n(), 0, 1.5),
           trat = factor(sample(rep(niveis_n, 10)), levels = niveis_n),
           efeito = c(0, .12, .35)[as.integer(trat)],
           crescimento = .5 + .02 * agua + efeito + rnorm(n(), 0, .12))
}

fig_experimento_mapa <- function(d = dados_experimento()) {
  fundo <- tibble::tibble(x = seq(.4, 10.6, by = .05))
  a <- ggplot() +
    geom_raster(data = tidyr::crossing(fundo, y = c(.4, 3.6)),
                aes(x, y, fill = x), interpolate = TRUE, alpha = .6) +
    scale_fill_gradient(low = "#e8c99a", high = "#8fb3c4", guide = "none") +
    geom_tile(data = d, aes(col, lin, colour = trat), fill = "white",
              width = .72, height = .72, linewidth = 2.2) +
    geom_text(data = d, aes(col, lin, label = substr(trat, 1, 1)),
              size = 4.6, fontface = "bold", colour = cores[["texto"]]) +
    scale_colour_manual(values = setNames(cores_niveis, niveis_n),
                        name = NULL) +
    annotate("text", x = c(.5, 10.5), y = 3.85, hjust = c(0, 1),
             label = c("solo mais seco", "solo mais úmido"), size = 4.4) +
    coord_equal(clip = "off") +
    theme_void(base_size = 14) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          legend.position = "bottom")
  b <- ggplot(d, aes(trat, agua, colour = trat)) +
    geom_boxplot(width = .5, outlier.shape = NA, fill = NA, linewidth = .8) +
    geom_jitter(width = .1, height = 0, size = 2.2) +
    scale_colour_manual(values = setNames(cores_niveis, niveis_n),
                        guide = "none") +
    labs(x = NULL, y = "Água no solo (%)",
         title = "Água por tratamento") +
    tema_aula(14)
  a + b + plot_layout(widths = c(2.2, 1))
}

fig_experimento_resultado <- function(d = dados_experimento()) {
  r <- d |>
    group_by(trat) |>
    summarise(m = mean(crescimento), n = n(),
              ep = sd(crescimento) / sqrt(n), .groups = "drop") |>
    mutate(li = m - qt(.975, n - 1) * ep, ls = m + qt(.975, n - 1) * ep)
  ggplot(d, aes(trat, crescimento)) +
    geom_jitter(width = .1, height = 0, size = 3.4, shape = 21,
                fill = cores[["accent"]], colour = "white", stroke = .8) +
    geom_pointrange(data = r, aes(y = m, ymin = li, ymax = ls),
                    colour = cores[["accent_dark"]], size = .9,
                    linewidth = 1.4, position = position_nudge(x = .25)) +
    labs(x = NULL, y = "Crescimento per capita") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Desenho de regressão: amplitude e distribuição dos valores de X.
# ---------------------------------------------------------------------------
fig_regressao_amplitude <- function(seed = 4) {
  set.seed(seed)
  pop <- tibble::tibble(x = runif(400, 0, 10)) |>
    mutate(y = 2 + .6 * x + rnorm(400, 0, 1.3))
  amostra <- bind_rows(
    slice_sample(pop, n = 20) |> mutate(p = "X amostrado de 0 a 10"),
    slice_sample(filter(pop, between(x, 4, 5.5)), n = 20) |>
      mutate(p = "X amostrado só entre 4 e 5,5")
  ) |> mutate(p = factor(p, unique(p)))
  ggplot(amostra, aes(x, y)) +
    geom_point(data = select(pop, -any_of("p")), colour = cores[["neutro_cl"]],
               size = 1.4) +
    geom_smooth(method = "lm", se = TRUE, colour = cores[["accent_dark"]],
                fill = cores[["accent"]], alpha = .15, fullrange = FALSE) +
    geom_point(colour = cores[["accent"]], size = 2.8) +
    facet_wrap(~p) +
    labs(x = "Variável preditora (X)", y = "Variável resposta (Y)") +
    tema_aula(14)
}

fig_regressao_uniformidade <- function(seed = 6) {
  set.seed(seed)
  f <- function(x) 2 + .6 * x
  a <- tibble::tibble(x = seq(.5, 9.5, length.out = 20),
                      p = "X espalhado de modo uniforme")
  b <- tibble::tibble(x = c(runif(19, .5, 2.5), 9.5),
                      p = "X agrupado, com um valor isolado")
  d <- bind_rows(a, b) |>
    mutate(y = f(x) + rnorm(n(), 0, 1.3),
           p = factor(p, unique(p)))
  # o ponto isolado cai abaixo da reta verdadeira e arrasta o ajuste
  d$y[nrow(d)] <- f(9.5) - 3.5
  ggplot(d, aes(x, y)) +
    geom_abline(intercept = 2, slope = .6, linetype = "22",
                colour = cores[["neutro"]]) +
    geom_smooth(method = "lm", se = FALSE, colour = cores[["ferrugem"]],
                linewidth = 1.1, fullrange = TRUE) +
    geom_point(colour = cores[["accent"]], size = 2.8) +
    facet_wrap(~p) +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
    labs(x = "Variável preditora (X)", y = "Variável resposta (Y)",
         caption = "Tracejado: relação verdadeira. Linha contínua: reta ajustada à amostra.") +
    tema_aula(14)
}

# ---------------------------------------------------------------------------
# Duas preditoras, X (3 níveis) e X1 (contínua). Quando andam juntas, os
# efeitos se confundem; cruzadas, ou em blocos, eles se separam.
# ---------------------------------------------------------------------------
fig_confusao_x_x1 <- function(seed = 10) {
  set.seed(seed)
  niv <- c("baixo", "médio", "alto")
  efx <- c(baixo = 0, médio = 1, alto = 2)
  y_de <- function(x, x1) 1 + .5 * x1 + efx[x] + rnorm(length(x1), 0, .35)
  a <- tibble::tibble(x = rep(niv, each = 8),
                      x1 = c(runif(8, 0, 3.3), runif(8, 3.3, 6.6),
                             runif(8, 6.6, 10)),
                      p = "Confundidos")
  b <- tibble::tibble(x = rep(niv, 8), x1 = rep(seq(.5, 9.5, length.out = 8),
                                                each = 3),
                      p = "Cruzados")
  c <- tibble::tibble(x = rep(niv, 9),
                      x1 = rep(c(1.7, 5, 8.3), each = 9) + runif(27, -1, 1),
                      bloco = rep(1:3, each = 9),
                      p = "Em blocos")
  d <- bind_rows(a, b, c) |>
    mutate(y = y_de(x, x1), x = factor(x, niv), p = factor(p, unique(p)))
  blocos <- tibble::tibble(p = factor("Em blocos", levels(d$p)),
                           xmin = c(0, 3.3, 6.6) + .1,
                           xmax = c(3.3, 6.6, 10) - .1)
  ggplot(d, aes(x1, y, colour = x)) +
    geom_rect(data = blocos, aes(xmin = xmin, xmax = xmax, ymin = -Inf,
                                 ymax = Inf), inherit.aes = FALSE,
              fill = NA, colour = cores[["neutro"]], linetype = "22") +
    geom_point(size = 2.8) +
    scale_colour_manual(values = setNames(cores_niveis, niv),
                        name = "Nível de X") +
    facet_wrap(~p) +
    labs(x = "X1", y = "Y") +
    tema_aula(14)
}

# ---------------------------------------------------------------------------
# Intercalação (Hurlbert 1984): 12 parcelas ao longo de um gradiente.
# ---------------------------------------------------------------------------
fig_intercalacao <- function(seed = 3) {
  set.seed(seed)
  linha <- function(trat, nome) tibble::tibble(pos = 1:12, trat = trat,
                                              desenho = nome)
  d <- bind_rows(
    linha(sample(rep(niveis_n, 4)), "Completamente aleatorizado"),
    linha(as.vector(replicate(4, sample(niveis_n))), "Blocos aleatorizados"),
    linha(rep(niveis_n, each = 4), "Segregado")
  ) |>
    mutate(trat = factor(trat, niveis_n),
           desenho = factor(desenho, rev(unique(desenho))))
  blocos <- tibble::tibble(desenho = factor("Blocos aleatorizados",
                                            levels(d$desenho)),
                           xmin = c(.55, 3.55, 6.55, 9.55),
                           xmax = c(3.45, 6.45, 9.45, 12.45))
  ggplot(d) +
    geom_raster(data = tidyr::crossing(x = seq(.5, 12.5, by = .05),
                                       desenho = d$desenho |> unique()),
                aes(x, desenho, fill = x), interpolate = TRUE, alpha = .5) +
    scale_fill_gradient(low = "#e8c99a", high = "#8fb3c4", guide = "none") +
    geom_rect(data = blocos,
              aes(xmin = xmin, xmax = xmax,
                  ymin = as.integer(desenho) - .38,
                  ymax = as.integer(desenho) + .38),
              fill = NA, colour = cores[["texto"]], linetype = "22") +
    geom_point(aes(pos, desenho, colour = trat), shape = 15, size = 11) +
    scale_colour_manual(values = setNames(cores_niveis, niveis_n), name = NULL) +
    annotate("text", x = c(.6, 12.4), y = 3.75, hjust = c(0, 1),
             label = c("solo mais seco", "solo mais úmido"), size = 4.4) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL) +
    tema_aula(15) +
    theme(panel.grid = element_blank(), axis.text.x = element_blank(),
          plot.margin = margin(18, 5, 5, 5),
          axis.text.y = element_text(face = "bold", size = 14))
}

# ---------------------------------------------------------------------------
# Blocos aleatorizados num gradiente de solo.
# ---------------------------------------------------------------------------
fig_blocos <- function(seed = 9) {
  set.seed(seed)
  solos <- c("Argiloso", "Rochoso", "Arenoso")
  # cada bloco recebe 2 réplicas de cada nível, em ordem sorteada
  d <- tibble::tibble(bloco = rep(1:3, each = 6), pos = rep(1:6, 3)) |>
    group_by(bloco) |>
    mutate(trat = factor(sample(rep(niveis_n, 2)), niveis_n)) |>
    ungroup() |>
    mutate(x = (bloco - 1) * 4.2 + ((pos - 1) %% 3) * 1.2 + 1,
           y = ifelse(pos <= 3, 1.6, .5))
  rot <- tibble::tibble(bloco = 1:3, x = (0:2) * 4.2 + 2.2, solo = solos)
  ggplot() +
    geom_rect(data = rot, aes(xmin = x - 1.85, xmax = x + 1.85, ymin = -.15,
                              ymax = 2.3, fill = solo), alpha = .9) +
    scale_fill_manual(values = cores_solo, guide = "none") +
    geom_tile(data = d, aes(x, y, colour = trat), fill = "white",
              width = .9, height = .85, linewidth = 2.4) +
    geom_text(data = d, aes(x, y, label = substr(trat, 1, 1)),
              fontface = "bold", size = 5, colour = cores[["texto"]]) +
    scale_colour_manual(values = setNames(cores_niveis, niveis_n), name = NULL) +
    geom_text(data = rot, aes(x, 2.6, label = paste0("Bloco ", bloco, ": solo ",
                                                    tolower(solo))),
              fontface = "bold", size = 5) +
    coord_equal(clip = "off") +
    theme_void(base_size = 14) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          legend.position = "bottom")
}

# ---------------------------------------------------------------------------
# Delineamento aninhado: tratamento > parcela > árvore.
# ---------------------------------------------------------------------------
fig_aninhado <- function() {
  trat <- tibble::tibble(id = c("Pouco N", "Muito N"), x = c(4.5, 13.5), y = 3)
  parc <- tibble::tibble(t = rep(trat$id, each = 3),
                         id = paste0("P", 1:6),
                         x = c(1.5, 4.5, 7.5, 10.5, 13.5, 16.5), y = 2)
  arv <- tibble::tibble(p = rep(parc$id, each = 3),
                        x = rep(parc$x, each = 3) + rep(c(-1, 0, 1), 6), y = 1)
  ar1 <- left_join(parc, rename(trat, xt = x, yt = y), by = c(t = "id"))
  ar2 <- left_join(arv, rename(parc, xp = x, yp = y), by = c(p = "id"))
  niveis <- tibble::tibble(
    y = 3:1, lab = c("Tratamento\n(fixo)", "Parcela\n(aleatório)",
                     "Árvore\n(subamostra)"))
  ggplot() +
    geom_segment(data = ar1, aes(xt, yt, xend = x, yend = y),
                 colour = cores[["neutro"]], linewidth = .8) +
    geom_segment(data = ar2, aes(xp, yp, xend = x, yend = y),
                 colour = cores[["neutro"]], linewidth = .6) +
    geom_label(data = trat, aes(x, y, label = id), colour = "white",
               fill = cores_niveis[c(2, 3)], fontface = "bold", size = 5,
               label.padding = unit(.45, "lines"), linewidth = 0) +
    geom_point(data = parc, aes(x, y), size = 11, colour = cores[["neutro"]]) +
    geom_text(data = parc, aes(x, y, label = id), colour = "white",
              fontface = "bold", size = 4.2) +
    geom_point(data = arv, aes(x, y), size = 5, shape = 17,
               colour = cores[["verde"]]) +
    geom_text(data = niveis, aes(-1.6, y, label = lab), hjust = 1, size = 4.6,
              lineheight = .95, colour = cores[["texto"]]) +
    coord_cartesian(xlim = c(-4.5, 18), ylim = c(.6, 3.4), clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# Medidas repetidas: altura de mudas medida por 5 semanas.
# ---------------------------------------------------------------------------
dados_medidas_repetidas <- function(seed = 18) {
  set.seed(seed)
  tidyr::expand_grid(trat = factor(c("Controle", "Muito N"),
                                   c("Controle", "Muito N")),
                     muda = 1:6, semana = 1:5) |>
    group_by(trat, muda) |>
    mutate(id = paste0(substr(trat, 1, 1), muda),
           base = rnorm(1, 5, .8),
           taxa = rnorm(1, ifelse(trat == "Controle", 1, 1.7), .2),
           altura = round(base + taxa * (semana - 1) + rnorm(5, 0, .3), 1)) |>
    ungroup() |>
    select(id, trat, semana, altura)
}

tabela_medidas_repetidas <- function(d = dados_medidas_repetidas()) {
  d |>
    filter(id %in% c("C1", "C2", "C3", "M1", "M2", "M3")) |>
    pivot_wider(names_from = semana, values_from = altura,
                names_prefix = "S") |>
    rename(Muda = id, Tratamento = trat)
}

fig_medidas_repetidas <- function(d = dados_medidas_repetidas()) {
  cor_t <- c(Controle = cores[["neutro"]], `Muito N` = cores[["ferrugem"]])
  m <- d |> group_by(trat, semana) |> summarise(altura = mean(altura),
                                                .groups = "drop")
  ggplot(d, aes(semana, altura, colour = trat)) +
    geom_line(aes(group = id), alpha = .45, linewidth = .7) +
    geom_point(alpha = .45, size = 1.8) +
    geom_line(data = m, linewidth = 1.8) +
    scale_colour_manual(values = cor_t, name = NULL) +
    labs(x = "Semana", y = "Altura da muda (cm)") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# BACI com dados reais: anuros do Pantanal antes e depois de um incêndio
# (agosto de 2024). ARA é o controle e BAI, o local queimado. As unidades são
# gravações noturnas; `deteccoes` é o número de noites em que a espécie
# vocalizou. Dados de Provete, Sanches Farias et al. (manuscrito submetido).
# ---------------------------------------------------------------------------
dados_baci <- function() {
  read.csv(here::here("data/anuros_pantanal_baci.csv")) |>
    mutate(periodo = factor(periodo, c("Antes", "Depois")),
           grupo = factor(grupo, c("Controle", "Queimado"),
                          c("ARA (controle)", "BAI (queimado)")),
           prop = deteccoes / gravacoes)
}

fig_baci <- function(d = dados_baci(),
                     especies = c("Leptodactylus podicipinus",
                                  "Lysapsus limellum",
                                  "Leptodactylus fuscus",
                                  "Physalaemus albonotatus")) {
  cor_g <- c(`ARA (controle)` = cores[["accent"]],
             `BAI (queimado)` = cores[["ferrugem"]])
  d |>
    filter(especie %in% especies) |>
    mutate(especie = factor(especie, especies)) |>
    ggplot(aes(periodo, prop, colour = grupo, group = grupo)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 3.6) +
    facet_wrap(~especie, nrow = 1) +
    scale_colour_manual(values = cor_g, name = NULL) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                       limits = c(0, NA)) +
    labs(x = NULL, y = "Noites com\nvocalização") +
    tema_aula(14) +
    theme(strip.text = element_text(face = "italic"),
          legend.position = "bottom")
}
