# Aula 02, partes 1 e 2: da pergunta à réplica.
# Todos os dados são simulados; cada função fixa a própria semente para a
# figura sair igual a cada render.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))

# ---------------------------------------------------------------------------
# Diagrama causal mínimo (caixas e setas), usado para hipóteses e
# confundimento. `nos` tem colunas id, rotulo, x, y, cor; `arestas` tem de,
# para, rotulo e, opcionalmente, tracejada (lógico) e curva (numérico: 0 é
# reta; o sinal segue geom_curve). Arestas curvas servem para as voltas do
# fluxo e saem por cima (lado = "cima") ou por baixo das caixas; o rótulo
# delas fica em (lx, ly).
# ---------------------------------------------------------------------------
fig_diagrama <- function(nos, arestas, larg = 1.25, alt = .42, base = 5.2,
                         xlim = NULL, ylim = NULL) {
  if (!"tracejada" %in% names(arestas)) arestas$tracejada <- FALSE
  if (!"curva" %in% names(arestas)) arestas$curva <- 0
  if (!"lado" %in% names(arestas)) arestas$lado <- "baixo"
  if (!"lx" %in% names(arestas)) arestas$lx <- NA_real_
  if (!"ly" %in% names(arestas)) arestas$ly <- NA_real_
  pos <- select(nos, id, x, y)
  ar <- arestas |>
    left_join(rename(pos, de = id, x0 = x, y0 = y), by = "de") |>
    left_join(rename(pos, para = id, x1 = x, y1 = y), by = "para") |>
    mutate(
      dx = x1 - x0, dy = y1 - y0,
      # recua a seta até a borda da caixa (larg x alt), pelo lado que ela cruza
      s0 = pmin(abs((larg / 2 + .06) / dx), abs((alt / 2 + .06) / dy)),
      s1 = pmin(abs((larg / 2 + .10) / dx), abs((alt / 2 + .10) / dy)),
      xa = x0 + dx * s0, ya = y0 + dy * s0,
      xb = x1 - dx * s1, yb = y1 - dy * s1,
      xm = coalesce(lx, (xa + xb) / 2), ym = coalesce(ly, (ya + yb) / 2)
    )
  # aresta curva sai e chega pelo mesmo lado (cima ou baixo) das caixas
  curvas <- filter(ar, curva != 0) |>
    mutate(s = ifelse(lado == "cima", 1, -1),
           xa = x0, ya = y0 + s * (alt / 2 + .04),
           xb = x1, yb = y1 + s * (alt / 2 + .08))
  camadas_curvas <- lapply(seq_len(nrow(curvas)), function(i)
    geom_curve(data = curvas[i, ],
               aes(x = xa, y = ya, xend = xb, yend = yb, linetype = tracejada),
               curvature = curvas$curva[i], linewidth = 1.1,
               colour = cores[["neutro"]],
               arrow = arrow(length = unit(.3, "cm"), type = "closed")))
  g <- ggplot() +
    geom_segment(data = filter(ar, curva == 0),
                 aes(x = xa, y = ya, xend = xb, yend = yb,
                     linetype = tracejada),
                 linewidth = 1.3, colour = cores[["neutro"]],
                 arrow = arrow(length = unit(.32, "cm"), type = "closed")) +
    camadas_curvas +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"),
                          guide = "none") +
    geom_label(data = filter(ar, !is.na(rotulo)),
               aes(xm, ym, label = rotulo), size = base * .9,
               fontface = "bold", fill = cores[["papel"]],
               colour = cores[["texto"]], linewidth = 0) +
    geom_tile(data = nos, aes(x, y), width = larg, height = alt,
              fill = nos$cor, colour = NA) +
    geom_text(data = nos, aes(x, y, label = rotulo), colour = "white",
              size = base, lineheight = .95, fontface = "bold") +
    coord_equal(clip = "off", xlim = xlim, ylim = ylim) +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  g
}

# Da pergunta à predição, com as voltas do fluxo (Ruxton & Colegrave 2016,
# cap. 2).
fig_fluxo_pergunta <- function() {
  passos <- c(P = "Pergunta\nclara", H = "Hipóteses", R = "Predições",
              L = "Seguem da\nhipótese?", T = "São\ntestáveis?",
              C = "Planejar\na coleta")
  nos <- tibble::tibble(id = names(passos), rotulo = unname(passos),
                        x = (seq_along(passos) - 1) * 2.35, y = 0,
                        cor = c(rep(cores[["accent"]], 3),
                                rep(cores[["ocre"]], 2), cores[["verde"]]))
  arestas <- tibble::tribble(
    ~de, ~para, ~rotulo, ~tracejada, ~curva, ~lado,   ~lx,  ~ly,
    "P", "H",   NA,      FALSE,      0,      "baixo", NA,   NA,
    "H", "R",   NA,      FALSE,      0,      "baixo", NA,   NA,
    "R", "L",   NA,      FALSE,      0,      "baixo", NA,   NA,
    "L", "T",   "sim",   FALSE,      0,      "baixo", NA,   NA,
    "T", "C",   "sim",   FALSE,      0,      "baixo", NA,   NA,
    # indo para a esquerda, curvatura positiva arqueia para cima
    "L", "H",   "não",   TRUE,       .4,     "cima",  4.7,  1.4,
    "T", "R",   "não",   TRUE,       -.4,    "baixo", 7.05, -1.4
  )
  fig_diagrama(nos, arestas, larg = 1.85, alt = .8, base = 5,
               ylim = c(-1.6, 1.6))
}

fig_hipotese_n <- function() {
  nos <- tibble::tribble(
    ~id, ~rotulo,                              ~x,  ~y, ~cor,
    "N", "Nitrogênio\nno solo",                 2.6, 0,  cores[["verde"]],
    "C", "Taxa de crescimento\nper capita",      0,   0,  cores[["accent"]]
  )
  arestas <- tibble::tibble(de = "N", para = "C", rotulo = "+")
  fig_diagrama(nos, arestas, larg = 1.45, alt = .6)
}

# ---------------------------------------------------------------------------
# Expressão gráfica das hipóteses: nula e três alternativas.
# ---------------------------------------------------------------------------
dados_hipotese <- function(tipo, n = 9, seed = 2) {
  set.seed(seed)
  x <- sort(runif(n, .5, 9.5))
  f <- switch(tipo,
    nula      = function(x) rep(5, length(x)),
    linear    = function(x) 1.2 + .75 * x,
    saturante = function(x) 1 + 6.5 * x / (1.3 + x),
    unimodal  = function(x) 1.5 + 1.9 * x - .19 * x^2
  )
  list(pontos = tibble::tibble(x = x, y = f(x) + rnorm(n, 0, .75)),
       curva  = tibble::tibble(x = seq(.2, 9.8, length.out = 200),
                               y = f(seq(.2, 9.8, length.out = 200))))
}

fig_hipotese <- function(tipo = "nula", titulo = NULL, base = 15) {
  d <- dados_hipotese(tipo)
  ggplot(d$pontos, aes(x, y)) +
    geom_line(data = d$curva, colour = cores[["verde"]], linewidth = 1.4) +
    geom_point(size = 3.2, colour = cores[["accent"]]) +
    scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
    labs(x = "Nitrogênio no solo (mg/g)",
         y = "Crescimento per capita", title = titulo) +
    tema_aula(base, conceitual = TRUE)
}

fig_hipoteses_alternativas <- function() {
  fig_hipotese("linear",    "H1: linear",    base = 13) +
  fig_hipotese("saturante", "H2: saturante", base = 13) +
  fig_hipotese("unimodal",  "H3: unimodal",  base = 13)
}

# ---------------------------------------------------------------------------
# Mesma diferença real entre grupos, três tamanhos de amostra.
# ---------------------------------------------------------------------------
fig_tamanho_amostra <- function(ns = c(3, 10, 30), dif = 1.5, dp = 2,
                                seed = 7) {
  set.seed(seed)
  d <- purrr::map_dfr(ns, function(k) tibble::tibble(
    n = k,
    grupo = rep(c("Floresta A", "Floresta B"), each = k),
    y = c(rnorm(k, 10, dp), rnorm(k, 10 + dif, dp))
  )) |>
    mutate(painel = factor(paste0("n = ", n, " por grupo"),
                           levels = paste0("n = ", ns, " por grupo")))
  resumo <- d |>
    group_by(painel, grupo) |>
    summarise(m = mean(y), n = n(),
              ep = sd(y) / sqrt(n), .groups = "drop") |>
    mutate(li = m - qt(.975, n - 1) * ep, ls = m + qt(.975, n - 1) * ep)

  ggplot(d, aes(grupo, y)) +
    geom_hline(yintercept = c(10, 10 + dif), linetype = "22",
               colour = cores[["neutro"]], linewidth = .5) +
    geom_jitter(width = .12, height = 0, size = 2, alpha = .45,
                colour = cores[["neutro"]]) +
    geom_pointrange(data = resumo, aes(y = m, ymin = li, ymax = ls,
                                       colour = grupo),
                    linewidth = 1.2, size = .7,
                    position = position_nudge(x = .28)) +
    scale_colour_manual(values = c(cores[["accent"]], cores[["ferrugem"]]),
                        guide = "none") +
    facet_wrap(~painel) +
    labs(x = NULL, y = "Área foliar (cm²)") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Pseudorréplica espacial: três padrões numa grade 10 x 10.
# ---------------------------------------------------------------------------
fig_autocorrelacao_espacial <- function(k = 10, seed = 4) {
  set.seed(seed)
  g <- expand.grid(x = 1:k, y = 1:k)
  # positiva: campo suave (média móvel de ruído), cortado na mediana
  z <- matrix(rnorm((k + 4)^2), k + 4)
  suave <- sapply(seq_len(nrow(g)), function(i)
    mean(z[g$x[i] + 0:4, g$y[i] + 0:4]))
  pos <- as.integer(suave > median(suave))
  nul <- rbinom(nrow(g), 1, .5)
  neg <- (g$x + g$y) %% 2
  d <- bind_rows(
    mutate(g, v = pos, p = "Autocorrelação positiva"),
    mutate(g, v = nul, p = "Sem autocorrelação"),
    mutate(g, v = neg, p = "Autocorrelação negativa")
  ) |>
    mutate(p = factor(p, levels = unique(p)))
  ggplot(d, aes(x, y, fill = factor(v))) +
    geom_tile(colour = cores[["papel"]], linewidth = .6) +
    scale_fill_manual(values = c(cores[["neutro_cl"]], cores[["accent_dark"]]),
                      guide = "none") +
    coord_equal() +
    facet_wrap(~p) +
    theme_void(base_size = 15) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          strip.text = element_text(face = "bold", margin = margin(b = 6)))
}

# ---------------------------------------------------------------------------
# Pseudorréplica temporal: série AR(1) e correlação em função da defasagem.
# ---------------------------------------------------------------------------
fig_autocorrelacao_temporal <- function(n = 120, phi = .85, seed = 11) {
  set.seed(seed)
  y <- as.numeric(arima.sim(list(ar = phi), n = n)) + 20
  d <- tibble::tibble(semana = seq_len(n), y = y)
  destaque <- tibble::tibble(semana = c(30, 31, 90),
                             grupo = c("vizinhas", "vizinhas", "distante")) |>
    left_join(d, by = "semana")

  a <- ggplot(d, aes(semana, y)) +
    geom_line(colour = cores[["neutro"]], linewidth = .6) +
    geom_point(data = destaque, aes(colour = grupo), size = 4) +
    scale_colour_manual(values = c(vizinhas = cores[["accent"]],
                                   distante = cores[["ferrugem"]]),
                        name = NULL) +
    labs(x = "Semana", y = "Abundância (log)",
         title = "Uma população acompanhada por 120 semanas") +
    tema_aula(14)

  ac <- acf(y, lag.max = 30, plot = FALSE)
  dac <- tibble::tibble(lag = as.numeric(ac$lag)[-1],
                        r = as.numeric(ac$acf)[-1])
  b <- ggplot(dac, aes(lag, r)) +
    geom_hline(yintercept = 0, colour = cores[["neutro"]]) +
    geom_segment(aes(xend = lag, yend = 0), colour = cores[["accent"]],
                 linewidth = 1.1) +
    geom_point(colour = cores[["accent"]], size = 1.8) +
    labs(x = "Distância entre as medidas (semanas)",
         y = "Correlação",
         title = "Correlação por defasagem") +
    tema_aula(14)
  a + b + plot_layout(widths = c(1.4, 1))
}

# ---------------------------------------------------------------------------
# Pseudorréplica filogenética. Dois caracteres evoluem por movimento
# browniano, independentes um do outro, numa árvore com dois clados. A
# correlação entre espécies aparece só porque espécies do mesmo clado
# herdaram valores parecidos do ancestral comum.
# ---------------------------------------------------------------------------
simula_filogenia <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  cl <- function(tips) {
    t <- ape::rcoal(8, tip.label = tips)
    t$edge.length <- t$edge.length / max(ape::node.depth.edgelength(t)) * .35
    t
  }
  a <- cl(paste0("A", 1:8)); b <- cl(paste0("B", 1:8))
  # junta os dois clados por uma raiz com ramos de 0,65
  arv <- ape::read.tree(text = paste0(
    "(", sub(";$", "", ape::write.tree(a)), ":.65,",
    sub(";$", "", ape::write.tree(b)), ":.65);"))
  x <- ape::rTraitCont(arv, sigma = 3)
  y <- ape::rTraitCont(arv, sigma = 3)
  # rTraitCont devolve os valores na ordem de tip.label
  sp <- arv$tip.label
  list(arvore = arv, dados = tibble::tibble(
    especie = sp, x = as.numeric(x), y = as.numeric(y),
    clado = ifelse(substr(sp, 1, 1) == "A", "Clado A", "Clado B")))
}

# Procura uma semente em que a correlação entre todas as espécies é forte e
# a correlação dentro de cada clado é fraca, para o exemplo ser didático.
# O resultado foi fixado em `seed` abaixo; a função fica para referência.
procura_semente_filogenia <- function(seeds = 1:500) {
  for (s in seeds) {
    d <- simula_filogenia(s)$dados
    r_tot <- cor(d$x, d$y)
    r_in <- d |> group_by(clado) |> summarise(r = cor(x, y)) |> pull(r)
    if (abs(r_tot) > .7 && all(abs(r_in) < .25)) return(s)
  }
  NA
}

fig_sinal_filogenetico <- function(seed = 249) {
  s <- simula_filogenia(seed)
  d <- s$dados
  cor_clado <- c(`Clado A` = cores[["accent"]], `Clado B` = cores[["ocre"]])

  # árvore desenhada em ggplot a partir das coordenadas do ape
  arv <- s$arvore
  # plot.phylo(plot = FALSE) ainda abre um dispositivo gráfico, e no Quarto
  # isso vira uma imagem em branco no slide; por isso o dispositivo nulo
  grDevices::pdf(NULL)
  ape::plot.phylo(arv, plot = FALSE, direction = "upwards")
  ult <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
  grDevices::dev.off()
  ed <- tibble::as_tibble(arv$edge, .name_repair = ~c("pai", "filho")) |>
    mutate(x0 = ult$xx[pai], x1 = ult$xx[filho],
           y0 = ult$yy[pai], y1 = ult$yy[filho])
  nt <- ape::Ntip(arv)
  pontas <- tibble::tibble(especie = arv$tip.label,
                           x = ult$xx[1:nt], y = ult$yy[1:nt]) |>
    left_join(select(d, especie, clado), by = "especie")
  ga <- ggplot() +
    geom_segment(data = ed, aes(x0, y0, xend = x1, yend = y0),
                 colour = cores[["texto"]], linewidth = .7) +
    geom_segment(data = ed, aes(x1, y0, xend = x1, yend = y1),
                 colour = cores[["texto"]], linewidth = .7) +
    geom_point(data = pontas, aes(x, y, colour = clado), size = 3.4) +
    scale_colour_manual(values = cor_clado, guide = "none") +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))

  r_tot <- cor(d$x, d$y)
  gb <- ggplot(d, aes(x, y)) +
    geom_smooth(method = "lm", se = FALSE, colour = cores[["neutro"]],
                linetype = "22", linewidth = .9) +
    geom_smooth(aes(colour = clado), method = "lm", se = FALSE,
                linewidth = .9) +
    geom_point(aes(colour = clado), size = 3.4) +
    scale_colour_manual(values = cor_clado, name = NULL) +
    annotate("label", x = -Inf, y = Inf, hjust = -.05, vjust = 1.2,
             label = sprintf("todas as espécies: r = %.2f", r_tot),
             size = 4.6, fill = cores[["papel"]], linewidth = 0) +
    labs(x = "Caráter X", y = "Caráter Y") +
    tema_aula(14)
  ga + gb + plot_layout(widths = c(1, 1.15))
}

# ---------------------------------------------------------------------------
# Cenário 3: folhas dentro de árvores dentro de florestas.
# ---------------------------------------------------------------------------
fig_hierarquia_folhas <- function(seed = 5) {
  set.seed(seed)
  arvores <- function(floresta, n_arv, folhas_por, x0, desenho) {
    # com uma árvore só, ela fica no centro e as folhas se espalham mais
    espalho <- if (n_arv == 1) .9 else .09
    ax <- if (n_arv == 1) x0 + 1.5 else x0 + seq(.35, 2.65, length.out = n_arv)
    tibble::tibble(arv = seq_len(n_arv), ax = ax, ay = 1.6) |>
      tidyr::uncount(folhas_por, .remove = FALSE) |>
      mutate(fx = ax + runif(n(), -espalho, espalho),
             fy = runif(n(), .35, 1.2),
             floresta = floresta, desenho = desenho)
  }
  d <- bind_rows(
    arvores("A", 1, 50, 0,   "Como foi feito: 1 árvore, 50 folhas"),
    arvores("B", 1, 50, 3.4, "Como foi feito: 1 árvore, 50 folhas"),
    arvores("A", 10, 5, 0,   "Como deveria ser: 10 árvores, 5 folhas cada"),
    arvores("B", 10, 5, 3.4, "Como deveria ser: 10 árvores, 5 folhas cada")
  ) |>
    mutate(desenho = factor(desenho, levels = unique(desenho)))
  cor_fl <- c(A = cores[["verde"]], B = cores[["ferrugem"]])
  arv <- distinct(d, desenho, floresta, arv, ax, ay)
  rot <- tibble::tibble(floresta = c("A", "B"), x = c(1.5, 4.9), y = 2.15,
                        lab = c("Floresta A", "Floresta B"))
  ggplot() +
    geom_rect(data = tibble::tibble(floresta = c("A", "B"),
                                    x0 = c(0, 3.4), x1 = c(3, 6.4)),
              aes(xmin = x0, xmax = x1, ymin = .15, ymax = 1.95),
              fill = NA, colour = cores[["neutro"]], linewidth = .5,
              linetype = "22") +
    geom_segment(data = d, aes(ax, ay - .12, xend = fx, yend = fy,
                               colour = floresta), alpha = .18,
                 linewidth = .35) +
    geom_point(data = arv, aes(ax, ay, colour = floresta), shape = 17,
               size = 5) +
    geom_point(data = d, aes(fx, fy, colour = floresta), size = 1.3,
               alpha = .8) +
    geom_text(data = rot, aes(x, y, label = lab, colour = floresta),
              fontface = "bold", size = 5) +
    scale_colour_manual(values = cor_fl, guide = "none") +
    facet_wrap(~desenho, ncol = 1) +
    coord_cartesian(clip = "off") +
    theme_void(base_size = 15) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          strip.text = element_text(face = "bold", hjust = 0,
                                    margin = margin(b = 4, t = 6)))
}
