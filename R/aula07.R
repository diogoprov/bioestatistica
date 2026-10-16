# Aula 07: teste de hipóteses. Os dados das plantas de Pinus são simulados;
# cada função que sorteia fixa a própria semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/aula02_pergunta.R"))   # fig_diagrama()

vermelho <- "#c0392b"
azul_beta <- "#5b8db8"
cinza_caixa <- "#b9b2a6"
num_br <- function(v, d = 2) format(round(v, d), nsmall = d, decimal.mark = ",")

fundo_papel <- plot_annotation(theme = theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA)))

fundo <- function() {
  theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# O roteiro do teste de hipóteses em oito passos, com o passo da vez em
# destaque. Os passos 6 e 7 se dividem nos dois métodos (valor P e valor
# crítico), que levam à mesma decisão.
# ---------------------------------------------------------------------------
fig_roteiro <- function(passo = NULL) {
  nos <- tibble::tribble(
    ~id,  ~rotulo,                                            ~x,   ~y,  ~etapa,
    "1",  "1. Identificar\na afirmativa",                     0,    1,   1,
    "2",  "2. Forma simbólica\nse a afirmativa\nfor falsa",          2.6,  1,   2,
    "3",  "3. Hipóteses nula\ne alternativa",                 5.2,  1,   3,
    "4",  "4. Nível de\nsignificância (α)",                   7.8,  1,   4,
    "5",  "5. Estatística\ndo teste",                         10.4, 1,   5,
    "6P", "6. Calcular\no valor P",                           2.6,  -.4, 6,
    "7P", "7. Rejeitar H0\nse P ≤ α",                         5.2,  -.4, 7,
    "6C", "6. Achar o(s)\nvalor(es) crítico(s)",              2.6,  -1.8, 6,
    "7C", "7. Rejeitar H0 se a\nestatística cair na\nregião crítica", 5.2, -1.8, 7,
    "8",  "8. Conclusão\nem termos\nnão técnicos", 9.1, -1.1, 8
  ) |>
    mutate(cor = if (is.null(passo)) cores[["accent"]]
                 else ifelse(etapa %in% passo, cores[["accent"]], cinza_caixa))
  arestas <- tibble::tribble(
    ~de,  ~para,
    "1",  "2",  "2", "3",  "3", "4",  "4", "5",
    "6P", "7P", "6C", "7C", "7P", "8", "7C", "8"
  ) |> mutate(rotulo = NA_character_)
  # do passo 5 para os dois métodos: uma linha que desce e volta à esquerda
  g <- fig_diagrama(nos, arestas, larg = 2.2, alt = 1.05, base = 4.6,
                    xlim = c(-1.2, 11.6), ylim = c(-2.5, 1.7))
  g +
    annotate("path", x = c(10.4, 10.4, .2, .2), y = c(.43, .3, .3, -.4),
             linewidth = 1.1, colour = cores[["neutro"]]) +
    annotate("segment", x = .2, xend = 1.43, y = c(-.4, -1.8), yend = c(-.4, -1.8),
             linewidth = 1.1, colour = cores[["neutro"]],
             arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
    annotate("path", x = c(.2, .2), y = c(-.4, -1.8),
             linewidth = 1.1, colour = cores[["neutro"]]) +
    annotate("text", x = c(-1.15, -1.15), y = c(-.4, -1.8), hjust = 0,
             label = c("Método do\nvalor P", "Método do\nvalor crítico"),
             size = 4.3, fontface = "italic", colour = cores[["texto"]], lineheight = .9)
}

# ---------------------------------------------------------------------------
# Pergunta → hipótese → predição (teoria e dedução nas setas).
# ---------------------------------------------------------------------------
fig_pergunta_hipotese <- function() {
  nos <- tibble::tribble(
    ~id, ~rotulo,          ~x, ~y,   ~cor,
    "P", "Pergunta",       0,  2,    cores[["accent"]],
    "H", "Hipótese(s)",    0,  1,    cores[["verde"]],
    "R", "Predição(ões)",  0,  0,    vermelho
  )
  arestas <- tibble::tribble(
    ~de, ~para, ~rotulo,  ~lx,  ~ly,
    "P", "H",   "Teoria",  .95, 1.5,
    "H", "R",   "Dedução", .95, .5
  )
  fig_diagrama(nos, arestas, larg = 1.5, alt = .5, base = 6,
               xlim = c(-1, 1.5))
}

# ---------------------------------------------------------------------------
# Unidade amostral: 20 áreas independentes (10 de cada solo) ou 20 parcelas
# dentro de só duas áreas, uma de cada solo (pseudorréplica).
# ---------------------------------------------------------------------------
fig_unidade_amostral <- function(marca = FALSE, seed = 4) {
  set.seed(seed)
  t <- seq(0, 2 * pi, length.out = 40)
  mancha <- function(cx, cy, r, id) tibble::tibble(
    id = id, x = cx + r * cos(t) * (1 + .12 * sin(3 * t + cx)),
    y = cy + r * sin(t) * (1 + .12 * cos(2 * t + cy)))
  # A: 20 áreas espalhadas, cada uma com uma parcela
  ca <- tibble::tibble(id = 1:20, cx = runif(20, .5, 9.5), cy = runif(20, .5, 5.5),
                       solo = sample(rep(c("Argiloso", "Rochoso"), 10)))
  # afasta as áreas que caíram muito perto umas das outras
  for (k in 1:200) {
    d <- as.matrix(dist(ca[, c("cx", "cy")])); diag(d) <- Inf
    if (min(d) > 1.25) break
    i <- which(d == min(d), arr.ind = TRUE)[1, 1]
    ca$cx[i] <- runif(1, .5, 9.5); ca$cy[i] <- runif(1, .5, 5.5)
  }
  ma <- purrr::pmap_dfr(ca, \(id, cx, cy, solo) mancha(cx, cy, .5, id)) |>
    left_join(select(ca, id, solo), by = "id")
  pa <- ggplot() +
    geom_polygon(data = ma, aes(x, y, group = id, fill = solo), colour = "white") +
    geom_tile(data = ca, aes(cx, cy), width = .32, height = .32,
              fill = NA, colour = cores[["texto"]], linewidth = .7) +
    labs(title = "20 áreas, 10 de cada solo",
         subtitle = "uma parcela por área")
  # B: duas áreas, dez parcelas em cada
  mb <- bind_rows(mancha(2.5, 3, 2.2, 1) |> mutate(solo = "Argiloso"),
                  mancha(7.5, 3, 2.2, 2) |> mutate(solo = "Rochoso"))
  pb_par <- tidyr::expand_grid(cx = c(2.5, 7.5), i = 1:10) |>
    mutate(x = cx + rep(c(-1.2, -.6, 0, .6, 1.2), 4),
           y = 3 + rep(rep(c(.45, -.45), each = 5), 2))
  pb <- ggplot() +
    geom_polygon(data = mb, aes(x, y, group = id, fill = solo), colour = "white") +
    geom_tile(data = pb_par, aes(x, y), width = .32, height = .32,
              fill = NA, colour = cores[["texto"]], linewidth = .7) +
    labs(title = "2 áreas, uma de cada solo",
         subtitle = "10 parcelas em cada área")
  arruma <- function(g, simbolo, cor) {
    g <- g + scale_fill_manual(values = cores_solo, name = "Solo") +
      coord_equal(xlim = c(0, 10), ylim = c(0, 6), clip = "off") + fundo() +
      theme(plot.title = element_text(face = "bold", size = 14, hjust = .5,
                                      colour = cores[["texto"]]),
            plot.subtitle = element_text(size = 13, hjust = .5, colour = "#4a5568"),
            legend.position = "bottom", legend.text = element_text(size = 13),
            legend.title = element_text(size = 13))
    if (marca) g <- g + annotate("text", x = 9.9, y = 6.3, label = simbolo,
                                 size = 22, colour = cor, fontface = "bold")
    g
  }
  arruma(pa, "✓", cores[["verde"]]) + arruma(pb, "✗", vermelho) +
    plot_layout(guides = "collect") + fundo_papel &
    theme(legend.position = "bottom")
}

# ---------------------------------------------------------------------------
# Distribuição nula (normal ou t) com a região crítica sombreada.
# tipo: "bi", "esq" ou "dir". `obs` marca a estatística observada.
# ---------------------------------------------------------------------------
curva_nula <- function(gl = NULL, lim = 4) {
  x <- seq(-lim, lim, by = .01)
  tibble::tibble(x = x, y = if (is.null(gl)) dnorm(x) else dt(x, gl))
}

valor_critico <- function(tipo, alfa, gl = NULL) {
  q <- if (is.null(gl)) qnorm else function(p) qt(p, gl)
  switch(tipo, bi = q(1 - alfa / 2) * c(-1, 1), esq = q(alfa), dir = q(1 - alfa))
}

fig_regiao_critica <- function(tipo = "bi", alfa = .05, gl = NULL, obs = NULL,
                               rotulos = TRUE, base = 15, titulo = NULL,
                               cor_regiao = "#7fb7d6") {
  d <- curva_nula(gl)
  vc <- valor_critico(tipo, alfa, gl)
  na_regiao <- switch(tipo, bi = d$x <= vc[1] | d$x >= vc[2],
                      esq = d$x <= vc, dir = d$x >= vc)
  topo <- max(d$y)
  g <- ggplot(d, aes(x, y))
  if (tipo %in% c("bi", "esq"))
    g <- g + geom_ribbon(data = filter(d, x <= min(vc)), aes(ymin = 0, ymax = y), fill = cor_regiao)
  if (tipo %in% c("bi", "dir"))
    g <- g + geom_ribbon(data = filter(d, x >= max(vc)), aes(ymin = 0, ymax = y), fill = cor_regiao)
  g <- g + geom_line(linewidth = 1.1) +
    geom_segment(data = tibble::tibble(v = vc), aes(x = v, xend = v, y = 0, yend = topo * .78),
                 inherit.aes = FALSE, linetype = "22", colour = cores[["texto"]])
  nome <- if (is.null(gl)) "z" else "t"
  quebras <- sort(c(vc, 0))
  g <- g + scale_x_continuous(breaks = quebras,
                              labels = ifelse(quebras == 0, "0", num_br(quebras, 3))) +
    labs(x = if (is.null(gl)) "Estatística do teste (z)"
             else sprintf("Estatística do teste (t, %s graus de liberdade)",
                       num_br(gl, if (gl %% 1 == 0) 0 else 1)),
         y = NULL, title = titulo) +
    tema_aula(base) +
    theme(axis.text.y = element_blank(), panel.grid = element_blank(),
          plot.title = element_text(hjust = .5))
  if (rotulos) {
    caudas <- switch(tipo, bi = c(-3.1, 3.1), esq = -3.1, dir = 3.1)
    g <- g +
      annotate("label", x = caudas, y = topo * .55, label = "Região crítica:\nrejeita-se H0",
               size = base * .27, colour = "#1f5f80", fill = cores[["papel"]],
               linewidth = 0, lineheight = .9) +
      annotate("label", x = switch(tipo, bi = 0, esq = .9, dir = -.9), y = topo * .3,
               label = "Região de não\nrejeição de H0", size = base * .27,
               colour = cores[["texto"]], fill = cores[["papel"]], linewidth = 0,
               lineheight = .9) +
      annotate("text", x = caudas, y = topo * .9,
               label = sprintf("α%s = %s", if (tipo == "bi") "/2" else "",
                               num_br(if (tipo == "bi") alfa / 2 else alfa, if (tipo == "bi") 3 else 2)),
               size = base * .3)
  }
  if (!is.null(obs)) {
    rejeita <- switch(tipo, bi = abs(obs) >= max(vc), esq = obs <= vc, dir = obs >= vc)
    cor <- if (rejeita) cores[["verde"]] else vermelho
    g <- g +
      annotate("segment", x = obs, xend = obs, y = topo * 1.02, yend = 0,
               colour = cor, linewidth = 1.4,
               arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
      annotate("label", x = obs, y = topo * 1.1,
               label = sprintf("%s observado = %s", nome, num_br(obs, 2)),
               size = base * .3, colour = cor, fill = cores[["papel"]], fontface = "bold")
  }
  g
}

# As três formas da região crítica: bicaudal, unicaudal à esquerda e à direita.
fig_caudas <- function() {
  p <- function(tipo, tit) fig_regiao_critica(tipo, rotulos = FALSE, base = 13, titulo = tit) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  p("bi", "Bicaudal\nH1: ≠ (não direcional)") +
    p("esq", "Unicaudal à esquerda\nH1: <") +
    p("dir", "Unicaudal à direita\nH1: >") + fundo_papel
}

# ---------------------------------------------------------------------------
# Exemplo fictício para os dois métodos: 482 de 926 filhotes de tartaruga
# são fêmeas; H0: p = 0,5 e H1: p > 0,5. z = 1,25.
# ---------------------------------------------------------------------------
# arredondado a duas casas, como se faz ao consultar a tabela
z_tartarugas <- function(x = 482, n = 926, p0 = .5) round((x / n - p0) / sqrt(p0 * (1 - p0) / n), 2)

fig_valor_p <- function(z = z_tartarugas(), base = 15) {
  d <- curva_nula()
  pv <- 1 - pnorm(z)
  ggplot(d, aes(x, y)) +
    geom_ribbon(data = filter(d, x >= z), aes(ymin = 0, ymax = y), fill = "#f2c38b") +
    geom_line(linewidth = 1.1) +
    annotate("segment", x = z, xend = z, y = 0, yend = .3, colour = cores[["texto"]]) +
    annotate("label", x = z + .3, y = .33, hjust = 0, size = base * .3,
             label = sprintf("Valor P = %s", num_br(pv, 4)), fill = cores[["papel"]], linewidth = 0) +
    scale_x_continuous(breaks = c(0, z), labels = c("0", sprintf("z = %s", num_br(z, 2)))) +
    labs(x = "Estatística do teste (z)", y = NULL, title = "Método do valor P") +
    tema_aula(base) +
    theme(axis.text.y = element_blank(), panel.grid = element_blank(),
          plot.title = element_text(hjust = .5))
}

fig_dois_metodos <- function() {
  fig_valor_p() +
    (fig_regiao_critica("dir", obs = z_tartarugas(), rotulos = FALSE,
                        titulo = "Método do valor crítico") +
       annotate("text", x = 2.9, y = .12, label = "α = 0,05", size = 4.5)) +
    fundo_papel
}

# ---------------------------------------------------------------------------
# Erros tipo I e II: as distribuições da estatística sob H0 e sob H1.
# efeito em erros padrão (efeito × √n / dp), teste unicaudal à direita.
# ---------------------------------------------------------------------------
poder_teste <- function(efeito = .5, n = 20, dp = 1, alfa = .05) {
  delta <- efeito * sqrt(n) / dp
  crit <- qnorm(1 - alfa)
  c(delta = delta, crit = crit, beta = pnorm(crit - delta), poder = 1 - pnorm(crit - delta))
}

fig_alfa_beta <- function(efeito = .5, n = 20, dp = 1, alfa = .05, titulo = NULL,
                          base = 15, legenda = TRUE) {
  pt_ <- poder_teste(efeito, n, dp, alfa)
  x <- seq(-3.5, max(7.5, pt_[["delta"]] + 3.5), by = .01)
  d <- tibble::tibble(x = x, h0 = dnorm(x), h1 = dnorm(x, pt_[["delta"]]))
  crit <- pt_[["crit"]]
  g <- ggplot(d, aes(x)) +
    geom_ribbon(data = filter(d, x <= crit), aes(ymin = 0, ymax = h1), fill = azul_beta, alpha = .55) +
    geom_ribbon(data = filter(d, x >= crit), aes(ymin = 0, ymax = h0), fill = vermelho, alpha = .7) +
    geom_line(aes(y = h0), linewidth = 1.1) +
    geom_line(aes(y = h1), linewidth = 1.1, linetype = "42") +
    geom_vline(xintercept = crit, colour = cores[["texto"]], linetype = "22") +
    annotate("text", x = c(0, pt_[["delta"]]), y = .43, size = base * .28,
             label = if (legenda) c("H0 verdadeira", "H1 verdadeira") else c("H0", "H1")) +
    scale_y_continuous(limits = c(0, .46)) +
    labs(x = NULL, y = NULL,
         title = titulo,
         subtitle = sprintf("β = %s · poder = %s", num_br(pt_[["beta"]], 2), num_br(pt_[["poder"]], 2))) +
    tema_aula(base, conceitual = TRUE) +
    theme(axis.line = element_blank(), plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5))
  if (legenda)
    g <- g +
      annotate("label", x = crit + 1.1, y = .06, label = "α\n(erro tipo I)", size = base * .27,
               colour = vermelho, fill = cores[["papel"]], linewidth = 0, lineheight = .9) +
      annotate("label", x = crit - 1.3, y = .06, label = "β\n(erro tipo II)", size = base * .27,
               colour = "#2f5f88", fill = cores[["papel"]], linewidth = 0, lineheight = .9) +
      annotate("text", x = crit, y = -.0, vjust = 1.2, label = "valor crítico", size = base * .25)
  g
}

legenda_poder <- function() {
  ggplot() +
    annotate("rect", xmin = 0, xmax = .5, ymin = c(2.6, 1.6), ymax = c(3.1, 2.1),
             fill = c(vermelho, azul_beta), alpha = c(.7, .55)) +
    annotate("segment", x = 0, xend = .5, y = c(.85, .35), yend = c(.85, .35),
             linetype = c("solid", "42"), linewidth = 1.1) +
    annotate("text", x = .7, y = c(2.85, 1.85, .85, .35), hjust = 0, size = 4.4,
             label = c("α: erro tipo I", "β: erro tipo II", "distribuição sob H0",
                       "distribuição sob H1")) +
    coord_cartesian(xlim = c(0, 3.5), ylim = c(0, 3.4)) + fundo()
}

# O que diminui β: cada painel muda uma coisa em relação à referência.
fig_poder <- function() {
  p <- function(..., tit) fig_alfa_beta(..., titulo = tit, base = 12, legenda = FALSE)
  (p(tit = "Referência\n(α = 0,05; n = 20; efeito 0,5; dp 1)") +
     p(alfa = .10, tit = "α maior\n(0,10)") +
     p(n = 50, tit = "Tamanho amostral maior\n(n = 50)")) /
    (p(efeito = .8, tit = "Efeito maior\n(0,8)") +
       p(dp = 1.6, tit = "Variabilidade maior\n(dp 1,6)") +
       legenda_poder()) + fundo_papel
}

# ---------------------------------------------------------------------------
# Pesquisador cético (bicaudal) e otimista (unicaudal), mesmo t = 1,80.
# ---------------------------------------------------------------------------
fig_cetico_otimista <- function(t_obs = 1.80, gl = 29) {
  fig_regiao_critica("bi", gl = gl, obs = t_obs, rotulos = FALSE, base = 13,
                     titulo = "Cético (bicaudal): t crítico = ±2,045") +
    fig_regiao_critica("dir", gl = gl, obs = t_obs, rotulos = FALSE, base = 13,
                       titulo = "Otimista (unicaudal): t crítico = +1,699",
                       cor_regiao = "#e8a39b") +
    fundo_papel
}

# ---------------------------------------------------------------------------
# Altura de plantas adultas de Pinus em solo argiloso e rochoso (simulado).
# ---------------------------------------------------------------------------
dados_pinus <- function(n = 25, seed = 6) {
  set.seed(seed)
  tibble::tibble(
    solo = factor(rep(c("argiloso", "rochoso"), each = n), levels = c("argiloso", "rochoso")),
    altura = round(c(rnorm(n, 22.5, .55), rnorm(n, 22.2, .75)), 2)
  ) |>
    mutate(altura = replace(altura, c(n, 2 * n), c(18.1, 18.9)))   # um valor baixo em cada solo
}

fig_pinus_densidade <- function(d = dados_pinus()) {
  ggplot(d, aes(altura, fill = solo)) +
    geom_density(alpha = .6, colour = cores[["texto"]], adjust = 1.2) +
    scale_fill_manual(values = c(argiloso = cores_solo[["Argiloso"]], rochoso = "#7f8c8d"),
                      name = "Solo") +
    labs(x = expression("Altura das plantas adultas de" ~ italic(Pinus) ~ "(m)"), y = "Densidade") +
    tema_aula(15)
}

fig_pinus_boxplot <- function(d = dados_pinus()) {
  ggplot(d, aes(solo, altura)) +
    geom_boxplot(aes(fill = solo), width = .5, outlier.shape = NA, alpha = .8,
                 show.legend = FALSE) +
    geom_point(position = position_jitter(width = .12, seed = 2), alpha = .6,
               colour = cores[["texto"]]) +
    scale_fill_manual(values = c(argiloso = cores_solo[["Argiloso"]], rochoso = "#a9a9a0")) +
    labs(x = "Tipo de solo", y = expression("Altura das plantas adultas de" ~ italic(Pinus) ~ "(m)")) +
    tema_aula(15)
}

fig_pinus_decisao <- function(d = dados_pinus(), alfa = .05) {
  tt <- t.test(altura ~ solo, data = d, alternative = "greater")
  gl <- unname(tt$parameter)
  fig_regiao_critica("dir", alfa = alfa, gl = gl, obs = unname(tt$statistic), rotulos = TRUE)
}

# ---------------------------------------------------------------------------
# Teste de permutação: se o solo não importa, os rótulos são intercambiáveis.
# Embaralhamos os rótulos e recalculamos a diferença entre as médias.
# ---------------------------------------------------------------------------
dif_medias <- function(altura, solo) {
  mean(altura[solo == "argiloso"]) - mean(altura[solo == "rochoso"])
}

permuta_pinus <- function(d = dados_pinus(), b = 5000, seed = 12) {
  set.seed(seed)
  replicate(b, dif_medias(d$altura, sample(d$solo)))
}

fig_permutacao <- function(d = dados_pinus(), b = 5000, seed = 12) {
  nula <- permuta_pinus(d, b, seed)
  obs <- dif_medias(d$altura, d$solo)
  p <- mean(nula >= obs - 1e-9)   # tolerância para a igualdade numérica
  ggplot(tibble::tibble(dif = nula), aes(dif, fill = dif >= obs - 1e-9)) +
    geom_histogram(binwidth = .03, boundary = obs - 1e-9, colour = cores[["papel"]]) +
    geom_vline(xintercept = obs, colour = cores[["ferrugem"]], linewidth = 1.2) +
    scale_fill_manual(values = c(`FALSE` = cores[["neutro"]], `TRUE` = cores[["ferrugem"]]),
                      guide = "none") +
    labs(x = "Diferença entre as médias, argiloso − rochoso (m)",
         y = "Número de embaralhamentos",
         subtitle = paste0(format(b, big.mark = "."), " embaralhamentos dos rótulos de solo\n",
                           "Linha: diferença observada (", num_br(obs, 2), " m); ",
                           "P = ", num_br(p, 3), " (proporção à direita)")) +
    tema_aula(14)
}
