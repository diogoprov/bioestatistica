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

# ---------------------------------------------------------------------------
# Cérebro visto de lado (frente à esquerda), em coordenadas do painel: (cx,
# cy) é o centro e k a escala. O eixo y do galaxy brain é 1,4 vez mais
# "alto" que o x, então as alturas são divididas por 1,4. Devolve áreas
# (tronco, cerebelo, cérebro) e linhas (giros, fissuras, folhas do
# cerebelo), nessa ordem de desenho.
# ---------------------------------------------------------------------------
cerebro_lateral <- function(cx, cy, k) {
  fecha <- function(u, v, n = 160) {
    sp <- spline(seq_along(c(u, u[1])), c(u, u[1]), n = n, method = "periodic")$y
    sq <- spline(seq_along(c(v, v[1])), c(v, v[1]), n = n, method = "periodic")$y
    tibble::tibble(u = sp, v = sq)
  }
  # contorno do cérebro: lobo frontal à esquerda, occipital à direita e o
  # lobo temporal descendo na frente, por baixo
  contorno <- fecha(c(-.95, -.86, -.58, -.12, .35, .74, .95, .9, .66, .36, .1, -.16, -.46, -.72, -.82),
                    c( .05,  .40,  .62,  .70, .65, .46, .15, -.14, -.27, -.24, -.3, -.46, -.49, -.36, -.14))
  # giros: o contorno encolhido, com uma ondulação, em trechos separados
  giros <- purrr::map_dfr(c(.78, .56, .34), \(f) {
    a <- seq(0, 2 * pi, length.out = 160)
    tibble::tibble(u = contorno$u * f + .025 * sin(a * 22 * f), v = contorno$v * f + .05 + .03 * cos(a * 19 * f),
                   parte = paste0("giro", f, "_", cut(seq_along(a), 5, labels = FALSE))) |>
      filter(!grepl("_(2|4)$", parte) | f == .34)
  })
  fissuras <- dplyr::bind_rows(
    tibble::tibble(u = seq(-.74, .12, length.out = 30), v = -.16 + .18 * ((u + .74) / .86)^1.4, parte = "sylvius"),
    tibble::tibble(u = seq(.02, -.16, length.out = 20), v = seq(.69, .02, length.out = 20) + .03 * sin(seq(0, 9, length.out = 20)), parte = "central"))
  a <- seq(0, 2 * pi, length.out = 80)
  cerebelo <- tibble::tibble(u = .6 + .32 * cos(a), v = -.43 + .18 * sin(a))
  folhas <- purrr::map_dfr(c(-.5, -.43, -.36), \(y0) tibble::tibble(
    u = seq(.36, .84, length.out = 30), v = y0 + .015 * sin(seq(0, 12, length.out = 30)), parte = paste0("folha", y0)))
  tronco <- tibble::tibble(u = c(.12, .22, .36, .4), v = c(-.32, -.78, -.76, -.34))
  areas <- dplyr::bind_rows(mutate(tronco, parte = "1tronco"), mutate(cerebelo, parte = "2cerebelo"),
                            mutate(contorno, parte = "3cerebro")) |> mutate(tipo = "area")
  linhas <- dplyr::bind_rows(giros, fissuras, folhas) |> mutate(tipo = "linha")
  dplyr::bind_rows(areas, linhas) |> mutate(x = cx + k * u, y = cy + k * v / 1.4)
}

# ---------------------------------------------------------------------------
# "Galaxy brain": quatro níveis de iluminação, cada um com uma frase do
# slide. O cérebro brilha mais e o fundo vira galáxia a cada nível. `k`
# mostra só os k primeiros níveis (para revelar um por vez nos slides).
# ---------------------------------------------------------------------------
fig_galaxy_brain <- function(k = 4,
                             frases = c(
                               "Médias amostrais são estimadores não\nenviesados de médias populacionais",
                               "Posso comparar médias de\nduas populações",
                               "Existem vários tipos de teste t",
                               "Diferentes testes t são tipos particulares\nde um modelo linear unificado"
                             )) {
  set.seed(42)
  fundos <- c("#f3eee3", "#a9c3d4", "#2e4a6b", "#0b1026")
  niveis <- tibble::tibble(nivel = 1:4, y0 = 4 - (1:4), fundo = fundos, frase = frases)

  t <- seq(0, 2 * pi, length.out = 120)
  esc <- c(.7, .8, .9, 1)          # o cérebro cresce a cada nível
  partes <- purrr::map_dfr(1:4, \(i) cerebro_lateral(8, 4.5 - i, .72 * esc[i]) |> mutate(nivel = i))
  # brilho: círculos concêntricos, mais numerosos a cada nível
  brilho <- purrr::map_dfr(1:4, \(i) tibble::tibble(nivel = i, f = seq(0, 1, length.out = 2 * i + 1)[-1])) |>
    mutate(rx = .95 + f * .22 * nivel, ry = .38 + f * .02 * nivel, alpha = .10 + .03 * nivel)
  circ <- brilho |>
    mutate(id = row_number()) |>
    tidyr::crossing(t = t) |>
    mutate(x = 8 + rx * cos(t), y = 4.5 - nivel + ry * sin(t))
  # estrelas e raios no último nível
  estrelas <- tibble::tibble(x = runif(70, 6.1, 9.9), y = runif(70, 0.03, .97), s = runif(70, .2, 1.4))
  raios <- tibble::tibble(a = seq(0, 2 * pi, length.out = 17)[-17]) |>
    mutate(x = 8 + 1.0 * cos(a), y = .5 + .4 * sin(a), xend = 8 + 1.85 * cos(a), yend = .5 + .46 * sin(a))

  mostra <- function(d) filter(d, nivel <= k)
  ggplot() +
    geom_rect(data = niveis, aes(xmin = 0, xmax = 6, ymin = y0 + .03, ymax = y0 + .97),
              fill = "white", colour = cores[["texto"]], linewidth = .3) +
    geom_rect(data = mostra(niveis), aes(xmin = 6.05, xmax = 10, ymin = y0 + .03, ymax = y0 + .97, fill = I(fundo)),
              colour = cores[["texto"]], linewidth = .3) +
    geom_rect(data = filter(niveis, nivel > k), aes(xmin = 6.05, xmax = 10, ymin = y0 + .03, ymax = y0 + .97),
              fill = cores[["papel"]], colour = "grey80", linewidth = .3, linetype = 2) +
    { if (k >= 4) geom_point(data = estrelas, aes(x, y, size = s), colour = "white", alpha = .8, show.legend = FALSE) } +
    { if (k >= 4) geom_segment(data = raios, aes(x, y, xend = xend, yend = yend), colour = "#ffe9a8", linewidth = 1.1, alpha = .8) } +
    geom_polygon(data = mostra(circ), aes(x, y, group = id, alpha = I(alpha)), fill = "#ffd76a", colour = NA) +
    geom_polygon(data = mostra(filter(partes, tipo == "area")), aes(x, y, group = interaction(nivel, parte),
                 fill = I(c("#e9a8ad", "#f2c0b5", "#ffe2a8", "#fff7dd")[nivel])),
                 colour = "#7a3b44", linewidth = .6) +
    geom_path(data = mostra(filter(partes, tipo == "linha")), aes(x, y, group = interaction(nivel, parte)),
              colour = "#7a3b44", linewidth = .45, lineend = "round") +
    geom_text(data = mostra(niveis), aes(x = .3, y = y0 + .5, label = frase), hjust = 0, size = 5.6,
              lineheight = .95, colour = cores[["texto"]], fontface = ifelse(mostra(niveis)$nivel == 4, "bold", "plain")) +
    scale_size_identity() +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 4), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# Três grupos, duas dummies: α é a média do controle; β1 e β2 são as
# distâncias de cada tratamento até ela.
# ---------------------------------------------------------------------------
fig_dummy3 <- function(seed = 4, n = 10) {
  set.seed(seed)
  grupos <- c("Controle", "Tratamento A", "Tratamento B")
  d <- tibble::tibble(g = rep(1:3, each = n), y = rnorm(3 * n, rep(c(10, 14, 7.5), each = n), 1.3))
  m <- tapply(d$y, d$g, mean)
  setas <- tibble::tibble(g = 2:3, y0 = m[1], y1 = m[2:3],
                          rot = c(sprintf("β1 = %s", num_br(m[2] - m[1], 1)),
                                  sprintf("β2 = %s", num_br(m[3] - m[1], 1))))
  ggplot(d, aes(g, y)) +
    geom_hline(yintercept = m[1], colour = cores[["accent"]], linetype = "22", linewidth = .9) +
    geom_point(position = position_jitter(width = .08, height = 0, seed = 1), size = 2.4,
               alpha = .6, colour = cores[["texto"]]) +
    annotate("segment", x = 1:3 - .25, xend = 1:3 + .25, y = m, yend = m, linewidth = 1.3,
             colour = cores[["texto"]]) +
    geom_segment(data = setas, aes(x = g + .32, xend = g + .32, y = y0, yend = y1), colour = vermelho,
                 linewidth = 1.3, arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
    geom_label(data = setas, aes(x = g + .38, y = (y0 + y1) / 2, label = rot), hjust = 0, size = 5,
               colour = vermelho, fill = cores[["papel"]], linewidth = 0, fontface = "bold") +
    annotate("label", x = 1, y = m[1] + 1.6, size = 5, colour = cores[["accent"]], fill = cores[["papel"]],
             linewidth = 0, fontface = "bold", label = sprintf("α = %s", num_br(m[1], 1))) +
    scale_x_continuous(breaks = 1:3, labels = grupos, limits = c(.6, 3.9)) +
    labs(x = NULL, y = "Y") +
    tema_aula(15) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
}
