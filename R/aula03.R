# Aula 03: organização e apresentação de dados. Tabelas e gráficos
# redesenhados dos slides originais com dados simulados; cada função fixa a
# própria semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))

vermelho <- "#c0392b"
num_br <- function(v, d = 1) format(round(v, d), nsmall = d, decimal.mark = ",",
                                    big.mark = ".")

# ---------------------------------------------------------------------------
# Anatomia de uma tabela: a Tabela 1.1 de Vieira (Censo 2010), com a parte
# indicada em `parte` destacada, como na sequência de slides do original.
# ---------------------------------------------------------------------------
fig_anatomia_tabela <- function(parte = NULL) {
  linhas_tab <- tibble::tibble(
    y = c(4.05, 3.55, 3.05, 2.55),
    c1 = c("Sexo", "Homens", "Mulheres", "Total"),
    c2 = c("População residente", "93.406.990", "97.348.809", "190.755.799"),
    face = c("bold", "plain", "plain", "plain"))
  # caixa (xmin, xmax, ymin, ymax) e ponta da seta de cada parte
  partes <- tibble::tribble(
    ~id,         ~rotulo,                     ~x0,   ~x1,  ~y0,   ~y1,
    "numero",    "Número",                    .15,  1.75,  5.38,  5.82,
    "titulo",    "Título",                    .15,  9.85,  4.55,  5.35,
    "cabecalho", "Cabeçalho",                 .15,  4.05,  3.82,  4.28,
    "linha",     "Linha ⇒ observação",        .15,  4.05,  3.32,  3.78,
    "colunas",   "Colunas ⇒ variáveis",       .15,  1.55,  2.32,  4.28,
    "celula",    "Célula ⇒ um único dado",   1.65,  4.05,  3.32,  3.78,
    "fonte",     "Fonte (apenas caso não tenha\nsido produzida pelo autor)", .15, 5.6, 1.72, 2.12
  )
  g <- ggplot() +
    annotate("rect", xmin = 0, xmax = 10, ymin = 1.4, ymax = 6.1, fill = "#eef0ee") +
    annotate("segment", x = .15, xend = 9.85, y = c(5.92, 4.5), yend = c(5.92, 4.5),
             colour = cores[["accent"]], linewidth = 1.2) +
    annotate("text", x = .2, y = 5.6, label = "Tabela 1.1", hjust = 0,
             fontface = "bold", colour = "#8b1e1e", size = 6.5) +
    annotate("text", x = .2, y = 4.95, hjust = 0, fontface = "bold", size = 5.6,
             lineheight = .95, label = "População residente no Brasil, segundo o sexo, de acordo com o Censo\nDemográfico 2010") +
    annotate("rect", xmin = .2, xmax = 4, ymin = 2.3, ymax = 4.3, fill = "white",
             colour = cores[["texto"]], linewidth = .6) +
    annotate("segment", x = .2, xend = 4, y = c(3.8, 3.3, 2.8), yend = c(3.8, 3.3, 2.8),
             colour = cores[["texto"]], linewidth = .4) +
    annotate("segment", x = 1.6, xend = 1.6, y = 2.3, yend = 4.3,
             colour = cores[["texto"]], linewidth = .4) +
    geom_text(data = linhas_tab, aes(.3, y, label = c1, fontface = face), hjust = 0, size = 4.8) +
    geom_text(data = linhas_tab, aes(1.7, y, label = c2, fontface = face), hjust = 0, size = 4.8) +
    annotate("text", x = .2, y = 1.92, hjust = 0, size = 4.8,
             label = "Fonte: Censo Demográfico 2010. IBGE (2011).")
  if (!is.null(parte)) {
    p <- filter(partes, id == parte)
    g <- g +
      annotate("rect", xmin = p$x0, xmax = p$x1, ymin = p$y0, ymax = p$y1,
               fill = NA, colour = vermelho, linewidth = 1.4) +
      annotate("segment", x = 7, xend = p$x1 + .08, y = 3, yend = (p$y0 + p$y1) / 2,
               colour = vermelho, linewidth = 1.2,
               arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
      annotate("label", x = 7.05, y = 3, label = p$rotulo, hjust = 0,
               fontface = "bold", size = 5, colour = vermelho, fill = "white",
               linewidth = 0)
  }
  g + coord_cartesian(xlim = c(0, 10), ylim = c(1.4, 6.1), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# Um conjunto de dados de girinos, para a tabela de dados quantitativos.
# ---------------------------------------------------------------------------
dados_girinos <- function(n = 36, seed = 3) {
  set.seed(seed)
  tibble::tibble(
    `Nº` = seq_len(n),
    `Espécie` = sample(c("Espécie A", "Espécie B", "Espécie C"), n, replace = TRUE),
    `Hábitat` = sample(c("Poça temporária", "Lagoa permanente"), n, replace = TRUE),
    `Estágio de Gosner` = sample(25:38, n, replace = TRUE),
    `Comprimento total (mm)` = round(rnorm(n, 32, 6), 1),
    `Massa (g)` = round(rlnorm(n, log(.35), .35), 2)
  )
}

# ---------------------------------------------------------------------------
# Galeria dos tipos de gráficos (o painel de ícones do original).
# ---------------------------------------------------------------------------
mini <- function(g, titulo) {
  g + labs(title = titulo, x = NULL, y = NULL) +
    theme_void(base_size = 12) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          plot.title = element_text(hjust = .5, face = "bold", size = 12,
                                    margin = margin(b = 4)),
          legend.position = "none")
}

treemap_retangulos <- function(valores, x0 = 0, y0 = 0, x1 = 1, y1 = 1) {
  # divide os valores em duas metades de soma parecida, corta o retângulo no
  # lado mais comprido e repete em cada metade
  if (length(valores) == 1)
    return(tibble::tibble(id = names(valores), xmin = x0, xmax = x1, ymin = y0, ymax = y1))
  v <- sort(valores, decreasing = TRUE)
  k <- which.min(abs(cumsum(v) - sum(v) / 2))
  a <- v[seq_len(k)]; b <- v[-seq_len(k)]
  f <- sum(a) / sum(v)
  if ((x1 - x0) >= (y1 - y0)) {
    xm <- x0 + f * (x1 - x0)
    bind_rows(treemap_retangulos(a, x0, y0, xm, y1), treemap_retangulos(b, xm, y0, x1, y1))
  } else {
    ym <- y0 + f * (y1 - y0)
    bind_rows(treemap_retangulos(a, x0, y0, x1, ym), treemap_retangulos(b, x0, ym, x1, y1))
  }
}

fig_galeria <- function(seed = 1) {
  set.seed(seed)
  pal <- c(cores[["accent"]], cores[["ferrugem"]], cores[["verde"]], cores[["ocre"]], "#6b4c8a")
  cat5 <- tibble::tibble(k = LETTERS[1:5], v = c(35, 25, 18, 12, 10))
  pizza <- ggplot(cat5, aes("", v, fill = k)) + geom_col(width = 1, colour = "white") +
    coord_polar("y") + scale_fill_manual(values = pal)
  pirulito <- ggplot(cat5, aes(k, v)) + geom_segment(aes(xend = k, y = 0, yend = v),
                                                     colour = cores[["neutro"]]) +
    geom_point(colour = cores[["ferrugem"]], size = 3) + coord_flip()
  tm <- treemap_retangulos(setNames(c(30, 20, 15, 12, 10, 8, 5), letters[1:7]))
  treemap <- ggplot(tm) + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                        fill = id), colour = "white", linewidth = .8) +
    scale_fill_manual(values = rep(pal, 2))
  rosa <- ggplot(tibble::tibble(m = factor(1:12), v = c(3, 4, 6, 9, 12, 14, 13, 11, 8, 6, 4, 3)),
                 aes(m, v)) + geom_col(width = 1, fill = cores[["accent"]], colour = "white") +
    coord_polar()
  xy <- tibble::tibble(x = rnorm(40)) |> mutate(y = .7 * x + rnorm(40, 0, .6))
  dispersao <- ggplot(xy, aes(x, y)) + geom_point(colour = cores[["accent"]], size = 1.6)
  hist <- ggplot(tibble::tibble(x = rnorm(300)), aes(x)) +
    geom_histogram(bins = 15, fill = cores[["accent"]], colour = "white")
  box <- ggplot(tibble::tibble(g = rep(c("a", "b", "c"), each = 40),
                               y = rnorm(120, rep(c(0, 1, .5), each = 40))), aes(g, y)) +
    geom_boxplot(fill = "#cfe0e6", colour = cores[["accent_dark"]], width = .6)
  barra <- ggplot(cat5, aes(k, v)) + geom_col(fill = cores[["verde"]], width = .7)
  linha <- ggplot(tibble::tibble(t = 1:12, y = cumsum(rnorm(12))), aes(t, y)) +
    geom_line(colour = cores[["ferrugem"]], linewidth = 1) + geom_point(colour = cores[["ferrugem"]])
  quali <- mini(pizza, "Pizza (setores)") | mini(pirulito, "Pirulito") | mini(treemap, "Treemap")
  quanti <- (mini(rosa, "Diagrama de rosa") | mini(dispersao, "Dispersão") | mini(hist, "Histograma")) /
    (mini(box, "Boxplot") | mini(barra, "Barras") | mini(linha, "Linha"))
  wrap_elements(quali + plot_annotation(title = "Variáveis qualitativas",
                                        theme = theme(plot.title = element_text(face = "bold", size = 15)))) /
    wrap_elements(quanti + plot_annotation(title = "Variáveis quantitativas",
                                           theme = theme(plot.title = element_text(face = "bold", size = 15)))) +
    plot_layout(heights = c(1, 2)) &
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# Precisão com que se decodifica informação quantitativa (Cleveland &
# McGill 1984): do mais fácil (posição numa escala comum) ao mais difícil.
# Cada linha mostra os mesmos três valores codificados de um jeito.
# ---------------------------------------------------------------------------
fig_precisao <- function() {
  v <- c(.45, .7, 1)
  tipos <- c("Posição numa escala comum", "Posição em escalas não alinhadas",
             "Comprimento", "Ângulo e inclinação", "Área", "Volume",
             "Cor (saturação)")
  y <- rev(seq_along(tipos))
  g <- ggplot() + xlim(-.2, 9) + ylim(.3, 7.8)
  base <- function(i) y[i] - .3
  # 1. posição numa escala comum
  g <- g + annotate("segment", x = 0, xend = 0, y = base(1), yend = base(1) + .65,
                    colour = cores[["neutro"]]) +
    annotate("point", x = c(.6, 1.2, 1.8), y = base(1) + .6 * v, colour = cores[["accent"]], size = 3)
  # 2. escalas não alinhadas
  g <- g + annotate("segment", x = c(.4, 1.1, 1.8), xend = c(.4, 1.1, 1.8),
                    y = base(2) + c(0, .1, -.05), yend = base(2) + c(0, .1, -.05) + .65,
                    colour = cores[["neutro"]]) +
    annotate("point", x = c(.55, 1.25, 1.95), y = base(2) + c(0, .1, -.05) + .6 * v,
             colour = cores[["accent"]], size = 3)
  # 3. comprimento
  g <- g + annotate("segment", x = c(.4, .4, .4), xend = .4 + 1.8 * v,
                    y = base(3) + c(.1, .3, .5), yend = base(3) + c(.1, .3, .5),
                    colour = cores[["accent"]], linewidth = 2.2)
  # 4. ângulo
  ang <- v * pi / 2
  g <- g + annotate("segment", x = c(.4, 1.1, 1.8), y = base(4) + .05,
                    xend = c(.4, 1.1, 1.8) + .55 * cos(ang), yend = base(4) + .05 + .55 * sin(ang),
                    colour = cores[["accent"]], linewidth = 1.6) +
    annotate("segment", x = c(.4, 1.1, 1.8), xend = c(.95, 1.65, 2.35), y = base(4) + .05,
             yend = base(4) + .05, colour = cores[["neutro"]])
  # 5. área
  g <- g + annotate("point", x = c(.5, 1.2, 2), y = base(5) + .32, size = 14 * sqrt(v),
                    colour = cores[["accent"]])
  # 6. volume (cubos em perspectiva simples)
  for (k in 1:3) {
    s <- .45 * v[k]^(1 / 3); x0 <- c(.3, 1, 1.75)[k]; y0 <- base(6) + .05; d <- s * .35
    g <- g + annotate("polygon", x = c(x0, x0 + s, x0 + s, x0), y = c(y0, y0, y0 + s, y0 + s),
                      fill = cores[["accent"]], colour = "white") +
      annotate("polygon", x = c(x0, x0 + d, x0 + s + d, x0 + s), y = c(y0 + s, y0 + s + d, y0 + s + d, y0 + s),
               fill = "#5f9bb3", colour = "white") +
      annotate("polygon", x = c(x0 + s, x0 + s + d, x0 + s + d, x0 + s), y = c(y0, y0 + d, y0 + s + d, y0 + s),
               fill = cores[["accent_dark"]], colour = "white")
  }
  # 7. cor
  g <- g + annotate("tile", x = c(.5, 1.2, 1.9), y = base(7) + .32, width = .55, height = .5,
                    fill = grDevices::colorRampPalette(c("#e3eef2", cores[["accent_dark"]]))(10)[c(4, 7, 10)])
  g + annotate("text", x = 3, y = y, label = tipos, hjust = 0, size = 5.2, colour = cores[["texto"]]) +
    annotate("segment", x = 8.6, xend = 8.6, y = 7.4, yend = .7, linewidth = 1.2,
             colour = cores[["neutro"]], arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
    annotate("text", x = 8.6, y = c(7.7, .4), label = c("Mais fácil", "Mais difícil"),
             fontface = "bold", size = 4.8) +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# Gráficos de pizza: um útil e a piada clássica.
# ---------------------------------------------------------------------------
fig_pizza <- function() {
  causas <- tibble::tibble(
    causa = factor(c("Predação", "Dessecação da poça", "Doença", "Outras"),
                   c("Predação", "Dessecação da poça", "Doença", "Outras")),
    n = c(55, 25, 12, 8))
  a <- ggplot(causas, aes("", n, fill = causa)) +
    geom_col(width = 1, colour = "white") + coord_polar("y") +
    scale_fill_manual(values = c(cores[["accent_dark"]], cores[["accent"]], "#7fb0c2", "#c8dde5"),
                      name = NULL) +
    labs(title = "Causas de morte de girinos\nnuma lagoa (dados fictícios)") +
    theme_void(base_size = 14) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", hjust = .5))
  piada <- tibble::tibble(
    g = factor(c("Pessoas que gostam de\nfazer gráficos de setores",
                 "Pessoas que desejam que\nisso seja uma pizza de verdade"),
               c("Pessoas que gostam de\nfazer gráficos de setores",
                 "Pessoas que desejam que\nisso seja uma pizza de verdade")),
    n = c(3, 97))
  b <- ggplot(piada, aes("", n, fill = g)) +
    geom_col(width = 1, colour = "white") + coord_polar("y") +
    scale_fill_manual(values = c(cores[["ocre"]], "#4a5568"), name = NULL) +
    labs(title = "O gráfico de setores\nfala por si") +
    theme_void(base_size = 14) +
    theme(legend.position = "right", plot.title = element_text(face = "bold", hjust = .5))
  (a | b) & theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# A mesma informação em pizzas e em barras.
# ---------------------------------------------------------------------------
fig_pizza_vs_barras <- function() {
  d <- tibble::tibble(cat = rep(LETTERS[1:5], 2),
                      momento = factor(rep(c("Antes", "Agora"), each = 5), c("Antes", "Agora")),
                      p = c(22, 25, 34, 29, 32, 23, 30, 29, 27, 30) / c(rep(142, 5), rep(139, 5)))
  tons <- c("#d6dde1", "#9aa9b1", "#5f7380", "#2f3f49", "#ecf0f2")
  pz <- ggplot(d, aes("", p, fill = cat)) + geom_col(width = 1, colour = "white") +
    coord_polar("y") + facet_wrap(~momento) +
    scale_fill_manual(values = tons, name = NULL) +
    theme_void(base_size = 14) + theme(strip.text = element_text(face = "bold"))
  br <- ggplot(d, aes(cat, p, fill = momento)) +
    geom_col(position = position_dodge(.75), width = .7) +
    scale_fill_manual(values = c(Antes = "#aab4ba", Agora = "#2f3f49"), name = NULL) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    labs(x = NULL, y = NULL) + tema_aula(14)
  (pz / br) & theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# Pirulito com dois grupos.
# ---------------------------------------------------------------------------
fig_pirulito <- function(seed = 4) {
  set.seed(seed)
  d <- tibble::tibble(sp = paste("Espécie", LETTERS[1:14]),
                      `Poça temporária` = rpois(14, 12),
                      `Lagoa permanente` = rpois(14, 9)) |>
    mutate(sp = reorder(sp, `Poça temporária`))
  dl <- pivot_longer(d, -sp, names_to = "hab", values_to = "n")
  ggplot(d) +
    geom_segment(aes(y = sp, yend = sp, x = `Lagoa permanente`, xend = `Poça temporária`),
                 colour = cores[["neutro_cl"]], linewidth = 1.6) +
    geom_point(data = dl, aes(n, sp, colour = hab), size = 3.6) +
    scale_colour_manual(values = c(`Poça temporária` = cores[["ocre"]],
                                   `Lagoa permanente` = cores[["accent"]]), name = NULL) +
    labs(x = "Número de registros (dados fictícios)", y = NULL) +
    tema_aula(14)
}

# ---------------------------------------------------------------------------
# Treemap: proporção de várias categorias.
# ---------------------------------------------------------------------------
fig_treemap <- function() {
  v <- c(Hylidae = 32, Leptodactylidae = 22, Bufonidae = 9, Microhylidae = 7,
         Odontophrynidae = 5, Phyllomedusidae = 6, Craugastoridae = 4, Outras = 6)
  tm <- treemap_retangulos(v) |>
    mutate(p = v[id] / sum(v),
           lab = paste0(id, "\n", round(100 * p), "%"))
  ggplot(tm) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = id),
              colour = "white", linewidth = 1.5) +
    geom_text(aes((xmin + xmax) / 2, (ymin + ymax) / 2, label = lab), colour = "white",
              fontface = "bold", size = 4.6, lineheight = .9) +
    scale_fill_manual(values = c(cores[["accent_dark"]], cores[["accent"]], cores[["verde"]],
                                 cores[["ocre"]], cores[["ferrugem"]], "#6b4c8a", "#5f7380",
                                 cores[["neutro"]]), guide = "none") +
    coord_equal(ratio = .55, expand = FALSE) +
    labs(caption = "Proporção de espécies por família numa taxocenose fictícia de anuros.") +
    theme_void(base_size = 13) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}

# ---------------------------------------------------------------------------
# Dispersão: altura e comprimento do pé de 40 pessoas.
# ---------------------------------------------------------------------------
fig_dispersao <- function(seed = 9, n = 40) {
  set.seed(seed)
  d <- tibble::tibble(pe = rnorm(n, 25.5, 1.6)) |>
    mutate(altura = 60 + 4.3 * pe + rnorm(n, 0, 4.5))
  base <- ggplot(d, aes(pe, altura)) +
    geom_point(colour = cores[["accent"]], size = 2.6) +
    labs(x = "Comprimento do pé (cm)", y = "Altura (cm)") + tema_aula(14)
  (base + labs(title = "40 pares de dados")) |
    (base + geom_smooth(method = "lm", se = FALSE, colour = cores[["ferrugem"]]) +
       labs(title = "Com a reta de regressão"))
}

# ---------------------------------------------------------------------------
# Diagrama de pontos: frequência cardíaca de repouso.
# ---------------------------------------------------------------------------
fig_pontos <- function(seed = 2, n = 150) {
  set.seed(seed)
  d <- tibble::tibble(fc = round(rnorm(n, 70, 11)))
  ggplot(d, aes(fc)) +
    geom_dotplot(binwidth = 1.5, dotsize = .9, stackratio = 1.1, method = "histodot",
                 fill = cores[["texto"]], colour = NA) +
    scale_y_continuous(NULL, breaks = NULL) +
    labs(x = "Frequência cardíaca de repouso (batimentos por minuto)",
         title = sprintf("Diagrama de pontos de %d pessoas", n)) +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Histograma e tabela de frequência (o exemplo do fósforo em folhas, Zar).
# ---------------------------------------------------------------------------
dados_fosforo <- function(seed = 5, n = 130) {
  set.seed(seed)
  round(pmin(pmax(rnorm(n, 8.7, .21), 8.16), 9.24), 2)
}

classes_fosforo <- seq(8.15, 9.25, by = .1)

tabela_frequencia <- function(x = dados_fosforo(), breaks = classes_fosforo,
                              destaque = NULL) {
  cl <- cut(x, breaks, right = FALSE, include.lowest = TRUE)
  f <- as.integer(table(cl))
  rot <- sprintf("%s–%s", num_br(head(breaks, -1), 2), num_br(tail(breaks, -1) - .01, 2))
  d <- tibble::tibble(`Fósforo (mg/g de folha)` = rot, `Frequência` = f,
                      `Acumulada (dos menores)` = cumsum(f),
                      `Acumulada (dos maiores)` = rev(cumsum(rev(f))))
  if (!is.null(destaque)) {
    d <- mutate(d, across(everything(), as.character))
    d[destaque, ] <- lapply(d[destaque, ], function(v) paste0("**", v, "**"))
  }
  d
}

fig_histograma_zar <- function(x = dados_fosforo(), destaque = NULL) {
  d <- tibble::tibble(x = x) |>
    mutate(classe = cut(x, classes_fosforo, right = FALSE, include.lowest = TRUE),
           k = as.integer(classe), dest = if (is.null(destaque)) FALSE else k %in% destaque)
  ggplot(d, aes(x, fill = dest)) +
    geom_histogram(breaks = classes_fosforo, colour = "white") +
    scale_fill_manual(values = c(`FALSE` = cores[["neutro"]], `TRUE` = vermelho), guide = "none") +
    scale_x_continuous(breaks = classes_fosforo, labels = num_br(classes_fosforo, 2)) +
    labs(x = "Fósforo (mg/g de folha)", y = "Frequência") +
    tema_aula(14) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ---------------------------------------------------------------------------
# A mesma turma com três larguras de classe (o "change it up" da atividade).
# ---------------------------------------------------------------------------
dados_alturas <- function(seed = 12, n = 40) {
  set.seed(seed)
  sexo <- sample(c("F", "M"), n, replace = TRUE)
  round(ifelse(sexo == "F", rnorm(n, 162, 6.5), rnorm(n, 175, 7)))
}

fig_histograma_larguras <- function(x = dados_alturas(), larguras = c(2, 5, 10)) {
  d <- purrr::map_dfr(larguras, \(w) tibble::tibble(x = x, w = w)) |>
    mutate(painel = factor(sprintf("Classes de %d cm", w), sprintf("Classes de %d cm", larguras)))
  ggplot(d, aes(x)) +
    purrr::map(larguras, \(w) geom_histogram(data = filter(d, w == !!w), binwidth = w,
                                             boundary = 140, fill = cores[["ocre"]],
                                             colour = "white")) +
    facet_wrap(~painel, scales = "free_y") +
    labs(x = "Altura (cm)", y = "Número de estudantes",
         caption = sprintf("As mesmas %d alturas (dados simulados) nos três painéis.", length(x))) +
    tema_aula(14)
}

# ---------------------------------------------------------------------------
# Anatomia de um boxplot.
# ---------------------------------------------------------------------------
fig_boxplot_anatomia <- function(seed = 21) {
  set.seed(seed)
  y <- c(rnorm(48, .25, .035), .40, .42)
  q <- quantile(y, c(.25, .5, .75)); iqr <- q[[3]] - q[[1]]
  sup <- max(y[y <= q[[3]] + 1.5 * iqr]); inf <- min(y[y >= q[[1]] - 1.5 * iqr])
  rot <- tibble::tibble(
    y = c(sup, q[[3]], q[[2]], q[[1]], inf),
    lab = c("Limite superior (maior valor até Q3 + 1,5 × IIQ)", "Q3 (percentil 75%)",
            "Mediana", "Q1 (percentil 25%)", "Limite inferior (menor valor até Q1 − 1,5 × IIQ)"))
  ggplot(tibble::tibble(g = "", y = y), aes(g, y)) +
    geom_boxplot(width = .35, fill = "#f2b59a", colour = cores[["texto"]], linewidth = .7,
                 outlier.colour = vermelho, outlier.size = 3) +
    annotate("segment", x = 1.25, xend = 1.45, y = rot$y, yend = rot$y, colour = cores[["neutro"]]) +
    annotate("text", x = 1.48, y = rot$y, label = rot$lab, hjust = 0, size = 4.8) +
    annotate("text", x = 1.1, y = max(y), label = "Valores extremos (outliers)", hjust = 0,
             size = 4.8, colour = vermelho) +
    annotate("segment", x = .7, xend = .7, y = q[[1]], yend = q[[3]], colour = cores[["accent"]],
             linewidth = 1, arrow = arrow(ends = "both", length = unit(.2, "cm"))) +
    annotate("text", x = .66, y = q[[2]], label = "Intervalo\ninterquartil\n(IIQ = Q3 − Q1)",
             hjust = 1, size = 4.4, colour = cores[["accent"]], lineheight = .9) +
    coord_cartesian(xlim = c(.2, 3.2), clip = "off") +
    labs(x = NULL, y = "Comprimento do espinho (mm)") +
    tema_aula(15) + theme(panel.grid.major.x = element_blank())
}

# ---------------------------------------------------------------------------
# Histograma e boxplot de três formas de distribuição.
# ---------------------------------------------------------------------------
fig_distribuicoes_boxplot <- function(seed = 8, n = 200) {
  set.seed(seed)
  d <- tibble::tibble(
    forma = factor(rep(c("Assimétrica", "Normal", "Uniforme"), each = n),
                   c("Assimétrica", "Normal", "Uniforme")),
    x = c(rgamma(n, 2, .5), rnorm(n, 10, 2), runif(n, 0, 20)))
  h <- ggplot(d, aes(x)) + geom_histogram(bins = 15, fill = cores[["neutro"]], colour = "white") +
    facet_wrap(~forma, scales = "free") + labs(x = NULL, y = "Frequência") + tema_aula(14)
  b <- ggplot(d, aes(x, "")) + geom_boxplot(width = .5, fill = "#f2b59a", colour = cores[["texto"]]) +
    facet_wrap(~forma, scales = "free_x") + labs(x = NULL, y = NULL) + tema_aula(14) +
    theme(strip.text = element_blank())
  h / b + plot_layout(heights = c(3, 1))
}

# ---------------------------------------------------------------------------
# Barras para frequências de uma variável qualitativa.
# ---------------------------------------------------------------------------
fig_barras_freq <- function() {
  d <- tibble::tibble(sitio = factor(c("Vegetação emergente", "Margem", "Água aberta", "Fora da poça"),
                                     c("Vegetação emergente", "Margem", "Água aberta", "Fora da poça")),
                      n = c(41, 26, 9, 4)) |>
    mutate(p = n / sum(n))
  ggplot(d, aes(sitio, p)) +
    geom_col(fill = cores[["verde"]], width = .65) +
    geom_text(aes(label = paste0(round(100 * p), "%")), vjust = -.4, size = 5) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                       expand = expansion(mult = c(0, .12))) +
    labs(x = "Sítio de vocalização", y = "Porcentagem dos machos",
         caption = "80 machos observados (dados fictícios).") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Média com desvio-padrão e com erro-padrão.
# ---------------------------------------------------------------------------
fig_barras_dp_ep <- function(seed = 21, n = 50) {
  set.seed(seed)
  y <- rnorm(n, .25, .045)
  m <- mean(y); dp <- sd(y); ep <- dp / sqrt(n)
  d <- tibble::tibble(tipo = factor(c("Média ± desvio-padrão", "Média ± erro-padrão"),
                                    c("Média ± desvio-padrão", "Média ± erro-padrão")),
                      m = m, e = c(dp, ep))
  ggplot(d, aes(tipo, m)) +
    geom_col(fill = "#c5e0d6", colour = cores[["verde"]], width = .55) +
    geom_errorbar(aes(ymin = m, ymax = m + e), width = .15, linewidth = .9) +
    labs(x = NULL, y = "Comprimento do espinho (mm)",
         caption = sprintf("n = %d; o erro-padrão é o desvio-padrão dividido por √n.", n)) +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Gráfico de linha: médias ± erro-padrão ao longo do tempo, duas estações.
# ---------------------------------------------------------------------------
fig_linha <- function() {
  d <- tibble::tibble(
    estacao = rep(c("Primavera", "Verão"), each = 4),
    mes = rep(1:4, 2),
    m = c(9, 8.2, 6.5, 5.6, 7.4, 5.6, 4.8, 4.2),
    ep = c(.6, .5, .5, .4, .5, .5, .4, .4))
  ggplot(d, aes(mes, m, colour = estacao, shape = estacao)) +
    geom_errorbar(aes(ymin = m - ep, ymax = m + ep), width = .08, linewidth = .7) +
    geom_line(linewidth = 1.1) + geom_point(size = 3.4, fill = "white") +
    scale_shape_manual(values = c(Primavera = 16, Verão = 22), name = NULL) +
    scale_colour_manual(values = c(Primavera = cores[["texto"]], Verão = cores[["neutro"]]), name = NULL) +
    scale_x_continuous(breaks = 1:4, labels = paste("Mês", 1:4)) +
    labs(x = NULL, y = "Densidade de girinos\n(média ± erro-padrão)",
         caption = "Dados fictícios.") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Como escolher o gráfico: árvore redesenhada do esquema de R.S. Olson
# (inspirado em A. Abela; tradução do IBPAD), com uma miniatura de gráfico
# em cada folha, como no original. Os dados das miniaturas são inventados.
# ---------------------------------------------------------------------------
icone <- function(g, nome, cond, cor) {
  g + labs(title = nome, subtitle = cond) +
    theme_void(base_size = 10) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          panel.background = element_rect(fill = "white", colour = cor, linewidth = .6),
          plot.title = element_text(size = 10.5, face = "bold", colour = cor, hjust = .5),
          plot.subtitle = element_text(size = 9, colour = "#4a5568", hjust = .5,
                                       margin = margin(b = 2)),
          plot.margin = margin(3, 4, 3, 4), legend.position = "none",
          aspect.ratio = .62)
}

fig_escolha_grafico <- function() {
  cc <- c(Comparação = cores[["accent"]], Relação = cores[["verde"]],
          Distribuição = cores[["ocre"]], Composição = cores[["ferrugem"]])
  tons <- function(cor) grDevices::colorRampPalette(c("white", cor))(5)[2:5]
  k4 <- tibble::tibble(x = factor(1:4), y = c(3, 5, 4, 6))
  t8 <- tibble::tibble(t = 1:8, y = c(2, 3, 2.6, 4, 3.6, 5, 4.7, 6))
  pilha <- tidyr::expand_grid(x = factor(1:4), g = factor(1:3)) |>
    mutate(y = c(3, 2, 1, 2, 3, 2, 4, 2, 2, 3, 3, 3))
  area <- tidyr::expand_grid(t = 1:8, g = factor(1:3)) |>
    mutate(y = rep(c(2, 1.5, 1), 8) + rep(c(0, .3, .5, .4, .8, 1, .9, 1.2), each = 3) * c(1, .5, .2))

  # --- Comparação
  cor <- cc[["Comparação"]]
  comp <- list(
    icone(ggplot(k4, aes(x, y)) + geom_col(fill = cor, width = .6), "Colunas", "poucas categorias", cor),
    icone(ggplot(tibble::tibble(x = factor(1:6), y = c(6, 5, 4.5, 3, 2.5, 1.5)), aes(y, x)) +
            geom_col(fill = cor, width = .6), "Barras", "vários itens", cor),
    icone(ggplot(tibble::tibble(x0 = c(0, 1.2, 3), x1 = c(1, 2.8, 3.6), y = c(3, 5, 4))) +
            geom_rect(aes(xmin = x0, xmax = x1, ymin = 0, ymax = y), fill = cor),
          "Colunas de\nlargura variável", "duas variáveis por item", cor),
    icone(ggplot(tidyr::expand_grid(c = 1:3, r = 1:3) |> mutate(h = runif(9, .2, .8))) +
            geom_tile(aes(c, r), fill = NA, colour = "#c9c2b3", width = 1, height = 1) +
            geom_rect(aes(xmin = c - .3, xmax = c + .3, ymin = r - .4, ymax = r - .4 + h), fill = cor),
          "Tabela com\ngráficos", "muitas categorias", cor),
    icone(ggplot(t8, aes(t, y)) + geom_line(colour = cor, linewidth = 1), "Linha",
          "ao longo do tempo", cor),
    icone(ggplot(tibble::tibble(a = seq(0, 2 * pi, length.out = 7)[-7] + pi / 2,
                                r = c(4, 6, 3, 5, 2.5, 4.5))) +
            geom_polygon(aes(r * cos(a), r * sin(a)), fill = NA, colour = cor, linewidth = 1) +
            annotate("segment", x = 0, y = 0, xend = 6.3 * cos(seq(0, 2 * pi, length.out = 7)[-7] + pi / 2),
                     yend = 6.3 * sin(seq(0, 2 * pi, length.out = 7)[-7] + pi / 2),
                     colour = "#d8d2c4") +
            coord_equal(), "Radar", "dados cíclicos", cor))

  # --- Relação
  cor <- cc[["Relação"]]
  set.seed(2)
  xy <- tibble::tibble(x = runif(15)) |> mutate(y = x + rnorm(15, 0, .15), s = runif(15))
  rel <- list(
    icone(ggplot(xy, aes(x, y)) + geom_point(colour = cor, size = 1.3), "Dispersão", "duas variáveis", cor),
    icone(ggplot(xy, aes(x, y, size = s)) + geom_point(colour = cor, alpha = .6) +
            scale_size(range = c(1, 5)), "Bolhas", "três variáveis", cor))

  # --- Distribuição
  cor <- cc[["Distribuição"]]
  dist <- list(
    icone(ggplot(tibble::tibble(x = 1:7, y = c(1, 3, 5, 7, 5, 3, 1)), aes(x, y)) +
            geom_col(fill = cor, width = 1, colour = "white"), "Histograma\nde colunas",
          "poucas observações", cor),
    icone(ggplot(tibble::tibble(x = seq(-3, 3, .1)), aes(x, dnorm(x))) +
            geom_area(fill = cor, alpha = .35) + geom_line(colour = cor, linewidth = 1),
          "Histograma\nde linha", "muitas observações", cor),
    icone(ggplot(xy, aes(x, y)) + geom_point(colour = cor, size = 1.3), "Dispersão",
          "duas variáveis", cor),
    icone(ggplot(tidyr::expand_grid(x = seq(-2, 2, .1), y = seq(-2, 2, .1)) |>
                   mutate(z = exp(-(x^2 + y^2) / 2)), aes(x, y, fill = z)) +
            geom_raster() + scale_fill_gradient(low = "white", high = cor),
          "Superfície", "três variáveis", cor))

  # --- Composição
  cor <- cc[["Composição"]]
  tc <- scale_fill_manual(values = tons(cor)[2:4])
  compo <- list(
    icone(ggplot(pilha, aes(x, y, fill = g)) + geom_col(width = .6) + tc,
          "Colunas\nempilhadas", "poucas obs. no tempo", cor),
    icone(ggplot(pilha, aes(x, y, fill = g)) + geom_col(width = .6, position = "fill") + tc,
          "Colunas\nempilhadas 100%", "só diferenças relativas", cor),
    icone(ggplot(area, aes(t, y, fill = g)) + geom_area() + tc,
          "Área\nempilhada", "muitas obs. no tempo", cor),
    icone(ggplot(area, aes(t, y, fill = g)) + geom_area(position = "fill") + tc,
          "Área\nempilhada 100%", "só diferenças relativas", cor),
    icone(ggplot(tibble::tibble(g = factor(1:3), y = c(5, 3, 2)), aes("", y, fill = g)) +
            geom_col(width = 1, colour = "white") + coord_polar("y") + tc,
          "Pizza", "fração simples do total", cor) + theme(aspect.ratio = 1),
    icone(ggplot(tibble::tibble(x = 1:4, y0 = c(0, 4, 6, 0), y1 = c(4, 6, 5, 5))) +
            geom_rect(aes(xmin = x - .35, xmax = x + .35, ymin = y0, ymax = y1), fill = cor),
          "Cascata", "acumulação ou subtração", cor),
    icone(ggplot(tibble::tibble(x = factor(rep(1:3, each = 3)), g = factor(rep(1:3, 3)),
                                y = c(5, 3, 2, 2, 4, 4, 3, 3, 4)), aes(x, y, fill = g)) +
            geom_col(position = "fill", width = .8, colour = "white") + tc,
          "Empilhado 100%\ncom subconjuntos", "conjuntos de conjuntos", cor))

  # cabeçalho: a pergunta e as quatro categorias, alinhadas com os blocos
  larg <- c(2, 1, 2, 3)
  centros <- (cumsum(larg) - larg / 2)
  cab <- tibble::tibble(cat = names(cc), x = centros, cor = unname(cc))
  topo <- ggplot() +
    annotate("segment", x = sum(larg) / 2, y = 1.55, xend = cab$x, yend = .62,
             colour = cores[["neutro"]], linewidth = 1,
             arrow = arrow(length = unit(.22, "cm"), type = "closed")) +
    annotate("label", x = sum(larg) / 2, y = 1.75, label = "O que você deseja mostrar?",
             fill = cores[["texto"]], colour = "white", fontface = "bold", size = 5.4,
             label.padding = unit(.5, "lines"), linewidth = 0) +
    geom_label(data = cab, aes(x, .4, label = cat, fill = cor), colour = "white",
               fontface = "bold", size = 5, label.padding = unit(.4, "lines"), linewidth = 0) +
    scale_fill_identity() +
    coord_cartesian(xlim = c(0, sum(larg)), ylim = c(.2, 2), expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))

  bloco <- function(icones, ncol, nrow = 3) {
    vazio <- nrow * ncol - length(icones)
    wrap_plots(c(icones, rep(list(plot_spacer()), vazio)), ncol = ncol, nrow = nrow)
  }
  corpo <- wrap_plots(
    bloco(comp, 2), bloco(rel, 1), bloco(dist, 2), bloco(compo, 3),
    nrow = 1, widths = larg)
  (topo / corpo + plot_layout(heights = c(.7, 4))) &
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
}
