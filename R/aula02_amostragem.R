# Aula 02, partes 3 e 4: escala, controles e técnicas de amostragem.
# Dados simulados; cada função fixa a própria semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/aula02_pergunta.R"))   # fig_diagrama()

fundo_void <- function(base = 15) {
  theme_void(base_size = base) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          strip.text = element_text(face = "bold", margin = margin(b = 6)),
          plot.title = element_text(face = "bold", hjust = .5,
                                    margin = margin(b = 6)),
          legend.position = "bottom")
}

# ---------------------------------------------------------------------------
# Grão e extensão.
# ---------------------------------------------------------------------------
fig_grao_extensao <- function(seed = 3) {
  set.seed(seed)
  lado_ext <- c(`Extensão pequena` = 4, `Extensão grande` = 9.4)
  lado_gr  <- c(`Grão pequeno` = .3, `Grão grande` = 1.1)
  casos <- expand.grid(grao = names(lado_gr), ext = names(lado_ext),
                       stringsAsFactors = FALSE)
  parc <- purrr::pmap_dfr(casos, function(grao, ext) {
    L <- lado_ext[[ext]]; g <- lado_gr[[grao]]; o <- (10 - L) / 2
    cx <- o + runif(6, g / 2, L - g / 2)
    cy <- o + runif(6, g / 2, L - g / 2)
    tibble::tibble(grao, ext, xmin = cx - g / 2, xmax = cx + g / 2,
                   ymin = cy - g / 2, ymax = cy + g / 2)
  })
  extensao <- casos |>
    mutate(L = lado_ext[ext], xmin = (10 - L) / 2, xmax = xmin + L,
           ymin = xmin, ymax = xmax)
  fat <- function(d) mutate(d, grao = factor(grao, names(lado_gr)),
                            ext = factor(ext, names(lado_ext)))
  ggplot(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 10,
             fill = "#eef0e6") +
    geom_rect(data = fat(extensao), fill = NA,
              colour = cores[["accent_dark"]], linetype = "22",
              linewidth = .8) +
    geom_rect(data = fat(parc), fill = cores[["verde"]]) +
    facet_grid(ext ~ grao, switch = "y") +
    coord_equal() +
    fundo_void(14) +
    theme(strip.text.y.left = element_text(angle = 90, face = "bold"))
}

# ---------------------------------------------------------------------------
# Tratamento confundido com um gradiente de temperatura vs. intercalado.
# ---------------------------------------------------------------------------
fig_gradiente_confundido <- function(seed = 8) {
  set.seed(seed)
  fundo <- expand.grid(x = seq(.05, 9.95, by = .1), y = seq(-.85, 5.95, by = .1))
  pos <- expand.grid(col = 1:6, lin = 1:3) |>
    mutate(x = .9 + (col - 1) * 1.65, y = 1.1 + (lin - 1) * 1.9)
  a <- mutate(pos, trat = ifelse(col <= 3, "Néctar removido", "Controle"),
              desenho = "Confundido")
  b <- mutate(pos, trat = sample(rep(c("Néctar removido", "Controle"), 9)),
              desenho = "Intercalado e sorteado")
  d <- bind_rows(a, b) |>
    mutate(desenho = factor(desenho, unique(desenho)), id = row_number())
  # beija-flor (silhueta do PhyloPic); com néctar removido, sob um "proibido"
  bf <- function(d, alt = 1.2) purrr::pmap_dfr(d, function(x, y, id, ...)
    mutate(silhueta("beija-flor", x, y - alt / 2, alt, id), ...))
  proibido <- function(d, r = .72) {
    ang <- seq(0, 2 * pi, length.out = 60)
    list(
      geom_path(data = tidyr::crossing(filter(d, trat == "Néctar removido"),
                                       a = ang),
                aes(x + r * cos(a), y + r * sin(a), group = id),
                colour = "#b42318", linewidth = 1.3),
      geom_segment(data = filter(d, trat == "Néctar removido"),
                   aes(x - r * .71, y + r * .71, xend = x + r * .71,
                       yend = y - r * .71),
                   colour = "#b42318", linewidth = 1.3)
    )
  }
  g <- ggplot() +
    geom_raster(data = tidyr::crossing(fundo, desenho = unique(d$desenho)),
                aes(x, y, fill = x), interpolate = TRUE) +
    scale_fill_gradient(low = "#d98a3d", high = "#8fb3c4", guide = "none") +
    geom_polygon(data = bf(d, 1.3), aes(x, y, group = id, subgroup = parte),
                 fill = cores[["texto"]], rule = "evenodd") +
    proibido(d) +
    geom_text(data = tidyr::crossing(
                tibble::tibble(x = c(.2, 9.8), h = c(0, 1),
                               lab = c("mais quente", "mais frio")),
                desenho = unique(d$desenho)),
              aes(x, -.5, label = lab, hjust = h), size = 4.6,
              colour = cores[["texto"]]) +
    facet_wrap(~desenho, ncol = 2) +
    coord_equal(clip = "off") +
    fundo_void(14)
  # legenda desenhada à mão, com as mesmas silhuetas
  leg <- tibble::tibble(x = c(0, 5), y = 0, id = 1:2,
                        trat = c("Controle", "Néctar removido"))
  l <- ggplot() +
    geom_polygon(data = bf(leg, 1.2), aes(x, y, group = id, subgroup = parte),
                 fill = cores[["texto"]], rule = "evenodd") +
    proibido(leg) +
    geom_text(data = leg, aes(x + .95, y, label = trat), hjust = 0, size = 5,
              colour = cores[["texto"]]) +
    coord_equal(xlim = c(-.8, 9.6), ylim = c(-.8, .8), clip = "off") +
    fundo_void(14)
  g / l + plot_layout(heights = c(1, .16))
}

# ---------------------------------------------------------------------------
# Classificação das técnicas de amostragem (segundo Vieira, Estatística
# básica, cap. 7).
# ---------------------------------------------------------------------------
fig_arvore_tecnicas <- function() {
  nos <- tibble::tribble(
    ~id,  ~rotulo,                 ~x,   ~y,  ~cor,
    "A",  "Técnicas de\namostragem", 5.45, 2.3, cores[["accent_dark"]],
    "P",  "Probabilística",        1.1,  1.15, cores[["accent"]],
    "S",  "Semi-\nprobabilística",  6.5,  1.15, cores[["accent"]],
    "N",  "Não\nprobabilística",   10.9, 1.15, cores[["accent"]],
    "P1", "Aleatória\nsimples",    0,    0,   cores[["verde"]],
    "P2", "Estratificada",         2.2,  0,   cores[["verde"]],
    "S1", "Sistemática",           4.4,  0,   cores[["verde"]],
    "S2", "Por\nconglomerados",    6.5,  0,   cores[["neutro"]],
    "S3", "Por cotas",             8.6,  0,   cores[["neutro"]],
    "N1", "Por\nconveniência",     10.9, 0,   cores[["ferrugem"]]
  )
  arestas <- tibble::tibble(
    de   = c("A", "A", "A", "P", "P", "S", "S", "S", "N"),
    para = c("P", "S", "N", "P1", "P2", "S1", "S2", "S3", "N1"),
    rotulo = NA)
  fig_diagrama(nos, arestas, larg = 2.05, alt = .72, base = 4.6)
}

# ---------------------------------------------------------------------------
# Paisagem simulada com três tipos de solo e a biomassa de árvores em cada
# célula. É a "população" de onde as técnicas de amostragem tiram amostras.
# ---------------------------------------------------------------------------
paisagem <- function(k = 16, seed = 21) {
  set.seed(seed)
  g <- expand.grid(x = 1:k, y = 1:k)
  ruido <- rnorm(nrow(g), 0, .9)
  solo <- with(g, case_when(
    x + .35 * y + ruido < 7.5        ~ "Argiloso",
    y + .3 * x + ruido > 13.5        ~ "Arenoso",
    TRUE                             ~ "Rochoso"))
  media <- c(Argiloso = 140, Rochoso = 60, Arenoso = 95)
  g |>
    mutate(id = row_number(),
           solo = factor(solo, levels = names(media)),
           biomassa = media[as.character(solo)] + rnorm(n(), 0, 15))
}

amostra_simples <- function(p, n = 16) sample(p$id, n)

amostra_estratificada <- function(p, n = 16) {
  prop <- c(table(p$solo)) / nrow(p)   # vetor nomeado por tipo de solo
  aloc <- setNames(pmax(1, round(n * prop)), names(prop))
  # ajusta o arredondamento para somar exatamente n
  while (sum(aloc) != n) {
    i <- if (sum(aloc) > n) which.max(aloc) else which.max(n * prop - aloc)
    aloc[i] <- aloc[i] + sign(n - sum(aloc))
  }
  unlist(lapply(names(aloc), function(s)
    sample(p$id[p$solo == s], aloc[[s]])))
}

amostra_sistematica <- function(p, passo = 4) {
  ini <- sample(1:passo, 2, replace = TRUE)
  p$id[(p$x - ini[1]) %% passo == 0 & (p$y - ini[2]) %% passo == 0]
}

fig_paisagem_amostra <- function(p, ids = NULL, titulo = NULL, legenda = FALSE) {
  g <- ggplot(p, aes(x, y)) +
    geom_tile(aes(fill = solo), colour = cores[["papel"]], linewidth = .25) +
    scale_fill_manual(values = cores_solo, name = "Solo") +
    coord_equal() +
    labs(title = titulo) +
    fundo_void(14)
  if (!legenda) g <- g + theme(legend.position = "none")
  if (!is.null(ids))
    g <- g + geom_tile(data = filter(p, id %in% ids), fill = cores[["accent"]],
                       colour = cores[["accent_dark"]], linewidth = .6,
                       width = .82, height = .82)
  g
}

fig_tecnicas <- function(seed = 2) {
  p <- paisagem()
  set.seed(seed)
  (fig_paisagem_amostra(p, amostra_simples(p), "Aleatória simples") |
   fig_paisagem_amostra(p, amostra_estratificada(p), "Estratificada") |
   fig_paisagem_amostra(p, amostra_sistematica(p), "Sistemática")) /
    guide_area() +
    plot_layout(heights = c(1, .08), guides = "collect") &
    theme(legend.position = "bottom")
}

# Repete cada técnica muitas vezes e compara as estimativas da média.
simula_tecnicas <- function(reps = 2000, seed = 99) {
  p <- paisagem()
  set.seed(seed)
  est <- function(f) replicate(reps, mean(p$biomassa[match(f(p), p$id)]))
  tibble::tibble(
    `Aleatória simples` = est(amostra_simples),
    Estratificada       = est(amostra_estratificada),
    `Sistemática`       = est(amostra_sistematica)
  ) |>
    pivot_longer(everything(), names_to = "tecnica", values_to = "media") |>
    mutate(tecnica = factor(tecnica, levels = c("Aleatória simples",
                                                "Estratificada",
                                                "Sistemática")),
           verdade = mean(p$biomassa))
}

fig_distribuicao_estimativas <- function(sim = simula_tecnicas()) {
  resumo <- sim |>
    group_by(tecnica) |>
    summarise(dp = sd(media), vies = mean(media) - first(verdade),
              .groups = "drop") |>
    mutate(lab = sprintf("desvio-padrão das\nestimativas: %.1f", dp))
  ggplot(sim, aes(media)) +
    geom_histogram(binwidth = 2, fill = cores[["accent"]], colour = "white",
                   linewidth = .2) +
    geom_vline(aes(xintercept = verdade), colour = cores[["ferrugem"]],
               linewidth = 1.1) +
    geom_text(data = resumo, aes(x = -Inf, y = Inf, label = lab),
              hjust = -.06, vjust = 1.3, size = 4.4, lineheight = .95,
              colour = cores[["texto"]]) +
    facet_wrap(~tecnica, ncol = 3) +
    labs(x = "Biomassa média estimada a partir de 16 parcelas (t/ha)",
         y = "Nº de amostras (de 2000)") +
    tema_aula(14)
}

# ---------------------------------------------------------------------------
# Amostragem sistemática num ambiente periódico: o intervalo coincide com o
# período e todas as parcelas caem no mesmo ponto do ciclo.
# ---------------------------------------------------------------------------
fig_sistematica_periodica <- function(seed = 1) {
  set.seed(seed)
  f <- function(x) 10 + 4 * sin(2 * pi * x / 10)
  curva <- tibble::tibble(x = seq(0, 100, by = .2), y = f(x))
  verdade <- mean(curva$y)
  a1 <- tibble::tibble(x = seq(2.5, 100, by = 10), y = f(x),
                       desenho = "A cada 10 m (igual ao período)")
  a2 <- tibble::tibble(x = seq(1, 100, by = 7), y = f(x),
                       desenho = "A cada 7 m")
  d <- bind_rows(a1, a2) |> mutate(desenho = factor(desenho, unique(desenho)))
  est <- d |> group_by(desenho) |>
    summarise(m = mean(y), .groups = "drop") |>
    mutate(lab = sprintf("média estimada = %.1f   (verdadeira = %.1f)",
                         m, verdade))
  ggplot(d, aes(x, y)) +
    geom_line(data = tidyr::crossing(curva, desenho = levels(d$desenho)),
              colour = cores[["neutro"]], linewidth = .8) +
    geom_hline(yintercept = verdade, linetype = "22",
               colour = cores[["neutro"]]) +
    geom_hline(data = est, aes(yintercept = m), colour = cores[["ferrugem"]],
               linewidth = 1) +
    geom_point(colour = cores[["accent"]], size = 3.4) +
    geom_text(data = est, aes(x = 50, y = 16.2, label = lab), size = 4.6,
              colour = cores[["texto"]]) +
    facet_wrap(~desenho, ncol = 1) +
    scale_y_continuous(limits = c(5, 17)) +
    labs(x = "Posição ao longo do transecto (m)",
         y = "Umidade do solo (%)") +
    tema_aula(14)
}
