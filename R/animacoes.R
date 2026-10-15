# Animações dos mínimos quadrados (Aulas 9 e 11). Cada quadro é um ggplot;
# o GIF é montado pelo ffmpeg, sem pacote extra do R. Os GIFs prontos ficam
# em assets/ e os slides só os mostram. Para refazer (precisa do ffmpeg
# instalado):
#   source(here::here("R/animacoes.R"))
#   gera_animacoes()

source(here::here("R/aula09.R"))   # pontos_dispersao(), num_br(), cores e tema

# ---------------------------------------------------------------------------
# Animação 1: os resíduos ao quadrado vão se somando, primeiro numa reta
# qualquer e depois na reta de mínimos quadrados.
# ---------------------------------------------------------------------------
quadro_soma <- function(d, a, b, k, titulo) {
  q <- d |> mutate(i = row_number(), pred = a + b * x, e = y - pred)
  feitos <- filter(q, i <= k)
  parcial <- sum(feitos$e^2)
  termos <- if (k == 0) "" else if (k <= 3) paste0("d", seq_len(k), "²", collapse = " + ")
            else paste0("d1² + d2² + ... + d", k, "²")
  formula <- if (k == 0) "SQ =" else paste0("SQ = ", termos, " = ", num_br(parcial, 1))
  ggplot(q, aes(x, y)) +
    geom_rect(data = feitos, aes(xmin = x, xmax = x + abs(e), ymin = pmin(y, pred), ymax = pmax(y, pred)),
              fill = vermelho, alpha = .25, colour = vermelho, linewidth = .3) +
    geom_segment(data = feitos, aes(xend = x, yend = pred), linetype = 2, colour = vermelho, linewidth = .7) +
    geom_text(data = feitos, aes(label = paste0("d", i), y = (y + pred) / 2), hjust = 1.3, size = 4,
              colour = vermelho) +
    geom_abline(intercept = a, slope = b, colour = cores[["texto"]], linewidth = 1.2) +
    geom_point(colour = azul_ponto, size = 3) +
    coord_equal(xlim = c(0, 11.5), ylim = c(0, 9), expand = FALSE) +
    labs(title = titulo, subtitle = formula, x = "X", y = "Y") +
    tema_aula(16, conceitual = TRUE) +
    theme(plot.subtitle = element_text(colour = vermelho, face = "bold"))
}

quadros_soma_quadrados <- function(d = pontos_dispersao(), pausa = 6) {
  m <- coef(lm(y ~ x, d))
  retas <- list(list(a = 5.5, b = .1, titulo = "Uma reta qualquer"),
                list(a = m[[1]], b = m[[2]], titulo = "A reta de mínimos quadrados"))
  out <- list()
  for (r in retas) {
    for (k in 0:nrow(d)) out <- c(out, list(quadro_soma(d, r$a, r$b, k, r$titulo)))
    out <- c(out, rep(list(quadro_soma(d, r$a, r$b, nrow(d), r$titulo)), pausa))
  }
  out
}

# ---------------------------------------------------------------------------
# Animação 2: o otimizador (optim, Nelder-Mead) testa uma reta por vez. À
# esquerda, a reta da iteração atual; à direita, o erro de cada tentativa
# (RMSE, a raiz da média dos resíduos ao quadrado), que despenca até o
# mínimo, como no animate-lm-optimization.R original.
# ---------------------------------------------------------------------------
historico_otimizacao <- function(d = pontos_dispersao(), inicio = c(0, 0)) {
  hist <- list()
  sq <- function(p) {
    s <- sum((d$y - (p[1] + p[2] * d$x))^2)
    hist[[length(hist) + 1]] <<- c(a = p[1], b = p[2], sq = s)
    s
  }
  optim(inicio, sq)
  as.data.frame(do.call(rbind, hist)) |>
    mutate(iteracao = row_number(), rmse = sqrt(sq / nrow(d)))
}

quadro_otimizacao <- function(d, h, k) {
  atual <- h[k, ]
  antes <- h[max(1, k - 6):k, ]
  esq <- ggplot(d, aes(x, y)) +
    geom_abline(data = antes, aes(intercept = a, slope = b), colour = "grey70", linewidth = .5) +
    geom_abline(intercept = atual$a, slope = atual$b, colour = cores[["texto"]], linewidth = 1.2) +
    geom_point(colour = azul_ponto, size = 3) +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 9), expand = FALSE) +
    labs(title = sprintf("Tentativa %d", k),
         subtitle = sprintf("Y = %s + %s X", num_br(atual$a, 2), num_br(atual$b, 2)), x = "X", y = "Y") +
    tema_aula(15, conceitual = TRUE)
  dir <- ggplot(h, aes(iteracao, rmse)) +
    geom_line(data = h[seq_len(k), ], colour = vermelho, linewidth = .8) +
    geom_point(data = atual, colour = vermelho, size = 3.5) +
    coord_cartesian(xlim = c(1, nrow(h)), ylim = c(0, max(h$rmse))) +
    labs(title = "Erro da reta a cada tentativa",
         subtitle = sprintf("RMSE = %s", num_br(atual$rmse, 2)), x = "Tentativa", y = "RMSE") +
    tema_aula(15)
  esq + dir + fundo_papel
}

quadros_otimizacao <- function(d = pontos_dispersao(), max_quadros = 90, pausa = 15) {
  h <- historico_otimizacao(d)
  ks <- unique(round(seq(1, nrow(h), length.out = min(max_quadros, nrow(h)))))
  c(lapply(ks, \(k) quadro_otimizacao(d, h, k)), rep(list(quadro_otimizacao(d, h, nrow(h))), pausa))
}

# ---------------------------------------------------------------------------
# Monta o GIF: salva os quadros em PNG e junta com o ffmpeg (paleta própria,
# para o arquivo ficar pequeno e sem serrilhado).
# ---------------------------------------------------------------------------
salva_gif <- function(quadros, arquivo, fps = 4, largura = 10, altura = 5.6, dpi = 100) {
  pasta <- tempfile("quadros")
  dir.create(pasta)
  for (i in seq_along(quadros)) {
    ggsave(file.path(pasta, sprintf("q%04d.png", i)), quadros[[i]],
           width = largura, height = altura, dpi = dpi, bg = cores[["papel"]])
  }
  entrada <- file.path(pasta, "q%04d.png")
  paleta <- file.path(pasta, "paleta.png")
  system2("ffmpeg", c("-loglevel", "error", "-y", "-framerate", fps, "-i", shQuote(entrada),
                      "-vf", "palettegen=stats_mode=diff", shQuote(paleta)))
  system2("ffmpeg", c("-loglevel", "error", "-y", "-framerate", fps, "-i", shQuote(entrada),
                      "-i", shQuote(paleta), "-lavfi", shQuote("paletteuse=dither=none"),
                      "-loop", "0", shQuote(arquivo)))
  unlink(pasta, recursive = TRUE)
  invisible(arquivo)
}

gera_animacoes <- function() {
  salva_gif(quadros_soma_quadrados(), here::here("assets/aula09-soma-quadrados.gif"), fps = 4)
  salva_gif(quadros_otimizacao(), here::here("assets/aula09-otimizacao.gif"), fps = 8,
            largura = 12, altura = 5.4)
}
