# Aula 06: estimativa pontual e intervalar. Figuras com os dados da
# pescaria do original ou simuladas; cada função que sorteia fixa a semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))

vermelho <- "#c0392b"
num_br <- function(v, d = 1) format(round(v, d), nsmall = d, decimal.mark = ",")
mu_lago <- 250   # o peso médio "verdadeiro" das tilápias, que só a Mãe Natureza sabe

# As cinco redes da atividade da pescaria (dados do original).
redes <- tibble::tibble(
  rede = rep(paste("Rede", 1:5), each = 10),
  peso = c(245, 255, 250, 248, 252, 260, 240, 238, 251, 255,
           230, 240, 245, 250, 235, 228, 241, 233, 229, 239,
           260, 265, 258, 270, 255, 262, 268, 259, 261, 263,
           231, 235, 252, 282, 241, 223, 254, 246, 239, 236,
           262, 272, 268, 251, 270, 217, 242, 226, 231, 259))

# ---------------------------------------------------------------------------
# Pescaria: as cinco médias amostrais e o valor verdadeiro.
# ---------------------------------------------------------------------------
fig_pescaria <- function(revela = TRUE) {
  m <- redes |> group_by(rede) |> summarise(media = mean(peso))
  g <- ggplot(redes, aes(peso, rede)) +
    geom_point(size = 3, alpha = .55, colour = cores[["accent"]],
               position = position_jitter(width = 0, height = .12, seed = 1)) +
    geom_point(data = m, aes(media, rede), shape = 124, size = 12, colour = cores[["texto"]]) +
    geom_text(data = m, aes(286, rede, label = sprintf("média = %s g", num_br(media, 1))),
              hjust = 0, size = 4.6) +
    coord_cartesian(xlim = c(214, 300), clip = "off") +
    labs(x = "Peso da tilápia (g)", y = NULL) +
    tema_aula(15) + theme(plot.margin = margin(5, 110, 5, 5))
  if (revela)
    g <- g + geom_vline(xintercept = mu_lago, linetype = "22", colour = vermelho, linewidth = 1) +
      annotate("label", x = mu_lago, y = 5.55, label = "μ = 250 g (só a Mãe Natureza sabe)",
               colour = vermelho, size = 4.4, linewidth = 0, fill = cores[["papel"]])
  g
}

# Amostras da mesma população têm médias diferentes.
fig_amostras_diferentes <- function(seed = 7, k = 5, n = 10) {
  set.seed(seed)
  x <- seq(200, 300, length.out = 300)
  am <- purrr::map_dfr(seq_len(k), \(i) tibble::tibble(i = i, peso = rnorm(n, mu_lago, 15)))
  dens <- am |> group_by(i) |> reframe(x = x, y = dnorm(x, mean(peso), sd(peso)))
  med <- am |> group_by(i) |> summarise(m = mean(peso))
  pal <- c(cores[["accent"]], "#d98a3d", cores[["verde"]], cores[["ferrugem"]], "#6b4c8a")
  ggplot(dens, aes(x, y, colour = factor(i))) +
    geom_line(linewidth = 1.1) +
    geom_vline(data = med, aes(xintercept = m, colour = factor(i)), linetype = "22", linewidth = .9) +
    scale_colour_manual(values = pal, guide = "none") +
    labs(x = "Peso da tilápia (g)", y = NULL,
         caption = sprintf("%d amostras de %d tilápias do mesmo lago (dados simulados); tracejado: a média de cada amostra.", k, n)) +
    tema_aula(15) + theme(axis.text.y = element_blank())
}

# ---------------------------------------------------------------------------
# Distribuição amostral: da população para a distribuição das médias.
# ---------------------------------------------------------------------------
fig_distribuicao_amostral <- function(seed = 2, n = 10, reps = 5000) {
  set.seed(seed)
  pop <- tibble::tibble(peso = rgamma(20000, 25, 25 / mu_lago))
  medias <- tibble::tibble(m = replicate(reps, mean(sample(pop$peso, n))))
  a <- ggplot(pop, aes(peso)) +
    geom_histogram(bins = 40, fill = cores[["neutro"]], colour = "white") +
    geom_vline(xintercept = mean(pop$peso), colour = vermelho, linewidth = 1) +
    labs(title = "População: o peso de cada tilápia", x = "Peso (g)", y = NULL) +
    tema_aula(14) + theme(axis.text.y = element_blank())
  b <- ggplot(medias, aes(m)) +
    geom_histogram(bins = 40, fill = cores[["accent"]], colour = "white") +
    geom_vline(xintercept = mean(pop$peso), colour = vermelho, linewidth = 1) +
    labs(title = sprintf("Distribuição amostral: a média de %s amostras de %d", format(reps, big.mark = "."), n),
         x = "Média amostral (g)", y = NULL) +
    coord_cartesian(xlim = range(pop$peso)) +
    tema_aula(14) + theme(axis.text.y = element_blank())
  a / b
}

# ---------------------------------------------------------------------------
# Viés e variância: os quatro alvos.
# ---------------------------------------------------------------------------
fig_alvos <- function(seed = 3) {
  set.seed(seed)
  niv_vies <- c("Baixo viés\n(alta acurácia)", "Alto viés\n(baixa acurácia)")
  niv_var <- c("Baixa variância\n(alta precisão)", "Alta variância\n(baixa precisão)")
  casos <- tidyr::expand_grid(vies = niv_vies, var = niv_var) |>
    mutate(cx = ifelse(grepl("Baixo", vies), 0, 1.3), cy = ifelse(grepl("Baixo", vies), 0, 1.1),
           s = ifelse(grepl("Baixa", var), .18, .65))
  tiros <- purrr::pmap_dfr(casos, \(vies, var, cx, cy, s) tibble::tibble(vies, var, x = rnorm(12, cx, s), y = rnorm(12, cy, s)))
  t <- seq(0, 2 * pi, length.out = 120)
  aneis <- tidyr::expand_grid(casos |> select(vies, var), r = c(2.2, 1.5, .8)) |>
    group_by(vies, var, r) |> reframe(x = r * cos(t), y = r * sin(t)) |>
    # fator explícito: converter o raio para texto sofre com a vírgula decimal
    mutate(anel = as.character(factor(r, levels = c(2.2, 1.5, .8), labels = c("fora", "meio", "centro"))))
  # níveis dos painéis na ordem do original: baixo viés em cima, baixa variância à esquerda
  fat <- function(d) mutate(d, vies = factor(vies, niv_vies), var = factor(var, niv_var))
  aneis <- fat(aneis); tiros <- fat(tiros)
  ggplot() +
    # um anel por camada, de fora para dentro, para o de fora não cobrir os outros
    lapply(c("fora", "meio", "centro"), \(k) geom_polygon(data = filter(aneis, anel == k),
                                                         aes(x, y, group = interaction(vies, var), fill = anel),
                                                         colour = "white")) +
    scale_fill_manual(values = c(centro = "#2b6cb0", meio = "#90b4dd", fora = "#dbe7f3"), guide = "none") +
    geom_point(data = tiros, aes(x, y), colour = vermelho, shape = 15, size = 2.4) +
    facet_grid(vies ~ var, switch = "y") +
    coord_equal(xlim = c(-2.3, 2.3), ylim = c(-2.3, 2.3)) +
    theme_void(base_size = 14) +
    theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
          strip.text = element_text(face = "bold", lineheight = .9, margin = margin(3, 3, 3, 3)),
          strip.text.y.left = element_text(angle = 90, face = "bold"))
}

# ---------------------------------------------------------------------------
# Lei dos Grandes Números: a média acumulada converge para μ e o intervalo
# de 95% encolhe com o tamanho amostral.
# ---------------------------------------------------------------------------
fig_lgn <- function(seed = 11, nmax = 500, mu = .253, s = .04) {
  set.seed(seed)
  x <- rnorm(nmax, mu, s)
  d <- tibble::tibble(n = 2:nmax) |>
    mutate(m = cumsum(x)[n] / n,
           sd = purrr::map_dbl(n, \(k) sd(x[1:k])),
           li = m - qt(.975, n - 1) * sd / sqrt(n), ls = m + qt(.975, n - 1) * sd / sqrt(n))
  ggplot(d, aes(n)) +
    geom_hline(yintercept = mu, linetype = "22", colour = cores[["texto"]]) +
    geom_line(aes(y = li), colour = cores[["verde"]], linewidth = .9) +
    geom_line(aes(y = ls), colour = cores[["verde"]], linewidth = .9) +
    geom_line(aes(y = m), colour = cores[["texto"]], linewidth = 1) +
    coord_cartesian(ylim = c(.2, .31)) +
    labs(x = "Tamanho amostral (n)", y = "Comprimento da tíbia (mm)",
         caption = sprintf("Tracejado: a média da população (%s). Linha preta: a média amostral. Verde: o intervalo de 95%%. Dados simulados.", num_br(mu, 3))) +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Valor crítico: as caudas de α/2 na normal padrão ou na t.
# ---------------------------------------------------------------------------
fig_valor_critico <- function(conf = .95, gl = NULL) {
  a <- 1 - conf
  crit <- if (is.null(gl)) qnorm(1 - a / 2) else qt(1 - a / 2, gl)
  dens <- if (is.null(gl)) dnorm else function(x) dt(x, gl)
  x <- seq(-4, 4, by = .01)
  d <- tibble::tibble(x = x, y = dens(x))
  nome <- if (is.null(gl)) "z" else "t"
  ggplot(d, aes(x, y)) +
    geom_ribbon(data = filter(d, x <= -crit), aes(ymin = 0, ymax = y), fill = "#7cc36e") +
    geom_ribbon(data = filter(d, x >= crit), aes(ymin = 0, ymax = y), fill = "#7cc36e") +
    geom_line(linewidth = 1.1) +
    annotate("text", x = c(-crit - .55, crit + .55), y = dens(crit) + .06,
             label = sprintf("α/2 = %s", num_br(a / 2, 3)), size = 4.6) +
    annotate("segment", x = -crit, xend = crit, y = max(d$y) + .04, yend = max(d$y) + .04,
             arrow = arrow(ends = "both", length = unit(.2, "cm")), colour = cores[["texto"]]) +
    annotate("text", x = 0, y = max(d$y) + .08, size = 4.8,
             label = sprintf("Nível de confiança: %s%%", num_br(100 * conf, 0))) +
    scale_x_continuous(breaks = c(-crit, 0, crit),
                       labels = c(sprintf("−%s = %s", paste0(nome, "[α/2]"), num_br(-crit, 3)), "0",
                                  sprintf("%s = %s", paste0(nome, "[α/2]"), num_br(crit, 3)))) +
    labs(x = if (is.null(gl)) "Escore-z" else sprintf("t com %d graus de liberdade", gl), y = NULL) +
    tema_aula(15) + theme(axis.text.y = element_blank(), panel.grid = element_blank())
}

# ---------------------------------------------------------------------------
# O arqueiro: o centro do alvo é fixo, as flechas (e os círculos) variam.
# ---------------------------------------------------------------------------
fig_arqueiro <- function(etapa = 1, seed = 6, k = 12) {
  set.seed(seed)
  # raio de 10 cm contendo 95% das flechas: σ tal que o raio é o quantil 95%
  s <- 10 / sqrt(qchisq(.95, 2))
  fl <- tibble::tibble(i = seq_len(k), x = rnorm(k, 0, s), y = rnorm(k, 0, s)) |>
    mutate(acerta = sqrt(x^2 + y^2) <= 10)
  t <- seq(0, 2 * pi, length.out = 120)
  base <- ggplot() + coord_equal(xlim = c(-24, 24), ylim = c(-19, 18), clip = "off") +
    theme_void() + theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA))
  if (etapa == 1) {
    aneis <- purrr::map_dfr(c(15, 10, 5), \(r) tibble::tibble(r = r, x = r * cos(t), y = r * sin(t)))
    base +
      geom_polygon(data = aneis, aes(x, y, group = r, fill = factor(r)), colour = "white") +
      scale_fill_manual(values = c("5" = "#c0392b", "10" = "#f4d03f", "15" = "#dbe7f3"), guide = "none") +
      annotate("path", x = 10 * cos(t), y = 10 * sin(t), linetype = "22", linewidth = .9) +
      geom_point(data = fl, aes(x, y), shape = 4, size = 3.5, stroke = 1.6, colour = cores[["texto"]]) +
      annotate("text", x = 0, y = -17, size = 4.6, label = "círculo de 10 cm: 95% das flechas")
  } else {
    circ <- fl |> rowwise() |> reframe(i = i, acerta = acerta, cx = x + 10 * cos(t), cy = y + 10 * sin(t))
    base +
      geom_path(data = circ, aes(cx, cy, group = i, colour = acerta), linewidth = .7, alpha = .8) +
      geom_point(data = fl, aes(x, y, colour = acerta), shape = 4, size = 3.5, stroke = 1.6) +
      scale_colour_manual(values = c(`TRUE` = cores[["accent"]], `FALSE` = vermelho), guide = "none") +
      {if (etapa == 3) annotate("point", x = 0, y = 0, shape = 21, size = 5, fill = "#f4d03f", colour = cores[["texto"]])} +
      {if (etapa == 3) annotate("text", x = 0, y = -17, size = 4.6,
                                label = sprintf("%d de %d círculos contêm o centro", sum(fl$acerta), k))}
  }
}

# ---------------------------------------------------------------------------
# Teorema do Limite Central: quatro populações, médias de amostras de
# tamanhos crescentes.
# ---------------------------------------------------------------------------
fig_tlc <- function(seed = 1, reps = 4000, ns = c(1, 2, 5, 30), anota = FALSE) {
  set.seed(seed)
  pops <- list(
    "Uniforme"   = function(n) runif(n, 0, 1),
    "Bimodal"    = function(n) ifelse(runif(n) < .5, rbeta(n, 2, 8), rbeta(n, 8, 2)),
    "Assimétrica" = function(n) rexp(n, 1),
    "Muito assimétrica" = function(n) rlnorm(n, 0, 1.1))
  d <- purrr::imap_dfr(pops, \(f, nm) purrr::map_dfr(ns, \(k)
    tibble::tibble(pop = nm, n = k, m = replicate(reps, mean(f(k)))))) |>
    mutate(pop = factor(pop, names(pops)),
           n = factor(paste0("n = ", n), paste0("n = ", ns)))
  g <- ggplot(d, aes(m)) +
    geom_histogram(bins = 30, fill = cores[["accent"]], colour = "white", linewidth = .1) +
    facet_wrap(pop ~ n, ncol = length(ns), scales = "free",
               labeller = labeller(.multi_line = FALSE)) +
    labs(x = "Média amostral", y = NULL,
         caption = "n = 1 é a própria população. Cada painel: 4.000 médias simuladas.") +
    tema_aula(12) + theme(axis.text = element_blank(), panel.grid = element_blank())
  if (anota) g <- g + labs(subtitle = "Quanto mais a população se afasta da normal, maior tem de ser o n para a média ficar normal.")
  g
}

# ---------------------------------------------------------------------------
# Intervalos de confiança de muitas amostras: quantos capturam μ?
# ---------------------------------------------------------------------------
ic_simulados <- function(k = 20, n = 10, mu = mu_lago, s = 20, conf = .95, seed = 1) {
  set.seed(seed)
  purrr::map_dfr(seq_len(k), function(i) {
    x <- rnorm(n, mu, s); e <- qt(1 - (1 - conf) / 2, n - 1) * sd(x) / sqrt(n)
    tibble::tibble(i = i, m = mean(x), li = mean(x) - e, ls = mean(x) + e)
  }) |> mutate(captura = li <= mu & ls >= mu)
}

fig_ic_repetidos <- function(k = 20, seed = 1, mostra_mu = TRUE) {
  d <- ic_simulados(k = k, seed = seed)
  g <- ggplot(d, aes(y = i)) +
    geom_errorbar(aes(xmin = li, xmax = ls, colour = captura), orientation = "y", width = .4, linewidth = .9) +
    geom_point(aes(x = m, colour = captura), size = 2.2) +
    scale_colour_manual(values = c(`TRUE` = cores[["accent"]], `FALSE` = vermelho), guide = "none") +
    labs(x = "Peso médio da tilápia (g)", y = "Amostra",
         caption = sprintf("%d amostras de 10 tilápias; IC de 95%% de cada uma (dados simulados).", k)) +
    tema_aula(14)
  if (mostra_mu)
    g <- g + geom_vline(xintercept = mu_lago, colour = cores[["texto"]], linewidth = 1) +
      labs(title = sprintf("%d de %d intervalos capturam μ = 250 g", sum(d$captura), k))
  g
}

# Proporções: o cenário mendeliano (p = 0,25, n = 100).
fig_ic_proporcao <- function(k = 25, n = 100, p = .25, conf = .95, seed = 9) {
  set.seed(seed)
  z <- qnorm(1 - (1 - conf) / 2)
  d <- tibble::tibble(i = seq_len(k), ph = rbinom(k, n, p) / n) |>
    mutate(e = z * sqrt(ph * (1 - ph) / n), li = ph - e, ls = ph + e,
           captura = li <= p & ls >= p)
  ggplot(d, aes(y = i)) +
    geom_vline(xintercept = p, linewidth = 1, colour = cores[["texto"]]) +
    geom_errorbar(aes(xmin = li, xmax = ls, colour = captura), orientation = "y", width = .4, linewidth = .9) +
    geom_point(aes(x = ph, colour = captura), size = 2.2) +
    scale_colour_manual(values = c(`TRUE` = cores[["verde"]], `FALSE` = vermelho), guide = "none") +
    scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    labs(x = "Proporção de sementes rugosas", y = "Experimento",
         title = sprintf("%d de %d intervalos capturam p = 25%%", sum(d$captura), k),
         caption = sprintf("%d experimentos simulados com %d sementes cada; IC de 95%%.", k, n)) +
    tema_aula(14)
}

# Fatores que mudam a largura do IC.
fig_largura_ic <- function(s = 20, n0 = 30) {
  e <- function(conf, n, s) qt(1 - (1 - conf) / 2, n - 1) * s / sqrt(n)
  d <- bind_rows(
    tibble::tibble(fator = "Nível de confiança", rot = c("90%", "95%", "99%"),
                   e = e(c(.9, .95, .99), n0, s)),
    tibble::tibble(fator = "Tamanho da amostra", rot = c("n = 10", "n = 30", "n = 100"),
                   e = e(.95, c(10, 30, 100), s)),
    tibble::tibble(fator = "Variabilidade", rot = c("s = 10 g", "s = 20 g", "s = 40 g"),
                   e = e(.95, n0, c(10, 20, 40)))) |>
    group_by(fator) |> mutate(y = rev(seq_along(rot)), rot = factor(rot, unique(rot))) |> ungroup()
  ggplot(d, aes(y = y)) +
    geom_errorbar(aes(xmin = mu_lago - e, xmax = mu_lago + e), orientation = "y", width = .3,
                  linewidth = 1.2, colour = cores[["accent"]]) +
    geom_point(aes(x = mu_lago), size = 2.6) +
    geom_text(aes(x = mu_lago + e + 1, label = rot), hjust = 0, size = 4.4) +
    facet_wrap(~fator) +
    scale_y_continuous(NULL, breaks = NULL, expand = expansion(add = .6)) +
    coord_cartesian(xlim = c(225, 285)) +
    labs(x = "Peso médio (g)") +
    tema_aula(14)
}
