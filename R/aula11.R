# Aula 11: análise de variância (ANOVA) como modelo linear. Os dados são
# simulados, exceto os da atividade (do original) e os girinos 2 x 2 da aula
# prática (data/girinos_2x2.csv); cada função que sorteia fixa a semente.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/aula02_pergunta.R"))   # fig_diagrama()

vermelho <- "#c0392b"
num_br <- function(v, d = 2) format(round(v, d), nsmall = d, decimal.mark = ",")
fundo_papel <- plot_annotation(theme = theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA)))
cores_grupos <- c(cores[["accent"]], cores[["verde"]], cores[["ocre"]], cores[["ferrugem"]])

# Atividade "Decodificando a tabela de ANOVA" (dados do original).
atividade <- tibble::tibble(grupo = rep(LETTERS[1:4], each = 5),
                            y = c(25, 26, 20, 23, 21, 31, 25, 28, 27, 24,
                                  22, 26, 28, 25, 29, 33, 29, 31, 34, 28))

girinos <- read.csv(here::here("data/girinos_2x2.csv"), stringsAsFactors = TRUE)

# ---------------------------------------------------------------------------
# Diversidade de diatomáceas em quatro níveis de zinco.
# ---------------------------------------------------------------------------
dados_diatomaceas <- function(seed = 4, n = 8) {
  set.seed(seed)
  tibble::tibble(zinco = factor(rep(c("BACK", "LOW", "MED", "HIGH"), each = n),
                                levels = c("BACK", "LOW", "MED", "HIGH")),
                 diversidade = c(rnorm(n, 1.8, .3), rnorm(n, 2.0, .3), rnorm(n, 1.85, .35), rnorm(n, 1.3, .3)))
}

fig_diatomaceas <- function(d = dados_diatomaceas()) {
  ggplot(d, aes(zinco, diversidade)) +
    geom_boxplot(width = .55, fill = "white", outlier.shape = NA) +
    geom_point(position = position_jitter(width = .08, seed = 1), alpha = .6, colour = cores[["accent"]]) +
    labs(x = "Nível de zinco", y = "Diversidade de diatomáceas") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Variação entre e dentro dos grupos: mesmas médias com muita variação dentro
# vs. médias diferentes com pouca variação dentro.
# ---------------------------------------------------------------------------
fig_variacao <- function(seed = 2, n = 8) {
  set.seed(seed)
  grupos <- c("BACK", "LOW", "MED", "HIGH")
  a <- tibble::tibble(cenario = "Alta variação dentro, baixa entre:\nmédias iguais",
                      grupo = rep(grupos, each = n), y = rnorm(4 * n, 0, .55)) |>
    group_by(grupo) |> mutate(y = 1.46 + y - mean(y)) |> ungroup()   # médias exatamente iguais
  b <- tibble::tibble(cenario = "Baixa variação dentro, alta entre:\nmédias diferentes",
                      grupo = rep(grupos, each = n),
                      y = rep(c(1.2, 2.3, 1.8, .7), each = n) + rnorm(4 * n, 0, .08))
  d <- bind_rows(a, b) |> mutate(grupo = factor(grupo, levels = grupos))
  m <- d |> group_by(cenario, grupo) |> summarise(y = mean(y), .groups = "drop")
  ggplot(d, aes(grupo, y, colour = grupo)) +
    geom_point(position = position_jitter(width = .1, seed = 3), size = 2, alpha = .7, show.legend = FALSE) +
    geom_crossbar(data = m, aes(ymin = y, ymax = y), width = .5, colour = cores[["texto"]]) +
    scale_colour_manual(values = cores_grupos) +
    facet_wrap(~ cenario) +
    labs(x = "Nível de zinco", y = "Diversidade de diatomáceas") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# ANOVA como regressão com dummies: o grupo de referência dá o intercepto e
# cada β é a distância de um grupo até ele.
# ---------------------------------------------------------------------------
fig_dummy_grupos <- function(d = dados_diatomaceas()) {
  m <- d |> group_by(zinco) |> summarise(y = mean(diversidade)) |> mutate(x = as.integer(zinco))
  ref <- m$y[1]
  ggplot(d, aes(as.integer(zinco), diversidade)) +
    geom_hline(yintercept = ref, colour = cores[["accent"]], linewidth = 1) +
    geom_point(position = position_jitter(width = .06, seed = 1), alpha = .5, colour = cores[["texto"]]) +
    geom_segment(data = m, aes(x = x - .25, xend = x + .25, y = y, yend = y), colour = vermelho, linewidth = 1.2) +
    geom_segment(data = m[-1, ], aes(x = x + .3, xend = x + .3, y = ref, yend = y), colour = vermelho,
                 arrow = arrow(length = unit(.2, "cm")), linewidth = .9) +
    geom_text(data = m[-1, ], aes(x = x + .38, y = (ref + y) / 2, label = paste0("β[", x - 1, "]")),
              parse = TRUE, hjust = 0, colour = vermelho, size = 5.5) +
    annotate("text", x = .55, y = ref + .08, label = "α (média de BACK)", hjust = 0, colour = cores[["accent"]], size = 4.6) +
    scale_x_continuous(breaks = 1:4, labels = levels(d$zinco), limits = c(.5, 4.7)) +
    labs(x = "Nível de zinco (BACK é a referência)", y = "Diversidade de diatomáceas") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Da estatística F à decisão.
# ---------------------------------------------------------------------------
fig_fluxo_F <- function() {
  perto <- cores[["verde"]]; longe <- cores[["ferrugem"]]
  nos <- tibble::tribble(
    ~id,  ~rotulo,                                              ~x,  ~y,  ~cor,
    "A",  "Médias amostrais\nsão todas próximas",               .6,  2.6, perto,
    "B",  "Pelo menos uma média\namostral é muito diferente",   4.4, 2.6, longe,
    "C",  "F pequeno,\nP grande",                               .6,  1.4, perto,
    "D",  "F grande,\nP pequeno",                               4.4, 1.4, longe,
    "E",  "Deixar de rejeitar a igualdade\ndas médias populacionais", .6, .2, perto,
    "F",  "Rejeitar a igualdade das\nmédias populacionais",     4.4, .2,  longe
  )
  arestas <- tibble::tribble(~de, ~para, ~rotulo, "A", "C", NA, "B", "D", NA, "C", "E", NA, "D", "F", NA)
  fig_diagrama(nos, arestas, larg = 3, alt = .8, base = 4.8, xlim = c(-1.2, 6.2), ylim = c(-.4, 3.1))
}

# ---------------------------------------------------------------------------
# Hipóteses da ANOVA em barras.
# ---------------------------------------------------------------------------
fig_hipoteses_barras <- function() {
  d <- tibble::tibble(
    painel = rep(c("Hipótese nula", "Hipótese alternativa"), each = 3),
    trat = rep(c("T1", "T2", "T3"), 2), peso = c(10, 10, 10, 4, 13, 8.5)
  ) |> mutate(painel = factor(painel, levels = c("Hipótese nula", "Hipótese alternativa")))
  ggplot(d, aes(trat, peso, fill = trat)) +
    geom_col(width = .8, colour = cores[["texto"]], show.legend = FALSE) +
    scale_fill_manual(values = c("#fff6c8", "#dff5f8", "#e3f5df")) +
    facet_wrap(~ painel) +
    labs(x = "Tratamentos", y = "Peso (g)") +
    tema_aula(15, conceitual = TRUE)
}

# ---------------------------------------------------------------------------
# Mesma ANOVA, interpretações diferentes: o padrão das médias importa.
# ---------------------------------------------------------------------------
fig_cenarios_controle <- function() {
  grupos <- c("Sem manipulação", "Controle", "Tratamento")
  d <- tibble::tibble(
    painel = rep(c("A. Efeito do tratamento não significativo",
                   "B. Efeito do tratamento significativo",
                   "C. Tratamento e controle diferem da não manipulação:\nefeito da manipulação",
                   "D. Efeito da manipulação e do tratamento"), each = 3),
    grupo = factor(rep(grupos, 4), levels = grupos),
    media = c(15.5, 14.5, 16, 11.5, 11.2, 16, 11.5, 16.5, 16, 10.5, 13.5, 16),
    ep = c(1.6, 1.5, 1.8, .9, .9, 1.4, .9, 1.7, 1.5, .5, 1.2, 1.6)
  )
  ggplot(d, aes(grupo, media)) +
    geom_col(fill = "#a8d5c8", colour = cores[["texto"]], width = .6) +
    geom_errorbar(aes(ymin = media - ep, ymax = media + ep), width = .15) +
    coord_cartesian(ylim = c(8, 19)) +
    facet_wrap(~ painel, ncol = 2) +
    labs(x = NULL, y = "Variável resposta") +
    tema_aula(13) + theme(strip.text = element_text(size = 11))
}

# ---------------------------------------------------------------------------
# Crescimento das raízes de mangue com esponjas (médias simuladas).
# ---------------------------------------------------------------------------
fig_mangue <- function() {
  d <- tibble::tibble(grupo = factor(c("Sem manipulação", "Espuma", "Haliclona", "Tedania"),
                                     levels = c("Sem manipulação", "Espuma", "Haliclona", "Tedania")),
                      media = c(.35, .7, .78, .92), ep = c(.12, .14, .12, .15),
                      letra = c("a", "ab", "b", "b"))
  ggplot(d, aes(grupo, media)) +
    geom_col(aes(fill = grupo), width = .6, colour = cores[["texto"]], show.legend = FALSE) +
    geom_errorbar(aes(ymin = media - ep, ymax = media + ep), width = .15) +
    geom_text(aes(y = media + ep + .06, label = letra), size = 5.5) +
    scale_fill_manual(values = c("white", "#c7e9c0", "#41ab5d", "#006d2c")) +
    labs(x = NULL, y = "Taxa de crescimento da raiz (mm/dia)") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Atividade: os dados e os intervalos de Tukey.
# ---------------------------------------------------------------------------
fig_atividade <- function(d = atividade) {
  m <- d |> group_by(grupo) |> summarise(y = mean(y))
  ggplot(d, aes(grupo, y, colour = grupo)) +
    geom_point(position = position_jitter(width = .08, height = 0, seed = 1), size = 3, show.legend = FALSE) +
    geom_crossbar(data = m, aes(ymin = y, ymax = y), width = .45, colour = cores[["texto"]]) +
    scale_colour_manual(values = cores_grupos) +
    labs(x = "Grupo", y = "Resposta") +
    tema_aula(15)
}

fig_tukey <- function(d = atividade) {
  tk <- TukeyHSD(aov(y ~ grupo, data = d))$grupo
  t <- tibble::tibble(par = rownames(tk), dif = tk[, "diff"], lwr = tk[, "lwr"], upr = tk[, "upr"],
                      p = tk[, "p adj"]) |>
    mutate(par = factor(par, levels = rev(par)), sig = ifelse(p <= .05, "P ≤ 0,05", "P > 0,05"))
  ggplot(t, aes(dif, par, colour = sig)) +
    geom_vline(xintercept = 0, linetype = "22") +
    geom_errorbar(aes(xmin = lwr, xmax = upr), width = .2, orientation = "y", linewidth = 1) +
    geom_point(size = 3) +
    scale_colour_manual(values = c("P ≤ 0,05" = vermelho, "P > 0,05" = cores[["neutro"]]), name = NULL) +
    labs(x = "Diferença entre as médias (IC 95% de Tukey)", y = "Par de grupos") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# ANOVA de dois fatores: o procedimento e os gráficos de interação.
# ---------------------------------------------------------------------------
fig_fluxo_fatorial <- function() {
  az <- cores[["accent"]]; vd <- cores[["verde"]]; fe <- cores[["ferrugem"]]
  nos <- tibble::tribble(
    ~id, ~rotulo,                                                     ~x,  ~y,   ~cor,
    "I", "Teste a interação:\nF = QM(interação) / QM(erro)",          0,   3.3,  az,
    "S", "Há efeito de\ninteração?",                                  0,   1.9,  az,
    "P", "Pare: não interprete os efeitos\nprincipais isoladamente",  4.2, 1.9,  fe,
    "L", "Teste o fator das linhas:\nF = QM(linhas) / QM(erro)",      0,   .5,   vd,
    "C", "Teste o fator das colunas:\nF = QM(colunas) / QM(erro)",    0,   -.9,  vd
  )
  # caixas afastadas o bastante para as setas aparecerem; os rótulos "sim"
  # e "não" ficam ao lado da seta, não em cima dela
  arestas <- tibble::tribble(~de, ~para, ~rotulo, ~lx, ~ly,
                             "I", "S", NA, NA, NA,
                             "S", "P", "sim", 2.1, 2.12,
                             "S", "L", "não", .32, 1.2,
                             "L", "C", NA, NA, NA)
  fig_diagrama(nos, arestas, larg = 2.9, alt = .8, base = 4.6, xlim = c(-1.6, 5.8), ylim = c(-1.4, 3.8))
}

fig_tipos_interacao <- function() {
  d <- tibble::tibble(
    painel = rep(c("Sem interação: linhas paralelas", "Com interação: linhas que se cruzam"), each = 4),
    luz = factor(rep(c("Sol", "Sombra"), 4), levels = c("Sol", "Sombra")),
    fertilizante = rep(rep(c("Sem fertilizante", "Com fertilizante"), each = 2), 2),
    crescimento = c(10, 6, 15, 11, 10, 7, 16, 4)
  ) |> mutate(painel = factor(painel, levels = unique(painel)))
  ggplot(d, aes(luz, crescimento, colour = fertilizante, group = fertilizante)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3.5) +
    scale_colour_manual(values = c(`Sem fertilizante` = cores[["neutro"]], `Com fertilizante` = cores[["verde"]]),
                        name = NULL) +
    facet_wrap(~ painel) +
    labs(x = "Luz", y = "Crescimento médio") +
    tema_aula(15, conceitual = TRUE) + theme(axis.text.x = element_text(colour = cores[["texto"]]))
}

fig_interacao_girinos <- function(d = girinos) {
  m <- d |> group_by(predador, alimento) |>
    summarise(media = mean(massa_mg), ep = sd(massa_mg) / sqrt(n()), .groups = "drop")
  ggplot(m, aes(alimento, media, colour = predador, group = predador)) +
    geom_line(linewidth = 1.2, position = position_dodge(width = .1)) +
    geom_errorbar(aes(ymin = media - ep, ymax = media + ep), width = .08, position = position_dodge(width = .1)) +
    geom_point(size = 3.5, position = position_dodge(width = .1)) +
    scale_colour_manual(values = c(Ausente = cores[["accent"]], Presente = vermelho), name = "Pista de predador") +
    labs(x = "Nível de alimento", y = "Massa média na metamorfose (mg)") +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# ANCOVA: seis padrões possíveis entre um tratamento e uma covariável.
# ---------------------------------------------------------------------------
fig_ancova <- function(seed = 5, n = 8) {
  set.seed(seed)
  x <- rep(seq(1, 10, length.out = n), 2)
  trat <- rep(c("Controle", "Tratamento"), each = n)
  painel <- function(nome, a1, b1, a2, b2) tibble::tibble(
    painel = nome, x = x, trat = trat,
    y = ifelse(trat == "Controle", a1 + b1 * x, a2 + b2 * x) + rnorm(2 * n, 0, .6))
  d <- bind_rows(
    painel("A. Nem tratamento nem covariável", 5, 0, 5, 0),
    painel("B. Só a covariável", 2, .6, 2, .6),
    painel("C. Só o tratamento", 4, 0, 7, 0),
    painel("D. Tratamento e covariável", 1, .6, 4, .6),
    painel("E. Interação: a ordem dos\ntratamentos se inverte", 2, .6, 8, -.4),
    painel("F. Interação: a ordem\nnão se inverte", 4, .1, 10, -.3)
  )
  ggplot(d, aes(x, y, colour = trat)) +
    geom_point(size = 1.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = .9) +
    scale_colour_manual(values = c(Controle = cores[["neutro"]], Tratamento = cores[["verde"]]), name = NULL) +
    facet_wrap(~ painel, ncol = 3) +
    labs(x = "X (covariável)", y = "Y (resposta)") +
    tema_aula(12, conceitual = TRUE) +
    theme(legend.text = element_text(size = 12, colour = cores[["texto"]]))
}

# ---------------------------------------------------------------------------
# O teste F como comparação de modelos: o modelo nulo prevê a média geral
# para todos; o completo, a média de cada grupo. Os resíduos encolhem, e o
# quanto encolhem é a SQ entre grupos.
# ---------------------------------------------------------------------------
fig_f_modelos <- function(d = atividade) {
  d <- d |> mutate(g = as.integer(factor(grupo)),
                   x = g + rep(seq(-.22, .22, length.out = 5), times = 4))
  geral <- mean(d$y)
  d <- d |> group_by(grupo) |> mutate(media_g = mean(y)) |> ungroup()
  sq_tot <- sum((d$y - geral)^2); sq_res <- sum((d$y - d$media_g)^2)
  medias <- d |> distinct(grupo, g, media_g)
  base <- function(titulo) list(
    scale_colour_manual(values = cores_grupos, guide = "none"),
    scale_x_continuous(breaks = 1:4, labels = levels(factor(d$grupo))),
    labs(x = "Grupo", y = "Resposta", title = titulo),
    coord_cartesian(ylim = range(d$y) + c(-1, 1.5)),
    tema_aula(14), theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()))
  nulo <- ggplot(d, aes(x, y, colour = grupo)) +
    geom_segment(aes(xend = x, yend = geral), colour = vermelho, alpha = .7) +
    geom_hline(yintercept = geral, linewidth = 1.1, colour = cores[["texto"]]) +
    geom_point(size = 2.6) +
    annotate("label", x = 2.5, y = max(d$y) + 1.3, size = 4.6, fill = cores[["papel"]], linewidth = 0,
             label = sprintf("Resíduos até a média geral: SQ total = %s", num_br(sq_tot, 1))) +
    base("Modelo nulo: uma só média")
  completo <- ggplot(d, aes(x, y, colour = grupo)) +
    geom_segment(aes(xend = x, yend = media_g), colour = vermelho, alpha = .7) +
    geom_segment(data = medias, aes(x = g - .32, xend = g + .32, y = media_g, yend = media_g),
                 linewidth = 1.1, colour = cores[["texto"]]) +
    geom_point(size = 2.6) +
    annotate("label", x = 2.5, y = max(d$y) + 1.3, size = 4.6, fill = cores[["papel"]], linewidth = 0,
             label = sprintf("Resíduos até a média do grupo: SQ resíduo = %s", num_br(sq_res, 1))) +
    base("Modelo completo: uma média por grupo")
  (nulo + completo) +
    plot_annotation(caption = sprintf("SQ entre grupos = SQ total − SQ resíduo = %s − %s = %s",
                                      num_br(sq_tot, 1), num_br(sq_res, 1), num_br(sq_tot - sq_res, 1)),
                    theme = theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA),
                                  plot.caption = element_text(size = 15, hjust = .5, colour = cores[["texto"]])))
}

# ---------------------------------------------------------------------------
# As três premissas da ANOVA: como cada uma aparece quando é atendida (em
# cima) e quando é violada (embaixo). Dados simulados.
# ---------------------------------------------------------------------------
fig_premissas <- function(seed = 11, n = 12) {
  set.seed(seed)
  grupos <- c("A", "B", "C")
  tema_p <- function(g) g + tema_aula(12) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid = element_blank(), plot.title = element_text(size = 12.5))
  pts <- function(d, cor = "grupo", titulo) tema_p(
    ggplot(d, aes(grupo, y, colour = .data[[cor]])) +
      geom_point(position = position_jitter(width = .15, seed = 2), size = 2) +
      labs(x = NULL, y = NULL, title = titulo))
  # independência: cada ponto é uma unidade × grupos de 4 medidas da mesma unidade
  ind_ok <- tibble::tibble(grupo = rep(grupos, each = n), y = rnorm(3 * n))
  unid <- tibble::tibble(grupo = rep(grupos, each = 3), u = factor(1:9), m = rnorm(9, 0, 1.2))
  ind_viol <- unid[rep(1:9, each = 4), ] |> mutate(y = m + rnorm(36, 0, .15))
  # normalidade: resíduos em sino × muito assimétricos
  res_ok <- tibble::tibble(r = rnorm(150))
  res_viol <- tibble::tibble(r = rlnorm(150, 0, .9) - exp(.9^2 / 2))
  hist_p <- function(d, titulo, cor) tema_p(ggplot(d, aes(r)) +
    geom_histogram(bins = 18, fill = cor, colour = "white") + labs(x = "resíduos", y = NULL, title = titulo))
  # homogeneidade: mesma dispersão × um grupo muito mais disperso
  hom_ok <- tibble::tibble(grupo = rep(grupos, each = n), y = rnorm(3 * n, rep(c(0, .5, .2), each = n), 1))
  hom_viol <- tibble::tibble(grupo = rep(grupos, each = n),
                             y = rnorm(3 * n, rep(c(0, .5, .2), each = n), rep(c(.3, .5, 2.4), each = n)))
  pal <- c(A = cores[["accent"]], B = cores[["verde"]], C = cores[["ocre"]])
    (pts(ind_ok, titulo = "Independência: atendida\n(cada ponto, uma unidade)") + scale_colour_manual(values = pal, guide = "none") |
     hist_p(res_ok, "Normalidade: atendida\n(resíduos em forma de sino)", cores[["accent"]]) |
     pts(hom_ok, titulo = "Homogeneidade: atendida\n(dispersão parecida)") + scale_colour_manual(values = pal, guide = "none")) /
    (pts(ind_viol, "u", "Independência: violada\n(medidas repetidas da mesma unidade)") +
       scale_colour_manual(values = rep(c("#5b4a8a", "#a15e3f", "#527238", "#96661f", "#1c6e8c"), 2), guide = "none") |
     hist_p(res_viol, "Normalidade: violada\n(resíduos muito assimétricos)", vermelho) |
     pts(hom_viol, titulo = "Homogeneidade: violada\n(um grupo muito mais disperso)") + scale_colour_manual(values = pal, guide = "none")) +
    plot_annotation(theme = theme(plot.background = element_rect(fill = cores[["papel"]], colour = NA)))
}
