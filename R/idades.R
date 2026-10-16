# Fio condutor das idades: a adivinhação de idades da Aula 1 (Gelman &
# Nolan 2002, seção 2.1) volta nas Aulas 3, 4, 6, 8, 9 e 11. As figuras
# usam dados simulados; com os dados da turma, passe-os em `d`, com as
# colunas grupo, foto, idade_real e idade_estimada.

library(ggplot2)
library(dplyr)
source(here::here("R/tema.R"))

#' Dados simulados de 10 grupos estimando a idade de 10 fotos
#'
#' Quem estima puxa os palpites para o meio: superestima os jovens e
#' subestima os idosos (`inclinacao` < 1). Cada grupo tem o seu viés.
idades_simuladas <- function(n_grupos = 10,
                             idade_real = c(20, 25, 32, 34, 38, 39, 45, 48, 70, 77),
                             inclinacao = .8, vies = 1, sd_grupo = 2, sd_erro = 5,
                             semente = 2026) {
  set.seed(semente)
  efeito_grupo <- rnorm(n_grupos, 0, sd_grupo)
  tidyr::expand_grid(grupo = seq_len(n_grupos),
                     foto = seq_along(idade_real)) |>
    mutate(idade_real = idade_real[foto],
           idade_estimada = round(45 + vies + inclinacao * (idade_real - 45) +
                                    efeito_grupo[grupo] + rnorm(n(), 0, sd_erro)),
           erro = idade_estimada - idade_real)
}

# erro = estimada − real, caso os dados da turma venham sem a coluna
com_erro <- function(d) mutate(d, erro = idade_estimada - idade_real)

# Aula 3: histograma de todos os erros
fig_idades_histograma <- function(d = idades_simuladas()) {
  d <- com_erro(d)
  ggplot(d, aes(erro)) +
    geom_histogram(binwidth = 4, boundary = 0, fill = cores[["accent"]],
                   colour = cores[["papel"]]) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = cores[["ferrugem"]],
               linewidth = 1) +
    annotate("text", x = 0, y = Inf, vjust = 1.5, hjust = -.05, size = 4.5,
             label = "acerto exato", colour = cores[["ferrugem"]]) +
    labs(x = "Erro (anos) = idade estimada − idade real",
         y = "Número de palpites") +
    tema_aula()
}

# Aula 3: tabela de dupla entrada, faixa etária da foto × direção do erro
faixa_etaria <- function(idade) {
  cut(idade, c(0, 35, 60, Inf), labels = c("Até 35 anos", "36 a 60", "Mais de 60"))
}

tab_idades_dupla <- function(d = idades_simuladas()) {
  d <- com_erro(d) |>
    mutate(Foto = faixa_etaria(idade_real),
           direcao = cut(erro, c(-Inf, -.5, .5, Inf),
                         labels = c("Subestimou", "Acertou", "Superestimou")))
  t <- table(d$Foto, d$direcao)
  t <- cbind(t, Total = rowSums(t))
  t <- rbind(t, Total = colSums(t))
  data.frame(`Idade real da foto` = rownames(t), t, check.names = FALSE,
             row.names = NULL)
}

# Aula 4: viés (erro médio) e precisão (desvio-padrão) de cada grupo
fig_idades_vies <- function(d = idades_simuladas()) {
  d <- com_erro(d)
  res <- d |> group_by(grupo) |>
    summarise(media = mean(erro), dp = sd(erro), .groups = "drop")
  ggplot(d, aes(factor(grupo), erro)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = cores[["neutro"]]) +
    geom_jitter(width = .12, height = 0, size = 2.4, alpha = .55,
                colour = cores[["accent"]]) +
    geom_point(data = res, aes(y = media), shape = 23, size = 5,
               fill = cores[["ocre"]], colour = cores[["texto"]]) +
    geom_text(data = res, aes(y = max(d$erro) + 6,
                              label = paste0("DP = ", format(round(dp, 1), nsmall = 1))),
              size = 3.8, colour = cores[["texto"]]) +
    labs(x = "Grupo", y = "Erro (anos)",
         subtitle = "Pontos: os 10 palpites do grupo · losango: erro médio (viés)") +
    tema_aula()
}

# Aula 6: IC de 95% do erro médio de cada foto (n = número de grupos)
ic_idades <- function(d = idades_simuladas(), nivel = .95) {
  com_erro(d) |>
    group_by(foto, idade_real) |>
    summarise(n = n(), media = mean(erro), ep = sd(erro) / sqrt(n),
              .groups = "drop") |>
    mutate(t = qt(1 - (1 - nivel) / 2, n - 1),
           li = media - t * ep, ls = media + t * ep)
}

fig_idades_ic <- function(d = idades_simuladas()) {
  ic <- ic_idades(d) |>
    mutate(cruza = li <= 0 & ls >= 0)
  ggplot(ic, aes(factor(idade_real), media)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = cores[["neutro"]]) +
    geom_errorbar(aes(ymin = li, ymax = ls, colour = cruza), width = .25,
                  linewidth = 1.1) +
    geom_point(aes(colour = cruza), size = 3.5) +
    scale_colour_manual(values = c(`TRUE` = cores[["neutro"]],
                                   `FALSE` = cores[["ferrugem"]]),
                        labels = c(`TRUE` = "IC inclui o zero",
                                   `FALSE` = "IC não inclui o zero"),
                        name = NULL) +
    labs(x = "Idade real da pessoa na foto (anos)",
         y = "Erro médio dos grupos (anos)") +
    tema_aula()
}

# Aula 8: o erro médio de cada grupo, para o teste t de uma amostra
erro_por_grupo <- function(d = idades_simuladas()) {
  com_erro(d) |> group_by(grupo) |> summarise(erro = mean(erro), .groups = "drop")
}

fig_idades_teste_t <- function(d = idades_simuladas()) {
  g <- erro_por_grupo(d)
  tt <- t.test(g$erro)
  ggplot(g, aes(x = erro, y = 0)) +
    annotate("rect", xmin = tt$conf.int[1], xmax = tt$conf.int[2],
             ymin = -.35, ymax = .35, fill = cores[["neutro_cl"]]) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = cores[["ferrugem"]],
               linewidth = 1) +
    geom_point(size = 4, alpha = .75, colour = cores[["accent"]]) +
    annotate("point", x = mean(g$erro), y = 0, shape = 23, size = 6,
             fill = cores[["ocre"]], colour = cores[["texto"]]) +
    annotate("text", x = 0, y = .55, label = " H0: erro médio = 0", hjust = 0,
             colour = cores[["ferrugem"]], size = 4.8) +
    annotate("label", x = mean(g$erro), y = -.55, size = 4.5,
             fill = cores[["papel"]], label.size = 0, colour = cores[["texto"]],
             label = "losango: média dos grupos · faixa cinza: IC de 95%") +
    scale_y_continuous(limits = c(-.8, .8), breaks = NULL) +
    labs(x = "Erro médio de cada grupo (anos)", y = NULL) +
    tema_aula()
}

# Aula 9: idade estimada contra a real, com a reta 1:1 e a reta ajustada
fig_idades_regressao <- function(d = idades_simuladas()) {
  ggplot(d, aes(idade_real, idade_estimada)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                colour = cores[["neutro"]], linewidth = 1) +
    geom_jitter(width = .6, height = 0, size = 2.2, alpha = .5,
                colour = cores[["accent"]]) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                colour = cores[["ferrugem"]], linewidth = 1.3) +
    annotate("text", x = 80, y = 84, label = "acerto perfeito (1:1)",
             hjust = 1, colour = cores[["neutro"]], size = 4.5) +
    annotate("text", x = 16, y = 50, label = "reta ajustada", hjust = 0,
             colour = cores[["ferrugem"]], size = 4.5) +
    coord_equal(xlim = c(15, 85), ylim = c(15, 85)) +
    labs(x = "Idade real (anos)", y = "Idade estimada (anos)") +
    tema_aula()
}

# Aula 11: erro médio de cada grupo em cada faixa etária da foto
erro_por_faixa <- function(d = idades_simuladas()) {
  com_erro(d) |>
    mutate(faixa = faixa_etaria(idade_real)) |>
    group_by(grupo, faixa) |>
    summarise(erro = mean(erro), .groups = "drop")
}

fig_idades_anova <- function(d = idades_simuladas()) {
  f <- erro_por_faixa(d)
  medias <- f |> group_by(faixa) |> summarise(erro = mean(erro))
  ggplot(f, aes(faixa, erro)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = cores[["neutro"]]) +
    geom_jitter(aes(colour = faixa), width = .1, height = 0, size = 3, alpha = .7) +
    geom_point(data = medias, shape = 23, size = 6, fill = cores[["ocre"]],
               colour = cores[["texto"]]) +
    scale_colour_manual(values = cores_niveis, guide = "none") +
    labs(x = "Idade real da pessoa na foto",
         y = "Erro médio do grupo (anos)",
         subtitle = "Cada ponto: um grupo · losango: média da faixa") +
    tema_aula()
}
