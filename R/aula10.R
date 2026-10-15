# Aula 10: qui-quadrado, teste exato de Fisher e teste G. As contagens são
# as dos exemplos do original; nenhuma função sorteia dados.

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
source(here::here("R/tema.R"))
source(here::here("R/aula02_pergunta.R"))   # fig_diagrama()

vermelho <- "#c0392b"
num_br <- function(v, d = 2) format(round(v, d), nsmall = d, decimal.mark = ",")

# Exemplos do original.
mendel <- c(`Amarela/lisa` = 290, `Amarela/rugosa` = 88, `Verde/lisa` = 95, `Verde/rugosa` = 27)
mendel_p <- c(9, 3, 3, 1) / 16
mm <- tibble::tibble(cor = c("Azul", "Laranja", "Verde", "Amarelo", "Vermelho", "Marrom"),
                     observado = c(30, 25, 28, 20, 25, 22),
                     prop = c(.24, .20, .16, .14, .13, .13))
# Status de populações de plantas raras e proteção das terras (Gotelli & Ellison, Tabela 11.2).
plantas <- matrix(c(18, 15, 8, 32), 2,
                  dimnames = list(Status = c("Em declínio", "Estável ou aumentando"),
                                  Protecao = c("Desprotegidas", "Protegidas")))
# Vacina tríplice viral e autismo (Triola, Tabela 11-1).
vacina <- matrix(c(25, 362, 64, 1427), 2,
                 dimnames = list(Autismo = c("Com autismo", "Sem autismo"),
                                 Vacina = c("Não vacinado", "Vacinado")))
# Germinação de sementes de orquídea com e sem hormônio.
germinacao <- matrix(c(4, 1, 2, 5), 2,
                     dimnames = list(Grupo = c("Hormônio", "Controle"),
                                     Resultado = c("Germinou", "Não germinou")))

# ---------------------------------------------------------------------------
# Distribuição qui-quadrado com a região crítica e o valor observado.
# ---------------------------------------------------------------------------
fig_qui_quadrado <- function(gl = 3, obs = NULL, alfa = .05) {
  crit <- qchisq(1 - alfa, gl)
  xmax <- max(crit * 1.8, if (is.null(obs)) 0 else obs * 1.2)
  d <- tibble::tibble(x = seq(.01, xmax, length.out = 600)) |> mutate(y = dchisq(x, gl))
  topo <- max(d$y[d$x > .3])
  g <- ggplot(d, aes(x, y)) +
    geom_ribbon(data = filter(d, x >= crit), aes(ymin = 0, ymax = y), fill = "#7fb7d6") +
    geom_line(linewidth = 1.1) +
    geom_vline(xintercept = crit, linetype = "22", colour = cores[["texto"]]) +
    annotate("label", x = crit, y = topo * .55, hjust = -.05, size = 4.6, fill = cores[["papel"]],
             linewidth = 0, label = sprintf("valor crítico = %s\n(α = %s; gl = %d)",
                                            num_br(crit, 2), num_br(alfa, 2), gl)) +
    coord_cartesian(ylim = c(0, topo * 1.15)) +
    labs(x = expression(chi^2), y = NULL) +
    tema_aula(15) + theme(axis.text.y = element_blank(), panel.grid = element_blank())
  if (!is.null(obs)) {
    cor_obs <- if (obs >= crit) cores[["verde"]] else vermelho
    g <- g +
      annotate("segment", x = obs, xend = obs, y = topo * .95, yend = 0, colour = cor_obs,
               linewidth = 1.4, arrow = arrow(length = unit(.3, "cm"), type = "closed")) +
      annotate("label", x = obs, y = topo * 1.05, colour = cor_obs, fill = cores[["papel"]],
               fontface = "bold", size = 4.8, label = sprintf("χ² calculado = %s", num_br(obs, 2)))
  }
  g
}

# ---------------------------------------------------------------------------
# M&M's: frequências observadas e esperadas pela afirmação da empresa.
# ---------------------------------------------------------------------------
fig_mm <- function(d = mm) {
  n <- sum(d$observado)
  longo <- d |> mutate(esperado = prop * n) |>
    pivot_longer(c(esperado, observado), names_to = "tipo", values_to = "contagem") |>
    mutate(tipo = factor(tipo, levels = c("esperado", "observado"), labels = c("Esperado", "Observado")),
           cor = factor(cor, levels = d$cor))
  ggplot(longo, aes(cor, contagem, fill = tipo)) +
    geom_col(position = position_dodge(width = .8), width = .75) +
    scale_fill_manual(values = c(Esperado = "grey70", Observado = cores[["accent"]]), name = NULL) +
    labs(x = "Cor das balas", y = sprintf("Contagem (n = %d)", n)) +
    tema_aula(15)
}

# ---------------------------------------------------------------------------
# Gráfico de mosaico: a largura de cada coluna é proporcional ao total da
# coluna; a altura de cada retângulo, à frequência dentro da coluna.
# ---------------------------------------------------------------------------
fig_mosaico <- function(tab = plantas) {
  col_tot <- colSums(tab)
  larg <- col_tot / sum(tab)
  x0 <- c(0, cumsum(larg)[-length(larg)]) + c(0, rep(.02, length(larg) - 1))
  d <- as.data.frame(as.table(tab)) |>
    group_by(Protecao) |>
    mutate(prop = Freq / sum(Freq), y1 = cumsum(prop), y0 = y1 - prop) |>
    ungroup() |>
    mutate(j = as.integer(Protecao), xmin = x0[j], xmax = x0[j] + larg[j],
           ymin = y0 + .01, ymax = y1 - .01)
  ggplot(d) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Status),
              colour = cores[["texto"]]) +
    geom_text(aes((xmin + xmax) / 2, (ymin + ymax) / 2, label = Freq), size = 6) +
    scale_fill_manual(values = c("#a8d5c8", "#e3eee9"), name = "Status da população") +
    scale_x_continuous(breaks = x0 + larg / 2, labels = colnames(tab)) +
    scale_y_continuous(breaks = NULL) +
    labs(x = "Estado de proteção das terras", y = NULL) +
    tema_aula(15) + theme(panel.grid = element_blank())
}

# ---------------------------------------------------------------------------
# Da distância entre O e E à decisão.
# ---------------------------------------------------------------------------
fig_fluxo_qui2 <- function() {
  perto <- cores[["verde"]]; longe <- cores[["ferrugem"]]
  nos <- tibble::tribble(
    ~id,  ~rotulo,                                        ~x,   ~y,   ~cor,
    "C",  "Compare os valores\nobservados (O) com\nos esperados (E)", 2.5, 3.2, cores[["accent"]],
    "P1", "O e E próximos:\nχ² pequeno,\nP grande",         .6,   1.9, perto,
    "P2", "O e E distantes:\nχ² grande,\nP pequeno",        4.4,  1.9, longe,
    "D1", "Não rejeitar H0:\nbom ajuste à\ndistribuição suposta", .6, .6, perto,
    "D2", "Rejeitar H0:\nmau ajuste à\ndistribuição suposta",  4.4, .6, longe
  )
  arestas <- tibble::tribble(
    ~de,  ~para, ~rotulo,
    "C",  "P1",  NA,
    "C",  "P2",  NA,
    "P1", "D1",  NA,
    "P2", "D2",  NA
  )
  fig_diagrama(nos, arestas, larg = 2.3, alt = .95, base = 4.8, xlim = c(-.8, 5.8), ylim = c(0, 3.8))
}

# ---------------------------------------------------------------------------
# Germinação com e sem hormônio, em porcentagem.
# ---------------------------------------------------------------------------
fig_germinacao <- function(tab = germinacao) {
  d <- as.data.frame(as.table(tab)) |>
    group_by(Grupo) |> mutate(pct = 100 * Freq / sum(Freq)) |> ungroup()
  ggplot(d, aes(Grupo, pct, fill = Resultado)) +
    geom_col(position = position_dodge(width = .8), width = .75) +
    geom_text(aes(label = sprintf("%d de %d", Freq, 6)), position = position_dodge(width = .8),
              vjust = -.4, size = 4.6) +
    scale_fill_manual(values = c(Germinou = "#4caf50", `Não germinou` = "#ff5722"), name = NULL) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(x = "Grupo", y = "Porcentagem das sementes (%)", title = "Efeito do hormônio na germinação") +
    tema_aula(15)
}
