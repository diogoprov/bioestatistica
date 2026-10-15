library(ggplot2)

# Paleta do Biodiversity Synthesis Lab, a mesma do site e do tema dos slides.
cores <- c(
  accent      = "#1c6e8c",
  accent_dark = "#144f64",
  verde       = "#527238",
  ocre        = "#96661f",
  ferrugem    = "#a15e3f",
  neutro      = "#8a8172",
  neutro_cl   = "#e6e0d3",
  texto       = "#2d3748",
  papel       = "#faf7f0"
)

# Três níveis de um tratamento (controle / pouco / muito, ou baixo / médio /
# alto), do mais claro ao mais escuro, para continuar legível em P&B.
cores_niveis <- c("#e6c35c", "#d98a3d", "#a15e3f")

# Tipos de solo da paisagem simulada (aulas de amostragem e delineamento).
cores_solo <- c(Argiloso = "#c9a77c", Rochoso = "#a9a9a0", Arenoso = "#e8dcae")

#' Tema base dos gráficos das aulas
#'
#' @param base tamanho base do texto (pt); 15 fica legível projetado
#' @param conceitual TRUE apaga números dos eixos e desenha setas nas
#'   pontas, para gráficos que ilustram uma ideia e não um conjunto de dados
tema_aula <- function(base = 15, conceitual = FALSE) {
  t <- theme_minimal(base_size = base) +
    theme(
      plot.background  = element_rect(fill = cores[["papel"]], colour = NA),
      panel.background = element_rect(fill = cores[["papel"]], colour = NA),
      panel.grid.minor = element_blank(),
      text             = element_text(colour = cores[["texto"]]),
      axis.text        = element_text(colour = "#4a5568"),
      strip.text       = element_text(face = "bold", size = rel(1),
                                      colour = cores[["texto"]]),
      plot.title       = element_text(face = "bold", size = rel(1.05)),
      plot.subtitle    = element_text(colour = "#4a5568"),
      legend.position  = "bottom"
    )
  if (conceitual) {
    t <- t + theme(
      panel.grid = element_blank(),
      axis.text  = element_blank(),
      axis.line  = element_line(
        colour = cores[["texto"]], linewidth = .7,
        arrow = grid::arrow(length = grid::unit(.22, "cm"), type = "closed"))
    )
  }
  t
}
