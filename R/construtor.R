# Dados simulados do construtor de modelos (construa-um-modelo.qmd).
#
# O construtor precisa de um conjunto com uma resposta numérica, um preditor
# numérico e um fator com três níveis, para mostrar regressão, ANOVA de um
# fator e ANCOVA com os mesmos dados. Rode uma vez para gravar o CSV:
#
#   source(here::here("R/construtor.R"))
#   simula_girinos_lagoas() |> write.csv(here::here("data/girinos_lagoas.csv"), row.names = FALSE)

#' Girinos de três tipos de lagoa, criados em temperaturas diferentes
#'
#' A massa na metamorfose cresce com a temperatura da água (mesma inclinação
#' nas três lagoas) e muda com o tipo de lagoa.
#'
#' @param n_por_lagoa girinos por tipo de lagoa
#' @param inclinacao mg a mais por grau Celsius
#' @param efeito_lagoa diferença de cada lagoa em relação à temporária (mg)
#' @param sd desvio padrão do erro (mg)
#' @param seed semente
simula_girinos_lagoas <- function(n_por_lagoa = 15, inclinacao = 3,
                                  efeito_lagoa = c("Temporária" = 0, Permanente = -12, "Açude" = -20),
                                  sd = 8, seed = 42) {
  set.seed(seed)
  lagoa <- rep(names(efeito_lagoa), each = n_por_lagoa)
  temperatura <- round(runif(length(lagoa), 20, 32), 1)
  massa <- 30 + inclinacao * temperatura + efeito_lagoa[lagoa] + rnorm(length(lagoa), 0, sd)
  data.frame(
    individuo = seq_along(lagoa),
    lagoa = lagoa,
    temperatura_c = temperatura,
    massa_mg = round(massa, 1)
  )
}
