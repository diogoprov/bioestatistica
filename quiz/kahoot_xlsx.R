# Gera as planilhas de importação do Kahoot (quiz/kahoot/aNN-kahoot.xlsx) a
# partir das perguntas em quiz/aNN.json, as mesmas do "Teste-se" do site.
# Colunas na ordem do modelo do Kahoot (Question, Answer 1-4, Time limit,
# Correct answer). Rode da raiz do repositório: Rscript quiz/kahoot_xlsx.R
# O Kahoot não importa caracteres especiais pela planilha: letras gregas e
# símbolos viram texto (o site continua com os símbolos).
sem_simbolos <- function(x) {
  trocas <- c("−∞" = "-infinito", "+∞" = "+infinito", "−" = "-", "≠" = "diferente de",
              "βx" = "beta*x", "α" = "alfa", "β" = "beta", "ε" = "erro", "μ" = "mi", "≤" = "<=", "²" = "2")
  for (k in names(trocas)) x <- gsub(k, trocas[[k]], x, fixed = TRUE)
  x
}

for (f in Sys.glob(here::here("quiz/a*.json"))) {
  q <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  q <- lapply(q, \(x) { x$pergunta <- sem_simbolos(x$pergunta)
                         x$alternativas <- lapply(x$alternativas, sem_simbolos); x })
  d <- data.frame(
    `Question - max 120 characters` = vapply(q, \(x) x$pergunta, ""),
    `Answer 1 - max 75 characters`  = vapply(q, \(x) x$alternativas[[1]], ""),
    `Answer 2 - max 75 characters`  = vapply(q, \(x) x$alternativas[[2]], ""),
    `Answer 3 - max 75 characters`  = vapply(q, \(x) x$alternativas[[3]], ""),
    `Answer 4 - max 75 characters`  = vapply(q, \(x) x$alternativas[[4]], ""),
    `Time limit (sec) – 5, 10, 20, 30, 60, 90, 120, or 240 secs` = vapply(q, \(x) as.numeric(x$tempo), 0),
    `Correct answer(s) - choose at least one` = vapply(q, \(x) as.numeric(x$correta), 0),
    check.names = FALSE
  )
  saida <- here::here("quiz/kahoot", sub("\\.json$", "-kahoot.xlsx", basename(f)))
  writexl::write_xlsx(d, saida)
  message("ok ", saida)
}
