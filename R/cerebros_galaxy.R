# Gera os quatro cérebros do "galaxy brain" da Aula 8 com o cerebroViz
# (Bahl et al., <https://github.com/ethanbahl/cerebroViz>, GPL-3): a cada
# nível, mais lobos acendem. Rode uma vez; os PNGs ficam em assets/ e
# fig_galaxy_brain() (R/aula08.R) só os lê, então o cerebroViz não é
# necessário para renderizar os slides.
#
#   remotes::install_github("ethanbahl/cerebroViz")
#   source(here::here("R/cerebros_galaxy.R")); gera_cerebros()

gera_cerebros <- function(dir = here::here("assets")) {
  # o cerebroViz testa `class(x) != "matrix"`, que quebra no R >= 4.0
  # (class() de uma matriz tem dois elementos); trocamos por is.matrix()
  src <- deparse(cerebroViz::cerebroViz)
  src <- gsub('class(x) != "matrix"', '!is.matrix(x)', src, fixed = TRUE)
  cv <- eval(parse(text = src))
  environment(cv) <- asNamespace("cerebroViz")
  regioes <- cerebroViz::regionMap$cerebroViz
  # lobos visíveis na vista lateral, acesos de trás para a frente
  acesos <- list(c("OL"), c("OL", "TL"), c("OL", "TL", "PL"),
                 c("OL", "TL", "PL", "FL", "CB", "BS"))
  tmp <- tempfile(); dir.create(tmp)
  for (i in seq_along(acesos)) {
    # valores 0/1 zeram o intervalo interquartil e quebram o corte de outliers
    # do pacote; um pequeno espalhamento (apagadas ~1, acesas ~10) resolve
    v <- seq(0, .5, length.out = length(regioes))
    x <- matrix(ifelse(regioes %in% acesos[[i]], 9.5 + v, 1 + v), ncol = 1, dimnames = list(regioes, NULL))
    pref <- file.path(tmp, paste0("nivel", i))
    suppressWarnings(cv(x, filePrefix = pref, palette = c("#d8c9cb", "#ffd23f"),
                        clamp = 100, legend = FALSE))
    arq <- file.path(dir, sprintf("aula08-cerebro-%d.png", i))
    rsvg::rsvg_png(paste0(pref, "_outer_1.svg"), arq, width = 500)
    # o SVG vem com fundo branco; o branco vira transparente para o brilho
    # do galaxy brain aparecer em volta do cérebro
    im <- png::readPNG(arq)
    if (dim(im)[3] == 3) im <- array(c(im, array(1, dim(im)[1:2])), c(dim(im)[1:2], 4))
    im[, , 4][im[, , 1] > .97 & im[, , 2] > .97 & im[, , 3] > .97] <- 0
    # corta as margens transparentes
    lin <- which(rowSums(im[, , 4]) > 0); col <- which(colSums(im[, , 4]) > 0)
    png::writePNG(im[min(lin):max(lin), min(col):max(col), ], arq)
  }
  invisible(file.path(dir, sprintf("aula08-cerebro-%d.png", seq_along(acesos))))
}
