# Bioestatística

Materiais da disciplina obrigatória de **Bioestatística** (68 h) do curso de
Ciências Biológicas do Instituto de Biociências da Universidade Federal de
Mato Grosso do Sul.

**Site publicado: <https://provetelab.org/bioestatistica/>**

## O que há aqui

Onze aulas, cada uma em sua própria pasta com a página correspondente
(`index.qmd`). Os slides ficam em `slides/aNN.qmd`, em Quarto/revealjs,
com figuras geradas em R a partir de dados simulados. Os PDFs da versão
anterior (2025) continuam em `slides/` e são publicados para não quebrar
links antigos. O código das figuras
fica em `R/`, um arquivo por bloco da aula.

| Aula | Tópico |
|-----:|--------|
| 1 | Introdução à estatística biológica |
| 2 | Amostragem e delineamento |
| 3 | Organização e apresentação de dados |
| 4 | Medidas de tendência central e de dispersão |
| 5 | Probabilidade, variáveis aleatórias e principais distribuições |
| 6 | Estimativa pontual e intervalar |
| 7 | Testes de hipótese em estatística |
| 8 | Introdução aos modelos lineares: teste t de Student |
| 9 | Modelos lineares: regressão (e correlação) linear |
| 10 | Modelos lineares generalizados: teste de qui-quadrado |
| 11 | Modelos lineares: análise de variância (ANOVA) |

## Como o site é construído

Site [Quarto](https://quarto.org) publicado no GitHub Pages por GitHub
Actions (`.github/workflows/publish.yml`). **A pasta `docs/` não é
versionada** — ela é gerada a cada push. Para publicar uma alteração, basta
editar o `.qmd`, commitar e dar push.

Para trabalhar localmente:

```bash
quarto preview      # pré-visualiza com recarga automática
quarto render       # gera docs/
```

Os slides convertidos têm blocos de R. O projeto usa `freeze: auto`, e o
resultado da execução fica em `_freeze/`, que **é versionado**. O CI usa
esse resultado e precisa só do Quarto. Ao mudar um `.qmd` com código ou
uma função em `R/`, renderize localmente e commite `_freeze/` junto:

```bash
quarto render slides/a02.qmd
```

Pacotes usados: `ggplot2`, `dplyr`, `tidyr`, `purrr`, `tibble`,
`patchwork`, `ape`, `here` e `knitr`.

Se você mudar só uma função em `R/`, o Quarto não percebe que o `.qmd`
precisa ser executado de novo. Renderize o arquivo explicitamente, como
acima.

## Origem

Estes materiais viviam em `teaching/bioestatistica/` no repositório do site
do laboratório ([diogoprov.github.io](https://github.com/diogoprov/diogoprov.github.io)),
de onde foram extraídos com o histórico preservado. O histórico anterior à
migração continua disponível lá.

---

[Biodiversity Synthesis Lab](https://provetelab.org/) · Diogo B. Provete ·
Instituto de Biociências, UFMS
