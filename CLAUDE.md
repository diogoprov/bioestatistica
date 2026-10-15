# Bioestatística: conversão dos slides para Quarto/revealjs

Repositório da disciplina obrigatória de **Bioestatística** (68 h), Ciências
Biológicas, INBio/UFMS. Site publicado em <https://provetelab.org/bioestatistica/>.
Responda em português.

## Objetivo

Converter as 11 aulas de graduação, hoje em PowerPoint/PDF, em slides
**Quarto/revealjs** (`slides/aNN.qmd`), com **todas as figuras geradas em R a
partir de dados simulados**. Os alunos devem poder abrir o código em `R/`,
mudar os parâmetros e ver o que acontece.

## Histórico

- **08/jun/2026**: o curso entra como prova de conceito em
  `teaching/bioestatistica/` no repositório do site do laboratório
  (`diogoprov/diogoprov.github.io`).
- **18/jun/2026**: as aulas são reordenadas para seguir o ciclo **PPDAC**
  (Problema → Plano → Dados → Análise → Conclusão), e delineamento passa a
  ser a Aula 2.
- **set/2026**: o curso é extraído para este repositório, com o histórico
  preservado. Depois vieram o scaffolding do site (`_quarto.yml`, estilos e
  workflow do Pages), o README e a extensão fontawesome.
- A **Aula 2** é a única convertida até agora e serve de modelo para as
  demais.

## Situação das aulas

| Aula | Pasta | Slides hoje | Original (.pptx) |
|-----:|-------|-------------|------------------|
| 1 | `aula-01-introducao` | PDF | `Aula 1 - Intodução.pptx` |
| 2 | `aula-02-amostragem` | **`slides/a02.qmd` ✔** | `Aula 2 - Delineamento amostral.pptx` |
| 3 | `aula-03-organizacao-dados` | PDF | `Aula 4 - organizacao e visualizacao dados.pptx` |
| 4 | `aula-04-tendencia-dispersao` | PDF | `Aula 3 - medidas tendencia central.pptx` |
| 5 | `aula-05-probabilidade` | PDF | `Aula 5 - Probabilidade e distribuições.pptx` |
| 6 | `aula-06-estimativas` | PDF | `Aula 6 - Estimativas.pptx` (há também `Aula 6 - Distribuicoes estatisticas.pptx`) |
| 7 | `aula-07-teste-hipoteses` | PDF | `Aula 7 - Teste de hipótese.pptx` |
| 8 | `aula-08-modelos-lineares-teste-t` | PDF | `Aula 8 - Teste t.pptx` |
| 9 | `aula-09-regressao-correlacao` | PDF | `Aula 9 - Regressão linear.pptx` |
| 10 | `aula-10-qui-quadrado` | PDF | `Aula 10 - qui-quadrado.pptx` |
| 11 | `aula-11-anova` | PDF | `Aula 11 - ANOVA.pptx` |

Os números dos arquivos originais não batem com os do site nas aulas 3 e 4:
a ordem foi trocada na reorganização pelo PPDAC. **A numeração que vale é a
do site.**

Ainda não há uma ordem combinada para as próximas conversões. A sugestão é
seguir a sequência do curso (Aula 1, depois 3, e assim por diante).
Confirme com o Diogo antes de começar.

### Onde estão os originais (no Mac)

```
~/Library/CloudStorage/OneDrive-Pessoal/Aulas/2017 - Estatística aplicada à biologia - UFMS/
```

A pasta também tem material de apoio útil:

- `Transcrição áudio aula delineamento amostral.docx/.pdf`
- `transcrição aula probabilidade 2.docx` e `Resumo da Aula de Probabilidade 2.docx`
- gravações `.mp3` (estimativa pontual, regressão, teste de hipótese)
- `animate-lm-optimization.R` e os GIFs da animação do ajuste da reta (Aula 9)
- `exemplo_histograma.r` e `exercicio_histograma.R` (Aula 3)
- exercícios de ANOVA e o `aula_pratica_ANOVA.zip` (Aula 11)
- `Aula revisão.pptx` e o script da revisão da P1

## Modelo a seguir: Aula 2

### Arquivos

- `slides/a02.qmd`: os slides (~1140 linhas, 59 slides `##`)
- `slides/theme.scss`: o tema compartilhado por todas as aulas
- `R/tema.R`: paleta `cores`, `cores_niveis`, `cores_solo` e `tema_aula(base = 15, conceitual = FALSE)`
- `R/ppdac.R`: `ppdac_plot(destaque = c(...))`, o diagrama do ciclo com as etapas da aula destacadas
- `R/aula02_pergunta.R`, `R/aula02_amostragem.R`, `R/aula02_delineamento.R`:
  **um arquivo R por bloco da aula**, só com funções que desenham as figuras
- `aula-02-amostragem/index.qmd`: a página da aula, com botão para os slides

### YAML dos slides

Copie o cabeçalho de `slides/a02.qmd`. Os pontos principais são
`theme: [default, theme.scss]`, `slide-number: c/t`, `width: 1250`,
`height: 750`, `auto-stretch: false`, `footer: "Bioestatística · Ciências Biológicas/UFMS"`,
fundo das figuras `#faf7f0` em `knitr: opts_chunk: dev.args: bg`, e
`execute: echo: false`, `fig-width: 10`, `fig-height: 4.4`. Mantenha `lang: pt`
e `author: "Diogo B. Provete"`. A data vem do `index.qmd` da aula.

### Estrutura dos slides

1. Um chunk `setup` com `source(here::here("R/..."))` para `ppdac.R` e para
   os arquivos da aula.
2. `## Hoje`: a lista numerada dos blocos, seguida de um `::: {.nota}`
   dizendo que todas as figuras foram geradas em R com dados simulados e
   apontando para `R/` no GitHub.
3. `## Onde estamos no ciclo PPDAC`, com `ppdac_plot(destaque = ...)`.
4. Cada bloco abre com `# N. Título {.secao}` seguido de
   `[Etapa]{.ppdac-etapa}` (por exemplo `[Plano → Dados]{.ppdac-etapa}`).
5. Classes do tema disponíveis: `.secao`, `.ppdac-etapa`, `.definicao`,
   `.nota`, `.ref` (referências em itálico pequeno), `.atividade`,
   `.tabela-grande`, `.codigo-compacto`, `.smaller`, `.columns`/`.column`.
6. Toda figura leva `fig-alt` descritivo em português.

### Página da aula (`aula-NN-*/index.qmd`)

Troque o botão do PDF:

```markdown
[{{< fa solid file-pdf >}} Baixar slides em PDF](../slides/aula-NN-....pdf){.btn .btn-outline-primary}
```

pelo botão dos slides, acompanhado do parágrafo sobre o código em `R/`:

```markdown
[{{< fa solid display >}} Abrir slides](../slides/aNN.html){.btn .btn-outline-primary}
```

Na Aula 2, a página também ganhou um parágrafo de resumo (com a etapa do
PPDAC) e uma seção `## Conteúdo` que lista os blocos. O PDF antigo da Aula 2
continua em `slides/`, então não apague os PDFs.

## Regras de conteúdo

- **Figuras só com dados simulados.** Não use figuras copiadas de livros ou
  artigos. Uma figura do original que venha de outra fonte deve ser
  **refeita em R** ou substituída por uma explicação.
- **Exceção: imagens sob CC0 ou CC-BY** (por exemplo, silhuetas do
  [PhyloPic](https://www.phylopic.org)) podem ser usadas, sempre com o
  crédito de autor e licença num `::: {.nota}` do slide. Ficam em
  `assets/`. Na Aula 2, o pictograma `assets/aula02-tres-cenarios.svg`
  (os três cenários de pseudorréplica) foi feito assim, adaptado do E7 de
  Análise de Dados Univariados.
- O conteúdo segue o pptx original. Melhorias de didática são bem-vindas,
  mas mudanças de escopo precisam ser combinadas antes.
- **Não invente referências, DOIs nem URLs.** Use as que já estão no
  original ou no `index.qmd` da aula. **Exceção:** uma referência nova é
  permitida se for verificada no Crossref (DOI, autores, ano, periódico,
  volume e páginas conferidos em `api.crossref.org`). Cite só o que o
  Crossref confirmou; por exemplo, se ele der só a primeira página, cite só
  a primeira página. Livros sem DOI podem ser conferidos no Open Library.
  Se não der para verificar, deixe `<!-- TODO: referência -->` e avise.
  Na Aula 2 entraram assim Wild & Pfannkuch 1999, Wiens 1989,
  Stewart-Oaten et al. 1986 e Krzywinski & Altman 2014.
- Pacotes já em uso: `ggplot2`, `dplyr`, `tidyr`, `purrr`, `tibble`,
  `patchwork`, `ape`, `here` e `knitr`. Pergunte antes de adicionar outro.

## Build e publicação

- O site é publicado por GitHub Actions (`.github/workflows/publish.yml`),
  que roda **só o Quarto, sem R**. O comentário no topo do workflow diz que
  não há chunks executáveis, mas isso está desatualizado: os chunks existem
  e funcionam graças ao `freeze`.
- `freeze: auto` está ligado. O resultado da execução fica em `_freeze/`,
  que **é versionado**. Por isso, depois de mudar um `.qmd` com código ou
  uma função em `R/`, renderize **localmente** e commite `_freeze/` junto:

  ```bash
  quarto render slides/aNN.qmd
  quarto preview            # para conferir o site inteiro
  git add -A
  git commit -m "Aula NN: slides em Quarto/revealjs"
  git push
  ```

- Se mudou só uma função em `R/`, o Quarto não percebe. Renderize o `.qmd`
  explicitamente.
- `docs/` não é versionado; o CI gera essa pasta.

## Cursos irmãos (mesmo padrão)

- `~/GitHub/intro-r`: Introdução ao R (slides em `slides/aula-NN.qmd`)
- `~/GitHub/multidimensionais`: Métodos multidimensionais (pós-graduação)
- `~/Library/CloudStorage/OneDrive-Pessoal/website_github/diogoprov.github.io`:
  site do laboratório, provetelab.org
