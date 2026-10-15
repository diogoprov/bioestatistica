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
- **out/2026**: Aulas 2, 1, 3, 4, 5, 6, 7, 8 e 9 convertidas, nesta ordem. A Aula 2 serve de
  modelo para as demais; a identidade visual do PPDAC (uma cor por etapa)
  entrou junto com a Aula 1.

## Situação das aulas

| Aula | Pasta | Slides hoje | Original (.pptx) |
|-----:|-------|-------------|------------------|
| 1 | `aula-01-introducao` | **`slides/a01.qmd` ✔** | `Aula 1 - Intodução.pptx` |
| 2 | `aula-02-amostragem` | **`slides/a02.qmd` ✔** | `Aula 2 - Delineamento amostral.pptx` |
| 3 | `aula-03-organizacao-dados` | **`slides/a03.qmd` ✔** | `Aula 4 - organizacao e visualizacao dados.pptx` |
| 4 | `aula-04-tendencia-dispersao` | **`slides/a04.qmd` ✔** | `Aula 3 - medidas tendencia central.pptx` |
| 5 | `aula-05-probabilidade` | **`slides/a05.qmd` ✔** | `Aula 5 - Probabilidade e distribuições.pptx` |
| 6 | `aula-06-estimativas` | **`slides/a06.qmd` ✔** | `Aula 6 - Estimativas.pptx` (há também `Aula 6 - Distribuicoes estatisticas.pptx`) |
| 7 | `aula-07-teste-hipoteses` | **`slides/a07.qmd` ✔** | `Aula 7 - Teste de hipótese.pptx` |
| 8 | `aula-08-modelos-lineares-teste-t` | **`slides/a08.qmd` ✔** | `Aula 8 - Teste t.pptx` |
| 9 | `aula-09-regressao-correlacao` | **`slides/a09.qmd` ✔** | `Aula 9 - Regressão linear.pptx` |
| 10 | `aula-10-qui-quadrado` | PDF | `Aula 10 - qui-quadrado.pptx` |
| 11 | `aula-11-anova` | PDF | `Aula 11 - ANOVA.pptx` |

Os números dos arquivos originais não batem com os do site nas aulas 3 e 4:
a ordem foi trocada na reorganização pelo PPDAC. **A numeração que vale é a
do site.**

As conversões seguem a sequência do curso: a próxima é a **Aula 10**
(original: `Aula 10 - qui-quadrado.pptx`), que tem o Kahoot "Qui-quadrado
e Teste G".

### Fonte da conversão: sempre o `.pptx` mais recente

Use o `.pptx` da pasta do OneDrive, não o PDF de `slides/`. Os PDFs do
repositório são de 2025, e alguns `.pptx` foram atualizados depois. A Aula
2 foi convertida do PDF antigo e teve de ser completada depois com os
slides novos do `.pptx`. Extraia também as notas do apresentador, que vão
para `::: {.nota}`.

### Preserve o original

Mantenha ao máximo o texto dos slides (corrija só erros óbvios de
digitação) e todos os esquemas e figuras, redesenhados em R. Quando o
original mostra uma tabela ou um gráfico com dados de um livro, refaça com
dados simulados num contexto biológico e diga no slide "dados fictícios"
ou "dados simulados", com "Inspirado na Figura X de ..." na `.ref`. Dados
públicos verificáveis (como o Censo 2010 da Aula 3) podem ser mantidos. Uma figura só
sai quando não tem como ser refeita nem existe versão com licença aberta;
nesse caso, troque por texto ou referência.

### Aulas pesadas: aula enxuta + "Para saber mais"

Quando a aula fica pesada demais para o tempo (como a Aula 5), o material
formal sai do fluxo da aula, mas **não some**: vai para uma seção final
`# Para saber mais {.secao}`, no fim do deck. Na Aula 5 foram para lá os
axiomas, o teorema de Bayes algébrico, as fórmulas com integral, a *t* de
Student e a tabela de modelos contínuos. No lugar entram ganchos e
atividades (moeda real × inventada, aniversários, "O teste que mente") e um
fio condutor biológico (a vida do girino). Combine com o Diogo antes de
aplicar isso numa aula.

### Aprendizagem ativa (inegociável)

- Todas as atividades do original ficam, em caixas `::: {.atividade}`.
- **Kahoot**: o perfil público é
  <https://create.kahoot.it/profiles/1d311cc2-3d7f-4365-96a5-af3d40f3f73a>.
  Mantenha o Kahoot da aula com um link para
  `https://create.kahoot.it/details/<id>`. Mapa atual (numeração do site):

  | Aula | Kahoot | id |
  |-----:|--------|----|
  | 1, 2, 3, 9, 11 | (não há) | |
  | 4 | Revisão média e variância | `44148d47-cfae-4aaf-af68-81710e641a21` |
  | 5 | Probabilidade e distribuições de probabilidade | `c2996a96-77cb-49bc-a526-b2393a950a38` |
  | 6 | Intervalo de confiança | `5a154caf-a064-4482-9305-3970ebf3bdc6` |
  | 6 | Estimativa pontual e intervalar | `d308b304-73c3-45ee-80ff-df00e7760a7a` |
  | 7 | Teste de hipótese | `f81ec20f-f843-4cdb-957d-b2ac3922d48b` |
  | 8 | Teste t | `095cb8f0-fc9f-437a-9449-105b02de0750` |
  | 8 | Introdução aos Modelos Lineares | `dfd7caf5-696f-4d48-8d54-cdfb347ae74c` |
  | 10 | Qui-quadrado e Teste G | `24e6dd0f-ab42-4583-9b42-602515a0843c` |
  | revisão | Revisão estatística descritiva e inferencial | `41366a84-3e6e-4131-b17c-646ae6fbb2f8` |

- **Slido**: os `.pptx` das aulas 4 e 7 (numeração do site) têm enquetes do
  Slido. Mantenha a pergunta do original numa caixa `.atividade` com o link
  do evento da turma (na Aula 4: <https://app.sli.do/event/9EAtGB3oLhcpCV6HJqSJPX>; na Aula 7:
  <https://app.sli.do/event/c8RFbj984d5b8J7d15PigR>)
  e uma alternativa sem Slido; enquetes de múltipla escolha viram
  **perguntas para a turma** com a resposta em fragmento.
- **Perguntas para a turma** (*peer instruction*), usadas onde não há Kahoot
  ou Slido: um slide `## Pergunta para a turma {.smaller}` com a pergunta e
  quatro alternativas numa caixa `.atividade`; a instrução "votem sozinhos,
  discutam com quem está ao lado e votem de novo"; e a resposta com a
  justificativa num `::: {.fragment}` com `.definicao`. Veja os três
  exemplos em `slides/a01.qmd`.

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
- `R/tema.R`: paletas `cores`, `cores_ppdac`, `cores_niveis`, `cores_solo` e `tema_aula(base = 15, conceitual = FALSE)`
- `R/ppdac.R`: `ppdac_plot(destaque = c(...))`, o diagrama do ciclo com as
  etapas da aula destacadas, e `ppdac_topicos(list(Problema = ..., ...))`, o
  ciclo com os tópicos de cada etapa
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
4. Cada bloco abre com `# N. Título {.secao}` seguido do selo da etapa.
   **Não ponha `---` antes de um `# `**: o título de nível 1 já abre um
   slide novo, e o `---` gera um slide em branco antes dele. O selo vem
   `[Plano]{.ppdac-etapa .plano}`. Duas etapas viram dois selos:
   `[Plano]{.ppdac-etapa .plano} → [Dados]{.ppdac-etapa .dados}`. As classes
   de etapa são `.problema`, `.plano`, `.dados`, `.analise` e `.conclusao`.
5. **Identidade visual do PPDAC:** cada etapa tem uma cor fixa, a mesma no
   diagrama (`cores_ppdac` em `R/tema.R`) e nos selos (`$ppdac-*` em
   `slides/theme.scss`). Problema `#a15e3f`, Plano `#1c6e8c`, Dados
   `#527238`, Análise `#96661f`, Conclusão `#6b4c8a`. Se mudar uma cor, mude
   nos dois lugares. Use essas cores sempre que uma figura se referir a uma
   etapa do ciclo.
6. Classes do tema disponíveis: `.secao`, `.ppdac-etapa`, `.definicao`,
   `.nota`, `.ref` (referências em itálico pequeno), `.atividade`,
   `.tabela-grande`, `.codigo-compacto`, `.smaller`, `.columns`/`.column`.
7. Toda figura leva `fig-alt` descritivo em português.

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
- Silhuetas do PhyloPic ficam em `assets/phylopic/`, com autor e licença em
  `assets/phylopic/FONTES.md`. A API exige o parâmetro `build` (consulte
  `https://api.phylopic.org/`, que redireciona para `/?build=N`).
- **Figura com licença CC BY (ou CC0): use a original, não refaça.** Artigos
  com licença CC BY têm a figura original reproduzida, com
  crédito. Confira o campo `license` em `api.crossref.org/works/<DOI>`. Dos
  artigos usados nos originais, são CC BY 4.0: Simmonds et al. 2024 (TREE),
  Gerstner et al. 2017 (MEE) e Weissgerber et al. 2015 (PLOS Biology). Não
  são: Nature Methods (Krzywinski & Altman), Wiley/ESA (Hurlbert 1984,
  Ives & Zhu 2006) e JASA (Cleveland & McGill 1984). Livros-texto (Vieira,
  Triola, Bussab & Morettin, Gotelli & Ellison, Zar, Quinn & Keough, Ruxton
  & Colegrave) não têm licença aberta: as figuras deles continuam sendo
  redesenhadas.
- **Figuras do livro *Análises ecológicas no R*** (Da Silva et al. 2022,
  CC BY-NC 4.0; o Diogo é coautor) podem ser usadas como estão, com
  crédito, em `assets/livro-aer/` (fontes em `assets/livro-aer/FONTES.md`).
  As figuras ficam em <https://paternogbc.github.io/livro_aer/>; o teste
  *t* está no cap. 7.
- **Domínio público** também vale (por exemplo, retratos e frontispícios
  históricos do Wikimedia Commons). Confira a licença na API do Commons
  (`LicenseShortName` igual a "Public domain" ou "CC0"), baixe uma versão
  reduzida para `assets/historia/` e registre arquivo, original, autor e
  licença em `assets/historia/FONTES.md`. **CC BY-SA não serve.**
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
- **Dados reais de domínio público** ficam em `data/`, com a origem em
  `data/FONTES.md` (por exemplo, `data/galton.csv`, de Galton 1886, usado na
  Aula 9). Leia com `read.csv(here::here("data/..."))`.
- Pacotes já em uso: `ggplot2`, `dplyr`, `tidyr`, `purrr`, `tibble`,
  `patchwork`, `ape`, `here` e `knitr`. Pergunte antes de adicionar outro.
- **Subscritos Unicode (H₀, H₁) não aparecem nas figuras do R**: a fonte do
  dispositivo gráfico não tem o glifo. Nas figuras, escreva H0 e H1; no
  texto dos slides, H₀ e H₁ funcionam.
- Números em português: `R/tema.R` liga `options(OutDec = ",")`, então os
  eixos já saem com vírgula decimal. Em textos montados com `sprintf()`, use
  `format(..., decimal.mark = ",")` (há `num_br()` em `R/aula03.R`).

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
