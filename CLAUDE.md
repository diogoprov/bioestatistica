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
- **out/2026**: Aulas 2, 1, 3, 4, 5, 6, 7, 8, 9, 10 e 11 convertidas, nesta ordem. A Aula 2 serve de
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
| 10 | `aula-10-qui-quadrado` | **`slides/a10.qmd` ✔** | `Aula 10 - qui-quadrado.pptx` |
| 11 | `aula-11-anova` | **`slides/a11.qmd` ✔** | `Aula 11 - ANOVA.pptx` |

Os números dos arquivos originais não batem com os do site nas aulas 3 e 4:
a ordem foi trocada na reorganização pelo PPDAC. **A numeração que vale é a
do site.**

**As 11 aulas estão convertidas.** O próximo passo combinado é remodelar o
site inteiro para ficar coerente com os slides em Quarto, corrigir erros e
modernizar (veja a memória do projeto).

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
  | 1 | Introdução à estatística para biólogos | `f7bf73e0-37ad-4ef6-bc08-04c068028b17` |
  | 2 | Estatística, amostragem e delineamento experimental | `0eb9e376-fd88-4802-93c3-e613f655bec1` |
  | 3 | Gráficos, tabelas e armadilhas na visualização de dados | `0aab1775-914f-406c-acde-60851dc3f446` |
  | 4 | Revisão média e variância | `44148d47-cfae-4aaf-af68-81710e641a21` |
  | 5 | Probabilidade e distribuições de probabilidade | `c2996a96-77cb-49bc-a526-b2393a950a38` |
  | 6 | Intervalo de confiança | `5a154caf-a064-4482-9305-3970ebf3bdc6` |
  | 6 | Estimativa pontual e intervalar | `d308b304-73c3-45ee-80ff-df00e7760a7a` |
  | 7 | Teste de hipótese | `f81ec20f-f843-4cdb-957d-b2ac3922d48b` |
  | 8 | Teste t | `095cb8f0-fc9f-437a-9449-105b02de0750` |
  | 8 | Introdução aos Modelos Lineares | `dfd7caf5-696f-4d48-8d54-cdfb347ae74c` |
  | 9 | Correlação, regressão e armadilhas na análise de dados | `747d4b86-8cb4-4d54-93af-6aba4f7c3a0d` |
  | 10 | Qui-quadrado e Teste G | `24e6dd0f-ab42-4583-9b42-602515a0843c` |
  | 11 | ANOVA, ANCOVA e testes de hipóteses | `07605bb6-f29e-4e74-92a5-3390ee797242` |
  | revisão | Revisão estatística descritiva e inferencial | `41366a84-3e6e-4131-b17c-646ae6fbb2f8` |

- **Slido**: os `.pptx` das aulas 4 e 7 (numeração do site) têm enquetes do
  Slido. Mantenha a pergunta do original numa caixa `.atividade` com o link
  do evento da turma (na Aula 4: <https://app.sli.do/event/9EAtGB3oLhcpCV6HJqSJPX>; na Aula 7:
  <https://app.sli.do/event/c8RFbj984d5b8J7d15PigR>)
  e uma alternativa sem Slido; enquetes de múltipla escolha viram
  **perguntas para a turma** com a resposta em fragmento.
- **Perguntas para a turma** (*peer instruction*), usadas onde não há Kahoot
  ou Slido: um slide `## Pergunta para a turma {.smaller}` com a pergunta e
  quatro alternativas numa caixa `.atividade` (use `.medio` ou `.smaller`
  só se não couber); a instrução "votem sozinhos,
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
   `.tabela-grande`, `.codigo-compacto`, `.medio`, `.smaller`, `.columns`/`.column`.
   **Tamanho da letra:** o padrão é 32 px. Se o slide não couber, use
   `.medio` (~27 px) e só então `.smaller` (~22 px), medindo no navegador
   (out/2026: 111 slides voltaram à letra cheia e 41 foram para `.medio`).
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

### Site (depois da remodelação de out/2026)

- **Páginas:** `index.qmd` (curso em uma frase, PPDAC, trilha, cronograma
  em cartões), `comece-aqui.qmd`, `programa.qmd` e, no menu "Estudar",
  `exercicios-swirl.qmd`, `revisao.qmd` (resumos e Kahoots) e
  `glossario.qmd`. Estilos em `styles.css`.
- **YAML de cada aula:** `description` (o conteúdo do cartão), `gancho`
  (a pergunta do cartão) e `etapas` (lista com `problema`, `plano`,
  `dados`, `analise` e `conclusao`). Os cartões vêm de `_cronograma.ejs`.
- **Esqueleto da página:** faixa `.aula-meta` com data e selos, resumo,
  Slides, Em 3 minutos (três ideias e um erro comum), Conteúdo,
  Atividades, Leituras, swirl, Brinque com o código e Recursos externos.
- **Selo da etapa nos slides:** `slides/_metadata.yml` inclui
  `slides/_selo-etapa.html` em todos os decks. O selo do canto é copiado do
  slide de seção (`# N. Título {.secao}`) do bloco, então cada seção
  precisa dos seus selos.
- **Teste-se e Kahoot:** as perguntas das Aulas 1, 2, 3, 9 e 11 ficam em
  `quiz/aNN.json` (alternativas já embaralhadas, `correta` de 1 a 4). Elas
  alimentam o quiz da página (`::: {.quiz-teste-se data-quiz="../quiz/aNN.json"}`,
  script em `_teste-se.html`) e as planilhas de importação do Kahoot em
  `quiz/kahoot/`, já no modelo oficial do Kahoot. Para gerar de novo, baixe
  o modelo (support.kahoot.com, artigo 115002812547) e rode
  `python3 quiz/kahoot_xlsx.py modelo.xlsx`. O Kahoot limita a pergunta a
  120 caracteres e cada alternativa a 75, e não importa caracteres
  especiais (o script troca letras gregas e símbolos por texto).
- Se mudar a trilha ou as etapas de uma aula, atualize também a trilha em
  `index.qmd`, que é escrita à mão.

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
  licença em `assets/historia/FONTES.md`. **CC BY-SA também vale**
  (autorizado pelo Diogo em out/2026: o curso não é vendido), com crédito e
  licença na `.nota`.
- **Não mencione o original nos slides** ("O original dizia…", "No
  original…"): as correções entram direto no texto, sem nota.
- **Imagens geradas por IA** (Gamma, Gemini) são permitidas quando o Diogo
  pedir, com uma `.nota` dizendo que foram geradas por IA. Peça a imagem
  sem texto e escreva os rótulos no slide. Na Aula 7: os erros tipo I e II
  (`assets/aula07-erro-tipo*.jpg`) e os ícones da matriz do júri
  (`assets/aula07-juri-*.png`).
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
- **Exceções autorizadas pelo Diogo** (com crédito no slide), em
  `assets/terceiros/` (fontes em `assets/terceiros/FONTES.md`): a Tabela 1
  da apostila de Paglia & De Marco e a tirinha xkcd 552 (CC BY-NC), na
  Aula 9. Na Aula 3, a foto do histograma vivo da UConn (K. Hanley,
  *Hartford Courant*, 1996), citando Schilling et al. 2002. Na Aula 7, os
  retratos de Neyman (CC BY-SA) e de Egon Pearson (*fair use*), a foto de
  Popper (LSE) e a capa do *Logik der Forschung* (*fair use*). Digitalizações de obras do século XIX tiradas dos `.pptx` (as
  figuras de Galton de 1886 e 1888) também valem, porque estão em domínio
  público.
- **Memes**: GIFs e frames de séries ou filmes não entram. Use pinturas em
  domínio público do Commons com legenda própria, na classe `.meme` de
  `slides/theme.scss` (imagens em `assets/memes/`, fontes em
  `assets/memes/FONTES.md`), ou refaça o formato em R, como o *galaxy brain*
  da Aula 8 (`fig_galaxy_brain()` em `R/aula08.R`).
- **Animações (GIF):** feitas em R sem pacote extra. Os quadros são ggplots
  e o GIF é montado com o ffmpeg em `R/animacoes.R` (`gera_animacoes()`).
  Os GIFs prontos ficam em `assets/`: `aula09-soma-quadrados.gif`, nas
  Aulas 9 e 11, e `aula09-otimizacao.gif`, na Aula 9, com dois painéis
  (a reta a cada tentativa e o RMSE), como no `animate-lm-optimization.R`
  original.
- **Dados reais de domínio público** ficam em `data/`, com a origem em
  `data/FONTES.md` (por exemplo, `data/galton.csv`, de Galton 1886, usado na
  Aula 9). Os dados didáticos do curso também ficam ali (`data/girinos_2x2.csv`,
  da aula prática de ANOVA). Leia com `read.csv(here::here("data/..."))`.
- **Fio condutor das idades** (Gelman & Nolan 2002, seção 2.1): os dados
  da adivinhação de idades da Aula 1 voltam nas Aulas 3, 4, 6, 8, 9 e 11,
  em slides "De volta às idades". As funções ficam em `R/idades.R` e
  aceitam os dados da turma em `d` (colunas grupo, foto, idade_real,
  idade_estimada). As fotos do FACES (Max Planck, Berlim) só podem ser
  projetadas em sala, nunca publicadas no site.
- **Inferência por simulação** (out/2026): *bootstrap* na Aula 6
  (`fig_bootstrap()`, com as redes da pescaria) e teste de permutação na
  Aula 7 (`fig_permutacao()`, com os *Pinus*), cada um com uma atividade
  de papel e uma célula webR na página. Referências: Dormann & Ellison
  2025 e Manly & Navarro Alberto 2020. GLM/GLMM não entram na disciplina: a Aula 10 só tem
  um "Para saber mais" com links para Análise de Dados Univariados e o
  GLMM Analyzer de T. Pavlic.
- A bibliografia básica vem do PPC e não pode mudar; a complementar pode.
- Pacotes já em uso: `ggplot2`, `dplyr`, `tidyr`, `purrr`, `tibble`,
  `patchwork`, `ape`, `here` e `knitr`. Pergunte antes de adicionar outro.
- **R no navegador (webR, extensão quarto-live em `_extensions/r-wasm/`)**:
  o "Brinque com o código" das 11 páginas de aula roda no navegador (só
  nas páginas, não nos slides). As Aulas 9 e 11 também levam os CSVs de
  `data/` em `resources`. A página usa `format: live-html` + `engine: knitr`, inclui
  `_extensions/r-wasm/live/_knitr.qmd`, lista os pacotes em `webr: packages`
  e os arquivos de `R/` em `webr: resources` e `resources`. O `live-html`
  não herda as opções de `html` do `_quarto.yml` (tema, `styles.css`,
  `_teste-se.html`): elas vão repetidas no YAML da página. Uma célula
  oculta faz `source(f, local = TRUE)` dos arquivos de `R/`; sem
  `local = TRUE`, as funções não ficam visíveis para as outras células.
- **Subscritos Unicode (H₀, H₁) não aparecem nas figuras do R**: a fonte do
  dispositivo gráfico não tem o glifo. Nas figuras, escreva H0 e H1; no
  texto dos slides, H₀ e H₁ funcionam.
- Números em português: `R/tema.R` liga `options(OutDec = ",")`, então os
  eixos já saem com vírgula decimal. Em textos montados com `sprintf()`, use
  `format(..., decimal.mark = ",")` (há `num_br()` em `R/aula03.R`).

## Build e publicação

- O site é publicado por GitHub Actions (`.github/workflows/publish.yml`),
  que roda **só o Quarto, sem R**: os chunks funcionam graças ao `freeze`.
  A página inicial também tem um chunk (o diagrama PPDAC).
- Os PDFs antigos de `slides/` não têm mais link, mas entram em
  `resources` no `_quarto.yml` para que URLs antigas continuem valendo.
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

## Memória de sessões recentes (importada do Claude Code, 23/10/2026)

Condensado de `~/.claude/projects/-Users-diogoborgesprovete-GitHub-bioestatistica/memory/`,
onde está o detalhe completo. O que já está neste CLAUDE.md (regras de conteúdo,
CC BY, webR, quiz) não se repete aqui.

**O redesign do site está feito** (PRs diogoprov/bioestatistica#2–#8) — isto
supersede o "próximo passo" da seção *Situação das aulas*. No ar: trilha PPDAC
clicável na home, "Em 3 minutos" e "Brinque com o código" em todas as aulas,
página de Revisão e Kahoots, Glossário, badges PPDAC nos cabeçalhos, webR em todas
as páginas (40–60 s de carga na primeira vez, com R local como opcional).
Aberto: alinhamento entre título de página e de slide; ~57 itens de fidelidade de
baixa severidade. As datas do oferecimento condensado (15–27/02/2027, prova 22/02)
são detalhe — não gastar esforço nelas.

**Correções de conteúdo já aplicadas nos decks — não reverter:**
- a04: a 5ª altura é 1,83 m (não 1,85), para média 1,80.
- a05: eventos dependentes é P(A)·P(B|A); axioma 3 exige i ≠ j.
- a06: a não-tendenciosidade de x̄ não precisa de normalidade; s é levemente
  tendencioso para σ; a LGN é sobre n, não sobre o número de amostras; retrato
  de Markov removido do slide da TLC.
- a07: "arenoso" → "rochoso"; "aceitar H0" → "não rejeitar"; n = 30 (o original
  dizia n < 30); "1,80 desvios-padrão" → erros padrão; Welch em vez de
  `var.equal = TRUE`.
- a08: tabela dos lagartos tinha "killed"/"living" trocados; memes refeitos como
  pinturas de domínio público com legendas nossas; marcas de sopa anonimizadas.
- a09: H0 da inclinação é β ≠ 0; df da tabela ANOVA 927/926; −1 ≤ r ≤ 1 e
  0 ≤ r² ≤ 1; fator de encolhimento (1 − R²); P ajustados recalculados quando não
  fechavam com o F; "H0: t = 0" → ρ = 0.
- a10: o P ≈ 0,18 original da orquídea está errado — bicaudal
  `fisher.test` dá 0,242.
- a11: a fórmula do Tukey HSD estava sem o fator 1/2.
- Em todos: nada de `---` imediatamente antes de um `# Seção` (gera slide em branco).

**Armadilhas de build:** `ape::plot.phylo(plot = FALSE)` abre device — envolver em
`pdf(NULL)`/`dev.off()`; mermaid `%%{init}%%` é ignorado no revealjs (usar
`fig_diagrama()`); dentro de `mutate()`/`tibble()`, argumento com nome de coluna é
sombreado (renomear para cx, cy, k); `ggforce::geom_circle(fill = vetor)` falha;
adicionar/remover chunk renumera figuras — apagar `_freeze/slides/aNN` antes de
renderizar; testar overflow servindo `docs/` (porta 4321) e medindo
`slide.scrollHeight > 750`; screenshots costumam atrasar uma navegação (~1 s).
Antes de dar push, conferir `git log origin/main..HEAD`: este checkout já empurrou
commits locais de outras sessões.

**Atividades de Gelman & Nolan (2002)** — o fio de "adivinhar idades" está
implementado (Aulas 1, 3, 4, 6, 8, 9 e 11; `R/idades.R`). Propostas, ainda **não
aprovadas**, uma por aula: §8.1 "pesar uma amostra aleatória" (Aula 2); §8.3.3
"terra ou água?" (Aula 6); §8.7.2 data dredging com dados (Aula 7); §5.3.1+§8.3.2
ancoragem (Aula 8); §4.3.1 regressão à média (Aula 9); §5.3.3+§8.5.3 teste de
sabor (Aula 10). Fotos do FACES: projetar em sala, nunca publicar no site.
