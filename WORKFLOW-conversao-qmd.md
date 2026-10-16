# Workflow: converter aulas em PowerPoint para Quarto/revealjs

Este arquivo descreve como as 11 aulas de **Bioestatística** (graduação,
UFMS) foram convertidas de `.pptx` para slides Quarto/revealjs, com
figuras geradas em R e o site remodelado em seguida. Ele serve de semente
para uma nova sessão que faça o mesmo com outra disciplina, a começar por
**Métodos multidimensionais** (`~/GitHub/multidimensionais`,
<https://provetelab.org/multidimensionais/>).

Leia junto o `CLAUDE.md` deste repositório (`~/GitHub/bioestatistica`):
ele é a versão completa e testada das regras resumidas aqui, e a Aula 2
(`slides/a02.qmd` e `R/aula02_*.R`) é o modelo de referência.

---

## 1. Como trabalhar com o Diogo

- Converse em **português**. O conteúdo das aulas também é em português.
  Termos técnicos (*metacommunity*, *beta diversity*) podem ficar em
  inglês.
- Aplique sempre a skill **karpathy-guidelines**: mudanças cirúrgicas,
  nada de abstração prematura, critério de sucesso verificável.
- **Commit e push só com aprovação explícita de cada lote.** Mostre o
  resultado antes ("me mostre antes de commitar"). Aprovação de um lote
  não vale para o próximo.
- Antes de reescrever muito ou mudar escopo (cortar conteúdo, criar
  seção "Para saber mais"), **combine primeiro**.
- Ele manda correções no meio do trabalho (um link de slide e "o texto
  está sobreposto aqui"). Trate na hora, sem perder o fio do que estava
  fazendo.
- Aponte os erros que encontrar no original (contas, fórmulas, tabelas
  trocadas): corrija no `.qmd` e **relate a correção** no fim do lote.

## 2. Fases do projeto (na ordem em que funcionou)

1. **Preparar o repositório.** Site Quarto (`_quarto.yml`, `styles.css`,
   workflow do GitHub Pages) e um `CLAUDE.md` com o plano, a tabela de
   situação das aulas e as regras. A memória do projeto guarda as
   preferências e as armadilhas que surgirem.
2. **Converter uma aula-modelo** (foi a Aula 2) e acertar com o Diogo o
   tema, a estrutura e o tom antes de seguir. As outras copiam o modelo.
3. **Converter aula a aula**, uma por lote: converter, renderizar,
   conferir, mostrar, commitar com aprovação e atualizar o `CLAUDE.md` e a
   memória.
4. **Remodelar o site** para ficar coerente com os slides (cartões por
   aula, "Comece aqui", glossário, revisão). Foi feito num PR separado.
5. **Extras de aprendizagem ativa**:
   - "Teste-se" (10 perguntas comentadas por aula, em `quiz/aNN.json`);
   - planilhas de importação do Kahoot;
   - "Brinque com o código" com R no navegador (webR/quarto-live);
   - atividades de Gelman & Nolan (*Teaching Statistics*).

## 3. Pipeline de uma aula

### 3.1 Fonte

- Use **sempre o `.pptx` mais recente** da pasta do OneDrive da
  disciplina, não o PDF antigo do repositório.
  - A pasta da Bioestatística é
    `~/Library/CloudStorage/OneDrive-Pessoal/Aulas/2017 - Estatística aplicada à biologia - UFMS/`.
  - Para a multivariada, **pergunte onde estão os `.pptx`**.
- Descompacte numa pasta própria do scratchpad, porque é dado não
  confiável. Extraia o texto dos slides e as **notas do apresentador**,
  que viram `::: {.nota}`:

  ```bash
  mkdir -p "$TMPDIR/aNN" && unzip -o -q "Aula N - ....pptx" -d "$TMPDIR/aNN"
  for f in $(ls "$TMPDIR"/aNN/ppt/slides/slide*.xml | sort -V); do
    echo "== $f"; sed 's/<[^>]*>/ /g' "$f" | tr -s ' '; done
  ls "$TMPDIR"/aNN/ppt/notesSlides/ "$TMPDIR"/aNN/ppt/media/
  ```

- Procure material de apoio na mesma pasta: transcrições de aula,
  gravações, scripts R e exercícios.
- Os números das aulas podem não bater com os do site. **Vale a
  numeração do site.**

### 3.2 Inventário das figuras

Classifique cada imagem do `.pptx` antes de escrever código:

| Origem da figura | O que fazer |
|---|---|
| Esquema ou diagrama | Redesenhar em R (ggplot2) |
| Gráfico ou tabela com dados de livro | Refazer com **dados simulados** num contexto biológico; no slide, "dados simulados"; na `.ref`, "Inspirado na Figura X de ..." (só cite o número da figura se o original der) |
| Artigo **CC BY / CC0** (confira o campo `license` em `api.crossref.org/works/<DOI>`) | **Usar a original, não refazer**, com crédito |
| Livro do próprio Diogo (*Análises ecológicas no R*, CC BY-NC) | Usar como está, com crédito, em `assets/livro-aer/` |
| Domínio público ou CC0 no Wikimedia Commons (`LicenseShortName`) | Usar, com o registro em `assets/historia/FONTES.md` |
| Silhuetas do PhyloPic (CC0/CC BY; a API exige `?build=N`) | Usar, com crédito na `.nota` e em `assets/phylopic/FONTES.md` |
| **CC BY-SA**, Wiley/Elsevier/Nature sem licença aberta, livros-texto | Redesenhar ou trocar por texto, **salvo exceção autorizada pelo Diogo** (vai para `assets/terceiros/FONTES.md`) |
| GIFs e frames de filmes ou séries (memes), imagens de banco de imagens, imagens geradas por IA | Retirar. Para memes, usar pinturas em domínio público com legenda própria, ou refazer o formato em R |

Silhuetas do PhyloPic dentro de figuras R:

- `assets/phylopic/svg2csv.py` converte o SVG em polígonos.
- `silhueta()`, em `R/tema.R`, desenha esses polígonos com
  `geom_polygon(rule = "evenodd")`.
- Assim não é preciso nenhum pacote novo.

### 3.3 Código R

- **Um arquivo R por bloco da aula** (`R/aulaNN.R` ou
  `R/aulaNN_bloco.R`), só com funções `fig_*()` que desenham as figuras.
  Cada função fixa a própria semente.
- `R/tema.R` concentra o que é comum:
  - as paletas e `tema_aula()`;
  - fundo `#faf7f0`;
  - `options(OutDec = ",")`, para a vírgula decimal;
  - `silhueta()`.
- Pacotes permitidos: `ggplot2`, `dplyr`, `tidyr`, `purrr`, `tibble`,
  `patchwork`, `ape`, `here` e `knitr`. **Pergunte antes de adicionar
  outro.** Na multivariada, `vegan` provavelmente entra; confirme.
- Dados reais só quando forem públicos e verificáveis, ou dados do
  próprio Diogo com a permissão dele. Ficam em `data/`, com a origem em
  `data/FONTES.md`.

### 3.4 O `.qmd` dos slides

- Copie o YAML de `slides/a02.qmd`:
  - `theme: [default, theme.scss]`, `width: 1250`, `height: 750`;
  - `auto-stretch: false`, `slide-number: c/t`, o footer;
  - `execute: echo: false`, `fig-width: 10`, `fig-height: 4.4`;
  - `dev.args: bg` igual ao fundo `#faf7f0`;
  - `lang: pt`.
- Estrutura do deck:
  1. chunk `setup` com `source(here::here("R/..."))`;
  2. `## Hoje`, com uma `.nota` sobre as figuras em R;
  3. o diagrama do ciclo (PPDAC) com as etapas da aula destacadas;
  4. os blocos, cada um aberto com `# N. Título {.secao}` e o selo da
     etapa.
- **Não ponha `---` antes de um `# `**: isso gera um slide em branco.
- Classes do tema: `.secao`, `.definicao`, `.nota`, `.ref`, `.atividade`,
  `.tabela-grande`, `.codigo-compacto`, `.smaller` e `.columns`.
- Toda figura leva `fig-alt` descritivo em português.
- **Preserve o texto do original**, corrigindo só erros óbvios de
  digitação. Melhorias didáticas são bem-vindas; mudanças de escopo, só
  combinadas.

### 3.5 Aprendizagem ativa (inegociável)

- Toda atividade do original fica, numa caixa `::: {.atividade}`, com os
  passos explícitos.
- **Kahoot**: mantenha o link `https://create.kahoot.it/details/<id>`.
  Pegue os ids no perfil público do Diogo e registre o mapa no
  `CLAUDE.md`.
- **Slido**: mantenha a pergunta, o link do evento e uma alternativa sem
  Slido.
- Onde não houver Kahoot nem Slido, crie **"Pergunta para a turma"**
  (*peer instruction*):
  - quatro alternativas;
  - "votem sozinhos, discutam com quem está ao lado e votem de novo";
  - a resposta justificada em `::: {.fragment}` com `.definicao`.
- Aula pesada demais para o tempo: a aula fica enxuta e o material formal
  vai para uma seção final `# Para saber mais {.secao}`. **Combine
  antes.**

### 3.6 Referências (regra rígida)

- Nunca invente referências, DOIs, URLs, números de figura ou páginas.
- Use só o que está no original, na página da aula ou o que você
  **verificou**:
  - artigos: no Crossref, conferindo autores, ano, periódico, volume e
    páginas (cite só o que ele confirmar);
  - livros: no Open Library.
- Se não der para verificar, deixe `<!-- TODO: referência -->` e avise.

### 3.7 Renderizar e conferir

```bash
quarto render slides/aNN.qmd
```

- `freeze: auto` está ligado e `_freeze/` **é versionado**: o CI roda só
  o Quarto, sem R. Renderize localmente e commite `_freeze/` junto.
- Se mudou só uma função em `R/`, renderize o `.qmd` explicitamente.
- Se acrescentou ou removeu um chunk, apague `_freeze/slides/aNN` antes,
  porque as figuras são renumeradas.
- **Teste de transbordamento** no navegador embutido:
  1. sirva `docs/` (`.claude/launch.json`, porta 4321);
  2. ponha o viewport em **1250×750** (num viewport em retrato o teste
     não vale);
  3. rode o código abaixo;
  4. volte o viewport para `desktop` no fim.

  ```js
  const out = [];
  for (const s of Reveal.getSlides()) {
    Reveal.slide(...Object.values(Reveal.getIndices(s)));
    await new Promise(r => setTimeout(r, 120));
    s.querySelectorAll('.fragment').forEach(f => f.classList.add('visible'));
    let b = 0;
    s.querySelectorAll('*').forEach(e => {
      const r = e.getBoundingClientRect(); if (r.height > 0) b = Math.max(b, r.bottom);
    });
    if (b > 748) out.push((s.querySelector('h2')?.textContent ?? '?') + ': ' + Math.round(b));
  }
  out
  ```

- Corrija os transbordamentos reduzindo `fig-height`, dimensionando
  imagens por `height`, usando `.smaller` ou dividindo o slide.
- Confira as figuras novas em PNG: salve com `ggsave` em `$TMPDIR` e leia
  a imagem. Procure rótulos sobrepostos e contraste baixo; o Diogo pediu,
  por exemplo, pontos com contorno branco.

### 3.8 Página da aula e fechamento do lote

- Em `aula-NN-*/index.qmd`, troque o botão do PDF por "Abrir slides"
  (`../slides/aNN.html`) e escreva um resumo com a etapa do ciclo e uma
  seção `## Conteúdo`.
- Não apague os PDFs antigos: eles continuam em `resources`, para que
  links antigos sigam funcionando.
- Mostre o resultado e espere a aprovação. Depois:
  1. `git log origin/main..HEAD`, para não empurrar commits de outra
     sessão;
  2. commit e push;
  3. `gh run watch` até o deploy terminar;
  4. `curl` na página publicada, para confirmar.
- Atualize a tabela de situação no `CLAUDE.md` e a memória (o que foi
  corrigido no original, o que foi retirado e por quê).

## 4. Extras que valem replicar

- **Identidade visual do ciclo**: uma cor fixa por etapa, a mesma no
  diagrama (`R/tema.R`) e nos selos dos slides (`slides/theme.scss`). Um
  selo de etapa aparece no canto de cada slide (`slides/_selo-etapa.html`).
- **"Brinque com o código" com webR** na página de cada aula:
  - formato `live-html` com `engine: knitr` e o include de
    `_extensions/r-wasm/live/_knitr.qmd`;
  - os pacotes vão em `webr: packages`, e os arquivos de `R/` em
    `webr: resources` e em `resources`;
  - repita as opções de `html` no YAML da página, porque o `live-html`
    não as herda do `_quarto.yml`;
  - a célula oculta de setup faz `source(f, local = TRUE)` dos arquivos.
- **Teste-se**: `quiz/aNN.json` (10 perguntas com 4 alternativas,
  `correta` de 1 a 4 e explicação), renderizado por `_teste-se.html`.
- **Fio condutor** entre aulas: os mesmos dados da turma voltam em várias
  aulas, como as idades da Aula 1 em `R/idades.R`.

## 5. Armadilhas já encontradas

- Subscritos Unicode (H₀) não aparecem nas figuras do R. Nas figuras,
  escreva H0; no texto do slide, H₀ funciona.
- Em `sprintf()`, a vírgula decimal não sai sozinha: use
  `format(..., decimal.mark = ",")`.
- Dentro de `mutate()` ou `tibble()`, um argumento com nome de coluna (x,
  y, n) é sombreado. Renomeie o argumento.
- `ape::plot.phylo(plot = FALSE)` abre um dispositivo gráfico e gera uma
  imagem extra em branco. Envolva em `pdf(NULL)`/`dev.off()`.
- Temas do Mermaid são ignorados no revealjs: desenhe os diagramas em R.
- Um render interrompido deixa `slides/aNN.rmarkdown` para trás. Apague o
  arquivo antes de commitar.
- Uma página `live-html` sem `engine: knitr` cai no Jupyter e falha com o
  erro de `yaml`.
- A API do Wikimedia Commons limita a frequência: espace as chamadas em
  ~5 s. O Python local não tem certificados SSL, então use `curl`.
- O site da Wiley bloqueia `curl` e o navegador com uma verificação
  anti-robô. Não tente contornar: peça ao Diogo para baixar a figura (ele
  costuma deixar em `~/Downloads`).
- Pastas do Box ou do OneDrive podem travar a leitura de arquivos que
  ainda não foram baixados. Use timeout ou rode em segundo plano.
- Fotos de bancos com restrição de uso (como o FACES, do Max Planck) só
  podem ser projetadas em sala, nunca publicadas.

## 6. Adaptar para Métodos multidimensionais

O que já se sabe e o que é preciso perguntar antes de começar:

- **Repositório:** `~/GitHub/multidimensionais` já tem o site e 7 aulas.
  Pastas `aula-01-intro-multivariada` a `aula-07-metodos-baseados-modelos`
  e PDFs em `slides/aula-0N.pdf`.
- **É pós-graduação:** o tom pode ser mais formal e os exemplos, de
  pesquisa.
- **Perguntar:** onde estão os `.pptx` mais recentes?
- **Perguntar:** o ciclo PPDAC também organiza esta disciplina, ou há
  outro fio condutor? O selo de etapa só faz sentido se houver um.
- **Perguntar:** dados simulados ou conjuntos reais? Os do `vegan` (como
  `dune` e `varespec`), os do livro *Análises ecológicas no R* ou os dados
  do próprio Diogo. Confirme também a entrada de `vegan` e outros
  pacotes.
- **Perguntar:** quais Kahoots, Slidos e atividades existem por aula?
- **Perguntar:** qual aula será o modelo? A sugestão é uma aula
  representativa, como ordenações irrestritas.
- **Referências de formato:**
  - este repositório;
  - Análise de Dados Univariados (`~/GitHub/modelos-lineares-2026-2`),
    de onde vieram o tema e o diagrama do ciclo;
  - Introdução ao R (`~/GitHub/intro-r`).

### Prompt sugerido para a nova sessão

> Leia `~/GitHub/bioestatistica/WORKFLOW-conversao-qmd.md` e o
> `CLAUDE.md` desse repositório. Quero converter as aulas de Métodos
> multidimensionais (`~/GitHub/multidimensionais`) para Quarto/revealjs
> seguindo o mesmo workflow. Antes de converter qualquer aula, me faça as
> perguntas da seção 6, monte o `CLAUDE.md` do repositório e proponha a
> aula-modelo.
