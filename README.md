# Bioestatística

Materiais da disciplina obrigatória de **Bioestatística** (68 h) do curso de
Ciências Biológicas do Instituto de Biociências da Universidade Federal de
Mato Grosso do Sul.

**Site publicado: <https://provetelab.org/bioestatistica/>**

## O que há aqui

Onze aulas, cada uma em sua própria pasta com a página correspondente
(`index.qmd`). Os slides em PDF ficam em `slides/`.

| Aula | Tópico |
|-----:|--------|
| 1 | Introdução à estatística biológica |
| 2 | Princípios de amostragem e delineamento experimental |
| 3 | Organização e apresentação de dados |
| 4 | Medidas de tendência central e dispersão |
| 5 | Princípios de probabilidade e distribuições |
| 6 | Estimativa pontual e intervalar |
| 7 | Teste de hipóteses |
| 8 | Modelos lineares: teste t |
| 9 | Modelos lineares: regressão e correlação |
| 10 | Modelos lineares generalizados: qui-quadrado |
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

Estas páginas não contêm blocos de código executável, então o render precisa
apenas do Quarto — nem R é necessário.

## Origem

Estes materiais viviam em `teaching/bioestatistica/` no repositório do site
do laboratório ([diogoprov.github.io](https://github.com/diogoprov/diogoprov.github.io)),
de onde foram extraídos com o histórico preservado. O histórico anterior à
migração continua disponível lá.

---

[Biodiversity Synthesis Lab](https://provetelab.org/) · Diogo B. Provete ·
Instituto de Biociências, UFMS
