// Construtor de modelos lineares (construa-um-modelo.qmd).
// Arrastar (ou clicar) variáveis monta a fórmula do lm(); a página mostra o
// nome do teste, a equação ajustada por mínimos quadrados, um gráfico e o
// código em R. Tudo em JS puro: o R de verdade fica para o código copiado.
(function () {
  "use strict";

  const CONJUNTOS = {
    girinos_2x2: {
      rotulo: "Girinos: predador × alimento (dados fictícios)",
      arquivo: "data/girinos_2x2.csv",
      ignorar: ["individuo"],
      descricao: "Massa na metamorfose (mg) de 32 girinos criados com pista química de predador ausente ou presente e com alimento alto ou baixo.",
    },
    girinos_lagoas: {
      rotulo: "Girinos de três lagoas (dados simulados)",
      arquivo: "data/girinos_lagoas.csv",
      ignorar: ["individuo"],
      descricao: "Massa na metamorfose (mg) de 45 girinos de lagoas temporárias, permanentes e açudes, criados em temperaturas de 20 a 32 °C.",
    },
    girinos_mesocosmos: {
      rotulo: "Girinos em mesocosmos: três preditores numéricos (dados simulados)",
      arquivo: "data/girinos_mesocosmos.csv",
      ignorar: ["mesocosmo"],
      descricao: "Massa na metamorfose (mg) em 60 mesocosmos com cobertura do dossel (%), temperatura da água (°C) e densidade de girinos (por litro). Bom para a regressão múltipla: compare massa_mg ~ dossel_pct com massa_mg ~ dossel_pct + temperatura_c.",
    },
    galton: {
      rotulo: "Galton (1886): altura de pais e filhos (dados reais)",
      arquivo: "data/galton.csv",
      ignorar: [],
      descricao: "Altura média dos pais (parent) e altura do filho adulto (child), em polegadas, de 928 filhos.",
    },
  };
  const SITE = "https://provetelab.org/bioestatistica/";
  const PALETA = ["#1c6e8c", "#a15e3f", "#527238", "#96661f", "#6b4c8a", "#8a8172"];
  const LIMITE_PESCARIA = 5;

  const raiz = document.getElementById("construtor");
  if (!raiz) return;
  // anúncio para leitores de tela: fica fora do que desenha() recria
  const avisoVivo = document.createElement("div");
  avisoVivo.className = "visually-hidden";
  avisoVivo.setAttribute("aria-live", "polite");
  raiz.after(avisoVivo);

  const estado = {
    chave: "girinos_2x2",
    linhas: [],
    vars: [],      // {nome, tipo: "num" | "fator", niveis}
    y: null,
    xs: [],
    op: "+",
    tentados: new Set(),
  };

  // ---------- utilidades ----------
  const el = (tag, attrs = {}, ...filhos) => {
    const e = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs)) {
      if (k === "class") e.className = v;
      else if (k.startsWith("on")) e.addEventListener(k.slice(2), v);
      else if (v !== null && v !== undefined && v !== false) e.setAttribute(k, v === true ? "" : v);
    }
    for (const f of filhos.flat()) if (f !== null && f !== undefined) e.append(f.nodeType ? f : document.createTextNode(f));
    return e;
  };
  const num = (v, d) => {
    const a = Math.abs(v);
    if (d === undefined && a !== 0 && a < 0.01) return v.toLocaleString("pt-BR", { maximumSignificantDigits: 3 });
    const casas = d ?? (a !== 0 && a < 1 ? 3 : 2);
    return v.toLocaleString("pt-BR", { minimumFractionDigits: casas, maximumFractionDigits: casas });
  };
  // mesma ordem dos níveis que o factor() do R em pt_BR/en_US (Açude antes de Permanente)
  const ordena = (a) => [...a].sort((p, q) => p.localeCompare(q, "pt-BR"));

  function lerCSV(texto) {
    const linhas = texto.trim().split(/\r?\n/);
    const campos = (l) => {
      const out = []; let cur = "", aspas = false;
      for (const c of l) {
        if (c === '"') aspas = !aspas;
        else if (c === "," && !aspas) { out.push(cur); cur = ""; }
        else cur += c;
      }
      out.push(cur);
      return out;
    };
    const cab = campos(linhas[0]);
    return linhas.slice(1).map((l) => Object.fromEntries(campos(l).map((v, i) => [cab[i], v])));
  }

  function descreveVars(linhas, ignorar) {
    return Object.keys(linhas[0]).filter((n) => !ignorar.includes(n)).map((nome) => {
      const valores = linhas.map((r) => r[nome]);
      const numerica = valores.every((v) => v !== "" && !isNaN(Number(v)));
      if (numerica) {
        linhas.forEach((r) => (r[nome] = Number(r[nome])));
        return { nome, tipo: "num" };
      }
      return { nome, tipo: "fator", niveis: ordena(new Set(valores)) };
    });
  }
  const varPorNome = (n) => estado.vars.find((v) => v.nome === n);

  // ---------- matriz do modelo (contrastes de tratamento, como o lm) ----------
  function colunasDoTermo(nomes) {
    // cada termo é um produto de variáveis; fatores viram indicadoras dos níveis 2..k
    let cols = [{ nome: "", partes: [], f: () => 1 }];
    for (const n of nomes) {
      const v = varPorNome(n);
      const novas = [];
      const pecas = v.tipo === "num"
        ? [{ rotulo: n, parte: { tipo: "num", nome: n }, f: (r) => r[n] }]
        : v.niveis.slice(1).map((lv) => ({ rotulo: n + lv, parte: { tipo: "fator", nome: n, nivel: lv }, f: (r) => (r[n] === lv ? 1 : 0) }));
      for (const c of cols) for (const p of pecas) {
        novas.push({ nome: c.nome ? c.nome + ":" + p.rotulo : p.rotulo, partes: [...c.partes, p.parte], f: (r) => c.f(r) * p.f(r) });
      }
      cols = novas;
    }
    return cols;
  }
  function termos() {
    const xs = estado.xs;
    const t = xs.map((x) => [x]);
    if (estado.op === "*" && xs.length > 1) {
      const comb = (inicio, atual) => {
        for (let i = inicio; i < xs.length; i++) {
          const novo = [...atual, xs[i]];
          if (novo.length > 1) t.push(novo);
          comb(i + 1, novo);
        }
      };
      comb(0, []);
      t.sort((a, b) => a.length - b.length);
    }
    return t;
  }
  function ajusta() {
    const cols = [{ nome: "(Intercept)", partes: [], f: () => 1 }, ...termos().flatMap(colunasDoTermo)];
    const X = estado.linhas.map((r) => cols.map((c) => c.f(r)));
    const y = estado.linhas.map((r) => r[estado.y]);
    const p = cols.length;
    // equações normais X'X b = X'y, eliminação de Gauss com pivoteamento
    const A = Array.from({ length: p }, (_, i) => {
      const linha = new Array(p + 1).fill(0);
      for (let k = 0; k < X.length; k++) {
        for (let j = 0; j < p; j++) linha[j] += X[k][i] * X[k][j];
        linha[p] += X[k][i] * y[k];
      }
      return linha;
    });
    const tol = 1e-10 * Math.max(...A.map((l, i) => Math.abs(l[i])));
    for (let c = 0; c < p; c++) {
      let piv = c;
      for (let r = c + 1; r < p; r++) if (Math.abs(A[r][c]) > Math.abs(A[piv][c])) piv = r;
      if (Math.abs(A[piv][c]) <= tol) return { erro: "Algum coeficiente não pode ser estimado: há preditores redundantes (um é combinação dos outros) ou combinações de grupos sem nenhuma observação." };
      [A[c], A[piv]] = [A[piv], A[c]];
      for (let r = 0; r < p; r++) {
        if (r === c) continue;
        const fator = A[r][c] / A[c][c];
        for (let j = c; j <= p; j++) A[r][j] -= fator * A[c][j];
      }
    }
    const b = A.map((linha, i) => linha[p] / linha[i]);
    const prediz = (r) => cols.reduce((s, c, j) => s + b[j] * c.f(r), 0);
    const ajustados = estado.linhas.map(prediz);
    const media = y.reduce((s, v) => s + v, 0) / y.length;
    const sqt = y.reduce((s, v) => s + (v - media) ** 2, 0);
    const sqr = y.reduce((s, v, i) => s + (v - ajustados[i]) ** 2, 0);
    return { cols, b, prediz, r2: sqt > 0 ? 1 - sqr / sqt : 0, n: y.length };
  }

  // ---------- que teste é este? ----------
  function nomeDoTeste() {
    const xs = estado.xs.map(varPorNome);
    const nNum = xs.filter((v) => v.tipo === "num").length;
    const fatores = xs.filter((v) => v.tipo === "fator");
    const inter = estado.op === "*" && xs.length > 1;
    const aula = (n, nome) => ({ href: `aula-${n}/index.qmd`.replace("index.qmd", "index.html"), nome });
    const A8 = aula("08-modelos-lineares-teste-t", "Aula 8");
    const A9 = aula("09-regressao-correlacao", "Aula 9");
    const A11 = aula("11-anova", "Aula 11");
    const A4 = aula("04-tendencia-dispersao", "Aula 4");
    if (xs.length === 0)
      return { nome: "Só o intercepto: a média", texto: "Sem preditores, o modelo estima um único número para todos: a média de y. Cuidado: o valor-p do summary() testa se essa média é zero, o que raramente faz sentido (nenhum girino pesa 0 mg). Para comparar a média com um valor de referência, use o teste t de uma amostra, t.test(y, mu = valor).", aulas: [A4, A8] };
    if (xs.length === 1 && nNum === 1)
      return { nome: "Regressão linear simples", texto: "Um preditor numérico: β é a inclinação da reta. O valor-p de β é o mesmo do teste da correlação de Pearson entre as duas variáveis.", aulas: [A9] };
    if (xs.length === 1 && fatores[0].niveis.length === 2)
      return { nome: "Teste t para duas amostras independentes", texto: "Um fator com dois grupos vira uma variável indicadora (0 ou 1): β é a diferença entre as duas médias. É o teste t que supõe variâncias iguais nos dois grupos; se elas forem muito diferentes, use a correção de Welch.", aulas: [A8] };
    if (xs.length === 1)
      return { nome: `ANOVA de um fator (${fatores[0].niveis.length} grupos)`, texto: `Um fator com ${fatores[0].niveis.length} grupos vira ${fatores[0].niveis.length - 1} indicadoras: cada β compara um grupo com o de referência. O teste F da ANOVA pergunta se todos esses β são zero ao mesmo tempo.`, aulas: [A11] };
    if (xs.length === 2 && fatores.length === 2)
      return inter
        ? { nome: "ANOVA de dois fatores, com interação", texto: "O termo de interação permite que o efeito de um fator dependa do nível do outro: as linhas do gráfico de interação podem não ser paralelas.", aulas: [A11] }
        : { nome: "ANOVA de dois fatores, sem interação", texto: "Os efeitos dos dois fatores se somam: o efeito de um é o mesmo em qualquer nível do outro. Troque + por * para testar se isso é verdade.", aulas: [A11] };
    if (xs.length === 2 && nNum === 1)
      return inter
        ? { nome: "ANCOVA com inclinações diferentes", texto: "Com a interação, cada grupo ganha a sua própria reta, com intercepto e inclinação próprios. Compare com o modelo sem interação: as retas eram mesmo paralelas?", aulas: [A11, A9] }
        : { nome: "ANCOVA", texto: "Retas paralelas: a mesma inclinação para todos os grupos, com interceptos diferentes. A diferença entre grupos é medida com a covariável fixa.", aulas: [A11, A9] };
    if (nNum === xs.length)
      return inter
        ? { nome: "Regressão múltipla com interação", texto: "Com interação, a inclinação de cada preditor muda conforme o valor dos outros. Por isso, o β de um preditor sozinho vale só quando os outros são zero, que muitas vezes está fora dos dados. Compare com o modelo sem interação (+).", aulas: [A9] }
        : { nome: "Regressão múltipla", texto: "Mais de um preditor numérico: cada β é a mudança em y por unidade daquele preditor, mantidos os outros fixos.", aulas: [A9] };
    return inter
      ? { nome: `Modelo linear com ${xs.length} preditores e interação`, texto: "Esse modelo vai além do que vemos na disciplina. Com interação, o efeito de cada variável depende das outras, e os β das variáveis sozinhas valem só no grupo de referência ou com as numéricas em zero.", aulas: [A11] }
      : { nome: `Modelo linear com ${xs.length} preditores`, texto: "Esse modelo vai além do que vemos na disciplina, mas a lógica é a mesma: cada β é uma inclinação ou uma diferença em relação ao grupo de referência, com os outros preditores fixos.", aulas: [A11] };
  }

  // ---------- leitura dos coeficientes ----------
  function referencia() {
    return estado.xs.map(varPorNome).filter((v) => v.tipo === "fator").map((v) => `${v.nome} = ${v.niveis[0]}`);
  }
  // numéricas do modelo cujo zero fica fora dos dados observados
  function foraDosDados() {
    return estado.xs.map(varPorNome).filter((v) => v.tipo === "num").map((v) => v.nome).filter((n) => {
      const vals = estado.linhas.map((r) => r[n]);
      return Math.min(...vals) > 0 || Math.max(...vals) < 0;
    });
  }
  function leCoeficiente(c, j, b) {
    const valor = num(b[j]);
    if (c.nome === "(Intercept)") {
      const ref = referencia();
      const nums = estado.xs.map(varPorNome).filter((v) => v.tipo === "num");
      let txt = estado.xs.length === 0 ? `média de ${estado.y}` : `valor previsto de ${estado.y}`;
      const cond = [...ref, ...nums.map((v) => `${v.nome} = 0`)];
      if (cond.length) txt += ` quando ${cond.join(" e ")}`;
      const fora = foraDosDados();
      const alerta = fora.length ? `${fora.join(" = 0 e ")} = 0 ${fora.length > 1 ? "estão" : "está"} fora dos dados: o intercepto é uma extrapolação, sem sentido biológico.` : null;
      return { termo: "Intercepto (α)", valor, txt, alerta };
    }
    // com interação, um termo vale só quando as variáveis que não estão nele ficam na referência (fator) ou em 0 (numérica)
    const nomesTermo = c.partes.map((p) => p.nome);
    const condicoes = estado.xs.filter((n) => !nomesTermo.includes(n)).map((n) => {
      const v = varPorNome(n);
      return v.tipo === "num" ? `${n} = 0` : `${n} = ${v.niveis[0]}`;
    });
    const foraAqui = foraDosDados().filter((n) => !nomesTermo.includes(n));
    const outros = estado.op === "*" && condicoes.length
      ? ` (vale quando ${condicoes.join(" e ")}${foraAqui.length ? `; ${foraAqui.join(" = 0 e ")} = 0 fica fora dos dados, então este número é uma extrapolação` : ""})` : "";
    if (c.partes.length === 1) {
      const p = c.partes[0];
      if (p.tipo === "num") return { termo: c.nome, valor, txt: `quanto ${estado.y} muda a cada 1 unidade a mais de ${p.nome}${estado.xs.length > 1 && !outros ? ", mantidas as outras variáveis fixas" : ""}${outros}` };
      const ref = varPorNome(p.nome).niveis[0];
      return { termo: c.nome, valor, txt: `diferença entre ${p.nome} = ${p.nivel} e ${p.nome} = ${ref}${outros}` };
    }
    const numericas = c.partes.filter((p) => p.tipo === "num").map((p) => p.nome);
    const grupos = c.partes.filter((p) => p.tipo === "fator").map((p) => `${p.nome} = ${p.nivel}`);
    if (c.partes.length > 2) {
      const ult = c.partes[c.partes.length - 1], antes = c.partes.slice(0, -1).map((p) => p.nome);
      const comQue = ult.tipo === "num" ? `a cada unidade a mais de ${ult.nome}` : `em ${ult.nome} = ${ult.nivel}`;
      return { termo: c.nome, valor, txt: `interação de ${c.partes.length} variáveis: quanto a interação entre ${antes.join(" e ")} muda ${comQue}${outros}` };
    }
    if (numericas.length === 1 && grupos.length)
      return { termo: c.nome, valor, txt: `interação: quanto a inclinação de ${numericas[0]} muda em ${grupos.join(" e ")}, em relação ao grupo de referência${outros}` };
    if (!numericas.length)
      return { termo: c.nome, valor, txt: `interação: ajuste extra para quem é ${grupos.join(" e ")}, além da soma dos dois efeitos; mede o quanto o efeito de um fator muda no nível do outro${outros}` };
    return { termo: c.nome, valor, txt: `interação: quanto a inclinação de ${numericas[0]} muda a cada unidade a mais de ${numericas[1]}${outros}` };
  }

  // ---------- regressão múltipla: efeito parcial e colinearidade ----------
  const media = (a) => a.reduce((s, v) => s + v, 0) / a.length;
  const dp = (a) => { const m = media(a); return Math.sqrt(a.reduce((s, v) => s + (v - m) ** 2, 0) / (a.length - 1)); };
  const correl = (a, b) => { const ma = media(a), mb = media(b); let sab = 0, saa = 0, sbb = 0; a.forEach((v, i) => { sab += (v - ma) * (b[i] - mb); saa += (v - ma) ** 2; sbb += (b[i] - mb) ** 2; }); return sab / Math.sqrt(saa * sbb); };
  // mínimos quadrados genérico (colunas de X já com o intercepto); devolve os coeficientes ou null
  function mmq(X, y) {
    const p = X[0].length;
    const A = Array.from({ length: p }, (_, i) => {
      const l = new Array(p + 1).fill(0);
      X.forEach((xr, k) => { for (let j = 0; j < p; j++) l[j] += xr[i] * xr[j]; l[p] += xr[i] * y[k]; });
      return l;
    });
    for (let c = 0; c < p; c++) {
      let piv = c;
      for (let r = c + 1; r < p; r++) if (Math.abs(A[r][c]) > Math.abs(A[piv][c])) piv = r;
      if (Math.abs(A[piv][c]) < 1e-12) return null;
      [A[c], A[piv]] = [A[piv], A[c]];
      for (let r = 0; r < p; r++) if (r !== c) { const f = A[r][c] / A[c][c]; for (let j = c; j <= p; j++) A[r][j] -= f * A[c][j]; }
    }
    return A.map((l, i) => l[p] / l[i]);
  }
  const numericos = () => estado.xs.map(varPorNome).filter((v) => v.tipo === "num");
  // painéis de efeito parcial só quando há 2+ numéricas e nenhuma interação
  const usaParcial = () => numericos().length >= 2 && estado.op === "+";

  function colinearidade() {
    const nums = numericos();
    if (nums.length < 2) return null;
    const col = (n) => estado.linhas.map((r) => r[n]);
    return nums.map((v) => {
      const outros = nums.filter((o) => o !== v);
      const b = mmq(estado.linhas.map((r) => [1, ...outros.map((o) => r[o.nome])]), col(v.nome));
      let vif = Infinity;
      if (b) {
        const yv = col(v.nome), m = media(yv);
        const prev = estado.linhas.map((r) => b[0] + outros.reduce((s, o, j) => s + b[j + 1] * r[o.nome], 0));
        const r2 = 1 - yv.reduce((s, val, i) => s + (val - prev[i]) ** 2, 0) / yv.reduce((s, val) => s + (val - m) ** 2, 0);
        vif = 1 / (1 - r2);
      }
      const rs = outros.map((o) => ({ com: o.nome, r: correl(col(v.nome), col(o.nome)) }));
      const maior = rs.reduce((a, c) => (Math.abs(c.r) > Math.abs(a.r) ? c : a));
      return { nome: v.nome, vif, maior };
    });
  }

  function painelColinearidade() {
    const c = colinearidade();
    if (!c) return null;
    const problema = c.filter((l) => l.vif > 5 || Math.abs(l.maior.r) > 0.7);
    const tabela = el("table", { class: "table table-sm cm-efeitos" },
      el("thead", {}, el("tr", {}, ...["Preditor", "Correlação mais forte", "VIF"].map((h) => el("th", {}, h)))),
      el("tbody", {}, ...c.map((l) => el("tr", { class: l.vif > 5 ? "cm-ruim" : "" },
        el("td", {}, el("code", {}, l.nome)), el("td", {}, `r = ${num(l.maior.r)} com ${l.maior.com}`), el("td", {}, isFinite(l.vif) ? num(l.vif) : "∞")))));
    return el("div", { class: "cm-colin" },
      el("div", { class: "cm-sub" }, "Os preditores estão correlacionados entre si?"),
      el("p", { class: "cm-nota" }, "O VIF (fator de inflação da variância) diz quantas vezes a variância de um coeficiente aumenta porque aquele preditor pode ser previsto pelos outros. VIF = 1: nenhuma sobreposição; acima de 5, cuidado."),
      tabela,
      problema.length ? el("div", { class: "cm-alerta cm-pequeno" },
        el("strong", {}, "Colinearidade. "),
        `${problema.map((l) => l.nome).join(" e ")} carregam quase a mesma informação. O modelo tem dificuldade para separar o efeito de cada um: os erros padrão incham e os coeficientes podem mudar muito ao tirar ou pôr um preditor. Compare o modelo com e sem cada um deles.`) : null);
  }

  function graficoParcial(fit) {
    const ns = "http://www.w3.org/2000/svg";
    const L = estado.linhas, y = L.map((r) => r[estado.y]);
    const ybar = media(y);
    const res = L.map((r, i) => y[i] - fit.prediz(r));
    const paineis = numericos().map((v) => {
      const x = L.map((r) => r[v.nome]), xbar = media(x);
      const bj = fit.b[fit.cols.findIndex((c) => c.nome === v.nome)];
      const bs = correl(x, y) * dp(y) / dp(x); // inclinação da regressão simples, só com este preditor
      const yp = x.map((xi, i) => ybar + res[i] + bj * (xi - xbar)); // resíduo parcial, centrado na média de y
      const W = 320, H = 250, m = { l: 48, r: 10, t: 12, b: 40 };
      const svg = document.createElementNS(ns, "svg");
      svg.setAttribute("viewBox", `0 0 ${W} ${H}`); svg.setAttribute("class", "cm-svg"); svg.setAttribute("role", "img");
      svg.setAttribute("aria-label", `Efeito parcial de ${v.nome}: inclinação ${num(bj)} com os outros preditores fixos, contra ${num(bs)} sozinho.`);
      const s = (tag, a, t) => { const e = document.createElementNS(ns, tag); for (const [k, val] of Object.entries(a)) e.setAttribute(k, val); if (t !== undefined) e.textContent = t; svg.appendChild(e); return e; };
      const ext = (a) => { const lo = Math.min(...a), hi = Math.max(...a), p = (hi - lo) * 0.06 || 1; return [lo - p, hi + p]; };
      const [x0, x1] = ext(x);
      const [y0, y1] = ext([...yp, ...y]);
      const X = (val) => m.l + (W - m.l - m.r) * (val - x0) / (x1 - x0);
      const Y = (val) => m.t + (H - m.t - m.b) * (1 - (val - y0) / (y1 - y0));
      const marcas = (a, b) => { const passo = Math.pow(10, Math.floor(Math.log10((b - a) / 5))); const k = [1, 2, 2.5, 5, 10].find((f) => (b - a) / (f * passo) <= 5) * passo; const o = []; for (let val = Math.ceil(a / k) * k; val <= b; val += k) o.push(+val.toFixed(10)); return o; };
      s("line", { x1: m.l, x2: m.l, y1: m.t, y2: H - m.b, class: "cm-eixo" });
      s("line", { x1: m.l, x2: W - m.r, y1: H - m.b, y2: H - m.b, class: "cm-eixo" });
      marcas(y0, y1).forEach((t) => s("text", { x: m.l - 5, y: Y(t) + 4, "text-anchor": "end", class: "cm-tick" }, t.toLocaleString("pt-BR")));
      marcas(x0, x1).forEach((t) => s("text", { x: X(t), y: H - m.b + 15, "text-anchor": "middle", class: "cm-tick" }, t.toLocaleString("pt-BR")));
      yp.forEach((val, i) => s("circle", { cx: X(x[i]), cy: Y(val), r: 3.2, fill: PALETA[0], class: "cm-ponto" }));
      const linha = (b, cls, cor) => s("line", { x1: X(x0), x2: X(x1), y1: Y(ybar + b * (x0 - xbar)), y2: Y(ybar + b * (x1 - xbar)), stroke: cor, class: cls });
      linha(bs, "cm-simples", "#8a8172");
      linha(bj, "cm-ajuste", PALETA[1]);
      s("text", { x: (W + m.l) / 2, y: H - 5, "text-anchor": "middle", class: "cm-rotulo" }, v.nome);
      s("text", { x: 11, y: (H - m.b + m.t) / 2, transform: `rotate(-90 11 ${(H - m.b + m.t) / 2})`, "text-anchor": "middle", class: "cm-rotulo" }, `${estado.y} sem os outros`);
      return el("figure", {}, svg, el("figcaption", {}, `Parcial: ${num(bj)} por unidade · sozinho: ${num(bs)}`));
    });
    return el("div", { class: "cm-figura" },
      el("div", { class: "cm-legenda-parcial" },
        el("span", { class: "cm-traco cm-traco-parcial" }), " inclinação com os outros preditores fixos (o coeficiente do modelo)   ",
        el("span", { class: "cm-traco cm-traco-simples" }), " inclinação sozinho (regressão simples nos dados brutos, desenhada pelo mesmo centro só para comparar)"),
      el("div", { class: "cm-residuos" }, ...paineis),
      el("p", { class: "cm-nota" }, `Cada painel mostra ${estado.y} depois de descontar o efeito dos outros preditores (resíduos parciais). Quando as duas linhas discordam, parte do efeito "sozinho" era, na verdade, de outro preditor correlacionado com ele.`));
  }

  // ---------- gráfico (SVG) ----------
  function grafico(fit) {
    if (fit && usaParcial()) return graficoParcial(fit);
    // no celular, um viewBox mais estreito mantém o texto legível
    const larg = raiz.clientWidth || 640;
    const W = larg < 700 ? Math.max(340, larg - 40) : 640, H = larg < 700 ? 300 : 340, m = { l: 62, r: 16, t: 14, b: 48 };
    const svg = el("svg", { viewBox: `0 0 ${W} ${H}`, class: "cm-svg", role: "img" });
    const ns = "http://www.w3.org/2000/svg";
    const s = (tag, attrs, texto) => {
      const e = document.createElementNS(ns, tag);
      for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v);
      if (texto !== undefined) e.textContent = texto;
      svg.appendChild(e);
      return e;
    };
    const svgReal = document.createElementNS(ns, "svg");
    svgReal.setAttribute("viewBox", `0 0 ${W} ${H}`);
    svgReal.setAttribute("class", "cm-svg");
    svgReal.setAttribute("role", "img");

    const xs = estado.xs.map(varPorNome);
    const xv = xs.find((v) => v.tipo === "num") || xs[0] || null;
    const cor = xs.find((v) => v.tipo === "fator" && v !== xv) || null;
    const extras = xs.filter((v) => v !== xv && v !== cor);
    const L = estado.linhas;
    const yv = L.map((r) => r[estado.y]);
    let ymin = Math.min(...yv), ymax = Math.max(...yv);
    const pad = (ymax - ymin) * 0.06 || 1; ymin -= pad; ymax += pad;
    const Y = (v) => m.t + (H - m.t - m.b) * (1 - (v - ymin) / (ymax - ymin));
    const corDe = (r) => (cor ? PALETA[cor.niveis.indexOf(r[cor.nome]) % PALETA.length] : PALETA[0]);
    const jit = (i) => (((i * 9301 + 49297) % 233280) / 233280 - 0.5);

    // eixos
    const ticks = (a, b, n = 5) => { const passo = Math.pow(10, Math.floor(Math.log10((b - a) / n))); const k = [1, 2, 2.5, 5, 10].find((f) => (b - a) / (f * passo) <= n) * passo; const out = []; for (let v = Math.ceil(a / k) * k; v <= b; v += k) out.push(+v.toFixed(10)); return out; };
    s("line", { x1: m.l, x2: m.l, y1: m.t, y2: H - m.b, class: "cm-eixo" });
    s("line", { x1: m.l, x2: W - m.r, y1: H - m.b, y2: H - m.b, class: "cm-eixo" });
    for (const t of ticks(ymin, ymax)) { s("text", { x: m.l - 6, y: Y(t) + 4, "text-anchor": "end", class: "cm-tick" }, t.toLocaleString("pt-BR")); s("line", { x1: m.l, x2: W - m.r, y1: Y(t), y2: Y(t), class: "cm-grade" }); }
    s("text", { x: 14, y: (H - m.b + m.t) / 2, transform: `rotate(-90 14 ${(H - m.b + m.t) / 2})`, "text-anchor": "middle", class: "cm-rotulo" }, estado.y);

    if (xv && xv.tipo === "num") {
      const xvals = L.map((r) => r[xv.nome]);
      let xmin = Math.min(...xvals), xmax = Math.max(...xvals);
      const px = (xmax - xmin) * 0.04 || 1; xmin -= px; xmax += px;
      const X = (v) => m.l + (W - m.l - m.r) * (v - xmin) / (xmax - xmin);
      for (const t of ticks(xmin, xmax)) s("text", { x: X(t), y: H - m.b + 18, "text-anchor": "middle", class: "cm-tick" }, t.toLocaleString("pt-BR"));
      s("text", { x: (W + m.l) / 2, y: H - 8, "text-anchor": "middle", class: "cm-rotulo" }, xv.nome);
      L.forEach((r) => s("circle", { cx: X(r[xv.nome]), cy: Y(r[estado.y]), r: L.length > 300 ? 2.2 : 4, fill: corDe(r), class: "cm-ponto" }));
      if (fit && !extras.length) {
        const grupos = cor ? cor.niveis : [null];
        grupos.forEach((g, i) => {
          const sub = cor ? L.filter((r) => r[cor.nome] === g) : L;
          const a = Math.min(...sub.map((r) => r[xv.nome])), b = Math.max(...sub.map((r) => r[xv.nome]));
          const base = { ...sub[0] };
          const p1 = fit.prediz({ ...base, [xv.nome]: a }), p2 = fit.prediz({ ...base, [xv.nome]: b });
          s("line", { x1: X(a), x2: X(b), y1: Y(p1), y2: Y(p2), stroke: PALETA[i % PALETA.length], class: "cm-ajuste" });
        });
      }
    } else {
      const cats = xv ? xv.niveis : [""];
      const larg = (W - m.l - m.r) / cats.length;
      const subs = cor ? cor.niveis : [null];
      const dodge = (k) => (subs.length > 1 ? (k - (subs.length - 1) / 2) * Math.min(larg / (subs.length + 1), 60) : 0);
      const cx = (c, k) => m.l + larg * (cats.indexOf(c) + 0.5) + dodge(k);
      cats.forEach((c) => s("text", { x: m.l + larg * (cats.indexOf(c) + 0.5), y: H - m.b + 18, "text-anchor": "middle", class: "cm-tick" }, c || "todos"));
      if (xv) s("text", { x: (W + m.l) / 2, y: H - 8, "text-anchor": "middle", class: "cm-rotulo" }, xv.nome);
      const meia = Math.min(larg / (subs.length + 1), 60) * 0.32;
      L.forEach((r, i) => {
        const k = cor ? cor.niveis.indexOf(r[cor.nome]) : 0;
        s("circle", { cx: cx(xv ? r[xv.nome] : "", k) + jit(i) * meia * 1.6, cy: Y(r[estado.y]), r: 4, fill: corDe(r), class: "cm-ponto" });
      });
      if (fit && !extras.length) {
        const pontos = subs.map(() => []);
        cats.forEach((c) => subs.forEach((g, k) => {
          const linha = L.find((r) => (!xv || r[xv.nome] === c) && (!cor || r[cor.nome] === g));
          if (!linha) return;
          const v = fit.prediz(linha);
          const x0 = cx(c, k);
          pontos[k].push([x0, Y(v)]);
          s("line", { x1: x0 - meia * 1.3, x2: x0 + meia * 1.3, y1: Y(v), y2: Y(v), stroke: PALETA[k % PALETA.length], class: "cm-media" });
        }));
        if (cor && xv) pontos.forEach((pts, k) => pts.length > 1 && s("polyline", { points: pts.map((p) => p.join(",")).join(" "), stroke: PALETA[k % PALETA.length], class: "cm-ligacao" }));
      }
    }
    // legenda
    if (cor) {
      let x = m.l + 10;
      cor.niveis.forEach((g, k) => {
        const rot = `${cor.nome} = ${g}`;
        s("circle", { cx: x, cy: m.t + 6, r: 5, fill: PALETA[k % PALETA.length] });
        s("text", { x: x + 9, y: m.t + 10, class: "cm-tick", "text-anchor": "start" }, rot);
        x += 9 + rot.length * 6.6 + 18;
      });
    }
    while (svg.firstChild) svgReal.appendChild(svg.firstChild);
    const legenda = !fit ? "" : extras.length
      ? "Este modelo tem mais variáveis do que cabem num gráfico de duas dimensões: o gráfico mostra só os dados, sem os valores previstos."
      : xv && xv.tipo === "num" ? "Pontos: dados. Linhas: valores previstos pelo modelo."
      : "Pontos: dados. Traços: valores previstos pelo modelo (as médias que ele estima).";
    svgReal.setAttribute("aria-label", `Gráfico de ${estado.y}${xv ? " em função de " + xv.nome : ""}${cor ? ", por " + cor.nome : ""}. ${legenda}`);
    return el("figure", { class: "cm-figura" }, svgReal, el("figcaption", {}, legenda));
  }

  // ---------- interface ----------
  function chip(v, ondeEsta) {
    const tipo = v.tipo === "num" ? "numérica" : `fator, ${v.niveis.length} níveis`;
    const remover = ondeEsta !== "pool"
      ? el("span", { class: "cm-x", title: "Tirar", "aria-hidden": "true" }, "×") : null;
    return el("button", {
      type: "button",
      class: `cm-chip cm-${v.tipo}`,
      draggable: "true",
      "data-var": v.nome,
      "data-foco": `var:${v.nome}`,
      title: ondeEsta === "pool" ? "Arraste para uma caixa, ou clique para adicionar" : "Clique para tirar",
      "aria-label": `${v.nome} (${tipo})${ondeEsta === "pool" ? ", adicionar" : ", tirar"}`,
      ondragstart: (e) => { e.dataTransfer.setData("text/plain", v.nome); e.dataTransfer.effectAllowed = "move"; },
      onclick: () => (ondeEsta === "pool" ? adicionaPorClique(v) : tira(v.nome)),
    }, el("span", { class: "cm-chip-nome" }, v.nome), el("span", { class: "cm-chip-tipo" }, v.tipo === "num" ? "num" : `fator ${v.niveis.length}`), remover);
  }
  function caixa(titulo, ajuda, alvo, conteudo) {
    const z = el("div", {
      class: "cm-zona",
      "data-alvo": alvo,
      ondragover: (e) => { e.preventDefault(); z.classList.add("cm-sobre"); },
      ondragleave: () => z.classList.remove("cm-sobre"),
      ondrop: (e) => { e.preventDefault(); z.classList.remove("cm-sobre"); solta(e.dataTransfer.getData("text/plain"), alvo); },
    }, el("div", { class: "cm-zona-titulo" }, titulo), el("div", { class: "cm-zona-chips" }, conteudo.length ? conteudo : el("span", { class: "cm-vazio" }, ajuda)));
    return z;
  }
  function solta(nome, alvo) {
    if (!varPorNome(nome)) return;
    tira(nome, false);
    if (alvo === "y") { if (estado.y) estado.xs.push(estado.y); estado.y = nome; }
    else if (alvo === "x") estado.xs.push(nome);
    desenha();
  }
  function adicionaPorClique(v) {
    if (!estado.y && v.tipo === "num") estado.y = v.nome; else estado.xs.push(v.nome);
    desenha();
  }
  function tira(nome, redesenha = true) {
    if (estado.y === nome) estado.y = null;
    estado.xs = estado.xs.filter((x) => x !== nome);
    if (redesenha) desenha();
  }

  function formula() {
    return `${estado.y} ~ ${estado.xs.length ? estado.xs.join(` ${estado.op} `) : "1"}`;
  }
  // Fixa a ordem dos níveis: o 1º é a referência. Sem isso, o R ordena pelo
  // locale, e "Açude" vem depois de "Temporária" no locale C (o do webR).
  function linhasFator() {
    return estado.xs.map(varPorNome).filter((v) => v.tipo === "fator").map((v) =>
      `dados$${v.nome} <- factor(dados$${v.nome}, levels = c(${v.niveis.map((n) => JSON.stringify(n)).join(", ")}))  # o 1º nível é a referência`);
  }
  // O "caminho clássico" que dá o mesmo valor-p do lm, quando existe um.
  function classico() {
    const xs = estado.xs.map(varPorNome);
    if (xs.length !== 1) return null;
    const x = xs[0], y = estado.y;
    if (x.tipo === "num")
      return { codigo: `cor.test(dados$${x.nome}, dados$${y})`, nome: "na correlação de Pearson", pLm: `summary(modelo)$coefficients[2, 4]` };
    if (x.niveis.length === 2)
      return { codigo: `t.test(${y} ~ ${x.nome}, data = dados, var.equal = TRUE)`, nome: "no teste t", pLm: `summary(modelo)$coefficients[2, 4]` };
    return { codigo: `oneway.test(${y} ~ ${x.nome}, data = dados, var.equal = TRUE)`, nome: "na ANOVA de um fator (oneway.test)", pLm: `anova(modelo)[1, "Pr(>F)"]` };
  }
  function efeitoR() {
    const xs = estado.xs.map(varPorNome);
    if (!xs.length) return [];
    if (xs.length === 1 && xs[0].tipo === "num") return [`summary(modelo)$r.squared   # R²; o r (com IC) está no cor.test()`];
    if (xs.length === 1 && xs[0].niveis.length === 2) return [`coef(modelo)[2] / sigma(modelo)   # d de Cohen: diferença ÷ desvio padrão combinado`];
    if (xs.every((v) => v.tipo === "num") && estado.op === "+") {
      const nomes = xs.map((v) => v.nome);
      const vifs = nomes.map((n) => `1 / (1 - summary(lm(${n} ~ ${nomes.filter((o) => o !== n).join(" + ")}, data = dados))$r.squared)   # VIF de ${n}`);
      return [
        `coef(lm(scale(${estado.y}) ~ ${nomes.map((n) => `scale(${n})`).join(" + ")}, data = dados))   # coeficientes padronizados`,
        `ct <- coef(summary(modelo))[-1, ]; ct[, "t value"]^2 / (ct[, "t value"]^2 + df.residual(modelo))   # η² parcial`,
        ``,
        `# Colinearidade`,
        `cor(dados[c(${nomes.map((n) => JSON.stringify(n)).join(", ")})])`,
        ...vifs,
      ];
    }
    return [
      `d1 <- drop1(modelo, test = "F")`,
      `setNames(d1[["Sum of Sq"]] / (d1[["Sum of Sq"]] + d1$RSS[1]), rownames(d1))[-1]   # η² parcial de cada termo`,
    ];
  }
  function codigoR(arquivo) {
    const c = CONJUNTOS[estado.chave];
    const cl = classico();
    return [
      `dados <- read.csv("${arquivo || SITE + c.arquivo}", fileEncoding = "UTF-8")`,
      ...linhasFator(),
      `modelo <- lm(${formula()}, data = dados)`,
      `summary(modelo)   # coeficientes, erros padrão e valores-p`,
      estado.xs.length ? `anova(modelo)     # tabela de ANOVA (testes F, em sequência)` : null,
      estado.xs.length > 1 ? `drop1(modelo, test = "F")   # cada termo testado depois de todos os outros` : null,
      `plot(modelo)      # gráficos dos resíduos`,
      cl ? `${cl.codigo}   # o mesmo valor-p, pelo caminho clássico` : null,
      cl && cl.codigo.startsWith("t.test") ? `${cl.codigo.replace(", var.equal = TRUE", "")}   # Welch: não supõe variâncias iguais` : null,
      ``,
      `# Tamanho do efeito`,
      `confint(modelo)   # IC 95% de cada coeficiente`,
      ...efeitoR(),
    ].filter(Boolean).join("\n");
  }

  // ---------- R de verdade (webR, carregado só quando o aluno pede) ----------
  const WEBR_URL = "https://webr.r-wasm.org/v0.6.0/"; // a mesma versão do quarto-live das aulas
  const R = { estado: "parado", webR: null, escritos: new Set(), cache: new Map(), pedido: 0, fila: Promise.resolve() };

  async function iniciaR() {
    if (R.estado !== "parado" && R.estado !== "erro") return;
    R.estado = "carregando";
    desenha();
    try {
      const { WebR } = await import(WEBR_URL + "webr.mjs");
      R.webR = new WebR({ baseUrl: WEBR_URL });
      await R.webR.init();
      await R.webR.evalRVoid("options(width = 72)");
      R.estado = "pronto";
    } catch (e) {
      console.error(e);
      R.estado = "erro";
    }
    desenha();
  }

  // Uma execução por vez: as chamadas usam objetos globais do R (dados, modelo...)
  // e não podem se intercalar quando o aluno muda o modelo no meio de uma.
  function rodaR() {
    const pedido = R.fila.then(rodaRAgora);
    R.fila = pedido.catch(() => {});
    return pedido;
  }
  async function rodaRAgora() {
    // fotografia do modelo agora: o aluno pode mudá-lo durante os await
    const cl = classico(), f = formula(), lf = linhasFator();
    const doisNiveis = estado.xs.length === 1 && varPorNome(estado.xs[0]).tipo === "fator" && varPorNome(estado.xs[0]).niveis.length === 2;
    const ySnap = estado.y, x0 = estado.xs[0];
    const chave = estado.chave + "|" + f;
    if (R.cache.has(chave)) return R.cache.get(chave);
    const c = CONJUNTOS[estado.chave];
    const caminho = "/home/web_user/" + c.arquivo.split("/").pop();
    if (!R.escritos.has(caminho)) {
      await R.webR.FS.writeFile(caminho, new TextEncoder().encode(estado.csvTexto));
      R.escritos.add(caminho);
    }
    const w = R.webR;
    const texto = async (cod) => w.evalRString(`paste(capture.output(${cod}), collapse = "\\n")`);
    // o webR não converte NA para number[]: vira um sentinela no R e NaN aqui
    const numeros = async (cod) => (await w.evalRRaw(`local({ x <- as.numeric(${cod}); x[is.na(x)] <- -9.87654e300; x })`, "number[]"))
      .map((v) => (v === -9.87654e300 ? NaN : v));
    try {
      await w.evalRVoid([`dados <- read.csv("${caminho}", encoding = "UTF-8")`, ...lf, `modelo <- lm(${f}, data = dados)`].join("\n"));
      const out = {
        summary: await texto("summary(modelo)"),
        anova: estado.xs.length ? await texto("anova(modelo)") : null,
        ajustados: await numeros("fitted(modelo)"),
        residuos: await numeros("residuals(modelo)"),
        qq: await numeros(`unlist(qqnorm(rstandard(modelo), plot.it = FALSE))`),
      };
      // números para os tamanhos de efeito e o texto do resultado
      await w.evalRVoid([
        "s <- summary(modelo)",
        "fst <- if (is.null(s$fstatistic)) c(NA, NA, NA) else s$fstatistic",
        "ct <- cbind(coef(s), confint(modelo))",
        "av <- anova(modelo)",
        "d1 <- if (length(attr(terms(modelo), 'term.labels'))) drop1(modelo, test = 'F') else NULL",
      ].join("\n"));
      const g = await numeros("c(nobs(modelo), df.residual(modelo), s$r.squared, fst, pf(fst[1], fst[2], fst[3], lower.tail = FALSE), sigma(modelo))");
      out.global = { n: g[0], gl: g[1], r2: g[2], F: g[3], gl1: g[4], gl2: g[5], p: g[6], sigma: g[7] };
      const nc = await numeros("ct"); // por coluna: estimativa, EP, t, P, IC inferior, IC superior
      const k = nc.length / 6;
      out.coefs = (await w.evalRRaw("rownames(ct)", "string[]")).map((nome, i) => ({
        nome, b: nc[i], ep: nc[k + i], t: nc[2 * k + i], p: nc[3 * k + i], li: nc[4 * k + i], ls: nc[5 * k + i],
      }));
      if (/~ 1$/.test(f) === false) {
        // drop1(): cada termo testado depois de todos os outros (respeitando a
        // hierarquia: com interação, só os termos de ordem mais alta). Não depende da ordem.
        out.drop1 = await texto("d1");
        const na = await numeros('c(d1$Df, d1[["Sum of Sq"]], d1[["F value"]], d1[["Pr(>F)"]], d1$RSS[1])');
        const m = (na.length - 1) / 4;
        const rss = na[na.length - 1];
        out.termos = (await w.evalRRaw("rownames(d1)", "string[]")).slice(1).map((nome, i) => ({
          nome: nome.trim(), gl: na[i + 1], F: na[2 * m + i + 1], p: na[3 * m + i + 1], eta2p: na[m + i + 1] / (na[m + i + 1] + rss),
        }));
      }
      if (doisNiveis) {
        const [tw, glw, pw] = await numeros(`unlist(t.test(${ySnap} ~ ${x0}, data = dados)[c("statistic", "parameter", "p.value")])`);
        out.welch = { t: tw, gl: glw, p: pw, codigo: `t.test(${ySnap} ~ ${x0}, data = dados)` };
      }
      if (cl) {
        await w.evalRVoid(`classico <- ${cl.codigo}`);
        out.classico = { nome: cl.nome, codigo: cl.codigo, texto: await texto("classico"), p: (await numeros("c(classico$p.value, " + cl.pLm + ")")) };
        if (cl.codigo.startsWith("cor.test")) {
          const [r, li, ls] = await numeros("c(classico$estimate, classico$conf.int)");
          out.r = { r, li, ls };
        }
      }
      R.cache.set(chave, out);
      return out;
    } catch (e) {
      return { erro: String(e.message || e) };
    }
  }

  function dispersao({ x, y, xlab, ylab, ref }) {
    const ns = "http://www.w3.org/2000/svg";
    const W = 320, H = 240, m = { l: 46, r: 10, t: 10, b: 38 };
    const svg = document.createElementNS(ns, "svg");
    svg.setAttribute("viewBox", `0 0 ${W} ${H}`);
    svg.setAttribute("class", "cm-svg");
    svg.setAttribute("role", "img");
    const s = (tag, attrs, t) => { const e = document.createElementNS(ns, tag); for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v); if (t !== undefined) e.textContent = t; svg.appendChild(e); return e; };
    const ext = (a) => { let lo = Math.min(...a), hi = Math.max(...a); const p = (hi - lo) * 0.06 || 1; return [lo - p, hi + p]; };
    let [x0, x1] = ext(x), [y0, y1] = ext(y);
    if (ref === "identidade") { x0 = y0 = Math.min(x0, y0); x1 = y1 = Math.max(x1, y1); }
    const X = (v) => m.l + (W - m.l - m.r) * (v - x0) / (x1 - x0);
    const Y = (v) => m.t + (H - m.t - m.b) * (1 - (v - y0) / (y1 - y0));
    s("line", { x1: m.l, x2: m.l, y1: m.t, y2: H - m.b, class: "cm-eixo" });
    s("line", { x1: m.l, x2: W - m.r, y1: H - m.b, y2: H - m.b, class: "cm-eixo" });
    const marcas = (a, b) => { const passo = Math.pow(10, Math.floor(Math.log10((b - a) / 5))); const k = [1, 2, 2.5, 5, 10].find((f) => (b - a) / (f * passo) <= 5) * passo; const o = []; for (let v = Math.ceil(a / k) * k; v <= b; v += k) o.push(+v.toFixed(10)); return o; };
    marcas(y0, y1).forEach((t) => s("text", { x: m.l - 5, y: Y(t) + 4, "text-anchor": "end", class: "cm-tick" }, t.toLocaleString("pt-BR")));
    marcas(x0, x1).forEach((t) => s("text", { x: X(t), y: H - m.b + 15, "text-anchor": "middle", class: "cm-tick" }, t.toLocaleString("pt-BR")));
    if (ref === "zero") s("line", { x1: m.l, x2: W - m.r, y1: Y(0), y2: Y(0), class: "cm-ref" });
    else s("line", { x1: X(x0), x2: X(x1), y1: Y(x0), y2: Y(x1), class: "cm-ref" });
    x.forEach((v, i) => s("circle", { cx: X(v), cy: Y(y[i]), r: x.length > 300 ? 1.8 : 3.2, fill: PALETA[0], class: "cm-ponto" }));
    s("text", { x: (W + m.l) / 2, y: H - 5, "text-anchor": "middle", class: "cm-rotulo" }, xlab);
    s("text", { x: 11, y: (H - m.b + m.t) / 2, transform: `rotate(-90 11 ${(H - m.b + m.t) / 2})`, "text-anchor": "middle", class: "cm-rotulo" }, ylab);
    return svg;
  }

  // ---------- tamanho do efeito e texto do resultado ----------
  // Convenções de Cohen (1988): rótulos gerais, não substituem o julgamento biológico.
  const rotulo = (v, cortes) => {
    const a = Math.abs(v);
    return a < cortes[0] ? "desprezível" : a < cortes[1] ? "pequeno" : a < cortes[2] ? "médio" : "grande";
  };
  const CORTES = { d: [0.2, 0.5, 0.8], r: [0.1, 0.3, 0.5], eta2: [0.01, 0.06, 0.14] };
  const fP = (p) => (p < 0.001 ? "P < 0,001" : `P = ${num(p, 3)}`);
  const ic = (li, ls) => `IC 95%: ${num(li)} a ${num(ls)}`;
  const nomeTermo = (t) => (t.includes(":") ? `interação ${t.split(":").join(" × ")}` : t);

  const lista = (a) => (a.length < 2 ? a.join("") : `${a.slice(0, -1).join(", ")} e ${a[a.length - 1]}`);
  // IC para frases com valor absoluto ("32 mg menor"): em módulo quando não cruza o zero
  const icAbs = (c) => (c.li * c.ls > 0
    ? `IC 95%: ${num(Math.min(Math.abs(c.li), Math.abs(c.ls)))} a ${num(Math.max(Math.abs(c.li), Math.abs(c.ls)))}`
    : `IC 95%: ${num(c.li)} a ${num(c.ls)}, inclui o zero`);

  function analisaResultado(out) {
    const y = estado.y, G = out.global;
    const xs = estado.xs.map(varPorNome);
    const coef = (nome) => out.coefs.find((c) => c.nome === nome);
    const linhas = []; // tabela de tamanhos de efeito: [medida, valor, IC, leitura]
    const frases = [];
    const sentido = (b) => (b >= 0 ? "maior" : "menor");
    const verbo = (b) => (b >= 0 ? "aumentou" : "diminuiu");

    if (!xs.length) {
      const c = out.coefs[0];
      linhas.push(["média de " + y, num(c.b), `${num(c.li)} a ${num(c.ls)}`, "na unidade dos dados"]);
      frases.push(`A média de ${y} foi ${num(c.b)} (${ic(c.li, c.ls)}; n = ${G.n}).`);
      return { linhas, frases };
    }
    const tipo = nomeDoTeste().nome.replace(/ \((.*)\)$/, ", $1"); // sem parênteses dentro de parênteses
    if (xs.length === 1 && xs[0].tipo === "num") {
      const x = xs[0].nome, c = coef(x);
      linhas.push([`inclinação (${y} por unidade de ${x})`, num(c.b), `${num(c.li)} a ${num(c.ls)}`, "na unidade dos dados"]);
      if (out.r) linhas.push(["correlação r de Pearson", num(out.r.r), `${num(out.r.li)} a ${num(out.r.ls)}`, rotulo(out.r.r, CORTES.r)]);
      linhas.push(["R² (variação explicada)", num(G.r2), "—", ""]);
      frases.push(`Ajustamos uma regressão linear simples de ${y} em função de ${x} (n = ${G.n}).`);
      frases.push(`${y} ${verbo(c.b)} em média ${num(Math.abs(c.b))} a cada unidade a mais de ${x} (${icAbs(c)}; t(${G.gl}) = ${num(c.t)}; ${fP(c.p)}).`);
      frases.push(`O modelo explicou ${num(G.r2 * 100, 1)}% da variação em ${y} (R² = ${num(G.r2)})` +
        (out.r ? `, e a correlação foi de tamanho ${rotulo(out.r.r, CORTES.r)} (r = ${num(out.r.r)}; IC 95%: ${num(out.r.li)} a ${num(out.r.ls)}).` : "."));
      return { linhas, frases };
    }
    if (xs.length === 1 && xs[0].niveis.length === 2) {
      const x = xs[0], lv = x.niveis[1], ref = x.niveis[0], c = coef(x.nome + lv);
      const d = c.b / G.sigma;
      const v = variancias();
      linhas.push([`diferença (${lv} − ${ref})`, num(c.b), `${num(c.li)} a ${num(c.ls)}`, "na unidade dos dados"]);
      linhas.push(["d de Cohen (diferença ÷ desvio padrão)", num(d), "—", rotulo(d, CORTES.d)]);
      frases.push(`Comparamos ${y} entre os dois níveis de ${x.nome} com um modelo linear, equivalente ao teste t para amostras independentes com variâncias iguais (n = ${G.n}).`);
      frases.push(`Com ${x.nome} = ${lv}, ${y} foi em média ${num(Math.abs(c.b))} ${sentido(c.b)} do que com ${x.nome} = ${ref} (${icAbs(c)}; t(${G.gl}) = ${num(c.t)}; ${fP(c.p)}), uma diferença ${rotulo(d, CORTES.d) === "desprezível" ? "desprezível" : "de tamanho " + rotulo(d, CORTES.d)} (d de Cohen = ${num(d)}).`);
      if (v && v.razao > 4 && out.welch)
        frases.push(`Os desvios padrão foram muito diferentes entre os grupos (${v.grupos.map((g) => `${g.nivel}: ${num(g.dp)}`).join("; ")}), o que viola o pressuposto de variâncias iguais. Com a correção de Welch, t(${num(out.welch.gl, 1)}) = ${num(-out.welch.t)}; ${fP(out.welch.p)}.`); // mesmo sinal do lm()
      return { linhas, frases };
    }
    // Regressão múltipla sem interação: testes t do summary() (não dependem da
    // ordem dos termos), η² parcial = t² / (t² + gl) e coeficiente padronizado.
    if (xs.every((v) => v.tipo === "num") && estado.op === "+") {
      const sdY = dp(estado.linhas.map((r) => r[y]));
      const colin = colinearidade() || [];
      frases.push(`Ajustamos uma regressão linear múltipla de ${y} em função de ${lista(xs.map((v) => v.nome))} (n = ${G.n}). O modelo explicou ${num(G.r2 * 100, 1)}% da variação em ${y} (R² = ${num(G.r2)}; F(${G.gl1}, ${G.gl2}) = ${num(G.F)}; ${fP(G.p)}).`);
      for (const v of xs) {
        const c = coef(v.nome);
        const beta = c.b * dp(estado.linhas.map((r) => r[v.nome])) / sdY;
        const eta = (c.t * c.t) / (c.t * c.t + G.gl);
        linhas.push([`inclinação de ${v.nome}`, num(c.b), `${num(c.li)} a ${num(c.ls)}`, "na unidade dos dados"]);
        linhas.push([`↳ padronizada (em desvios padrão)`, num(beta), "—", "compara preditores"]);
        linhas.push([`↳ η² parcial`, num(eta), "—", rotulo(eta, CORTES.eta2)]);
        frases.push(c.p < 0.05
          ? `Com os outros preditores fixos, ${y} ${verbo(c.b)} ${num(Math.abs(c.b))} a cada unidade a mais de ${v.nome} (${icAbs(c)}; t(${G.gl}) = ${num(c.t)}; ${fP(c.p)}; coeficiente padronizado = ${num(beta)}).`
          : `Com os outros preditores fixos, não detectamos efeito de ${v.nome} (inclinação = ${num(c.b)}; ${ic(c.li, c.ls)}; t(${G.gl}) = ${num(c.t)}; ${fP(c.p)}).`);
      }
      linhas.push(["R² do modelo", num(G.r2), "—", ""]);
      const ruins = colin.filter((l) => l.vif > 5);
      if (ruins.length) {
        // um par por vez, sem repetir (a com b e b com a)
        const pares = [...new Map(ruins.map((l) => [[l.nome, l.maior.com].sort().join("|"), l])).values()];
        const vifs = ruins.map((l) => `${l.nome} = ${num(l.vif)}`).join(", ");
        frases.push(`${pares.map((l) => `${l.nome} e ${l.maior.com} (r = ${num(l.maior.r)})`).join("; ")} estavam fortemente correlacionados (VIF: ${vifs}), o que aumenta a incerteza sobre o efeito de cada um.`);
      }
      return { linhas, frases };
    }

    // ANOVA, fatorial, ANCOVA e afins. Os F vêm do drop1(): cada termo é testado
    // depois de todos os outros, então o resultado não depende da ordem dos preditores.
    const desc = (t) => `F(${t.gl}, ${G.gl}) = ${num(t.F)}; ${fP(t.p)}; η²p = ${num(t.eta2p)}`;
    frases.push(`Ajustamos um modelo linear (${tipo}) com ${y} como resposta e ${lista(estado.xs)} como ${xs.length > 1 ? "preditores" : "preditor"} (n = ${G.n}; R² = ${num(G.r2)}).`);
    const inter = out.termos.filter((t) => t.nome.includes(":"));

    if (!inter.length) {
      // sem interação: cada coeficiente é uma diferença ou inclinação com IC, na unidade dos dados
      for (const v of xs) {
        if (v.tipo === "num") { const c = coef(v.nome); linhas.push([`inclinação de ${v.nome}`, num(c.b), `${num(c.li)} a ${num(c.ls)}`, "na unidade dos dados"]); }
        else for (const lv of v.niveis.slice(1)) { const c = coef(v.nome + lv); linhas.push([`diferença ${v.nome}: ${lv} − ${v.niveis[0]}`, num(c.b), `${num(c.li)} a ${num(c.ls)}`, "na unidade dos dados"]); }
      }
      for (const t of out.termos) linhas.push([`η² parcial de ${t.nome}`, num(t.eta2p), "—", rotulo(t.eta2p, CORTES.eta2)]);
      linhas.push(["R² do modelo", num(G.r2), "—", ""]);
      const fixos = xs.length > 1 ? ", com os outros preditores fixos," : "";
      for (const t of out.termos) {
        frases.push(t.p < 0.05
          ? `O efeito de ${t.nome} sobre ${y}${fixos} foi significativo e ${rotulo(t.eta2p, CORTES.eta2)} (${desc(t)}).`
          : `Não detectamos efeito de ${t.nome} sobre ${y}${fixos.replace(/,$/, "")} (${desc(t)}).`);
      }
      for (const x of xs.filter((v) => v.tipo === "fator")) {
        const comps = x.niveis.slice(1).map((lv) => { const c = coef(x.nome + lv); return `${x.nome} = ${lv} teve ${y} ${num(Math.abs(c.b))} ${sentido(c.b)} (${icAbs(c)}; ${fP(c.p)})`; });
        const outrosNomes = xs.filter((v) => v !== x).map((v) => v.nome);
        const abre = outrosNomes.length ? `Com ${lista(outrosNomes)} ${outrosNomes.length > 1 ? "fixos" : "fixo"}, em` : "Em";
        frases.push(`${abre} relação a ${x.nome} = ${x.niveis[0]}, ${comps.join("; ")}.` +
          (x.niveis.length > 2 ? " Estas são comparações com o grupo de referência; para comparar todos os pares, use o teste de Tukey (Aula 11)." : ""));
      }
      for (const v of xs.filter((v) => v.tipo === "num")) {
        const c = coef(v.nome);
        frases.push(`Com os outros preditores fixos, ${y} ${verbo(c.b)} ${num(Math.abs(c.b))} a cada unidade a mais de ${v.nome} (${icAbs(c)}; ${fP(c.p)}).`);
      }
      return { linhas, frases };
    }

    // Com interação: o drop1() só testa a interação de ordem mais alta. Os efeitos
    // principais não são interpretados sozinhos, porque dependem do nível das outras variáveis.
    for (const t of inter) linhas.push([`η² parcial da ${nomeTermo(t.nome)}`, num(t.eta2p), "—", rotulo(t.eta2p, CORTES.eta2)]);
    const algumaSig = inter.some((t) => t.p < 0.05);
    for (const t of inter) {
      const vs = t.nome.split(":");
      const temNum = vs.some((n) => varPorNome(n).tipo === "num");
      frases.push(t.p < 0.05
        ? `A interação entre ${lista(vs)} foi significativa (${desc(t)}): o efeito de cada uma dessas variáveis depende ${vs.length > 2 ? "das outras" : temNum ? "do valor da outra" : "do nível da outra"}. Por isso, não interpretamos os efeitos de cada variável sozinha.`
        : `A interação entre ${lista(vs)} não foi significativa (${desc(t)}): não há evidência de que o efeito de uma dependa ${vs.length > 2 ? "das outras" : "da outra"}. Para estimar o efeito de cada variável, ajuste o modelo sem interação (troque * por +).`);
    }
    if (algumaSig) efeitosSimples(xs, coef, linhas, frases);
    linhas.push(["R² do modelo", num(G.r2), "—", ""]);
    return { linhas, frases };
  }

  // Com interação significativa, descreve o padrão: as médias previstas de cada
  // combinação (dois fatores), a inclinação em cada grupo (fator × numérica) ou
  // quanto uma inclinação muda com a outra numérica.
  function efeitosSimples(xs, coef, linhas, frases) {
    if (xs.length !== 2) return;
    const fit = ajusta();
    const y = estado.y;
    const linhaCom = (cond) => estado.linhas.find((r) => Object.entries(cond).every(([k, v]) => r[k] === v));
    const [a, b] = xs;
    if (a.tipo === "fator" && b.tipo === "fator") {
      const medias = [];
      for (const lb of b.niveis) for (const la of a.niveis) {
        const r = linhaCom({ [a.nome]: la, [b.nome]: lb });
        if (r) { const m = fit.prediz(r); medias.push({ la, lb, m }); linhas.push([`média prevista (${a.nome} = ${la}, ${b.nome} = ${lb})`, num(m), "—", "na unidade dos dados"]); }
      }
      const porB = b.niveis.map((lb) => {
        const ref = medias.find((m) => m.lb === lb && m.la === a.niveis[0]);
        const difs = a.niveis.slice(1).map((la) => { const m = medias.find((x) => x.lb === lb && x.la === la); return ref && m ? `${a.nome} = ${la} teve ${y} ${num(Math.abs(m.m - ref.m))} ${m.m >= ref.m ? "maior" : "menor"} que ${a.nome} = ${a.niveis[0]}` : null; }).filter(Boolean);
        return `com ${b.nome} = ${lb}, ${difs.join("; ")}`;
      });
      frases.push(`Pelas médias previstas pelo modelo, ${porB.join(", enquanto ")} (veja o gráfico de interação).`);
      return;
    }
    if (a.tipo !== b.tipo) {
      const f = a.tipo === "fator" ? a : b, n = a.tipo === "num" ? a : b;
      const incl = f.niveis.map((g) => {
        const r = linhaCom({ [f.nome]: g });
        return { g, b: fit.prediz({ ...r, [n.nome]: 1 }) - fit.prediz({ ...r, [n.nome]: 0 }) };
      });
      incl.forEach((i) => linhas.push([`inclinação de ${n.nome} em ${f.nome} = ${i.g}`, num(i.b), "—", "na unidade dos dados"]));
      frases.push(`A inclinação de ${n.nome} foi ${incl.map((i) => `${num(i.b)} em ${f.nome} = ${i.g}`).join(", ")} (${y} por unidade de ${n.nome}; veja as retas no gráfico).`);
      return;
    }
    const c = coef(`${a.nome}:${b.nome}`);
    if (c) {
      linhas.push([`mudança na inclinação de ${a.nome} por unidade de ${b.nome}`, num(c.b), `${num(c.li)} a ${num(c.ls)}`, "na unidade dos dados"]);
      frases.push(`A inclinação de ${a.nome} ${verboInt(c.b)} ${num(Math.abs(c.b))} a cada unidade a mais de ${b.nome} (${icAbs(c)}).`);
    }
  }
  const verboInt = (b) => (b >= 0 ? "aumentou" : "diminuiu");

  // desvios padrão por grupo, quando o modelo é um fator só
  function variancias() {
    if (estado.xs.length !== 1) return null;
    const x = varPorNome(estado.xs[0]);
    if (x.tipo !== "fator") return null;
    const grupos = x.niveis.map((nivel) => ({ nivel, dp: dp(estado.linhas.filter((r) => r[x.nome] === nivel).map((r) => r[estado.y])) }));
    const vs = grupos.map((g) => g.dp ** 2);
    return { grupos, razao: Math.max(...vs) / Math.min(...vs) };
  }

  function blocoResultado(out) {
    const { linhas, frases } = analisaResultado(out);
    const tabela = el("table", { class: "table table-sm cm-efeitos" },
      el("thead", {}, el("tr", {}, ...["Medida", "Valor", "IC 95%", "Leitura"].map((h) => el("th", {}, h)))),
      el("tbody", {}, ...linhas.map((l) => el("tr", {}, ...l.map((c) => el("td", {}, c))))));
    const texto = frases.join(" ");
    const copiar = el("button", { type: "button", class: "btn btn-sm btn-outline-primary", onclick: () => {
      navigator.clipboard?.writeText(texto).then(() => { copiar.textContent = "Copiado!"; setTimeout(() => (copiar.textContent = "Copiar texto"), 1500); });
    } }, "Copiar texto");
    return [
      el("div", { class: "cm-sub" }, "Tamanho do efeito"),
      el("p", { class: "cm-nota" }, "O valor-p diz se o efeito é distinguível de zero; o tamanho do efeito diz se ele é grande. Comece pela diferença ou pela inclinação na unidade dos dados (mg, °C, polegadas): é ela que tem sentido biológico."),
      tabela,
      el("p", { class: "cm-nota" }, "Os rótulos (pequeno, médio, grande) são convenções gerais de Cohen (1988), pensadas para as ciências do comportamento; para o η², os cortes são 0,01, 0,06 e 0,14. O η² parcial costuma sair maior que o η², porque desconta os outros termos, então use os rótulos com ainda mais cautela. Se uma diferença de 10 mg importa para um girino é uma pergunta de biologia, não de estatística (",
        el("a", { href: "aula-07-teste-hipoteses/index.html" }, "Aula 7"), ")."),
      el("div", { class: "cm-sub cm-sub-topo" }, el("span", {}, "Como escrever o resultado"), copiar),
      el("blockquote", { class: "cm-relato" }, texto),
      el("p", { class: "cm-nota" }, "Texto gerado automaticamente, inspirado no pacote ",
        el("a", { href: "https://easystats.github.io/report/" }, "report"),
        ", do easystats. É um ponto de partida: confira os resíduos acima antes de usá-lo, comece pela resposta à sua hipótese e diga o que o efeito significa para o organismo."),
    ];
  }

  function painelR() {
    const box = el("div", { class: "cm-r" }, el("span", { class: "cm-rot" }, "Saída do R"));
    if (R.estado === "parado" || R.estado === "erro") {
      box.append(
        el("p", { class: "cm-nota" }, "Até aqui, as contas foram feitas pela página. Para ver os valores-p, a tabela de ANOVA e os resíduos, rode o modelo no R, aqui mesmo no navegador."),
        el("button", { type: "button", class: "btn btn-primary btn-sm", "data-foco": "carregar", onclick: iniciaR }, "Carregar o R"));
      if (R.estado === "erro") box.append(el("div", { class: "cm-alerta cm-pequeno" }, "Não consegui carregar o R. Confira a conexão e tente de novo, ou copie o código abaixo para o RStudio."));
      return box;
    }
    if (R.estado === "carregando") {
      box.append(el("p", { class: "cm-instrucao" }, "Carregando o R no navegador… na primeira vez leva alguns segundos."));
      return box;
    }
    const alvo = el("div", {}, el("p", { class: "cm-instrucao" }, "Rodando lm()…"));
    box.append(alvo);
    const meu = ++R.pedido;
    rodaR().then((out) => {
      if (meu !== R.pedido) return; // o aluno já mudou o modelo
      if (out.erro) { alvo.replaceChildren(el("div", { class: "cm-alerta" }, "O R devolveu um erro: " + out.erro)); return; }
      mostraSaidaR(out, alvo);
    }).catch((e) => {
      console.error(e);
      if (meu === R.pedido) alvo.replaceChildren(el("div", { class: "cm-alerta" }, "Algo deu errado ao montar a saída do R. Mude o modelo ou recarregue a página."));
    });
    return box;
  }

  function mostraSaidaR(out, alvo) {
    {
      const pre = (t) => el("pre", { class: "cm-saida" }, el("code", {}, t));
      const partes = [
        el("p", { class: "cm-nota" }, "Em ", el("code", {}, "Estimate"), " estão os mesmos números da equação acima; ", el("code", {}, "Pr(>|t|)"), " é o valor-p de cada coeficiente (H₀: o coeficiente é zero)."),
        el("div", { class: "cm-sub" }, el("code", {}, "summary(modelo)")), pre(out.summary),
      ];
      if (out.anova) partes.push(
        el("div", { class: "cm-sub" }, el("code", {}, "anova(modelo)")),
        el("p", { class: "cm-nota" }, "Um teste F por termo. Com fatores de vários níveis, é aqui que se lê se o fator importa como um todo.",
          estado.xs.length > 1 ? " Atenção: a anova() testa os termos em sequência (soma de quadrados tipo I), e cada F só desconta os termos que vêm antes dele. Com preditores correlacionados ou grupos de tamanhos diferentes, mudar a ordem muda os F." : ""),
        pre(out.anova));
      if (out.drop1 && estado.xs.length > 1) partes.push(
        el("div", { class: "cm-sub" }, el("code", {}, 'drop1(modelo, test = "F")')),
        el("p", { class: "cm-nota" }, "Aqui cada termo é testado depois de todos os outros, então a ordem não importa. Com interação, só aparece a interação: não faz sentido tirar um efeito principal e deixar a interação que depende dele. Os F do texto de resultado abaixo vêm daqui."),
        pre(out.drop1));
      if (out.classico) {
        const [pc, pl] = out.classico.p;
        const fmt = (p) => (p < 0.001 ? p.toExponential(2).replace(".", ",") : num(p, 4));
        partes.push(
          el("div", { class: "cm-sub" }, "O mesmo teste pelo caminho clássico: ", el("code", {}, out.classico.codigo)),
          pre(out.classico.texto),
          el("div", { class: "cm-igual" }, `Valor-p no lm(): ${fmt(pl)} · ${out.classico.nome}: ${fmt(pc)}. `,
            Math.abs(pc - pl) <= 1e-8 + 1e-6 * pl ? el("strong", {}, "É o mesmo teste.") : "Diferem: confira os pressupostos."));
        if (out.welch) {
          const x = varPorNome(estado.xs[0]);
          partes.push(el("p", { class: "cm-nota" }, `O t do t.test() tem o sinal trocado em relação ao do lm(): o t.test() faz ${x.niveis[0]} − ${x.niveis[1]}, e o lm() faz ${x.niveis[1]} − ${x.niveis[0]}. O valor-p é o mesmo.`));
          const v = variancias();
          partes.push(el("div", { class: v.razao > 4 ? "cm-alerta" : "cm-nota" },
            v.razao > 4 ? el("strong", {}, "Variâncias muito diferentes. ") : null,
            `Desvios padrão: ${v.grupos.map((g) => `${g.nivel} = ${num(g.dp)}`).join("; ")} (a maior variância é ${num(v.razao, 1)} vezes a menor). `,
            v.razao > 4 ? "O lm() e o t.test(var.equal = TRUE) supõem variâncias iguais, e aqui isso não vale. " : "",
            `Com a correção de Welch, que não supõe variâncias iguais (`, el("code", {}, out.welch.codigo), `): t(${num(out.welch.gl, 1)}) = ${num(out.welch.t)}; ${fP(out.welch.p)}.`,
            v.razao > 4 ? " Às vezes a diferença de variâncias vem de outra variável que ficou fora do modelo: experimente acrescentar os outros preditores." : ""));
        }
      }
      const n = out.ajustados.length;
      partes.push(
        el("div", { class: "cm-sub" }, "Os resíduos estão bem-comportados?"),
        el("div", { class: "cm-residuos" },
          el("figure", {}, dispersao({ x: out.ajustados, y: out.residuos, xlab: "valores ajustados", ylab: "resíduos", ref: "zero" }),
            el("figcaption", {}, "Resíduos × ajustados: procure uma nuvem sem forma, de largura constante em torno do zero. Um funil indica variâncias diferentes; uma curva, relação não linear.")),
          el("figure", {}, dispersao({ x: out.qq.slice(0, n), y: out.qq.slice(n), xlab: "quantis teóricos (normal)", ylab: "resíduos padronizados", ref: "identidade" }),
            el("figcaption", {}, "Gráfico Q-Q: se os resíduos forem normais, os pontos seguem a diagonal. Desvios nas pontas indicam caudas pesadas ou assimetria."))));
      partes.push(...blocoResultado(out).filter(Boolean));
      alvo.replaceChildren(...partes);
    }
  }

  function anuncia() {
    let t = "";
    if (estado.y && varPorNome(estado.y).tipo === "num") t = `Fórmula ${formula()}: ${nomeDoTeste().nome}.`;
    else if (estado.y) t = "A resposta é um fator; o lm() precisa de uma resposta numérica.";
    if (avisoVivo.textContent !== t) avisoVivo.textContent = t;
  }

  function painelResultado() {
    const box = el("div", { class: "cm-resultado" });
    if (!estado.y) {
      box.append(el("p", { class: "cm-instrucao" }, "Comece arrastando uma variável numérica para a caixa Resposta (y)."));
      return box;
    }
    if (varPorNome(estado.y).tipo === "fator") {
      box.append(el("div", { class: "cm-alerta" },
        el("strong", {}, "A resposta é um fator. "),
        "O lm() precisa de uma resposta numérica. Para comparar contagens de categorias, veja o qui-quadrado na ",
        el("a", { href: "aula-10-qui-quadrado/index.html" }, "Aula 10"), "."));
      return box;
    }
    const f = formula();
    if (estado.xs.length) estado.tentados.add(f);
    const teste = nomeDoTeste();
    box.append(
      el("div", { class: "cm-formula" }, el("span", { class: "cm-rot" }, "Fórmula"), el("code", {}, f)),
      el("div", { class: "cm-teste" },
        el("div", { class: "cm-teste-nome" }, teste.nome),
        el("p", {}, teste.texto, " ", ...teste.aulas.flatMap((a, i) => [i ? " · " : "Veja a ", el("a", { href: a.href }, a.nome)]), "."))
    );
    const fit = ajusta();
    if (fit.erro) { box.append(el("div", { class: "cm-alerta" }, fit.erro)); return box; }
    const v = variancias();
    if (v && v.razao > 4) box.append(el("div", { class: "cm-alerta" },
      el("strong", {}, "Os grupos têm variâncias muito diferentes. "),
      `Desvios padrão: ${v.grupos.map((g) => `${g.nivel} = ${num(g.dp)}`).join("; ")}. O lm(), o teste t clássico e a ANOVA supõem variâncias iguais. `,
      v.grupos.length === 2 ? "Carregue o R para ver o teste de Welch, que não depende disso." : "Veja os resíduos e considere a ANOVA de Welch, oneway.test(var.equal = FALSE).",
      " Às vezes a diferença vem de outra variável que ficou fora do modelo."));

    // equação
    const termosEq = fit.cols.map((c, j) => {
      if (j === 0) return num(fit.b[0]);
      const sinal = fit.b[j] < 0 ? " − " : " + ";
      const nomeT = c.partes.map((p) => (p.tipo === "num" ? p.nome : `[${p.nome} = ${p.nivel}]`)).join(" · ");
      return [sinal, el("span", { class: "cm-coef" }, num(Math.abs(fit.b[j]))), " · ", nomeT];
    });
    box.append(el("div", { class: "cm-equacao" },
      el("span", { class: "cm-rot" }, "Equação ajustada"),
      el("div", { class: "cm-eq" }, `${estado.y} previsto = `, ...termosEq.flat()),
      el("p", { class: "cm-nota" }, estado.xs.some((n) => varPorNome(n).tipo === "fator") ? "[grupo = nível] vale 1 para quem é daquele grupo e 0 para os outros. " : "",
        `R² = ${num(fit.r2)} · n = ${fit.n}.`)));

    // leitura dos coeficientes
    const lista = el("ul", { class: "cm-coefs" });
    fit.cols.forEach((c, j) => {
      const l = leCoeficiente(c, j, fit.b);
      lista.append(el("li", {}, el("code", {}, l.termo), " = ", el("strong", {}, l.valor), ": ", l.txt, ".", l.alerta ? el("div", { class: "cm-alerta cm-pequeno" }, l.alerta) : null));
    });
    box.append(el("details", { class: "cm-detalhes", open: true }, el("summary", {}, "O que cada coeficiente quer dizer"), lista));

    box.append(grafico(fit));
    const colin = estado.op === "+" ? painelColinearidade() : null; // com interação, o VIF dos efeitos principais não descreve o modelo
    if (colin) box.append(colin);
    box.append(painelR());

    // código
    const codigo = codigoR();
    const botao = el("button", { type: "button", class: "btn btn-sm btn-outline-primary cm-copiar", onclick: () => {
      navigator.clipboard?.writeText(codigo).then(() => { botao.textContent = "Copiado!"; setTimeout(() => (botao.textContent = "Copiar"), 1500); });
    } }, "Copiar");
    box.append(el("div", { class: "cm-codigo" }, el("div", { class: "cm-codigo-topo" }, el("span", { class: "cm-rot" }, "O mesmo modelo no R"), botao), el("pre", {}, el("code", {}, codigo))));

    if (estado.tentados.size >= LIMITE_PESCARIA)
      box.append(el("div", { class: "cm-alerta" },
        el("strong", {}, `Você já montou ${estado.tentados.size} modelos diferentes. `),
        "A cada modelo testado, cresce a chance de achar um valor-p < 0,05 só por acaso. O modelo que você testa deve sair da hipótese, não da pescaria (",
        el("a", { href: "aula-07-teste-hipoteses/index.html" }, "Aula 7"), ")."));
    return box;
  }

  function desenha() {
    const c = CONJUNTOS[estado.chave];
    const seletor = el("select", { class: "form-select cm-select", id: "cm-dados", onchange: (e) => carrega(e.target.value) },
      ...Object.entries(CONJUNTOS).map(([k, v]) => el("option", { value: k, selected: k === estado.chave }, v.rotulo)));
    const naCaixa = new Set([estado.y, ...estado.xs]);
    const pool = estado.vars.filter((v) => !naCaixa.has(v.nome)).map((v) => chip(v, "pool"));
    const poolZona = el("div", {
      class: "cm-pool",
      ondragover: (e) => e.preventDefault(),
      ondrop: (e) => { e.preventDefault(); tira(e.dataTransfer.getData("text/plain")); },
    }, pool.length ? pool : el("span", { class: "cm-vazio" }, "Todas as variáveis estão no modelo."));

    const xsChips = [];
    estado.xs.forEach((n, i) => { if (i) xsChips.push(el("span", { class: "cm-op" }, estado.op)); xsChips.push(chip(varPorNome(n), "x")); });
    const opBotoes = estado.xs.length > 1 ? el("div", { class: "cm-ops", role: "group", "aria-label": "Como combinar os preditores" },
      el("button", { type: "button", class: `cm-opbtn ${estado.op === "+" ? "ativo" : ""}`, "aria-pressed": String(estado.op === "+"), "data-foco": "op+", onclick: () => { estado.op = "+"; desenha(); } }, "+ efeitos somados"),
      el("button", { type: "button", class: `cm-opbtn ${estado.op === "*" ? "ativo" : ""}`, "aria-pressed": String(estado.op === "*"), "data-foco": "op*", onclick: () => { estado.op = "*"; desenha(); } }, "* com interação")) : null;
    const opAjuda = estado.xs.length > 1 ? el("p", { class: "cm-nota" },
      el("code", {}, "A * B"), " é atalho para ", el("code", {}, "A + B + A:B"), ": os dois efeitos e a interação. Outros operadores de fórmula estão em ",
      el("a", { href: "https://ridiculas.wordpress.com/2012/07/23/semantica-para-descrever-modelos/" }, "Zeviani (2012), Ridículas"), ".") : null;

    const hipotese = el("textarea", { class: "form-control cm-hipotese", rows: 2, id: "cm-hip", placeholder: "Ex.: girinos expostos ao predador chegam à metamorfose mais leves." });
    hipotese.value = estado.hipotese || "";
    hipotese.addEventListener("input", (e) => (estado.hipotese = e.target.value));

    const painel = el("div", { class: "cm-painel" },
      el("label", { class: "cm-rot", for: "cm-hip" }, "1. Minha hipótese"), hipotese,
      el("label", { class: "cm-rot", for: "cm-dados" }, "2. Dados"), seletor,
      el("p", { class: "cm-nota" }, c.descricao),
      el("div", { class: "cm-rot" }, "3. Variáveis"),
      poolZona,
      el("div", { class: "cm-legenda-tipos" }, el("span", { class: "cm-chip cm-num cm-mini" }, "num"), " numérica  ", el("span", { class: "cm-chip cm-fator cm-mini" }, "fator"), " categórica"),
      el("div", { class: "cm-rot" }, "4. Monte o modelo"),
      caixa("Resposta (y)", "arraste uma variável numérica", "y", estado.y ? [chip(varPorNome(estado.y), "y")] : []),
      el("div", { class: "cm-til" }, "~"),
      caixa("Preditores (x)", "arraste um ou mais preditores, ou deixe vazio", "x", xsChips),
      opBotoes,
      opAjuda,
      estado.y || estado.xs.length ? el("button", { type: "button", class: "btn btn-sm btn-link cm-limpar", onclick: () => { estado.y = null; estado.xs = []; desenha(); } }, "Recomeçar") : null
    );
    const foco = document.activeElement && raiz.contains(document.activeElement)
      ? document.activeElement.closest("[data-foco]")?.getAttribute("data-foco") : null;
    raiz.replaceChildren(el("div", { class: "cm-grade-layout" }, painel, painelResultado()));
    if (foco) raiz.querySelector(`[data-foco="${CSS.escape(foco)}"]`)?.focus();
    anuncia();
  }

  async function carrega(chave, pre = {}) {
    estado.chave = chave;
    estado.y = null; estado.xs = []; estado.op = "+";
    raiz.replaceChildren(el("p", { class: "cm-instrucao" }, "Carregando os dados…"));
    try {
      const resp = await fetch(CONJUNTOS[chave].arquivo);
      if (!resp.ok) throw new Error(resp.status);
      estado.csvTexto = await resp.text();
      estado.linhas = lerCSV(estado.csvTexto);
      estado.vars = descreveVars(estado.linhas, CONJUNTOS[chave].ignorar);
    } catch (e) {
      raiz.replaceChildren(el("div", { class: "cm-alerta" }, "Não consegui carregar os dados. Abra esta página pelo site (não direto do arquivo)."));
      return;
    }
    if (pre.y && varPorNome(pre.y)) estado.y = pre.y;
    if (pre.x) estado.xs = pre.x.filter((n) => varPorNome(n) && n !== estado.y);
    if (pre.op === "*" || pre.op === "+") estado.op = pre.op;
    desenha();
  }

  // Atalhos por URL, para os links das aulas:
  // construa-um-modelo.html?dados=girinos_2x2&y=massa_mg&x=predador,alimento&op=*
  const q = new URLSearchParams(location.search);
  const chave = CONJUNTOS[q.get("dados")] ? q.get("dados") : "girinos_2x2";
  carrega(chave, { y: q.get("y"), x: q.get("x") ? [...new Set(q.get("x").split(","))] : null, op: q.get("op") === " " ? "+" : q.get("op") });
})();
