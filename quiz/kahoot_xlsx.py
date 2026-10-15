"""Preenche o modelo oficial de importação do Kahoot com as perguntas de
quiz/aNN.json (as mesmas do "Teste-se" do site) e grava
quiz/kahoot/aNN-kahoot.xlsx.

Uso, da raiz do repositório:
    python3 quiz/kahoot_xlsx.py caminho/do/modelo-kahoot.xlsx

O modelo se baixa em
https://support.kahoot.com/hc/pt-br/articles/115002812547 ("Baixe o
modelo oficial"). As perguntas vão nas linhas 9 em diante, colunas B a H
(pergunta, quatro alternativas, tempo e resposta correta), por cima da
pergunta de exemplo. Só usa a biblioteca padrão.
"""
import glob
import json
import os
import re
import sys
import zipfile
from xml.sax.saxutils import escape

# A importação do Kahoot não aceita caracteres especiais: letras gregas e
# símbolos viram texto (o site continua com os símbolos).
TROCAS = [("−∞", "-infinito"), ("+∞", "+infinito"), ("−", "-"),
          ("≠", "diferente de"), ("βx", "beta*x"), ("α", "alfa"),
          ("β", "beta"), ("ε", "erro"), ("μ", "mi"), ("≤", "<="), ("²", "2")]


def sem_simbolos(x):
    for a, b in TROCAS:
        x = x.replace(a, b)
    return x


def celula_texto(ref, estilo, texto):
    return (f'<c r="{ref}" s="{estilo}" t="inlineStr"><is><t xml:space="preserve">'
            f'{escape(texto)}</t></is></c>')


def preenche(planilha, perguntas):
    for i, q in enumerate(perguntas):
        r = 9 + i
        cels = [f'<c r="A{r}" s="20"><v>{i + 1}</v></c>',
                celula_texto(f"B{r}", 24, sem_simbolos(q["pergunta"]))]
        for col, alt in zip("CDEF", q["alternativas"]):
            cels.append(celula_texto(f"{col}{r}", 24, sem_simbolos(alt)))
        cels.append(f'<c r="G{r}" s="24"><v>{q["tempo"]}</v></c>')
        cels.append(celula_texto(f"H{r}", 26, str(q["correta"])))
        cels.append(f'<c r="I{r}" s="1"/>')
        linha = f'<row r="{r}" ht="30.0" customHeight="1">{"".join(cels)}</row>'
        planilha, n = re.subn(rf'<row r="{r}"[ >].*?</row>', linha, planilha, count=1, flags=re.S)
        assert n == 1, f"linha {r} não encontrada no modelo"
    return planilha


def main(modelo):
    for arq in sorted(glob.glob("quiz/a*.json")):
        perguntas = json.load(open(arq, encoding="utf-8"))
        for q in perguntas:
            assert len(sem_simbolos(q["pergunta"])) <= 120, q["pergunta"]
            assert all(len(sem_simbolos(a)) <= 75 for a in q["alternativas"])
        saida = os.path.join("quiz/kahoot", os.path.basename(arq).replace(".json", "-kahoot.xlsx"))
        with zipfile.ZipFile(modelo) as zin, zipfile.ZipFile(saida, "w", zipfile.ZIP_DEFLATED) as zout:
            for item in zin.infolist():
                dados = zin.read(item.filename)
                if item.filename == "xl/worksheets/sheet1.xml":
                    dados = preenche(dados.decode("utf-8"), perguntas).encode("utf-8")
                zout.writestr(item, dados)
        print("ok", saida)


if __name__ == "__main__":
    main(sys.argv[1])
