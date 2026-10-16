# Converte o path de um SVG do potrace (PhyloPic) em polígonos (x, y, parte),
# com altura 1 e base em y = 0, centralizado em x = 0.
import re, sys, csv
src, out = sys.argv[1], sys.argv[2]
txt = open(src).read()
ds = re.findall(r'\sd="([^"]*)"', txt)
tok = re.compile(r'[MmLlCcZzHhVv]|-?\d*\.?\d+(?:e-?\d+)?')
polys = []
for d in ds:
    t = tok.findall(d); i = 0; cmd = None; cx = cy = 0; sx = sy = 0; cur = []
    def num():
        global i
        v = float(t[i]); i += 1; return v
    while i < len(t):
        if re.match(r'[A-Za-z]', t[i]): cmd = t[i]; i += 1
        if cmd in 'Zz':
            if cur: polys.append(cur); cur = []
            cx, cy = sx, sy; continue
        if cmd in 'Mm':
            x, y = num(), num()
            if cmd == 'm': x += cx; y += cy
            if cur: polys.append(cur)
            cur = [(x, y)]; cx, cy = sx, sy = x, y
            cmd = 'l' if cmd == 'm' else 'L'
        elif cmd in 'Ll':
            x, y = num(), num()
            if cmd == 'l': x += cx; y += cy
            cur.append((x, y)); cx, cy = x, y
        elif cmd in 'Cc':
            p = [num() for _ in range(6)]
            if cmd == 'c': p = [p[0]+cx, p[1]+cy, p[2]+cx, p[3]+cy, p[4]+cx, p[5]+cy]
            for k in range(1, 5):
                s = k / 4; a = 1 - s
                cur.append((a**3*cx + 3*a*a*s*p[0] + 3*a*s*s*p[2] + s**3*p[4],
                            a**3*cy + 3*a*a*s*p[1] + 3*a*s*s*p[3] + s**3*p[5]))
            cx, cy = p[4], p[5]
        else:
            raise SystemExit("comando não suportado: " + cmd)
    if cur: polys.append(cur)
pts = [q for p in polys for q in p]
x0 = min(p[0] for p in pts); x1 = max(p[0] for p in pts)
y0 = min(p[1] for p in pts); y1 = max(p[1] for p in pts)
h = y1 - y0; xm = (x0 + x1) / 2
with open(out, 'w', newline='') as f:
    w = csv.writer(f); w.writerow(['x', 'y', 'parte'])
    for k, p in enumerate(polys, 1):
        for x, y in p: w.writerow([round((x - xm) / h, 4), round((y - y0) / h, 4), k])
print(out, len(polys), len(pts))
