#!/usr/bin/env python3
import os
import numpy as np
import matplotlib.pyplot as plt
from util_mapas import escolher_mapa
from datetime import datetime

# ===== MENU DE ESCOLHA DE ROBÔ =====
def escolher_robo():
    perfis = {
        "1": ("Xiaomi", 0.40, 300),
        "2": ("Roborock S7", 0.35, 540),
        "3": ("Gausium Beetle", 0.79, 1200),
        "4": ("LionsBot R12 Rex", 0.81, 2000)
    }
    print("\nPerfis de robôs disponíveis:")
    for k, v in perfis.items():
        print(f"{k}: {v[0]} (diâmetro {v[1]*100:.0f} cm, bateria ≈{v[2]} passos)")
    escolha = input("Escolha o robô padrão [1]: ") or "1"
    return perfis.get(escolha, perfis["1"])

nome_robo, resolucao, passos_max = escolher_robo()
print(f"Robô selecionado: {nome_robo}, célula = {resolucao} m, passos ≈ {passos_max}")

# ===== CARREGAR MAPA =====
caminho_mapa = escolher_mapa()
def carregar_mapa_txt(caminho):
    with open(caminho) as f:
        linhas = [l.strip() for l in f if l.strip() and not l.startswith("#")]
    ny = len(linhas)
    nx = max(len(l) for l in linhas)
    mapa = np.zeros((nx, ny), int)
    for j, l in enumerate(reversed(linhas)):
        for i, ch in enumerate(l[:nx]):
            if ch in "012": mapa[i, j] = int(ch)
    return mapa

ambiente = carregar_mapa_txt(caminho_mapa)
nx, ny = ambiente.shape
largura = nx * resolucao
altura = ny * resolucao
iniciar_x = altura > largura

# ===== ESCOLHA DE PADRÃO =====
def escolher_padrao():
    op = {"1": "parallel_line", "2": "spiral_shell"}
    print("\nPadrões disponíveis:")
    print("1: Parallel Line\n2: Spiral Shell")
    esc = input("Escolha padrão [1]: ") or "1"
    return op.get(esc, "parallel_line")

padrao = escolher_padrao()
print(f"Padrão selecionado: {padrao.replace('_', ' ').title()}")

# ===== CLASSE ROBO =====
class Robo:
    def __init__(self, id, pos, passo):
        self.id, self.pos = id, np.array(pos, float)
        self.passo, self.estado, self.etapas = passo, "normal", 0
        self.mapa, self.resol = ambiente, resolucao
        self.passos = passos_max
        self.ang = 0
        self.traj = [tuple(self.pos)]

    def idx(self, p=None):
        p = self.pos if p is None else p
        return int(p[0]/self.resol), int(p[1]/self.resol)

    def obst(self, p):
        i, j = self.idx(p)
        return 0 <= i < nx and 0 <= j < ny and self.mapa[i, j] == 1

    def marcar(self):
         i, j = self.idx()
         if 0 <= i < nx and 0 <= j < ny:
             if self.mapa[i, j] == 0:
                 self.mapa[i, j] = 2

    def mover_pl(self):
        if self.estado == "contornando":
            seq = [np.array([self.passo,0]), np.array([0,self.passo]), np.array([-self.passo,0])]
            self.pos += seq[self.etapas]
            self.etapas += 1
            if self.etapas > 2: self.estado, self.etapas = "normal", 0
        else:
            d = np.array([0,self.passo]) if iniciar_x else np.array([self.passo,0])
            p2 = self.pos + d
            if self.obst(p2):
                self.estado = "contornando"; self.etapas = 0
            else:
                self.pos = p2
                if iniciar_x and not (0 <= self.pos[1] <= altura):
                    self.pos += np.array([self.passo+0.2, 0])
                if not iniciar_x and not (0 <= self.pos[0] <= largura):
                    self.pos += np.array([0, self.passo+0.2])

    def mover_sp(self):
        r = self.passos * self.resol / (2*np.pi)
        self.ang += np.pi/8
        d = np.array([r*np.cos(self.ang), r*np.sin(self.ang)])
        p2 = self.pos + d
        if not self.obst(p2): self.pos = p2

    def mover(self):
        if self.passos <= 0: return

        # Executa o padrão selecionado
        if padrao=="parallel_line": self.mover_pl()
        else: self.mover_sp()

        # ➤ Ajuste de posição para não sair do mapa
        self.pos[0] = min(max(self.pos[0], 0), largura)
        self.pos[1] = min(max(self.pos[1], 0), altura)

        # Marca e registra a trajetória
        self.marcar()
        self.traj.append(tuple(self.pos))

        # Decrementa a bateria (passos restantes)
        self.passos -= 1

# ===== INICIALIZAÇÃO & SIMULAÇÃO =====
#n_robos = max(2, int((largura if iniciar_x else altura)//1))
#robos = []
#for k in range(n_robos):
#    if iniciar_x: pos = (k*1.0, 0)
#    else: pos = (0, k*1.0)
#    robos.append(Robo(k, pos, resolucao))
n_robos = max(2, int((largura if iniciar_x else altura) // 1))
robos = []
for k in range(n_robos):
    if iniciar_x:
        x = k * 1.0
        if x + resolucao/2 > largura: break
        pos = (x, 0)
    else:
        y = k * 1.0
        if y + resolucao/2 > altura: break
        pos = (0, y)
    robos.append(Robo(k, pos, resolucao))

for _ in range(passos_max):
    for r in robos: r.mover()

# ===== VISUALIZAÇÃO =====
plt.figure(figsize=(12,8))
cols = {0: "white",1:"red",2:"lightgreen"}
for i in range(nx):
    for j in range(ny):
        plt.gca().add_patch(plt.Rectangle((i*resolucao,j*resolucao),
                                          resolucao,resolucao,
                                          color=cols[ambiente[i,j]],alpha=0.6))
for r in robos:
    traj = np.array(r.traj)
    plt.plot(traj[:,0], traj[:,1], marker="o", label=f"Robô {r.id}")
plt.title(f"{nome_robo} - {padrao.replace('_',' ').title()} - {os.path.basename(caminho_mapa)}")
plt.legend(), plt.grid(), plt.tight_layout(), plt.show()


# ===== EXPORTAR MAPA DE TRAJETÓRIA =====
os.makedirs("mapas_trajetoria", exist_ok=True)
nome = f"trajetoria_{nome_robo}_{padrao}_{nx}x{ny}.txt"
with open(f"mapas_trajetoria/{nome}", "w") as f:
    f.write("# 0=livre 1=obstáculo 2=limpo\n\n")
    for j in reversed(range(ny)):
        linha = ''.join(str(ambiente[i, j]) for i in range(nx))
        f.write(linha + "\n")
print(f"✅ Mapa de trajetória salvo em mapas_trajetoria/{nome}")
