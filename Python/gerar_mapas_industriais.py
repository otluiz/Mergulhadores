#!/usr/bin/env python3
import os

def criar_mapa_industrial(nome, larg, alt, res, ilha=3.0, cor=2.0):
    nx = int(larg/res)
    ny = int(alt/res)
    mapa = [[0]*nx for _ in range(ny)]
    step = int((ilha+cor)/res)
    wall = int(ilha/res)
    for j in range(0, ny, step):
        for i in range(0, nx, step):
            for dj in range(wall):
                for di in range(wall):
                    if i+di < nx and j+dj < ny:
                        mapa[j+dj][i+di] = 1
    os.makedirs("mapas_iniciais", exist_ok=True)
    with open(f"mapas_iniciais/{nome}.txt","w") as f:
        f.write("# 0=livre,1=ilha,2=limpo\n\n")
        for linha in reversed(mapa):
            f.write("".join(str(c) for c in linha)+"\n")
    print(f"Gerado: mapas_iniciais/{nome}.txt")

if __name__=="__main__":
    #criar_mapa_industrial("mapa_100x200_industrial", 100, 200, 0.4)
    criar_mapa_industrial("mapa_30x60_industrial", 30, 60, 0.4)
