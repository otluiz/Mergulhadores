#!/usr/bin/env python3
import os

def escolher_mapa(diretorio="mapas_iniciais"):
    if not os.path.isdir(diretorio): return None
    arquivos = [f for f in os.listdir(diretorio) if f.endswith(".txt")]
    if not arquivos: return None
    print("\nMapas disponíveis:")
    for i,f in enumerate(arquivos, 1):
        print(f"{i}: {f}")
    escolha = input("Número do mapa desejado: ")
    try: idx = int(escolha)-1
    except: idx = 0
    return os.path.join(diretorio, arquivos[idx])
