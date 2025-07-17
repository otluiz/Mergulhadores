# Mergulhadores 2.0 – Enxame de Robôs de Limpeza 🧠

Nova versão do projeto usando **Python**, com simulação de múltiplos robôs, mapas industriais e geração de relatórios visuais.

---

## 🗂 Estrutura Atualizada
Mergulhadores/
├── Python/
│ ├── buscaMergulhadores.py
│ ├── util_mapas.py
│ └── gerar_mapas_industriais.py
├── mapas_iniciais/
├── mapas_trajetoria/
├── Imagens/
│ ├── parallel_line.png
│ └── spiral_shell.png
├── documentacao_projeto.tex
├── README.md # Este arquivo
├── README_old.md # Versão anterior em Common Lisp
├── Lisp/
│ ├── main.lisp
│ └── benchmark.lisp
└── ...


---

## 🚀 Visão Geral

Transformamos o projeto original em Lisp numa **plataforma modular em Python**, com os seguintes pilares:

- **Perfis de robôs reais** (Xiaomi, Roborock S7, Gausium Beetle, LionsBot R12 Rex)
- **Mapas industriais com ilhas de trabalho**, gerados com resolução configurável (ex. 0,4 m)
- **Padrões de movimento**: Parallel Line e Spiral Shell
- **Enxame de robôs** com trajetórias simultâneas e gravação de cobertura
- **Exportação automática** de mapas de trajetória (células marcadas como limpas)
- **Documentação** em `.tex` com código, imagens e instruções integradas

---

## ⚙️ Configuração

### 1. Pré-requisitos

- Python 3.7+ com `numpy` e `matplotlib`
- (Opcional) WSL ou Linux para compilar documentação `.tex`
- Repositório atualizado (branch `main`)

### 2. Instalando dependências

```bash
git clone https://github.com/otluiz/Mergulhadores.git
cd Mergulhadores
python3 -m venv venv
source venv/bin/activate
pip install numpy matplotlib

### 3. Gerando mapas
cd Python
python3 gerar_mapas_industriais.py
# Ex: mapas_iniciais/mapa_30x60_industrial.txt

### 4. Simulação do enxame
python3 buscaMergulhadores.py

Escolha o robô, padrão de movimento e mapa

A simulação exibe um gráfico e salva o mapa de passagem

Resultado: rotas armazenadas em mapas_trajetoria/

### 📊 O que vem por aí
Offset de ilhas para evitar paredes

Comportamentos inteligentes de contorno: Bug, espiral e retomada

Análise comparativa: cobertura, bateria, redundância

Geração de relatórios e gráficos comparativos

### 🤝 Contribuição
❗ Para acompanhar o código antigo em Lisp, confira README_old.md

Envie PRs para novas features com feature/... ou fix/...

Mantenha consistência e qualidade do código/documentação

### 📝 Licença
A parte em Lisp está disponível para referência histórica

Este módulo em Python está licenciado sob MIT (§ veja LICENSE)

