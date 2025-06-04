# Algoritmo dos Mergulhadores

Este repositório contém uma implementação em **Common Lisp** do "Algoritmo dos Mergulhadores". O código permite simular o comportamento de mergulhadores que buscam o mínimo global em um espaço de busca, podendo utilizar tabuleiros pré-definidos ou uma função de benchmark.

## Pré-requisitos

- Um interpretador **Common Lisp** (ex.: [SBCL](http://www.sbcl.org/) ou Allegro CL).
- Para a interface gráfica é necessário que **Allegro Common Graphics** esteja disponível.

## Como usar

1. Abra o **REPL** no diretório deste projeto.
2. Carregue o arquivo principal com:
   ```lisp
   (load "main.lisp")
   ```
3. Execute a função principal e siga as perguntas sobre tabuleiro, quantidade de mergulhadores e visualização:
   ```lisp
   (iniciar)
   ```

Os tabuleiros de exemplo se encontram em `Tabuleiros/tabuleiros.txt` e o arquivo `benchmark.lisp` gera a função de benchmark **Rastrigin**.

Para referências adicionais ou sugestões de consulta, verifique o arquivo `LEIA-ME` incluído no repositório.
