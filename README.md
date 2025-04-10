# Damas
O projeto consiste de um jogo de damas baseado em terminal, com suporte a PvP e PvCOMP.

# Como executar
Para executar o projeto, você pode usar o docker, caso não tenha instalado o docker no seu computador, pode instalar seguindo [este tutorial](https://docs.docker.com/desktop/).

Uma vez com o docker instalado, basta rodar estes comandos

```bash
docker build . -t prolog:damas              # Constrói a imagem.
docker run -it --rm prolog:damas /bin/sh    # Cria um container a partir da imagem construída, executando o bash.
```

Para rodar o programa, basta rodar o comando

```bash
swipl -s src/main.pl                                    # Abre o interpretador do Prolog e carrega o arquivo main.pl
main.                                                   # Executa o predicado principal.
```
