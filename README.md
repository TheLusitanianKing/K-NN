:england: [English version/Versão inglesa](README-en.md)
***

# k-vizinhos mais próximos ![Haskell CI](https://github.com/TheLusitanianKing/K-NN/workflows/Haskell%20CI/badge.svg)
Algoritmo dos k-vizinhos mais próximos.

## Em que consiste este algoritmo?
### Finalidades
O algoritmo tem duas finalidades:
- **classificação k-vmp**: determinar a classe de um objecto encontrando os **k** vizinhos mais próximos e dando a classe mais comum entre esses vizinhos.
- **regressão k-vmp**: determinar uma variável de um objecto encontrando os **k** vizinhos mais próximos e dando a média desta variável entre esses vizinhos.

### Como calcular as distâncias entre dois objectos?
Comummente usado quando se tratam de variáveis contínuas é a [distância Euclidiana](https://pt.wikipedia.org/wiki/Distância_euclidiana). No meu caso, redimensionei todas as variáveis entre `[0, 1]` e usei a distância Euclidiana *ponderada*.

## Instruções
Simplesmente `cabal run :k-nn 5`, modificando **k** à vontade, aqui é `5` mas podia ser `7` ou qualquer outro valor.

## Classificação
### Contexto
Usei dados dos maiores clubes europeus de futebol baseando-me no [Transfermarkt](https://www.transfermarkt.pt).
Cada clube tem:
- Um nome (principalmente para identificá-lo),
- Uma capacidade de estádio (em termos de assistência máxima),
- Um número de troféus Europeus (importantes) ganhos,
- Um número de troféus Nacionais (importantes) ganhos,
- Um valor de plantel avaliado por [Transfermarkt](https://www.transfermarkt.pt) *(no dia 18 de janeiro de 2021)*,
- E um número de jogadores internacionais dentro do plantel.

**N.B.**: no que diz respeito à contagem dos troféus, quando escrevo *importantes*, é no sentido que não serão contados troféus considerados poucos relevantes, como por exemplo um campeonato de segunda divisão, uma *Taça dos Clubes Vencedores de Taças*, uma *Taça Latina*, etc.

A ideia é simples, só olhando para estas variáveis: **predizer se um clube está (ou não) à jogar na Europa esta temporada** (não importa se for na Liga dos Campeões ou na Liga Europa).

Os dados de aprendizagem consistarão de clubes de: Portugal (Liga Nos), França (Ligue 1), Países Baixos (Eredivisie), Inglaterra (Premier League), Escócia (Campeonato Escocês), Rússia (Premier Liga), Alemanha (Bundesliga) e Espanha (La Liga).

Para todos os clubes italianos da Serie A, queremos predizer se estão à jogar na Europa esta temporada (ou seja 2020-2021).

É importante notar que todas estas variáveis têm um **peso**, por exemplo, decidi que a *capacidade do estádio* ou o *número de jogadores internacionais no plantel* não é tão importante para determinar a probabilidade de o clube jogar na Europa do que o seu *passado Europeu* e a *avaliação do seu valor de plantel*. Pode mudar esses pesos facilmente modificando o CSV de entrada.

### Avaliação
#### Resultados
| k  | Boas predições |
|----|----------------|
| 3  | 85%            |
| 5  | 85%            |
| 7  | 95%            |
| 9  | 85%            |
| 11 | 90%            |

O futebol é um desportivo bastante imprevisível portanto acredito que os resultados sejam óptimos, embora seja possível experimentar e mudar os pesos de cada variável para obter ainda melhores resultados.

## Regressão
### Contexto
Para a regressão, usei os mesmos dados. A ideia agora é **prever a capacidade do estádio** (e **o número de troféus Europeus ganhos**) de um clube italiano da Serie A sabendo das características do clube.

### Exemplo
Baseado nas características do clube, e com **k=9**, eis umas predições para as capacidades dos estádios:
| Clube                    | Predição | Capacidade real |
|--------------------------|----------|-----------------|
| FC Internazionale Milano | 61 686   | 80 018          |
| SSC Napoli               | 50 995   | 54 726          |
| ACF Fiorentina           | 47 120   | 47 282          |
| FC Crotone               | 16 189   | 16 547          |

Como pode observar, algumas predições são muito erradas, outras são bastante boas.

### Avaliação
Usei o [R-quadrado](https://pt.wikipedia.org/wiki/Coeficiente_de_determinação) para avaliar a qualidade da regressão.
O valor de um R-quadrado é sempre entre -∞ e 1:
- Um R-quadrado de 1 sendo o melhor: significa que as predições são exatamente as mesmas que os valores reais,
- Um R-quadrado de 0 sendo um modelo que vai prever sempre a média de todos os valores,
- E um R-quadrado negativo sendo um modelo que vai prever piores valores do que um R-quadrado de 0.

#### Resultado nas predições das capacidades dos estádios
| k  | R-quadrado |
|----|------------|
| 3  | 0.29       |
| 5  | 0.39       |
| 7  | 0.36       |
| 9  | 0.43       |
| 11 | 0.42       |
| 13 | 0.41       |
| 15 | 0.38       |

#### Resultado nas predições dos troféus Europeus ganhos
| k  | R-quadrado |
|----|------------|
| 3  | 0.31       |
| 5  | 0.34       |
| 7  | 0.39       |
| 9  | 0.49       |
| 11 | 0.44       |
| 13 | 0.63       |
| 15 | 0.58       |

Ambos as *capacidades dos estádios* e os *números de troféus Europeus ganhos* são difíceis de prever a partir dos dados que temos, portanto os resultados não são perfeitos mas é uma boa ilustração do que pode ser feito com regressão k-vmp.

## Licença
Licença MIT, ler [LICENSE](LICENSE) (em inglês).
