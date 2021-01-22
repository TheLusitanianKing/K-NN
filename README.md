# k-nearest neighbours
Simple k-nearest neighbours algorithm for demonstration purposes.

## What is this algorithm about?
### Purpose
The k-nn algorithm is mainly used for 2 purposes:
- **k-nn classification**: determine the class of an object by identifying its **k** nearest neighbours and assigning the most common class among them.
- **k-nn regression**: determine a feature of an object by identifying its **k** nearest neighbours and assigning the average value of the feature for those neighbours.

### How are distances calculated?
Commonly used when using continuous variables is the [Euclidean distance](https://en.wikipedia.org/wiki/Euclidean_distance). In my case, I first scaled all the features between `[0, 1]` then used the *weighted* Euclidean distance.

## Usage
Basically run the script this way: `cabal run :k-nn 5` modifying k as you want.

## Classification
### Scenario
I used data from major European football clubs that I mainly collected from [Transfermarkt](https://www.transfermarkt.pt). Each football club has:
- A name (for identifying purposes only),
- A stadium capacity,
- A number of major European trophies,
- A number of major domestic trophies,
- An estimated market value by [Transfermarkt](https://www.transfermarkt.pt) *(as of January 18th, 2021)*,
- And a number of international players.

The idea is, just by looking at those variables: **predict whether a club is playing in Europe or not this season** (it doesn't matter if it is the Champions League or the Europa League).

The learning data will consist of the clubs from: Portugal (Liga Nos), France (Ligue 1), the Netherlands (Eredivisie), England (Premier League), Scotland (Scottish Premiership), Russia (Premier Liga), Germany (Bundesliga) and Spain (La Liga).

For all the Italian clubs from the Serie A, we want to try to predict whether or not they are playing in Europe this season (2020-2021).

It is also good to know that each variable has a **weigh**, for example, I decided that the *stadium capacity* or the *number of international players* of the club isn't as important to determine to probability of the club playing in Europe than its *past European success* and its *estimated market value*. You can change the weighs easily by modifying the input CSV.

### Evaluating
#### Results
| k  | Good predictions |
|----|------------------|
| 3  | 85%              |
| 5  | 85%              |
| 7  | 95%              |
| 9  | 85%              |
| 11 | 90%              |

Football is a pretty unpredictable sport so I think the results are pretty good, although it is also possible to play with the weighs of each variable to try to make it more accurate.

## Regression
### Scenario
For the regression I used the same data, except this time, the idea is about **predicting the stadium capacity** (and **the number of major European trophies won**) of a Italian club from the Serie A knowing the club characterics (based on the same as above + if they are playing in Europe or not).

### Evaluating
I used the [R Squared](https://en.wikipedia.org/wiki/Coefficient_of_determination) to mesure the quality of the k-nn regression.

#### Results with predicting stadium capacity
| k  | R Squared |
|----|-----------|
| 3  | 0.29      |
| 5  | 0.39      |
| 7  | 0.36      |
| 9  | 0.43      |
| 11 | 0.42      |
| 13 | 0.41      |
| 15 | 0.38      |

#### Results with predicting major European trophies won
| k  | R Squared |
|----|-----------|
| 3  | 0.31      |
| 5  | 0.34      |
| 7  | 0.39      |
| 9  | 0.49      |
| 11 | 0.44      |
| 13 | 0.63      |
| 15 | 0.58      |

#### Analysis
Results are actually not very good as both the *stadium capacity* and the *number of major European trophies won* are very hard to predict from the data we have.

## License
See [License](LICENSE)