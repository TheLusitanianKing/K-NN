# k-NN
Simple k-Nearest Neighbours algorithm for demonstration purposes

## What is this algorithm about?
### Purpose
The k-NN algorithm is mainly used for 2 purposes :
- **k-NN classification** : determine the class of an object by identifying its K nearest neighbours and assigning the most common class among them.
- **k-NN regression** : determine a feature of an object by identifying its K nearest neighbours and assigning the average value of the feature for those neighbours.

### How are distance calculated?
Commonly used for continuous variable is the (Euclidean distance)[https://en.wikipedia.org/wiki/Euclidean_distance]. Here, I first scaled all the features between `[0, 1]` than I used the weighted Euclidean distance.

## Usage
Basically run the script this way: `cabal run classified.csv unclassified.csv 5`

## License
MIT, see (License)[LICENSE]