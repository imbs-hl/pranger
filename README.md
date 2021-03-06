## Proximities with ranger
Cesaire J. K. Fouodo

### Introduction
Pranger bases on unsupervised random forests generated with ranger to compute proximities between individuals.Two approaches are supported. The first approach of Shi and Horvath (2006) that increases the dissimilarity between two individuals to one if they don't belong to the same terminal node. The second approach of Fouodo et al. (2021) takes the tree depth into account and estimate the dissimilarity between two individuals basing on the length of the minimal path between the terminal nodes they belong to.

### Installation
Installation from Github:
```R
devtools::install_github("imbs-hl/pranger")
```

CRAN release coming soon.

### Usage
For usage in R, see ?pranger in R. Most importantly, see the Examples section. As a first example you could try 

```R  
pranger(data = iris[ , -5], strategy = "boostrepl")
```

### References
* Shi, T., Hovarth and S. (2006). Unsupervised Learning with Random Forest Predictors. Journal of Computational and Graphical Statistics 15 (1): 118–38.
* Fouodo, C.J.K, Szymczak, S., Wright, N.M. and K\"onig R.I. (2020). The use of tree depth improves unsupervised random forests dissimilarities. xxxx xx(xx): xx-xx.
