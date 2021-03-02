## Proximities with ranger
Cesaire J. K. Fouodo

### Introduction
This package computes proximities between observations using unsupervised random forests (URFs). Only the deep distance based approach of Fouodo et al. (2021) is actually implemented.

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
* Fouodo, C.J.K, Szymczak, S., Wright, N.M. and Koenig R.I. (2020). The use of tree depth improves unsupervised random forests dissimilarities. Arxiv, xx-xx.
