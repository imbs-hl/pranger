## Proximities with ranger
Cesaire J. K. Fouodo

### Introduction
Pranger bases on unsupervised random forests (URF) generated with ranger to compute proximities between individuals.Two approaches are supported. The first approach of Shi and Horvath (2006) that increases the dissimilarity between two individuals to one if they don't belong to the same terminal node. The second approach of Fouodo et al. (2021) takes the tree depth into account and estimates the dissimilarity between two individuals basing on the length of the minimal path between the terminal nodes they belong to.

### Installation
Installation from Github:
```R
devtools::install_github("imbs-hl/pranger")
```

CRAN release coming soon.

### Usage
For usage in R, see ?pranger in R. Most importantly, see the Examples section. As a first example you could try 

### An illustrative example with the iris dataset
```R  
library(ggplot2)
```
Using the Shi and Hovarth (2006) URF based method two compute dissimilarities. Please, use the function ```shi_ranger_one_tree``` for parallel computing over the trees. That is, you have to synthesize the two-classes classification problem using ```resampling```, grow the generate the ```ranger``` model and predict the Terminal nodes for the original observations. See ?```shi_ranger_one_tree``` for more details.
 
```R 
## URF dissimilarities with the Shi distance
iris_diss_shi <- pranger(data = iris[ , -5],
                    strategy = "boostrepl", num.trees = 100,
                    min.node.size = 15, approach = "shi", seed = 123)
iris_dist_shi <- pranger::cleandist(iris_diss_shi)
## MDS on the Shi and Hovarth (2006) dissimilarities
mds_iris_shi <- data.frame(cmdscale(iris_dist_shi, k = 2))
mds_iris_shi$class <- iris[ , 5]
names(mds_iris_shi) <- c("PC1", "PC2", "Species")
mds_iris_shi$Method <- "URF Shi"
```
Using the Fouodo (2021) URF based method two compute dissimilarities. Please use the function ```predicted_tree_distance``` for parallel computing for each tree.

```R 
## URF dissimilarities with the deep distance
iris_diss_deep <- pranger(data = iris[ , -5],
                    strategy = "boostrepl", nb_bootst = 5,
                    aggregation = mean, num.trees = 100,
                    min.node.size = 15, approach = "deep", seed = 123)
iris_dist_deep <- pranger::cleandist(iris_diss_deep)
## MDS on the deep dissimilarities
mds_iris_deep <- data.frame(cmdscale(iris_dist_deep, k = 2))
mds_iris_deep$class <- iris[ , 5]
names(mds_iris_deep) <- c("PC1", "PC2", "Species")
mds_iris_deep$Method <- "URF Deep"
```

Using PCA to capture latent effect.

```R 
## PCA
pca_iris <- princomp(iris[ , -5])$scores[ , 1:2]
pca_iris <- data.frame(pca_iris)
pca_iris$class <- iris[ , 5]
names(pca_iris) <- c("PC1", "PC2", "Species")
pca_iris$Method <- "PCA"
```

Visualization the results from the three methods.

```R 
## Plot
mds_iris_dist <- data.frame(rbind(pca_iris, mds_iris_shi, mds_iris_deep))
iris_plot <- ggplot(data = mds_iris_dist,
                            aes(x = PC1, y = PC2,
                            colour = Species)) +
              geom_point() +
              facet_wrap( ~ Method, scales = "free", nrow = 2)
print(iris_plot)
```


### References
* Shi, T., Hovarth and S. (2006). Unsupervised Learning with Random Forest Predictors. Journal of Computational and Graphical Statistics 15 (1): 118–38.
* Fouodo, K.C.J, Szymczak, S., Wright, N.M. and König R.I. (2020). The use of tree depth improves unsupervised random forests dissimilarities. xxxx xx(xx): xx-xx.
