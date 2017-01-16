library(psych)
library(ggplot2)
library(data.table)
setwd('/Users/ethen/Desktop/northwestern/winter/MSIA 421 Data Mining/hw2')

# ----------------------------------------------------------------------------
# problem 1
gene <- fread('gene.csv')

# a. run pca on the data and compute the fraction of
# variance explained by the first two principal components
# remove the id and the response column prior to fitting pca
gene_features <- gene[ , c(-1, -ncol(gene)), with = FALSE ]
pca <- prcomp(gene_features, scale = TRUE)
var_explained_ratio <- pca$sdev ^ 2 / ncol(gene_features)
sum(var_explained_ratio[1:2])

# b. generate a scatter plot using the first and second principal component
gene_pca <- data.table(pca$x[, 1:2])
gene_pca[ , sick := as.factor(gene$sick) ]
ggplot( gene_pca, aes(PC1, PC2, color = sick) ) +
geom_point() + theme_bw()
# the data points are well separated by the first principal component


# ----------------------------------------------------------------------------
# problem 2
news2 <- fread('news2.csv')

# a. use prcomp to find the principal component analysis,
# plot a scree plot and comment where the elbow is
news2[ , V1 := NULL ]
pca1 <- prcomp(news2, scale = TRUE)

# scree plot of the pca (normalized variance explained ratio), the
# scale will be different then doing plot(pca1), since it does not
# normalize the explained variance
var_explained_ratio <- ( pca1$sdev ^ 2 / ncol(gene_features) )[1:10]
visualize_screeplot <- function(var_explained_ratio) {
	var_dt <- data.table(pca = 1:length(var_explained_ratio), 
						 var = var_explained_ratio)
	scree_plot <- ggplot( var_dt, aes(pca, var) ) +
				  geom_bar(stat = 'identity', fill = 'lightblue') +
				  labs(title = 'Scree Plot', x = 'principal component', 
				  	   y = 'variance explained ratio') +
				  theme_bw()
	return(scree_plot)
}
visualize_screeplot(var_explained_ratio)
# the elbow of the curve is at the fourth principal component

# b. use the psych library to perform principal component analysis
# and interpret the loading vectors (a.k.a rotation, principal components)
pca2 <- psych::principal(news2, rotate = 'none', scores = FALSE, nfactor = 3)
pca2$loadings
# the first principal component is simply a mix of all the features,
# has a heavier weighting on FOX, average weighting of the 
# the second tries to separate people that watches FOX network with all
# the other network and the last one attempts to separate people that watches
# CNN versus MSNB


# ----------------------------------------------------------------------------
# problem 3
# proof question, omitted


# ----------------------------------------------------------------------------
# problem 4

# correlation matrix
R <- matrix(c(
	1, .4919, .2636, .4653, -.2277, .0652,
	.4919, 1, .3127, .3506, -.1917, .2045,
	.2635, .3127, 1, .4108, .0647, .2493,
	.4653, .3506, .4108, 1, -.2249, .2293,
	-.2277, -.1917, .0647, -.2249, 1, -.2144,
	.0652, .2045, .2493, .2293, -.2144, 1
), byrow = TRUE, ncol = 6)

# a. does x5, Walleye appear to group with the other fish
# omitted

# b. perform PCA using only x1-x4,
# and interpret the first two loading vectors
eig_decompose <- eigen(R[1:4, 1:4])
eig_decompose$vectors[, 1:2]
# The first loading vector is simply an average of everything. 
# The second loading vector separates x1 = Bluegill and x2 = Black crappie 
# from x3 = Smallmouth bass and x4 = Largemouth bass.

# c. peform PCA on all six variables,
# and generate a scree plot
eig_decompose <- eigen(R)
plot(eig_decompose$values ~ c(1:6), xlab = 'PC',
	 ylab = 'Eigenvalues', main = 'Prinicipal Components vs. Eigenvalues')
lines(eig_decompose$values ~ c(1:6))

# d. what fraction of variation accounted for
# by the first two PCs in part c
sum(var_explained_ratio[1:2])

# e. interpret the first loading vector from part c
eig_decompose$vector[, 1]
# The first principal component is contrasting Walleye with all the other fish.

# ----------------------------------------------------------------------------
# problem 5

# a. compute X^T X and X X^T
X <- matrix(c(1:4, 1, 4, 9, 16), nrow = 4)
tXX <- t(X) %*% X
tXX
XXt <- X %*% t(X)
XXt

# b., c., d. compute the eigen value, eigen vectors of the 
# two matrices above and comment on the eigen values
eigen(tXX)
eigen(XXt)
# the first two eigenvalues are essentially identical


