tradeshow <- read.csv('tradeshow.csv')
tradeshow <- tradeshow[, 1:3]
tradeshow <- scale(tradeshow)
set.seed(12345)
kmean <- kmeans(tradeshow, 3, iter.max = 100, nstart = 5)

summary.kmeans = function(fit) {
	p = ncol(fit$centers)
	k = nrow(fit$centers)
	n = sum(fit$size)
	sse = sum(fit$withinss)
	xbar = t(fit$centers)%*%fit$size/n
	ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2) 
	print(data.frame(
		n=c(fit$size, n),
		Pct=(round(c(fit$size, n)/n,2)),
		round(rbind(fit$centers, t(xbar)), 2),
		RMSE = round(sqrt(c(fit$withinss/(p*fit$size-1), sse/(p*(n-k)))), 4)
	))
	cat("SSE = ", sse, "; SSB = ", ssb, "\n")
	cat("R-Squared = ", ssb/(ssb+sse), "\n")
	cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
	invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}

# describe each of the three clusters
summary(kmean)
# 1. networkers (they don't buy)
# 2. window shopper (they buy a little more than average, but
# they network less than average) or someone that
# had to go because their boss forced them
# 3. omnivores (people that likes everything)


# gaussian mixtures
library(mclust)
vii <- Mclust(tradeshow, G = 3, modelNames = 'VII')
plot(vii)

# Do the cluster means tell the same story, or are there differences?
summary(kmean)
t(vii$parameters$mean)
# the first cluster in gaussian mixture (gm) is comparable to the
# third cluster in kmeans
# the second cluster in gm is comparable to the second cluster in kmeans
# finally the third cluster in gm is comparable to the first cluster

# comment on the cluster's size
summary(vii)
kmean$size
# VII, gives spherical clusters with "volumes" that differs across clusters
# on the other hand kmeans assumes equal size clusters


# comment on the within cluster standard deviations (RMSE) ??????????? (skip for now)

t(vii$parameters$mean)

# let the model decide the model name
eee <- Mclust(ts1, G = 3)
plot(eee)


# c. Describe in words the
# shape of the class-conditional distributions.
vii_sigma <- vii$parameters$variance$sigma
eee_sigma <- eee$parameters$variance$sigma
# there is some non-negligible variance between 
# social and education

# d. how many variance parameters are estimated in total
# vii estimated 3 parameters
# eee estimated 6 parameters

# e. which of the three solutions do we prefer
summary(kmean)
t(vii$parameters$mean)
t(eee$parameters$mean)


