library(lubridate)
library(data.table)
setwd('/Users/ethen/Desktop/northwestern/winter/MSIA 421 Data Mining/hw1')
items <- fread('items.csv')
orders <- fread('orders.csv')


# 1. how many unique customers
length( unique(orders$id) )


# 2. Suppose that the current date is 01JUL2013, compute the recency
# (most recent purchase)
# 3. find the monetary value (total amount spent, item price times quantity) 
# per customer
# 4. find frequency (number of orders per customer)
# report the mean and standard deviation of recency, frequency, monetary value
orders[ , orddate := dmy(orddate) ]
rfm <- orders[ , .( recency = max(orddate),
					frequency = length(unique(ordnum)),
					monetary = sum(price * qty) ), by = id ]
rfm[ , recency := as.numeric( dmy('01JUL2013') - recency ) ]
recency  <- rfm[['recency']]
monetary <- rfm[['monetary']]
freqency <- rfm[['frequency']]

compute_mean_and_sd <- function(value) {
	# pass in value to compute the mean and standard deviation,
	# the outputed list will contain the passed in variable
	# as an indication of what value is being computed
	value_name <- deparse( substitute(value) )
	mean_name <- paste('mean', value_name, sep = '_')
	sd_name <- paste('sd', value_name, sep = '_')
	info <- list( mean(value), sd(value) )
	names(info) <- c(mean_name, sd_name)
	return(info)
}
compute_mean_and_sd(recency)
compute_mean_and_sd(monetary)
compute_mean_and_sd(freqency)


# 5. number of unique items that each customer purchased in each category
# report the mean and standard deviation
merged <- merge(items, orders, by = 'sku')
category_counts <- merged[ , .( counts = length( unique(name) ) ), by = .(id, category) ]
counts <- category_counts[['counts']]
compute_mean_and_sd(counts)


# 6. compute the entropy and examine if those with higher entropy
# have more diversity in their reading interest

# compute the entropy and sort them in decreasing order
entropy <- category_counts[ , {
	p <- counts / sum(counts)
	entropy <- -sum( p * log10(p) )
	list(entropy = entropy)
}, by = id ][order(-entropy), ]

# the top and bottom entropy's customer information
category_counts[ id == 4335961, ]
category_counts[ id == 4313828, ]

