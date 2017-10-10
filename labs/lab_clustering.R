library(ISLR)

#######################################################################
###                      k-means clustering                         ###
###                       fake data                                 ###
#######################################################################

set.seed(2)

# generate dummy data
# there are 2 real clusters
X <- matrix(rnorm(50*2), ncol=2)
X[1:25,1] <- X[1:25,1]+3
X[1:25,2] <- x[1:25,2]-4

# k = 2
km_out <- kmeans(X,2,nstart=20)
km_out$cluster
plot(X, col=(km_out$cluster+1), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

set.seed(4)
# k = 3
km_out <- kmeans(X,3,nstart=20)
km_out
plot(X, col=(km_out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

set.seed(3)
# kmeans is dependent on initial cluster assignments, so always perform several runs!
km_out <- kmeans(X,3,nstart=1) # just 1 run
km_out$tot.withinss
km_out <- kmeans(X,3,nstart=20) # 20 runs
km_out$tot.withinss



#######################################################################
###                 hierarchical clustering                         ###
###                       fake data                                 ###
#######################################################################

# computes distance between elements of a matrix
?dist
m <- matrix(1:16, nrow = 4, byrow = TRUE)
m
dist(m, diag = TRUE) # euclidean
dist(m, diag = TRUE, method = "manhattan") # l1

?hclust
# complete linkage
hc_complete <- hclust(dist(X), method="complete")

# average linkage
hc_average <- hclust(dist(X), method="average")

# single linkage
hc_single <- hclust(dist(X), method="single")

par(mfrow=c(1,3))
plot(hc_complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc_average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc_single, main="Single Linkage", xlab="", sub="", cex=.9)

# determine cluster labels for a given cut of the tree
?cutree # k = desired number of groups
cutree(hc_complete, 2)
cutree(hc_average, 2)
cutree(hc_single, 2) # one node is a cluster of its own
cutree(hc_single, 4)

# scale before clustering
xsc <- scale(X)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")

# correlation-based distance
# makes sense only with 3 or more features
X <- matrix(rnorm(30*3), ncol=3)
dd <- as.dist(1-cor(t(X)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")



#######################################################################
###                        Hierarchical clustering                  ###
###                       NCI60                                     ###
#######################################################################

data("NCI60")
nci_labs <- NCI60$labs
nci_data <- NCI60$data

# scale
sd_data <- scale(nci_data)
par(mfrow=c(1,3))
data_dist <- dist(sd_data)

# 3 types of linkage
plot(hclust(data_dist), labels=nci_labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data_dist, method="average"), labels=nci_labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data_dist, method="single"), labels=nci_labs,  main="Single Linkage", xlab="", sub="",ylab="")

hc_out <- hclust(dist(sd_data)) # default method = complete
hc_clusters <- cutree(hc_out,4) # cut so we get 4 clusters
table(hc_clusters,nci_labs) # compare to real labels (which in this case we know)

# plot the 4 clusters (cutoff line)
par(mfrow=c(1,1))
plot(hc_out, labels=nci_labs)
abline(h=139, col="red")
hc_out


#######################################################################
###                     k-means                                     ###
###                       NCI60                                     ###
#######################################################################

set.seed(2)
km_out <- kmeans(sd_data, 4, nstart=20) # 4 clusters here too
km_clusters <- km_out$cluster

# compare the cluster assignments
table(km_clusters,hc_clusters)
