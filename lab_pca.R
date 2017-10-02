library(ISLR)

#######################################################################
###                         PCA                                     ###
###                       US arrests                                ###
#######################################################################

data("USArrests")
?USArrests

USArrests
states <- row.names(USArrests)
states

names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

?prcomp
pr_out <- prcomp(USArrests, scale=TRUE) # standardize data because of vastly different means and variances
names(pr_out)
pr_out$center # means before scaling
pr_out$scale # sd before scaling
pr_out$rotation # principal component loadings
# if we multiply X with the rotation matrix, we get the coordinates of the data in the new system 
# (the principal component scores)
# instead of multiplying ourselves, we can get the scores from pr_out$x
dim(pr_out$x) 
pr_out$x # principal component scores

biplot(pr_out, scale=0)

# principal components are unique just up to a sign change, so we could do
pr_out$rotation <- -pr_out$rotation
pr_out$x <- -pr_out$x
biplot(pr_out, scale=0)

# standard deviation of principal components
pr_out$sdev
# variance explained by each principal component
pr_var <- pr_out$sdev^2
pr_var
# proportion of variance explained
pve <- pr_var/sum(pr_var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')


#######################################################################
###                         PCA                                     ###
###                       NCI60                                     ###
#######################################################################

data("NCI60")
?NCI60
nci_labs <- NCI60$labs
nci_data <- NCI60$data
dim(nci_data)
nci_labs[1:4]

table(nci_labs)

pr_out <- prcomp(nci_data, scale=TRUE)
Cols <- function(vec){
    cols <- rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}

# plot principal component score vectors
par(mfrow=c(1,2))
# components 1 and 2
plot(pr_out$x[,1:2], col=Cols(nci_labs), pch=19,xlab="Z1",ylab="Z2")
# components 1 and 3
plot(pr_out$x[,c(1,3)], col=Cols(nci_labs), pch=19,xlab="Z1",ylab="Z3")

# proportion of variance explained
summary(pr_out)
plot(pr_out) # plot shows variance, not proportion

# calculate proportion of variance ourselves
pve <- 100*pr_out$sdev^2/sum(pr_out$sdev^2)
# and plot them
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")

