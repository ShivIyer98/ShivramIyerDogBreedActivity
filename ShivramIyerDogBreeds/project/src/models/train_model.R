# libraries ---------------------------------------------------------------

library(data.table)
library(ggplot2)
library(Rtsne)
library(ClusterR)

# load data ---------------------------------------------------------------

data <- fread("./project/volume/data/raw/data.csv")

id <- data$id
data$id <- NULL

# principal component analysis --------------------------------------------

pca <- prcomp(data)

### use the unclass() function to get the data in PC space
pca_dt <- data.table(unclass(pca)$x)

# t-SNE -------------------------------------------------------------------

set.seed(2)

### run t-sne on the PCAs
### find tuning parameters that have the the most optimal plots
tsne <- Rtsne(pca_dt, dim=3, perplexity=100, pca=F, check_duplicates=F, pca_center=F, normalize=T)

# see results -------------------------------------------------------------
### grab out the coordinates
tsne_dt <- data.table(tsne$Y)


### plot
### Look for 4 separate clusters
### Most optimal plots have the clearest separation and most even cluster shapes
ggplot(tsne_dt,aes(x=V1,y=V2)) + geom_point()
ggplot(tsne_dt,aes(x=V1,y=V3)) + geom_point()
ggplot(tsne_dt,aes(x=V2,y=V3)) + geom_point()

# GMM ---------------------------------------------------------------------
# use a gaussian mixture model to find optimal k

k_bic <- Optimal_Clusters_GMM(tsne_dt[,.(V1,V2,V3)], max_clusters=10, criterion="BIC")

opt_k <- 4 #since there are 4 breeds
gmm_data <- GMM(tsne_dt[,.(V1,V2,V3)],opt_k)

# prediction --------------------------------------------------------------

### the model gives a log-likelihood for each data point's membership to each cluster
### we need to convert log-likelihood into probability
l_clust <- gmm_data$Log_likelihood^(-10)
l_clust <- data.table(l_clust)
prob <- l_clust/rowSums(l_clust)

### We need to see the probabilities in order to correctly allocate each breed column
prob


submit <- data.table(id=id, breed_1=prob$V4, breed_2=prob$V1, breed_3=prob$V3, breed_4=prob$V2)
fwrite(submit,"./project/volume/data/processed/submit.csv")
