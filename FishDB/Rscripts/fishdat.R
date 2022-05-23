library(cluster)
#Determining Cluster VAL via elbow method WCSS
x = f_trends[,c("Richness","Abundance")]


set.seed(100)
wcss = vector()
for (i in 1:10) wcss[i]=sum(kmeans(x , i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste("Elbow Method"),
     xlab = 'Number of Clusters',
     ylab = 'WCSS')

#Clusplot (Richness and River Level) 
set.seed(100)
kmeans= kmeans(x= x,centers = 3)
cl = kmeans$cluster
print(cl)

clusplot(x , cl,
         lines= 0,
         shade= TRUE,
         color= TRUE,
         labels= 2,
         plotchar= FALSE,
         span= TRUE,
         main = paste('Clusters of Fish Populations'),
         xlab='Richness',
         ylab='Abundance')

#Hierarchical Dendrogram (Euclidean Distance)
dendrogram = hclust(d = dist(x, method ='euclidean')
                    , method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Yearly Population',
     ylab = 'Euclidean Distance')

#PCA for yearly trends // isolating categorical year
ftrends.numeric<-f_trends[,c(2,3,4,5,6)]
ftrends.cat<-f_trends[,"Year"]

#Log Transformation of Numeric Data w/ Histograms
log.ftrends <- log(ftrends.numeric[, 1:5])
ftrends.year <- ftrends.numeric[ , 5]
par(mfrow = c(3, 2))
hist(log.ftrends$Richness , breaks = 20)
hist(log.ftrends$Abundance , breaks = 20)
hist(log.ftrends$Temp , breaks = 20)
hist(log.ftrends$"River Level" , breaks = 20)
hist(log.ftrends$pH , breaks = 20)

#Ftrends PCA 
ftrends.pca <- prcomp(log.ftrends, center = TRUE, scale = TRUE)
print(ftrends.pca)

#Variance Plot for Principal Components
plot(ftrends.pca, type = 'line')
summary(ftrends.pca)

#Covariation Matrix (Pos = same direction corr ,  Neg = inverse corr) 5x5 dimensions of variables
ftrends.mat <- as.matrix(log.ftrends)
ftrends.covmat <- cor(ftrends.mat)

#eigenvectors show direction of principal components. Eigenvalues = represent contained variance
eigen(ftrends.covmat)

#Biplot of Principal Components
par(mfrow=c(1,3))
biplot(ftrends.pca, choice = 1:2, scale = 0)
biplot(ftrends.pca, choices = 3:4, scale = 0)
biplot(ftrends.pca, choices = 4:5, scale = 0)

#Scree Plots for Principal Components, Determining relevant PCs
pcaCharts <- function(x) {
        x.var <- x$sdev ^ 2
        x.pvar <- x.var/sum(x.var)
        print("proportions of variance:")
        print(x.pvar)
        
        par(mfrow=c(2,2))
        plot(x.pvar,xlab="Principal component",
             ylab="Proportion of variance explained",
             ylim=c(0,1), type='b')
        plot(cumsum(x.pvar),xlab="Principal component",
             ylab="Cumulative Proportion of variance explained",
             ylim=c(0,1), type='b')
        screeplot(x)
        screeplot(x,type="l")
        par(mfrow=c(1,1))
}
pcaCharts(ftrends.pca)

#Proportions of Variance determine Principal Component Relevancy. Looks like 1:2 -/3 (Keep 4 to retain 97+%)