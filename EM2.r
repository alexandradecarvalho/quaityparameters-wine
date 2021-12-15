library(NbClust)
#library(factoextra)
library(dplyr)

#consider all NUMERICAL data
# use a relative import . represent the directory that comes from
# the getwd() command
wine_data = read.table('/home/leonardo/Documenti/UNIFI/2021-2022/primoSemestre_Aveiro/estatistica_multivariada/seconda_prova_16_12/quaityparameters-wine/QualityParameterDados.txt',header=TRUE)[-1]

#apply pca
pca = prcomp(wine_data, scale=TRUE, center=TRUE)

optimal_n = function(pca, min_cum_var=0.7){
    #find the cumulative variance
    #that contain at least min_cum_var 
    threshold = sum(pca$sdev)*min_cum_var
    
    for (element in 1:length(pca$sdev)){
        cum_var = sum(pca$sdev[1:element])/sum(pca$sdev)
        if (cum_var >= threshold){
            n = element
            break
        }
    }
    return(n)
}
optimal_n(pca)

loading_analysis = function(input_matrix, pca, n){
  #take the first five elements of the Last row of the loadings of the PCA
  loadings = pca$rotation[15,][1:n]
  #calculate the country standard deviation
  country_SD = sqrt(diag(cov(input_matrix))[15])
  #calculate the standard deviation of the principal
  #components of interest
  pca_SD = pca$sdev[1:n]
  #calculate the correlation of Pearson
  cors = (loadings * pca_SD) / country_SD
  
  return(cors)
}

loading_analysis(wine_data, pca, 5)


## visualize all the graphs togheter
par(mfrow=c(1, 3))


#plot the first TWO PC using pca$x to refer to the PC of the wines, 
# pca$rotation contains, in the columns, the PC of the variables THE QUALITY
#   "É possível sugerir uma redução de dimensionalidade dos dados sem grande perda de
#   informação dos 44 vinhos?"
plot(pca$x[,1], pca$x[,2],xlab="PC1", ylab="PC2", main="plot of wine components")

#### CLUSTERING
## the next two lines creates graphs for visualize the
#wss, Whitin Sum of Squares
#silhouette graphs, they call a ggplot2
#they are used to determine the number of cluster needed 
# visually
fviz_nbclust(pca, kmeans, method = 'wss')
fviz_nbclust(pca, kmeans, method = 'silhouette')
k = 7
clusters = kmeans(pca, centers=k)



