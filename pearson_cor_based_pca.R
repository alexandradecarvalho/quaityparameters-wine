library(NbClust)
library(factoextra)
library(dplyr)

get_pcs = function(input_matrix) {
  return(prcomp(input_matrix, scale=TRUE,center = TRUE))
}

get_eigen_values = function(input_matrix){
  pearsons_cov_matrix = cor(input_matrix)
  eigen_values = eigen(pearsons_cov_matrix)$values

  return(eigen_values)
}

optimal_n = function(input_matrix){
  eigen_values = get_eigen_values(input_matrix)
  threshold = sum(eigen_values)*0.7
  
  for (element in 1:length(eigen_values)){
    if (sum(eigen_values[1:element]) >= threshold){
      n = element
      break
    }
  }
  return(n)
}

pearsons_pca = function(input_matrix,n){
  pcs = get_pcs(input_matrix)

  new_dataset = pcs$x
  
  print("principal components selected")
  print(new_dataset[,1:n])
  
  return(new_dataset[,1:n])
}


loading_analysis = function(input_matrix, pca, n){
  #take the first five elements of the Last row of the loadings of the PCA
  loadings = pca$rotation[15,][1:n]
  #calculate the country standard deviation
  country_SD = sqrt(diag(cov(input_matrix))[15])
  #calculate the standard deviation of the principal
  #components of interest
  pca_SD = pca$sdev[1:n]
  #calculate the correlation of Pearson
  cors = (loadings * pcs_SD) / country_SD
  
  return(cors)
}


clustering = function(my_pca){
  pca_transform = as.data.frame(my_pca)

  fviz_nbclust(pca_transform, kmeans, method = 'wss')

  fviz_nbclust(pca_transform, kmeans, method = 'silhouette')
  fviz_nbclust(pca_transform, kmeans, method = 'gap_stat')

  k = 7
  kmeans_pca = kmeans(pca_transform, centers = k, nstart = 50)
  fviz_cluster(kmeans_pca, data = pca_transform)
}


#consider all NUMERICAL data
# use a relative import . represent the directory that comes from
# the getwd() command
wine_data = read.table('./QualityParameterDados.txt',header=TRUE)[-1]

#apply PCA
pcs = get_pcs(wine_data)
print(summary(pcs))

n = optimal_n(wine_data)
principal_components = pearsons_pca(wine_data,n)

bidimentional_pca = pearsons_pca(wine_data,2) 
#biplot(pcs, main = "Biplot", scale = 0)

variable_impact = loading_analysis(wine_data,pcs,n)
print(variable_impact)

#plot the first TWO PC using pca$x to refer to the PC of the wines, 
# pca$rotation contains, in the columns, the PC of the variables THE QUALITY
#   "É possível sugerir uma redução de dimensionalidade dos dados sem grande perda de
#   informação dos 44 vinhos?"
plot(pca$x[,1], pca$x[,2],xlab="PC1", ylab="PC2", main="plot of wine components")


ClusterPCA = clustering(principal_components)
ClusterData = clustering(wine_data)
