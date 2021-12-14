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


loading_analysis = function(input_matrix, pcs,n){
  loadings = pcs$rotation
  loadings = loadings[nrow(loadings),1:n]
  
  country_variance = diag(cov(input_matrix))
  country_variance = country_variance[length(country_variance)]
  
  var_pcs = diag(cov(pcs$x[,1:n]))
  
  results = vector("list",n)
  for(pc in 1:n){
    value = (loadings[pc]/country_variance) * var_pcs[pc]
    results[[pc]] = value
  }
  
  return(var_pcs)
}


clustering = function(my_pca){
  pca_transform = as.data.frame(-my_pca)

  fviz_nbclust(pca_transform, kmeans, method = 'wss')

  fviz_nbclust(pca_transform, kmeans, method = 'silhouette')
  fviz_nbclust(pca_transform, kmeans, method = 'gap_stat')

  k = 7
  kmeans_pca = kmeans(pca_transform, centers = k, nstart = 50)
  fviz_cluster(kmeans_pca, data = pca_transform)
}



wine_data = read.table('C:/Users/Alexa/OneDrive - Universidade de Aveiro/Desktop/Mestrado/EM/Assignment2/quaityparameters-wine/QualityParameterDados.txt',header=TRUE)[-1]

pcs = get_pcs(wine_data)
print(summary(pcs))

n = optimal_n(wine_data)
principal_components = pearsons_pca(wine_data,n)

bidimentional_pca = pearsons_pca(wine_data,2) 
#biplot(pcs, main = "Biplot", scale = 0)

variable_impact = loading_analysis(wine_data,pcs,n)
print(variable_impact)

#clustering(principal_components)