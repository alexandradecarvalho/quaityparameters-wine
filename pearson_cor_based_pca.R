library(NbClust)

get_eigen_values = function(input_matrix){
  pearsons_cor_matrix = cor(input_matrix, method = c("pearson"))
  eigen_values = eigen(pearsons_cor_matrix)$values
  
  return(eigen_values)
}

pearsons_pca = function(input_matrix,n=0){
  eigen_values = get_eigen_values(input_matrix)
  threshold = sum(eigen_values)*0.7
  
  pcs =  prcomp(input_matrix, scale=TRUE,center = TRUE)
  
  if(n == 0){
    for (element in 1:length(eigen_values)){
      if (sum(eigen_values[1:element]) >= threshold){
        n = element
        break
      }
    }
  }

  new_dataset = pcs$x

  print("principal components selected")
  print(new_dataset[,1:n])
  
  return(new_dataset[,1:n])
}


loading_analysis = function(input_matrix, new_dataset, eigen_values){
  variances = diag(cov(input_matrix))
  
  #a data.frame perhaps is better
  cors_PCS = matrix(ncol=4, nrow=16)
  
  for (pc in 1:ncol(new_dataset)){
    print(ncol(new_dataset))
    for (original_var in 1:length(input_matrix) ){
      
      print(paste("corr between pc ", pc, " and original var", names(input_matrix[original_var])))
      
      loading = new_dataset[original_var, pc]
      variance_original_var = variances[original_var]
      eigen_values = get_eigen_values(input_matrix)
      eigen_value = eigen_values[pc]
      
      cor_PCS = (loading/sqrt(variance_original_var)) * sqrt(eigen_value) 
      
      cors_PCS[original_var, pc] = (loading/sqrt(variance_original_var)) * sqrt(eigen_value) 
      print(cors_PCS[original_var, pc])
    }
  }
  
  # notice the 11th row
  return(format(cors_PCS, digits=4, scientific=FALSE))
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
my_pca = pearsons_pca(wine_data)
summary(my_pca)

bidimentional_pca = pearsons_pca(wine_data,2) 
biplot(prcomp(wine_data, scale=TRUE,center = TRUE), main = "Biplot", scale = 0)

variable_impact = loading_analysis(wine_data,my_pca)

clustering(my_pca)

