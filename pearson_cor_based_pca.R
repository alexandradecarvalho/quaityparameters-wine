library(NbClust)

# 1 - PCA
input_matrix = read.table('C:/Users/Alexa/OneDrive - Universidade de Aveiro/Desktop/Mestrado/EM/Assignment2/quaityparameters-wine/pca-data.txt',header=TRUE)
plot(input_matrix[,-1])

pearsons_cor_matrix = cor(input_matrix, method = c("pearson"))

ev = eigen(pearsons_cor_matrix)
eigen_vectors = ev$vectors
eigen_values = ev$values

print(eigen_vectors)

total_variance = sum(eigen_values)
threshold = total_variance*0.7

pcs =  prcomp(input_matrix, scale=TRUE)

n = 0
for (element in 1:length(eigen_values)){
  if (sum(eigen_values[1:element]) >= threshold){
    n = element
    break
  }
}

new_dataset = pcs$x
print(new_dataset[,1:n])

sum = 0
for (pc in seq(1, n, by=1)){
  print((new_dataset[,pc]/sqrt(var(input_matrix[,1]))) * sqrt(var(new_dataset[,pc])))
}
print(sum)