library(NbClust)

# 1 - PCA
#input_matrix = read.table('C:/Users/Alexa/OneDrive - Universidade de Aveiro/Desktop/Mestrado/EM/Assignment2/quaityparameters-wine/pca-data.txt',header=TRUE)
input_matrix = read.table('./pca-data.txt',header=TRUE)
#plot(input_matrix[,-1],col=c("red","blue","green","yellow")[input_matrix[,1]])


barplot(table(input_matrix[1]), main = "Geographical origin of the studied red wines",xlab="Country",ylab="Frequency",ylim=c(0,20), names.arg = c("Argentina", "Chile", "Australia", "South Africa"))


#calculate correlation matrix
pearsons_cor_matrix = cor(input_matrix, method = c("pearson"))

ev = eigen(pearsons_cor_matrix)
eigen_vectors = ev$vectors
eigen_values = ev$values

#print(eigen_vectors)
#print(eigen_values)

total_variance = sum(eigen_values)
threshold = total_variance*0.7

pcs = prcomp(input_matrix, scale=TRUE)

#select the number of components to reach the threshold
n = 0
for (element in 1:length(eigen_values)){
  if (sum(eigen_values[1:element]) >= threshold){
    n = element
    break
  }
}

new_dataset = pcs$x
print("principal components selected")
print(new_dataset[,1:n])

#calculate the correlation between an original variable and 
# a principal component
#sum = 0

covariance_matrix = cov(input_matrix)
variances = diag(covariance_matrix)

#a data.frame perhaps is better
cors_PCS = matrix(ncol=4, nrow=16)

for (pc in 1:n){
  for (original_var in 1:length(input_matrix) ){

   # print(paste("corr between pc ", pc, " and original var", original_var))

    loading = new_dataset[original_var, pc]
    variance_original_var = variances[original_var]
    eigen_value = eigen_values[pc]

    cor_PCS = (loading/sqrt(variance_original_var)) * sqrt(eigen_value) 

    cors_PCS[original_var, pc] = (loading/sqrt(variance_original_var)) * sqrt(eigen_value) 
    #print(cor_PCS)
  }
}

# notice the 11th row
print(format(cors_PCS, digits=4, scientific=FALSE))
