library(NbClust)

# 1 - building the input matrix
input_matrix = read.table('C:/Users/Alexa/OneDrive - Universidade de Aveiro/Desktop/Mestrado/EM/Assignment2/QualityParameterDados.txt',header=TRUE)
plot(input_matrix[,-1])

# 2 - Pearson correlation = cov/sqrt(Var(a)*Var(b))
pearsons_cor_matrix = cor(input_matrix, method = c("pearson"))

# 3 - Calculate the correlation matrix's eigenvectors and eigenvalues (Eigenvalues = % variance captured)
ev = eigen(pearsons_cor_matrix)

eigen_vectors = ev$vectors
eigen_values = ev$values

print(eigen_values)

# 4 - Select the most important PCs: the number of PCs is at least 70% of cumulative percentage of total variation 
total_variance = sum(eigen_values)
threshold = total_variance*0.7

pcs = prcomp(input_matrix[-1], scale=TRUE)
#pcs = prcomp(input_matrix)

# bidimensional reduction          
n = 2

# 5 - Derive the new data set
new_dataset = pcs$x
print(new_dataset[,1:n])