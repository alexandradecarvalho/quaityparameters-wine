# Library required for fviz_cluster function
install.packages("factoextra")
library(factoextra)

# Installing required package
install.packages("dplyr")

# Loading the package
library(dplyr)



# Loading dataset
df <- read.delim('C:/Users/Alexa/OneDrive - Universidade de Aveiro/Desktop/Mestrado/EM/Assignment2/quaityparameters-wine/QualityParameterDados.txt')[-1]


# Apply PCA using prcomp function
# Need to scale / Normalize as 
# PCA depends on distance measure
my_pca <- prcomp(df, scale = TRUE, 
                 center = TRUE, retx = T)
names(my_pca)

# Summary
summary(my_pca)
my_pca

# View the principal component loading
# my_pca$rotation[1:5, 1:4]
my_pca$rotation

# See the principal components
dim(my_pca$x)
my_pca$x

# Plotting the resultant principal components
# The parameter scale = 0 ensures that arrows
# are scaled to represent the loadings 
biplot(my_pca, main = "Biplot", scale = 0)

# Compute standard deviation
my_pca$sdev

# Compute variance
my_pca.var <- my_pca$sdev ^ 2
my_pca.var

# Proportion of variance for a scree plot
propve <- my_pca.var / sum(my_pca.var)
propve

# Plot variance explained for each principal component
plot(propve, xlab = "principal component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b", 
     main = "Scree Plot")

# Plot the cumulative proportion of variance explained
plot(cumsum(propve), 
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Find Top n principal component 
# which will atleast cover 90 % variance of dimension
which(cumsum(propve) >= 0.9)[1]


# This part doesn't work because of the disp
# Predict mpg using first 5 new Principal Components
# Add a training set with principal components
train.data <- data.frame(disp = df$disp, my_pca$x[, 1:4])

print(train.data)

# Running a Decision tree algporithm
## Installing and loading packages
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

rpart.model <- rpart(disp ~ ., 
                     data = train.data, method = "anova")

rpart.plot(rpart.model)

#clustering

pca_transform = as.data.frame(-my_pca$x[,1:5])

fviz_nbclust(pca_transform, kmeans, method = 'wss')

fviz_nbclust(pca_transform, kmeans, method = 'silhouette')
fviz_nbclust(pca_transform, kmeans, method = 'gap_stat')

k = 7
kmeans_pca = kmeans(pca_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_pca, data = pca_transform)


