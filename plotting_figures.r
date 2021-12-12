input_matrix = read.table('C:/Users/Alexa/OneDrive - Universidade de Aveiro/Desktop/Mestrado/EM/Assignment2/quaityparameters-wine/pca-data.txt',header=TRUE)

barplot(table(input_matrix[1]), main = "Geographical origin of the studied red wines",xlab="Country",ylab="Frequency",ylim=c(0,20), names.arg = c("Argentina", "Chile", "Australia", "South Africa"))

plot(input_matrix[,-1],col=c("red","blue","green","yellow")[input_matrix[,1]])

print(names(input_matrix[2])[1])

par(mfrow=c(1,ncol(input_matrix)))
for (n in 1:ncol(input_matrix)){
  boxplot(x=input_matrix[,n],data=input_matrix,xlab=names(input_matrix[,n])) 
}