input_matrix = read.table('seconda_prova_16_12/quaityparameters-wine/QualityParameterDados.txt',header=TRUE)

barplot(table(input_matrix[1]), main = "Geographical origin of the studied red wines",xlab="Country",ylab="Frequency",ylim=c(0,20), names.arg = c("Argentina (ARG)", "Chile (CHI)", "Australia (AUS)", "South Africa (SOU)"))

plot(input_matrix[,-1],col=c("red","blue","green","yellow")[input_matrix[,1]],main="Relationships between variables")

print(names(input_matrix[2])[1])

par(mfrow=c(1,ncol(input_matrix)),main="Quantiles per Variable")
for (n in 1:ncol(input_matrix)){
  boxplot(x=input_matrix[,n],data=input_matrix,xlab=names(input_matrix[,n])) 
}

