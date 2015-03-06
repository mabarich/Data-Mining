dataset1 = read.csv(file = "C:/Users/Marco/Desktop/R/Pesce.csv", sep=";", header=T)
attach(dataset1)
a1=as.vector(Acciuga)
a1=as.integer(as.numeric(a1))
#Tolgo i negativi e i valori anomali per un risultato più corretto
neg=which(a1==-1, arr.ind=T)
a1=a1[-neg]
a1=a1[-491]
a1=a1[-485]
a1=a1[-480]
a1=a1[-485]
a1=a1[-479]
a1=a1[-478]
a1=a1[-485]
a1=a1[-482]
#Creo un dataframe appostito
pesci=data.frame(matrix(NA, nrow = length(a1)-1, ncol = 2))
#Rinomino i campi (A=anno da prevedere, A1=anno precedente)
names(pesci)=c("A1", "A")
#Riempio il dataframe 
supp=c(1:length(a1))
for (k in 1:length(a1)-1)
{
	pesci$A1[k]=a1[k]
}
for (k in 2:length(a1))
{
	pesci$A[supp[k]-1]=a1[k]
}
#Stima e verifica
acaso=sample(1:length(a1)-1, length(a1)/2)
stima=pesci[acaso,]
verifica=pesci[-acaso,]
#Regressione
library(sm)
val=matrix(NA, 150,1)
for(i in 1:150)
{
h=i/100
sm1=sm.regression(stima$A1, stima$A, h=h, add=F, ngrid=300)
val[i]=(sum(verifica$A-sm1$estimate)^2)
}  
best=which.min(val)/100
sm.regression(pesci$A1,pesci$A, h=best, add=F, ngrid=300)
dev.new()
plot(1:150/100, val)
