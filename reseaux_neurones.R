library(neuralnet)
data <- read.csv("data_pic.csv", header = TRUE)
data
#mélanger les data
v<-round(runif(1680,min=0,max=1),digits=2)
#avoir des indices aleatoires
order(v)
data<-data[order(v),]
head(data)

#normaliser entre 0 et 1
normalize <-function(v){
  
  min <- min(v)
  max <- max(v)
  
  v <-(v - min)/(max-min)
  return(v)
  
}

#normaliser les data
dataN <-lapply(data[,c(1:3)],normalize)
dataN <-as.data.frame(dataN)
dataN
#on ajoute la derniere colonne
dataN <-cbind(dataN,data$Classe)
dataN
colnames(dataN) <- c("Reflectivity", "Azimut", "Elevation", "Classe")
head(dataN)

#séparer les data
#1680*70%=1176
v <- sample(1:1680,1176) # 1680 lignes de donnes avec 1176 dans l'apprentissage
data.app <- dataN [ v,] 
data.app
data.test <- dataN [ -v, ]
data.test

# On va ajouter 3 colonnes (binaire) "eau, sol, foret" de type booléean
data.app$eau <-  data.app$Classe == "eau"     # le résultats de la comparaison est true ou false
data.app$sol <- data.app$Classe == "sol"
data.app$foret <-  data.app$Classe == "foret"
data.app
#Supprimer la derniere colonne
data.app$Classe <- NULL
head(data.app)

net.data <- neuralnet(eau + sol + foret ~ Reflectivity + Azimut + Elevation ,data=data.app,hidden=4, threshold=0.04, act.fct="tanh", linear.output=TRUE, stepmax=1e7)
plot(net.data)

#effectuer la prédiction sur l'ensemble de test
#compute permet de retourner neurons et net.result
#-4 pour enlever la colonne classe
prediction <- compute(net.data, data.test[,-4])
prediction 
colnames(prediction$net.result) <- c("eau", "sol","foret")
prediction$net.result 
#prediction$neurons

### recuperer les labeles prédits
#504 length  data.test
labels.predicted <- rep(0,504)
labels.predicted
#whichmax permet de savoir l'indice de la valeur maximale
for (i in 1:504)
  labels.predicted[i]<-names(which.max(prediction$net.result[i,]))
labels.predicted
table(data.test[,4],labels.predicted)
#le taux d'erreur 0

















