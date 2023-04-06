#Importation de la base de données
mydata <- read.csv("C:/Users/Dhalil/Desktop/Cours/MAG2/R/data_banknote_authentication.txt")

#Présentation des 6 premières lignes de notre dataset
head(mydata)

#Chargement des Packages 
library(foreign)
library(tidyverse)
library(haven)

#Spécification du dataset et analyse préliminaire des différentes variables
attach(mydata)
table(Classe)
summary(mydata[,1:4])

#Chargement du package pour la visualisation
library(ggplot2)

#Transformation de la variable d'interet en facteur 
mydata$Classe<-as.factor(mydata$Classe)

#Chargement du package GGally
library(GGally)

#Analyse descriptive 
ggpairs(mydata, aes(color=Classe))+
  scale_colour_manual(values = c("cornflowerblue", "red"))+
  scale_fill_manual(values=c("cornflowerblue", "red"))

#Echantillonnage aléatoire et propotionnelle
Ind.test = c(sample(1:760,760/4),sample(761:1372,612/4))

#Récupération des echantillons Learn et test 
Learn = mydata[-Ind.test,]
Test = mydata[Ind.test,]
prop.table(table(Test$Classe))
prop.table(table(Learn$Classe))

#Prédiction à partir d'un modèle Logit 
library(nnet)
mod1 = glm(Classe ~ . , family=binomial(link=logit), data=Learn)
summary(mod1)	
prev = predict(mod1,newdata=Test, type="response")

#Matrice de confusion
tab1<-table(prev>0.5,Test$Classe)
tab1

# compute the missclassification error
err.logistic = 100 * mean(prev != Test$Classe)

#Application de l'algorithme de KNN
# Installing Packages
install.packages("e1071")
install.packages("caTools")
install.packages("class")

# Loading package
library(e1071)
library(caTools)
library(class)

#Features Scalling 

Learn_scale <- scale(Learn[, 1:4])
Test_scale <- scale(Test[, 1:4])

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = Learn_scale,
                      test = Test_scale,
                      cl = Learn$Classe,
                      k = 1)
classifier_knn

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != Test$Classe)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = Learn_scale,
                      test = Test_scale,
                      cl = Learn$Classe,
                      k = 1)
misClassError <- mean(classifier_knn != Test$Classe)
print(paste('Accuracy =', 1-misClassError))

# K = 5

classifier_knn <- knn(train = Learn_scale,
                      test = Test_scale,
                      cl = Learn$Classe,
                      k = 5)
misClassError <- mean(classifier_knn != Test$Classe)
print(paste('Accuracy =', 1-misClassError))

# K = 7

classifier_knn <- knn(train = Learn_scale,
                      test = Test_scale,
                      cl = Learn$Classe,
                      k = 7)
misClassError <- mean(classifier_knn != Test$Classe)
print(paste('Accuracy =', 1-misClassError))

# K = 15

classifier_knn <- knn(train = Learn_scale,
                      test = Test_scale,
                      cl = Learn$Classe,
                      k = 15)
misClassError <- mean(classifier_knn != Test$Classe)
print(paste('Accuracy =', 1-misClassError))

# K = 19

classifier_knn <- knn(train = Learn_scale,
                      test = Test_scale,
                      cl = Learn$Classe,
                      k = 19)
misClassError <- mean(classifier_knn != Test$Classe)
print(paste('Accuracy =', 1-misClassError))

#Le modèle a atteint une précision de 99,7% avec k=15.

#Application de l'algorithme de l'Arbre de décision
#Chargement du package
library(rpart.plot)
library(rpart)

# Modèle de construction de l'arbre
fit<-rpart(Classe ~ ., data= Learn, method = 'class')
summary(fit)

# Représentation des differents paramètres de complexité, 
printcp(fit)
plotcp(fit)
fitSample <- prune(fit, cp=0.012)

# Représentation de l'arbre de décision
prp(fitSample, extra=1)

# Par exemple, la feuille la plus à droite classe les billets en vrai, 
# avec 378 individus de cette classe et 28 de la classe "faux".

library(tree)

# Prediction avec le modèle d'arbre de décision estimé

pred=predict(object = fit, newdata = Test, type='class')

# Matrice de confusion relative à l'arbre de décision
mc<- table(pred, Test$Classe)
mc

#Erreur de mauvais classsement
err.resub= 1-((mc[1,1]+mc[2,2])/sum(mc))




