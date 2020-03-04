#Balan Sebastian - 233
#Versin Ionela-Madalina - 233
#Zambitchi Alexandra - 233
#swiss contine 47 de randuri si 6 coloane

datasets::swiss

#Extragem datele in alte variabile pentru lucru

sFertility <- c(swiss$Fertility)
sAgriculture <- c(swiss$Agriculture)
sExamination <- c(swiss$Examination)
sEducation <- c(swiss$Education)
sCatholic <- c(swiss$Catholic)
sInfantM <- c(swiss$Infant.Mortality)

#Ex1 - medie, varianta, quartile, boxplot si interpertari

#medie----

meanFertility <- mean(sFertility)
meanAgriculture <- mean(sAgriculture)
meanExamination <- mean(sExamination)
meanEducation <- mean(sEducation)
meanCatholic <- mean(sCatholic)
meanInfantM <- mean(sInfantM)

#varianta----

varFertility <- var(sFertility)
varAgriculture <- var(sAgriculture)
varExamination <- var(sExamination)
varEducation <- var(sEducation)
varCatholic <- var(sCatholic)
varInfantM <- var(sInfantM)


#quartile Q1,Q2,Q3----
# Q1 - "lower quartile" imparte cele mai mici 25% de celelalte 75%
# Q2 - "median" imparte data set-ul in jumatate
# Q3 - "upper quartile" imparte cele mai mari 25% de celelalte 75%
# Sortam valorile pentru a face quartilele.

sortedFertility <- sort(sFertility)
sortedAgriculture <- sort(sAgriculture)
sortedExamination <- sort(sExamination)
sortedEducation <- sort(sEducation)
sortedCatholic <- sort(sCatholic)
sortedInfantM <- sort(sInfantM)


Q1Fertility <- quantile(sortedFertility, 1 / 4)
Q1Agriculture <- quantile(sortedAgriculture, 1 / 4)
Q1Examination <- quantile(sortedExamination, 1 / 4)
Q1Education <- quantile(sortedEducation, 1 / 4)
Q1Catholic <- quantile(sortedCatholic, 1 / 4)
Q1InfantM <- quantile(sortedInfantM, 1 / 4)


Q2Fertility <- quantile(sortedFertility, 1 / 2)
Q2Agriculture <- quantile(sortedAgriculture, 1 / 2)
Q2Examination <- quantile(sortedExamination, 1 / 2)
Q2Education <- quantile(sortedEducation, 1 / 2)
Q2Catholic <- quantile(sortedCatholic, 1 / 2)
Q2InfantM <- quantile(sortedInfantM, 1 / 2)


Q3Fertility <- quantile(sortedFertility, 3 / 4)
Q3Agriculture <- quantile(sortedAgriculture, 3 / 4)
Q3Examination <- quantile(sortedExamination, 3 / 4)
Q3Education <- quantile(sortedEducation, 3 / 4)
Q3Catholic <- quantile(sortedCatholic, 3 / 4)
Q3InfantM <- quantile(sortedInfantM, 3 / 4)

#boxplot && interpretari----

par(mfrow = c(2, 3))

boxplot(sFertility,main = "Fertility",col = (c("blue")),sub = paste("Outlier rows: ", boxplot.stats(sFertility)$out))
boxplot(sAgriculture,main = "Agriculture",col = (c("blue")),sub = paste("Outlier rows: ", boxplot.stats(sAgriculture)$out))
boxplot(sExamination,main = "Examination",col = (c("blue")),sub = paste("Outlier rows: ", boxplot.stats(sExamination)$out))
boxplot(sEducation,main = "Education",col = (c("blue")),sub = paste("Outlier rows: ", boxplot.stats(sEducation)$out))
boxplot(sCatholic, main = "Catholic",col = (c("blue")),sub = paste("Outlier rows: ", boxplot.stats(sCatholic)$out))
boxplot(sInfantM,main = "Infant Mortality",col = (c("blue")),sub = paste("Outlier rows: ", boxplot.stats(sInfantM)$out))


#Ex2
#2 modele de regreise - simpla si multipla----
par(mfrow = c(1, 2))
#regresie simpla
simpleLinearModelFertilityExamination <- lm(Infant.Mortality ~ Fertility, data = swiss)
AIC(simpleLinearModelFertilityExamination)  #Calculate akaike information criterion
BIC(simpleLinearModelFertilityExamination)

set.seed(100) 
trainingRowIndex <- sample(1:nrow(swiss), 0.8*nrow(swiss))  # row indices for training data
trainingData <- swiss[trainingRowIndex, ]  # model training data
testData  <- swiss[-trainingRowIndex, ]  # test data

lmMod <- lm(Infant.Mortality ~ Fertility, data = trainingData)  # build the model
InfantMPred <- predict(lmMod, testData)  # predict Infant.Mortality
summary (lmMod)  # model summary
actuals_preds <- data.frame(cbind(actuals = testData$Infant.Mortality, predicteds = InfantMPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  

plot(swiss$Infant.Mortality, swiss$Fertility)
b1 <- sum((swiss$Infant.Mortality - mean(swiss$Infant.Mortality))*(swiss$Fertility - mean(swiss$Fertility))) / sum((swiss$Infant.Mortality - mean(swiss$Infant.Mortality)) ^ 2)
a1 <- mean(swiss$Fertility) - b1 * mean(swiss$Infant.Mortality)
abline(a1,b1,col="magenta")


#(regresie multipla)

#(Fertility si Education + Agriculture)
set.seed(100) 
trainingRowIndex <- sample(1:nrow(swiss), 0.8*nrow(swiss))  # row indices for training data
trainingData <- swiss[trainingRowIndex, ]  # model training data
testData  <- swiss[-trainingRowIndex, ]  # test data

multipleLinearModel1 <- lm(Fertility ~ Education + Agriculture, data = trainingData)
print(multipleLinearModel1)
summary(multipleLinearModel1)

#Generam noile date
#Adaugam o variabila
swiss$Urbanization = round(runif(47,1,100),2)
print(swiss)
#Regresie simpla cu noua variabila
LinearModel1 <- lm(Education ~ Urbanization , data = swiss)
plot(swiss$Education ~ swiss$Urbanization )
abline(LinearModel1, col = "magenta")
summary(LinearModel1)

###Exercitiul 3

sizeOfFertilityVector <- length(sFertility)

par(mfrow = c(1, 2))

#(alegem p = 0.5, o probabilitate la intamplare)
p <- (1 / 2)

#(functia de masa)

plot(sFertility,dcauchy(sFertility, location = 0, scale = 1, p),col = "red",main = "Mass Function",xlab = "Fertilitate",ylab = "Probabilitate")

#functia de distributie
plot(sFertility, pcauchy(sFertility,location = 0,scale = 1,lower.tail = TRUE,p),col = "blue",main = "Distribution Function",xlab = "Fertilitate",ylab = "Distributie")
