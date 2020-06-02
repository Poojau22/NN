Forestfire<-read.csv("C:\\Users\\Administrator\\Downloads\\forestfires (1).csv")
View(Forestfire)
#EDA
library(DataExplorer)
str(Forestfire)
library(skimr)
skim(Forestfire)
plot_str(Forestfire)
plot_missing(Forestfire)
plot_histogram(Forestfire)
plot_density(Forestfire)
plot_correlation(Forestfire, type = "continuous", "Review.Date")
plot_bar(Forestfire)
create_report(Forestfire)

#Custom normalization function
forestfire_new<-Forestfire[,-c(1,2,31)]
View(forestfire_new)
normalize<-function(x){return((x-min(x)))/(max(x)-min(x))}

# Apply normalization to entire dataframe:-
Forestfire_norm<-as.data.frame(lapply(forestfire_new, normalize))

#Create training and test data
library(caret)
intraining<-createDataPartition(forestfire_new$area, p=.70, list = F)
trainingforest<-forestfire_new[intraining,]
testingforest<-forestfire_new[-intraining,]

#Training a model on the data:-
#Train the neuralnet model
install.packages("neuralnet")
library(neuralnet)
#Simple ANN with only a single hidden neuron
forestfire_model<-neuralnet(area~FFMC+DMC+DC+ISI+temp+RH+wind+rain, data=trainingforest,linear.output = F)
#Visualize the network topology
windows(); plot(forestfire_model)

## Evaluate model performance 
#obtain model results
model_results<-compute(forestfire_model, testingforest)
# obtain predicted strength values
predicted_strength<-model_results$net.result
# Examine the correlation between predicted & actual values
cor(predicted_strength, testingforest$area)

## Improve model performance
# a more complex neural network topology with 5 hidden neurons
Forestfire_model2<-neuralnet(area~FFMC+DMC+DC+ISI+temp+RH+wind+rain, data = trainingforest, hidden = c(5, 3),linear.output = F)
#plot the network
windows(); plot(Forestfire_model2)
# Evaluate the results as we did before
model_results2<-compute(Forestfire_model2, testingforest)
predicted_strength2<-model_results2$net.result
cor(predicted_strength2, testingforest$area)
