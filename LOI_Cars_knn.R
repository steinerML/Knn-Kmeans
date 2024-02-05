# Set working directory 
setwd("C:\\LOI CSV")
getwd() #Directory set OK!

#Load dataset
cars <- read.csv('auto.csv', stringsAsFactors = FALSE)

#Print Structure and Summary (Records, variables, max, min, mean, etc.)
str(cars)
summary(cars)
#Data preparation: Determine the 'label'variable

#Refactor feature from "USA"/ "ANDERS" to 'Amerikaanse'or 'niet-Amerikaanse'
cars$herkomst <- factor(cars$herkomst, levels = c("usa", "anders"), labels = c("Amerikaanse", "niet-Amerikaanse"))

#Check that labels have been refactored correctly.
prop.table(table(cars$herkomst))

#Round results and convert to percentages
round(prop.table(table(cars$herkomst))*100, digits=1)

#Normalize all dataset except the 'label'!

#Define function
normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}
#Test function
normalize(c(1,2,3,4))

#Apply normalization to dataset
cars_n <- as.data.frame(lapply(cars[2:10], normalize))
summary(cars_n)
#Normalization done! 

#Set seed for random sampling. Random integer used!
set.seed(01234)

#Number of records in dataset (74)
records <- nrow(cars_n)
str(records)

#Calculate 80% of dataset for training dataset (59)
numb_train <- as.integer(round(0.8 * records))
str(numb_train) #Check numb is correct

#Create a random vector using 80% of rows from original dataset.
train_sample <- sample(records, numb_train)
str(train_sample)

#Split dataset into training dataset and test dataset
cars_train <- cars_n[train_sample, ]
cars_test <- cars_n[-train_sample, ]

#Labels for training and test dataset
cars_train_labels <- cars[train_sample, 1]
cars_test_labels <- cars[-train_sample, 1]
length(cars_train_labels) #Check length training dataset labels!
length(cars_test_labels) #Check length test dataset labels!

install.packages("class")
library(class)
k1 <- round(sqrt(nrow(cars_n)))
str(k1) #sqrt(74) ~ 9 neighbours.

#Train the model
knn_pred <- knn(train = cars_train, test = cars_test, cl = cars_train_labels, k1)
str(knn_pred)
summary(knn_pred)

install.packages("gmodels")
library(gmodels)
CrossTable(x = cars_test_labels , y = knn_pred , chisq=TRUE, expected = TRUE, format = 'SPSS')
library(caret)
library(ggplot2)
confusionMatrix(table(knn_pred,cars_test_labels))

#We try to improve the model by means of Standardization (Z-score)
cars_z <- as.data.frame(scale(cars[2:10]))
summary(cars_z) #Dataset has been Standardized

knn_train_z <- cars_z[train_sample,]
knn_test_z <- cars_z[-train_sample,]
length(knn_test_z)
knn_pred_z <- knn(train = knn_train_z, test = knn_test_z, cl = cars_train_labels, k1)
CrossTable(x = cars_test_labels , y = knn_pred_z , chisq=TRUE, expected = TRUE, format = 'SPSS')
confusionMatrix(table(knn_pred_z, cars_test_labels)) #Accuracy still 86%! :C

# Create empty vectors to store the accuracy values and k values
accuracy_values <- c()
k_values <- c()

# Loop through different values of k
for (k in 1:30) {
  # Train the kNN algorithm using the training dataset
  knn_model <- knn(train = cars_train, test = cars_test, cl = cars_train_labels, k = k)
  # Calculate the accuracy of the kNN algorithm
  accuracy <- sum(knn_model == cars_test_labels) / length(cars_test_labels)
  # Store the accuracy value and k value
  accuracy_values <- c(accuracy_values, accuracy)
  k_values <- c(k_values, k)
}

# Create a data frame with the accuracy values and k values
accuracy_df <- data.frame(accuracy = accuracy_values, k = k_values)

# Find the maximum and minimum accuracy values
max_accuracy <- max(accuracy_df$accuracy)
min_accuracy <- min(accuracy_df$accuracy)

# Create a plot using ggplot2
plot <- ggplot(accuracy_df, aes(x = k, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(x = "k", y = "Accuracy") +
  ggtitle("Accuracy of kNN algorithm for different values of k")
# Add labels for maximum and minimum accuracy values
plot <- plot +
  geom_label(aes(label = paste0("Max Accuracy: ", round(max_accuracy, 2)), x = k[which.max(accuracy)], y = max_accuracy), 
             color = "red", vjust = -1) +
  geom_label(aes(label = paste0("Min Accuracy: ", round(min_accuracy, 2)), x = k[which.min(accuracy)], y = min_accuracy), 
             color = "blue", vjust = 1)
plot
dev.copy(png,'accuracy_vs_k_values_knn.png') #We save the plot to working directory!
dev.off()