
library(tidyverse)
library(googlesheets4)
library(class)
library(caret)
library(magrittr)




## Business Understanding


## Data Understanding
Three data sets are submitted, for training and testing. Ground-truth occupancy was obtained from time stamped pictures that were taken every minute.


url <- "https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/assignments/master/datasets/KNN-occupancy.csv"
rawDS <- read_csv(url)
str(rawDS)

The dataset has 7 variables (columns) and 8143 observations (rows)

## Data Preparation
###The first variable "date" can be removed from the dataset 

cleanDS1 <- rawDS[-1]
head(cleanDS1)

###The variable named "occupancy" contains the outcomes we are going to predict - '0' for 'not occupied', '1' for 'occupied status.'

cntDiag <- table(cleanDS1$Occupancy)
propDiag <- round(prop.table(cntDiag) * 100 , digits = 1)
cntDiag
propDiag


cleanDS1$Occupancy <- factor(cleanDS1$Occupancy, levels = c("1", "0"), labels = c("occupied status", "not occupied")) %>% relevel("not occupied")
head(cleanDS1, 10)

summary(cleanDS1[c("Temperature", "Humidity", "Light", "CO2")])

normalize <- function(x) { # Function takes in a vector
  return ((x - min(x)) / (max(x) - min(x))) # distance of item value - minimum vector value divided by the range of all vector values
}

cleanDS1_n <- sapply(1:5,
                    function(x) {
                      normalize(cleanDS1[,x])
                    }) %>% as.data.frame()

summary(cleanDS1_n[c("Temperature", "Humidity", "Light", "CO2")])

trainDS_feat <- cleanDS1_n[1:469,  ]
testDS_feat <- cleanDS1_n[470:8143,  ]

trainDS_labels <- cleanDS1[1:469,  1]
testDS_labels <- cleanDS1[470:8143,  1]
```

## Modeling

cleanDS_test_pred <- knn(train = as.matrix(trainDS_feat), test = as.matrix(testDS_feat), cl = as.matrix(trainDS_labels), k = 21)
head(cleanDS_test_pred)

confusionMatrix(cleanDS_test_pred, testDS_labels[[1]], positive = NULL, dnn = c("Prediction", "TRUE"))