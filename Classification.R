library(caret)
library(e1071)
library(randomForest)
library(nnet)
library(class)
library(rpart)

# install.packages() in console

dataset = read.csv("data/letters.csv")
# remove index
dataset =  dataset[, -1]
row.names(dataset) <- dataset$FileName
dataset$FileName = NULL
dataset$Letter = as.factor(dataset$Letter)


# percentage of data used for testing
test_percentage = 0.8

# split data
set.seed(123)
train_index <- createDataPartition(dataset$Letter, p = test_percentage, list = FALSE)
train_data = dataset[train_index, ]
test_data = dataset[-train_index, ]

y = dataset$Letter #labels
x = dataset[, -which(names(dataset) == "Letter")]

x_train = x[train_index, ]
y_train = y[train_index]
x_test = x[-train_index, ]
y_test = y[-train_index]

# SupportVectorMachine
svm_model = svm(Letter ~ ., data = train_data, type = "C-classification")
svm_predict = predict(svm_model, newdata = test_data)
svm_acc = sum(svm_predict == test_data$Letter) / length(svm_predict)

svm_acc = format(round(svm_acc, 2), nsmall = 2)
cat("svm_acc:", svm_acc, "\n")

# RandomForest
rf_model = randomForest(Letter ~ ., data = train_data)
rf_predict = predict(rf_model, test_data)
rf_acc = sum(rf_predict == test_data$Letter) / length(rf_predict)
rf_acc = format(round(rf_acc, 2), nsmall = 2)
cat("rf_acc:", rf_acc, "\n")

# NeuralNetwork
nn_model = nnet(Letter ~ ., data = train_data, size = 10, trace = FALSE)
nn_predict = predict(nn_model, test_data, type = "class")
nn_acc = sum(nn_predict == test_data$Letter) / length(nn_predict)
nn_acc = format(round(nn_acc, 2), nsmall = 2)
cat("nn_acc:", nn_acc, "\n")

# NaiveBayes
nb_model = naiveBayes(x_train, y_train)
nb_predict = predict(nb_model, x_test)
nb_acc = sum(nb_predict == y_test) / length(y_test)
nb_acc = format(round(nb_acc, 2), nsmall = 2)
cat("nb_acc:", nb_acc, "\n")


# K-NearestNeighbors
knn_model = knn(train = x_train, test = x_test, cl = y_train, k = 10)
knn_acc = format(round(mean(knn_model == y_test), 2), nsmall = 2)
cat("knn_acc:", knn_acc, "\n")

# DecisionTrees
tree_model = rpart(y_train ~ ., data = x_train)
tree_predict = predict(tree_model, newdata = x_test, type = "class")
tree_acc = sum(tree_predict == y_test) / length(y_test)
tree_acc = format(round(tree_acc, 2), nsmall = 2)
cat("tree_acc:", tree_acc, "\n")

# Result
columns = c("Letter", "SVM", "RForest", "NNM", "KNN", "DTree")
df_result = data.frame(matrix(nrow = 1000, ncol = length(columns)))
colnames(df_result) = columns

df_result$Letter = row.names(test_data)
df_result$SVM = svm_predict
df_result$RForest = rf_predict
df_result$NNM = nn_predict
df_result$KNN = knn_model
df_result$DTree = tree_predict

write.csv(df_result, file='data/classification.csv',fileEncoding = "UTF-8")
print("DONE")
