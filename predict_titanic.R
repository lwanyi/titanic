library(rattle)
library(rpart.plot)
library(rpart)
library(RColorBrewer)
library(party)
library(data.tree)

# Load data
titanic.data <- read.csv('M:/DataScientist/Kaggle/Titanic/train.csv')
titanic.test.data <- read.csv('M:/DataScientist/Kaggle/Titanic/test.csv')

## Ignored columns: PassangerId, Name, Ticket, Cabin
train.dataset <- data.frame(Pclass = titanic.data$Pclass, Sex = titanic.data$Sex, 
                         Age = titanic.data$Age, SibSp = titanic.data$SibSp,
                         Parch = titanic.data$Parch, Fare = titanic.data$Fare, 
                         Embarked = titanic.data$Embarked, Survived = titanic.data$Survived)

## Split train dataset into two part on 8:2 randomly
rows_of_trainset = nrow(train.dataset) # be careful to trainset and train data
size_of_train_data <- floor(0.8 * rows_of_trainset)  # float 2 int
set.seed(0)
index_of_train_data <- sample(seq_len(rows_of_trainset), size = size_of_train_data)
train.set <- train.dataset[index_of_train_data, ]  # to be used to train
test.set <- train.dataset[-index_of_train_data, ]  # to be used to test


# Train data set with models: simple classifier; dicision tree; random forests
size_of_unknown_data <- nrow(titanic.test.data)
# predicted_train_labels <- rep(1, size_of_train_data)
# predicted_test_labels <- rep(1, rows_of_trainset - size_of_train_data)
# predicted_unkonwn_labels <- rep(0, size_of_unknown_data)

format_confusion_matrix <- function(prediction, labels){
	size_of_data <- length(labels)
	# cprint(table(prediction, labels))
	real_pos_rate = length(prediction[labels==1 & prediction == labels]) / size_of_data
	faked_pos_rate = length(prediction[labels==1 & prediction != labels]) / size_of_data
	real_neg_rate = length(prediction[labels==0 & prediction == labels]) / size_of_data
	faked_neg_rate = length(prediction[labels==0 & prediction != labels]) / size_of_data
	
	# Print F1 score
	precision <- real_pos_rate / (real_pos_rate + faked_pos_rate)
	recall <- real_pos_rate / (real_pos_rate + faked_neg_rate)
	print(paste("F1 score is", (2*precision*recall)/(precision+recall)))
	return (matrix(c(real_pos_rate, faked_neg_rate, faked_pos_rate, real_neg_rate), ncol=2, nrow=2))
}

## model 1 : simple gender and age model
# best predicted train & test accuracy is the age of 16

predict_with_threshold_model <- function(dataset, age_to_split=16){
	# Givem dataset with labels and age, 
	# Build a simple classifier --> Split by gender and specified Age
	# Returns: confusion matrix
	# Because of the Captain's demand: 只有妇女儿童上救生艇
	#    			All Passangers
	#				      |
	#        Male ----------------- Female
	#          |                       |
	# Age<X ----- Age>=X       Age<X ----- Age>=X
	#    | 	          |          |             |
	# Survived      Death     Survived      Survived
	# 
	# Confusion Matrix:
	#          Pos Pred |  Neg Pred
	# Real Pos|  真阳率 |   假阴率
	# Real Neg|  假阳率 |   真阴率
	#
	print(paste("split age", age_to_split))
	size_of_data <- nrow(dataset)
	predicted_labels <- rep(0, size_of_data)
	predicted_labels[dataset$Sex == "female" | dataset$Age < age_to_split] <- 1
	accuracy <- sum(predicted_labels == dataset$Survived)/size_of_data
	conf_mat <- format_confusion_matrix(predicted_labels, dataset$Survived)
	return (list(accuracy, conf_mat))
}
### Draw the simple tree
th_tree <- Node$new("All Passangers")
male <- th_tree$AddChild("Male")
boys <- male$AddChild("Age < X")
men <- male$AddChild("Age >= X")
boys$AddChild("Survived")
men$AddChild("Death")
female <- th_tree$AddChild("Female")
girls <- female$AddChild("Age < X")
women <- female$AddChild("Age >= X")
girls$AddChild("Survived")
women$AddChild("Survived")
SetGraphStyle(th_tree, rankdir = "TB")
SetEdgeStyle(th_tree, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(th_tree, style = "filled,rounded", shape = "box", 
	fillcolor = "GreenYellow", fontname = "helvetica", 
	tooltip = GetDefaultTooltip)
SetNodeStyle(th_tree$Male$`Age >= X`, fillcolor = "Red", penwidth = "3px")
plot(th_tree)

### Compare and optimize
th_results <- c()
diff_nums <- c(10, 11, 12, 13, 14, 14.5, 15, 16, 17, 18)
for (i in diff_nums){
	th_acc <- predict_with_threshold_model(train.dataset, i)[[1]] # Using the whole dataset
	print(paste("With splited by the age of ", i, ", accuracy is ", th_acc))
	th_results <- c(th_results, th_acc)
}
print(predict_with_threshold_model(train.dataset, 10)[[2]])  # Using the age of 10
print(th_results)
plot(diff_nums, th_results, type="b", col="dark red", lwd=2, pch=18,
	main="Accuracy vs. Age",
	xlab="Different Age to Split", ylab="Accuracy")
text(diff_nums, th_results, labels=round(th_results, 3),cex=1, pos=1)


## model 2 : Build decision tree model
build_decision_tree_model <- function(dataset, smallest_leaf=12){
	# Given dataset and smallest_leaf, build a decision tree for classifying
	# params smallest_leaf(int): the number of nodes of smallest leaf
	# Decision tree is the augment version of Simple Gender and Age model, cuz it uses all features
	print(paste("the number of nodes: ", smallest_leaf))
	size_of_data <- nrow(dataset)
	survived.relation <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                                + Fare + Embarked, data = dataset, method = "class",
                              control=rpart.control(minsplit=smallest_leaf, cp=0))
	return (survived.relation)
}
predict_with_decision_tree <- function(model, dataset){
	predicted_labels <- predict(model, dataset, type = "class")
	accuracy <- sum(predicted_labels == dataset$Survived)/nrow(dataset)
	conf_mat <- format_confusion_matrix(predicted_labels, dataset$Survived)
	return (list(accuracy, conf_mat))
}

### Compare and optimize
dt_results <- c()
max_acc <- 0
diff_nums <- 2:20
for (i in diff_nums){  # optimization: 19
	dt_model <- build_decision_tree_model(train.set, i)
	dt_acc <- predict_with_decision_tree(dt_model, test.set)[[1]]  # return mutilple vars
	print(paste("With smallest leaf of ", i, "nodes, accuracy is ", dt_acc))
	dt_results <- c(dt_results, dt_acc)
	if (dt_acc > max_acc){
		max_acc <- dt_acc
		best_model <- dt_model
	}
}
print(predict_with_decision_tree(build_decision_tree_model(train.set, 19), test.set)[[2]])
layout(matrix(c(1,2), 2, 2, byrow=TRUE))
# Visualize tree
print(paste("The Best Performance is ", max_acc))
fancyRpartPlot(best_model, main="Visualize the Decision Tree with Highest Accuracy")
plot(diff_nums, dt_results, type="b", col="dark red", lwd=2, pch=18,
	main="Accuracy vs. Least Nodes",
	xlab="Different Nodes", ylab="Accuracy")
 text(diff_nums, dt_results, labels=round(dt_results, 2), cex=1, pos=1)

## model 3 : Build random forests model
build_random_forests_model <- function(dataset, num_of_trees=100, try_times=3){
	# Given data set, the number of trees and try times, build a random forests
	print(paste("The number of try times: ", try_times))
	set.seed(415)  # pick up a random seed for building RF model
	random_forests_model <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                 data = dataset, 
                 controls=cforest_unbiased(ntree=num_of_trees, mtry=try_times))
	return (random_forests_model)
}

predict_with_random_forests <- function(model, dataset){
	predicted_labels <- predict(model, dataset, OOB=TRUE, type = "response")
	accuracy <- sum(predicted_labels == dataset$Survived)/nrow(dataset)
	conf_mat <- format_confusion_matrix(predicted_labels, dataset$Survived)
	return (list(accuracy, conf_mat))
}
rf_model <- build_random_forests_model(train.set)
rf_acc <- predict_with_random_forests(rf_model, test.set)[[1]]
print(paste("With Random Forests, Accuracyis ", rf_acc))
