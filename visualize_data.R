# Load data
titanic.data <- read.csv('M:/DataScientist/Kaggle/Titanic/train.csv')
legend_text <- c("0", "1")
# color_type <- c("grey60","grey80")
color_type <- c("red", "green  ")
layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))
# Viz Survived with Fare
survived_with_fare <- table(titanic.data$Survived, 
	cut(titanic.data$Fare, c(0, 20, 40, 60, 80, 100, 200, 300, 400, 500, 600)))
fare_plot <- barplot(survived_with_fare,
	main="a. Grouped By Fare",
	xlab="Cost Fare",
	ylab="Number of Passagers",
	legend=legend_text,
	col=color_type)
# add frequency numbers upon the bars
# text(x=fare_plot, y=survived_with_fare, label=survived_with_fare, pos=2,col="red")
text(fare_plot,survived_with_fare[1,],labels=survived_with_fare[1,],cex=1)
text(fare_plot,colSums(survived_with_fare)-4,labels=survived_with_fare[2,],cex=1)

# Viz Survived with the classes of passagers
# someone in third class with an expensive ticket would be worse off in the accident, 
# but perhaps those more expensive cabins were located close to the iceberg impact site, 
# or further from exit stairs?
survived_with_pclass <- table(titanic.data$Survived, titanic.data$Pclass)
pclass_plot <- barplot(survived_with_pclass,
	main="b. Grouped By Passagers' Classes",
	xlab="Passagers' Classes",
	ylab="Number of Passagers",
	legend=legend_text,
	col=color_type)
text(pclass_plot,survived_with_pclass[1,]-4,labels=survived_with_pclass[1,],pos=2, cex=1.5)
text(pclass_plot,colSums(survived_with_pclass)-4,labels=survived_with_pclass[2,],pos=2, cex=1.5)

# Viz Survived with Age
survived_with_age <- table(titanic.data$Survived, 
	cut(titanic.data$Age, seq(from=0, to=80, by=10)))  # cut() transter continous var to categorical
age_plot <- barplot(survived_with_age, 
	main="c. Grouped By Age", 
	xlab="Age", 
	ylab="Number of Passagers", 
	legend=legend_text,
	col=color_type)
text(age_plot,survived_with_age[1,]-4,labels=survived_with_age[1,],cex=1.5)
text(age_plot,colSums(survived_with_age)-4,labels=survived_with_age[2,],cex=1.5)

# Viz Survived with Gender
survived_with_gender <- table(titanic.data$Survived, titanic.data$Sex)
gender_plot <- barplot(survived_with_gender,
	main="d. Grouped By Gender",
	xlab="Gender", 
	ylab="Number of Passagers", 
	legend = legend_text,
	col=color_type)
text(gender_plot,survived_with_gender[1,]-4,labels=survived_with_gender[1,],cex=1.5)
text(gender_plot,colSums(survived_with_gender)-4,labels=survived_with_gender[2,],cex=1.5)