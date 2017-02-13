##############################################
#Programming Assignment
#Last Name: Shah
#First Name: Sargam
#UTA Id: 1001275800
##############################################



# Decision Tree and Linear Regression

#Reading Skills dataset is used

library(party)
print(head(readingSkills))
input.dat <- readingSkills[c(1:105),]

png(file = "decision_tree.png")


output.tree <- ctree(
  nativeSpeaker ~ age + shoeSize + score, 
  data = input.dat)

plot(output.tree)
dev.off()

# Testing performed on the decision tree

test <- sample(readingSkills,1)
test <- readingSkills[sample((1:nrow(readingSkills)),1), ]
print(test)
 result <- predict(output.tree,test)
print(result)

#Linear Regression is as follows

x <- readingSkills$age
y <- readingSkills$score
relation <- lm(y~x)
print(relation)


#Test the model 
a <- data.frame(x=10)
result <- predict(relation,a)
print(result)

png(file="linr.png")

# Plot the chart.
plot(y,x,col = "blue",main = "Age and Score Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "age",ylab = "score")


dev.off()


#End of file