# Call library
library(sqldf) # Connect SQL
library(dplyr) # Easy to use command
library(mlbench) # Dataset for Example
library(caret) # ML Tools

# My first R Programming
print("Hello World !!")

# Get working directory
getwd()

# Get file in directory
list.files()

# Set Valiable
X <- 1000
Y <- 500
Z <- "Hello"
MyName <- "CHAMP"
XList <- 1:10 #List in R

# Note : in R | TRUE = 1 & FALSE = 0
sum(XList <= 5)
mean(XList <= 3)

# Case count number of genders
Genders <- c("M", "F", "F", "F", "F", "M", "M", "M", "M", "F", "F", "M", "M")
Genders <- factor(Genders) # Note : if want to use ML have to use factor.

sum(Genders == "M")
mean(Genders == "M")
sum(Genders == "F")
mean(Genders == "F")

# Matrix Note(Vector & Dataframe : must use 1 data type | list : can use more than 1 data type.)
(MX <- matrix(1:10, ncol = 5, byrow = TRUE))

# Dataframe
StudnetName <- c("Champ", "Toy", "A")
StudentAge <- c(27, 31, 29)
FoodAllergy <- c(F, T, F)

DFStudent <- data.frame(StudnetName, StudentAge, FoodAllergy)

# Write .CSV files
write.csv(DFStudent, "DFStudent.csv")

# Read .CSV fiels
DFStudentUpdate <- read.csv("DFStudent.csv")
DFStudentUpdate

# Casestudy mtcars data frame
colnames(mtcars) # Example show column name
mtcars[c(1:5, 10), 1:3] # Example sub set data by index : Note before , is row | after , is column.
mtcars[c("Mazda RX4", "Datsun 710"), c("mpg", "cyl")] # Example sub set data by text.
mtcars$hp # Example get data in cloumn
mtcars[mtcars$am == 0, 1:5] # Example sub set by filter data

# Function - Created
FunctionDouble <- function(number) { number * 2 }
FunctionTriple <- function(number) number * 3 # Note : Short command to create function.
FunctionCube <- function(number, exp = 2) { number ** exp } # Note : set parameter = ... is set defult.

# Case function roll 2 dices
FunctionRollDice <- function(DiceNumber = 1) { sum(sample(x = 1:6, size = DiceNumber, replace = TRUE)) }
FunctionRollDice()

# Simple Function 
head(mtcars) # Note : first 6 rows
tail(mtcars) # Note : last 5 rows
str(mtcars) # Note : view structure
summary(mtcars) # Note : view simple statistics

# Statistics Function
mean(mtcars$mpg)
sum(mtcars$mpg)
median(mtcars$mpg) # Note : https://pantip.com/topic/35866580 for dataset have very different min and max value.
min(mtcars$mpg)
max(mtcars$mpg)
sort(mtcars$mpg, decreasing = TRUE)
cor(mtcars$mpg, mtcars$hp) #Note : +, - is Reationship (+ is in same direction) and if nearly 1 it mean strong.
cor(mtcars$mpg, mtcars$wt)
cor(mtcars)
cor(mtcars[c("mpg", "hp", "wt")])

# Feature Scaling (x - min.x/ (max.x - min.x)) - for reduce caculation time and get same result.
FunctionScaling <- function(Value) { (Value - min(Value)) / (max(Value) - min(Value)) }
summary(mtcars$mpg)
summary(FunctionScaling(mtcars$mpg))

# Z Score (x - mean / SD.) - for find data and screen outliner.
ZScoreFunction <- function(Value) { (Value - mean(Value)) / sd(Value) }
summary(mtcars$hp)
summary(ZScoreFunction(mtcars$hp))

# Apply function to all data in dataset.
apply(mtcars, MARGIN = 1, FUN = mean) # Note : MARGIN 1 = by row | 2 = by column
apply(mtcars, MARGIN = 2, FUN = mean)
apply(mtcars, MARGIN = 2, FUN = ZScoreFunction)

# find missing value
x <- c(100, 200, NA)
is.na(x) # TRUE = Missing
is.na(mtcars)
complete.cases(mtcars) # TRUE = Not Missing
na.omit(mtcars) # Clean Missing Value

# SQL by sqldf
sqldf("SELECT * FROM mtcars")

# Get started with dplyr
# Basic
mtcars %>%
  select(1:3, gear, starts_with("a"), ends_with("p"))

# Filter
mtcars %>%
  select(mpg, hp, wt) %>%
  filter(hp <= 100 | wt > 2) %>% # Note : & = and, | = or
  arrange(desc(mpg)) %>%  # Sort by
  mutate(MgDouble = ifelse(mpg > 20, "ECO", "Normal")) # Create new column

# Calculate and Group by
mtcars %>%
  group_by(am, vs) %>%
  summarise(AVGHp = mean(hp),
            SUMHp = sum(hp),
            SDHp = sd(hp))

# Load Dataset form mlbench
data("BostonHousing")

### ML CaseStudy

## CaseStudy PimaIndiansDiabetes

# Explore data
data("PimaIndiansDiabetes")
DSDiabetes <- PimaIndiansDiabetes
str(DSDiabetes) # Check data overview
mean(complete.cases(DSDiabetes)) # Check NA(NULL) Value
table(DSDiabetes$diabetes) / 768 # Check baseline result (768 is all data in dataset)

# Split Data
set.seed(99) # Set seed for data sample not change if run again
id <- sample(1:768, size = 0.8*768, replace = FALSE)
DSTrain <- DSDiabetes[id, ]
DSTest <- DSDiabetes[-id, ] # Note : if use - is remove row from dataset.

# KNN Model
# Train Model (KNN)
# Note : Change train model - CV = K4 Cross Validation | verboseIter = Show traing step
Control <- trainControl(method = "CV", number = 5, verboseIter = TRUE)
# Note : agr = 1.Label Parameter, 2.Calculation Parameter (if . = ALL), 3.Dataset, 4.Model
KNNModel <- train(diabetes ~ . , data = DSTrain, method = "knn", trControl = Control)

# Prediction (KNN)
Result <- predict(KNNModel, newdata = DSTest)
mean(Result == DSTest$diabetes) # Compare result with real data.

# Logistic Regression
# Train Model (Logistic Regression)
Control <- trainControl(method = "CV", number = 5, verboseIter = TRUE)
LRModel <- train(diabetes ~ . , data = DSTrain, method = "glm", trControl = Control)

# Prediction (Logistic Regression)
Result <- predict(LRModel, newdata = DSTest)
mean(Result == DSTest$diabetes)

#Decision Tree
# Train Model (Decision Tree)
Control <- trainControl(method = "CV", number = 10, verboseIter = TRUE)
TreeModel <- train(diabetes ~ . , data = DSTrain, method = "rpart", trControl = Control)

# Prediction (Decision Tree)
Result <- predict(TreeModel, newdata = DSTest)
mean(Result == DSTest$diabetes)

##############

## CaseStudy BostonHousing

# Explore data
data("BostonHousing")
DSBostonHousing <- BostonHousing

# Split Data
id <- sample(1:nrow(DSBostonHousing), size = 0.8*nrow(DSBostonHousing), replace = FALSE)
DSHouseTrain <- DSBostonHousing[id, ]
DSHouseTest <- DSBostonHousing[-id, ] # Note : if use - is remove row from dataset.

# Linear Regression
# Train Model (Linear Regression)
LMModel <- train(medv ~ . , data = DSHouseTrain, method = "lm")

# Prediction (Logistic Regression)
Result <- predict(LMModel, newdata = DSHouseTest)
sqrt(mean((Result - DSHouseTest$medv)**2)) # Root Mean Squared Error

# KNN Model
# Train Model (KNN)
Control <- trainControl(method = "CV", number = 5, verboseIter = TRUE)
KNNModel <- train(medv ~ . , data = DSHouseTrain, method = "knn", trControl = Control)

# Prediction (KNN)
Result2 <- predict(KNNModel, newdata = DSHouseTest)
sqrt(mean((Result2 - DSHouseTest$medv)**2)) # Compare result with real data.