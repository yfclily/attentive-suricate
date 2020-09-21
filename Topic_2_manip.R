# Topic 2: Data manipulation

rm(list=ls())# clean all objects in memory

library (datasets) # load the dataset package
rm(list=ls()) # remove objects already present
data(iris) # load iris dataset
ls() # the function data import an object 'iris'
iris # print the object
fix(iris) # open a speadsheet on the object
summary(iris) # summarize the object
class(iris) # give the class of the object
str(iris) # examine the structure of the object

## Row and column selection
students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') # inspect the object created
class(students$height)
class(students[,1])
students$height<-as.numeric(students$height) # assigned numerical value to this column
class(students$height) # check the change was made
students[1,] #extract the first row of the table
students[1,1] # element in the first row, first column
students$height[1] # first element in our vector height

## Sub-setting
students$gender=="female" # check which information is for female
f<-students$gender=="female" # create a 'filter' called f
females<-students[f,] # filter our 'students' dataset
females
dim(females) # size of my new data frame
colnames(females) # names of the columns
rownames(females) # names of the rows
rownames(females)<-c('Vanessa', 'Vicky', 'Marianna', 'Joyce', 'Victoria')
females

## Sample
#sample(data, size, replace = FALSE, prob = NULL)
#get source code
# View(sample) OR getAnywhere(sample()) 
nrow(students) # number of rows in student, same as dim(students)[1] 
1:nrow(students) # create a vector from 1 to nrow(students) observation: 10
stuf<-sample(1:nrow(students), 2) # filter randonly 2 students on my 10 students
stuf # my two students
students[stuf,] # apply my filter

## Sorting
ind1<-order (students$height ) # create the vector of order: ind1
rownames(students) # the first ind '1 | height 167' should move at position 5
students [ind1,] # sorting my data with my vector of order, check ind.1 is at position 5 
students[order(students$height),]
ind2<-order(-students$height)
ind3<-order(students$height,decreasing =T)

## Recoding
colors<-ifelse(students$gender=='male', 'blue','red') # if condition is true it will take the value 'blue', if not the value 'red'
students$colors<-ifelse(students$gender=='male','blue','red') # create a new column
students$gender<-ifelse(students$gender=='male', 'blue','red') # replace an existing column
students$height <= 165 # students shorter or equal to 165 cm
students$shoesize < 37 # students with shoes size less than 37
students$height <= 165 & students$shoesize < 37 # students with height and shoes size <37
students$dual.cond<-ifelse(students$height<=165 & students$shoesize<37,'blue','red') # Use with ifelse()
students # only one student follow the two conditions


## Package `dplyr` 
library (dplyr)
selected <- select(iris, Sepal.Length, Sepal.Width, Petal.Length) #To select the following columns
head(selected) # print the beginning of the data set created
selected1 <- select(iris, Sepal.Length:Petal.Length) #To select all columns from Sepal.Length to Petal.Length
head(selected1, 4) #To print first four rows of the data set created                       
selected1 <- select(iris,c(3:5)) #To select columns with numeric indexes
head(selected1)
selected <- select(iris, -Sepal.Length, -Sepal.Width)
head(selected)

### `filter`
filtered <- filter(iris, Species == "setosa" )
head(filtered,3) #To select the first 3 rows with Species as setosa
filtered1 <- filter(iris, Species == "versicolor", Sepal.Width > 3)
tail(filtered1) #To select the last 5 rows with Species as versicolor and Sepal width more than 3

### `mutate`
col1 <- mutate(iris, Greater.Half = Sepal.Width > 0.5 * Sepal.Length)
tail(col1) #To create a column “Greater.Half” which stores TRUE if given condition is TRUE
table(col1$Greater.Half)

### `arrange`
arranged <- arrange(col1, Sepal.Width)
head(arranged) #To arrange Sepal Width in ascending order
arranged <- arrange(col1, desc(Sepal.Width))
head(arranged)#To arrange Sepal Width in descending order

### `summarise`
summarised <- summarise(arranged, Mean.Width = mean(Sepal.Width))
head(summarised)

### `group_by`
gp <- group_by(iris,Species)
mn <- summarise(gp,Mean.Sepal = mean(Sepal.Width))
mn #To find mean sepal width by Species, we use grouping as follows

## Pipe operator

iris %>% filter(Species == "setosa",Sepal.Width > 3.8) #To get rows with the following conditions
iris  %>% group_by(Species) %>% summarise(Mean.Length = mean(Sepal.Length))


## Data object type and structure
#### Numeric
x <- c(1.0, -3.4, 2, 140.1)
mode(x)
typeof(x)
x <- 4
typeof(x)
x <- 4L
typeof(x)

#### Character
x <- c("a", "f", "project", "house value")
typeof(x)
x <- 3
y <- 5.3
x + y
x <- "3"
y <- "5.3"
# Error in x + y: non-numeric argument to binary operator

#### Logical
x <- c(TRUE, FALSE, FALSE, TRUE)
x <- as.logical(c(1,0,0,1))

### Derived data types
#### Factor
a <- c("M", "F", "F", "U", "F", "M", "M", "M", "F", "U")
a.fact <- as.factor(a)
typeof(a)
typeof(a.fact)
a.fact
attributes(a.fact)
levels(a.fact)
factor(a, levels=c("U","F","M"))

#### NA and NULL
x <- c(23, NA, 1.2, 5)
y <- c(23, NULL, 1.2, 5)
mean(x)
mean(y)

### Data structures
#### (Atomic) vectors
x <- c(674 , 4186 , 5308 , 5083 , 6140 , 6381)
x
x[3]
x[c(1,3,4)]
x[2:4]
x[2] <- 0
x
x <- c("all", "b", "olive")
x
x <- c( 1.2, 5, "Rt", "2000")
typeof(x)

#### Matrices and arrays
m <- matrix(runif(9,0,10), nrow = 3, ncol = 3)
m
m <- array(runif(27,0,10), c(3,3,3))
m

#### Data frames
name   <- c("a1", "a2", "b3")
value1 <- c(23, 4, 12)
value2 <- c(1, 45, 5)
dat    <- data.frame(name, value1, value2)
dat
str(dat) # provide structure
attributes(dat) # provide attributes
names(dat) # extract colum names
rownames(dat) # extract row names

#### Lists
A <- data.frame(
  x = c(7.3, 29.4, 29.4, 2.9, 12.3, 7.5, 36.0, 4.8, 18.8, 4.2),
  y = c(5.2, 26.6, 31.2, 2.2, 13.8, 7.8, 35.2, 8.6, 20.3, 1.1) )
B <- c(TRUE, FALSE)
C <- c("apples", "oranges", "round")
lst <- list(A = A, B = B, C = C)
str(lst)
names(lst)
lst$A
lst[[1]]
class(lst[[1]])
lst.notags <- list(A, B, D)
lst.notags
names(lst.notags)
M <- lm( y ~ x, A)
str(M)
names(M)
str(M$qr)
M$qr$rank

### Coercing data
y   <- c("23.8", "6", "100.01","6")
y.c <- as.numeric(y)
y.c
as.integer(y)
numchar <- as.character(y.c)
numchar
numfac <- as.factor(y)
numfac
charfac <- as.factor(y.c)
charfac
as.character() #	Convert to character
as.numeric()  
as.double()	# Convert to double
as.integer()	# Convert to integer
as.factor()	# Convert to factor
as.logical()	#Convert to a Boolean

#######
# END #
#######

