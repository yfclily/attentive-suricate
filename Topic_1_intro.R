#####################
# Script in class 1 #
#####################

rm(list=ls())# clean all objects in memory
setwd ('D:/Data Lab/Data Vianney/Class/R ecology/Class 2017-2018/data_class') # set up new wd


# Check for update 

if(!require(installr)) {
  install.packages("installr"); require(installr)} 
updateR()

install.packages ("abc") # install the package 'abc'
library ("abc") # load the package 'abc'

### exercice 1a
install.packages('vegan')
library('vegan')
### end exercice

? median
help (median) # help on median function
median # basic use of the function	
?? median # help on all function with median in the description

getwd() # identify your current working directory
?setwd # set up a new working directory

# q() # to quit R

ls() # all objects in memory
rm(list=ls()) # clean all objects in memory
# press CTRL +L to clean all your console

# play with script editor in R and send command 'median' to R
install.packages('rite')
library(rite)
rite() # send command 'median' 
riteout() # send command 'median'. Try CTRL+ENT to send the command

# Using riteout, change your working directory
setwd ('D:/.../')
read.table ("taiwan.txt",header = TRUE,sep="\t", dec=".")
# same as following if we are not changing the working directory
read.table ("D:/.../taiwan.txt",header = T, 	sep="\t", dec=".")
# also same as:
read.table("taiwan.txt", TRUE, "\t",".")
# create and name object
taiwan <- read.table ("taiwan.txt", 	header = TRUE, sep="\t", dec=".")
taiwan

# you can locate the file by hand
dat <- read.table (file.choose (), 	header = T, sep="\t", dec=".")

# you can also use read.delim(), read.csv(), and others functions

#######
# END #
#######


