# Topic 1: First steps

rm(list=ls())# clean all objects in memory

# Check for update 
if(!require(installr)) {
  install.packages("installr"); require(installr)} 
updateR()

# Package
install.packages ("abc") # install the package 'abc'
library ("abc") # load the package 'abc'

# Help
? median
help (median) # help on median function
?? median # help on all function with median in the description

# Working directory
getwd() # identify your current working directory
?setwd # set up a new working directory

# Quit R
# q() R # not run

# List 
ls() # all objects in memory
a<-'corals' # create an object 'a' containing 'corals'
b<-'are' # create an object 'b' containing 'are'
c<-'cool'# create an object 'c' containing 'cool'
ls # list objects 'a', 'b', 'c'
rm(list=ls()) # clean all objects in memory
# press CTRL +L to clean all your console

# Calculator
3+2 # addition
3-2 # substraction
3*2 # multiplication
3/2 # division
3^3 # power
log(2) # logarithm
exp(2) # exponential
(5 + 3) / 4 # define priority using () or {} 
pi*4 # common function

# Reading
library(readxl) # I am loading a package (library) allowing me to read .xls file
read_excel('Data/reef_fish.xlsx') # I am reading my file
fish<-read_excel('Data/reef_fish.xlsx') # I am importing my file in an object called 'fish'
fish<-read.table('Data/reef_fish.txt', header=T, sep='\t', dec='.') # reading txt file
?read.table
fish<-read.table ("D:/.../Topic 1/Data/reef_fish.txt",header = TRUE,sep="\t", dec=".")# long version
fish<-read.table("taiwan.txt", TRUE, "\t",".")# you can also shorten some part once you get use to it, but be careful when using this
# fish<-read.table(file.choose (), header = TRUE,sep="\t", dec=".") # not run

# R Studio Environment
fish<-read.table('Data/reef_fish.txt', header=T, sep='\t', dec='.')
barplot(fish$richness, main="Top 10 reef fish Richness (Allen, 2000)", horiz=TRUE, names.arg=fish$country, cex.names=0.5, las=1)

# Random Assignment
library(dplyr)
list_student<-c('a','b','c','d','e')
sample(list_student, 1, replace = TRUE)

#######
# END #
#######


