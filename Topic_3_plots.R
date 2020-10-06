# Topic 3: Plots

rm(list=ls())# clean all objects in memory

## Base plotting environment
### Point and line plots
data(iris)
plot(iris$Petal.Length) # index observation for Petal.Length
plot(iris$Petal.Width) # index observation for Petal.Width
plot(Petal.Length ~ Petal.Width, dat = iris) # pairwise
plot(iris$Petal.Length ~ iris$Petal.Width) # using the $ operator
plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', # add label to x-axis 
     ylab = 'Petal length (cm)', # add label to y-axis 
     main = 'Petal width and length of iris flower') # add title

plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     pch = 19, cex=2, 
     col = rgb (0,0,0,0.10)) # set symbol types and color 

plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     pch = 19, cex=2, 
     col = rgb (1,0,0,0.10))  # set symbol types and color

col.iris<-ifelse(iris$Species=='setosa','purple',ifelse(iris$Species=='versicolor','blue','pink')) # symbols with condition
col.iris

library(scales) # function alpha()

plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     pch = 19, cex=2, 
     col = alpha(col.iris, 0.2)) # set symbol types and color 

legend(x="bottomright", pch= 19, cex=1.5, legend= c("versicolor","setosa", "virginica"), col=levels(as.factor(alpha(col.iris, 0.2))))

plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     pch = 19, cex=2, las=1, # las argument
     col = alpha(col.iris, 0.2)) # set up symbol types and their color 

ratio<-iris$Petal.Length/iris$Sepal.Width  # ratio between the length of petal and the width of Sepal
plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     cex.axis=1.0, cex.lab=1.5, cex.main=1.5,
     pch = 19, las=1, cex= ratio * 2, # incude this ratio in cex, multiply x2 to make it more visual
     col = alpha(col.iris, 0.2))
# looks like Sepal.Width is poorly informative in disriminating species

pairs(iris[1:4], pch=19, col = alpha(col.iris, 0.2))

iris<-iris[order(iris$Petal.Width),] # first order
blossom<-NULL
blossom$year <- 2010:2019                                               # 
blossom$count.alaska <- c(3, 1, 5, 2, 3, 8, 4, 7, 6, 9)
blossom$count.canada <- c(4, 6, 5, 7, 10, 8, 10, 11, 15, 17)
as.data.frame(blossom)
plot(count.alaska ~ year,dat = blossom, type='l',
     ylab = "No. of flower blossoming") 

plot(count.alaska ~ year,dat = blossom, type='b', pch=20,
     ylab = "No. of flower blossoming") 

plot(count.alaska ~ year,dat = blossom, type='b', pch=20,
     lty=2, lwd=0.5, col='red', ylab = "No. of flower blossoming") 

plot(count.alaska ~ year,dat = blossom, type='b', pch=20,
     lty=2, lwd=0.5, col='red', ylab = "No. of flower blossoming") 
lines(count.canada ~ year,dat = blossom, type='b', pch=20,
      lty=3, lwd=0.5, col='blue')

y.rng<-range(c(blossom$count.alaska, blossom$count.canada))

plot(count.alaska ~ year,dat = blossom, type='l', ylim = y.rng,
     lty=2, lwd=1, col='red', ylab = "No. of flower blossoming")

lines(count.canada ~ year,dat = blossom, lty=1, lwd=1, col='blue')

iris.ver<- subset(iris, Species == "versicolor")
iris.vir<- subset(iris, Species == "virginica")

y.rng <- range( c(iris.ver$Petal.Length, iris.vir$Petal.Length) , na.rm = TRUE) 
x.rng <- range( c(iris.ver$Petal.Width, iris.vir$Petal.width) , na.rm = TRUE) 


plot(Petal.Length ~ Petal.Width, dat = iris.ver,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     cex.axis=1.0, cex.lab=1.5, cex.main=1.5, type='n',
     xlim=x.rng,  ylim=y.rng) # Plot an empty plot


points(Petal.Length ~ Petal.Width, dat = iris.ver, pch = 20,cex=2, col = rgb(0,0,1,0.10)) # Add points for versicolor


points(Petal.Length ~ Petal.Width, dat = iris.vir, pch = 20,cex=2, col =  alpha('#fc03c6', 0.2)) # Add points for versicolor


legend("topleft", c("versicolor", "virginica"), pch = 19, cex=1.2, col = c(rgb(0,0,1,0.10), alpha('#fc03c6', 0.2))) # Add legend

### Boxplots
boxplot(iris$Sepal.Width, na.rm = TRUE)
boxplot(iris$Sepal.Width,iris$Sepal.Length, iris$Petal.Width,iris$Petal.Length, names = c("Sepal.Width", "Sepal.Length", "Petal.Length","Petal.Width"), main = "Iris flower traits")
boxplot(iris$Sepal.Width,iris$Sepal.Length, iris$Petal.Width,iris$Petal.Length, names = c("Sepal.Width", "Sepal.Length", "Petal.Length","Petal.Width"), main = "Iris flower traits",outline = FALSE, horizontal = TRUE )
boxplot(Sepal.Width ~ Species,iris)
iris$Species.ord <- reorder(iris$Species,iris$Sepal.Width, median)
levels(iris$Species.ord)
boxplot(Sepal.Width ~ Species.ord, iris)

### Histograms
hist(iris$Sepal.Width, xlab = "Width of Sepal", main = NA)
hist(iris$Sepal.Width, xlab = "Width of Sepal", main = NA, breaks=10)
n <- 10  # Define the number of bin
minx <- min(iris$Sepal.Width, na.rm = TRUE)
maxx <- max(iris$Sepal.Width, na.rm = TRUE)
bins <- seq(minx, maxx, length.out = n +1)
hist(iris$Sepal.Width, xlab = "Width of Sepal", main = NA, breaks = bins)

### Density plot
?density # kernel estimate
dens <- density(iris$Sepal.Width)
plot(dens, main = "Density distribution of the width of sepal")
dens <- density(iris$Sepal.Width, bw=0.05)
plot(dens, main = "Density distribution of the width of sepal")

### QQ plot
qqnorm(iris$Sepal.Width)
qqline(iris$Sepal.Width)

## Graphical options
par() # graphical options

par(bg="#FCE8C5", mar=c(4,4,4,4), pch = 19, las=1, cex=1.2, cex.main=1.2, cex.axis=1,cex.lab=1)

plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     col = alpha(col.iris, 0.2)) # set up symbol types and their color 

legend(x="bottomright", pch= 19, cex=0.8, legend= c("versicolor","setosa", "virginica"), col=levels(as.factor(alpha(col.iris, 0.2))))

dev.off()

## Exporting plot
tiff(filename = "Output/iris_plot.tif", width = 5, height = 6, units = "in", compression = "none", res = 300)

par(bg="#FCE8C5", mar=c(4,4,4,4), pch = 19, las=1, cex=1.2, cex.main=1.2, cex.axis=1,cex.lab=1)

plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     col = alpha(col.iris, 0.2)) # set up symbol types and their color 

legend(x="bottomright", pch= 19, cex=0.8, legend= c("versicolor","setosa", "virginica"), col=levels(as.factor(alpha(col.iris, 0.2))))

dev.off()

pdf(file= "Output/iris_plot.pdf", width = 5, height = 6)

par(bg="#FCE8C5", mar=c(4,4,4,4), pch = 19, las=1, cex=1.2, cex.main=1.2, cex.axis=1,cex.lab=1)

plot(Petal.Length ~ Petal.Width, dat = iris,
     xlab = 'Petal width (cm)', 
     ylab = 'Petal length (cm)', 
     main = 'Petal width and length of iris flower',
     col = alpha(col.iris, 0.2)) # set up symbol types and their color 

legend(x="bottomright", pch= 19, cex=0.8, legend= c("versicolor","setosa", "virginica"), col=levels(as.factor(alpha(col.iris, 0.2))))

dev.off()

## The `lattice` package
### Univariate distribution
library(lattice)
densityplot(~ Petal.Length| Species, iris, plot.points = "", layout=c(1,3))
histogram(~ Petal.Length| Species, iris, plot.points = "", nint = 20, layout=c(1,3))
qqmath(~ Petal.Length| Species, iris, plot.points = "", nint = 20, layout=c(3,1))
iris$variety<-rep(c(rep('pure',25), rep('hybrid',25)),3) # dummy variable
bwplot(Petal.Length ~  variety|Species, iris)

### Multivariate data
#### Scatter plots
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free", layout = c(2, 2), type=c("p","g"),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))

xyplot(Sepal.Length + Sepal.Width + Petal.Width ~ Petal.Length  | Species,
       data = iris, scales = "free", layout = c(2, 2), type=c("p","r"),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))

#### Basic line plots
xyplot(Sepal.Length + Sepal.Width + Petal.Width ~ Petal.Length  | Species, data = iris[order(iris$Petal.Length),], scales = "free", layout = c(2, 2), type=c("l"), auto.key = list(x = .6, y = .7, corner = c(0, 0)))

xyplot(Sepal.Length + Sepal.Width + Petal.Width ~ Petal.Length  | variety+Species, data = iris[order(iris$Petal.Length),], scales = "free", layout = c(3, 2), type=c("l"), auto.key = T)

trellis.par.get()



