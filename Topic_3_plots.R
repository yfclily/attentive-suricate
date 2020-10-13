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

legend(x="bottomright", pch= 19, cex=1.5, legend= c("versicolor","setosa", "virginica"), col=alpha(levels(as.factor(col.iris)),0.2))
       

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
     lty=2, lwd=4, col='red', ylab = "No. of flower blossoming") 

plot(count.alaska ~ year,dat = blossom, type='b', pch=20,
     lty=2, lwd=4, col='red', ylab = "No. of flower blossoming") 
lines(count.canada ~ year,dat = blossom, type='b', pch=20,
      lty=3, lwd=4, col='blue')

y.rng<-range(c(blossom$count.alaska, blossom$count.canada))

plot(count.alaska ~ year,dat = blossom, type='l', ylim = y.rng,
     lty=2, lwd=4, col='red', ylab = "No. of flower blossoming")

lines(count.canada ~ year,dat = blossom, lty=1, lwd=4, col='blue')

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


points(Petal.Length ~ Petal.Width, dat = iris.vir, pch = 20,cex=2, col =  alpha('#EB9121', 0.2)) # Add points for versicolor


legend("topleft", c("versicolor", "virginica"), pch = 19, cex=1.2, col = c(rgb(0,0,1,0.10), alpha('#EB9121', 0.2))) # Add legend

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


## The `ggplot` package
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

dat1 <- read.csv("Data/FAO_grains_NA.csv", header=TRUE)

dat1w <- dat1 %>%
   filter(Information == "Yield (Hg/Ha)", 
          Country =="United States of America", 
          Crop %in% c("Oats", "Maize", "Barley", "Buckwheat","Rye")) %>% 
   select (Year, Crop, Value) %>% spread(key = Crop, value="Value")

dat1l <- gather(dat1w, key = "Crop", value = "Yield", 2:6)

dat1l2 <- dat1 %>%
   filter(Information == "Yield (Hg/Ha)", 
          Crop       %in% c("Oats", "Maize", "Barley", "Buckwheat","Rye")) %>%
   select( Year, Crop, Country,  Yield = Value)  # Note that we are renaming the Value field

dat2 <- read.csv("Data/Income_education.csv", header=TRUE)

Edu.Gend <- data.frame( 
   variable   = paste("B200040", str_pad(1:18, width=2 , pad="0"),sep="" ),
   Level      = rep(c("All", "NoHS","HS","AD","BD","Grad"), times=3),
   Gender     = rep(c("All", "M","F"), each=6) )


st.reg <- data.frame(State = tolower(state.abb), Region = state.region)# Create a region/state join table
st.reg <- rbind(st.reg , data.frame(State="dc", Region="South") )

dat2b <- dat2 %>% # Start the piping operations
   gather(key = "variable", value = "value", -1:-2)  %>%
   left_join(Edu.Gend, by="variable" )              %>%
   select(State, County, Level, Gender, value)       %>%
   mutate(Level = factor(Level, levels = c("All","NoHS","HS", "AD", "BD", "Grad"))) %>%
   left_join(st.reg , by="State")

dat2c <- spread(dat2b, key = Gender, value = value )

ggplot(dat1l , aes(x = Year, y = Yield)) + geom_point()
ggplot(dat1l , aes(x = Year, y = Yield, color = Crop)) + geom_point()
ggplot(dat1l , aes(x = Year, y = Yield, color = Crop)) + geom_line()
ggplot(dat1l , aes(Year, Yield, color = Crop)) + geom_line()

### Geometries

#### `geom_line`
ggplot(dat1w, aes(x = Year, y = Oats)) + geom_line() 
ggplot(dat1w, aes(x = Year, y = Oats)) + 
   geom_line(linetype = 2, colour = "blue", size=0.4)

#### `geom_point`
ggplot(dat2, aes(x = B20004013, y = B20004007)) + geom_point(alpha = 0.3)
ggplot(dat2, aes(x = B20004013, y = B20004007)) + 
   geom_point(colour = "red", pch=3 , alpha = 0.3, cex=0.6)

#### `geom_hex`
library(hexbin)
ggplot(dat2, aes(x = B20004013, y = B20004007)) + 
   geom_hex(binwidth = c(1000, 1000))

#### `geom_boxplot`
ggplot(dat1l, aes(x = Crop, y = Yield)) + geom_boxplot(fill = "bisque") 
ggplot(dat1l, aes(x = "", y = Yield)) + 
   geom_boxplot(fill = "bisque") + xlab("All crops")

#### `geom_violin`
ggplot(dat1l, aes(x = "", y = Yield)) + geom_violin(fill = "bisque") 

#### `geom_histogram`
ggplot(dat1w, aes(x = Oats)) + geom_histogram(fill = "grey50")
ggplot(dat1w, aes(x = Oats)) + 
   geom_histogram(fill = "grey50", binwidth = 1000)
ggplot(dat1w, aes(x = Oats)) + 
   geom_histogram(fill = "grey50", bins = 8) 

#### `geom_bar`
library(forcats)
ggplot(dat2, aes(State)) + geom_bar()
ggplot(dat2, aes(fct_infreq(State,ordered = TRUE))) + geom_bar()
ggplot(dat2, aes(fct_rev(fct_infreq(State,ordered = TRUE)))) + geom_bar()

dat2.ct <- dat2 %>% group_by(State) %>% summarize(Counties = n())
ggplot(dat2.ct, aes(x=fct_reorder(State, Counties, .fun = median), y = Counties)) + geom_bar(stat = "identity")

#### dot plot
ggplot(dat2.ct , aes(x = Counties, y = State)) + geom_point()
ggplot(dat2.ct , aes(x = Counties, y = fct_reorder(State, Counties, .fun = median))) + geom_point()

#### Combining geometries

ggplot(dat2, aes(x = B20004013, y = B20004007)) + 
   geom_point(alpha = 0.3, color='#4f4b4b') + 
   geom_smooth(method = "lm", color='green')

ggplot(dat2, aes(x = B20004013, y = B20004007)) + 
   geom_point(alpha = 0.3, color='#4f4b4b') +
   geom_smooth(method = "loess", color='green')

### Tweaking a ggplot2 graph

#### Plot title 
ggplot(dat2, aes(State)) + geom_bar() + ggtitle("Number of counties by state")
ggplot(dat2, aes(x = B20004013, y = B20004007)) + geom_point(alpha = 0.3) + xlab("Female income ($)") + ylab("Male income ($)")

#### Axes labels
ggplot(dat2, aes(x = B20004013, y = B20004007))+ 
   geom_point(alpha = 0.3) +
   xlab("Female income ($)") + 
   ylab("Male income ($)") +
   scale_x_continuous(breaks = c(10000, 30000, 50000),labels = c("$10,000", "$30,000", "$50,000"))

ggplot(dat2, aes(x=B20004013, y=B20004007)) + geom_point(alpha=0.3) +
   xlab("Female income ($)") + ylab("Male income ($)") +
   scale_x_continuous(labels=scales::unit_format(suffix="k",scale=0.001, sep="")) +
   scale_y_continuous(labels=scales::unit_format(suffix="k", scale=0.001, sep=""))

ggplot(dat2, aes(x=B20004013, y=B20004007)) + geom_point(alpha=0.3) +
   xlab("Female income ($)") + ylab("Male income ($)") +
   scale_x_continuous(labels=scales::unit_format(suffix="k", scale=0.001, sep="")) +
   scale_y_continuous(labels=scales::unit_format(suffix="k", scale=0.001, sep="")) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#### Axes limits
ggplot(dat2, aes(x = B20004013, y = B20004007)) + geom_point(alpha = 0.3) +
   xlab("Female income ($)") + ylab("Male income ($)") +
   xlim(10000, 75000) + ylim(10000, 75000)

ggplot(dat2, aes(x = B20004013, y = B20004007)) + geom_point(alpha = 0.3) +
   xlab("Female income ($)") + ylab("Male income ($)") +
   scale_x_continuous(limit  = c(10000, 75000),labels = scales::comma_format()) +
   scale_y_continuous(limit  = c(10000, 75000), labels = scales::comma_format())

#### Axes breaks
ggplot(dat2, aes(x = B20004013, y = B20004007)) + geom_point(alpha = 0.3) +
   xlab("Female income ($)") + ylab("Male income ($)") +
   scale_x_continuous(limit  = c(10000, 75000),labels = scales::comma_format(),breaks = c(10000, 30000, 50000, 70000)) +
   scale_y_continuous(limit  = c(10000, 75000),labels = scales::comma_format(),breaks = c(10000, 30000, 50000, 70000))

#### Axes and data transformations
ggplot(dat2, aes(x = B20004013, y = B20004007)) + geom_point(alpha = 0.3) +
   xlab("Female income ($)") + ylab("Male income ($)") +
   coord_trans(x = "log")

ggplot(dat2, aes(x = B20004013, y = B20004007)) + geom_point(alpha = 0.3) +
   xlab("Female income ($)") + ylab("Male income ($)") +
   coord_trans(x = scales::boxcox_trans(-0.3))

ggplot(dat2, aes(x = B20004013, y = B20004007)) + geom_point(alpha = 0.3) +
   geom_smooth(method = "lm", se = FALSE) +
   xlab("Female income ($)") + ylab("Male income ($)") +
   scale_x_continuous(trans = "log", breaks = seq(10000,60000,10000))

#### Aspect ratio
ggplot(dat2, aes(x = B20004013, y = B20004007)) + geom_point(alpha = 0.3) +
   geom_smooth(method = "lm") +
   xlab("Female income ($)") + ylab("Male income ($)") +
   coord_equal(ratio = 1)

#### Colors
##### Continuous color schemes
dat2.ct2 <- dat2 %>% group_by(State) %>% summarize(Counties = n(), Income = median(B20004001))
ggplot(dat2.ct2, aes(x = fct_reorder(State, Counties), y = Counties, fill = Income)) +
   geom_bar(stat = "identity") + 
   scale_fill_gradient(low = "green", high = "red")


ggplot(dat2.ct2, aes(x = fct_reorder(State, Counties), y = Counties, fill = Income)) +
   geom_bar(stat = "identity") + 
   scale_fill_gradient2(low = "darkred", mid = "white",  high = "darkgreen", midpoint = 30892)


ggplot(dat2.ct2, aes(y = fct_reorder(State, Counties), x = Counties, col = Income)) +
   geom_point() +
   scale_colour_gradient2(low = "darkred", mid = "white",  high = "darkgreen", midpoint = 30892)

##### Discrete color schemes
ggplot(dat1l, aes(Year, Yield, col = Crop)) + 
   geom_line() +
   scale_colour_manual(values = c("red", "orange", "green", "blue", "yellow"))

ggplot(dat1l, aes(Year, Yield, col = Crop)) + 
   geom_line() +
   scale_colour_brewer(palette = "Dark2")

ggplot(dat1l, aes(Year, Yield, col = reorder(Crop, Yield, median))) + 
   geom_line() +
   guides(colour = guide_legend(title = "Crops")) +  
   scale_colour_brewer(palette = "Reds") 

#### Adding mathematical symbols to a plot
ggplot(dat2, aes(x = B20004013^0.333, y = sqrt(B20004007))) + geom_point(alpha = 0.3) +
   xlab( expression(("Female income") ^ frac(1,3))) + 
   ylab( expression(sqrt("Male income")))

### Faceting
#### Faceting by categorical variables
##### `facet_wrap`
ggplot(dat1l2, aes(x = Year, y = Yield, color = Crop)) + geom_line() + 
   facet_wrap( ~ Country, nrow = 1)

ggplot(dat1l2, aes(x = Year, y=Yield)) + geom_line() +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
   facet_wrap(Crop ~ Country, nrow = 1)

ggplot(dat1l2, aes(x = Year, y=Yield)) + geom_line() +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
   facet_wrap(Crop ~ Country, nrow = 1, labeller = label_wrap_gen(width = 12))

##### `facet_grid`
ggplot(dat1l2, aes(x = Year, y = Yield)) + geom_line() + 
   facet_grid( Crop ~ Country)

#### Faceting by continous variables (advance)
dat2c$incrng <- cut(dat2c$F , breaks = c(0, 25000, 50000, 75000, 100000) )
summary(dat2c$incrng)
dat2c <- na.omit(dat2c)
levels(dat2c$incrng)
levels(dat2c$incrng) <-  c("Under 25k", "25k-50k", "50k-75k", "75k-100k")
head(dat2c)
ggplot(dat2c, aes(x = F, y = M)) + geom_point(alpha=0.2, pch=20) +
   geom_smooth(method = "lm", col = "red") +
   facet_grid( . ~ incrng) 

### Exporting to an image
p1 <- ggplot(dat1l2, aes(x = Year, y = Yield, color = Crop)) + geom_line() + 
   facet_wrap( ~ Country, nrow = 1) +
   scale_y_continuous(labels = scales::comma_format())
ggsave("Output/fig0.png", plot = p1, width = 6, height = 2, units = "in", device = "png")

p2 <- ggplot(dat1l2, aes(x = Year, y = Yield, color = Crop)) + geom_line() + 
   facet_wrap( ~ Country, nrow = 1) +
   scale_y_continuous(labels = scales::comma_format()) +
   theme(axis.text    = element_text(size = 8, family = "mono"),
         axis.title   = element_text(size = 11, face = "bold"),
         strip.text   = element_text(size = 11, face="italic", family = "serif"),
         legend.title = element_text(size = 10, family = "sans"),
         legend.text  = element_text(size = 8,  color = "grey40"))
ggsave("Output/fig1.png", plot = p2, width = 6, height = 2, units = "in")

