# Topic 5: loop & functions

library('animation')
library('ggplot2')
library ('gganimate')

rm(list=ls())# clean all objects in memory


## Loops
### Basic structure

for(i in 1:100) {
   print("Hello world!")
   print(i*i)
   }


foo <- seq (1,100,by=2) # a sequence  1, 3, ..., 99
n<-length(foo) # the size of the sequence: 50
foo.squared = NULL # an empty object
for (i in 1:n) { # our counter
   foo.squared[i] = foo[i]^2 # the task the fist value of foo.squared will receive the fist value of foo
}

foo.df<-data.frame(foo,foo.squared) # we combine both sequences in a data.frame
plot (foo.df$foo~foo.df$foo.squared) # we plot both sequences together

foo.squared2<-foo^2
plot (foo~foo.squared2)

### Recycling

num_gen <- 10 # my counter, number of generation
N <- rep (0,num_gen) # this creates a "vector" of 10 zeros, can also be empty `NULL`
N[1] <- 2 # we set the first value equal to 2
for (i in 2:num_gen){ #  move from 2 to num_gen =10
   N[i]=2*N[i-1] # the population will double every generation
}
plot (N) # plotting the results


num_gen<-10 # my counter, number of generation
generation<-1:num_gen # create a variable generation
N <- rep (0,num_gen)# this creates a "vector" of 10 zeros, can also be empty `NULL`
N[1] <- 3 # we set the first value equal to 2
for (i in 2:num_gen) { # i will move from 2 to num_gen =10
   N[i]=2*(N[i-1]) # the population will double every generation
}
plot(N~generation, type='b') # plotting the results 


## Function
grow <- function (growth.rate) { # "growth rate" will be argument for our function grow 
   num_gen<-10
   generation<-1:num_gen
   N <- rep (0,num_gen)
   N[1] <- 1
   for (i in 2:num_gen) {
      N[i]=growth.rate*N[i-1] # we replace 2 by our argument
   }
   plot(N~generation,type='b', main=paste("Rate =", growth.rate)) 
}

par(mfrow=c(2,3))
for (i in 2:7){
   grow(i)
}

grow3 <- function (growth.rate) { 
   num_gen<-10
   generation<-1:num_gen
   N <- rep (0,num_gen)
   N[1] <- 1
   for (i in 2:num_gen) {
      N[i]=growth.rate*N[i-1]
   }
   plot(N~generation, xlim=c(0,10), ylim=c(0,100000), type='b', main=paste("Rate =", growth.rate))
}


## Animation

### GIF
saveGIF({ # combine different plot together using function saveGIF function
   for (i in 2:10){
      grow3(i)
   }})


### gganimate
demo<-NULL
demo$count<-N
demo$generation<-generation
demo<-as.data.frame(demo)

p <- ggplot(demo, aes(x = generation, y=count, size =2)) +
   geom_point(show.legend = FALSE, alpha = 0.7) +
   scale_color_viridis_d() +
   scale_size(range = c(0, 12)) +
   labs(x = "Generation", y = "Individuals")

p + transition_time(generation) +
   labs(title = "Generation: {frame_time}") +
   shadow_wake(wake_length = 0.2, alpha = FALSE)




