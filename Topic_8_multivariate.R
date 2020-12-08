library(vegan)
library(ade4)
library(gclus)
library(qgraph)



?varespec
data (varespec)
varespec[1:5,1:5]

# log,  hellinger, and presence/absence transformations
varespec.log<-decostand(varespec,'log')
varespec.hell<-decostand(varespec,'hellinger')
varespec.pa<-decostand(varespec,'pa')
varespec.pa [1:5,1:5]

# using varespec dataset
spe<-varespec

# quantitative data
# Bray-Curtis dissimilarity matrix on raw data
spe.db <- vegdist(spe)
head(spe.db)
# Bray-Curtis dissimilarity matrix on log-transformed data
spe.dbln <- vegdist(log1p(spe)) # log(x+1)
head(spe.dbln)
# Chord distance matrix
spe.norm<-decostand(spe,'nor')
spe.dc <- vegdist(spe.norm)
head(spe.dc)
# Hellinger distance matrix
spe.hel<-decostand(spe,'hel')
spe.dh <- vegdist(spe.hel)
head(spe.dh)

# quantitative data with a clear interpretation of double zeros use Euclidean distance D1
# using environmental dataset varechem
data(varechem)
env <- varechem
env.st<-decostand(env,'stan') # standardized using decostand or scale(env)
env.de<-vegdist(env.st,method='euc') # then compute D1

# binary data
# Jaccard dissimilarity matrix using vegdist()
spe.dj1 <- vegdist(spe,'jac',binary=T)# binary perform presence/absence standardization before analysis
head(spe.dj1)
# Jaccard dissimilarity matrix using dist()
spe.dj2 <- dist(spe,'binary') 
head(spe.dj2)
# Sorensen dissimilarity matrix using vegdist()
spe.ds<-vegdist(spe,binary=T)
head(spe.ds)
# also see Ochiai dissimilarity matrix with dist.binary() from the ade4 package
spe.och<-dist.binary(spe, method=7)
head(spe.och)

library(gclus)
source('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/coldiss.r') # import coldiss () function  (Borcard et al. 2011) 
coldiss(spe.db,byrank=F,diag=T) # for the bc dissimilarity on raw data 
coldiss(spe.dbln,byrank=F,diag=T) # for the bc dissimilarity on log-transformed data
coldiss(env.de, diag=T) # for the environmental data

qgraph(1-spe.db, layout='spring', vsize=4)

## spe.t <- t(spe)# transpose species matrix
## spe.t.chi <- decostand(spe.t,'chi.square') # Chi-square transformation
## spe.t.D16 <-dist(spe.t.chi)# euclidean distance
## coldiss(spe.t.D16, diag=T) # visualization

#  Pearson r linear correlation among env. variable
env.pearson <- cor(env) # default method = 'pearson')
env.pearson <- round(env.pearson,2)
# re-order the variables prior to plotting
env.o<-order.single(env.pearson)
# need panelutils () on ceiba
source ('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/panelutils.r')
pairs (env[,env.o], lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist, main='Pearson Correlation Matrix')

library(mvabund)
data(tikus)
