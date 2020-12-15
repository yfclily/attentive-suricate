library(vegan)
library(ade4)
library(gclus)
library(qgraph)
library(factoextra)



# ?varespec
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

## # past code

# Step 1: chord distance = normalization + euclidean
spe.norm<-decostand(spe,'normalize') 
spe.ch<-vegdist(spe.norm,'euc') 

# Step 2: single linkage agglomerative clustering
spe.ch.single <-hclust(spe.ch,method='single') 
# plot function
plot(spe.ch.single, main='Single linkage agglomerative clustering' ) 

spe.ch.complete<-hclust(spe.ch,method='complete') 
plot(spe.ch.complete, main='Complete linkage agglomerative clustering') 

spe.ch.UPGMA<-hclust(spe.ch,method='average') 
plot(spe.ch.UPGMA, main='Average (UPGMA) agglomerative clustering') 

spe.ch.ward<-hclust(spe.ch,method='ward.D') 
plot(spe.ch.ward, main='Ward clustering') 

# Single linkage clustering
spe.ch.single.coph <- cophenetic (spe.ch.single)
cor(spe.ch,spe.ch.single.coph)

# complete linkage clustering
spe.ch.complete.coph <- cophenetic (spe.ch.complete)
cor(spe.ch,spe.ch.complete.coph)

# Average clustering
spe.ch.UPGMA.coph <- cophenetic (spe.ch.UPGMA)
cor(spe.ch,spe.ch.UPGMA.coph)

# Ward clustering
spe.ch.ward.coph <- cophenetic (spe.ch.ward)
cor(spe.ch,spe.ch.ward.coph)

par(mfrow=c(2,2))

plot(spe.ch,spe.ch.single.coph,xlab='Chord distance',ylab='Chophenetic distance',asp=1, main=c('Single linkage',paste('Cophenetic correlation',round(cor(spe.ch,spe.ch.single.coph),3))))
abline (0,1)
lines(lowess(spe.ch,spe.ch.single.coph),col='red')

plot(spe.ch,spe.ch.complete.coph,xlab='Chord distance',ylab='Chophenetic distance',asp=1, main=c('Complete linkage',paste('Cophenetic correlation',round(cor(spe.ch, spe.ch.complete.coph),3))))
abline (0,1)
lines(lowess(spe.ch, spe.ch.complete.coph),col='red')

plot(spe.ch,spe.ch.UPGMA.coph,xlab='Chord distance',ylab='Chophenetic distance',asp=1, main=c('UPGMA',paste('Cophenetic correlation',round(cor(spe.ch,spe.ch.UPGMA.coph),3))))
abline (0,1)
lines(lowess(spe.ch,spe.ch.UPGMA.coph),col='red')

plot(spe.ch,spe.ch.ward.coph,xlab='Chord distance',ylab='Chophenetic distance',asp=1, main=c('Ward clustering',paste('Cophenetic correlation',round(cor(spe.ch,spe.ch.ward.coph),3))))
abline (0,1)
lines(lowess(spe.ch,spe.ch.ward.coph),col='red')

dev.off()

plot(spe.ch.UPGMA$height, nrow(spe):2, 
     type='S',main='Fusion levels - chord - UPGMA',
     ylab='k (number of clusters)', xlab='h (node height)', col='grey')
text (spe.ch.UPGMA$height,nrow(spe):2, nrow(spe):2, col='red', cex=0.8)

plot(spe.ch.UPGMA)
rect.hclust(spe.ch.UPGMA, k=6) # number of group
rect.hclust(spe.ch.UPGMA, h=0.79) # with height


par(mfrow=c(2,2))
# fusion level - single linkage clustering
plot(spe.ch.single$height, 
     nrow(spe):2, type='S',main='Fusion levels - chord - single',
     ylab='k (number of clusters)', xlab='h (node height)', col='grey')
text (spe.ch.single$height,nrow(spe):2, nrow(spe):2, col='red', cex=0.8)

# fusion level - complete linkage clustering
plot(spe.ch.complete$height, 
     nrow(spe):2, type='S',main='Fusion levels - chord - complete',
     ylab='k (number of clusters)', xlab='h (node height)', col='grey')
text (spe.ch.complete$height,nrow(spe):2, nrow(spe):2, col='red', cex=0.8)

# fusion level - UPGMA clustering
plot(spe.ch.UPGMA$height, nrow(spe):2, 
     type='S',main='Fusion levels - chord - UPGMA',
     ylab='k (number of clusters)', xlab='h (node height)', col='grey')
text (spe.ch.UPGMA$height,nrow(spe):2, nrow(spe):2, col='red', cex=0.8)

# fusion level - the ward clustering
plot(spe.ch.ward$height, nrow(spe):2,
     type='S',main='Fusion levels - chord - Ward',
     ylab='k (number of clusters)', xlab='h (node height)', col='grey')
text (spe.ch.ward$height,nrow(spe):2, nrow(spe):2, col='red', cex=0.8)

k<-5 # Number of groups (conscensus) 
spebc.single.g <- cutree(spe.ch.single, k)
spebc.complete.g <- cutree(spe.ch.complete, k)
spebc.UPGMA.g <- cutree(spe.ch.UPGMA, k)
spebc.ward.g <- cutree(spe.ch.ward, k)

table(spebc.single.g,spebc.complete.g) # Single vs complete

cutg<-cutree(spe.ch.UPGMA, k=3)
sil<-silhouette (cutg,spe.ch)
plot(sil)

## Mantel test
# Optimal number of clusters
# according to mantel statistic 
# Function to compute a binary distance matrix from groups
grpdist<-function(x){
  require (cluster)
  gr<-as.data.frame(as.factor(x))
  distgr<-daisy(gr,'gower')
  distgr
}
# run based on the UPGMA clustering
kt<-data.frame(k=1:nrow(spe),r=0)
for (i in 2:(nrow(spe)-1)){
  gr<-cutree(spe.ch.UPGMA,i)
  distgr<-grpdist(gr)
  mt<-cor(spe.ch,distgr, method='pearson')
  kt[i,2] <- mt
}
k.best <- which.max (kt$r)
plot(kt$k,kt$r, 
     type='h', main='Mantel-optimal number of clusters - UPGMA',
     xlab='k (number of groups)',ylab="Pearson's correlation")
axis(1,k.best, 
     paste('optimum', k.best, sep='\n'), col='red',font=2, col.axis='red')
points(k.best,max(kt$r),pch=16,col='red',cex=1.5)

fviz_nbclust(spe.norm, hcut, method = "wss",hc_method = "average")
# fviz_nbclust(spe.norm, hcut, method = "silhouette",hc_method = "average")

plot(spe.ch.UPGMA, main='Average linkage')
rect.hclust(spe.ch.UPGMA, k=3)
rect.hclust(spe.ch.UPGMA, k=8, border = 'blue')

# ?doubs
data(doubs)
doubs.spe<-doubs$fish
doubs.spa<-doubs$xy
# remove empty sample 8 from both datasets
doubs.spe <- doubs.spe[-8,]
doubs.spa <- doubs.spa[-8,]
# Calculates hierarchical cluster analysis of species data 
eucl.ward <- hclust (d = dist (doubs.spe), method = 'ward.D')
# Dendrogram with the observed groups
par(mfrow=c(1,2))
plot (eucl.ward)
rect.hclust (eucl.ward, k = 4, border = 1:4)
# Spatial distribution of samples with projection of hierarchical classification
eucl.ward.cluster <- cutree (eucl.ward, k = 4)
plot (y ~ x, data = doubs.spa, pch = eucl.ward.cluster, col = eucl.ward.cluster, type = 'b', main = 'Euclidean distance - Ward method')
dev.off()

spe.chwo<-reorder.hclust(spe.ch.ward,spe.ch)
dend<-as.dendrogram(spe.chwo) 
heatmap(as.matrix(spe.ch),Rowv=dend,symm=TRUE, margin=c(3,3))
