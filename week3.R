set.seed(12345)
par(mar=rep(0.2,4))
DM <- matrix(rnorm(400),nrow=40)
image(1:10,1:40,t(DM)[,nrow(DM):1])

heatmap(DM)

set.seed(678910)
for (i in 1:40) {

    coinFlip <- rbinom(1,size=1,prob=.5)
        if(coinFlip) {
        DM[i,] <- DM[i,] + rep(c(0,3),each=5)
        }
}


image(1:10,1:40,t(DM)[,nrow(DM):1])

heatmap(DM)

hh <-  hclust(dist(DM))
DMOrdered <- DM[hh$order,]
par(mfrow=c(1,3))
image(t(DMOrdered)[,nrow(DMOrdered):1])
plot(rowMeans(DMOrdered),40:1,, xlab="Row Means",ylab="Row",pch=19)
plot(colMeans(DMOrdered), xlab="Column",ylab="Column Mean",pch=19)

svd1 <- svd(scale(DMOrdered))


DM2 <- DMOrdered
DM2[sample(1:100,size=40,replace=F)] <- NA
DM2 <- impute.knn(DM2)
svd1 <- svd(scale(DM2Ordered))
svd2 <- svd(scale(DM2))
par(mfrow=c(1,2))
plot(svd1$v[,1],pch=19)
plot(svd2$v[,1],pch=19)

library(RColorBrewer)
cols <- brewer.pal(3,"BuGn")
pal <- colorRampPalette(cols)
image(volcano,col=pal(20))

x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x,y)

plot(x,y,pch=19,col=rgb(1,0,0,0.02))
