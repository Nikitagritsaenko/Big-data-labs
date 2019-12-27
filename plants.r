library(cluster)
library(NbClust)
# Read data
rm(list=ls())
gc(reset=TRUE)

data.plants <- read.table('plants.dat', 
                          sep=';', 
                          header=TRUE, 
                          na.strings="NA",
                          stringsAsFactors=T)

head(data.plants)

data.plants$plant.name=NULL

#data.plants <- scale(data.plants)

data_dim = 31;
data_size = 136;

for(i in seq(1,data_dim)){
  med<-median(data.plants[,i],na.rm = TRUE)
  for(j in seq(1,data_size)){
    if(is.na(data.plants[j,i])){
      data.plants[j,i]=med
    }
  }
}

dm = data.matrix(data.plants)
for(i in 1:data_dim){
  dm[,i] = dm[,i] / norm( data.matrix(dm[,i]), type = "M")
}

corr_dm = cor(dm, method = "pearson")
corr = 1:data_dim

for(i in 1:data_dim){
 corr[i]=sum(abs(corr_dm[,i]))
}

summary(data.plants)

#variables_idx = c(5, 30, 29, 25, 7)
variables_idx = c(5, 7, 30, 1, 6)
dim = 5
dm1 <- matrix(seq(1, 16), nrow = 136, ncol = dim)
for(i in 1 : dim){
  dm1[,i]=dm[,variables_idx[i]]
}
dataL = data.plants[, c("durflow", "begflow", "height")]

#NbClust(data = data.plants, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 4, method = "median", index = "all", alphaBeale = 0.1)

k.vec <- seq(2, 4)
diss.vec <- c()
cl.vec <- c()
cl$clustering

for (k in k.vec) {
  
  png(paste0("plant_k", k, ".png"), width = 500, height = 500)
  cl <- pam(dm1, k = k, metric = "euclidean", stand = T, keep.diss = TRUE)
  #cl <- clara(dataM, k=k, metric = "manhattan",
        #stand = TRUE,
        #keep.diss = TRUE)
  
  clusplot(dm1, cl$clustering, color=TRUE, shade=TRUE, labels=5, lines=2)
  #plot(cl)
  
  cl.vec <- c(cl.vec, cl)
  diss <- cl$silinfo$avg.widt
  diss.vec <- c(diss.vec, diss)
  
  dev.off()
}



diss.vec

custom.panel <- function() {
  axis(1, tck=1, col.ticks="light gray")
  axis(1, tck=-0.015, col.ticks="black")
  axis(2, tck=1, col.ticks="light gray")
  axis(2, tck=-0.015)
}

options(repr.plot.width = 10, repr.plot.height = 10)
plot(k.vec, diss.vec,
     type = "l",
     xlab = "k",
     ylab = "diss",
     lwd = 2,
     col = adjustcolor("black", alpha.f = 0.7),
     pch = 19,
     panel.first = custom.panel())

NbClust(data = dm1, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 4, method = "median", index = "all")
