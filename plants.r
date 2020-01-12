library(cluster)
library(NbClust)

euc.dist <- function(x1, x2)
  sqrt(sum((x1 - x2) ^ 2))

removeNA <- function(matrix) {
  data_dim = ncol(matrix)
  data_size = nrow(matrix)
  M = matrix
  for (i in seq(1, data_dim)) {
    med <- median(M[, i], na.rm = TRUE)
    for (j in seq(1, data_size)) {
      if (is.na(M[j, i])) {
        M[j, i] = med
      }
    }
  }
  return(M)
}

normalizeMatrix <- function(matrix) {
  data_dim = ncol(matrix)
  M = matrix
  for (i in seq(1, data_dim)) {
    M[, i] = M[, i] / norm(data.matrix(M[, i]), type = "M")
  }
  return(M)
}

clusterize <- function(matrix, num_of_clusters) {
  data_dim = ncol(matrix)
  data_size = nrow(matrix)
  
  cl <-
    pam(
      matrix,
      k = num_of_clusters,
      metric = "euclidean",
      stand = T,
      keep.diss = TRUE
    )
  
  centers = cl$medoids
  error = 0
  for (i in seq(1, data_size)) {
    min_dist = 1000000
    
    for (j in seq(1, num_of_clusters)) {
      dist = euc.dist(matrix[i, ], centers[j, ])
      
      if (dist < min_dist) {
        min_dist = dist
      }
    }
    error = error + min_dist ^ 2
  }
  
  return(sqrt(error))
}
# ------------------------------------------------------------------------

# Read data in matrix M
path = 'data/plants.dat'
data.plants <- read.table(
  path,
  sep = ';',
  header = TRUE,
  na.strings = "NA",
  stringsAsFactors = T
)

data.plants$plant.name = NULL
M = data.matrix(data.plants)

data_dim = ncol(M)
data_size = nrow(M)

# Process data
M = removeNA(M)
M = normalizeMatrix(M)

# Choose components
corr_dm = cor(M, method = "pearson")
corr = 1:data_dim

for (i in 1:data_dim) {
  corr[i] = sum(abs(corr_dm[, i]))
}

variables_idx = c(5, 7, 30, 1, 6)
dim = 5

newM <- matrix(0, nrow = data_size, ncol = dim)

for (i in 1:dim) {
  newM[, i] = M[, variables_idx[i]]
}

# Trying to clustering
min_num_of_clusters = 2
max_num_of_clusters = 4

error_vec = matrix(0, nrow = 1, ncol = 3)
i = 1

for (k in seq(min_num_of_clusters, max_num_of_clusters)) {
  error_vec[i] = clusterize(newM, k)
  i = i + 1
}

plot(min_num_of_clusters:max_num_of_clusters,
     error_vec,
     "l",
     main = "Sqr error for diffirent N of clusters",
     xlab = "num of clusters",
     ylab = "square error")

# Finding optimal N of clusters
NbClust(
  data = newM,
  diss = NULL,
  distance = "euclidean",
  min.nc = min_num_of_clusters,
  max.nc = max_num_of_clusters,
  method = "median",
  index = "all"
)
