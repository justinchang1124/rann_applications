library(stringi)
library(ndtv)
library(htmlwidgets)
library(dplyr)

# ----------------
# HELPER FUNCTIONS
# ----------------

# creates an empty list
my_empty_list <- function(names)
{
  target <- vector(mode="list", length=length(names))
  names(target) <- names
  target
}

# fixed pattern replacement in a vector of strings
repStr <- function(x_stringi, pattern, replacement)
{
  stri_replace_all_fixed(
    x_stringi, pattern = pattern, 
    replacement = replacement, 
    vectorize_all = FALSE)
}

# makes a sequence of colors
color_seq <- function(n_colors)
{
  hcl(1:n_colors * (360/(n_colors+1))-15, 160, 60)
}

# makes colors for a vector
make_colors <- function(vec)
{
  numbers <- as.numeric(as.factor(vec))
  color_seq(length(unique(numbers)))[numbers]
}

# makes a grey sequence of colors
grey_seq <- function(n)
{
  result <- NULL
  for (i in 1:n-1)
  {
    result <- c(result, rgb(i/n, i/n, i/n, 1))
  }
  result
}

# makes grey for a vector
make_grey <- function(vec)
{
  numbers <- as.numeric(as.factor(vec))
  grey_seq(length(unique(numbers)))[numbers]
}

# opens a result from RANN
open_result <- function(filename)
{
  readLines(filename) %>% repStr(" ", "") %>% as.numeric()
}

# makes KNN data
make_knn <- function(indices, distances)
{
  mapply(function(a,b){
    sprintf("I = %s, D = %s", a, b)}, indices, distances) %>% data.frame()
}

# ----------------
# CREATE ANIMATION
# ----------------

# if instant, makes HTML
# otherwise, makes an animation htmlWidget for R Shiny
# min_dist: the minimum distance between graph edges
make_animation <- function(indices, 
                           # distances, 
                           min_dist = 10, instant=TRUE, metadata=NULL)
{
  # if (nrow(indices) != nrow(distances) || ncol(indices) != ncol(distances))
  # {
  #   print("Error: Indices and distances are of incorrect dimension.")
  #   return()
  # }
  
  # create the network
  wheel <- network.initialize(nrow(indices))
  
  # colors for metadata
  met_colors <- "#0000FF"
  if (!is.null(metadata))
    met_colors <- make_colors(metadata)
  
  # add edges
  for (j in 1:ncol(indices))
  {
    add.edges.active(
      wheel, 
      tail=1:nrow(indices), 
      head=as.numeric(indices[,j]), 
      onset=j, terminus=ncol(indices)+1,
      names.eval = rep(list(list("Time"
                                 # , "Distance"
                                 )), nrow(indices)),
      vals.eval = rep(list(list(j
                                # , distances[,j]
                                )), nrow(indices))
    )
  }
  
  # calculate distance layouts, but with the actual distance
  network.layout.animate.circle <- function(
    net, dist.mat = NULL,
    default.dist = NULL, seed.coords = NULL, layout.par = list(),
    verbose=FALSE){
    distances <- sapply(
      layout.distance(net, default.dist = default.dist), function(x) {
        max(min_dist, x)
      }
    )
    
    network.layout.animate.kamadakawai(
      net, dist.mat = distances,
      default.dist = NULL, seed.coords = NULL, layout.par = list(),
      verbose=FALSE)
  }
  
  compute.animation(wheel, animation.mode = "circle",
                    slice.par=list(start=0, end=ncol(indices), 
                                   interval=1,
                                   aggregate.dur=1, rule='any'))
  
  mode <- ifelse(instant, 'HTML', 'htmlWidget')
  
  knn <- render.d3movie(
    wheel, 
    vertex.lwd = 0.5,
    edge.lwd = 1,
    vertex.col = met_colors,
    edge.col = make_grey(wheel %e% "Time"),
    vertex.tooltip = paste("<b>Name:</b>", 1:nrow(indices) , "<br>",
                           "<b>Type:</b>", metadata), 
    # edge.tooltip = paste("<b>Neighbor:</b>", wheel %e% "Time" , "<br>",
    #                      "<b>Length:</b>", wheel %e% "Distance"), 
    output.mode=mode
  )
  
  if (!instant)
    knn$sizingPolicy$defaultWidth = "100%"
  
  knn
}

# ------------
# ANIMATE DATA
# ------------

all_data <- my_empty_list(sprintf("Example %s", 1:7))

# Raw data, KNN data, NDTV D3 movies
for (i in 1:7)
  all_data[[i]] <- my_empty_list(c("Raw", "KNN", "D3M"))

# EXAMPLE 0
testflat <- rep(1:100, 100)
test <- matrix(testflat, nrow=100)
write.table(test, "data/test.csv", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

i0 <- open_result("data/i07_m100_n0000100_k050_i005.txt")
i0 <- data.frame(matrix(i0, ncol=50, byrow = TRUE))[,1:10]
d0 <- open_result("data/d07_m100_n0000100_k050_i005.txt")
d0 <- sqrt(data.frame(matrix(d0, ncol=50, byrow = TRUE))[,1:10])

all_data[[7]][["Raw"]] <- read.csv("data/test.csv", sep=" ", header=FALSE)
all_data[[7]][["KNN"]] <- make_knn(i0, d0)
all_data[[7]][["D3M"]] <- make_animation(i0, 10, instant=FALSE)

# EXAMPLE 1
i1 <- open_result("data/i07_m320_n0000344_k010.txt")
i1 <- data.frame(matrix(i1, nrow=344, byrow=TRUE)[,1:10])
d1 <- open_result("data/d07_m320_n0000344_k010.txt")
d1 <- data.frame(matrix(d1, nrow=344, byrow=TRUE)[,1:10])
m1 <- readLines("data/covid_metadata.txt")

all_data[[1]][["Raw"]] <- read.csv("data/time_series_covid_19_confirmed_US_small.csv",
                                   sep = ' ', header=FALSE)
all_data[[1]][["KNN"]] <- make_knn(i1, d1)
all_data[[1]][["D3M"]] <- make_animation(i1, 0, instant=FALSE, metadata=m1)

# EXAMPLE 2
i2 <- open_result("data/i07_m30000_n0000100_k010.txt")
i2 <- data.frame(matrix(i2, nrow=100, byrow=TRUE)[,1:10])
d2 <- open_result("data/d07_m30000_n0000100_k010.txt")
d2 <- data.frame(sqrt(matrix(d2, nrow=100, byrow=TRUE)[,1:10]))

m2 <- open_result("data/bird_metadata.txt")

all_data[[2]][["Raw"]] <- NULL
all_data[[2]][["KNN"]] <- make_knn(i2, d2)
all_data[[2]][["D3M"]] <- make_animation(i2, 0, instant=FALSE, metadata=m2)

# EXAMPLE 3
ed1 <- read.csv("data/entex_data.csv")[,-1]
write.table(hmm, "~/rann_applications/entex_fin.csv", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

i3 <- open_result("data/i07_m010_n0000086_k050_i005.txt")
i3 <- data.frame(matrix(i3, nrow=86, byrow=TRUE)[,1:10])
d3 <- open_result("data/d07_m010_n0000086_k050_i005.txt")
d3 <- data.frame(sqrt(matrix(d3, nrow=86, byrow=TRUE)[,1:10]))

m3 <- read.csv("data/entex_metadata.csv")$TISSUE

all_data[[3]][["Raw"]] <- ed1
all_data[[3]][["KNN"]] <- make_knn(i3, d3)
all_data[[3]][["D3M"]] <- make_animation(i3, 0, instant=FALSE, metadata=m3)








saveRDS(all_data, "rann_app/data/all_data.rds")