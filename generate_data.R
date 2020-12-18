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
  readLines(filename) %>% repStr(" ", "")
}

# makes KNN data
make_knn <- function(indices, distances)
{
  mapply(function(a,b){
    sprintf("I = %s, D = %s", a, b)}, indices, distances) %>% data.frame()
}

# given a vector of length n x k from RANN, converts it into an n x k data.frame
# and keeps the first 'cap' columns
cap_df <- function(vec, n, cap)
{
  data.frame(matrix(vec, nrow=n, byrow=TRUE)[,1:cap])
}


# ----------------
# CREATE ANIMATION
# ----------------

# given a vector of distances, ensures each entry is at least min_dist
bound_lower <- function(distances, min_dist)
{
  sapply(distances, function(x) {
    max(min_dist, x)
  })
}

# performs normal kamadakawai layout, but with a lower bound on distances
network_layout_bound <- function(min_dist)
{
  function(net, dist.mat = NULL, default.dist = NULL, 
           seed.coords = NULL, layout.par = list(),verbose=FALSE){
    default_dists <- layout.distance(net, default.dist = default.dist)
    
    network.layout.animate.kamadakawai(
      net, dist.mat = bound_lower(default_dists, min_dist), default.dist = default.dist, 
      seed.coords = seed.coords, layout.par = layout.par, verbose = verbose
    )
  }
}

# if instant, makes HTML
# otherwise, makes an animation htmlWidget for R Shiny
# min_dist: the minimum distance between graph edges
make_animation <- function(indices, min_dist = 10, instant=TRUE, metadata=NULL)
{
  # create the network
  wheel <- network.initialize(nrow(indices))
  
  # colors for metadata
  met_colors <- make_colors(metadata)
  if (is.null(metadata))
  {
    metadata <- rep("None", nrow(indices))
    met_colors <- "#00356B" # everyone loves Yale Blue
  }
  
  # add edges
  for (j in 1:ncol(indices))
    add.edges.active(
      wheel, 
      tail=1:nrow(indices), 
      head=as.numeric(indices[,j]), 
      onset=j, terminus=ncol(indices)+1,
      names.eval = list(list("Time")) %>% rep(nrow(indices)),
      vals.eval = list(list(j)) %>% rep(nrow(indices))
    )
  
  # calculate distance layouts, but with a minimum
  network.layout.animate.circle <- network_layout_bound(min_dist)
  
  # compute movement, prior to adding colors / labels to vertices / edges
  compute.animation(
    wheel, animation.mode = "circle", slice.par = list(
      start=0, end=ncol(indices), interval=1, aggregate.dur=1, rule='any'
    )
  )
  
  knn <- render.d3movie(
    wheel, 
    vertex.lwd = 0.5,
    edge.lwd = 1,
    vertex.col = met_colors,
    edge.col = make_grey(wheel %e% "Time"),
    vertex.tooltip = paste("<b>Name:</b>", 1:nrow(indices) , "<br>",
                           "<b>Type:</b>", metadata), 
    output.mode = ifelse(instant, 'HTML', 'htmlWidget')
  )
  # knn <- render.d3movie(
  #   wheel, 
  #   vertex.lwd = 0.5,
  #   edge.lwd = 1,
  #   vertex.col = met_colors,
  #   edge.col = make_grey(wheel %e% "Time"),
  #   vertex.tooltip = paste("<b>Name:</b>", 1:nrow(indices) , "<br>",
  #                          "<b>Bird:</b>", metadata[,1], "<br>",
  #                          "<b>Transformation:</b>", metadata[,2]), 
  #   output.mode = 'htmlWidget'
  # )
  
  # necessary to make the widget resize in R Shiny
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

i0 <- open_result("data/i07_m100_n0000100_k050_i005.txt") %>% cap_df(100, 10)
d0 <- open_result("data/d07_m100_n0000100_k050_i005.txt") %>% cap_df(100, 10)

all_data[[7]][["Raw"]] <- read.csv("data/test.csv", sep=" ", header=FALSE)
all_data[[7]][["KNN"]] <- make_knn(i0, d0)
all_data[[7]][["D3M"]] <- make_animation(i0, 15, instant=FALSE)

# EXAMPLE 1
i1 <- open_result("data/i07_m320_n0000344_k010.txt") %>% cap_df(344, 10)
d1 <- open_result("data/d07_m320_n0000344_k010.txt") %>% cap_df(344, 10)
m1 <- readLines("data/covid_metadata.txt")

all_data[[1]][["Raw"]] <- read.csv("data/time_series_covid_19_confirmed_US_small.csv",
                                   sep = ' ', header=FALSE)
all_data[[1]][["KNN"]] <- make_knn(i1, d1)
all_data[[1]][["D3M"]] <- make_animation(i1, 0, instant=FALSE, metadata=m1)

# EXAMPLE 2
i2 <- open_result("data/i07_m30000_n0000100_k010.txt") %>% cap_df(100, 10)
d2 <- open_result("data/d07_m30000_n0000100_k010.txt") %>% cap_df(100, 10)
m2 <- open_result("data/metadata_with_image_types.txt")
m2 <- strsplit(m2, split="-", fixed=TRUE) %>% unlist() %>% matrix(ncol=2, byrow=TRUE)

all_data[[2]][["Raw"]] <- NULL
all_data[[2]][["KNN"]] <- make_knn(i2, d2)
# all_data[[2]][["D3M"]] <- make_animation(i2, 0, instant=FALSE, metadata=m2)
all_data[[2]][["D3M"]] <- knn

# EXAMPLE 3
ed1 <- read.csv("data/entex_data.csv")[,-1]
write.table(hmm, "data/entex_fin.csv", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

i3 <- open_result("data/i07_m010_n0000086_k050_i005.txt") %>% cap_df(86, 10)
d3 <- open_result("data/d07_m010_n0000086_k050_i005.txt") %>% cap_df(86, 10)
m3 <- read.csv("data/entex_metadata.csv")$TISSUE

all_data[[3]][["Raw"]] <- ed1
all_data[[3]][["KNN"]] <- make_knn(i3, d3)
all_data[[3]][["D3M"]] <- make_animation(i3, 0, instant=FALSE, metadata=m3)

# EXAMPLE 4
rand_ind <- sample(3000, 1000)
r4 <- read.csv("data/scRNA.csv",header=FALSE,sep=",")[rand_ind,]
write.table(r4, "data/scRNA_q.csv", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)
saveRDS(rand_ind, "data/rand_ind.rds")
i4 <- open_result("data/i07_m100_n0001000_k020_i005.txt") %>% cap_df(1000, 10)
d4 <- open_result("data/d07_m100_n0001000_k020_i005.txt") %>% cap_df(1000, 10)
m4 <- read.csv("data/scRNA_labels.csv", header=FALSE)[,1] %>% as.numeric()

all_data[[4]][["Raw"]] <- r4
all_data[[4]][["KNN"]] <- make_knn(i4, d4)
all_data[[4]][["D3M"]] <- make_animation(i4, instant=FALSE, metadata=m4)

# EXAMPLE 5 - Positional
r5 <- read.csv("data/positional.csv",header=FALSE,sep=",")
r5[1,1] <- 56.72837707
r5[,1] <- as.numeric(r5[,1])

i5 <- open_result("data/i07_m010_n0000416_k007_pos.txt") %>% cap_df(416, 7)
d5 <- open_result("data/d07_m010_n0000416_k007_pos.txt") %>% cap_df(416, 7)
m5 <- sprintf("Tier %s", read.csv("data/sports_metadata.csv")$Team_Tier)

all_data[[5]][["Raw"]] <- r5
all_data[[5]][["KNN"]] <- make_knn(i5, d5)
all_data[[5]][["D3M"]] <- make_animation(i5, instant=FALSE, metadata=m5)

# EXAMPLE 6 - Functional
r6 <- read.csv("data/functional.csv",header=FALSE,sep=",")
r6[1,1] <- 59.50225989
r6[,1] <- as.numeric(r5[,1])

i6 <- open_result("data/i07_m009_n0000416_k007.txt") %>% cap_df(416, 7)
d6 <- open_result("data/d07_m009_n0000416_k007.txt") %>% cap_df(416, 7)
m6 <- m5

all_data[[6]][["Raw"]] <- r6
all_data[[6]][["KNN"]] <- make_knn(i6, d6)
all_data[[6]][["D3M"]] <- make_animation(i6, instant=FALSE, metadata=m6)

saveRDS(all_data, "rann_app/data/all_data.rds")