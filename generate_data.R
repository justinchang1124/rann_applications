library(stringi)
library(ndtv)
library(htmlwidgets)

testflat <- rep(1:100, 100)
test <- matrix(testflat, nrow=100)
write.table(test, "~/rann_applications/test.csv", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

make_animation <- function(data, num_neighbors)
{
  wheel <- network.initialize(nrow(data))
  
  for (i in 1:nrow(data))
  {
    add.edges.active(wheel, tail=i, head=as.numeric(data[i,1:num_neighbors]), 
                     onset=i, terminus=nrow(data)+1)
  }
  
  compute.animation(wheel, animation.mode = "kamadakawai",
                    slice.par=list(start=0, end=nrow(data), interval=1,
                                   aggregate.dur=1, rule='any'))
  
  knn <- render.d3movie(
    wheel, 
    vertex.tooltip = 1:nrow(data), 
    vertex.cex = 1, 
    edge.lwd = 3
    ,output.mode='htmlWidget'
  )
  
  knn$sizingPolicy$defaultWidth = "100%"
  
  knn
}


for (i in 1:10)
{
  if (i == 1)
    add.edges.active(wheel, tail=i, head=2:3, onset=i, terminus=11)
  if (i > 1 && i < 10)
    add.edges.active(wheel, tail=i, head=c(i-1, i+1), onset=i, terminus=11)
  if (i == 10)
    add.edges.active(wheel, tail=i, head=8:9, onset=i, terminus=11)
}

# 
# 
# # wheel <- network.initialize(10) # create a toy network
# # add.edges.active(wheel,tail=1:9,head=c(2:9,1),onset=1:9, terminus=11)
# # add.edges.active(wheel,tail=10,head=c(1:9),onset=10, terminus=12)
# plot(wheel) # peek at the static version
# render.animation(wheel) # compute and render
# ani.replay()

# nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
# links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
# 
# net3 <- network(links,  vertex.attr=nodes, matrix.type="edgelist",
#                 loops=F, multiple=F, ignore.eval = F)
# 
# vs <- data.frame(onset=0, terminus=50, vertex.id=1:17)
# es <- data.frame(onset=1:49, terminus=50,
#                  head=as.matrix(net3, matrix.type="edgelist")[,1],
#                  tail=as.matrix(net3, matrix.type="edgelist")[,2])
# 
# net3.dyn <- networkDynamic(base.net=net3, edge.spells=es, vertex.spells=vs)
# 
# compute.animation(net3.dyn, animation.mode = "kamadakawai",
#                   slice.par=list(start=0, end=50, interval=1,
#                                  aggregate.dur=1, rule='any'))
# 
# color_seq <- function(n_colors)
# {
#   return(hcl(1:n_colors * (360/(n_colors+1))-15, 160, 60))
# }
# 
# hmm <-as.numeric(as.factor(net3.dyn %v% "type.label"))
# colors <- color_seq(length(unique(hmm)))[hmm]
# 
# saveRDS(colors, "colors.rds")

# target <- render.d3movie(net3.dyn, usearrows = F,
#                displaylabels = F, label=net3 %v% "media",
#                bg="#ffffff", vertex.border="#333333",
#                vertex.cex = degree(net3)/2,
#                vertex.col = colors,
#                edge.lwd = (net3.dyn %e% "weight")/3,
#                edge.col = '#55555599',
#                vertex.tooltip = paste("<b>Name:</b>", (net3.dyn %v% "media") , "<br>",
#                                       "<b>Type:</b>", (net3.dyn %v% "type.label")),
#                edge.tooltip = paste("<b>Edge type:</b>", (net3.dyn %e% "type"), "<br>",
#                                     "<b>Edge weight:</b>", (net3.dyn %e% "weight" ) ),
#                launchBrowser=T, filename="temp.html",
#                render.par=list(tween.frames = 30, show.time = F),
#                plot.par=list(mar=c(0,0,0,0)), output.mode='htmlWidget' )

# saveRDS(target, "target.rds")

target$sizingPolicy$defaultWidth = "100%"

my_empty_list <- function(names)
{
  target <- vector(mode="list", length=length(names))
  names(target) <- names
  target
}


knn <- readLines("i07_m100_n0000100_k050_i005.txt")
h2 <- repStr(knn, " ", "")
h3 <- as.numeric(h2)
h4 <- data.frame(matrix(h3, ncol=50, byrow = TRUE))

all_data <- my_empty_list(sprintf("Example %s", 1:6))
for (i in 1:6)
{
  all_data[[i]] <- my_empty_list(c("Raw", "KNN"))
  
  all_data[[i]][["Raw"]] <- read.csv("test.csv", sep=" ", header=FALSE)
  all_data[[i]][["KNN"]] <- my_empty_list(sprintf("KNN%s", 1:20))
  
  for (j in 1:20)
  {
    all_data[[i]][["KNN"]][[j]] <- make_animation(data, j)
  }
}

saveRDS(all_data, "rann_app/all_data.rds")

# fixed pattern replacement in a vector of strings
repStr <- function(x_stringi, pattern, replacement)
{
  stri_replace_all_fixed(
    x_stringi, pattern = pattern, 
    replacement = replacement, 
    vectorize_all = FALSE)
}