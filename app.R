library(shiny)
library(dplyr)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(ndtv)
library(htmlwidgets)

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

target <- readRDS("target.rds")

instructions <- "Hi! Instructions go here!"
citations <- "Hi! Citations go here!"

# Creates an action button with the given id, name, icon name,
# color, background color, and border color.
action <- function(id, name, icon_name, color, bk, br)
{
  actionButton(
    inputId = id, label = name, icon = icon(icon_name), style=
      sprintf("color: %s; background-color: %s; border-color: %s", color, bk, br))
}

# adds a spinner to content that may need to be refreshed
my_spin <- function(content)
{
  content %>% withSpinner(type = 6)
}

my_css_styling <- HTML("
/* Everyone's favorite color - Yale Blue! */
.skin-blue .main-header .logo {
  background-color: #00356B !important;
}

/* Place sidebar toggle on right! */
.sidebar-toggle {
  float: right !important;
}

/* Prevents weird sidebar glitch */
.wrapper {
  height: auto !important; 
  position: relative !important; 
  overflow-x: hidden !important; 
  overflow-y: hidden !important;
}
")

ui <- function(request){
  dashboardPage(
    skin="blue",
    dashboardHeader(title="RANN Applications", titleWidth="100%"),
    dashboardSidebar(
      width=300,
      sidebarMenu(
        menuItem(
          "Settings",
          startExpanded = TRUE,
          sliderInput("neighbors1", "Neighbors", 
                      min=1, max=1000, value=20, step=1, ticks = FALSE),
          sliderInput("iterations1", "Number of Iterations", 
                      min=1, max=20, value=10, step=1, ticks = FALSE)
          
        )
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(tags$style(my_css_styling)),
      box(
        title = "Controls",
        collapsible=TRUE, collapsed=FALSE, width="100%",
        downloadButton('downloadData', 'Download All Data'),
        downloadButton('downloadPaper', 'Download Paper'),
        action("instructions", "Instructions", "book", "#FFF", "#9400D3", "#00356B"),
        action("citations", "Citations", "book", "#FFF", "#9400D3", "#00356B"),
        bookmarkButton()
      ),
      uiOutput("plainTitleUI"),
      tabBox(
        width="100%",
        height = "800px",
        id = 'plotPanels',
        tabPanel(
          title = "Example 1", 
          uiOutput("example1_out")
        )
      )
    )
  )
}

# ------------------------------------------
# REACTIVE SERVER: INITIALIZATION, OBSERVERS
# ------------------------------------------

server <- function(input, output, session) {
  # shows instructions
  observeEvent(input$instructions, {
    showModal(modalDialog(
      title = HTML("<b>Instructions</b>"), 
      HTML(instructions)
    ))
  })
  
  # shows citations
  observeEvent(input$citations, {
    showModal(modalDialog(
      title = HTML("<b>Citations</b>"), 
      HTML(citations)
    ))
  })
  
  output$example1_out <- renderUI({
    div(target)
  })
}

# -----------
# RUN THE APP
# -----------
shinyApp(ui = ui, server = server, enableBookmarking = "url")