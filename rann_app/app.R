library(shiny)
library(DT)
library(dplyr)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(ndtv)
library(htmlwidgets)

# ----------------
# HELPER FUNCTIONS
# ----------------

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

# a slide for examples
example_slider <- function(id)
{
  sliderInput(id, "In the future, parameters may go here!", 
              min=1, max=100, value=10, step=2, ticks = FALSE)
}

empty_df <- matrix(nrow=0, ncol=1) %>% data.frame()
colnames(empty_df) <- "Unknown"

# Creates a datatable from a data frame
my_datatable <- function(df)
{
  if (class(df) != "data.frame" || ncol(df) < 1)
    df <- empty_df
  
  DT::datatable(
    df, editable=FALSE, escape=TRUE, filter="top", 
    selection="none", options=list(
      scrollX=TRUE,
      scrollY=TRUE,
      autoWidth=FALSE
    )
  )
}

# ---------
# LOAD DATA
# ---------

all_data <- readRDS("data/all_data.rds")

instructions <- "
Hello! Please feel free to explore these data visualizations, which were
generated by Justin Chang, Yasmina Abukhadra, Michael Barresi, Daniel Ortega
Alex Lawson, and Abhinav Godavarthi.
<br><br>
The analyses were done with the Randomized Approximate
Nearest Neighbors Algorithm from (Jones, Osipov, Rokhlin, 2011).
<br><br>
To reset any visualization errors, resize the page. 
A convenient way of doing so is to click the 
upper-right navigation bar icon twice.
<br><br> 
The slider on the bottom controls the timestamp of the animation.
<br><br> 
The buttons on the bottom-right can be used to 
step back, reverse, pause, play, and step forward the animation.
<br><br> 
To change the speed of the animation, click the upper-right visualization icon.
<br><br> 
To move around, click and hold in the visualization.
<br><br> 
To zoom in / out, use the mouse wheel.
<br><br> 
To get information about a point, click on it.
<br><br> 
To highlight a single point and its neighbors, double click on it.
"


# ---------------
# BUILD INTERFACE
# ---------------

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

h3 {
  text-align: center !important;
}
")

ui <- function(request){
  dashboardPage(
    skin="blue",
    dashboardHeader(title="RANN Applications", titleWidth="100%"),
    dashboardSidebar(
      width=300,
      collapsed=TRUE,
      sidebarMenu(
        menuItem(
          "Settings",
          startExpanded = TRUE,
          example_slider("test1"),
          example_slider("test2"),
          example_slider("test3")
        )
      )
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(tags$style(my_css_styling)),
      box(
        title = "Controls",
        collapsible=TRUE, collapsed=FALSE, width="100%",
        # downloadButton('downloadPaper', 'Download Paper'),
        action("instructions", "Instructions", "book", "#FFF", "#9400D3", "#00356B"),
        bookmarkButton()
      ),
      uiOutput("plainTitleUI"),
      tabBox(
        width="100%",
        id = 'plotPanels',
        tabPanel(
          title = "Numbers", 
          h3("Network Dynamic"),
          uiOutput("example0_out") %>% my_spin(),
          h3("Nearest Neighbors"),
          DT::DTOutput("example0_knn") %>% my_spin(),
          h3("Raw Data"),
          DT::DTOutput("example0_raw") %>% my_spin()
        ),
        tabPanel(
          title = "COVID19 (Figure 1)",
          h3("Network Dynamic"),
          uiOutput("example1_out") %>% my_spin(),
          h3("Nearest Neighbors"),
          DT::DTOutput("example1_knn") %>% my_spin(),
          h3("Raw Data"),
          DT::DTOutput("example1_raw") %>% my_spin()
        ),
        tabPanel(
          title = "Birds (Figure 2)", 
          h3("Network Dynamic"),
          uiOutput("example2_out") %>% my_spin(),
          h3("Nearest Neighbors"),
          DT::DTOutput("example2_knn") %>% my_spin(),
          h3("Raw Data"),
          DT::DTOutput("example2_raw") %>% my_spin()
        ),
        tabPanel(
          title = "Tissue Samples (Figure 3)", 
          h3("Network Dynamic"),
          uiOutput("example3_out") %>% my_spin(),
          h3("Nearest Neighbors"),
          DT::DTOutput("example3_knn") %>% my_spin(),
          h3("Raw Data"),
          DT::DTOutput("example3_raw") %>% my_spin()
        ),
        tabPanel(
          title = "scRNAseq (Figure 4)", 
          h3("Network Dynamic"),
          uiOutput("example4_out") %>% my_spin(),
          h3("Nearest Neighbors"),
          DT::DTOutput("example4_knn") %>% my_spin(),
          h3("Raw Data"),
          DT::DTOutput("example4_raw") %>% my_spin()
        ),
        tabPanel(
          title = "Football Positions (Figure 5)", 
          h3("Network Dynamic"),
          uiOutput("example5_out") %>% my_spin(),
          h3("Nearest Neighbors"),
          DT::DTOutput("example5_knn") %>% my_spin(),
          h3("Raw Data"),
          DT::DTOutput("example5_raw") %>% my_spin()
        ),
        tabPanel(
          title = "Football Functions (Figure 6)", 
          h3("Network Dynamic"),
          uiOutput("example6_out") %>% my_spin(),
          h3("Nearest Neighbors"),
          DT::DTOutput("example6_knn") %>% my_spin(),
          h3("Raw Data"),
          DT::DTOutput("example6_raw") %>% my_spin()
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
  
  # downloads the paper
  output$downloadPaper <- downloadHandler(
    filename = function() {
      "paper.zip"
    },
    content = function(file) {
      print("Hi!")
      file.copy("data/paper.zip", file)
    }
  )
  
  # 0
  output$example0_out <- renderUI({
    div(all_data[[7]][["D3M"]])
  })
  
  output$example0_knn <- renderDT({
    my_datatable(all_data[[7]][["KNN"]])
  })
  
  output$example0_raw <- renderDT({
    my_datatable(all_data[[7]][["Raw"]])
  })
  
  # 1
  output$example1_out <- renderUI({
    div(all_data[[1]][["D3M"]])
  })
  
  output$example1_knn <- renderDT({
    my_datatable(all_data[[1]][["KNN"]])
  })
  
  output$example1_raw <- renderDT({
    my_datatable(all_data[[1]][["Raw"]])
  })
  
  # 2
  output$example2_out <- renderUI({
    div(all_data[[2]][["D3M"]])
  })
  
  output$example2_knn <- renderDT({
    my_datatable(all_data[[2]][["KNN"]])
  })
  
  output$example2_raw <- renderDT({
    my_datatable(all_data[[2]][["Raw"]])
  })
  
  # 3
  output$example3_out <- renderUI({
    div(all_data[[3]][["D3M"]])
  })
  
  output$example3_knn <- renderDT({
    my_datatable(all_data[[3]][["KNN"]])
  })
  
  output$example3_raw <- renderDT({
    my_datatable(all_data[[3]][["Raw"]])
  })
  
  # 4
  output$example4_out <- renderUI({
    div(all_data[[4]][["D3M"]])
  })
  
  output$example4_knn <- renderDT({
    my_datatable(all_data[[4]][["KNN"]])
  })
  
  output$example4_raw <- renderDT({
    my_datatable(all_data[[4]][["Raw"]])
  })
  
  # 5
  output$example5_out <- renderUI({
    div(all_data[[5]][["D3M"]])
  })
  
  output$example5_knn <- renderDT({
    my_datatable(all_data[[5]][["KNN"]])
  })
  
  output$example5_raw <- renderDT({
    my_datatable(all_data[[5]][["Raw"]])
  })
  
  # 6
  output$example6_out <- renderUI({
    div(all_data[[6]][["D3M"]])
  })
  
  output$example6_knn <- renderDT({
    my_datatable(all_data[[6]][["KNN"]])
  })
  
  output$example6_raw <- renderDT({
    my_datatable(all_data[[6]][["Raw"]])
  })
}

# -----------
# RUN THE APP
# -----------
shinyApp(ui = ui, server = server, enableBookmarking = "url")