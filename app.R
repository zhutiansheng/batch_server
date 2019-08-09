library(shiny)
library(shinydashboard)
sidebar <- dashboardSidebar(
  sidebarMenu(id="mysidebar",
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Data Input", icon = icon("th"), tabName = "dataInput",
             badgeLabel = "matrix", badgeColor = "green"),
    menuItem("Evaluatation", icon = icon("angle-right"), tabName = "evaluatation",
             badgeColor = "yellow",
             menuSubItem("PVCA", tabName = "pvca", icon = icon("angle-left")),
             menuSubItem("UMAP", tabName = "umap", icon = icon("angle-left"))
             ),
    menuItem("Elimination", icon = icon("line-chart"), tabName = "elimination",
             badgeLabel = "remove", badgeColor = "blue"),
    menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
    menuItem("About", tabName = "about", icon = icon("question"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h2("This is home")
    ),
    
    tabItem(tabName = "dataInput",
            # Input: Select a file ----
            fileInput("myd", "Please Choose Your Data File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ",",inline = T),
            
            # Horizontal line ----
            tags$hr(),
            # Input: Select a file ----
            fileInput("sample_info", "Please Choose Your Sample Information File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            # Input: Checkbox if file has header ----
            checkboxInput("sample_header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sample_sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ",",inline = T),
            
            # Horizontal line ----
            tags$hr(),
            actionButton("input_submit", "Submit", class = "btn-primary")
    ),
    
    tabItem(tabName = "pvca",
            selectInput("pvca_effect_name","Select Contributing Effect Column Name(s)",
                        choices = effect_name,multiple = T),
            actionButton("pvca_submit", "Submit", class = "btn-primary")
    ),
    tabItem(tabName = "umap",
            sliderInput("n_neighbors", "number of nearest neighbors:",
                        min = 1, max = 100,
                        value = 15),
            radioButtons("metric", "distances method",
                         choices = c(euclidean = "euclidean",
                                     manhattan = "manhattan",
                                     cosine = "cosine",
                                     pearson = "pearson"),
                         selected = "euclidean",inline = T),
            sliderInput("n_epochs", "number of  iterations:",
                        min = 1, max = 1000,
                        value = 200),
            radioButtons("init", "initial coordinates",
                         choices = c(spectral = "spectral",
                                     random = "random"),
                         selected = "spectral",inline = T),
            sliderInput("min_dist", "minimumn dist in the final layout",
                        min = 0.01, max = 1,
                        value = 0.1, step = 0.01),
            numericInput("alpha", "initial value of 'learning rate'", 1,
                         0.1, 10, 0.1),
            numericInput("gamma", "learning rate", 1,
                         0.1, 10, 0.1),
            numericInput("negative_sample_rate", "non-neighbor points are used per point and
                         per iteration during layout optimization", 5,
                         1, 100, 1),
            
            actionButton("umap_submit", "Set Parameters", class = "btn-primary"),
            tags$hr(),
            tags$h3("Select following to (re)draw UMAP"),
            selectInput("umap_effect_name","Select Contributing Effect Column Name(s)",
                        choices = effect_name,multiple = F),
            plotOutput("draw_umap"),
            uiOutput("umap_ui")
    ),
    tabItem(tabName = "elimination",
            h2("elimination tab content")
    )
  )
)

shiny::runApp(
  #port = 80,
  display.mode = "auto",
  host = getOption("shiny.host", "0.0.0.0"),
  quiet = T
  #test.mode = T
)
