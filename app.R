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
            selectInput("umap_effect_name","Select Contributing Effect Column Name(s)",
                        choices = effect_name,multiple = F),
            actionButton("umap_submit", "Submit", class = "btn-primary"),
            plotOutput("draw_umap")
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