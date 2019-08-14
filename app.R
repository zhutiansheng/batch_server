source("global.R")
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
            actionButton("input_submit", "Submit", class = "btn-primary"),
            verbatimTextOutput("upload_note")
    ),
    
    tabItem(tabName = "pvca",
            
            selectInput("pvca_effect_name","Select Contributing Effect Column Name(s)",
                        choices = effect_name,multiple = T),
            sliderInput("pvca_threshold", "Set the percentile value of the minimum amount of the variabilities that the selected principal components need to explain",
                        min = 0, max = 1,
                        value = 0.7, step = 0.1),
            actionButton("pvca_submit", "Submit", class = "btn-primary"),
            tags$hr(),
            tabsetPanel(
              tabPanel(
                "Barplot",
            plotOutput("draw_pvca"),
            uiOutput("pvca_ui")
              ),
            tabPanel(
              "Pieplot",
              plotOutput("draw_pie"),
              uiOutput("pvca_pie_ui")
              )
            )
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
            selectInput("batch_effect_name","Select Known Batch Effect Column Name",
                        choices = NULL,multiple = F),
            selectInput("adjust_variables","Select adjustment variable(s)",
                        choices = NULL,multiple = T),
            radioButtons("par.prior", "parametric estimate method",
                         choices = c(auto = "automatic",
                                     parameter = "parameter",
                                     noparameter = "noparameter"),
                         selected = "automatic",inline = T),
            radioButtons("fit.method", "fitness method",
                         choices = c("maximum likelihood" = "mle",
                                     "moment matching" =  "mme",
                                     "quantile matching" = "qme",
                                     "maximizing goodness-of-fit estimation" = "mge"),
                         selected = "mle",inline = T),
            radioButtons("mean.only", "Only adjusts the
mean of the batch effects across batches (default adjusts the mean and variance)",
                         choices = c(No = FALSE,
                                     Yes = TRUE),
                         selected = FALSE,inline = T),
            actionButton("elimination_submit", "Elimination", class = "btn-primary"),
            verbatimTextOutput("combat_log"),
            uiOutput("combat_ui")
    )
  )
)
options(shiny.reactlog = TRUE)
options(shiny.trace = TRUE)
options(shiny.fullstacktrace = TRUE)
options(shiny.error = browser)
shiny::runApp(
  port = 80,
  display.mode = "auto",
  host = getOption("shiny.host", "0.0.0.0"),
  quiet = F,
  test.mode = T
)
