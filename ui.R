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
             badgeColor = "blue",
             menuSubItem("ComBat", tabName = "combat", icon = icon("angle-left")),
             menuSubItem("RandomForest", tabName = "rf", icon = icon("angle-left"))
             ),
    menuItem("ReadMe", tabName = "readme",badgeLabel = "help", badgeColor = "red", icon=icon("mortar-board")),
    menuItem("About", tabName = "about", icon = icon("question"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h3("Welcome to Batch Server home"),
            h5("Batch effects are unwanted sources of variation irrelevant to biological variation inevitably introduced to the samples during experimental handling which would obscure the biological signal. Batch effects are one of the biggest challenges faced by high throughput omics science, especially in the context of large cohort of thousands of samples. Existing batch effect-correcting tools focus mainly on the development of methods that are not convenience of use, usually requiring extensive coding experiences, sometimes even need to know the prior distribution of the data. Moreover, few tools offer both evaluation and correction of batch effects. We developed an open-source web server-based batch effect correction tool, namely BatchServer, which enables users to interactively evaluate and correct batch effects of a variety of omics data.")
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
            checkboxInput("qn", "Quantile normalization", FALSE),
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
            h3("PVCA"),
            h4('PVCA assess the batch sourcs by fitting all "sources" as random effects including two-way interaction terms in the Mixed Model(depends on lme4 package) to selected principal components, which were obtained from the original data correlation matrix. Pierre Bushel (2019). pvca: Principal Variance Component Analysis (PVCA). R package version 1.24.0.'),
            selectInput("pvca_effect_name","Select Contributing Effect Column Name(s)",
                        choices = effect_name,multiple = T),
            sliderInput("pvca_threshold", "Set the percentile value of the minimum amount of the variabilities that the selected principal components need to explain",
                        min = 0, max = 1,
                        value = 0.7, step = 0.1),
            actionButton("pvca_submit", "Submit", class = "btn-primary"),
            tags$hr(),
            tabsetPanel(
            tabPanel(
              "Pieplot",
              plotOutput("draw_pie"),
              uiOutput("pvca_pie_ui")
              ),
            tabPanel(
              "Barplot",
              column(12,plotOutput("draw_pvca"),
                     uiOutput("pvca_ui"))
            )
            )
    ),
    tabItem(tabName = "umap",            
            h3("UMAP"),
            h4("Uniform Manifold Approximation and Projection (UMAP) is a dimension reduction technique that can be used for visualisation similarly to t-SNE, but also for general non-linear dimension reduction."),      
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
            
            actionButton("umap_submit", "Calculate", class = "btn-primary"),
            verbatimTextOutput("umap_note"),
            tags$hr(),
            selectInput("umap_effect_name","Select Contributing Effect Column Name(s)",
                        choices = effect_name,multiple = F),
            plotlyOutput("draw_umap"),
            uiOutput("umap_ui")
    ),
    tabItem(tabName = "combat",
            h3("Description:"),
            h4("The ComBat function adjusts for known batches using an empirical Bayesian
framework. So known batch variable is required in your dataset. Here you should pay attention to the [parametric estimate method] choice, which was improved compare to the original ComBat method. The option [automatic] will automatically decide to set parametric estimate method to parametric or nonparametric according to the data distribution."),
            hr(),
            selectInput("batch_effect_name","Select Known Batch Effect Column Name",
                        choices = NULL,multiple = F),
            selectInput("adjust_variables","Select adjustment variable(s)",
                        choices = NULL,multiple = T),
            radioButtons("par.prior", "Parametric estimate method",
                         choices = c(automatic= "auto",
                                     parameter = "parameter",
                                     noparameter = "noparameter"),
                         selected = "auto",inline = T),
            # radioButtons("fit.method", "Fitness method",
            #              choices = c("maximum likelihood" = "mle",
            #                          "moment matching" =  "mme",
            #                          "quantile matching" = "qme",
            #                          "maximizing goodness-of-fit estimation" = "mge"),
            #              selected = "mle",inline = T),
            radioButtons("mean.only", "Only adjusts the
mean of the batch effects across batches (default adjusts the mean and variance)",
                         choices = c(No = FALSE,
                                     Yes = TRUE),
                         selected = FALSE,inline = T),
            actionButton("elimination_submit", "Elimination", class = "btn-primary"),
            verbatimTextOutput("combat_log"),
            uiOutput("combat_ui")
    ),
    tabItem(tabName = "rf",
            h3("Description:"),
            h4("Remove most importances batch related variables using Random Forest "),
            hr(),
            selectInput("batch_effect_name_rf","Select Known Batch Effect Column Name",
                        choices = NULL,multiple = F),
            numericInput("ntree", "Number of trees to grow", 500,
                         1, 5000, 1),
            numericInput("nodesize", "Minimum size of terminal nodes", 5,
                         1, 500, 1),
            numericInput("topN", "Number of top effect batch related variables to delete", 5,
                         1, 500, 1),
            actionButton("rf_submit", "Submit", class = "btn-primary"),
            verbatimTextOutput("rf_log"),
            uiOutput("rf_ui")
    ),
    tabItem(tabName = "readme",
            h3("Test Data Download"),
            h5("There are two type of files users should prepare in order to use BatchServer:
data matrix file and sample information file. The format of these two files can be tab-delimited or space-separated .txt file or comma-delimited .csv file.
Here is an example of a data file and sample information file: "),
            h5("Sample information file:
The first column must contain the names of the samples (column names) as in your data file. The columns after sample name include batch and covariate name. Note since ComBat only deals with categorical covariates, numerical covariates have not been supported by BatchEffect currently.
"),            
            downloadButton("sampleData_download", "sampleInfo", class = "btn-primary"),
            h5("Data file:
The first column must contain the features (such as, protein or gene name). The first row must contain all sample name as exectly as in your sample information file.
"),            
            downloadButton("testData_download", "dataMatrix", class = "btn-primary"),

            h5("")
            ),
    tabItem(tabName = "about",
            h3("Author:"),
            h5("Tiansheng Zhu; tszhu @ fudan.edu.cn"),
            h3("License:"),            
            h5("Batch Server is an open-source software implemented in pure R language and the source code is freely available https://github.com/tszhu/webBatch. 
Now Batch Server is supported by both zhou’s lab of Fudan University (admis.fudan.edu.cn) and guomics lab of Westlake University (www.guomics.com). The software is published by ''")         
    )
  )
)
dashboardPage(skin = "purple",
                    dashboardHeader(title = "BatchServer"),
                    sidebar,
                    body
)
