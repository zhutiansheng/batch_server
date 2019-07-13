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
            h2("dataInput tab content")
    ),
    
    tabItem(tabName = "pvca",
            h2("pvca tab content")
    ),
    tabItem(tabName = "elimination",
            h2("elimination tab content")
    )
  )
)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "BatchEffect"),
  sidebar,
  body
)
server <- function(input, output) {
}
shinyApp(ui, server)