library(shiny)
library(shinydashboard)
library(shinythemes)
library(tm)
library(V8)
library(NLP)
library(markdown)
# Define UI for application that draws WORDCLOUD

  shinyUI(dashboardPage(
    dashboardHeader(title = "Facebook Analysis", disable = FALSE),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("wordcloud", tabName = "wordcloud", icon = icon("dashboard")),
        menuItem("data", tabName = "data", icon = icon("th"))
      ),
      textInput(inputId = "entree_texte", # nom associé à cet élément de contrôle, sera utilisé dans la partie 'server'
                label = "Entrez l'id/nom de la page Facebook:", # libellé associé à cet élément de contrôle
                value = "") # valeur par défaut
      ,
      
      textInput(inputId = "entree_token", # nom associé à cet élément de contrôle, sera utilisé dans la partie 'server'
                label = "Entrez le jeton d'accés:", # libellé associé à cet élément de contrôle
                value = "EAADZBNIyEfNABAOmS313C3NxfaIGMPbZBWicy5ZAL18wP6dIICQFW5ZCeu2v8osU9dCPEKthRJz6e0zZAMW60EGEeMJgnObWcpqpRZAZCj0NgPaDuOqD01cAEDxUWFKjVrIc3BE4Vnvg1r2ig5D4CJZABHZCqBsO5CtJrqROXX5qGkQZDZD"), # valeur par défaut
      #actionButton('Analyser',"Analyze")
      selectInput("dataset", label="Data Set", 
                  choices=c(" ","posts", "comments")),
      actionButton("goButton", "Scrap!"),
      downloadButton('download', 'Download')
    ),
    ## Body content
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
      tabItems(
        # First tab content
        tabItem(tabName = "wordcloud",
                fluidRow(
                  box(plotOutput("plots")),
               box(plotOutput("analysesentimentale")),
                box(plotOutput("centres")))
                
                ),
        
        # Second tab content
        tabItem(tabName = "data",
                valueBoxOutput("max"),
                valueBoxOutput("somme"),
                tableOutput('Tables')
        )
      )
    )
    )
  )
  