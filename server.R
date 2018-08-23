#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
# http://shiny.rstudio.com/
#
library(Rfacebook)
library(rjson)
library(RCurl)
library(httr)
library(shiny)
library(httpuv)
library(xlsx)
library(V8)
library(DT)
library(plotrix)
library(yarrr)
library(xlsxjars)
# Define server logic
source('script.R',local = TRUE)
shinyServer(function(input, output) {
  
  
  
  result<- eventReactive(input$goButton, {
    switch(input$dataset,
           "posts"=ScrappingPosts(input$entree_texte,input$entree_token),
           "comments"=ScrappingComms(input$entree_texte,input$entree_token))
  })
  output$Tables<-renderTable({
   result()
  })
  
  #wordcloud_rep <- repeatable(wordcloud)
  
  output$plots<-renderPlot({
    switch(input$dataset,
           "posts"= Functionwordcloud(result()),
           "comments"={
             x<-centre_dinteret(result())
             wordcloud_Comms(mots_frequentes(x))
           } )
    
  })
  output$download<-downloadHandler(
    filename = function() {
      paste(input$entree_texte,".xls")
    },
    content = function(file) {
      #wb<-createWorkbook(type="xls")
      #addDataFrame(as.data.frame(result()),"Sheet1", col.names=TRUE, row.names=TRUE)
      write.xlsx(result(),file ,sheetName = "Sheet1")
    },
    contentType = "xls"
  )
  output$centres<-renderPlot({
    switch(input$dataset,
           "comments"={
             x<-centre_dinteret(result())
             ggplot(mots_frequentes(x),aes(reorder(mots_frequentes(x)$Mots,mots_frequentes(x)$frequences),reorder(mots_frequentes(x)$frequences,mots_frequentes(x)$Mots), fill =mots_frequentes(x)$frequences)) +
               geom_col() +
               guides(fill = FALSE) +
               labs(x = NULL, y = "Frequences") +
               ggtitle("Sujets en discussion") +
               coord_flip()
           } )
    
  })
  output$analysesentimentale<-renderPlot({
    switch(input$dataset,
           "comments"={
             y<-centre_dinteret(result())
             l<-analyse_sentimentale(y)
             pie3D(l$frequences, labels = l$Mots, main = "Analyse sentimentale", explode=0.1, radius=.9, labelcex = 1.1,  start=1.9,col = brewer.pal(n=2,name='YlGnBu'))
           } )
    
  })
  output$max <- renderValueBox({
    x<-max(result()$likes_count)
    valueBox(
     "Ils ont aimé votre publication:", 
     value = x,
     icon = icon("users")
    )
  })
  output$somme <- renderValueBox({
    x<-sum(result()$likes_count)
    valueBox(
      
      "la somme des likes",
      value = x,
      icon = icon("area-chart"),
      color = "yellow"
    )
  })
  
})