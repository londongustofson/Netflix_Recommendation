cat("\014") # Clear console
rm(list = ls()) # Clear environment

library(dplyr)
library(tidyr)
library(reshape2)
library(DT)

ratings <-read.csv("ratings_Whiskers.csv")
movies <- read.csv("movies_Whiskers.csv")
# Merge both files by movieId 
movieratings <- merge(ratings,movies,by=c('movieId'),all.x=T)
rm (ratings)

# Drop the columns of the dataframe
movieratings <- select (movieratings,-c(col.1,timestamp,genres))

# Filter out movies that have less than 15 ratings
movieratings_new <-  filter(movieratings %>% group_by(userId) %>% filter(n()<250)) 
movieratings<-movieratings %>% anti_join(movieratings_new)
rm(movieratings_new)

#R Shiny App

library(shiny)
library(shinyWidgets)

ui<-fluidPage(
  
  img(src ="https://images-eu.ssl-images-amazon.com/images/I/616QXs8yg0L.png", height = 200, width = 600,style="display: block; margin-left: auto; margin-right: auto"),
  
tags$h1("You give us three seconds...we’ll give you the world",align="center",style="color:white"),
setBackgroundColor("brown"),

fluidRow(

column(width=4,selectInput("select_movie1", label = tags$h2("Choose a Movie",style="color:white"),
                          choices = as.character(movies$title[1:length(unique(movies$movieId))]),
                          selectize = TRUE),
              selectInput("Rate1", label = tags$h3("Rating 1-5",style="color:white"),
                          choices = as.factor(c("1.0","1.5","2.0","2.5","3.0","3.5","4.0","4.5","5.0")),
                          selectize = TRUE)),
   column(width=4,selectInput("select_movie2", label = tags$h2("Choose a Movie",style="color:white"),
                          choices = as.character(movies$title[1:length(unique(movies$movieId))]),
                          selectize = TRUE),
              selectInput("Rate2", label = tags$h3("Rating 1-5",style="color:white"),
                          choices = as.factor(c("1.0","1.5","2.0","2.5","3.0","3.5","4.0","4.5","5.0")),
                          selectize = TRUE)),
   column(width=4,selectInput("select_movie3", label = tags$h2("Choose a Movie",style="color:white"),
                          choices = as.character(movies$title[1:length(unique(movies$movieId))]),
                          selectize = TRUE),
              selectInput("Rate3", label = tags$h3("Rating 1-5",style="color:white"),
                          choices = as.factor(c("1.0","1.5","2.0","2.5","3.0","3.5","4.0","4.5","5.0")),
                          selectize = TRUE)),
     column(width=4,selectInput("select_movie4", label = tags$h2("Choose a Movie",style="color:white"),
                          choices = as.character(movies$title[1:length(unique(movies$movieId))]),
                          selectize = TRUE),
              selectInput("Rate4", label = tags$h3("Rating 1-5",style="color:white"),
                          choices = as.factor(c("1.0","1.5","2.0","2.5","3.0","3.5","4.0","4.5","5.0")),
                          selectize = TRUE)),
    column(width=4,selectInput("select_movie5", label = tags$h2("Choose a Movie",style="color:white"),
                          choices = as.character(movies$title[1:length(unique(movies$movieId))]),
                          selectize = TRUE),
              selectInput("Rate5", label = tags$h3("Rating 1-5",style="color:white"),
                          choices = as.factor(c("1.0","1.5","2.0","2.5","3.0","3.5","4.0","4.5","5.0")),
                          selectize = TRUE)),

#actionButton(inputId = "Button",label="Submit"),
#tableOutput(outputId="MoviesRecomended")

     column(width=4,align="center",offset=4,actionButton(inputId = "Button",label="Submit My Ratings"),tags$style(type='text/css', "#button { vertical-align- left; height- 50px; width- 100%; font-size- 60px;}")
     ),

  column(width=4,align="center",offset=4,tableOutput(outputId="MoviesRecomended"),style="color:white;text-style:bold;font-size:18px;font-family: Arial;font-style:italic;")
))


       
       
       
       

server<-function(input, output){
  

data<-eventReactive(input$Button,{
  data.frame(c("0","0","0","0","0"),c(input$Rate1,input$Rate2,input$Rate3,input$Rate4,input$Rate5),c(input$select_movie1,input$select_movie2,input$select_movie3,input$select_movie4,input$select_movie5))
})


x <- reactive({
  testdata <- data()
  colnames(testdata) <- c("userId", "rating", "title")
  testdata$title<-as.character(testdata$title)
  testdata
})

x1 <- reactive({
  x_reactive <- x()
  movies$title <- as.character(movies$title)
  x_reactive2<-left_join(x_reactive, movies, by = c("title"="title"))
  colnames(x_reactive2) <- c("userId", "rating","title","movieId","col1")
  x_reactive2<-select(x_reactive2,c("movieId","userId","rating","title"))
  x_reactive2
})

x2<-reactive({
  x1_reactive <- x1()
  movieratings$userId <- as.factor(movieratings$userId)
  movieratings$rating <- as.factor(movieratings$rating)
  x2_reactive <- rbind(x1_reactive, movieratings)
  x2_reactive$rating <- as.numeric(x2_reactive$rating)
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  x2_reactive$rating_nor <- ave(x2_reactive$rating, x2_reactive$userId, FUN=normalize) 
  x2_reactive
})


similarity_matrix<-reactive({
  x2_reactive <- x2()
  RatingsMatrix<-acast(x2_reactive, userId~title, value.var="rating_nor", fun.aggregate=NULL, fill=0.0)
  Matrix <- as.matrix(RatingsMatrix)
  similarity_matrix_ <- Matrix/sqrt(rowSums(Matrix * Matrix))
  similarity_matrix_ <- similarity_matrix_ %*% t(similarity_matrix_)
  similarity_matrix_
})


ClosestUsersRatings <- reactive({
  sim_matrix <- similarity_matrix()
  ClosestUserInd<-sort(abs(sim_matrix[-1,1] - sim_matrix[1,1]), index.return=TRUE)$ix[1:5] + 1
  userId <- as.character(unique(x2()$userId)[(ClosestUserInd)])
    ClosestUsers_ <- as.data.frame(userId)
    ClosestUsers_$userId <-as.character(userId)
    
    #Getting the distance among users from the matrix 
    similarity_matrix_1_<-as.data.frame(sim_matrix)
      similarity_matrix_1_$userId<-as.character(unique(x2()$userId))
      names(similarity_matrix_1_)[1] <- "user_base"
      similarity_matrix_1_<-select(similarity_matrix_1_,c("userId","user_base"))
      similarity_matrix_1_
      similarity_matrix_1_

  # Build a matrix with the closest users and the movies
movieratings$userId <- as.character(movieratings$userId)
z<-select(left_join(ClosestUsers_, movieratings, by = c("userId"="userId")),c("movieId","title","userId","rating"))

z1<-left_join(z, similarity_matrix_1_, by = c("userId"="userId"))
z1$rating <- as.numeric(z1$rating)
z1$score<-z1$rating*z1$user_base

ClosestUsersRatings_<-acast(z1, userId~title, value.var="score", fun.aggregate=NULL)
ClosestUsersRatings_
})


Final_Prediction_ <- reactive({
  ClosestUsersRatings_<-ClosestUsersRatings()
  
  prediction<-colMeans (ClosestUsersRatings_, na.rm = TRUE)
  
  # top 50 prediction
  prediction1<-as.data.frame(head(sort(prediction, decreasing=TRUE), 50))
  prediction1$"Movies You Might Enjoy!"<-as.character(rownames(prediction1),style="color:white")
  names(prediction1)[1] <- "top_movies"
  
  # filter the movies that the user has watched
  

  final_prediction <-filter(prediction1,!"Movies You Might Enjoy!"%in%c(input$select_movie1,input$select_movie2,input$select_movie3,input$select_movie4,input$select_movie5))

   
  #Top 10 recomendations
 top_n(final_prediction,10)[2]
 })



#Display 10 recomendations



output$MoviesRecomended<- renderTable({
  head(Final_Prediction_(),n = 10L)
  })
}

shinyApp(ui=ui,server=server)



