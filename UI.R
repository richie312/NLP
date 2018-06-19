library(shiny)
library(shinythemes)
library(stringi)
library(tm)
library(plyr)
library(stringr)
library(sentimentr)
library(plotrix)
library(tidytext)
library(ggplot2)
library(wordcloud2)
library(twitteR)
library(tuber)
library(dplyr)



## Set the working directory

#setwd("C:/Users/Richie/Desktop/NLP")

shinyUI(fluidPage(theme=shinytheme("cerulean"),       
        navbarPage(title="Text Mining",id="nav",
                   
                   tabPanel("Sentiment Analysis of Trending Entrepreneurs 2018",value="TextMining",includeCSS("mystyle.css"),
                            
                            
                            fluidRow(column(1),column(5,
                                            helpText(h3("Select Trending Entrepreneurs of 2018")),
                                            selectInput(inputId = "Entrepreneurs",label="Select Entrepreneurs",
                                                        choices=c("SundarPichai","Jackma","ElonMusk",
                                                                  "JeffBezos","NaveenJain"),
                                                        selected="Jackma"),
                                            
                                            
                                            imageOutput(outputId="image_plot",width="400",height="300")),
                                     br(),
                                     
                                          
                                     column(6,
                                            helpText(h3("Word Cloud")),
                                            column(6,selectInput("Entrepreneur1",label="Personality",
                                                        choices=c("SundarPichai","Jackma","ElonMusk",
                                                                   "JeffBezos","NaveenJain"),
                                                        selected="ElonMusk"),
                                            wordcloud2Output(outputId = "wordcloud2",width="300px",height="350px")),
                                            column(2),
                                            column(4,br(),br(),br(),br(),helpText(h4("Select Parameters")),br(),
                                                   sliderInput(inputId="number_words",label = "number of words",  min=1,max=200,value=100),
                                                   radioButtons(inputId="Color",label="Select Color",choices=c("random-light","random-dark")))
                                                   )),
                                            
                                            
                                            
                                          
                            
                            fluidRow(column(1),column(11,htmlOutput(outputId ="quote"),style="color: #E36A16;
                                     font-family: 'Freestyle Script';
                                     font-weight: 700;
                                     font-size: 300%;
                                    background-image: url(wood.jpg);")),
                                     fluidRow(column(1),
                                       column(5,
                                            helpText(h3("Different Category of Emotion (3D Pie)")),
                                            column(6,radioButtons(inputId = "lexicon",label="choose lexicon dictionary",
                                                         choices=c("nrc","bing"),selected="nrc")),
                                            column(6,sliderInput(inputId="explode",label="Explode the pie",min=0,max=1,value=0.18,step=0.01)),
                                            plotOutput(outputId = "pie3d")),
                                            
                                            
                                            column(6,helpText(h3("Word Count")),
                                            plotOutput(outputId = "word_count"))
                                            
                                            
                                          ),
                                     fluidRow(column(1),column(5,helpText(h3("Polarity Bar Plot")),
                                              plotOutput(outputId = "Bar_Plot", width="450px")),
                                              
                                          column(6,
                                          helpText(h3("Trending Videos with Number of likes and views")),
                                          selectInput("Category",label="Select Category",choices=c("viewCount",
                                                                                                   "likeCount",
                                                                                                   "dislikeCount",
                                                                                                   "commentCount"),
                                                      selected = c("viewCount")),
                                          plotOutput(outputId = "table_views"))
                                            
                                            )
                     
                   ),
                   tabPanel("Data Source and Code",value="Resources",
                            fluidRow(column(12)))
                )
            )
        
  )


