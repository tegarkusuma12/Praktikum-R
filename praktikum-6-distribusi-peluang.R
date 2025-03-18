#Diketahui data
x <- c(0,1,2,3)
f <- c(1/8,3/8,3/8,1/8)

#Menentukan mean, variansi, simpangan baku, dan peluang kumulatif
mu <- sum(x*f) 
mu 


#coba library shiny
library(shiny)
ui<-fluidPage(numericInput(inputId="n", "Samplesize", value=25),
              plotOutput(outputId="hist"))
server<-function(input,output){
  output$hist<-renderPlot({hist(rnorm(input$n))})
}