
library(shiny)

# Define UI for application that predicts gas mileage
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict your car's mileage"),
  
  # Sidebar with a slider input for number cylinders, horsepower, weight and transmission
  sidebarLayout(
    sidebarPanel(
      sliderInput("cyl1", "Number of Cylinders:", min = 4,max = 12,value = 6),
      sliderInput("hp1","Estimated horsepower:", min = 50,max = 600, value = 250),
      sliderInput("wt1","Weight in kg:", min = 1,max = 10, value = 2),
      sliderInput("am1","Transmission (0 = automatic, 1 = manual):", min = 0,max = 1, value = 0,step = 1),
      numericInput("conf1","Desired Prediction Interval Confidence (%):  ",min=50,max=100,value= 90),
      submitButton('Predict mileage')
    ),
    
    # Show the output along with prediction interval
    mainPanel(
       p('Predicted gas mileage based on inputs:'),
       verbatimTextOutput("prediction")
    )
  )
))
