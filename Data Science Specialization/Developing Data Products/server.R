
library(shiny) 
data(mtcars)

prediction<-function(cyl1,hp1,wt1,am1,conf1) {
  pred <- lm(mpg~cyl + hp + wt + am, data = mtcars)
  new_preds <- data.frame(cyl=cyl1,hp=hp1,wt=wt1,am=am1)
  p_pred <- predict(pred,newdata = new_preds, interval = "prediction",level = conf1/100)[1]
  p_lwr <- predict(pred,newdata = new_preds, interval = "prediction",level = conf1/100)[2]
  p_upr <- predict(pred,newdata = new_preds, interval = "prediction",level = conf1/100)[3]
  cat('Predicted mileage is ',round(p_pred,2), ' mpg with a ', conf1,'% prediction interval of (', round(p_lwr,2),' mpg,',round(p_upr,2),' mpg).')

}


shinyServer(
  function(input, output) {
    
    output$inputcylvalue <- renderPrint({input$cyl1})
    output$inputhpvalue <- renderPrint({input$hp1})
    output$inputwtvalue <- renderPrint({input$wt1})
    output$inputamvalue <- renderPrint({input$am1})
    output$inputconfvalue <- renderPrint({input$conf1})
    output$prediction <- renderPrint({prediction(input$cyl1,input$hp1,input$wt1,input$am1,input$conf1)})
    
  }

)
