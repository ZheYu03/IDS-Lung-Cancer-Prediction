source("SVM_KNN.R")

library(shiny)
library(shinythemes)


ui <- shinyUI(fluidPage(
  # Application title
  titlePanel("Prediction of Lung Cancer Risk"),
  theme = shinytheme("darkly"),
  
  sidebarLayout(
    # Obtain data from user to predict
    sidebarPanel(
      selectInput("GENDER", "GENDER: ", 
                  choices = list("M", "F")
      ),
      numericInput("AGE","Age :",
                   min = min(data$AGE),
                   max = max(data$AGE),
                   value = as.integer(20)
      ),
      numericInput("SMOKING","SMOKING HABIT :",
                   min = min(data$SMOKING),
                   max = max(data$SMOKING),
                   value = as.integer(mean(data$SMOKING))
      ),
      numericInput("YELLOW_FINGERS","PRESENCE OF YELLOW FINGER :",
                   min = min(data$YELLOW_FINGERS),
                   max = max(data$YELLOW_FINGERS),
                   value = as.integer(mean(data$YELLOW_FINGERS))
      ),
      numericInput("ANXIETY","PRESENCE OF ANXIETY :",
                   min = min(data$ANXIETY),max = max(data$ANXIETY),
                   value = as.integer(mean(data$ANXIETY))
      ),
      numericInput("PEER_PRESSURE","INFLUENCE OF ANY PEER PRESSURE :",
                   min = min(data$PEER_PRESSURE),
                   max = max(data$PEER_PRESSURE),
                   value = as.integer(mean(data$PEER_PRESSURE))
      ),
      numericInput("CHRONIC.DISEASE","PRESENCE OF ANY CHRONIC DISEASE :",
                   min = min(data$CHRONIC.DISEASE),
                   max = max(data$CHRONIC.DISEASE),
                   value = as.integer(mean(data$CHRONIC.DISEASE))
      ),
      numericInput("FATIGUE","PRESENCE OF ANY FATIGUE :",
                   min = min(data$FATIGUE),
                   max = max(data$FATIGUE),
                   value = as.integer(mean(data$FATIGUE))
      ),
      numericInput("ALLERGY","PRESENCE OF ANY ALLERGY :",
                   min = min(data$ALLERGY),
                   max = max(data$ALLERGY),
                   value = as.integer(mean(data$ALLERGY))
      ),
      numericInput("WHEEZING","PRESENCE OF ANY WHEEZING :",
                   min = min(data$WHEEZING),
                   max = max(data$WHEEZING),
                   value = as.integer(mean(data$WHEEZING))
      ),
      numericInput("ALCOHOL.CONSUMING","ANY ALCOHOL CONSUMING HABIT :",
                   min = min(data$ALCOHOL.CONSUMING),
                   max = max(data$ALCOHOL.CONSUMING),
                   value = as.integer(mean(data$ALCOHOL.CONSUMING))
      ),
      numericInput("COUGHING","PRESENCE OF ANY COUGH :",
                   min = min(data$COUGHING),
                   max = max(data$COUGHING),
                   value = as.integer(mean(data$COUGHING))
      ),
      numericInput("SHORTNESS.OF.BREATH","PRESENCE OF ANY SHORTNESS OF BREATH :",
                   min = min(data$SHORTNESS.OF.BREATH),
                   max = max(data$SHORTNESS.OF.BREATH),
                   value = as.integer(mean(data$SHORTNESS.OF.BREATH))
      ),
      numericInput("SWALLOWING.DIFFICULTY","PRESENCE OF ANY DIFFICULTY IN SWALLOWING :",
                   min = min(data$SWALLOWING.DIFFICULTY),
                   max = max(data$SWALLOWING.DIFFICULTY),
                   value = as.integer(mean(data$SWALLOWING.DIFFICULTY))
      ),
      numericInput("CHEST.PAIN","PRESENCE OF ANY PAIN IN THE CHEST :",
                   min = min(data$CHEST.PAIN),
                   max = max(data$CHEST.PAIN),
                   value = as.integer(mean(data$CHEST.PAIN))
      ),      
      submitButton('PREDICT')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('INSTRUCTION',
                 h3("INTRODUCTION"),
                 h4("This application is used to predict the lung cancer risk based on selected attributes that includes symptoms, factors that influenced and habits."),
                 h4("There are three tabs in this application such as stated below:"),
                 h4("Introduction - introduces the  function of the application to user."),
                 h4("Distribution and model accuracy - shows distribution charts and accuracy of the model."),
                 h4("Prediction -shows the result of lung cancer risk prediction based of Machine Learning algorithms."),
                 h1(""),
                 h3("INSTRUCTION"),
                 h4("Follow the instruction provided "),
                 h4("-Please fill in the input box on the left of the page"),
                 h4("-For the input, if the characteristic does not fit you, please enter 0 if the characteristic fits you, please enter 1"),
                 h4("-After filling in the characteristic, please click on PREDICT button at the bottom and results are shown in Prediction tab"),
                 h4("-The results is based on SVM and KNN models. 1 means YES, 0 means NO")),
        tabPanel('Distribution and Model Accuracy',
                 p(),
                 h3("Age distribution for the observation"), 
                 plotOutput("eda1"),
                 h3("Lung cancer on ages range"), 
                 plotOutput("eda2"),
                 h3("Lung cancer among smoker"), 
                 plotOutput("eda3"),
                 h3("Lung cancer on yellow fingers"), 
                 plotOutput("eda4"),
                 h3("Lung cancer on anxiety"), 
                 plotOutput("eda5"),
                 h3("Lung cancer on peer pressure"), 
                 plotOutput("eda6"),
                 h3("Lung cancer on chronic disease"), 
                 plotOutput("eda7"),
                 h3("Lung cancer on fatigue"), 
                 plotOutput("eda8"),
                 h3("Lung cancer on allergy"), 
                 plotOutput("eda9"),
                 h3("Lung cancer on wheezing"), 
                 plotOutput("eda10"),
                 h3("Lung cancer on alcohol consumer"), 
                 plotOutput("eda11"),
                 h3("Lung cancer on coughing"), 
                 plotOutput("eda12"),
                 h3("Lung cancer on shortness of breath"), 
                 plotOutput("eda13"),
                 h3("Lung cancer on swallowing difficulty"), 
                 plotOutput("eda14"),
                 h3("Lung cancer on chest pain"), 
                 plotOutput("eda15"),
                 h3("Model comparison between SVM and KNN"),
                 plotOutput("compare")),
        tabPanel('Prediction', 
                 plotOutput("PieChart"),
                 tableOutput("observation")
        )
      )
    )
  )
)

)


server <-shinyServer(function(input, output) {
  
  output$observation <- renderTable({
    ml_inputs <- reactiveValuesToList(input)
    
    ml_model <- "SUPPORT VECTOR MACHINE"
    pred <- as.character(predict(fit.svm, ml_inputs))
    
    ml_model <- append(ml_model, "K NEAREST NEIGHBOURS")
    pred <-  append(pred,
                    as.character(predict(fit.knn, ml_inputs)))
    
    prediction <- data.frame(ml_model, pred)
    colnames(prediction)[1] <- "Model"
    colnames(prediction)[2] <- "Prediction"
    
    #plot graph, histogram and piechart
    output$PieChart <- renderPlot({
      
      ggplot(prediction, aes(x=factor(1), fill = Prediction)) +
        geom_bar(width = 5 ) +
        coord_polar("y") +
        labs(title = "Lung cancer risk (Yes (1)/No(0))")
    })
    prediction
    
  })
  
  output$eda1 <- renderPlot({
    plot(density(data$AGE), main = "Age Distribution",
         xlab = "Age",col="darkblue",xlim=c(0,100))
  })
  
  output$eda2 <- renderPlot({
    tbl <- with(data, table(LUNG_CANCER, AGE))
    ggplot(as.data.frame(tbl), aes(factor(AGE), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
  })
  
  output$eda3 <- renderPlot({
    tbl1 <- with(data, table(LUNG_CANCER, SMOKING))
    ggplot(as.data.frame(tbl1), aes(factor(SMOKING), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
  })
  
  output$eda4 <- renderPlot({
    tbl2 <- with(data, table(LUNG_CANCER, YELLOW_FINGERS))
    ggplot(as.data.frame(tbl2), aes(factor(YELLOW_FINGERS), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
  })
  
  output$eda5 <- renderPlot({
    tbl3 <- with(data, table(LUNG_CANCER, ANXIETY))
    ggplot(as.data.frame(tbl3), aes(factor(ANXIETY), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
  })
  
  output$eda6 <- renderPlot({
    tbl4 <- with(data, table(LUNG_CANCER, PEER_PRESSURE))
    ggplot(as.data.frame(tbl4), aes(factor(PEER_PRESSURE), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
  })
  
  output$eda7 <- renderPlot({
    tbl5 <- with(data, table(LUNG_CANCER, CHRONIC.DISEASE))
    ggplot(as.data.frame(tbl5), aes(factor(CHRONIC.DISEASE), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
  })
  
  output$eda8 <- renderPlot({
    tbl6 <- with(data, table(LUNG_CANCER, FATIGUE))
    ggplot(as.data.frame(tbl6), aes(factor(FATIGUE), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
  })
  
  output$eda9 <- renderPlot({
    tbl7 <- with(data, table(LUNG_CANCER, ALLERGY))
    ggplot(as.data.frame(tbl7), aes(factor(ALLERGY), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
  })
  
  output$eda10 <- renderPlot({
    tbl8 <- with(data, table(LUNG_CANCER, WHEEZING))
    ggplot(as.data.frame(tbl8), aes(factor(WHEEZING), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
  })
  
  output$eda11 <- renderPlot({
    tbl9 <- with(data, table(LUNG_CANCER, ALCOHOL.CONSUMING))
    ggplot(as.data.frame(tbl9), aes(factor(ALCOHOL.CONSUMING), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
    
  })
  
  output$eda12 <- renderPlot({
    tbl10 <- with(data, table(LUNG_CANCER, COUGHING))
    ggplot(as.data.frame(tbl10), aes(factor(COUGHING), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
    
  })
  
  output$eda13 <- renderPlot({
    tbl11 <- with(data, table(LUNG_CANCER, SHORTNESS.OF.BREATH))
    ggplot(as.data.frame(tbl11), aes(factor(SHORTNESS.OF.BREATH), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
    
  })
  
  output$eda14 <- renderPlot({
    tbl12 <- with(data, table(LUNG_CANCER, SWALLOWING.DIFFICULTY))
    ggplot(as.data.frame(tbl12), aes(factor(SWALLOWING.DIFFICULTY), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
    
  })
  
  output$eda15 <- renderPlot({
    tbl13 <- with(data, table(LUNG_CANCER,CHEST.PAIN))
    ggplot(as.data.frame(tbl13), aes(factor(CHEST.PAIN), Freq, fill = LUNG_CANCER)) +     
      geom_col(position = 'dodge') +
      scale_fill_manual(values = c("#7f7f7f", "#17becf"))
    
    
  })
  
  #compare SVM and KNN
  output$compare <- renderPlot({
    bwplot(results, par.settings = list(box.rectangle = list(col = "red"),
                                        box.median = list(col = "blue"),
                                        box.whisker = list(col = "green")))
    
    
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)