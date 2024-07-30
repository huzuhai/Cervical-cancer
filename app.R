library(shiny)
library(ggplot2)
library(caret)
library(tidyverse)
library(DALEX)
library(ranger)

load('rf.Rdata')

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Postoperative VTE Risk Prediction Tool for Cervical Cancer Patients"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("Age", "Age", value = 30, min = 0, max = 100),
      radioButtons("Pathologictype", "Pathologic stype",  choices = c("SCC", "Others"), selected = "SCC"),
      radioButtons("Stage", "FIGO stage",  choices = c("I-II", "III-IV"), selected = "I-II"),
      radioButtons("D.dimer", "D-dimer",  choices = c("≤0.5", ">0.5"), selected = "≤0.5"),
      radioButtons("Chemotherapy", "Chemotherapy",  choices = c("NO", "YES"), selected = "NO"),
      numericInput("NLR", "Neutrophil-to-Lymphocyte Ratio", value = 0, min = 0, max = Inf),
      actionButton("predictBtn", "predict", class="btn-lg btn-success")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Predict outcome',
                 titlePanel('Predictive Results'),
                 h3("Cutoff"),
                 textOutput("cut"),
                 h3('Prob'),
                 textOutput("prob"),
                 h3("Patient Information"),
                 tableOutput("patient_information"),
                 plotOutput('plot_p'),
                 h3("Note"),
                 textOutput("suggestion")
        ),
        tabPanel('BD plot',
                 titlePanel('Predictive Results Interpretation'),
                 fluidRow(
                   column(8,
                          selectInput('order_vari', 
                                      "Variable Contribution Decomposition Order", 
                                      c('D.dimer','NLR','Age',
                                        'Chemotherapy','Pathologictype','Stage'),
                                      multiple = TRUE))),
                 fluidRow(
                   column(2, 
                          actionButton('variable_order','Confirm!'))
                 ),
                 h3('Break-down plot'),
                 plotOutput('bd'),
                 h3('Note'),
                 textOutput('bp_note')),
        tabPanel('SHAP plot',
                 titlePanel('Average Attributions'),
                 plotOutput('shap'),
                 h3('Note'),
                 textOutput('shap_note'))
      )
    )
  )
)

server <- function(input, output, session){
  
  rf_explain <- eventReactive(input$predictBtn, {
    DALEX::explain(fit_RF,
                   data = fit_RF$trainingData[,-1],
                   y=fit_RF$trainingData[['.outcome']],
                   label = 'RF')
  })
  newdata <- eventReactive(input$predictBtn,
    data.frame(
      VTE=0,
      Age = input$Age,
      Pathologictype = ifelse(input$Pathologictype=="SCC",0,1),
      Stage = ifelse(input$Stage=="I-II",0,1),
      D.dimer = ifelse(input$D.dimer=="≤0.5",0,1),
      Chemotherapy = ifelse(input$Chemotherapy=="NO",0,1),
      NLR = input$NLR
    )
  )
  data_model <- eventReactive(input$predictBtn, {
    vali <- newdata()
    vali <- vali %>% mutate(
      Age = as.numeric((Age-train_min[1])/((train_max - train_min)[1])),
      NLR = as.numeric((NLR-train_min[3])/((train_max - train_min)[3]))
    )
    vali
  })
  cutoff_out<- reactive(
    paste0('Suggested VTE Risk Threshold: ', round(cutoff,4),'.')
  )
  prob_out <- eventReactive(input$predictBtn, {
    round(predict(fit_RF ,newdata = data_model(),type = "prob")[,2],4)
  })
  table_out <- eventReactive(input$predictBtn,{
    tem <- newdata()[,-1]
    tem <- tem %>% mutate(
      Stage = input$Stage,
      Chemotherapy = input$Chemotherapy,
      D.dimer = input$D.dimer,
      Pathologictype = input$Pathologictype
    )
    tem
  })
  plot_out_p1 <- eventReactive(input$predictBtn,{
    plotdata=data.frame(x="VTE",y=prob_out())
    fill_color <- ifelse(prob_out()<cutoff, '#b0d992', '#fccccb')
    ggplot(plotdata,aes(x='VTE',y=prob_out()))+
      geom_bar(stat = 'identity', position = 'dodge',width = 0.2)+
      scale_y_continuous(limits = c(0,1))+
      labs(title = "Postoperative VTE Risk for cervical Cancer Patients",
           y = "Probability",
           x = "")+
      theme_minimal()+
      theme(plot.title=element_text(hjust = 0.5))+
      geom_hline(yintercept = cutoff, linetype = "dashed", color = "red",
                 linewidth = 1.5,alpha=0.2)+
      geom_bar(stat = 'identity', position = 'dodge', width = 0.2, 
               fill = fill_color)

  })
  variable_ord <- eventReactive(input$predictBtn, {
    if(input$variable_order){
      vari_tem <- input$order_vari
    } else{
      vari_tem <- c('D.dimer','NLR','Age',
                    'Chemotherapy','Pathologictype','Stage')
    }
    vari_tem
  }
  )
  plot_out_p2 <- eventReactive(input$predictBtn, {
    rf_ins_bd <- DALEX::predict_parts(rf_explain(),
                               new_observation = data_model()[,-1],
                               type = 'break_down',
                               order=variable_ord())
    plot(rf_ins_bd, 
         vnames=c('intercept',
                  paste0(variable_ord(),': ', 
                         c(input[[variable_ord()[1]]],
                         input[[variable_ord()[2]]],
                         input[[variable_ord()[3]]],
                         input[[variable_ord()[4]]],
                         input[[variable_ord()[5]]],
                         input[[variable_ord()[6]]])),
                  'prediction'))+
      facet_wrap(~label, ncol = 1, scales = "free_y",
                 labeller = as_labeller(c('RF'='')))+
      labs(title = " ", subtitle = 'Break-down plot')+
      theme(panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            axis.text = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            legend.text = element_text(size = 10),
            plot.title = element_text(size = 15, colour = 'tomato'))
  })
  plot_out_p3 <- eventReactive(input$predictBtn, {
    set.seed(999)
    rf_ins_shap <- predict_parts(rf_explain(),
                                 new_observation = data_model()[,-1],
                                 type = 'shap',
                                 B=20)
    rf_ins_shap <- rf_ins_shap %>%
      mutate(variable = case_when(
        grepl('Age', variable) ~ 
          paste0('Age: ',input[['Age']]),
        grepl('Chemotherapy', variable) ~ 
          paste0('Chemotherapy: ',input[['Chemotherapy']]),
        grepl('D.dimer', variable) ~ 
          paste0('D.dimer: ',input[['D.dimer']]),
        grepl('NLR', variable) ~ 
          paste0('NLR: ',input[['NLR']]),
        grepl('Pathologictype', variable) ~ 
          paste0('Pathologictype: ',input[['Pathologictype']]),
        grepl('Stage', variable) ~ 
          paste0('Stage: ',input[['Stage']]),
        TRUE ~ 'NA_real_' 
      ))
    
    plot(rf_ins_shap)+
      facet_wrap(~label, ncol = 1, scales = "free_y",
                 labeller = as_labeller(c('RF'='')))+
      labs(title = "", subtitle = paste0('Average attributions'))+
      theme(panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            axis.text = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            legend.text = element_text(size = 10),
            plot.title = element_text(size = 15, colour = 'tomato'))
    
  })
  
  output$bp_var <- renderText(
    paste0('The order of variables by contribution to prediction：', 
           paste0(variable_ord(), collapse = ", "))
  )
  output$shap <- renderPlot(
    plot_out_p3()
  )
  output$bd <- renderPlot(
    plot_out_p2()
  )
  output$plot_p <- renderPlot(
    plot_out_p1()
  )
  output$cut <- renderText(
    cutoff_out()
  )
  output$patient_information <- renderTable(
    table_out()
  )
  output$prob <- renderText(
    paste0("The current patient's risk probability is ", 
           prob_out(),
           ".")
  )
  output$suggestion <- renderText(
    "The results are for reference only. 
    The predictive outcomes do not indicate whether 
    postoperative VTE will occur; they are merely to alert us to 
    whether we should implement personalized interventions for 
    the patient to prevent the occurrence of postoperative VTE. 
    Please note that this tool is only a reference tool, 
    providing a reference for medical staff when making decisions."
  )
  output$bp_note <- renderText(
    paste0("In the graph, 'intercept' 
           represents the average predicted value of the model in the 
           development cohort, 'prediction' indicates the model's 
           forecast result for the current patient, and the bars 
           represent the contribution of each variable to the 
           predictive outcome for the current patient. 
           The contribution of a variable to the prediction may 
           depend on the order of the explanatory variables, 
           with the default order being D.dimer, NLR, Age, Chemotherapy, 
           Pathologic type, Stage. You can also customize the order of
           variables used for contribution decomposition."
    )
  )
  output$shap_note <- renderText(
    "Calculate the average contribution of variables to the 
    predictive outcome in different orders, where the bars represent 
    the average contribution size of each variable, 
    and the box plots represent the distribution of the variable's 
    contributions."
  )
}

# run the app

shinyApp(ui, server)
