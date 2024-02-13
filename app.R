rm(list=ls())
library(shiny)
library(shinythemes)
library(tidyverse)


ui <- fluidPage(
  theme = shinytheme('cerulean'),
  titlePanel('The US-CRS Score for Heart Transplant Allocation'),
  sidebarLayout(

    sidebarPanel(
      h4('Patient Characteristics'),
      
      
      radioButtons('sex', 'Gender', 
                   choices = c('Female' = 1, 'Male' = 0), selected = 1),
      numericInput('age', 'Age (years):', value = 50),
      
      numericInput('albumin', 'Albumin (g/dL):', value = 4),
      numericInput('bilirubin', 'Bilirubin (mg/dL):', value = 1),
      numericInput('creatinine', 'Creatinine (mEq/L):', value = 1),
      numericInput('sodium', 'Sodium (mEq/L):', value = 145),
      
      radioButtons('lvad', 'Left ventricular assist device?', 
                   choices = c('Yes' = 1, 'No' = 0), selected = 0),
      radioButtons('short_term_MCS', 'Ever on short-term mechanical circulatory support?', 
                   choices = c('Yes' = 1, 'No' = 0), selected = 0),
      radioButtons('BNP_type', 'Type of BNP', 
                   choices = c('NT-pro BNP' = 1, 'Regular BNP' = 0), selected = 0),
      numericInput('BNP_value', 'BNP (pg/mL)', value = 100),
      actionButton('calculateBtn', 'Calculate')
    ),
    mainPanel(
      h4(textOutput('result'), 
         style = 'text-align:center; font-size:20px;'),

      plotOutput(outputId = 'histogram'),

      h5('Kevin C. Zhang, MS, William F. Parker, MD, PhD, et al.'),
      h6('Predicting Death without Transplantation in Adult Heart Transplant Candidates:
         Developing and Validating the US Candidate Risk Score, JAMA, Feb. 2024')
    )
  )
)


load('model_final2.RData')

server <- function(input, output) {
  
  observeEvent(input$calculateBtn, {
    
    df <- data.frame('albumin' = input$albumin,
                     'bilirubin' = input$bilirubin,
                     'sex' = input$sex,
                     'age' = input$age,
                     'creatinine' = input$creatinine,
                     'sodium' = input$sodium,
                     'LVAD' = as.factor(as.character(input$lvad)),
                     'short_MCS_ever' = as.numeric(input$short_term_MCS),
                     'BNP_NT_Pro' = as.factor(as.character(input$BNP_type)),
                     'BNP' = input$BNP_value)
    
    df <- df %>%
      mutate(eGFR = case_when(
        
        sex == '1' & !is.na(creatinine) ~ 142 * (pmin((creatinine / 0.7), 1)^(-0.241)) *
          (pmax((creatinine / 0.7), 1)^(-1.2)) * 0.9938^(age) * 1.012,
        
        sex == '0' & !is.na(creatinine) ~ 142 * (pmin((creatinine / 0.9), 1)^(-0.302)) *
          (pmax((creatinine / 0.7), 1)^(-1.2)) * 0.9938^(age)))
    
    response <- predict(model_final, df, type='response')
    
    if (response < 0.0002414687) {score <- 1} 
    else if (response < 0.0019252998) {score <- 2} 
    else if (response < 0.0023406122) {score <- 3} 
    else if (response < 0.0026053226) {score <- 4} 
    else if (response < 0.0028629330) {score <- 5} 
    else if (response < 0.0030822307) {score <- 6} 
    else if (response < 0.0033125030) {score <- 7} 
    else if (response < 0.0035052083) {score <- 8} 
    else if (response < 0.0036847276) {score <- 9} 
    else if (response < 0.0038788431) {score <- 10} 
    
    else if (response < 0.0040652926) {score <- 11} 
    else if (response < 0.0042330307) {score <- 12} 
    else if (response < 0.0044408084) {score <- 13} 
    else if (response < 0.0046558164) {score <- 14} 
    else if (response < 0.0048401899) {score <- 15} 
    else if (response < 0.0050318944) {score <- 16} 
    else if (response < 0.0052317505) {score <- 17} 
    else if (response < 0.0054104743) {score <- 18} 
    else if (response < 0.0055949239) {score <- 19} 
    else if (response < 0.0058052005) {score <- 20} 
    
    else if (response < 0.0060102816) {score <- 21} 
    else if (response < 0.0061999520) {score <- 22} 
    else if (response < 0.0064498442) {score <- 23} 
    else if (response < 0.0067305407) {score <- 24} 
    else if (response < 0.0069936946) {score <- 25} 
    else if (response < 0.0072599684) {score <- 26} 
    else if (response < 0.0075419377) {score <- 27} 
    else if (response < 0.0078242595) {score <- 28} 
    else if (response < 0.0081003418) {score <- 29} 
    else if (response < 0.0084233431) {score <- 30} 
    
    else if (response < 0.0087711468) {score <- 31} 
    else if (response < 0.0091478702) {score <- 32} 
    else if (response < 0.0095864050) {score <- 33} 
    else if (response < 0.0101254803) {score <- 34} 
    else if (response < 0.0106085799) {score <- 35} 
    else if (response < 0.0111708917) {score <- 36} 
    else if (response < 0.0118524883) {score <- 37} 
    else if (response < 0.0125295036) {score <- 38} 
    else if (response < 0.0134169667) {score <- 39} 
    else if (response < 0.0143876027) {score <- 40} 
    
    else if (response < 0.0155228361) {score <- 41} 
    else if (response < 0.0168976873) {score <- 42} 
    else if (response < 0.0182511003) {score <- 43} 
    else if (response < 0.0199890044) {score <- 44} 
    else if (response < 0.0223121048) {score <- 45} 
    else if (response < 0.0256560892) {score <- 46} 
    else if (response < 0.0293034912) {score <- 47} 
    else if (response < 0.0346857267) {score <- 48} 
    else if (response < 0.0439990588) {score <- 49} 
    else {score <- 50}
    
    score <- score - 1
    
    output$result <- renderText({
      paste("US-CRS Score: ", score)})
    
    output$histogram <- renderPlot({
      df_plot <- data.frame('value' = seq(0,50, by=1))
      df_plot <- data.frame('value'= unlist(rep(df_plot, 10)))
      df_plot$color <- 0
      
      df_plot$color[df_plot$value >= (score - (score%%5) + 1) &
                      df_plot$value <= (score + 5 - (score%%5))] <- 1

      ggplot(df_plot, aes(x = value, fill = as.factor(color))) +
        geom_histogram(breaks = seq(0, 50, by = 5)) +
        xlim(c(0,50)) +
        scale_fill_manual(values = c('white', '#880d1e')) +
        scale_x_continuous(breaks = seq(0, 50, by = 5)) +
        labs(x = 'US-CRS Score',
             y =  '') +
        theme_bw() +
        theme(legend.position = 'none') +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(size = 14),
              axis.title.x = element_text(size = 16,
                                          margin = margin(t = 15, r = 0, 
                                                          b = 0, l = 0)))
      
    })
  })
}


shinyApp(ui = ui, server = server)
