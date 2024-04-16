rm(list=ls())
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)


ui <- fluidPage(
  theme = shinytheme('cerulean'),
  titlePanel('The US-CRS Score for Heart Transplant Allocation'),
  sidebarLayout(

    sidebarPanel(
      h2('Patient Characteristics'),
      
      
      radioButtons('sex', 'Gender', 
                   choices = c('Female' = 1, 'Male' = 0), selected = 1),
      numericInput('age', 'Age (years):', value = 50),
      
      sliderInput('albumin', 'Albumin (g/dL):', min = 1, max = 5.5, 
                  step = 0.05, value = 4.5, ticks = F),
      sliderInput('bilirubin', 'Bilirubin (mg/dL):', min = 0.5, max = 10, 
                  step = 0.05, value = 0.5, ticks = F),
      sliderInput('creatinine', 'Serum Creatinine (mg/dL):', min = 1.0, max = 5,
                  step = 0.05, value = 1.5, ticks = F),
      checkboxInput('dialysis', 'Check this box if candidate is on dialysis',
                    value = F),
      sliderInput('sodium', 'Sodium (mEq/L):', min = 110, max = 145,
                  step = 1, value = 140, ticks = F),
      radioButtons('lvad', 'Left ventricular assist device?', 
                   choices = c('Yes' = 1, 'No' = 0), selected = 0),
      radioButtons('short_term_MCS', 'Ever on short-term mechanical circulatory support?', 
                   choices = c('Yes' = 1, 'No' = 0), selected = 0),
      radioButtons('BNP_type', 'Type of BNP', 
                   choices = c('NT-pro BNP' = 1, 'Regular BNP' = 0), selected = 0),
      sliderInput('BNP_value', 'Natural Log of BNP (pg/mL)', min = -1, max = 10,
                  step = 0.1, value = 1.5, ticks = F)
    ),
    mainPanel(
      h2(textOutput('result'), 
         style = 'text-align:center; font-size:25px;'),

      plotOutput(outputId = 'histogram'),
      
      h3('The US-CRS Score is a novel continuous score that measures the 
         probability that a heart transplant candidate will die on the waitlist 
         without receiving a transplant. It can be viewed as a measure of medical
         urgency, and uses several clinical variables to produce a score from
         0 to 50, where higher scores represent more urgency.'),
      
      HTML('<br>'),
      
      h4('Predicting Death without Transplantation in Adult Heart Transplant Candidates:
         Developing and Validating the US Candidate Risk Score, JAMA, Feb. 2024.'),
      h5('Kevin C. Zhang, MS, William F. Parker, MD, PhD, et al.')
    )
  )
)


load('model_final.RData')

server <- function(input, output) {
  
  observe({
    
    df <- data.frame('albumin' = input$albumin,
                     'bilirubin' = input$bilirubin,
                     'sex' = input$sex,
                     'age' = input$age,
                     'creatinine' = input$creatinine,
                     'sodium' = input$sodium,
                     'LVAD' = as.factor(as.character(input$lvad)),
                     'short_MCS_ever' = as.numeric(input$short_term_MCS),
                     'BNP_NT_Pro' = as.factor(as.character(input$BNP_type)),
                     'BNP' = as.numeric(exp(input$BNP_value)),
                     'dialysis' = as.numeric(input$dialysis))
    
    df <- df %>%
      mutate(eGFR = case_when(
        
        sex == '1' & !is.na(creatinine) ~ 142 * (pmin((creatinine / 0.7), 1)^(-0.241)) *
          (pmax((creatinine / 0.7), 1)^(-1.2)) * 0.9938^(age) * 1.012,
        
        sex == '0' & !is.na(creatinine) ~ 142 * (pmin((creatinine / 0.9), 1)^(-0.302)) *
          (pmax((creatinine / 0.7), 1)^(-1.2)) * 0.9938^(age)))
    
    df$eGFR[df$dialysis == 1] <- 0
    
    response <- predict(model_final, df, type='response')
    
         if (response <= 0.001264987) {score <- 1} 
    else if (response <= 0.001512010) {score <- 2} 
    else if (response <= 0.001730142) {score <- 3} 
    else if (response <= 0.001915465) {score <- 4} 
    else if (response <= 0.002077231) {score <- 5} 
    else if (response <= 0.002265669) {score <- 6} 
    else if (response <= 0.002460944) {score <- 7} 
    else if (response <= 0.002656456) {score <- 8} 
    else if (response <= 0.002870943) {score <- 9} 
    else if (response <= 0.003053774) {score <- 10} 
    
    else if (response <= 0.003265650) {score <- 11} 
    else if (response <= 0.003480987) {score <- 12} 
    else if (response <= 0.003768343) {score <- 13} 
    else if (response <= 0.004031062) {score <- 14} 
    else if (response <= 0.004282552) {score <- 15} 
    else if (response <= 0.004547730) {score <- 16} 
    else if (response <= 0.004810713) {score <- 17} 
    else if (response <= 0.005049220) {score <- 18} 
    else if (response <= 0.005360812) {score <- 19} 
    else if (response <= 0.005642311) {score <- 20} 
    
    else if (response <= 0.005912152) {score <- 21} 
    else if (response <= 0.006187035) {score <- 22} 
    else if (response <= 0.006482131) {score <- 23} 
    else if (response <= 0.006795176) {score <- 24} 
    else if (response <= 0.007096159) {score <- 25} 
    else if (response <= 0.007475686) {score <- 26} 
    else if (response <= 0.007820915) {score <- 27} 
    else if (response <= 0.008191861) {score <- 28} 
    else if (response <= 0.008585006) {score <- 29} 
    else if (response <= 0.008929379) {score <- 30} 
    
    else if (response <= 0.009364284) {score <- 31} 
    else if (response <= 0.009784004) {score <- 32} 
    else if (response <= 0.010321594) {score <- 33} 
    else if (response <= 0.010914968) {score <- 34} 
    else if (response <= 0.011564956) {score <- 35} 
    else if (response <= 0.012269121) {score <- 36} 
    else if (response <= 0.013089024) {score <- 37} 
    else if (response <= 0.013902984) {score <- 38} 
    else if (response <= 0.014925043) {score <- 39} 
    else if (response <= 0.016103152) {score <- 40} 
    
    else if (response <= 0.017557660) {score <- 41} 
    else if (response <= 0.018975357) {score <- 42} 
    else if (response <= 0.020854602) {score <- 43} 
    else if (response <= 0.023320539) {score <- 44} 
    else if (response <= 0.026778868) {score <- 45} 
    else if (response <= 0.030349420) {score <- 46} 
    else if (response <= 0.035739083) {score <- 47} 
    else if (response <= 0.045293501) {score <- 48} 
    else if (response <= 0.063164834) {score <- 49} 
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
        scale_fill_manual(values = c('white', '#3f88c5')) +
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
