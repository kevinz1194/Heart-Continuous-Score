rm(list=ls())
library(shiny)


ui <- fluidPage(
  titlePanel(''),
  sidebarLayout(
    sidebarPanel(
      numericInput('albumin', 'Albumin value:', value = 0),
      numericInput('bilirubin', 'Bilirubin value:', value = 0),
      numericInput('eGFR', 'eGFR value:', value = 0),
      numericInput('sodium', 'Sodium value:', value = 0),
      radioButtons('lvad', 'Left ventricular assist device?', 
                   choices = c('Yes' = 1, 'No' = 0), selected = 0),
      radioButtons('short_term_MCS', 'Ever on short-term mechanical circulatory support?', 
                   choices = c('Yes' = 1, 'No' = 0), selected = 0),
      radioButtons('BNP_type', 'Type of BNP', 
                   choices = c('NT-pro BNP' = 1, 'Regular BNP' = 0), selected = 0),
      numericInput('BNP_value', 'BNP value', value = 0),
      actionButton('calculateBtn', 'Calculate')
    ),
    mainPanel(
      h3(textOutput('result'), 
         style = 'text-align:center; font-size:20px;')
    )
  )
)


load('C:/Users/Kevin (Work)/Desktop/US-CRS/model_final.RData')

server <- function(input, output) {
  
  observeEvent(input$calculateBtn, {
    
    df <- data.frame('albumin' = input$albumin,
                     'bilirubin' = input$bilirubin,
                     'eGFR' = input$eGFR,
                     'sodium' = input$sodium,
                     'LVAD' = as.factor(as.character(input$lvad)),
                     'short_MCS_ever' = as.numeric(input$short_term_MCS),
                     'BNP_NT_Pro' = as.factor(as.character(input$BNP_type)),
                     'BNP' = input$BNP_value)
    
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
  })
}


shinyApp(ui = ui, server = server)
