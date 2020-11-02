
inputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(HTML(".shiny-input-checkboxgroup {margin-left:15px}"))),
    fileInput(ns('datafile'),'Upload csv file',accept=c('.csv','.txt')),
    hr(),
    sliderInput(ns('conf_level'),'Confidence Level for Plots',
                min=.80,max=.99,value=.95,step=.01),
    hr(),
    selectInput(ns('response_type'),"Repsonse Type",
                choices = c('KV','LE','SFA')),
    hr(),
    h4("Select Models to Fit"),
    
    checkboxInput(ns('ht'),'Hyperbolic Tangent',FALSE),
    conditionalPanel(condition = "input.ht == '1'",{
                     checkboxGroupInput(ns('ht_opt'),label=NULL,
                         choices = c('Shelves not fixed',
                                     'Both shelves fixed',
                                     'Upper shelf fixed'))
    },ns=ns),
    
    checkboxInput(ns('aht'),'Assymetric Hyperbolic Tangent'),
    conditionalPanel(condition = "input.aht == '1'",{
                     checkboxGroupInput(ns('aht_opt'),label=NULL,
                         choices = c('Shelves not fixed',
                                     'Both shelves fixed',
                                     'Upper shelf fixed'))
    },ns=ns),
    
    checkboxInput(ns('abur'),'Assymetric Burr'),
    conditionalPanel(condition = "input.abur == '1'",{
      checkboxGroupInput(ns('abur_opt'),label=NULL,
                         choices = c('Shelves not fixed',
                                     'Both shelves fixed',
                                     'Upper shelf fixed'))
    },ns=ns),
    
    checkboxInput(ns('koh'),'Kohout (symmetric)'),
    conditionalPanel(condition = "input.koh == '1'",{
      checkboxGroupInput(ns('koh_opt'),label=NULL,
                         choices = c('Shelves not fixed',
                                     'Both shelves fixed',
                                     'Upper shelf fixed'))
    },ns=ns),
    
    checkboxInput(ns('akoh'),'Kohout (asymmetric)'),
    conditionalPanel(condition = "input.akoh == '1'",{
      checkboxGroupInput(ns('akoh_opt'),label=NULL,
                         choices = c('Shelves not fixed',
                                     'Both shelves fixed',
                                     'Upper shelf fixed'))
    },ns=ns),
    hr(),
    h5("Specify Fixed Upper and Lower Shelves (if applicable)"),
    numericInput(ns('upper_shelf'),"Upper Shelf",100),
    numericInput(ns('lower_shelf'), "Lower Shelf",0),
    hr(),
    selectInput(ns('def_or_cus'),'Initial Parameter Values',
                c('Default','Custom')),
    conditionalPanel(condition = "input.def_or_cus == 'Custom'", {
      tagList(
        h5('Starting Values for Hyperbolic Tangent Models'),
        numericInput(ns('ht_c_prov'),'c_prov',50),
        numericInput(ns('ht_d_prov'),'d_prov',.0001),
        numericInput(ns('ht_t0_prov'),'t0_prov',10),
        h5('Starting Values for Hyperbolic Tangent Models'),
        numericInput(ns('bur_k_prov'),'k_prov',.04),
        numericInput(ns('bur_m_prov'),'m_prov',.04),
        h5('Starting Values for Hyperbolic Tangent Models'),
        numericInput(ns('koh_c_prov'),'ck_prov',20),
        numericInput(ns('koh_c_prov'),'p_prov',2),
        numericInput(ns('koh_c_prov'),'dbtt',-5))
    },ns=ns),
    
    actionButton(ns('go'),'Go')
  )
  
}

inputServer <- function(id) {
  moduleServer(
    id,
    function(input,output,session){
      
      userInputs <- eventReactive(input$go, {
        # format user inputs
        
      })
      
      return(userInputs)
      
    }
  )
}



