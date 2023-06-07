

inputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns('userID'),'Data Information',placeholder="Material, Condition, Specimen Type, etc."),
    br(),
    fileInput(ns('datafile'),'Upload csv file',accept=c('.csv','.txt')),
    downloadButton(ns('download'), label = "Download Template File", class = NULL),
    hr(),
    sliderInput(ns('conf_level'),'Confidence Level for Plots',
                min=.80,max=.99,value=.95,step=.01),
    hr(),
    selectInput(ns('response_type'),"Response Variable to be Fit",
                choices = c('KV (J)'=1,'LE (mm)'=2,'SFA (%)'=3,'Other'=4)),
    
    conditionalPanel(condition = 'input.response_type == 4',
      textInput(ns('custom_param'),"Parameter (Unit)",value="KV (J)"),ns=ns),
    hr(),
    
    checkboxGroupInput(ns('which_models'),'Select Regression Models',
                  choiceNames=c('Hyperbolic Tangent (HT)',
                                'Asymmetric Hyperbolic Tangent (AHT)',
                                'Burr (BUR)',
                                'Arctangent (ACT)',
                                'Kohout (KHT)'),
                  choiceValues=c('ht','aht','abur','koh','akoh')),
    hr(),
    
    h4("Specify whether shelves are fixed or variable"),
    h6(paste("(Variable shelves are fit using the optimization procedure, whereas fixed shelves are not.",
             "If shelves are fixed, the value of the shelf must be provided along with its associated uncertainty.",
             "Note that if SFA is selected as the response variable, shelves will be assumed fixed with 0 uncertainty.)")),
    
    # Fixed vs. Variable shelves (for SFA, shelves fixed at 0 and 100)
    conditionalPanel(condition = 'input.response_type != 3',
      selectInput(ns('lower_shelf_option'),
                  'Lower Shelf:',
                  choices=c('Fixed','Variable'),
                  selected='Variable'),
      
      selectInput(ns('upper_shelf_option'),
                  'Upper Shelf:',
                  choices=c('Fixed','Variable'),
                  selected='Variable'),ns=ns),
    
    conditionalPanel(condition = 'input.response_type == 3',
       selectInput(ns('lower_shelf_option'),
                   'Lower Shelf:',
                   choices=c('Fixed'),
                   selected='Fixed'),
       
       selectInput(ns('upper_shelf_option'),
                   'Upper Shelf:',
                   choices=c('Fixed'),
                   selected='Fixed'),ns=ns),
    
    hr(),
    uiOutput(ns('shelf_selections')),
    br(),
    h4("Uncertainty for Fixed Shelves"),
    h6(paste("(If a shelf is selected as fixed, an uncertainty value must be provided.",
             "Click the 'More Info' button below for further details.)")),
    uiOutput(ns('lower_shelf_uncertainty')),
    uiOutput(ns('upper_shelf_uncertainty')),
    actionButton(ns('help_uncertainty'),'More Info'),
    hr(),
    uiOutput(ns('additional_temps')),
    br(),
    
    hr(),
    uiOutput(ns('initial_params')),
    
    actionButton(ns('goButton'),'Go')
  )
  
}

inputServer <- function(id) {
  moduleServer(
    id,
    function(input,output,session){
      
      template_file = readr::read_csv('./data/sample.csv')
      
      observeEvent(input$help_uncertainty, {
        showModal(modalDialog(
          title = "Shelf Uncertainties: Details",
          paste("If a shelf is selected as fixed, a corresponding uncertainty value must be provided.",
                "The uncertainty in the upper shelf (if selected as 'Fixed') is represented by a normal distribution,",
                "centered at the chosen upper shelf value with a standard deviation",
                "given by the user's input for the 'upper shelf uncertainty'.",
                "The uncertainty in the lower shelf is represented in the same manner",
                "except that the distribution is truncated at 0.",
                "For more details, see the technical manuscript regarding this application."
                ),
        ))
      })
      
      output$shelf_selections <- renderUI({
        
        if(is.null(input$datafile)) {
          return(NULL)
        }
        
        req(input$response_type)
        
        fit = as.numeric(input$response_type)
        
        if(fit < 4) {
          yvar_name = c('KV (J)','LE (mm)','SFA (%)')[fit]
          
          yvar_name = str_split(yvar_name,' ')[[1]][2]
        } else {
          yvar_name = input$custom_param
        }
        
        dataset = read_csv(input$datafile$datapath)
        
        if(fit == 3) {
          return(
            tagList(
              h5("For SFA, the lower and upper shelf are assumed to be fixed at 0% and 100%")
            )
          )
        }
        
        else{
          return(
            tagList(
              h4("Specify lower and upper shelves"),
              h6("(Used as fixed values for models with fixed shelves and initial values otherwise)"),
              numericInput(session$ns('lower_shelf'),paste("Lower Shelf",yvar_name),min(dataset$Y),min=0),
              numericInput(session$ns('upper_shelf'),paste("Upper Shelf",yvar_name),max(dataset$Y),min=0)
            )
          )
        }
      
      })
      
      output$lower_shelf_uncertainty <- renderUI({
        
        if(is.null(input$datafile)) {
          return(NULL)
        }
      
        if(input$response_type == 1 && input$lower_shelf_option == "Fixed") {
          tagList(
            numericInput(session$ns('uls'),"Lower Shelf Uncertainty",value=.3,min=0)
          )
        } else if (input$response_type == 2 && input$lower_shelf_option == "Fixed") {
          tagList(
            numericInput(session$ns('uls'),"Lower Shelf Uncertainty",value=.03,min=0)
          )                 
        } else if (input$response_type == 4 && input$lower_shelf_option == "Fixed") {
          tagList(
            numericInput(session$ns('uls'),"Lower Shelf Uncertainty",value=NA,min=0)
          )                 
        } else {
          return(NULL)
        }
    
      })
      
      output$upper_shelf_uncertainty <- renderUI({
        
        if(is.null(input$datafile)) {
          return(NULL)
        }
        
        if(input$response_type != 3 && input$upper_shelf_option == "Fixed") {
          tagList(
            numericInput(session$ns('uus'),"Upper Shelf Uncertainty",value=.05*input$upper_shelf)
          )
        }  else {
          return(NULL)
        }
        
      })
      
      output$additional_temps <- renderUI({
        
        req(input$response_type)
        
        fit = as.numeric(input$response_type)
        
        if(fit < 4) {
          yvar_name = c('KV (J)','LE (mm)','SFA (%)')[fit]
          
          yvar_name = str_split(yvar_name,' ')[[1]][2]
        } else {
          yvar_name = input$custom_param
        }
        
        tagList(
          selectInput(session$ns('num_temps'),"Number of Additional Characteristic Temperatures to be Estimated",
                      choices = 0:3, selected=0),
          
          conditionalPanel(condition = "input.num_temps >= 1", {
            numericInput(session$ns('respval1'),
                         paste('Response Value 1',yvar_name),
                         0,min=0,max=100)
          },ns=session$ns),
          
          conditionalPanel(condition = "input.num_temps >= 2", {
            numericInput(session$ns('respval2'),
                         paste('Response Value 2',yvar_name),
                         0,min=0,max=100)
          },ns=session$ns),
          
          conditionalPanel(condition = "input.num_temps == 3", {
            numericInput(session$ns('respval3'),
                         paste('Response Value 3',yvar_name),
                         0,min=0,max=100)
          },ns=session$ns)
        )

      })
      
      output$initial_params <- renderUI({
        
        if(is.null(input$datafile)) {
          return(NULL)
        }
        
        dataset = read_csv(input$datafile$datapath)
        
        med_temp = median(dataset$temperature)
        
        tagList(
          conditionalPanel(condition = "input.which_models.includes('ht') || input.which_models.includes('aht') || input.which_models.includes('koh')", {
            tagList(
              h5('Initial Values for Hyperbolic Tangent and Arctangent Models'),
              numericInput(session$ns('c_prov'),'C (\u00B0C)',25),
              numericInput(session$ns('d_prov'),'D',.0001),
              numericInput(session$ns('t0_prov'),'DBTT (\u00B0C)',med_temp))
          },ns=session$ns),
          conditionalPanel(condition = "input.which_models.includes('abur')", {
            tagList(
              h5('Initial Values for Burr Model'),
              numericInput(session$ns('k_prov'),'k',1),
              numericInput(session$ns('m_prov'),'m',.1),
              numericInput(session$ns('t0_prov_bur'),'T0 (\u00B0C)',med_temp))
          },ns=session$ns),
          conditionalPanel(condition = "input.which_models.includes('akoh')", {
            tagList(
              h5('Initial Values for Kohout Model'),
              numericInput(session$ns('ck_prov'),'C (\u00B0C)',25),
              numericInput(session$ns('p_prov'),'p',1),
              numericInput(session$ns('dbtt'),'T0 (\u00B0C)',med_temp))
          },ns=session$ns)
        )
      })
      
      output$download <- downloadHandler(
        filename = function() {
          paste("template_file.csv")
        },
        
        content = function(file) {
          write.csv(template_file, file, row.names = FALSE)
        }
      )
      

      userInputs <- eventReactive(input$goButton, {
        # format user inputs
        req(input$datafile)
    
        dataset = read_csv(input$datafile$datapath)
        
        validate(
          need(!is.null(dataset$temperature),
               "No column named 'temperature'."),
          
          need(!is.null(dataset$Y),
               "No column named 'Y'."),
          
          need(length(dataset$Y) == length(dataset$temperature),
               "'temperature' and 'Y' different lengths"),
         
          need(length(dataset$Y) > 6,
               "At least 7 observations (rows in .csv file) needed to fit models."),
          
          need(sum(is.na(c(dataset$Y,dataset$temperature))) < 1, 
               "Missing values detected in dataset."),
          
          need(sum(c(dataset$Y,dataset$temperature) == '') < 1, 
               "Empty cells detected in dataset."),
          
          need(is.numeric(dataset$Y) && is.numeric(dataset$temperature),
               "Non-numeric values detected in dataset."),
          
          need(length(input$which_models) > 0,
               "No models selected." ),
          
          need(as.numeric(c(input$c_prov,input$d_prov,input$t0_prov,
                            input$k_prov,input$m_prov,input$ck_prov,
                            input$p_prov,input$dbtt,input$t0_prov_bur,
                            input$upper_shelf,input$lower_shelf)) %>%
               is.numeric(),
               "Non-numeric parameters detected."), 
          
          need(!any(is.na(as.numeric(c(input$c_prov,input$d_prov,input$t0_prov,
                                       input$k_prov,input$m_prov,input$ck_prov,
                                       input$p_prov,input$dbtt,input$t0_prov_bur,
                                       input$upper_shelf,input$lower_shelf)))),
               "Empty parameter fields detected."),
          
          need(input$lower_shelf < input$upper_shelf,
               "Lower shelf value must be less than the upper shelf value.")
          
        )
        

        c_prov = as.numeric(input$c_prov)
        d_prov = as.numeric(input$d_prov)
        t0_prov = as.numeric(input$t0_prov)
        t0_prov_bur = as.numeric(input$t0_prov_bur)
        k_prov = as.numeric(input$k_prov)
        m_prov = as.numeric(input$m_prov)
        ck_prov = as.numeric(input$ck_prov)
        p_prov = as.numeric(input$p_prov)
        dbtt = as.numeric(input$dbtt)
        fit = as.numeric(input$response_type)
        lower_shelf_option = input$lower_shelf_option
        upper_shelf_option = input$upper_shelf_option
        conf_level = as.numeric(input$conf_level)
        upper_shelf = as.numeric(input$upper_shelf)
        lower_shelf = as.numeric(input$lower_shelf)
        which_models = input$which_models
        num_temps = input$num_temps
        respval1 = input$respval1
        respval2 = input$respval2
        respval3 = input$respval3
        custom_param = input$custom_param
        uls_in = ifelse(is.null(input$uls),NA,as.numeric(input$uls))
        uus_in = ifelse(is.null(input$uus),NA,as.numeric(input$uus))
        userID = input$userID
        
        nboot = 2000
        
        computedResults = run_charpy_analysis(dataset,
                                              fit, # 1=KV, 2=LE, 3=SFA, 4=other,
                                              nboot,
                                              c_prov,
                                              d_prov,
                                              t0_prov, 
                                              t0_prov_bur,
                                              k_prov,
                                              m_prov,
                                              ck_prov, 
                                              p_prov,
                                              dbtt,
                                              lower_shelf_option,
                                              upper_shelf_option,
                                              conf_level,
                                              upper_shelf,
                                              lower_shelf,
                                              which_models,
                                              num_temps,
                                              respval1,
                                              respval2,
                                              respval3,
                                              custom_param, 
                                              uls_in,
                                              uus_in,
                                              userID)
      
      return(computedResults)
        
      })
    }
  )
}



