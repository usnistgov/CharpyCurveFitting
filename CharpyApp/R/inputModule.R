

inputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns('userID'),'Data ID',placeholder="Material, Condition, Specimen Type, etc."),
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
    h6(paste("(Variable shelves will be fit using the optimization procedure,",
              "whereas fixed shelves are assumed known, up to some amount of uncertainty.",
             "Note that if SFA is selected as the response variable, shelves will be assumed",
             "fixed with 0 uncertainty.)")),
    
    # Fixed vs. Variable shelves (for SFA, shelves fixed at 0 and 100)
    conditionalPanel(condition = 'input.response_type != 3',
      selectInput(ns('lower_shelf_option'),
                  'Lower Shelf:',
                  choices=c('Fixed','Variable'),
                  selected='Fixed'),
      
      selectInput(ns('upper_shelf_option'),
                  'Upper Shelf:',
                  choices=c('Fixed','Variable'),
                  selected='Fixed'),ns=ns),
    
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
    h4("Unceratainty for fixed shelves"),
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
                "The uncertainty in the lower shelf is represented by a uniform",
                "distribution, centered at the user's selection for the lower shelf.",
                "The 'lower shelf uncertainty' provides the half-width of the uniform interval.",
                "(If the lower endpoint of the interval exceeds 0, the interval is lower-truncated at 0.)",
                "The uncertainty in the upper shelf (if selected as 'Fixed') is represented by a normal distribution,",
                "centered at the chosen upper shelf value with a standard deviation",
                "given by the input 'upper shelf uncertainty'.",
                "For more details, see the technical manuscript regarding this application."),
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
              h6("(Used as fixed values for models with fixed shelves and starting values otherwise)"),
              numericInput(session$ns('lower_shelf'),paste("Lower Shelf",yvar_name),min(dataset$y),min=0),
              numericInput(session$ns('upper_shelf'),paste("Upper Shelf",yvar_name),max(dataset$y),min=0)
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
            numericInput(session$ns('uls'),"Lower Shelf Uncertainy",value=.5,min=0)
          )
        } else if (input$response_type == 2 && input$lower_shelf_option == "Fixed") {
          tagList(
            numericInput(session$ns('uls'),"Lower Shelf Uncertainy",value=.05,min=0)
          )                 
        } else if (input$response_type == 4 && input$lower_shelf_option == "Fixed") {
          tagList(
            numericInput(session$ns('uls'),"Lower Shelf Uncertainy",value=NA,min=0)
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
              h5('Starting Values for Hyperbolic Tangent and Arctangent Models'),
              numericInput(session$ns('c_prov'),'C (\u00B0C)',25),
              numericInput(session$ns('d_prov'),'D',.0001),
              numericInput(session$ns('t0_prov'),'DBTT (\u00B0C)',med_temp))
          },ns=session$ns),
          conditionalPanel(condition = "input.which_models.includes('abur')", {
            tagList(
              h5('Starting Values for Burr Model'),
              numericInput(session$ns('k_prov'),'k',1),
              numericInput(session$ns('m_prov'),'m',.1),
              numericInput(session$ns('t0_prov_bur'),'T0 (\u00B0C)',med_temp))
          },ns=session$ns),
          conditionalPanel(condition = "input.which_models.includes('akoh')", {
            tagList(
              h5('Starting Values for Kohout Model'),
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
        start = list()
        
        withProgress(message = "Checking Inputs...",value=0, {
          

          dataset = read_csv(input$datafile$datapath)
          
          validate(
            need(!is.null(dataset$temperature),
                 "No column named 'temperature'."),
            
            need(!is.null(dataset$y),
                 "No column named 'y'."),
            
            need(length(dataset$y) == length(dataset$temperature),
                 "'temperature' and 'y' different lengths"),
           
            need(length(dataset$y) > 6,
                 "At least 7 observations (rows in .csv file) needed to fit models."),
            
            need(sum(is.na(c(dataset$y,dataset$temperature))) < 1, 
                 "Missing values detected in dataset."),
            
            need(sum(c(dataset$y,dataset$temperature) == '') < 1, 
                 "Empty cells detected in dataset."),
            
            need(is.numeric(dataset$y) && is.numeric(dataset$temperature),
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
                 "Lower shelf value must be less than the upper shelf value."),
            
            need(!is.na(input$uls),
                 "Missing value detected in lower shelf uncertainty."),
            
            need(input$uls >= 0 && input$uus >= 0,
                 "Shelf uncertainties must be non-negative")
            
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
          
          # for SFA, shelves fixed at 100 and 0
          upper_shelf = ifelse(fit == 3,100,as.numeric(input$upper_shelf))
          lower_shelf = ifelse(fit == 3,0,as.numeric(input$lower_shelf))
          
          # number bootstrap
          nsim = 1000
          conf_level = as.numeric(input$conf_level)
          
          shelves_in = c(input$lower_shelf_option,input$upper_shelf_option)
          
          if(all(shelves_in == c('Fixed','Fixed') )) {
            shelves = 'bsf'
          
          } else if(all(shelves_in == c('Fixed','Variable') )) {
            shelves = 'lsf'
            
          } else if(all(shelves_in == c('Variable','Fixed') )) {
            shelves = 'usf'
          
          } else {
            shelves = 'snf'
          }
          
          
          if('ht' %in% input$which_models) {
            if(shelves == 'snf') {
              start$ht    = c(c=c_prov, t0=t0_prov, lse=lower_shelf, use=upper_shelf)
            
            } else if(shelves == 'bsf') {
              start$htf   = c(c=c_prov, t0=t0_prov)
              
            } else if(shelves == 'usf') {
              start$htuf  = c(c=c_prov, t0=t0_prov, lse=lower_shelf)
            
            } else if(shelves == 'lsf') {
              start$htlf  = c(c=c_prov, t0=t0_prov, use=upper_shelf)
            }
            
          }
          
          if('aht' %in% input$which_models) {
            if(shelves == 'snf') {
              start$aht   = c(c=c_prov, t0=t0_prov, d=d_prov, lse=lower_shelf, use=upper_shelf)
              
            } else if(shelves == 'bsf') {
              start$ahtf  = c(c=c_prov, t0=t0_prov, d=d_prov)
            
            } else if(shelves == 'usf') {
              start$ahtuf = c(c=c_prov, t0=t0_prov, d=d_prov, lse=lower_shelf)
            
            } else if(shelves == 'lsf') {
              start$ahtlf = c(c=c_prov, t0=t0_prov, d=d_prov, use=upper_shelf)
            }
          }
          
          if('abur' %in% input$which_models) {
            if(shelves == 'snf') {
              start$abur   = c(k=k_prov, t0=t0_prov_bur, m=m_prov, lse=lower_shelf, use=upper_shelf)
              
            } else if(shelves == 'bsf') {
              start$aburf  = c(k=k_prov, t0=t0_prov_bur, m=m_prov) 
              
            } else if(shelves == 'usf') {
              start$aburuf = c(k=k_prov, t0=t0_prov_bur, m=m_prov, lse=lower_shelf) 
            
            } else if(shelves == 'lsf') {
              start$aburlf = c(k=k_prov, t0=t0_prov_bur, m=m_prov, use=upper_shelf) 
            }
          }
          
          if('koh' %in% input$which_models) {
            if(shelves == 'snf') {
              start$koh   = c(c=c_prov, DBTT=t0_prov, lse=lower_shelf, use=upper_shelf)
              
            } else if(shelves == 'bsf') {
              start$kohf  = c(c=c_prov, DBTT=t0_prov)
              
            } else if(shelves == 'usf') {
              start$kohuf = c(c=c_prov, DBTT=t0_prov, lse=lower_shelf)
            
            } else if(shelves == 'lsf') {
              start$kohlf = c(c=c_prov, DBTT=t0_prov, use=lower_shelf)
            }
          }
          
          if('akoh' %in% input$which_models) {
            if(shelves == 'snf') {
              start$akoh   = c(c=ck_prov, t0=dbtt, p=p_prov, lse=lower_shelf, use=upper_shelf )
            
            } else if(shelves == 'bsf') {
              start$akohf  = c(c=ck_prov, t0=dbtt, p=p_prov)
              
            } else if(shelves == 'usf') {
              start$akohuf = c(c=ck_prov, t0=dbtt, p=p_prov, lse=lower_shelf)
            
            } else if(shelves == 'lsf') {
              start$akohlf = c(c=ck_prov, t0=dbtt, p=p_prov, use=lower_shelf)
            }
          }
          
          incProgress(amount=.5)
  
          ## translating to Jolene's variables
          mod = names(start)
          temp = dataset$temperature
          yy = dataset$y
          nn = length(yy)
          n.new = 100
          
          
          
          if(fit < 4) {
            yvar_name = c('KV (J)','LE (mm)','SFA (%)')[fit]
          } else {
            yvar_name = input$custom_param
          }
          
          if(input$num_temps > 0) {
            yval = as.numeric(c(input$respval1,input$respval2,input$respval3)[1:input$num_temps])
          
          } else {
            yval = NA
          }
          
          # shelf uncertainties
          if (fit == 3) {
            laa = 0
            lbb = 0
            uus = 0
          
          } else {
            laa = max(0,lower_shelf - as.numeric(input$uls))
            lbb = lower_shelf + as.numeric(input$uls)
            uus = as.numeric(input$uus)
          }
          
          
          # create new temperature values for plotting
          t = seq(min(temp), max(temp), length.out=n.new)
          newt = data.frame(t)
          names(newt) = c("temp")
          
          # Jolene's code:
          # cat('fitting')
          fitres = fits(mod,start,lower_shelf,upper_shelf,yy,temp,fit)
          mstats = fitres[[2]]
          results = fitres[[1]]
          names(results) = mod
          ################################
          # keep models with valid results
          nmod = length(mod)
          keepid = ifelse(mstats$conv %in% c(1,2,3), 1, 0)
          
          # save fit message for printing
          mod.not = data.frame(mstats$mod, keepid, mstats$mess)
          mod.not = mod.not[mod.not$keepid==0, c(1,3)]
          names(mod.not) = c("Model","Convergence Message")
          #mod.not
          
          # save model stats for models that converged
          mstats2 = mstats[keepid==1, 1:5]
          mod2 = as.character(mstats2$mod)
    
          
          other_vars = list(mod = mod,
                            temp = temp,
                            yy = yy,
                            nn = nn,
                            n.new = n.new,
                            nsim = nsim,
                            uus = uus,
                            laa = laa,
                            lbb = lbb,
                            fit = fit,
                            yval = yval,
                            t = t,
                            newt = newt,
                            upper_shelf = upper_shelf,
                            lower_shelf = lower_shelf,
                            nsim = nsim,
                            mstats2 = mstats2,
                            mod2 = mod2,
                            conf_level = conf_level,
                            alpha = 1 - conf_level,
                            start = start,
                            shelves=shelves,
                            yvar_name=yvar_name,
                            userID=input$userID)
        })
        
        computedResults = list(mstats=mstats,results=results,other_vars=other_vars)
        
        if(length(mod2) > .5) {
          boots_res = compute_boot(computedResults)
          computedResults$boots = boots_res$bout
          computedResults$coef_ints = boots_res$coef_ints
          computedResults$tpout = boots_res$tpout
          computedResults$dbtt = boots_res$dbtt
        }
        
        return(computedResults)
        
      })
      
    }
  )
}



