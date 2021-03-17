

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
    selectInput(ns('response_type'),"Parameter To Be Fit",
                choices = c('KV (J)'=1,'LE (mm)'=2,'SFA (%)'=3,'Other'=4)),
    
    conditionalPanel(condition = 'input.response_type == 4',{
      textInput(ns('custom_param'),"Parameter (Unit)",value="KV (J)")
    },ns=ns),
    hr(),
    
    checkboxGroupInput(ns('which_models'),'Select Regression Models',
                  choiceNames=c('Hyperbolic Tangent (HT)',
                                'Asymmetric Hyperbolic Tangent (AHT)',
                                'Burr (BUR)',
                                'Arctangent (ACT)',
                                'Kohout (KHT)'),
                  choiceValues=c('ht','aht','abur','koh','akoh')),
    hr(),
    selectInput(ns('lower_shelf_option'),
                'Lower Shelf:',
                choices=c('Constant','Variable'),
                selected='Constant'),
    
    selectInput(ns('upper_shelf_option'),
                'Upper Shelf:',
                choices=c('Constant','Variable'),
                selected='Constant'),
    
    hr(),
    uiOutput(ns('shelf_selections')),
    hr(),
    
    uiOutput(ns('additional_temps')),
    br(),
    
    #selectInput(ns('nsim'),"Number Bootstrap Iterations per Model",
    #            choices = list('1000 (full run)'=1000, '100 (test run)'=100)),
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
        
        tagList(
          h5("Specify lower and upper shelves"),
          h6("(Used as constant values for models with constant shelves and starting values otherwise)"),
          numericInput(session$ns('lower_shelf'),paste("Lower Shelf",yvar_name),min(dataset$y)),
          numericInput(session$ns('upper_shelf'),paste("Upper Shelf",yvar_name),max(dataset$y))
        )
        
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
          
          upper_shelf = as.numeric(input$upper_shelf)
          lower_shelf = as.numeric(input$lower_shelf)
          nsim = 1000
          conf_level = as.numeric(input$conf_level)
          
          shelves_in = c(input$lower_shelf_option,input$upper_shelf_option)
          
          if(all(shelves_in == c('Constant','Constant') )) {
            shelves = 'bsf'
          
          } else if(all(shelves_in == c('Constant','Variable') )) {
            shelves = 'lsf'
            
          } else if(all(shelves_in == c('Variable','Constant') )) {
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
          
          fit = as.numeric(input$response_type)
          
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
          
          
          
          # default values for each response
          if (fit == 1) {
            laa = 1.5
            lbb = 2.5
            uls = (lbb - laa)/sqrt(12)
            uus = 0.05*upper_shelf
            
          } else if (fit == 2){
            laa = 0
            lbb = 0.1
            uls = (lbb - laa)/sqrt(12)
            uus = 0.05*upper_shelf
            
          } else if (fit %in% c(3,4)) {
            laa = 0
            lbb = 0
            uls = 0
            uus = 0
          
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
                            uls = uls,
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



