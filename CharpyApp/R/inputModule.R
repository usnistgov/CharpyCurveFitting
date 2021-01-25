

inputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
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
    
    selectInput(ns('lower_shelf_option'),
                'Lower Shelf:',
                choices=c('Fixed','Not Fixed'),
                selected='Not Fixed'),
    
    selectInput(ns('upper_shelf_option'),
                'Upper Shelf:',
                choices=c('Fixed','Not Fixed'),
                selected='Not Fixed'),
    
    hr(),
    uiOutput(ns('shelf_selections')),
    hr(),
    
    selectInput(ns('num_temps'),"Number Additional Characteristic Temperatures to be Estimated",
                choices = 0:3, selected=0),
    
    conditionalPanel(condition = "input.num_temps >= 1", {
      numericInput(ns('respval1'),'Response Value 1',0,min=0,max=100)
    },ns=ns),
    
    conditionalPanel(condition = "input.num_temps >= 2", {
      numericInput(ns('respval2'),'Response Value 2',0,min=0,max=100)
    },ns=ns),
    
    conditionalPanel(condition = "input.num_temps == 3", {
      numericInput(ns('respval3'),'Response Value 3',0,min=0,max=100)
    },ns=ns),
    hr(),
    
    selectInput(ns('nsim'),"Number Bootstrap Iterations per Model",
                choices = list('1000 (full run)'=1000, '100 (test run)'=100)),
    hr(),
    conditionalPanel(condition = "input.which_models.includes('ht') || input.which_models.includes('aht')", {
      tagList(
        h5('Starting Values for Hyperbolic Tangent Models'),
        numericInput(ns('c_prov'),'C (\u00B0C)',50),
        numericInput(ns('d_prov'),'D',.0001),
        numericInput(ns('t0_prov'),'DBTT (\u00B0C)',10))
    },ns=ns),
    conditionalPanel(condition = "input.which_models.includes('abur')", {
      tagList(
        h5('Starting Values for Burr Models'),
        numericInput(ns('k_prov'),'k',.04),
        numericInput(ns('m_prov'),'m',.04))
    },ns=ns),
    conditionalPanel(condition = "input.which_models.includes('koh') || input.which_models.includes('akoh')", {
      tagList(
        h5('Starting Values for Kohout Models'),
        numericInput(ns('ck_prov'),'C (\u00B0C)',20),
        numericInput(ns('p_prov'),'p',2),
        numericInput(ns('dbtt'),'T0 (\u00B0C)',-5))
    },ns=ns),
    
    actionButton(ns('goButton'),'Go')
  )
  
}

inputServer <- function(id) {
  moduleServer(
    id,
    function(input,output,session){
      
      output$shelf_selections <- renderUI({
        
        if(is.null(input$datafile)) {
          return(NULL)
        }
        
        dataset = read_csv(input$datafile$datapath)
        
        tagList(
          h5("Specify lower and upper shelves"),
          h6("(Used as fixed values for models with fixed shelves and starting values otherwise)"),
          numericInput(session$ns('lower_shelf'),"Lower Shelf",min(dataset$y)),
          numericInput(session$ns('upper_shelf'),"Upper Shelf",max(dataset$y))
        )
        
      })
      
      template_file = readr::read_csv('./data/sample.csv')
      
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
        dataset = read_csv(input$datafile$datapath)

        c_prov = as.numeric(input$c_prov)
        d_prov = as.numeric(input$d_prov)
        t0_prov = as.numeric(input$t0_prov)
        k_prov = as.numeric(input$k_prov)
        m_prov = as.numeric(input$m_prov)
        ck_prov = as.numeric(input$ck_prov)
        p_prov = as.numeric(input$p_prov)
        dbtt = as.numeric(input$dbtt)
        
        upper_shelf = as.numeric(input$upper_shelf)
        lower_shelf = as.numeric(input$lower_shelf)
        nsim = as.numeric(input$nsim)
        conf_level = as.numeric(input$conf_level)
        
        shelves_in = c(input$lower_shelf_option,input$upper_shelf_option)
        
        if(all(shelves_in == c('Fixed','Fixed') )) {
          shelves = 'bsf'
        
        } else if(all(shelves_in == c('Fixed','Not Fixed') )) {
          shelves = 'lsf'
          
        } else if(all(shelves_in == c('Not Fixed','Fixed') )) {
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
            start$abur   = c(k=k_prov, t0=t0_prov, m=m_prov, lse=lower_shelf, use=upper_shelf)
            
          } else if(shelves == 'bsf') {
            start$aburf  = c(k=k_prov, t0=t0_prov, m=m_prov) 
            
          } else if(shelves == 'usf') {
            start$aburuf = c(k=k_prov, t0=t0_prov, m=m_prov, lse=lower_shelf) 
          
          } else if(shelves == 'lsf') {
            start$aburlf = c(k=k_prov, t0=t0_prov, m=m_prov, use=upper_shelf) 
          }
        }
        
        if('koh' %in% input$which_models) {
          if(shelves == 'snf') {
            start$koh   = c(c=ck_prov, DBTT=t0_prov, lse=lower_shelf, use=upper_shelf)
            
          } else if(shelves == 'bsf') {
            start$kohf  = c(c=ck_prov, DBTT=t0_prov)
            
          } else if(shelves == 'usf') {
            start$kohuf = c(c=ck_prov, DBTT=t0_prov, lse=lower_shelf)
          
          } else if(shelves == 'lsf') {
            start$kohlf = c(c=ck_prov, DBTT=t0_prov, use=lower_shelf)
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

        ## translating to Jolene's variables
        mod = names(start)
        temp = dataset$temperature
        yy = dataset$y
        nn = length(yy)
        n.new = 20
        
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
                          yvar_name=yvar_name)
        
        
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



