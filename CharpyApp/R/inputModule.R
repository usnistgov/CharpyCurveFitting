

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
                choices = c('KV'=1,'LE'=2,'SFA'=3)),
    hr(),
    h4("Select Models to Fit"),
    
    checkboxInput(ns('ht'),'Hyperbolic Tangent',FALSE),
    conditionalPanel(condition = "input.ht == '1'",{
                     checkboxGroupInput(ns('ht_opt'),label=NULL,
                         choices = c('Shelves not fixed'='snf',
                                     'Both shelves fixed'='bsf',
                                     'Upper shelf fixed'='usf'))
    },ns=ns),
    
    checkboxInput(ns('aht'),'Assymetric Hyperbolic Tangent'),
    conditionalPanel(condition = "input.aht == '1'",{
                     checkboxGroupInput(ns('aht_opt'),label=NULL,
                         choices = c('Shelves not fixed'='snf',
                                     'Both shelves fixed'='bsf',
                                     'Upper shelf fixed'='usf'))
    },ns=ns),
    
    checkboxInput(ns('abur'),'Assymetric Burr'),
    conditionalPanel(condition = "input.abur == '1'",{
      checkboxGroupInput(ns('abur_opt'),label=NULL,
                         choices = c('Shelves not fixed'='snf',
                                     'Both shelves fixed'='bsf',
                                     'Upper shelf fixed'='usf'))
    },ns=ns),
    
    checkboxInput(ns('koh'),'Kohout (symmetric)'),
    conditionalPanel(condition = "input.koh == '1'",{
      checkboxGroupInput(ns('koh_opt'),label=NULL,
                         choices = c('Shelves not fixed'='snf',
                                     'Both shelves fixed'='bsf',
                                     'Upper shelf fixed'='usf'))
    },ns=ns),
    
    checkboxInput(ns('akoh'),'Kohout (asymmetric)'),
    conditionalPanel(condition = "input.akoh == '1'",{
      checkboxGroupInput(ns('akoh_opt'),label=NULL,
                         choices = c('Shelves not fixed'='snf',
                                     'Both shelves fixed'='bsf',
                                     'Upper shelf fixed'='usf'))
    },ns=ns),
    hr(),
    h5("Specify Fixed Upper and Lower Shelves (if applicable)"),
    numericInput(ns('upper_shelf'),"Upper Shelf",100),
    numericInput(ns('lower_shelf'), "Lower Shelf",0),
    hr(),
    conditionalPanel(condition = "input.ht == '1' || input.aht == '1' ", {
      tagList(
        h5('Starting Values for Hyperbolic Tangent Models'),
        numericInput(ns('c_prov'),'c_prov',50),
        numericInput(ns('d_prov'),'d_prov',.0001),
        numericInput(ns('t0_prov'),'t0_prov',10))
    },ns=ns),
    conditionalPanel(condition = "input.abur == '1'", {
      tagList(
        h5('Starting Values for Burr Models'),
        numericInput(ns('k_prov'),'k_prov',.04),
        numericInput(ns('m_prov'),'m_prov',.04))
    },ns=ns),
    conditionalPanel(condition = "input.koh == '1' || input.akoh == '1'", {
      tagList(
        h5('Starting Values for Kohout Models'),
        numericInput(ns('ck_prov'),'ck_prov',20),
        numericInput(ns('p_prov'),'p_prov',2),
        numericInput(ns('dbtt'),'dbtt',-5))
    },ns=ns),
    
    actionButton(ns('goButton'),'Go')
  )
  
}

inputServer <- function(id) {
  moduleServer(
    id,
    function(input,output,session){
  
      
      userInputs <- eventReactive(input$goButton, {
        # format user inputs
        
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
    
        
        if(input$ht) {
          if('snf' %in% input$ht_opt) {
            start$ht    = c(c=c_prov, t0=t0_prov, lse=lower_shelf, use=upper_shelf)
          }
          if('bsf' %in% input$ht_opt) {
            start$htf   = c(c=c_prov, t0=t0_prov)
          }
          if('usf' %in% input$ht_opt) {
            start$htuf  = c(c=c_prov, t0=t0_prov, lse=lower_shelf)
          }
          
        }
        
        if(input$aht) {
          if('snf' %in% input$aht_opt) {
            start$aht   = c(c=c_prov, t0=t0_prov, d=d_prov, lse=lower_shelf, use=upper_shelf)
          }
          if('bsf' %in% input$aht_opt) {
            start$ahtf  = c(c=c_prov, t0=t0_prov, d=d_prov)
          }
          if('usf' %in% input$aht_opt) {
            start$ahtuf = c(c=c_prov, t0=t0_prov, d=d_prov, lse=lower_shelf)
          }
        }
        
        if(input$abur) {
          if('snf' %in% input$abur_opt) {
            start$abur   = c(k=k_prov, t0=t0_prov, m=m_prov, lse=lower_shelf, use=upper_shelf)
          }
          if('bsf' %in% input$abur_opt) {
            start$aburf  = c(k=k_prov, t0=t0_prov, m=m_prov) 
          }
          if('usf' %in% input$abur_opt) {
            start$aburuf = c(k=k_prov, t0=t0_prov, m=m_prov, lse=lower_shelf) 
          }
        }
        
        if(input$koh) {
          if('snf' %in% input$koh_opt) {
            start$koh   = c(c=ck_prov, DBTT=t0_prov, lse=lower_shelf, use=upper_shelf)
          }
          if('bsf' %in% input$koh_opt) {
            start$kohf  = c(c=ck_prov, DBTT=t0_prov)
          }
          if('usf' %in% input$koh_opt) {
            start$kohuf = c(c=ck_prov, DBTT=t0_prov, lse=lower_shelf)
          }
        }
        
        if(input$akoh) {
          if('snf' %in% input$akob_opt) {
            start$akoh   = c(c=ck_prov, t0=dbtt, p=p_prov, lse=lower_shelf, use=upper_shelf )
          }
          if('bsf' %in% input$akob_opt) {
            start$akohf  = c(c=ck_prov, t0=dbtt, p=p_prov)
          }
          if('usf' %in% input$akob_opt) {
            start$akohuf = c(c=ck_prov, t0=dbtt, p=p_prov, lse=lower_shelf)
          }
        }

        ## translating to Jolene's variables
        mod = names(start)
        temp = dataset$temperature
        yy = dataset$y
        nn = length(yy)
        n.new = 20
        nsim = 1000
        uls = 0.05
        uus = upper_shelf*0.05
        fit = as.numeric(input$response_type)

        if (fit==1) {
          yval = c(28,41)  # Absorbed energy values of interest to nuclear community
        } else if (fit==2) {
          yval = 0.89      # Lateral expansion value of interest to nuclear community
        } else if (fit==3) {
          yval = 50        # Shear Fracture Appearance
        }
        
        # create new temperature values for plotting
        t = seq(min(temp), max(temp), length.out=n.new)
        newt = data.frame(t)
        names(newt) = c("temp")
        
        # Jolene's code:
        cat('fitting')
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
                          fit = fit,
                          yval = yval,
                          t = t,
                          newt = newt,
                          upper_shelf = upper_shelf,
                          lower_shelf = lower_shelf)
        
        return(list(mstats=mstats,results=results,other_vars=other_vars))
        
      })
      
    }
  )
}



