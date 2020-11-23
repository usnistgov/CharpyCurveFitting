

inputUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(tags$style(HTML(".shiny-input-checkboxgroup {margin-left:15px}"))),
    fileInput(ns('datafile'),'Upload csv file',accept=c('.csv','.txt')),
    hr(),
    sliderInput(ns('conf_level'),'Confidence Level for Plots',
                min=.80,max=.99,value=.95,step=.01),
    hr(),
    selectInput(ns('response_type'),"Response Type",
                choices = c('KV'=1,'LE'=2,'SFA'=3)),
    hr(),
    h4("Select Models to Fit"),
    
    checkboxInput(ns('ht'),'Hyperbolic Tangent',FALSE),
    conditionalPanel(condition = "input.ht == '1'",{
                     checkboxGroupInput(ns('ht_opt'),label=NULL,
                         choices = c('Shelves not fixed (ht)'='snf',
                                     'Both shelves fixed (htf)'='bsf',
                                     'Upper shelf fixed (htuf)'='usf'))
    },ns=ns),
    
    checkboxInput(ns('aht'),'Asymmetric Hyperbolic Tangent'),
    conditionalPanel(condition = "input.aht == '1'",{
                     checkboxGroupInput(ns('aht_opt'),label=NULL,
                         choices = c('Shelves not fixed (aht)'='snf',
                                     'Both shelves fixed (ahtf)'='bsf',
                                     'Upper shelf fixed (ahtuf)'='usf'))
    },ns=ns),
    
    checkboxInput(ns('abur'),'Asymmetric Burr'),
    conditionalPanel(condition = "input.abur == '1'",{
      checkboxGroupInput(ns('abur_opt'),label=NULL,
                         choices = c('Shelves not fixed (abur)'='snf',
                                     'Both shelves fixed (aburf)'='bsf',
                                     'Upper shelf fixed (aburuf)'='usf'))
    },ns=ns),
    
    checkboxInput(ns('koh'),'Kohout (symmetric)'),
    conditionalPanel(condition = "input.koh == '1'",{
      checkboxGroupInput(ns('koh_opt'),label=NULL,
                         choices = c('Shelves not fixed (koh)'='snf',
                                     'Both shelves fixed (kohf)'='bsf',
                                     'Upper shelf fixed (kohuf)'='usf'))
    },ns=ns),
    
    checkboxInput(ns('akoh'),'Kohout (asymmetric)'),
    conditionalPanel(condition = "input.akoh == '1'",{
      checkboxGroupInput(ns('akoh_opt'),label=NULL,
                         choices = c('Shelves not fixed (akoh)'='snf',
                                     'Both shelves fixed (akohf)'='bsf',
                                     'Upper shelf fixed (akohuf)'='usf'))
    },ns=ns),
    hr(),
    h5("Specify upper and lower shelves"),
    h6("(Used as fixed values for models with fixed shelves and starting values otherwise)"),
    numericInput(ns('upper_shelf'),"Upper Shelf",1.43),
    numericInput(ns('lower_shelf'), "Lower Shelf",0),
    hr(),
    sliderInput(ns('nsim'),"Number Bootstrap Iterations per Model",
                min=50,max=1000,value=50,step=50),
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
        nsim = as.numeric(input$nsim)
        conf_level = as.numeric(input$conf_level)
    
        
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
        fit = as.numeric(input$response_type)

        if (fit==1) {
          yval = c(28,41)  # Absorbed energy values of interest to nuclear community
        } else if (fit==2) {
          yval = 0.89      # Lateral expansion value of interest to nuclear community
        } else if (fit==3) {
          yval = 50        # Shear Fracture Appearance
        }
        
        
        # for LSE, use bounds [laa, lbb] for all responses
        #
        # for USE, 
        #  1.  For KV and LE, assume normal(USE, u(USE))
        #  2.  for SFA, use bounds [c,d] for SFA (fit=3) (d=100)
        
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
          
        } else if (fit == 3) {
          #upper_shelf = 100
          #lower_shelf = 0
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
                          start = start)
        
        computedResults = list(mstats=mstats,results=results,other_vars=other_vars)
        
        
        boots_res = compute_boot(computedResults)
        
        computedResults$boots = boots_res$bout
        computedResults$coef_ints = boots_res$coef_ints
        
        return(computedResults)
        
      })
      
    }
  )
}



