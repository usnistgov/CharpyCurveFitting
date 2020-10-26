server = function(input, output) {
  
  source("functions_4_more_fun_v3.1.r")
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      
      yvals<-as.numeric(unlist(strsplit(gsub("\\s", "", input$yvals),",")))
      
      temps<-as.numeric(unlist(strsplit(gsub("\\s", "", input$temps),",")))
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      print(input$mod)
      
      # Set up parameters to pass to Rmd document
      params <- list(yy = yvals,
                     temp = temps,
                     main.title=input$main.title,
                     fit=input$fit,
                     mod=input$mod,
                     modPoints=input$modPoints,
                     predPoints=input$predPoints, #NOTE: NOT USING THIS YET!!!! #TODO
                     alpha=input$alpha,
                     dig=input$dig,
                     upper_shelf=input$upper_shelf,
                     lower_shelf=input$lower_shelf,
                     c_htf=input$c_htf,
                     t0_htf=input$t0_htf,
                     c_ahtf=input$c_ahtf,
                     t0_ahtf=input$t0_ahtf,
                     d_ahtf=input$d_ahtf,
                     k_aburf=input$k_aburf,
                     t0_aburf=input$t0_aburf,
                     m_aburf=input$m_aburf,
                     c_kohf=input$c_kohf,
                     DBTT_kohf=input$DBTT_kohf,
                     c_akohf=input$c_akohf,
                     t0_akohf=input$t0_akohf,
                     p_akohf=input$p_akohf
                     )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}