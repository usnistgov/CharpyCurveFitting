server = function(input, output) {
  
  source("functions_4_more_fun_v11.r")
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
                     c_prov = input$c_prov,
                     d_prov = input$d_prov,
                     t0_prov = input$t0_prov,
                     k_prov = input$k_prov,
                     m_prov = input$m_prov,
                     ck_prov = input$ck_prov,
                     p_prov = input$p_prov,
                     dbtt = input$dbtt
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