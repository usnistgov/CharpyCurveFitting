downloadAllUI <- function(id) {
  ns = NS(id)
  tagList(
    br(),
    uiOutput(ns('the_button')),
    br(),
    br()
  )
  
}

downloadAllServer <- function(id,computedResults,fits_info) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$the_button <- renderUI({
        if(is.null(computedResults()$results)) {
          return(NULL)
        }
        downloadButton(session$ns('download_all'),'Download All Results')
      })
      
      output$download_all <- downloadHandler(
        filename = function() {
          paste("ResultsDownload.pdf")
        },
        
        content = function(file) {
          
          plot_fits_out = plot_fits(computedResults,fits_info()$fits_to_show,fits_info()$show_CIs)
          coef_table_out = create_coefs_table(computedResults)
          fit_metrics_table_out = create_fit_metrics_table(computedResults)
          dbtt_table_out = create_dbtt_table(computedResults)
          plot_tpout_out = plot_tpout(computedResults)
          
          other_vars = computedResults()$other_vars
          results = computedResults()$results
          
          withProgress(message = 'Preparing file for download...', {
            rmarkdown::render("./markdown/ResultsDownload.Rmd", 
                              params = list(plot_fits_out=plot_fits_out,
                                            coef_table_out = coef_table_out,
                                            fit_metrics_table_out = fit_metrics_table_out,
                                            dbtt_table_out = dbtt_table_out,
                                            plot_tpout_out = plot_tpout_out,
                                            plot_resids = plot_resids,
                                            other_vars = other_vars,
                                            results = results))
          })
          
          file.copy('./markdown/ResultsDownload.pdf',file)
          
        }
      )
      
    }
  )
}