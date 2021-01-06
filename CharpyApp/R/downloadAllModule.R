downloadAllUI <- function(id) {
  ns = NS(id)
  tagList(
    br(),
    downloadButton(ns('download_all'),'Download All Results'),
    br()
  )
  
}

downloadAllServer <- function(id,computedResults,fits_info) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$download_all <- downloadHandler(
        filename = function() {
          paste("results.pdf")
        },
        
        content = function(file) {
          
          pdf("./data/results.pdf")
          p = plot_fits(computedResults,fits_info()$fits_to_show,fits_info()$show_CIs)
          print(p)
          #plot(rnorm(100))
          dev.off()
          
          file.copy('./data/results.pdf',file)
          
        }
      )
      
    }
  )
}