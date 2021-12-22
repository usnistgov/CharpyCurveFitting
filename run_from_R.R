packages = c('shiny','shinythemes','readr','stringr','dplyr','minpack.lm',
            'ggplot2','pracma','truncnorm','tinytex','DT')

needed_packages = packages[!(packages %in% installed.packages())]

if(length(needed_packages) > 0) {
  install.packages(needed_packages)
}

library(shiny)

runGitHub('CharpyCurveFitting','usnistgov')
