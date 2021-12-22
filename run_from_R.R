if (!require("pacman")) install.packages("pacman")

pacman::p_load('shiny','shinythemes','readr','stringr','dplyr','minpack.lm',
               'ggplot2','pracma','truncnorm','tinytex','DT')

runGitHub('CharpyCurveFitting','usnistgov')
