library(readr)
library(stringr)
library(dplyr)
library(minpack.lm)
library(ggplot2)
library(pracma)
library(truncnorm)

source('../R/utils.R')
source('../R/functions_4_more_fun_v12.R')
source('../R/plot_functions.R')

data_files = list.files()[grep('csv',list.files())]
the_models = c('ht','aht','abur','koh','akoh')

for(ii in 1:length(data_files)) {
  
  t_data = read.csv(data_files[ii])
  colnames(t_data)[1] = 'temperature'
  results = run_charpy_analysis(dataset=t_data,
                                fit=1,
                                nboot=500,
                                which_models=the_models)
  
  print(results$mstats)
  
  print(plot_fits(results,c("HT","AHT","BUR","ACT","KHT"),'Yes'))
  print(create_coefs_table(results))
  print(create_fit_metrics_table(results))
  print(create_dbtt_table(results))
  print(plot_tpout(results))
  print(plot_resids(results,'ht'))
  #plot_resids_md(results)
  
}

