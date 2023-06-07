library(readr)
library(stringr)
library(dplyr)
library(minpack.lm)
library(ggplot2)
library(pracma)
library(truncnorm)

source('R/utils.R')
source('R/functions_4_more_fun_v12.R')
source('R/plot_functions.R')

dataset = read.csv('data/sample.csv')
colnames(dataset)[1] = 'temperature'

results = run_charpy_analysis(dataset=dataset,
                              fit=1,
                              which_models='ht')

results$boots
