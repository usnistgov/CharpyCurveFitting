
## Don't think we need all of these libraries, but not sure which are no longer used. 
## From Jolene's new code
library(minpack.lm)
library(MASS)
library(ggplot2)
library(pracma)

## from old code
library(propagate)
library(onls)
library(nlstools)
library(shiny)
library(shinydashboard)
library(tippy)
library(dplyr)


shinyUI(
  dashboardPage(title = "test",
                dashboardHeader(title = "Charpy App", titleWidth = 200),
                dashboardSidebar(
                  collapsed = T,
                  width = 200,
                  sidebarMenu(
                    menuItem("Enter data", icon = icon("table"), tabName = "menu_top"),
                    # menuItem("Github", icon = icon("github"), href = "https://github.com/okiyuki99/ShinyAB"),
                    # menuItem("RStudio Cloud", icon = icon("cloud"), href = "https://rstudio.cloud/project/245977"),
                    # menuItem("shinyapps.io", icon = icon("external-link-square"), href = "https://okiyuki.shinyapps.io/ShinyAB"),
                    menuItem("About", icon = icon("question-circle-o"), tabName = "menu_about")
                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "menu_top",
                            fluidRow(
                              box(title = "Data", width = 6, solidHeader = T, status = "primary", 
                                  fluidRow(
                                    column(12,
                                           textInput("yvals",label = "y values",value = "0,0,6,16,28,39,53,79,100,100,100,100"),#"3.38,14.12,420.69,338.01,453.41,456.97,437.82,455.49,448.84,447.65,435.53,410.32"),
                                           textInput("temps",label = "Temperatures",value = "-80,-50,-35,-15,0,10,21,40,76,100,148,198"),#"-196,-100,-95,-90,-70,-60,-50,-40,-30,-20,0,22")
                                           textInput("main.title",label = "Main Title",value = "Dataset 1"
                                           ),
                                           numericInput("fit",
                                                        label = "Variable being fitted (KV = 1, LE = 2, SFA = 3)",value = 3
                                           ),
                                           
                                    )
                                  )
                              ),
                              
                              box(title = "Plot info", width = 6, solidHeader = T, status = "primary", 
                                  fluidRow(
                                    # column(6, 
                                    #        # p(HTML("<b>UU</b>"),span(shiny::icon("info-circle"), id = "info_uu"),numericInput('uu', NULL, 10000),
                                    #        #   tippy::tippy_this(elementId = "info_uu",tooltip = "Number of Unique Users of your experiment",placement = "right")
                                    #        # )
                                    #        
                                    #        
                                    # ),
                                    column(12, 
                                           numericInput("modPoints",label = "Number of points for plots of model fit",value = 20),
                                           numericInput("predPoints",label = "Number of points for prediction and confidence intervals",value = 10),
                                           numericInput("alpha",label = "Confidence level for prediction and confidence intervals",value = .05),
                                           numericInput("dig",
                                                        label = "Number of decimal places for parameters in plots",value = 4
                                           )
                                           
                                    ),
                                    # column(4, 
                                           # textInput("main.title",
                                           # label = "Specify title for analysis",value = "X70 Or. L"
                                           # ),
                                           # textInput("y.label",
                                           #           label = "Specify label for y variable",value = "Absorbed Energy KV, J"
                                           # )
                                    # ),
                                    
                                  )   
                              ),
                              box(title = "Model and starting values", width = 12, solidHeader = T, status = "primary",
                                  h5("Specify fixed shelves"),
                                  numericInput("upper_shelf",label = "Upper shelf",value = 100),#1.438),
                                  numericInput("lower_shelf",label = "Lower shelf",value = 0),
                                  checkboxGroupInput("mod", label="Specify models to be fit",
                                                     choiceNames = c("hyperbolic tangent","hyperbolic tangent with fixed shelves","hyperbolic tangent with fixed upper shelf",
                                                                     "asymmetric hyperbolic tangent","asymmetric hyperbolic tangent with fixed shelves","asymemtric hyperbolic tangent with fixed upper shelf",
                                                                     "Burr distribution (asymmetric)","Burr distribution (asymmetric - with fixed shelves)","Burr distribution (asymmetric with fixed upper shelf)",
                                                                     "Kohout symmetric","Kohout symmetric with fixed shelves","Kohout symmetric with fixed upper shelf",
                                                                     "Kohout asymmetric","Kohout asymmetric with fixed shelves","Kohout asymmetric with fixed upper shelf"),
                                                     choiceValues = c("ht","htf","htuf",
                                                                            "aht","ahtf","ahtuf",
                                                                            "abur","aburf","aburuf",
                                                                            "koh","kohf","kohuf",
                                                                            "akoh","akohf","akohuf"),
                                                     selected = c("ht","htf","htuf",
                                                                      "aht","ahtf","ahtuf",
                                                                      "abur","aburf","aburuf",
                                                                      "koh","kohf","kohuf",
                                                                      "akoh","akohf","akohuf"),
                                                     inline = F, width = NULL),
                                  ### TODO: update the conditional panels based on new models and input names, server and rmd already updated, below not working
                                  conditionalPanel("input.mod.includes('ht' || 'htf' || 'htuf'|| 'aht' || 'ahtf'|| 'ahtuf')",
                                                   column(12,
                                                          h5("Starting values for hyperbolic tangent models"),
                                                          column(3,numericInput("c_prov",label = h5("c_prov"),value = 50)),
                                                          column(3,numericInput("d_prov",label = h5("d_prov"),value = 0.0001)),
                                                          column(3,numericInput("t0_prov",label = h5("t0_prov"),value = 10)),
                                                   )
                                  ),
                                  conditionalPanel("input.mod.includes('abur','aburf','aburuf')",
                                                   column(12,
                                                          h5("Starting values for Burr models"),
                                                          column(3,numericInput("k_prov",label = h5("k_prov"),value = .04)),
                                                          column(3,numericInput("m_prov",label = h5("m_prov"),value = .04)),
                                                   )
                                  ),
                                  conditionalPanel("input.mod.includes('koh','kohf','kohuf','akoh','akohf','akohuf')",
                                                   column(12,
                                                          h5("Starting values for Kohout models"),
                                                          column(3,numericInput("ck_prov",label = h5("ck_prov"),value = 20)),
                                                          column(3,numericInput("p_prov",label = h5("p_prov"),value = 2)),
                                                          column(3,numericInput("dbtt",label = h5("dbtt"),value = -5)),
                                                          
                                                   )
                                  )
                                  
                                  
                                  
                              ),
                              box(title = "Fit", width = 12, solidHeader = T, status = "success",
                                  fluidRow(
                                    column(10, 
                                           downloadButton("report", "Generate report")
                                           )
                                    )
                                  )
                              )
                            ),
                    tabItem(tabName = "menu_about",
                            h5("Add about information here")
                            )
                    )
                  )
                )
)
