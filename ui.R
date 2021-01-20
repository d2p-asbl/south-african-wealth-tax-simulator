library(reactable)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinyBS)
library(gridExtra)
library(scales)
library(hrbrthemes)

defaultNumberBrackets <- 3
defaultEvasionRate <- 30
defaultStockDepreciation <- 20
defaultIndividualExample <- 4000000
defaultRate <- 0.1

listBrackets <- list("1% (3.8m Rand)" = 99000,
     "0.9% (4.2m Rand)" = 99100,
     "0.8% (4.7m Rand)" = 99200,
     "0.7% (5.4m Rand)" = 99300,
     "0.6% (6.2m Rand)" = 99400,
     "0.5% (7.3m Rand)" = 99500,
     "0.4% (9.3m Rand)" = 99600,
     "0.3% (12m Rand)" = 99700,
     "0.2% (17m Rand)" = 99800,
     "0.1% (30m Rand)" = 99900,
     "0.09% (32m Rand)" = 99910,
     "0.08% (33m Rand)" = 99920,
     "0.07% (36m Rand)" = 99930,
     "0.06% (41m Rand)" = 99940,
     "0.05% (47m Rand)" = 99950,
     "0.04% (56m Rand)" = 99960,
     "0.03% (68m Rand)" = 99970,
     "0.02% (92m Rand)" = 99980,
     "0.01% (147m Rand)" = 99990)

navbarPage("",
     
      tabPanel("Home",
               fluidRow(
                 column(1,
                        ),
                 column(5,
                        h4("Welcome to the South African Wealth Tax Simulator!"),
                        br(), 
                        "The purpose of this simulator is to allow you to estimate", 
                        tags$b("how much tax revenue could be collected from a progressive wealth tax on the richest 1% in South Africa."), 
                        br(),
                        br(),
                        "This tool is the result of a collaborative project between", 
                        HTML(paste0("Aroop Chatterjee,",tags$sup("1"))), 
                        HTML(paste0("Léo Czajka,",tags$sup("2,3"))), 
                        HTML(paste0("and Amory Gethin",tags$sup("2"))), 
                        ".",
                        br(),
                        br(),
                        "The detailed companion study to this simulator can be found", tags$a(href="http://www.wid.world/news-article/a-wealth-tax-for-southafrica/", "here.", target="_blank"),
                        "The source code is", tags$a(href="https://github.com/d2p-asbl/south-african-wealth-tax-simulator/", "here.", target="_blank"),
                        br(),
                        br(),
                        "The underlying data used to compute aggregate figures come from our recent work on", 
                        tags$a(href="https://www.wider.unu.edu/sites/default/files/Publications/Working-paper/PDF/wp2020-45.pdf", "the distribution of household wealth in South Africa.", target="_blank"),
                        br(), 
                        br(),
                        "Media: please contact",
                        HTML(paste0(tags$i("arabo.ewinyu@wits.ac.za"),tags$sup("1"))), "or",
                        HTML(paste0(tags$i("olivia.ronsain@psemail.eu."),tags$sup("2"))), 
                        br(), 
                        br(),
                        h6(tags$a(href="https://www.wits.ac.za/scis/","1 : Southern Centre for Inequality Studies - Johannesburg", target="_blank")), 
                        h6(tags$a(href="https://wid.world/", "2 : World Inequality Lab - Paris", target="_blank")),
                        h6(tags$a(href="https://uclouvain.be/en/research-institutes/lidam/ires", "3 : Louvain Institute of Data Analysis and Modeling in economics and statistics - Louvain-la-Neuve", target="_blank")), 
                        splitLayout(cellWidths = c("33%", "33%", "33%"),  
                                    tags$a(href="https://www.wits.ac.za/scis/",img(src="logo-scis.png",height="80%", width="80%"), target="_blank"),
                                    tags$a(href="https://wid.world/", img(src="logo-wil.png",height = "70%", width = "80%"), target="_blank"),
                                    tags$a(href="https://uclouvain.be/en/research-institutes/lidam/ires", img(src="lidam.png",height = "80%", width = "80%", style = "margin: 2px; margin-left: -4px; "), target="_blank")
                                    )
                 ),
                 column(6,
                        # emptyness
                 )
               )
      ),
                     
      tabPanel("Simulation", 
         includeCSS("style.css"),
         fluidPage(
           # define a Title
           # headerPanel("Shiny Text"),
           # set color of error message
           tags$head(
             tags$style(HTML("
              .shiny-output-error-validation {
              color: red;},
              "))
           ),
           sidebarLayout(
             
             # organize side panel
             sidebarPanel(
              # shinyjs::useShinyjs(),
              # id = "side-panel",
               
               fixedRow(
                        "The idea of a",  tags$i("progressive"), "wealth tax is that rates should increase with wealth (like Personal Income Tax, but for wealth).",
                        br(),
                        br(),
                        "Select each parameter below to design your own progressive wealth tax.",
                        "Then click on the",
                        tags$b("Apply"),
                        "button to display the estimation results."
               ),
               br(),
         
              # delcare number of brackets
               sliderInput(inputId ="nbrBrackets",  
                           label = h4("Number of Brackets"), 
                           min = 1,
                           max = 8, 
                           value = defaultNumberBrackets),
              bsPopover("nbrBrackets", title ="",
                         "Select the number of tax brackets. Pick as many brackets as tax rates you have in mind.", 
                         placement = "right", options = list(container = "body")),
               br(),
     
               # number of brackets = 1
               conditionalPanel(
                 condition = "input.nbrBrackets == 1",
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p1b1r", label = "Rate 1 (%)", value = 4, min = 0.1 , max = 100, step = 0.1),
                          bsPopover("p1b1r", title = "",
                                    "Tax rates will only apply to wealth <i>above</i> the corresponding threshold. They must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")), 
                          bsPopover("p2b1r", title = "",
                                    "Tax rates will only apply to wealth <i>above</i> the corresponding threshold. They must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")), 
                          bsPopover("p3b1r", title = "",
                                    "Tax rates will only apply to wealth <i>above</i> the corresponding threshold. They must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")), 
                          bsPopover("p4b1r", title = "",
                                    "Tax rates will only apply to wealth <i>above</i> the corresponding threshold. They must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")),
                          bsPopover("p5b1r", title = "",
                                    "Tax rates will only apply to wealth <i>above</i> the corresponding threshold. They must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")), 
                          bsPopover("p6b1r", title = "",
                                    "Tax rates will only apply to wealth <i>above</i> the corresponding threshold. They must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")), 
                          bsPopover("p7b1r", title = "",
                                    "Tax rates will only apply to wealth <i>above</i> the corresponding threshold. They must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")), 
                          bsPopover("p8b1r", title = "",
                                    "Tax rates will only apply to wealth <i>above</i> the corresponding threshold. They must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body"))
                   ),
                   
                   column(8,
                          selectInput(inputId = "p1b1t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 0.1),
                          bsPopover("p1b1t", title = "",
                                    "Select the corresponding wealth group. Thresholds are expressed here <i>before</i> accounting for Stock Depreciation. The lowest threshold is 3.8m Rand, corresponding to the wealth required to belong to the top 1%. The highest threshold is equal to 147m Rand, corresponding to the wealth required to belong to the richest 0.01%. As for tax rates, thresholds must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")),
                          bsPopover("p2b1t", title = "",
                                    "Select the corresponding wealth group. Thresholds are expressed here <i>before</i> accounting for Stock Depreciation. The lowest threshold is 3.8m Rand, corresponding to the wealth required to belong to the top 1%. The highest threshold is equal to 147m Rand, corresponding to the wealth required to belong to the richest 0.01%. As for tax rates, thresholds must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")),
                          bsPopover("p3b1t", title = "",
                                    "Select the corresponding wealth group. Thresholds are expressed here <i>before</i> accounting for Stock Depreciation. The lowest threshold is 3.8m Rand, corresponding to the wealth required to belong to the top 1%. The highest threshold is equal to 147m Rand, corresponding to the wealth required to belong to the richest 0.01%. As for tax rates, thresholds must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")),
                          bsPopover("p4b1t", title = "",
                                    "Select the corresponding wealth group. Thresholds are expressed here <i>before</i> accounting for Stock Depreciation. The lowest threshold is 3.8m Rand, corresponding to the wealth required to belong to the top 1%. The highest threshold is equal to 147m Rand, corresponding to the wealth required to belong to the richest 0.01%. As for tax rates, thresholds must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")),
                          bsPopover("p5b1t", title = "",
                                    "Select the corresponding wealth group. Thresholds are expressed here <i>before</i> accounting for Stock Depreciation. The lowest threshold is 3.8m Rand, corresponding to the wealth required to belong to the top 1%. The highest threshold is equal to 147m Rand, corresponding to the wealth required to belong to the richest 0.01%. As for tax rates, thresholds must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")),
                          bsPopover("p6b1t", title = "",
                                    "Select the corresponding wealth group. Thresholds are expressed here <i>before</i> accounting for Stock Depreciation. The lowest threshold is 3.8m Rand, corresponding to the wealth required to belong to the top 1%. The highest threshold is equal to 147m Rand, corresponding to the wealth required to belong to the richest 0.01%. As for tax rates, thresholds must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")),
                          bsPopover("p7b1t", title = "",
                                    "Select the corresponding wealth group. Thresholds are expressed here <i>before</i> accounting for Stock Depreciation. The lowest threshold is 3.8m Rand, corresponding to the wealth required to belong to the top 1%. The highest threshold is equal to 147m Rand, corresponding to the wealth required to belong to the richest 0.01%. As for tax rates, thresholds must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body")),
                          bsPopover("p8b1t", title = "",
                                    "Select the corresponding wealth group. Thresholds are expressed here <i>before</i> accounting for Stock Depreciation. The lowest threshold is 3.8m Rand, corresponding to the wealth required to belong to the top 1%. The highest threshold is equal to 147m Rand, corresponding to the wealth required to belong to the richest 0.01%. As for tax rates, thresholds must be <b>strictly increasing</b>.",
                                    placement = "right", options = list(container = "body"))
                   )
                 )
               ), 
               
               # number of brackets = 2
               conditionalPanel(
                 condition = "input.nbrBrackets == 2",
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p2b1r", label = "Rate 1 (%)", value = 3, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p2b1t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99000)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p2b2r", label = "Rate 2 (%)", value = 5, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p2b2t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99900)
                   )
                 )
               ), 
               
               # number of brackets = 3
               conditionalPanel(
                 condition = "input.nbrBrackets == 3",
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p3b1r", label = "Rate 1 (%)", value = 3, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p3b1t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99000)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p3b2r", label = "Rate 2 (%)", value = 5, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p3b2t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99900)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p3b3r", label = "Rate 3 (%)", value = 7, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p3b3t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99990)
                   )
                 )
               ), 
               
               # number of brackets = 4
               conditionalPanel(
                 condition = "input.nbrBrackets == 4",
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p4b1r", label = "Rate 1 (%)", value = 2, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p4b1t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99000)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p4b2r", label = "Rate 2 (%)", value = 5, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p4b2t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99900)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p4b3r", label = "Rate 3 (%)", value = 7, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p4b3t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99950)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p4b4r", label = "Rate 4 (%)", value = 9, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p4b4t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99990)
                   )
                 )
               ), 
               
               # number of brackets = 5
               conditionalPanel(
                 condition = "input.nbrBrackets == 5",
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p5b1r", label = "Rate 1 (%)", value = 2, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p5b1t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99000)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p5b2r", label = "Rate 2 (%)", value = 3, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p5b2t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99500)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p5b3r", label = "Rate 3 (%)", value = 5, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p5b3t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99900)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p5b4r", label = "Rate 4 (%)", value = 7, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p5b4t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99950)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p5b5r", label = "Rate 5 (%)", value = 9, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p5b5t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99990)
                   )
                 )
               ), 
               
               
               # number of brackets = 6
               conditionalPanel(
                 condition = "input.nbrBrackets == 6",
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p6b1r", label = "Rate 1 (%)", value = 2, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p6b1t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99000)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p6b2r", label = "Rate 2 (%)", value = 3, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p6b2t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99500)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p6b3r", label = "Rate 3 (%)", value = 5, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p6b3t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99900)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p6b4r", label = "Rate 4 (%)", value = 7, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p6b4t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99950)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p6b5r", label = "Rate 5 (%)", value = 8, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p6b5t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99970)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p6b6r", label = "Rate 6 (%)", value = 9, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p6b6t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99990)
                   )
                 )
               ), 
               
               
               # number of brackets = 7
               conditionalPanel(
                 condition = "input.nbrBrackets == 7",
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p7b1r", label = "Rate 1 (%)", value = 1, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p7b1t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99000)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p7b2r", label = "Rate 2 (%)", value = 3, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p7b2t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99500)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p7b3r", label = "Rate 3 (%)", value = 5, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p7b3t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99900)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p7b4r", label = "Rate 4 (%)", value = 7, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p7b4t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99950)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p7b5r", label = "Rate 5 (%)", value = 8, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p7b5t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99970)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p7b6r", label = "Rate 6 (%)", value = 9, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p7b6t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99980)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p7b7r", label = "Rate 7 (%)", value = 10, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p7b7t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99990)
                   )
                 )
               ), 
               
               # number of brackets = 8
               conditionalPanel(
                 condition = "input.nbrBrackets == 8",
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p8b1r", label = "Rate 1 (%)", value = 1, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p8b1t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99000)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p8b2r", label = "Rate 2 (%)", value = 2, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p8b2t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99500)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p8b3r", label = "Rate 3 (%)", value = 5, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p8b3t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99900)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p8b4r", label = "Rate 4 (%)", value = 6, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p8b4t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99950)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p8b5r", label = "Rate 5 (%)", value = 7, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p8b5t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99960)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p8b6r", label = "Rate 6 (%)", value = 8, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p8b6t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99970)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p8b7r", label = "Rate 7 (%)", value = 9, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p8b7t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99980)
                   )
                 ),
                 fixedRow(
                   column(4,
                          numericInput(inputId = "p8b8r", label = "Rate 8 (%)", value = 10, min = 0.1 , max = 100, step = 0.1)
                   ),
                   column(8,
                          selectInput(inputId = "p8b8t", label = "On the Top", 
                                      choices = listBrackets, 
                                      selected = 99990)
                   )
                 )
               ), 
               br(), 
               
               fixedRow(
                 column(6,
                        numericInput(inputId = "evasionRate", label = "Evasion Rate (%)", value = defaultEvasionRate), 
                        bsPopover("evasionRate", title = "",
                                  "This parameter defines the expected share of taxable wealth that will go undeclared due to tax evasion in its various forms (underreporting, offshoring, fraud, etc.). An evasion rate of 30% means that 30% of the net value of taxable wealth will not be reported, and thus that the expected tax revenue will be 30% lower than what it would be without evasion.",
                                  placement = "right", options = list(container = "body"))
                 ),
                 column(6,
                        numericInput(inputId = "stockDepreciation", label = "Stock Depreciation (%)", value = defaultStockDepreciation),
                        bsPopover("stockDepreciation", title = "",
                                  "This parameter allows to adjust <i>financial</i> wealth in South Africa to account for the economic shock induced by the COVID-19 pandemic and/or the anticipated negative impact the introduction of a wealth tax could have on financial assets prices. Assuming a depreciation rate of 20% amounts to considering that the market value of bonds and stock has declined / would decline by 20%. Thresholds are adjusted ex-ante to take this into account.",
                                  placement = "right", options = list(container = "body"))
                 )
               ),
               br(), 
               # delcare "m"
              
              fixedRow(
                column(12,
                       numericInput(inputId = "individualExample", 
                                    label = "Individual Example (Total Wealth in Rand)", 
                                    value = defaultIndividualExample, 
                                    min = 100000,
                                    max = 100000000000, 
                                    step = 50000
                       ),
                       bsPopover("individualExample", title = "",
                                 "Please choose a wealth level. This will allow you to simulate the impact of the reform for a given individual.",
                                 placement = "right", options = list(container = "body"))
                ),
                column(12,
                )
              ),
              
               fixedRow(
                #column(11,
                #),
                 column(12,
                 actionButton(inputId = "action1", label = "Apply",
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4; float:right")
                 )
               )
               
                    ),

             # organize main panel
             mainPanel(
               includeCSS("style.css"),
               
               tabsetPanel(
                 
                 # dispay the value of 
                 tabPanel("Main Results", 
                          br(),
                          fluidRow(
                              column(12,
                                     htmlOutput("firstParagraph", inline = TRUE),
                              ),
                            ), 
                          br(), 
                          # add a sentence that says something like : individuals with wealth bellow XXX do not pay tax. 
                          # rates apply only to wealth ABOVE XXX
                          br(),
                          br(),
                          fluidRow( 
                            column(6,
                                htmlOutput("secondParagraph", align = 'left')
                            ),
                            column(6,
                                   reactableOutput("table_ind"),
                                   bsPopover("table_ind", title = "",
                                             "Thresholds are adjusted for stock depreciation. If you prefer keeping the original thresholds, please set the stock depreciation factor to 0.",
                                             placement = "left", options = list(container = "body"))
                            )
                          ),
                          br(), 
                          br(),
                          fluidRow(
                            column(12,
                                   htmlOutput("thirdParagraph", inline = TRUE)
                              )
                            ),
                          br(),
                          br(), 
                          
                          fluidRow(
                            column(2,
                            ),
                            column(8,
                                   plotOutput("plotCompareMacroRev"), plotOutput("plotCompareMacroTax")
                            ),
                            column(2,
                            )
                            )
                          ),

                 # delcare "summary"
                 tabPanel("Summary Table",
                          h4(htmlOutput("tableTitle"), style = "margin: 25px 90px;"),
                          reactableOutput("table_all"),
                          bsPopover("table_all", title = "",
                                    "Thresholds and averages before tax are adjusted for stock depreciation. If you prefer keeping the original values, please set the stock depreciation factor to 0.",
                                    placement = "left", options = list(container = "body")),
                          br(),
                          div(downloadButton('download',"Download the Table"), align = "center")
                          ),
                 
                 # delcare "hist"
                 tabPanel("Other Results", 
                          br(), 
                          plotlyOutput("plotTaxRatesByPercentile"),
                          br(), 
                          br(),
                          plotlyOutput("plotTaxRatesByAverage")
                          )
                 
               )
               
             )
           )
         )
    )
      

)


  