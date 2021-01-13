
# error messages
type1_error_message <- "Mis-specification: Please select strictly increasing thresholds. The threshold for the top 1% is the lowest available option here, while the threshold for the top 0.01% is the highest."

# macro data 
  # source : World Inequality Lab - last checked in March 2020
  deflator_from_2017_to_2018 <- 0.96521288
  # source : World Development Indicators - GDP LCU - last checked in January 2020
  gdp_2018  <- 4873899000000/1000000
  
# source Budget Review 2019
  # on expenditure : page 288 of the PDF - column 2017/2018
  exp_education_2017 <- 312624.3/deflator_from_2017_to_2018
  exp_socialProtection_2017 <- 235702.1/deflator_from_2017_to_2018
  exp_health_2017 <- 188842.8/deflator_from_2017_to_2018
  exp_orderSafety_2017 <- 139029.9/deflator_from_2017_to_2018
  exp_debtService_2017 <- 162644.6/deflator_from_2017_to_2018
  expenditures <- c(exp_education_2017, exp_socialProtection_2017, exp_health_2017, exp_orderSafety_2017, exp_debtService_2017)
  # on tax revenues : page 274 of the PDF - column 2017/2018
  rev_PIT_2017 <- 460952.841/deflator_from_2017_to_2018
  rev_netVAT_2017 <- 297997.586/deflator_from_2017_to_2018
  rev_CIT_2017 <- 217412.046/deflator_from_2017_to_2018
  rev_allOtherTax <- 128016.064/deflator_from_2017_to_2018
  rev_exciseDutiesAndLevies <- 112085.339/deflator_from_2017_to_2018
  other_tax_revenues <- c(rev_PIT_2017, rev_netVAT_2017, rev_CIT_2017, rev_allOtherTax, rev_exciseDutiesAndLevies)

function(input, output, server){
  
  # define all reactive values I'll need
  # output$value <- renderPrint({ input$action })
  
  individualExampleR <- eventReactive(input$action1, {
    input$individualExample
  })
  numberBracketsR <- eventReactive(input$action1, {
    input$nbrBrackets
  })
  # not sure I need the definitions below
  evasionRateR <- reactive({
    input$evasionRate
  })
  stockDepreciationR <- reactive({
    input$stockDepreciation
  })
  
  # compute the microsimulation results
  allResults <- eventReactive(input$action1, {
    datapath <- "dina-tabulations-pwealth_indi-2017.csv"
    
  if( numberBracketsR() == 1){
      tableParameter <- data.frame( p = c(input$p1b1t), 
                                    brackets = c(1), 
                                    taxrates = c(input$p1b1r), 
                                    stockDep = input$stockDepreciation , evasion = input$evasionRate)
      v_rates <- c(input$p1b1r)
      v_bracket <- c(input$p1b1t)
  }
  if(numberBracketsR() == 2){
      tableParameter <- data.frame( p = c(input$p2b1t, input$p2b2t), 
                                    brackets = c(1,2), 
                                    taxrates = c(input$p2b1r, input$p2b2r), 
                                    stockDep = input$stockDepreciation , evasion = input$evasionRate)
      v_rates <- c(input$p2b1r, input$p2b2r)
      v_bracket <- c(input$p2b1t, input$p2b2t)
      validate(
        need(all(diff(as.numeric(v_bracket))>0), type1_error_message)
      )
    }
  if(numberBracketsR() == 3){
      tableParameter <- data.frame( p = c(input$p3b1t, input$p3b2t, input$p3b3t), 
                                    brackets = c(1, 2, 3), 
                                    taxrates = c(input$p3b1r, input$p3b2r, input$p3b3r), 
                                    stockDep = input$stockDepreciation , evasion = input$evasionRate)
      v_rates <- c(input$p3b1r, input$p3b2r, input$p3b3r)
      v_bracket <- c(input$p3b1t, input$p3b2t, input$p3b3t)
      validate(
        need(all(diff(as.numeric(v_bracket))>0), type1_error_message)
      )
  }
    if(numberBracketsR() == 4){
      tableParameter <- data.frame( p = c(input$p4b1t, input$p4b2t, input$p4b3t, input$p4b4t), 
                                    brackets = c(1,2, 3, 4), 
                                    taxrates = c(input$p4b1r, input$p4b2r, input$p4b3r, input$p4b4r), 
                                    stockDep = input$stockDepreciation , evasion = input$evasionRate)
      v_rates <- c(input$p4b1r, input$p4b2r, input$p4b3r, input$p4b4r)
      v_bracket <- c(input$p4b1t, input$p4b2t, input$p4b3t, input$p4b4t)
      validate(
        need(all(diff(as.numeric(v_bracket))>0), type1_error_message)
      )
    }
    if(numberBracketsR() == 5){
      tableParameter <- data.frame( p = c(input$p5b1t, input$p5b2t, input$p5b3t, input$p5b4t, input$p5b5t), 
                                    brackets = c(1,2, 3, 4, 5), 
                                    taxrates = c(input$p5b1r, input$p5b2r, input$p5b3r, input$p5b4r, input$p5b5r), 
                                    stockDep = input$stockDepreciation , evasion = input$evasionRate)
      v_rates <- c(input$p5b1r, input$p5b2r, input$p5b3r, input$p5b4r, input$p5b5r)
      v_bracket <- c(input$p5b1t, input$p5b2t, input$p5b3t, input$p5b4t, input$p5b5t)
      validate(
        need(all(diff(as.numeric(v_bracket))>0), type1_error_message)
      )
    }
    if(numberBracketsR() == 6){
      tableParameter <- data.frame( p = c(input$p6b1t, input$p6b2t, input$p6b3t, input$p6b4t, input$p6b5t, input$p6b6t), 
                                    brackets = c(1,2, 3, 4, 5, 6),
                                    taxrates = c(input$p6b1r, input$p6b2r, input$p6b3r, input$p6b4r, input$p6b5r, input$p6b6r), 
                                    stockDep = input$stockDepreciation , evasion = input$evasionRate)
      v_rates <- c(input$p6b1r, input$p6b2r, input$p6b3r, input$p6b4r, input$p6b5r, input$p6b6r)
      v_bracket <- c(input$p6b1t, input$p6b2t, input$p6b3t, input$p6b4t, input$p6b5t, input$p6b6t)
      validate(
        need(all(diff(as.numeric(v_bracket))>0), type1_error_message)
      )
    }
    if(numberBracketsR() == 7){
      tableParameter <- data.frame( p = c(input$p7b1t, input$p7b2t, input$p7b3t, input$p7b4t, input$p7b5t, input$p7b6t, input$p7b7t), 
                                    brackets = c(1,2, 3, 4, 5, 6, 7),
                                    taxrates = c(input$p7b1r, input$p7b2r, input$p7b3r, input$p7b4r, input$p7b5r, input$p7b6r, input$p7b7r), 
                                    stockDep = input$stockDepreciation , evasion = input$evasionRate)
      v_rates <- c(input$p7b1r, input$p7b2r, input$p7b3r, input$p7b4r, input$p7b5r, input$p7b6r, input$p7b7r)
      v_bracket <- c(input$p7b1t, input$p7b2t, input$p7b3t, input$p7b4t, input$p7b5t, input$p7b6t, input$p7b7t)
      validate(
        need(all(diff(as.numeric(v_bracket))>0), type1_error_message)
      )
    }
    if(numberBracketsR() == 8){
      tableParameter <- data.frame( p = c(input$p8b1t, input$p8b2t, input$p8b3t, input$p8b4t, input$p8b5t, input$p8b6t, input$p8b7t, input$p8b8t), 
                                    brackets = c(1,2, 3, 4, 5, 6, 7, 8),
                                    taxrates = c(input$p8b1r, input$p8b2r, input$p8b3r, input$p8b4r, input$p8b5r, input$p8b6r, input$p8b7r, input$p8b8r), 
                                    stockDep = input$stockDepreciation , evasion = input$evasionRate)
      v_rates <- c(input$p8b1r, input$p8b2r, input$p8b3r, input$p8b4r, input$p8b5r, input$p8b6r, input$p8b7r, input$p8b8r)
      v_bracket <- c(input$p8b1t, input$p8b2t, input$p8b3t, input$p8b4t, input$p8b5t, input$p8b6t, input$p8b7t, input$p8b8t)
      validate(
        need(all(diff(as.numeric(v_bracket))>0), type1_error_message)
      )
    }
    
    d_original <- read.csv(datapath, sep=",")
    tableParameter$p <- as.numeric(tableParameter$p)
    print(tableParameter)
    
    d <- merge(d_original, tableParameter, all = TRUE)
    # str(d)
    
    d$threshold <- as.numeric(!(is.na(d$brackets)))
    d <- d %>% fill(stockDep)
    d <- d %>% fill(evasion)
    d <- d %>% fill(brackets)
    
    # drop obs bellow threshold
    d <- d[!(is.na(d$brackets)),]
    # d <- setDT(d)
    
    d$pop <- as.numeric(d$pop)
    d$a <- as.numeric(d$a)
    
    # before collapsing I need to decrease total wealth and threshold by 
    # by stockDep 
    d$a <- d$a - (d$a * d$stock * (d$stockDep/100))
    
    # same for thresholds 
    d$t <- d$t - (d$t * d$stock * (d$stockDep/100))
    v_threshold <- d[d$threshold == TRUE,]$t
    
    d$tax <- 0
    i <- 1
    while(i<input$nbrBrackets){
      # before last brackets
     d$tax <- (d$tax + ((v_rates[i] / 100) * (d$a - v_threshold[i])) * as.numeric(d$p>=v_bracket[i] & d$p<v_bracket[i+1]))
      d$tax <- (d$tax + ((v_rates[i] / 100) * (v_threshold[i+1] - v_threshold[i])) * as.numeric(d$p>=v_bracket[i+1]))
      i <- i+1
    }
    # last bracket
   d$tax <- (d$tax + ((v_rates[i] / 100) * (d$a - v_threshold[i])) * as.numeric(d$p>=v_bracket[i]))
    
   d$tax <- d$tax*(1 - d$evasion/100)
   d$tax_pop <- d$tax * d$pop
   d$tot_tax_pop <- sum(d$tax_pop)
   d$tax_share <- d$tax_pop/d$tot_tax_pop
   d$average_after_tax <- d$a - d$tax
   d$average_taxrate <- d$tax/d$a
   
    ## create di
    di <- d[which(d$threshold==1),c("t", "brackets", "taxrates") ]
    
    # di$tax <- 0
    i <- 1
    while(i<input$nbrBrackets){
      # before last brackets
      di$tax <- (di$tax + ((v_rates[i] / 100) * (v_threshold[i+1] - v_threshold[i])) 
                 * as.numeric(di$brackets==i) * as.numeric(v_threshold[i+1]<input$individualExample))
      di$tax <- (di$tax + ((v_rates[i] / 100) * (input$individualExample - v_threshold[i])) 
                * as.numeric(di$brackets==i) * as.numeric(v_threshold[i+1]>input$individualExample))
      i <- i+1
    }
    # last bracket 
      di$tax[i] <- (di$tax[i] + ((v_rates[i] / 100) * (input$individualExample - v_threshold[i])) * as.numeric(input$individualExample > v_threshold[i]))
      di$tax <- di$tax - di$taxrates
      di$tax <- di$tax + (-1*di$tax) * as.numeric(di$tax<0)
      
    # last cleaning table_all
    table_all <- d[,c("p", "pop", "t", "tax", "a", "average_after_tax", "tax_share", "average_taxrate")]
      table_all$p <- table_all$p/1000
      table_all$t <- round(table_all$t/1000)*1000
      table_all$tax <- round(table_all$tax/1000)*1000
      table_all$a <- round(table_all$a/1000)*1000
      table_all$average_after_tax <- round(table_all$average_after_tax/1000)*1000
      table_all$tax_share <- round(table_all$tax_share*10000)/100
      table_all$average_taxrate <- round(table_all$average_taxrate*10000)/100
      table_all$total_tax_by_bracket <- table_all$pop*table_all$tax
      
      total_tax_nationwide <- sum(table_all$total_tax_by_bracket)
      table_all$total_tax_by_bracket <- NULL
      
      col_order <- c("p", "pop", "t", "a", "average_after_tax", "tax", "average_taxrate", "tax_share")
      table_all <- table_all[, col_order]
      for (col in c("pop", "t", "a", "average_after_tax", "tax")) {
        table_all[, col] <- comma(table_all[, col])
      }
      names(table_all)[names(table_all) == 'p']  <- 'Percentile'
      names(table_all)[names(table_all) == 'pop']  <- 'Number of Individuals'
      names(table_all)[names(table_all) == 't']  <- 'Threshold'
      names(table_all)[names(table_all) == 'tax']  <- 'Average Tax paid'
      names(table_all)[names(table_all) == 'a']  <- 'Average Wealth before Tax'
      names(table_all)[names(table_all) == 'average_after_tax']  <- 'Average Wealth after Tax'
      names(table_all)[names(table_all) == 'average_taxrate']  <- 'Effective Tax Rate'
      names(table_all)[names(table_all) == 'tax_share']  <- 'Tax Share'
      print(table_all)
     
    # last cleaning table_ind
    di <- di[,c("t", "brackets", "taxrates", "tax")]
      di$t <- round(di$t/1000)*1000
      di$tax <- round(di$tax/1000)*1000
      total_tax_individual <- sum(di$tax)
      
      col_order <- c("brackets", "t", "taxrates", "tax")
      di <- di[, col_order]
      for (col in c("t", "tax")) {
        di[, col] <- comma(di[, col])
      }
      names(di)[names(di) == 'brackets']  <- 'Brackets'
      names(di)[names(di) == 't']  <- 'Threshold'
      names(di)[names(di) == 'tax']  <- 'Tax paid'
      names(di)[names(di) == 'taxrates']  <- 'Marginal Tax Rate'
      # print(di)
      
    # return all object as a list
    list(table_ind = di,  table_all = table_all, table_parameter = tableParameter, total_tax_nationwide = total_tax_nationwide, total_tax_individual = total_tax_individual)
  })
  
  # # Compute output for the Individual Panel
  # numberBracketsR <- eventReactive(input$action1, {
  #   input$nbrBrackets
  # })
  
  output$firstParagraph <- renderText({
    minimum_thr <- allResults()$table_ind[[2]][1]
    min_tax <- allResults()$table_ind[[3]][1]
    max_tax <- tail(allResults()$table_ind[[3]], 1)
    if(numberBracketsR() > 1){
      paste0("<b>How does this work?</b>",
            " This is a test with ", tags$b(numberBracketsR()), " <b>brackets</b>.", 
            " With this wealth tax, South Africans whose net wealth exceeds<b> ", 
            minimum_thr, 
            " Rand</b> (after accounting for stock depreciation)",
            " would be liable to <i>marginal</i> tax rates ranging from<b> ", 
            min_tax, 
            "% </b>to<b> ", 
            max_tax, 
            "%</b> on all wealth held <i>above</i> this threshold. A taxpayer with a wealth of exactly ", 
            minimum_thr,
            " Rand would not pay any tax."
      )
    } else {
      paste0("<b>How does this work?</b>",
             " This is a test with ", tags$b("1"), " <b>bracket</b>.", 
            " With this wealth tax, South Africans whose net wealth exceeds<b> ", 
            minimum_thr, 
            " Rand</b> (after accounting for stock depreciation)", 
            " would be liable to a marginal tax rate equal to<b> ", 
            min_tax, 
            "%</b> ", 
            "on all wealth held above this threshold. A taxpayer with a wealth of exactly<b> ", 
            minimum_thr,
            " Rand</b> would not pay any tax."
      )
    }
  }) 
    
    output$secondParagraph <- renderText({
      averageTaxRateIndividual<- round(allResults()$total_tax_individual/individualExampleR()*100, digit =2)
      paste0("\n", 
            "<b>How much would an individual with a wealth equal to ",
            comma(individualExampleR()),
            " Rand pay?</b>", 
            " <br></br>",
            "She would pay a total of<b> ", 
            comma(allResults()$total_tax_individual), 
            " Rand</b> i.e.<b> ", 
            averageTaxRateIndividual, 
            "% of her total wealth</b>.",
            " <br></br>",            
            "The table on the right decomposes her tax liability by bracket."
      )
    })
    
  
  output$thirdParagraph <- renderText({
    total <- round(allResults()$total_tax_nationwide/1000000, digit = -3)
    total_tax_to_gdp <- round(total/gdp_2018*100, digit = 2)
    total_tax_to_socialProtection <- round(total/exp_socialProtection_2017*100)
    total_tax_to_netVAT_2017 <- round(total/rev_netVAT_2017*100)
    
    paste0("<b>How much revenue would be collected?</b>",
          " This wealth tax would raise a total of about<b> ", 
          comma(total), 
          " million Rand</b>,", 
          " i.e. <b>",
          total_tax_to_gdp, 
          "%</b> of South Africa's 2018 GDP. In the two graphs below, you can also compare this figure to selected government expenditures (left), or revenues collected through other tax instruments (right).",
          " For instance, it represents<b> ",
          total_tax_to_socialProtection, 
          "% </b> of all government expenditures on social protection, or <b> ",
          total_tax_to_netVAT_2017,
          "% </b> of all revenue collected through the Value Added Tax."
          )
  })

  
output$plotCompareMacroRev <- renderPlotly({
  # Create data
  data <- data.frame(
    name=c("Education", "Social Protection", "Health", "Public Order\nand Safety", "Debt \nService") ,  
    Percentage= (round(allResults()$total_tax_nationwide/1000000) / expenditures) * 100 
  )
  g1 <- ggplot(data, aes(x = reorder(name, Percentage), y=Percentage)) + 
    geom_bar(stat = "identity", fill = "#FF6666", width=0.6) +
    ggtitle("Wealth Tax Revenue Expressed as % of Government Expenditures") +
    theme(plot.title = element_text(size=10, face="bold", hjust = 0.5), 
          axis.title.x = element_text(),
          axis.text.x = element_text(colour="black", hjust=1),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "grey"),
          panel.border = element_rect(colour = "black", fill=NA, size=1)
                  ) +
    labs(y="Share of total (%)", x = "")
  ggplotly(g1, tooltip=c("Percentage"))
})  


output$plotCompareMacroTax <- renderPlotly({
  # Create data
  data <- data.frame(
    name=c("Personal\nIncome Tax", "Value\nAdded Tax", "Corporate\nIncome Tax", "All Other\nTaxes", "Excise Duties\nand Fuel Levy") ,  
    Percentage=  (round(allResults()$total_tax_nationwide/1000000) / other_tax_revenues) * 100
  )

  g2 <- ggplot(data, aes(x = reorder(name, Percentage), y=Percentage)) + 
    geom_bar(stat = "identity", fill = "#FF6666", width=0.6) +
    ggtitle("Wealth Tax Revenue Expressed as % of Other Taxes") +
    theme(plot.title = element_text(size=10, face="bold", hjust = 0.5), 
          axis.title.x = element_text(),
          axis.text.x = element_text(colour="black", hjust=1),
          panel.background = element_blank(),
          panel.grid.major.y = element_line(colour = "grey"),
          panel.border = element_rect(colour = "black", fill=NA, size=1)
          ) +
    labs(y="Share of total (%)", x = "")
  
  ggplotly(g2, tooltip=c("Percentage"))
})  

  # Generate final table 
  output$table_ind <- renderReactable({ 
      reactable(allResults()$table_ind, 
        defaultPageSize = nrow(allResults()$table_ind),
        defaultExpanded = TRUE,
        defaultColDef = colDef(align = "right",
                               headerStyle = list(align = "right", background = "#f7f7f8")),
        fullWidth = TRUE, 
        bordered = TRUE,
        highlight = TRUE
      )
  })
  

  output$tableTitle <- renderText({
    paste("Expected Revenue and Tax Paid by Wealth Group from a Wealth Tax in South Africa")
  })
  
  # Generate final table 
  output$table_all <- renderReactable({ 
    reactable(allResults()$table_all, 
      defaultPageSize = nrow(allResults()$table_all),
      defaultExpanded = TRUE,
      defaultColDef = colDef(align = "right",
                             headerStyle = list(align = "center", background = "#f7f7f8")),
      fullWidth = TRUE,
      bordered = TRUE,
      highlight = TRUE
    )
  })

output$download <- downloadHandler(
    filename = function(){"PWI_summary_table.csv"}, 
    content = function(fname){
      write.csv(allResults()$table_all, fname)
    }
  )
  
  output$plotTaxRatesByPercentile <- renderPlotly({
  
    Wealth.Group.Percentile <- allResults()$table_all[, "Percentile"]
    Effective.Tax.Rate <- allResults()$table_all[, "Effective Tax Rate"]
    
    g <- ggplot(allResults()$table_all, aes(x= Wealth.Group.Percentile, y=Effective.Tax.Rate)) +
      geom_area( fill="#FF6666", alpha=0.4) +
      geom_line(color="#FF6666", size=1) +
      geom_point(size=1, color="#FF6666") +
      theme_ipsum() +
      theme(axis.title.x=element_text(size=12,face="bold"),
            axis.title.y=element_text(size=12,face="bold")
                  ) +
      xlab("Wealth Group (percentile)") +
      xlim(99.00 , 99.99) +
      ylab("Effective Tax Rate (%)") +
      ggtitle("Effective Tax Rate Faced By Wealth Group (Percentile)")
    ggplotly(g, tooltip=c("Effective.Tax.Rate", "Wealth.Group.Percentile"))
  })
  
  # Average tax rate by average wealth
  output$plotTaxRatesByAverage <- renderPlotly({
    
    Average.Wealth.before.Tax <- round(as.numeric(gsub(",","", allResults()$table_all[, "Average Wealth before Tax"]))/1000000, digit = 1)
    Effective.Tax.Rate <- allResults()$table_all[, "Effective Tax Rate"]
    
    # allResults()$table_all[, "Average Wealth before Tax"]
    g <- ggplot(allResults()$table_all, aes(x= Average.Wealth.before.Tax, y=Effective.Tax.Rate)) +
      geom_area( fill="#FF6666", alpha=0.4) +
      geom_line(color="#FF6666", size=1) +
      geom_point(size=1, color="#FF6666") +
      theme_ipsum() +
      theme(axis.title.x=element_text(size=12,face="bold"),
            axis.title.y=element_text(size=12,face="bold")
      ) +
      xlab("Average Wealth before Tax (in millions of Rand)") +
      ylab("Effective Tax Rate (%)") +
      ggtitle("Effective Tax Rate Faced By Net Wealth (Rand)") 
    ggplotly(g, tooltip=c("Effective.Tax.Rate","Average.Wealth.before.Tax"))
  })
  
}
