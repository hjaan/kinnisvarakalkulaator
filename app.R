library(shiny)
library(ggplot2)
library(reshape2)
library(scales)  # For formatting monetary values

# ------------------------
# Helper Functions
# ------------------------

monthly_interest_rate <- function(annual_rate) {
  return(annual_rate / 12.0)
}

annuity_monthly_payment <- function(principal, annual_rate, years) {
  r <- monthly_interest_rate(annual_rate)
  n <- years * 12
  if (r == 0) {
    return(principal / n)
  }
  principal * (r / (1 - (1 + r)^(-n)))
}

simulate_mortgage_and_investing <- function(
    principal,
    annual_mort_rate,
    total_monthly_budget,
    years,
    annual_invest_return,
    annual_property_growth,
    mortgage_style = 'annuity',
    initial_property_value
) {
  months <- years * 12
  r_m <- monthly_interest_rate(annual_mort_rate)
  r_i <- monthly_interest_rate(annual_invest_return)
  r_p <- monthly_interest_rate(annual_property_growth)
  
  month <- 1:months
  mortgage_payment <- numeric(months)
  interest_paid <- numeric(months)
  principal_paid <- numeric(months)
  invested_this_month <- numeric(months)
  principal_owed <- numeric(months)
  property_value <- numeric(months)
  investment_balance <- numeric(months)
  
  cumulative_investment_contribution <- numeric(months)
  investment_gains <- numeric(months)
  property_appreciation <- numeric(months)
  real_estate_equity <- numeric(months)
  
  curr_principal <- principal
  investment <- 0.0
  property_val <- initial_property_value
  cum_invested <- 0.0
  
  if (mortgage_style == 'annuity') {
    monthly_mortgage_payment <- annuity_monthly_payment(principal, annual_mort_rate, years)
  }
  
  for (m in seq_len(months)) {
    # 1) Property grows
    property_val <- property_val * (1 + r_p)
    
    # 2) Interest
    interest <- curr_principal * r_m
    
    # 3) Mortgage Payment & Principal
    if (mortgage_style == 'annuity') {
      mort_payment <- monthly_mortgage_payment
      principal_pay <- mort_payment - interest
      if (principal_pay > curr_principal) {
        principal_pay <- curr_principal
        mort_payment <- interest + principal_pay
      }
    } else if (mortgage_style == 'equal_principal') {
      principal_fixed <- principal / (years * 12)
      if (curr_principal < principal_fixed) {
        principal_fixed <- curr_principal
      }
      mort_payment <- principal_fixed + interest
      principal_pay <- principal_fixed
    } else {
      stop("Unsupported mortgage style.")
    }
    
    # 4) Update principal
    curr_principal <- curr_principal - principal_pay
    if (curr_principal < 0) curr_principal <- 0
    
    # 5) Determine how much to invest
    invest_this <- total_monthly_budget - mort_payment
    if (invest_this < 0) invest_this <- 0
    
    # 6) Grow investment
    investment <- investment * (1 + r_i) + invest_this
    cum_invested <- cum_invested + invest_this
    
    # 7) Store
    mortgage_payment[m] <- mort_payment
    interest_paid[m] <- interest
    principal_paid[m] <- principal_pay
    invested_this_month[m] <- invest_this
    principal_owed[m] <- curr_principal
    property_value[m] <- property_val
    investment_balance[m] <- investment
    
    cumulative_investment_contribution[m] <- cum_invested
    investment_gains[m] <- investment - cum_invested
    property_appreciation[m] <- property_val - initial_property_value
    real_estate_equity[m] <- property_val - curr_principal
  }
  
  data.frame(
    month = month,
    mortgage_payment = mortgage_payment,
    interest_paid = interest_paid,
    principal_paid = principal_paid,
    invested_this_month = invested_this_month,
    principal_owed = principal_owed,
    property_value = property_value,
    investment_balance = investment_balance,
    cumulative_investment_contribution = cumulative_investment_contribution,
    investment_gains = investment_gains,
    property_appreciation = property_appreciation,
    real_estate_equity = real_estate_equity
  )
}

simulate_renting_and_investing <- function(
    monthly_invest,
    start_invest = 20000,
    annual_invest_return = 0.05,
    years = 30
) {
  months <- years * 12
  r_i <- monthly_interest_rate(annual_invest_return)
  
  month <- 1:months
  investment_balance <- numeric(months)
  cumulative_investment_contribution <- numeric(months)
  investment_gains <- numeric(months)
  
  balance <- start_invest
  cum_invested <- start_invest
  
  for (m in seq_len(months)) {
    balance <- balance * (1 + r_i) + monthly_invest
    cum_invested <- cum_invested + monthly_invest
    
    investment_balance[m] <- balance
    cumulative_investment_contribution[m] <- cum_invested
    investment_gains[m] <- balance - cum_invested
  }
  
  data.frame(
    month = month,
    investment_balance = investment_balance,
    cumulative_investment_contribution = cumulative_investment_contribution,
    investment_gains = investment_gains,
    # placeholders for rent scenario
    property_value = NA,
    property_appreciation = NA,
    principal_owed = NA,
    real_estate_equity = NA
  )
}

# ------------------------
# UI
# ------------------------

ui <- fluidPage(
  titlePanel("Mortgage and Investment Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Simulation Parameters"),
      
      numericInput("annual_mort_rate", "Annual Mortgage Rate (%):", 
                   value = 3.7, min = 0, step = 0.1),
      helpText("Enter the annual interest rate for the mortgage."),
      
      numericInput("annual_invest_return", "Annual Investment Return (%):", 
                   value = 5, min = 0, step = 0.1),
      helpText("Enter the expected annual return rate for investments."),
      
      numericInput("annual_property_growth", "Annual Property Growth (%):", 
                   value = 2, min = 0, step = 0.1),
      helpText("Enter the expected annual growth rate for property value."),
      
      sliderInput("years", "Mortgage Term (Years):", 
                  min = 1, max = 40, value = 30, step = 1),
      
      sliderInput("interval_years", "Analysis Interval (Years):", 
                  min = 1, max = 10, value = 5, step = 1),
      
      hr(),
      
      # Option A
      h4("Option A: Buy (3-room)"),
      numericInput("full_priceA", "Full Price (€):", value = 200000, min = 0, step = 1000),
      numericInput("down_paymentA", "Down Payment (€):", value = 20000, min = 0, step = 1000),
      numericInput("investable_incomeA", "Investable Income (€):", value = 1445, min = 0, step = 100),
      selectInput("mortgage_styleA", "Mortgage Style:", 
                  choices = c("Annuity" = "annuity", "Equal Principal" = "equal_principal"), 
                  selected = "annuity"),
      
      hr(),
      
      # Option B
      h4("Option B: Buy (2-room)"),
      numericInput("full_priceB", "Full Price (€):", value = 150000, min = 0, step = 1000),
      numericInput("down_paymentB", "Down Payment (€):", value = 15000, min = 0, step = 1000),
      numericInput("investable_incomeB", "Investable Income (€):", value = 1631, min = 0, step = 100),
      selectInput("mortgage_styleB", "Mortgage Style:", 
                  choices = c("Annuity" = "annuity", "Equal Principal" = "equal_principal"), 
                  selected = "annuity"),
      
      hr(),
      
      # Option C
      h4("Option C: Rent & Invest"),
      numericInput("start_investC", "Initial Investment (€):", value = 20000, min = 0, step = 1000),
      numericInput("monthly_investC", "Monthly Investment (€):", value = 1640, min = 0, step = 100),
      
      hr(),
      helpText("Use the sliders to adjust the mortgage term and analysis interval.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Equity Table", 
                 br(), 
                 tableOutput("equityTable")
        ),
        tabPanel("Investment Balances",
                 br(),
                 plotOutput("plotInvestmentBalances", height = "600px")
        ),
        tabPanel("Principal Owed",
                 br(),
                 plotOutput("plotPrincipalOwed", height = "600px")
        ),
        tabPanel("Property Value",
                 br(),
                 plotOutput("plotPropertyValue", height = "600px")
        ),
        tabPanel("Monthly Cash Flows",
                 br(),
                 plotOutput("plotMonthlyCashFlows", height = "600px")
        ),
        tabPanel("Detailed Gains & Contributions",
                 br(),
                 plotOutput("plotAllDetailedGains", height = "600px")
        )
      )
    )
  )
)

# ------------------------
# Server
# ------------------------

server <- function(input, output) {
  
  # ------------------------
  # Reactive Simulations
  # ------------------------
  
  dataA <- reactive({
    principalA <- input$full_priceA - input$down_paymentA
    if (principalA < 0) {
      showNotification("Down Payment for Option A cannot exceed Full Price.", type = "error")
      principalA <- 0
    }
    
    if (input$mortgage_styleA == "annuity") {
      mort_payment <- annuity_monthly_payment(principalA, input$annual_mort_rate / 100, input$years)
    } else {
      mort_payment <- (principalA / (input$years * 12)) + 
        (principalA * (input$annual_mort_rate / 100) / 12)
    }
    total_budgetA <- input$investable_incomeA + mort_payment
    
    simulate_mortgage_and_investing(
      principal = principalA,
      annual_mort_rate = input$annual_mort_rate / 100,
      total_monthly_budget = total_budgetA,
      years = input$years,
      annual_invest_return = input$annual_invest_return / 100,
      annual_property_growth = input$annual_property_growth / 100,
      mortgage_style = input$mortgage_styleA,
      initial_property_value = input$full_priceA
    )
  })
  
  dataB <- reactive({
    principalB <- input$full_priceB - input$down_paymentB
    if (principalB < 0) {
      showNotification("Down Payment for Option B cannot exceed Full Price.", type = "error")
      principalB <- 0
    }
    
    if (input$mortgage_styleB == "annuity") {
      mort_payment <- annuity_monthly_payment(principalB, input$annual_mort_rate / 100, input$years)
    } else {
      mort_payment <- (principalB / (input$years * 12)) + 
        (principalB * (input$annual_mort_rate / 100) / 12)
    }
    total_budgetB <- input$investable_incomeB + mort_payment
    
    simulate_mortgage_and_investing(
      principal = principalB,
      annual_mort_rate = input$annual_mort_rate / 100,
      total_monthly_budget = total_budgetB,
      years = input$years,
      annual_invest_return = input$annual_invest_return / 100,
      annual_property_growth = input$annual_property_growth / 100,
      mortgage_style = input$mortgage_styleB,
      initial_property_value = input$full_priceB
    )
  })
  
  dataC <- reactive({
    simulate_renting_and_investing(
      monthly_invest = input$monthly_investC,
      start_invest = input$start_investC,
      annual_invest_return = input$annual_invest_return / 100,
      years = input$years
    )
  })
  
  # ------------------------
  # Equity computation
  # ------------------------
  
  equity_at_year <- function(data_dict, year, init_property_val, init_principal) {
    
    m <- year * 12
    if (m > nrow(data_dict)) {
      stop(paste("Year", year, "exceeds the simulation period."))
    }
    
    # For A/B:
    prop_val  <- data_dict$property_value[m]
    prop_app  <- data_dict$property_appreciation[m]
    owed      <- data_dict$principal_owed[m]
    invest_bal<- data_dict$investment_balance[m]
    contrib   <- data_dict$cumulative_investment_contribution[m]
    gains     <- data_dict$investment_gains[m]
    
    # If renting, those might be NA
    if (is.na(prop_val))  prop_val  <- 0
    if (is.na(prop_app))  prop_app  <- 0
    if (is.na(owed))      owed      <- 0
    
    total_eq <- (prop_val - owed) + invest_bal
    
    list(
      # Mortgage / property
      property_value        = prop_val,
      property_appreciation = prop_app,
      principal_owed        = owed,
      
      # Investments
      cumulative_contrib = contrib,
      investment_gains   = gains,
      investment_balance = invest_bal,
      
      # Totals
      total_equity = total_eq
    )
  }
  
  equityTable <- reactive({
    total_years <- input$years
    interval    <- input$interval_years
    
    yrs_vector <- seq(interval, total_years, by = interval)
    if (tail(yrs_vector, 1) != total_years) {
      yrs_vector <- c(yrs_vector, total_years)
    }
    yrs_vector <- unique(yrs_vector[yrs_vector <= total_years])
    if (length(yrs_vector) == 0) {
      return(NULL)
    }
    
    eqA <- lapply(yrs_vector, function(y) {
      equity_at_year(dataA(), y, input$full_priceA, input$full_priceA - input$down_paymentA)
    })
    eqB <- lapply(yrs_vector, function(y) {
      equity_at_year(dataB(), y, input$full_priceB, input$full_priceB - input$down_paymentB)
    })
    eqC <- lapply(yrs_vector, function(y) {
      equity_at_year(dataC(), y, 0, 0)
    })
    
    get_val <- function(lst, field) if (!is.null(lst[[field]])) lst[[field]] else 0
    
    # Option A
    A_propVal <- sapply(eqA, get_val, "property_value")
    A_propApp <- sapply(eqA, get_val, "property_appreciation")
    A_principal<- sapply(eqA, get_val, "principal_owed")
    A_contrib <- sapply(eqA, get_val, "cumulative_contrib")
    A_gains   <- sapply(eqA, get_val, "investment_gains")
    A_invest  <- sapply(eqA, get_val, "investment_balance")
    A_equity  <- sapply(eqA, get_val, "total_equity")
    
    # Option B
    B_propVal <- sapply(eqB, get_val, "property_value")
    B_propApp <- sapply(eqB, get_val, "property_appreciation")
    B_principal<- sapply(eqB, get_val, "principal_owed")
    B_contrib <- sapply(eqB, get_val, "cumulative_contrib")
    B_gains   <- sapply(eqB, get_val, "investment_gains")
    B_invest  <- sapply(eqB, get_val, "investment_balance")
    B_equity  <- sapply(eqB, get_val, "total_equity")
    
    # Option C
    C_propVal <- sapply(eqC, get_val, "property_value")
    C_propApp <- sapply(eqC, get_val, "property_appreciation")
    C_principal<- sapply(eqC, get_val, "principal_owed")
    C_contrib <- sapply(eqC, get_val, "cumulative_contrib")
    C_gains   <- sapply(eqC, get_val, "investment_gains")
    C_invest  <- sapply(eqC, get_val, "investment_balance")
    C_equity  <- sapply(eqC, get_val, "total_equity")
    
    df <- data.frame(
      Year = as.integer(yrs_vector),
      
      # Option A
      A_PropertyValue = A_propVal,
      A_PropertyApp   = A_propApp,
      A_PrincipalOwed = A_principal,
      A_Contrib       = A_contrib,
      A_Gains         = A_gains,
      A_Invest        = A_invest,
      A_Equity        = A_equity,
      
      # Option B
      B_PropertyValue = B_propVal,
      B_PropertyApp   = B_propApp,
      B_PrincipalOwed = B_principal,
      B_Contrib       = B_contrib,
      B_Gains         = B_gains,
      B_Invest        = B_invest,
      B_Equity        = B_equity,
      
      # Option C
      C_PropertyValue = C_propVal,
      C_PropertyApp   = C_propApp,
      C_PrincipalOwed = C_principal,
      C_Contrib       = C_contrib,
      C_Gains         = C_gains,
      C_Invest        = C_invest,
      C_Equity        = C_equity
    )
    
    # Format big numbers with space as thousands separator
    numeric_cols <- setdiff(names(df), "Year")
    for (col in numeric_cols) {
      df[[col]] <- formatC(df[[col]], format = "f", big.mark = " ", digits = 0)
    }
    
    df
  })
  
  output$equityTable <- renderTable({
    equityTable()
  },
  striped = TRUE,
  hover = TRUE,
  align = "c",
  sanitize.text.function = function(x) x,
  include.rownames = FALSE,
  caption = "Equity at Selected Intervals"
  )
  
  # ------------------------
  # Plots
  # ------------------------
  
  output$plotInvestmentBalances <- renderPlot({
    plotA <- dataA()
    plotB <- dataB()
    plotC <- dataC()
    
    dfA <- data.frame(month = plotA$month, value = plotA$investment_balance, option = "A")
    dfB <- data.frame(month = plotB$month, value = plotB$investment_balance, option = "B")
    dfC <- data.frame(month = plotC$month, value = plotC$investment_balance, option = "C")
    
    df <- rbind(dfA, dfB, dfC)
    ggplot(df, aes(x = month, y = value, color = option)) +
      geom_line() +
      scale_x_continuous(limits = c(0, input$years * 12)) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title = "Investment Balances Over Time", x = "Month", y = "€", color = "Option") +
      theme_minimal()
  })
  
  output$plotPrincipalOwed <- renderPlot({
    plotA <- dataA()
    plotB <- dataB()
    
    dfA <- data.frame(month = plotA$month, value = plotA$principal_owed, option = "A")
    dfB <- data.frame(month = plotB$month, value = plotB$principal_owed, option = "B")
    
    df <- rbind(dfA, dfB)
    ggplot(df, aes(x = month, y = value, color = option)) +
      geom_line() +
      scale_x_continuous(limits = c(0, input$years * 12)) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title = "Mortgage Principal Owed", x = "Month", y = "€", color = "Option") +
      theme_minimal()
  })
  
  output$plotPropertyValue <- renderPlot({
    plotA <- dataA()
    plotB <- dataB()
    
    dfA <- data.frame(month = plotA$month, value = plotA$property_value, option = "A")
    dfB <- data.frame(month = plotB$month, value = plotB$property_value, option = "B")
    
    df <- rbind(dfA, dfB)
    ggplot(df, aes(x = month, y = value, color = option)) +
      geom_line() +
      scale_x_continuous(limits = c(0, input$years * 12)) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title = "Property Value Growth", x = "Month", y = "€", color = "Option") +
      theme_minimal()
  })
  
  output$plotMonthlyCashFlows <- renderPlot({
    plotA <- dataA()
    cashflowA <- plotA[, c("month", "mortgage_payment", "invested_this_month")]
    cashflowA_long <- melt(cashflowA, id.vars = "month",
                           variable.name = "CashFlow",
                           value.name = "Amount")
    cashflowA_long$CashFlow <- factor(cashflowA_long$CashFlow,
                                      levels = c("mortgage_payment", "invested_this_month"),
                                      labels = c("Mortgage Payment", "Invested Amount"))
    
    ggplot(cashflowA_long, aes(x = month, y = Amount, color = CashFlow)) +
      geom_line() +
      scale_x_continuous(limits = c(0, input$years * 12)) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title = "Monthly Cash Flows (Option A)", x = "Month", y = "€", color = "Cash Flow") +
      theme_minimal()
  })
  
  output$plotAllDetailedGains <- renderPlot({
    dfA <- dataA(); dfA$Option <- "A"
    dfB <- dataB(); dfB$Option <- "B"
    dfC <- dataC(); dfC$Option <- "C"
    
    dfAll <- rbind(
      dfA[, c("month","Option","cumulative_investment_contribution",
              "investment_gains","property_appreciation","real_estate_equity")],
      dfB[, c("month","Option","cumulative_investment_contribution",
              "investment_gains","property_appreciation","real_estate_equity")],
      dfC[, c("month","Option","cumulative_investment_contribution",
              "investment_gains","property_appreciation","real_estate_equity")]
    )
    
    dfLong <- melt(dfAll, id.vars = c("month","Option"))
    ggplot(dfLong, aes(x = month, y = value, color = Option)) +
      geom_line(na.rm = TRUE) +
      facet_wrap(~variable, scales = "free_y",
                 labeller = as_labeller(c(
                   "cumulative_investment_contribution" = "Cumulative Invested",
                   "investment_gains" = "Investment Gains",
                   "property_appreciation" = "Property Appreciation",
                   "real_estate_equity" = "Real Estate Equity"
                 ))) +
      scale_x_continuous(limits = c(0, input$years * 12)) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title = "Detailed Gains & Contributions (All Options)",
           x = "Month", y = "€", color = "Option") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
