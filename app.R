# --------------------------------------------
# GLOBALS & LIBRARIES
# --------------------------------------------
library(shiny)
library(ggplot2)
library(reshape2)
library(scales)    # For formatting monetary values
library(plotly)    # For interactive plots

# --------------------------------------------
# Helper Functions
# --------------------------------------------

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
  # Number of months
  months <- years * 12
  
  # Monthly rates
  r_m <- monthly_interest_rate(annual_mort_rate)      # mortgage
  r_i <- monthly_interest_rate(annual_invest_return)  # investment
  r_p <- monthly_interest_rate(annual_property_growth)# property
  
  # Initialize vectors
  month                     <- 1:months
  mortgage_payment         <- numeric(months)
  interest_paid            <- numeric(months)  # will store cumulative interest
  principal_paid           <- numeric(months)
  invested_this_month      <- numeric(months)
  principal_owed           <- numeric(months)
  property_value           <- numeric(months)
  investment_balance       <- numeric(months)
  
  cumulative_investment_contribution <- numeric(months)
  investment_gains                   <- numeric(months)
  property_appreciation             <- numeric(months)
  real_estate_equity                <- numeric(months)
  
  # Tracking variables
  curr_principal <- principal
  investment     <- 0.0
  property_val   <- initial_property_value
  cum_invested   <- 0.0
  running_interest <- 0.0   # <--- to track cumulative interest over time
  
  # Pre-calculate monthly mortgage payment if annuity
  if (mortgage_style == 'annuity') {
    monthly_mortgage_payment <- annuity_monthly_payment(principal, annual_mort_rate, years)
  }
  
  for (m in seq_len(months)) {
    
    # 1) Property grows
    property_val <- property_val * (1 + r_p)
    
    # 2) Compute *this month's* interest amount on the current principal
    monthly_interest <- curr_principal * r_m
    
    # 3) Mortgage Payment & Principal Pay
    if (mortgage_style == 'annuity') {
      mort_payment   <- monthly_mortgage_payment
      principal_pay  <- mort_payment - monthly_interest
      # Prevent overpaying principal near the end
      if (principal_pay > curr_principal) {
        principal_pay <- curr_principal
        mort_payment  <- monthly_interest + principal_pay
      }
    } else if (mortgage_style == 'equal_principal') {
      principal_fixed <- principal / (years * 12)
      if (curr_principal < principal_fixed) {
        principal_fixed <- curr_principal
      }
      mort_payment  <- principal_fixed + monthly_interest
      principal_pay <- principal_fixed
    } else {
      stop("Unsupported mortgage style.")
    }
    
    # 4) Update mortgage principal
    curr_principal <- curr_principal - principal_pay
    if (curr_principal < 0) curr_principal <- 0
    
    # 5) Determine how much to invest (budget minus mortgage payment)
    invest_this <- total_monthly_budget - mort_payment
    if (invest_this < 0) invest_this <- 0
    
    # 6) Grow investment
    investment   <- investment * (1 + r_i) + invest_this
    cum_invested <- cum_invested + invest_this
    
    # 7) Update cumulative interest
    running_interest <- running_interest + monthly_interest
    
    # 8) Store in vectors
    mortgage_payment[m]               <- mort_payment
    interest_paid[m]                  <- running_interest   # now cumulative!
    principal_paid[m]                 <- principal_pay
    invested_this_month[m]            <- invest_this
    principal_owed[m]                 <- curr_principal
    property_value[m]                 <- property_val
    investment_balance[m]             <- investment
    
    cumulative_investment_contribution[m] <- cum_invested
    investment_gains[m]               <- investment - cum_invested
    property_appreciation[m]          <- property_val - initial_property_value
    real_estate_equity[m]             <- property_val - curr_principal
  }
  
  # Put into data.frame
  df <- data.frame(
    month                               = month,
    mortgage_payment                    = mortgage_payment,
    # interest_paid is now cumulative
    interest_paid                       = interest_paid, 
    principal_paid                      = principal_paid,
    invested_this_month                 = invested_this_month,
    principal_owed                      = principal_owed,
    property_value                      = property_value,
    investment_balance                  = investment_balance,
    cumulative_investment_contribution = cumulative_investment_contribution,
    investment_gains                    = investment_gains,
    property_appreciation              = property_appreciation,
    real_estate_equity                 = real_estate_equity
  )
  
  # total_equity = real_estate_equity + investment_balance
  df$total_equity <- df$real_estate_equity + df$investment_balance
  
  return(df)
}

# Updated: Rent & Invest (no mortgage interest to accumulate)
simulate_renting_and_investing <- function(
    monthly_budget,
    start_invest = 20000,
    initial_rent = 800,
    annual_rent_increase = 0.02,
    annual_invest_return = 0.05,
    years = 30
) {
  months <- years * 12
  r_i <- annual_invest_return / 12  # monthly invest return
  
  month_vec <- 1:months
  investment_balance <- numeric(months)
  cumulative_investment_contribution <- numeric(months)
  investment_gains <- numeric(months)
  monthly_rent_paid <- numeric(months)
  monthly_invested <- numeric(months)
  
  balance <- start_invest
  cum_invested <- start_invest
  
  for (m in month_vec) {
    # which "year" we are in (0-based)
    yearIndex <- (m - 1) %/% 12
    
    # current rent
    current_rent <- initial_rent * (1 + annual_rent_increase)^yearIndex
    
    # leftover
    amount_left_for_invest <- monthly_budget - current_rent
    if (amount_left_for_invest < 0) amount_left_for_invest <- 0
    
    # grow existing investment
    balance <- balance * (1 + r_i)
    
    # add this month's new investment
    balance <- balance + amount_left_for_invest
    cum_invested <- cum_invested + amount_left_for_invest
    
    # store
    investment_balance[m] <- balance
    cumulative_investment_contribution[m] <- cum_invested
    investment_gains[m] <- balance - cum_invested
    monthly_rent_paid[m] <- current_rent
    monthly_invested[m] <- amount_left_for_invest
  }
  
  df <- data.frame(
    month = month_vec,
    investment_balance = investment_balance,
    cumulative_investment_contribution = cumulative_investment_contribution,
    investment_gains = investment_gains,
    monthly_rent = monthly_rent_paid,
    monthly_invested = monthly_invested,
    
    # placeholders
    property_value = NA,
    property_appreciation = NA,
    principal_owed = NA,
    real_estate_equity = NA
  )
  
  # treat NA real_estate_equity as 0
  df$real_estate_equity[is.na(df$real_estate_equity)] <- 0
  
  # total_equity
  df$total_equity <- df$real_estate_equity + df$investment_balance
  
  return(df)
}

# --------------------------------------------
# UI
# --------------------------------------------
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
      
      # We fix the mortgage to 30 years, so no slider for loan length.
      
      # Interval for the Equity Table only
      sliderInput("interval_years", "Analysis Interval (Years):", 
                  min = 1, max = 10, value = 5, step = 1),
      
      # slider to mask the x-axis range in plots (and table)
      sliderInput("maxPlotYears", "Show up to Year:", 
                  min = 1, max = 30, value = 10, step = 1),
      
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
      numericInput("monthly_investC", "Monthly Budget for Rent+Invest (€):", value = 1640, min = 0, step = 100),
      numericInput("initial_rentC", "Initial Monthly Rent (€):", value = 800, min = 0, step = 50),
      numericInput("annual_rent_increaseC", "Annual Rent Increase (%):", value = 2, min = 0, step = 0.1),
      
      hr(),
      helpText("Use the 'Show up to Year' slider to limit both the table range and the plot range.")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Equity Table",
                 br(),
                 # Explanation text for Equity Table columns
                 HTML("<b>Columns Explanation:</b><br>
                      <ul>
                        <li><b>Year:</b> The specific year of the simulation.</li>
                        <li><b>_PropertyValue:</b> The estimated value of the property at that time.</li>
                        <li><b>_PropertyApp:</b> The total appreciated amount of the property (relative to initial value).</li>
                        <li><b>_PrincipalOwed:</b> The remaining mortgage principal owed.</li>
                        <li><b>_Contrib:</b> The cumulative amount contributed (invested) so far.</li>
                        <li><b>_Gains:</b> The net investment gains (Balance - Contributions).</li>
                        <li><b>_Invest:</b> The current investment balance.</li>
                        <li><b>_Equity:</b> The sum of (Property Value - Principal Owed + Investment Balance).</li>
                      </ul>"),
                 tableOutput("equityTable")
        ),
        
        tabPanel("Total Equity Over Time",
                 br(),
                 plotlyOutput("plotInvestmentBalances", height = "600px")
        ),
        
        tabPanel("Principal Owed + Interest Paid",
                 br(),
                 plotlyOutput("plotPrincipalOwed", height = "600px")
        ),
        tabPanel("Property Value",
                 br(),
                 plotlyOutput("plotPropertyValue", height = "600px")
        ),
        
        tabPanel("Detailed Gains & Contributions",
                 br(),
                 # Explanation for each metric in the Detailed Gains
                 HTML("<b>Explanation of Gains & Contributions:</b><br>
                      <ul>
                        <li><b>Cumulative Invested:</b> Total amount of money contributed towards investments so far.</li>
                        <li><b>Investment Gains:</b> The net growth in the investment portion (balance minus contributions).</li>
                        <li><b>Property Appreciation:</b> How much the property has increased in value over the initial purchase price.</li>
                        <li><b>Real Estate Equity:</b> The portion of the property you own (Property Value minus Mortgage Principal).</li>
                      </ul>
                      <p>All amounts in €.</p>"),
                 plotlyOutput("plotAllDetailedGains", height = "600px")
        )
      )
    )
  )
)

# --------------------------------------------
# SERVER
# --------------------------------------------
server <- function(input, output) {
  
  # Fixed mortgage term = 30 years
  totalTermYears <- 30
  
  # ------------------------
  # Reactive Simulations
  # ------------------------
  
  # Option A
  dataA <- reactive({
    principalA <- input$full_priceA - input$down_paymentA
    if (principalA < 0) {
      showNotification("Down Payment for Option A cannot exceed Full Price.", type = "error")
      principalA <- 0
    }
    
    if (input$mortgage_styleA == "annuity") {
      mort_payment <- annuity_monthly_payment(principalA, input$annual_mort_rate / 100, totalTermYears)
    } else {
      # Simple approximation for equal principal style
      mort_payment <- (principalA / (totalTermYears * 12)) +
        (principalA * (input$annual_mort_rate / 100) / 12)
    }
    total_budgetA <- input$investable_incomeA + mort_payment
    
    simulate_mortgage_and_investing(
      principal = principalA,
      annual_mort_rate = input$annual_mort_rate / 100,
      total_monthly_budget = total_budgetA,
      years = totalTermYears,
      annual_invest_return = input$annual_invest_return / 100,
      annual_property_growth = input$annual_property_growth / 100,
      mortgage_style = input$mortgage_styleA,
      initial_property_value = input$full_priceA
    )
  })
  
  # Option B
  dataB <- reactive({
    principalB <- input$full_priceB - input$down_paymentB
    if (principalB < 0) {
      showNotification("Down Payment for Option B cannot exceed Full Price.", type = "error")
      principalB <- 0
    }
    
    if (input$mortgage_styleB == "annuity") {
      mort_payment <- annuity_monthly_payment(principalB, input$annual_mort_rate / 100, totalTermYears)
    } else {
      # Simple approximation for equal principal style
      mort_payment <- (principalB / (totalTermYears * 12)) +
        (principalB * (input$annual_mort_rate / 100) / 12)
    }
    total_budgetB <- input$investable_incomeB + mort_payment
    
    simulate_mortgage_and_investing(
      principal = principalB,
      annual_mort_rate = input$annual_mort_rate / 100,
      total_monthly_budget = total_budgetB,
      years = totalTermYears,
      annual_invest_return = input$annual_invest_return / 100,
      annual_property_growth = input$annual_property_growth / 100,
      mortgage_style = input$mortgage_styleB,
      initial_property_value = input$full_priceB
    )
  })
  
  # Option C
  dataC <- reactive({
    simulate_renting_and_investing(
      monthly_budget       = input$monthly_investC,
      start_invest        = input$start_investC,
      initial_rent        = input$initial_rentC,
      annual_rent_increase= input$annual_rent_increaseC / 100,
      annual_invest_return= input$annual_invest_return / 100,
      years               = totalTermYears
    )
  })
  
  # ------------------------
  # Equity Table
  # ------------------------
  
  equity_at_year <- function(data_dict, year) {
    m <- year * 12
    if (m > nrow(data_dict)) {
      stop(paste("Year", year, "exceeds the simulation period."))
    }
    
    prop_val  <- data_dict$property_value[m]
    prop_app  <- data_dict$property_appreciation[m]
    owed      <- data_dict$principal_owed[m]
    invest_bal<- data_dict$investment_balance[m]
    contrib   <- data_dict$cumulative_investment_contribution[m]
    gains     <- data_dict$investment_gains[m]
    
    # Handle NAs for rent scenario
    if (is.na(prop_val))  prop_val  <- 0
    if (is.na(prop_app))  prop_app  <- 0
    if (is.na(owed))      owed      <- 0
    
    total_eq <- (prop_val - owed) + invest_bal
    
    list(
      property_value        = prop_val,
      property_appreciation = prop_app,
      principal_owed        = owed,
      cumulative_contrib    = contrib,
      investment_gains      = gains,
      investment_balance    = invest_bal,
      total_equity          = total_eq
    )
  }
  
  equityTable <- reactive({
    # We'll limit the table to input$maxPlotYears
    endYear   <- min(totalTermYears, input$maxPlotYears)
    interval  <- input$interval_years
    
    # Step by 'interval' up to 'endYear'
    yrs_vector <- seq(interval, endYear, by = interval)
    if (tail(yrs_vector, 1) != endYear) {
      yrs_vector <- c(yrs_vector, endYear)
    }
    yrs_vector <- unique(yrs_vector[yrs_vector <= endYear])
    if (length(yrs_vector) == 0) {
      return(NULL)
    }
    
    eqA <- lapply(yrs_vector, function(y) equity_at_year(dataA(), y))
    eqB <- lapply(yrs_vector, function(y) equity_at_year(dataB(), y))
    eqC <- lapply(yrs_vector, function(y) equity_at_year(dataC(), y))
    
    get_val <- function(lst, field) if (!is.null(lst[[field]])) lst[[field]] else 0
    
    # Option A
    A_propVal    <- sapply(eqA, get_val, "property_value")
    A_propApp    <- sapply(eqA, get_val, "property_appreciation")
    A_principal  <- sapply(eqA, get_val, "principal_owed")
    A_contrib    <- sapply(eqA, get_val, "cumulative_contrib")
    A_gains      <- sapply(eqA, get_val, "investment_gains")
    A_invest     <- sapply(eqA, get_val, "investment_balance")
    A_equity     <- sapply(eqA, get_val, "total_equity")
    
    # Option B
    B_propVal    <- sapply(eqB, get_val, "property_value")
    B_propApp    <- sapply(eqB, get_val, "property_appreciation")
    B_principal  <- sapply(eqB, get_val, "principal_owed")
    B_contrib    <- sapply(eqB, get_val, "cumulative_contrib")
    B_gains      <- sapply(eqB, get_val, "investment_gains")
    B_invest     <- sapply(eqB, get_val, "investment_balance")
    B_equity     <- sapply(eqB, get_val, "total_equity")
    
    # Option C
    C_propVal    <- sapply(eqC, get_val, "property_value")
    C_propApp    <- sapply(eqC, get_val, "property_appreciation")
    C_principal  <- sapply(eqC, get_val, "principal_owed")
    C_contrib    <- sapply(eqC, get_val, "cumulative_contrib")
    C_gains      <- sapply(eqC, get_val, "investment_gains")
    C_invest     <- sapply(eqC, get_val, "investment_balance")
    C_equity     <- sapply(eqC, get_val, "total_equity")
    
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
    
    # Format big numbers (thousands separator)
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
  # PLOTS (using plotly for interactivity)
  # ------------------------
  
  reactivePlotMonths <- reactive({
    input$maxPlotYears * 12
  })
  
  # 1) Total Equity Over Time
  output$plotInvestmentBalances <- renderPlotly({
    plotA <- dataA()
    plotB <- dataB()
    plotC <- dataC()
    
    dfA <- data.frame(month = plotA$month, value = plotA$total_equity, option = "A")
    dfB <- data.frame(month = plotB$month, value = plotB$total_equity, option = "B")
    dfC <- data.frame(month = plotC$month, value = plotC$total_equity, option = "C")
    
    df <- rbind(dfA, dfB, dfC)
    
    p <- ggplot(df, aes(x = month, y = value, color = option)) +
      geom_line() +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(
        title = "Total Equity Over Time",
        x = "Month",
        y = "€",
        color = "Option"
      ) +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(x = 1, y = 1))
  })
  
  # 2) Principal Owed + Interest Paid
  #    We now have 'interest_paid' as a *cumulative* figure.
  #    We'll facet so interest doesn't appear "flat".
  output$plotPrincipalOwed <- renderPlotly({
    plotA <- dataA()
    plotB <- dataB()
    
    dfA <- data.frame(
      month = plotA$month,
      PrincipalOwed = plotA$principal_owed,
      InterestPaid  = plotA$interest_paid,  # now cumulative
      option = "A"
    )
    dfB <- data.frame(
      month = plotB$month,
      PrincipalOwed = plotB$principal_owed,
      InterestPaid  = plotB$interest_paid,  # now cumulative
      option = "B"
    )
    df <- rbind(dfA, dfB)
    
    # Melt for two lines
    dfLong <- melt(df, id.vars = c("month", "option"),
                   variable.name = "Metric", value.name = "Value")
    
    # facet_wrap with free_y
    p <- ggplot(dfLong, aes(x = month, y = Value, color = option)) +
      geom_line() +
      facet_wrap(~Metric, scales = "free_y") +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title = "Mortgage Principal Owed + Interest Paid (Faceted)",
           x = "Month", y = "€", color = "Option") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(x = 1, y = 1))
  })
  
  # 3) Property Value (Only for mortgages A/B)
  output$plotPropertyValue <- renderPlotly({
    plotA <- dataA()
    plotB <- dataB()
    
    dfA <- data.frame(month = plotA$month, value = plotA$property_value, option = "A")
    dfB <- data.frame(month = plotB$month, value = plotB$property_value, option = "B")
    
    df <- rbind(dfA, dfB)
    
    p <- ggplot(df, aes(x = month, y = value, color = option)) +
      geom_line() +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title = "Property Value Growth", x = "Month", y = "€", color = "Option") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(x = 1, y = 1))
  })
  
  # 4) Detailed Gains & Contributions
  output$plotAllDetailedGains <- renderPlotly({
    dfA <- dataA(); dfA$Option <- "A"
    dfB <- dataB(); dfB$Option <- "B"
    dfC <- dataC(); dfC$Option <- "C"
    
    # Combine relevant columns
    dfAll <- rbind(
      dfA[, c("month","Option","cumulative_investment_contribution",
              "investment_gains","property_appreciation","real_estate_equity")],
      dfB[, c("month","Option","cumulative_investment_contribution",
              "investment_gains","property_appreciation","real_estate_equity")],
      dfC[, c("month","Option","cumulative_investment_contribution",
              "investment_gains","property_appreciation","real_estate_equity")]
    )
    
    dfLong <- melt(dfAll, id.vars = c("month","Option"))
    
    p <- ggplot(dfLong, aes(x = month, y = value, color = Option)) +
      geom_line(na.rm = TRUE) +
      facet_wrap(~variable, scales = "free_y",
                 labeller = as_labeller(c(
                   "cumulative_investment_contribution" = "Cumulative Invested",
                   "investment_gains" = "Investment Gains",
                   "property_appreciation" = "Property Appreciation",
                   "real_estate_equity" = "Real Estate Equity"
                 ))) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title = "Detailed Gains & Contributions (All Options)",
           x = "Month", y = "€", color = "Option") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(x = 1, y = 1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
