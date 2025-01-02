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
  month                <- 1:months
  mortgage_payment     <- numeric(months)
  total_interest_paid  <- numeric(months)  # cumulative interest
  monthly_principal_paid <- numeric(months)
  
  invested_this_month  <- numeric(months)
  property_value       <- numeric(months)
  inv_balance          <- numeric(months)
  
  inv_contrib          <- numeric(months)  # cumulative investment
  inv_gains            <- numeric(months)
  property_app         <- numeric(months)
  real_estate_equity   <- numeric(months)
  
  # Tracking variables
  curr_principal   <- principal
  investment       <- 0.0
  property_val     <- initial_property_value
  cum_investment   <- 0.0
  running_interest <- 0.0
  
  # Pre-calc monthly mortgage payment (annuity)
  if (mortgage_style == 'annuity') {
    monthly_mortgage_payment <- annuity_monthly_payment(principal, annual_mort_rate, years)
  }
  
  for (m in seq_len(months)) {
    
    # 1) Property grows
    property_val <- property_val * (1 + r_p)
    
    # 2) Interest on current principal
    monthly_interest <- curr_principal * r_m
    
    # 3) Mortgage Payment & Principal Pay
    if (mortgage_style == 'annuity') {
      mort_payment <- monthly_mortgage_payment
      principal_pay <- mort_payment - monthly_interest
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
    
    # 5) Investment
    invest_this <- total_monthly_budget - mort_payment
    if (invest_this < 0) invest_this <- 0
    
    # 6) Grow investment
    investment     <- investment * (1 + r_i) + invest_this
    cum_investment <- cum_investment + invest_this
    
    # 7) Cumulative interest
    running_interest <- running_interest + monthly_interest
    
    # 8) Store values
    mortgage_payment[m]       <- mort_payment
    total_interest_paid[m]    <- running_interest
    monthly_principal_paid[m] <- principal_pay
    invested_this_month[m]    <- invest_this
    
    property_value[m]         <- property_val
    inv_balance[m]            <- investment
    
    inv_contrib[m]            <- cum_investment
    inv_gains[m]              <- investment - cum_investment
    property_app[m]           <- property_val - initial_property_value
    real_estate_equity[m]     <- property_val - curr_principal
  }
  
  df <- data.frame(
    month                 = month,
    mortgage_payment      = mortgage_payment,
    total_interest_paid   = total_interest_paid,
    monthly_principal_paid= monthly_principal_paid,
    invested_this_month   = invested_this_month,
    property_value        = property_value,
    inv_balance           = inv_balance,
    inv_contrib           = inv_contrib,
    inv_gains             = inv_gains,
    property_app          = property_app,
    real_estate_equity    = real_estate_equity
  )
  
  # total_equity = real_estate_equity + inv_balance
  df$total_equity <- df$real_estate_equity + df$inv_balance
  df$total_principal_paid <- cumsum(df$monthly_principal_paid)
  
  return(df)
}

simulate_renting_and_investing <- function(
    monthly_income,
    monthly_expenses,
    initial_rent = 800,
    annual_rent_increase = 0.02,
    annual_invest_return = 0.05,
    years = 30
) {
  months <- years * 12
  r_i <- annual_invest_return / 12
  
  month_vec <- 1:months
  inv_balance  <- numeric(months)
  inv_contrib  <- numeric(months)
  inv_gains    <- numeric(months)
  monthly_rent <- numeric(months)
  monthly_invested <- numeric(months)
  
  balance <- 0
  cum_invested <- 0
  
  # We approximate that each month, 
  # rent grows based on annual_rent_increase
  for (m in month_vec) {
    yearIndex   <- (m - 1) %/% 12
    rentNow     <- initial_rent * (1 + annual_rent_increase)^yearIndex
    
    # monthly leftover = income - expenses
    leftover_for_invest = monthly_income - monthly_expenses - rentNow
    if (leftover_for_invest < 0) leftover_for_invest <- 0
    
    # Grow existing investment
    balance <- balance * (1 + r_i)
    
    # Add this month's new contribution
    balance <- balance + leftover_for_invest
    cum_invested <- cum_invested + leftover_for_invest
    
    inv_balance[m]  <- balance
    inv_contrib[m]  <- cum_invested
    inv_gains[m]    <- balance - cum_invested
    monthly_rent[m] <- rentNow
    monthly_invested[m] <- leftover_for_invest
  }
  
  df <- data.frame(
    month              = month_vec,
    inv_balance        = inv_balance,
    inv_contrib        = inv_contrib,
    inv_gains          = inv_gains,
    monthly_rent       = monthly_rent,
    monthly_invested   = monthly_invested,
    property_value     = NA,
    property_app       = NA,
    total_interest_paid= NA,
    monthly_principal_paid= NA,
    total_principal_paid= NA,
    real_estate_equity = NA
  )
  
  df$real_estate_equity[is.na(df$real_estate_equity)] <- 0
  df$total_equity <- df$real_estate_equity + df$inv_balance
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
      
      # Updated default values per request
      numericInput("monthly_income", "Monthly Income (€):", 
                   value = 2800, min = 0, step = 100),
      helpText("Enter your total monthly income."),
      
      numericInput("monthly_expenses", "Monthly Expenses (€):", 
                   value = 670, min = 0, step = 50),
      helpText("Enter your total monthly expenses (excluding mortgage/invest)."),
      
      hr(),
      
      numericInput("annual_mort_rate", "Annual Mortgage Rate (%):", 
                   value = 2, min = 0, step = 0.1),
      helpText("Annual interest rate for mortgage."),
      
      numericInput("annual_invest_return", "Annual Investment Return (%):", 
                   value = 8, min = 0, step = 0.1),
      helpText("Expected annual return rate for investments."),
      
      numericInput("annual_property_growth", "Annual Property Growth (%):", 
                   value = 7, min = 0, step = 0.1),
      helpText("Expected annual growth rate for property value."),
      
      helpText("Mortgage term is fixed at 30 years."),
      
      hr(),
      
      # Display read-only mortgage payment & investment for Option A
      h4("Option A (Property 1) Calculated Allocations"),
      textOutput("optionA_mortgagePayment"),
      textOutput("optionA_investment"),
      
      hr(),
      # Display read-only mortgage payment & investment for Option B
      h4("Option B (Property 2) Calculated Allocations"),
      textOutput("optionB_mortgagePayment"),
      textOutput("optionB_investment"),
      
      hr(),
      # Display read-only rent and investment for Option C
      h4("Option C (Rent & Invest) Calculated Allocations"),
      textOutput("optionC_rentPayment"),
      textOutput("optionC_investment"),
      
      hr(),
      # Interval for the Equity Table only
      sliderInput("interval_years", "Analysis Interval (Years):", 
                  min = 1, max = 30, value = 5, step = 1),
      
      # Slider for x-axis range in plots
      sliderInput("maxPlotYears", "Show up to Year:", 
                  min = 1, max = 30, value = 10, step = 1),
      
      hr(),
      
      # Option A
      h4("Option A: Property 1"),
      numericInput("full_priceA", "Full Price (€):", value = 200000, min = 0, step = 1000),
      numericInput("down_paymentA", "Down Payment (€):", value = 20000, min = 0, step = 1000),
      selectInput("mortgage_styleA", "Mortgage Style:",
                  choices = c("Annuity" = "annuity", "Equal Principal" = "equal_principal"), 
                  selected = "annuity"),
      
      hr(),
      
      # Option B
      h4("Option B: Property 2"),
      numericInput("full_priceB", "Full Price (€):", value = 150000, min = 0, step = 1000),
      numericInput("down_paymentB", "Down Payment (€):", value = 15000, min = 0, step = 1000),
      selectInput("mortgage_styleB", "Mortgage Style:",
                  choices = c("Annuity" = "annuity", "Equal Principal" = "equal_principal"), 
                  selected = "annuity"),
      
      hr(),
      
      # Option C
      h4("Option C: Rent & Invest"),
      numericInput("start_investC", "Initial Investment (€):", 
                   value = 20000, min = 0, step = 1000),
      
      # Removed 'monthly_investC'
      numericInput("initial_rentC", "Initial Monthly Rent (€):", 
                   value = 550, min = 0, step = 50),
      numericInput("annual_rent_increaseC", "Annual Rent Increase (%):", 
                   value = 0, min = 0, step = 0.1),
      
      hr(),
      helpText("Use 'Show up to Year' to limit table/plot range.")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Equity Table",
                 br(),
                 HTML("<b>Columns Explanation:</b><br>
                      <ul>
                        <li><b>Year:</b> The specific year in the simulation.</li>
                        <li><b>PropertyValue(A/B/C):</b> Estimated property value (NA for renting).</li>
                        <li><b>PropertyApp(A/B/C):</b> How much the property has appreciated relative to its initial value.</li>
                        <li><b>TotalPrincipalPaid(A/B):</b> Total principal paid so far (mortgage). NA for rent.</li>
                        <li><b>TotalInterestPaid(A/B):</b> Total interest paid so far (mortgage). NA for rent.</li>
                        <li><b>InvContrib(A/B/C):</b> Cumulative money contributed into investments.</li>
                        <li><b>InvGains(A/B/C):</b> Net growth in the investment portion (InvBalance - InvContrib).</li>
                        <li><b>InvBalance(A/B/C):</b> Current total investment balance.</li>
                        <li><b>TotalEquity(A/B/C):</b> (RealEstateEquity + InvBalance) or just investment balance for renting.</li>
                      </ul>"),
                 tableOutput("equityTable")
        ),
        
        tabPanel("Total Equity Over Time",
                 br(),
                 plotlyOutput("plotInvestmentBalances", height = "600px")
        ),
        
        tabPanel("Principal & Interest Paid",
                 br(),
                 plotlyOutput("plotPrincipalOwed", height = "600px")
        ),
        
        tabPanel("Property Value",
                 br(),
                 plotlyOutput("plotPropertyValue", height = "600px")
        ),
        
        tabPanel("Detailed Gains & Contributions",
                 br(),
                 HTML("<b>Details & Formulas:</b><br>
                      <ul>
                        <li><b>Cumulative Invested (InvContrib):</b> 
                          Total amount contributed to stocks/investments, excluding mortgage. 
                          <br><i>Formula</i>: 
                          <code>InvContrib[m] = sum(invested_this_month[1..m])</code>
                        </li>
                        <li><b>Investment Gains (InvGains):</b> 
                          Difference between current investment balance and total contributions. 
                          <br><i>Formula</i>: 
                          <code>InvGains[m] = InvBalance[m] - InvContrib[m]</code>
                        </li>
                        <li><b>Property Appreciation:</b> 
                          Increase in property value over its initial purchase price. 
                          <br><i>Formula</i>: 
                          <code>PropertyApp[m] = PropertyValue[m] - InitialPropertyValue</code>
                        </li>
                        <li><b>Real Estate Equity:</b> 
                          Amount of the property you own (PropertyValue - PrincipalOwed). Not used for renting. 
                          <br><i>Formula</i>:
                          <code>RealEstateEquity[m] = PropertyValue[m] - PrincipalOwed[m]</code>
                        </li>
                      </ul>"),
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
  
  totalTermYears <- 30
  
  # Pool: (income - expenses)
  common_pool <- reactive({
    pool <- input$monthly_income - input$monthly_expenses
    if (pool < 0) {
      showNotification("Monthly expenses exceed monthly income. Please adjust inputs.", type = "error")
      pool <- 0
    }
    pool
  })
  
  # --------------------------
  # Option A: Mortgage Payment + Investment
  # --------------------------
  monthlyAllocA <- reactive({
    principalA <- input$full_priceA - input$down_paymentA
    validate(
      need(principalA >= 0, "Down Payment for Option A cannot exceed Full Price.")
    )
    
    # Calculate required mortgage payment
    if (input$mortgage_styleA == "annuity") {
      required_mort_payment <- annuity_monthly_payment(
        principalA, input$annual_mort_rate / 100, totalTermYears
      )
    } else {
      required_mort_payment <- (principalA / (totalTermYears * 12)) +
        (principalA * (input$annual_mort_rate / 100) / 12)
    }
    
    invests <- common_pool() - required_mort_payment
    if (invests < 0) {
      showNotification("Option A mortgage payment exceeds your available pool. Setting investment to 0.", type="warning")
      invests <- 0
    }
    
    list(mortgage = required_mort_payment, invest = invests)
  })
  
  output$optionA_mortgagePayment <- renderText({
    req(monthlyAllocA())
    paste0("Monthly Mortgage Payment: €", 
           formatC(monthlyAllocA()$mortgage, format="f", digits=2, big.mark=" "))
  })
  
  output$optionA_investment <- renderText({
    req(monthlyAllocA())
    paste0("Monthly Investment: €", 
           formatC(monthlyAllocA()$invest, format="f", digits=2, big.mark=" "))
  })
  
  dataA <- reactive({
    total_budgetA <- monthlyAllocA()$mortgage + monthlyAllocA()$invest
    simulate_mortgage_and_investing(
      principal             = input$full_priceA - input$down_paymentA,
      annual_mort_rate      = input$annual_mort_rate / 100,
      total_monthly_budget  = total_budgetA,
      years                 = totalTermYears,
      annual_invest_return  = input$annual_invest_return / 100,
      annual_property_growth= input$annual_property_growth / 100,
      mortgage_style        = input$mortgage_styleA,
      initial_property_value= input$full_priceA
    )
  })
  
  # --------------------------
  # Option B: Mortgage Payment + Investment
  # --------------------------
  monthlyAllocB <- reactive({
    principalB <- input$full_priceB - input$down_paymentB
    validate(
      need(principalB >= 0, "Down Payment for Option B cannot exceed Full Price.")
    )
    
    if (input$mortgage_styleB == "annuity") {
      required_mort_payment <- annuity_monthly_payment(
        principalB, input$annual_mort_rate / 100, totalTermYears
      )
    } else {
      required_mort_payment <- (principalB / (totalTermYears * 12)) +
        (principalB * (input$annual_mort_rate / 100) / 12)
    }
    
    invests <- common_pool() - required_mort_payment
    if (invests < 0) {
      showNotification("Option B mortgage payment exceeds your available pool. Setting investment to 0.", type="warning")
      invests <- 0
    }
    
    list(mortgage = required_mort_payment, invest = invests)
  })
  
  output$optionB_mortgagePayment <- renderText({
    req(monthlyAllocB())
    paste0("Monthly Mortgage Payment: €", 
           formatC(monthlyAllocB()$mortgage, format="f", digits=2, big.mark=" "))
  })
  
  output$optionB_investment <- renderText({
    req(monthlyAllocB())
    paste0("Monthly Investment: €", 
           formatC(monthlyAllocB()$invest, format="f", digits=2, big.mark=" "))
  })
  
  dataB <- reactive({
    total_budgetB <- monthlyAllocB()$mortgage + monthlyAllocB()$invest
    simulate_mortgage_and_investing(
      principal             = input$full_priceB - input$down_paymentB,
      annual_mort_rate      = input$annual_mort_rate / 100,
      total_monthly_budget  = total_budgetB,
      years                 = totalTermYears,
      annual_invest_return  = input$annual_invest_return / 100,
      annual_property_growth= input$annual_property_growth / 100,
      mortgage_style        = input$mortgage_styleB,
      initial_property_value= input$full_priceB
    )
  })
  
  # --------------------------
  # Option C: Rent & Invest
  # --------------------------
  # We'll display the first-month rent & investment leftover
  monthlyAllocC <- reactive({
    # first month rent = initial_rentC
    rent_first_month <- input$initial_rentC
    invests <- common_pool() - rent_first_month
    if (invests < 0) {
      showNotification("Option C rent exceeds your available pool. Setting investment to 0.", type="warning")
      invests <- 0
    }
    list(rent = rent_first_month, invest = invests)
  })
  
  output$optionC_rentPayment <- renderText({
    req(monthlyAllocC())
    paste0("Monthly Rent Payment (First Month): €", 
           formatC(monthlyAllocC()$rent, format="f", digits=2, big.mark=" "))
  })
  
  output$optionC_investment <- renderText({
    req(monthlyAllocC())
    paste0("Monthly Investment (First Month): €", 
           formatC(monthlyAllocC()$invest, format="f", digits=2, big.mark=" "))
  })
  
  dataC <- reactive({
    # We'll pass monthly_income & monthly_expenses
    simulate_renting_and_investing(
      monthly_income       = input$monthly_income,
      monthly_expenses     = input$monthly_expenses,
      initial_rent         = input$initial_rentC,
      annual_rent_increase = input$annual_rent_increaseC / 100,
      annual_invest_return = input$annual_invest_return / 100,
      years               = totalTermYears
    )
  })
  
  # --------------------------------------------
  # Equity Table
  # --------------------------------------------
  equity_at_year <- function(data_dict, year) {
    m <- year * 12
    if (m > nrow(data_dict)) m <- nrow(data_dict)
    list(
      PropertyValue       = data_dict$property_value[m],
      PropertyApp         = data_dict$property_app[m],
      TotalInterestPaid   = data_dict$total_interest_paid[m],
      TotalPrincipalPaid  = data_dict$total_principal_paid[m],
      InvContrib          = data_dict$inv_contrib[m],
      InvGains            = data_dict$inv_gains[m],
      InvBalance          = data_dict$inv_balance[m],
      TotalEquity         = data_dict$total_equity[m]
    )
  }
  
  equityTable <- reactive({
    endYear  <- min(totalTermYears, input$maxPlotYears)
    interval <- input$interval_years
    yrs_vector <- seq(interval, endYear, by = interval)
    if (tail(yrs_vector,1) != endYear) {
      yrs_vector <- c(yrs_vector, endYear)
    }
    yrs_vector <- unique(yrs_vector[yrs_vector <= endYear])
    if (length(yrs_vector) == 0) {
      return(NULL)
    }
    
    eqA <- lapply(yrs_vector, function(y) equity_at_year(dataA(), y))
    eqB <- lapply(yrs_vector, function(y) equity_at_year(dataB(), y))
    eqC <- lapply(yrs_vector, function(y) equity_at_year(dataC(), y))
    
    get_val <- function(lst, field) {
      val <- lst[[field]]
      if (is.na(val)) val <- 0
      return(val)
    }
    
    dfOut <- data.frame(Year = yrs_vector)
    
    # Fill A columns
    for (i in seq_along(yrs_vector)) {
      dfOut$PropertyValueA[i]      <- get_val(eqA[[i]], "PropertyValue")
      dfOut$PropertyAppA[i]        <- get_val(eqA[[i]], "PropertyApp")
      dfOut$TotalInterestPaidA[i]  <- get_val(eqA[[i]], "TotalInterestPaid")
      dfOut$TotalPrincipalPaidA[i] <- get_val(eqA[[i]], "TotalPrincipalPaid")
      dfOut$InvContribA[i]         <- get_val(eqA[[i]], "InvContrib")
      dfOut$InvGainsA[i]           <- get_val(eqA[[i]], "InvGains")
      dfOut$InvBalanceA[i]         <- get_val(eqA[[i]], "InvBalance")
      dfOut$TotalEquityA[i]        <- get_val(eqA[[i]], "TotalEquity")
    }
    
    # Fill B columns
    for (i in seq_along(yrs_vector)) {
      dfOut$PropertyValueB[i]      <- get_val(eqB[[i]], "PropertyValue")
      dfOut$PropertyAppB[i]        <- get_val(eqB[[i]], "PropertyApp")
      dfOut$TotalInterestPaidB[i]  <- get_val(eqB[[i]], "TotalInterestPaid")
      dfOut$TotalPrincipalPaidB[i] <- get_val(eqB[[i]], "TotalPrincipalPaid")
      dfOut$InvContribB[i]         <- get_val(eqB[[i]], "InvContrib")
      dfOut$InvGainsB[i]           <- get_val(eqB[[i]], "InvGains")
      dfOut$InvBalanceB[i]         <- get_val(eqB[[i]], "InvBalance")
      dfOut$TotalEquityB[i]        <- get_val(eqB[[i]], "TotalEquity")
    }
    
    # Fill C columns
    for (i in seq_along(yrs_vector)) {
      dfOut$PropertyValueC[i]      <- get_val(eqC[[i]], "PropertyValue")
      dfOut$PropertyAppC[i]        <- get_val(eqC[[i]], "PropertyApp")
      dfOut$TotalInterestPaidC[i]  <- get_val(eqC[[i]], "TotalInterestPaid")
      dfOut$TotalPrincipalPaidC[i] <- get_val(eqC[[i]], "TotalPrincipalPaid")
      dfOut$InvContribC[i]         <- get_val(eqC[[i]], "InvContrib")
      dfOut$InvGainsC[i]           <- get_val(eqC[[i]], "InvGains")
      dfOut$InvBalanceC[i]         <- get_val(eqC[[i]], "InvBalance")
      dfOut$TotalEquityC[i]        <- get_val(eqC[[i]], "TotalEquity")
    }
    
    numeric_cols <- setdiff(colnames(dfOut), "Year")
    for (col in numeric_cols) {
      dfOut[[col]] <- formatC(dfOut[[col]], format="f", big.mark=" ", digits=0)
    }
    
    dfOut
  })
  
  output$equityTable <- renderTable({
    equityTable()
  },
  striped = TRUE,
  hover = TRUE,
  align = "c",
  sanitize.text.function = function(x) x,
  include.rownames = FALSE
  )
  
  # --------------------------------------------
  # PLOTS
  # --------------------------------------------
  reactivePlotMonths <- reactive({
    input$maxPlotYears * 12
  })
  
  # 1) Total Equity Over Time
  output$plotInvestmentBalances <- renderPlotly({
    dfA <- dataA()
    dfB <- dataB()
    dfC <- dataC()
    
    dA <- data.frame(month = dfA$month, value = dfA$total_equity, option="Option A")
    dB <- data.frame(month = dfB$month, value = dfB$total_equity, option="Option B")
    dC <- data.frame(month = dfC$month, value = dfC$total_equity, option="Option C")
    
    df <- rbind(dA, dB, dC)
    
    p <- ggplot(df, aes(x = month, y = value, color = option)) +
      geom_line(size=1.2) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title="Total Equity Over Time", x="Month", y="€", color="Option") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(x=1, y=1))
  })
  
  # 2) Principal & Interest Paid
  output$plotPrincipalOwed <- renderPlotly({
    dfA <- dataA()
    dfB <- dataB()
    
    dA <- data.frame(
      month = dfA$month,
      totalInterestPaid = dfA$total_interest_paid,
      totalPrincipalPaid= dfA$total_principal_paid,
      option = "Option A"
    )
    dB <- data.frame(
      month = dfB$month,
      totalInterestPaid = dfB$total_interest_paid,
      totalPrincipalPaid= dfB$total_principal_paid,
      option = "Option B"
    )
    dAll <- rbind(dA, dB)
    
    dLong <- reshape2::melt(dAll, id.vars=c("month","option"), 
                            variable.name="Metric", value.name="Value")
    
    p <- ggplot(dLong, aes(x=month, y=Value, color=option)) +
      geom_line(size=1.2) +
      facet_wrap(~Metric, scales="free_y",
                 labeller = as_labeller(c(
                   "totalInterestPaid" = "Total Interest Paid",
                   "totalPrincipalPaid"= "Total Principal Paid"
                 ))) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(labels = function(x) paste0("€", comma(x))) +
      labs(title="Cumulative Principal & Interest Paid", x="Month", y="€", color="Option") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(x=1, y=1))
  })
  
  # 3) Property Value
  output$plotPropertyValue <- renderPlotly({
    dfA <- dataA(); dfB <- dataB()
    dA <- data.frame(month=dfA$month, value=dfA$property_value, option="Option A")
    dB <- data.frame(month=dfB$month, value=dfB$property_value, option="Option B")
    
    df <- rbind(dA, dB)
    
    p <- ggplot(df, aes(x=month, y=value, color=option)) +
      geom_line(size=1.2) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(labels=function(x) paste0("€", comma(x))) +
      labs(title="Property Value Growth", x="Month", y="€", color="Option") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend = list(x=1, y=1))
  })
  
  # 4) Detailed Gains & Contributions
  output$plotAllDetailedGains <- renderPlotly({
    dfA <- dataA(); dfA$Option <- "Option A"
    dfB <- dataB(); dfB$Option <- "Option B"
    dfC <- dataC(); dfC$Option <- "Option C"
    
    dfAll <- rbind(
      dfA[, c("month","Option","inv_contrib","inv_gains","property_app","real_estate_equity")],
      dfB[, c("month","Option","inv_contrib","inv_gains","property_app","real_estate_equity")],
      dfC[, c("month","Option","inv_contrib","inv_gains","property_app","real_estate_equity")]
    )
    
    dLong <- melt(dfAll, id.vars=c("month","Option"))
    
    p <- ggplot(dLong, aes(x=month, y=value, color=Option)) +
      geom_line(size=1.2, na.rm=TRUE) +
      facet_wrap(~variable, scales="free_y",
                 labeller = as_labeller(c(
                   "inv_contrib"       = "Cumulative Invested (InvContrib)",
                   "inv_gains"         = "Investment Gains (InvGains)",
                   "property_app"      = "Property Appreciation",
                   "real_estate_equity"= "Real Estate Equity"
                 ))) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(labels=function(x) paste0("€", comma(x))) +
      labs(title="Detailed Gains & Contributions (All Options)",
           x="Month", y="€", color="Option") +
      theme_minimal()
    
    ggplotly(p) %>% layout(legend=list(x=1,y=1))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
