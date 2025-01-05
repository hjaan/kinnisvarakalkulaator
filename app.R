library(shiny)
library(ggplot2)
library(reshape2)
library(scales)
library(plotly)

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
    initial_property_value,
    initial_investment = 0  # <-- ADDED ARGUMENT
) {
  # Number of months
  months <- years * 12
  
  # Monthly rates
  r_m <- monthly_interest_rate(annual_mort_rate)      # mortgage
  r_i <- monthly_interest_rate(annual_invest_return)  # investment
  r_p <- monthly_interest_rate(annual_property_growth)# property
  
  # Initialize vectors
  month                   <- 1:months
  mortgage_payment        <- numeric(months)
  total_interest_paid     <- numeric(months)  # cumulative interest
  monthly_principal_paid  <- numeric(months)
  
  invested_this_month     <- numeric(months)
  property_value          <- numeric(months)
  inv_balance             <- numeric(months)
  
  inv_contrib             <- numeric(months)  # cumulative investment
  inv_gains               <- numeric(months)
  property_app            <- numeric(months)
  real_estate_equity      <- numeric(months)
  
  # Tracking variables
  curr_principal   <- principal
  investment       <- initial_investment  # <-- START WITH THE PROVIDED INITIAL INVESTMENT
  property_val     <- initial_property_value
  cum_investment   <- initial_investment
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
    
    # 5) Investment (whatever is left in total_monthly_budget after mortgage)
    invest_this <- total_monthly_budget - mort_payment
    if (invest_this < 0) invest_this <- 0
    
    # 6) Grow investment
    investment     <- investment * (1 + r_i) + invest_this
    cum_investment <- cum_investment + invest_this
    
    # 7) Cumulative interest
    running_interest <- running_interest + monthly_interest
    
    # 8) Store values
    mortgage_payment[m]        <- mort_payment
    total_interest_paid[m]     <- running_interest
    monthly_principal_paid[m]  <- principal_pay
    invested_this_month[m]     <- invest_this
    
    property_value[m]          <- property_val
    inv_balance[m]             <- investment
    
    inv_contrib[m]             <- cum_investment
    inv_gains[m]               <- investment - inv_contrib[m]
    property_app[m]            <- property_val - initial_property_value
    real_estate_equity[m]      <- property_val - curr_principal
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
  df$total_equity         <- df$real_estate_equity + df$inv_balance
  df$total_principal_paid <- cumsum(df$monthly_principal_paid)
  
  return(df)
}

simulate_renting_and_investing <- function(
    monthly_income,
    monthly_expenses,
    initial_rent = 800,
    annual_rent_increase = 0.02,
    annual_invest_return = 0.05,
    years = 30,
    start_invest = 0
) {
  months <- years * 12
  r_i <- annual_invest_return / 12
  
  month_vec         <- 1:months
  inv_balance       <- numeric(months)
  inv_contrib       <- numeric(months)
  inv_gains         <- numeric(months)
  monthly_rent      <- numeric(months)
  monthly_invested  <- numeric(months)
  
  balance       <- start_invest
  cum_invested  <- start_invest
  
  for (m in month_vec) {
    # Which 'year' are we in?
    yearIndex <- (m - 1) %/% 12
    rentNow   <- initial_rent * (1 + annual_rent_increase)^yearIndex
    
    # leftover = income - expenses - rent
    leftover_for_invest = monthly_income - monthly_expenses - rentNow
    if (leftover_for_invest < 0) leftover_for_invest <- 0
    
    # Grow existing investment
    balance <- balance * (1 + r_i)
    
    # Add this month's new contribution
    balance <- balance + leftover_for_invest
    cum_invested <- cum_invested + leftover_for_invest
    
    inv_balance[m]      <- balance
    inv_contrib[m]      <- cum_invested
    inv_gains[m]        <- balance - cum_invested
    monthly_rent[m]     <- rentNow
    monthly_invested[m] <- leftover_for_invest
  }
  
  df <- data.frame(
    month                = month_vec,
    inv_balance          = inv_balance,
    inv_contrib          = inv_contrib,
    inv_gains            = inv_gains,
    monthly_rent         = monthly_rent,
    monthly_invested     = monthly_invested,
    property_value       = NA_real_,
    property_app         = NA_real_,
    total_interest_paid  = NA_real_,
    monthly_principal_paid= NA_real_,
    total_principal_paid = NA_real_,
    real_estate_equity   = NA_real_
  )
  
  # For renting, real estate equity is 0, so total_equity is just the investment
  df$real_estate_equity[is.na(df$real_estate_equity)] <- 0
  df$total_equity <- df$inv_balance
  
  return(df)
}

# --------------------------------------------
# UI
# --------------------------------------------
ui <- fluidPage(
  titlePanel("Kodulaen & investeeringud"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("toggle_lang", "EST", class = "btn-primary"),
      br(),
      br(),
      
      # ---------------------------
      # GENERAL PARAMETERS
      # ---------------------------
      uiOutput("gen_params_title"),
      uiOutput("monthly_income_ui"),
      uiOutput("monthly_income_help_ui"),
      
      uiOutput("monthly_expenses_ui"),
      uiOutput("monthly_expenses_help_ui"),
      
      uiOutput("annual_mort_rate_ui"),
      uiOutput("annual_mort_rate_help_ui"),
      
      uiOutput("annual_invest_return_ui"),
      uiOutput("annual_invest_return_help_ui"),
      
      uiOutput("annual_property_growth_ui"),
      uiOutput("annual_property_growth_help_ui"),
      
      helpText(textOutput("mortgage_term_help")),
      
      uiOutput("interval_years_ui"),
      
      uiOutput("maxPlotYears_ui"),
      
      # ---------------------------
      # CALCULATED OUTPUTS (A, B, C)
      # ---------------------------
      hr(),
      uiOutput("calc_allocations_title"),
      
      hr(),
      uiOutput("optionA_title"),
      textOutput("optionA_mortgagePayment"),
      textOutput("optionA_investment"),
      
      hr(),
      uiOutput("optionB_title"),
      textOutput("optionB_mortgagePayment"),
      textOutput("optionB_investment"),
      
      hr(),
      uiOutput("optionC_title"),
      textOutput("optionC_rentPayment"),
      textOutput("optionC_investment"),
      
      # ---------------------------
      # PROPERTY / RENTING OPTIONS (A, B, C)
      # ---------------------------
      hr(),
      
      # 1) Option A Inputs
      uiOutput("propertyA_title"),
      uiOutput("labelA_ui"),
      uiOutput("full_priceA_ui"),
      uiOutput("down_paymentA_ui"),
      uiOutput("mortgage_styleA_ui"),
      
      # NEW: Initial Investment for A
      uiOutput("initial_investA_ui"),  # <-- ADDED
      
      uiOutput("investA_ui"),
      
      hr(),
      
      # 2) Option B Inputs
      uiOutput("propertyB_title"),
      uiOutput("labelB_ui"),
      uiOutput("full_priceB_ui"),
      uiOutput("down_paymentB_ui"),
      uiOutput("mortgage_styleB_ui"),
      
      # NEW: Initial Investment for B
      uiOutput("initial_investB_ui"),  # <-- ADDED
      
      uiOutput("investB_ui"),
      
      hr(),
      
      # 3) Option C Inputs
      uiOutput("propertyC_title"),
      uiOutput("labelC_ui"),
      uiOutput("start_investC_ui"),
      uiOutput("initial_rentC_ui"),
      uiOutput("annual_rent_increaseC_ui"),
      
      uiOutput("investC_ui")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(textOutput("tab_equity_table"),
                 br(),
                 htmlOutput("equity_table_help"),
                 br(),
                 tableOutput("equityTable")  # Added tableOutput for Equity Table
        ),
        
        tabPanel(textOutput("tab_total_equity"),
                 br(),
                 plotlyOutput("plotInvestmentBalances", height = "600px")
        ),
        
        tabPanel(textOutput("tab_principal_interest"),
                 br(),
                 plotlyOutput("plotPrincipalOwed", height = "600px")
        ),
        
        tabPanel(textOutput("tab_property_value"),
                 br(),
                 plotlyOutput("plotPropertyValue", height = "600px")
        ),
        
        tabPanel(textOutput("tab_detailed_gains"),
                 br(),
                 htmlOutput("detailed_gains_help"),
                 plotlyOutput("plotAllDetailedGains", height = "600px")
        ),
        
        # NEW TAB: Compare Mortgage Styles
        tabPanel(textOutput("tab_compare_mortgage"),
                 br(),
                 plotlyOutput("plotCompareMortgageStyles", height = "600px"),
                 br(),
                 plotlyOutput("plotCompareMortgageStylesPerc", height = "600px")  # Additional plot
        )
      )
    )
  )
)

# --------------------------------------------
# SERVER
# --------------------------------------------
server <- function(input, output, session) {
  
  # Reactive value to store the language
  lang <- reactiveVal("ENG")
  
  # Observe toggle button and switch language
  observeEvent(input$toggle_lang, {
    if (lang() == "ENG") {
      lang("EST")
      updateActionButton(session, "toggle_lang", label = "ENG")
    } else {
      lang("ENG")
      updateActionButton(session, "toggle_lang", label = "EST")
    }
  })
  
  # Translations
  translations <- list(
    ENG = list(
      app_title = "Mortgage and Investment Simulation",
      gen_params_title = "General Parameters",
      monthly_income_label = "Monthly Income (€):",
      monthly_income_help = "Enter your total monthly income.",
      
      monthly_expenses_label = "Monthly Expenses (€):",
      monthly_expenses_help = "Enter your total monthly expenses (excluding mortgage/rent/invest). This is a general estimation for food, fun, utilities.",
      
      annual_mort_rate_label = "Annual Mortgage Rate (%):",
      annual_mort_rate_help = "Over a long horizon, many models might assume ~3%-4% for planning purposes.",
      
      annual_invest_return_label = "Annual Investment Return (%):",
      annual_invest_return_help = "In a long-term model, you might assume an 8%-10% nominal return for broad U.S. equities. More conservative planners might model nearer to 7%.",
      
      annual_property_growth_label = "Annual Property Growth (%):",
      annual_property_growth_help = "In Estonia historically, estimates often hover around 4%-6% nominal annual growth over the long run.",
      
      mortgage_term_help = "Mortgage term is fixed at 30 years.",
      
      interval_years_label = "Analysis Interval (Years):",
      maxPlotYears_label = "Show up to Year:",
      
      calc_allocations_title = "Calculated Allocations",
      
      optionA_title = "Property 1 Calculated Allocations",
      optionA_mortgagePayment_label = "Monthly Mortgage Payment:",
      optionA_investment_label = "Monthly Investment:",
      
      optionB_title = "Property 2 Calculated Allocations",
      optionB_mortgagePayment_label = "Monthly Mortgage Payment:",
      optionB_investment_label = "Monthly Investment:",
      
      optionC_title = "Rent & Invest Calculated Allocations",
      optionC_rentPayment_label = "Monthly Rent Payment (First Month):",
      optionC_investment_label = "Monthly Investment (First Month):",
      
      propertyA_title = "Property 1",
      labelA_label = "Label A:",
      full_priceA_label = "Full Price (€):",
      down_paymentA_label = "Down Payment (€):",
      mortgage_styleA_label = "Mortgage Style:",
      investA_label = "Monthly Investment for A:",
      
      propertyB_title = "Property 2",
      labelB_label = "Label B:",
      full_priceB_label = "Full Price (€):",
      down_paymentB_label = "Down Payment (€):",
      mortgage_styleB_label = "Mortgage Style:",
      investB_label = "Monthly Investment for B:",
      
      # Added translation key for the new "Initial Investment" field
      initial_investAB_label = "Initial Investment (€):",
      
      propertyC_title = "Rent & Invest",
      labelC_label = "Label C:",
      start_investC_label = "Initial Investment (€):",
      initial_rentC_label = "Initial Monthly Rent (€):",
      annual_rent_increaseC_label = "Annual Rent Increase (%):",
      investC_label = "Monthly Investment for C:",
      
      tab_equity_table = "Equity Table",
      tab_total_equity = "Total Equity Over Time",
      tab_principal_interest = "Principal & Interest Paid",
      tab_property_value = "Property Value",
      tab_detailed_gains = "Detailed Gains & Contributions",
      tab_compare_mortgage = "Compare Mortgage Styles",
      
      equity_table_help = "<b>Columns Explanation:</b><br>
                           <ul>
                             <li><b>Year:</b> The specific year in the simulation.</li>
                             <li><b>PropertyValue:</b> Estimated property value.</li>
                             <li><b>PropertyApp:</b> How much the property has appreciated relative to its initial value.</li>
                             <li><b>TotalPrincipalPaid:</b> Total principal paid so far (mortgage).</li>
                             <li><b>%Owned:</b> The percentage of the property you own.</li>
                             <li><b>TotalInterestPaid:</b> Total interest paid so far (mortgage).</li>
                             <li><b>InvContrib:</b> Cumulative money contributed into investments.</li>
                             <li><b>InvGains:</b> Net growth in the investment portion (InvBalance - InvContrib).</li>
                             <li><b>InvBalance:</b> Current total investment balance.</li>
                             <li><b>TotalEquity:</b> RealEstateEquity + InvBalance.</li>
                           </ul>",
      
      detailed_gains_help = "<b>Details & Formulas:</b><br>
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
                             </ul>"
    ),
    EST = list(
      app_title = "Kodulaenu ja investeerimisportfoolio simulatsioon",
      gen_params_title = "Üldised parameetrid",
      monthly_income_label = "Igakuine sissetulek (€):",
      monthly_income_help = "Sisesta oma igakuine netosissetulek.",
      
      monthly_expenses_label = "Igakuised kulud (€):",
      monthly_expenses_help = "Sisesta oma kogu igakuised kulutused (välja arvatud laen/üür/investeeringud). See on üldine hinnang toidule, meelelahutusele, kommunaalkuludele.",
      
      annual_mort_rate_label = "Aastane kodulaenu intressimäär (%):",
      annual_mort_rate_help = "Pikas perspektiivis hindavad mudelid planeerimiseks umbes 3%-4%.",
      
      annual_invest_return_label = "Aastane investeeringute tootlus (%):",
      annual_invest_return_help = "Pikas perspektiivis võite eeldada SP500 puhul 8%-10% tootlust. Konservatiivsemad mudelit modelleerivad pigem 7%.",
      
      annual_property_growth_label = "Aastane kinnisvara kasv (%):",
      annual_property_growth_help = "Eestis on ajalooliselt (2005-2024) kasvanud kinnisvara umbes 4%-6% aastas.",
      
      mortgage_term_help = "Kodulaen on selles mudelis fikseeritud 30 aastaks.",
      
      interval_years_label = "Kuva tabelis intervall (aastates):",
      maxPlotYears_label = "Modelleerimise periood (aastates):",
      
      calc_allocations_title = "Sissetulekute, kulude ja laenude põhjal arvutatud jaotused",
      
      optionA_title = "Kinnisvara 1 arvutatud jaotused",
      optionA_mortgagePayment_label = "Kodulaenu igakuine makse:",
      optionA_investment_label = "Igakuiselt investeeritud summa:",
      
      optionB_title = "Kinnisvara 2 arvutatud jaotused",
      optionB_mortgagePayment_label = "Kodulaenu igakuine makse:",
      optionB_investment_label = "Igakuiselt investeeritud summa:",
      
      optionC_title = "Üür & investeerimine arvutatud jaotused",
      optionC_rentPayment_label = "Igakuine üür (esimene kuu):",
      optionC_investment_label = "Igakuiselt investeeritud summa:",
      
      propertyA_title = "Kinnisvara 1",
      labelA_label = "Nimetus A:",
      full_priceA_label = "Täishind (€):",
      down_paymentA_label = "Sissemakse (€):",
      mortgage_styleA_label = "Kodulaenu tüüp:",
      investA_label = "Igakuiselt investeeritud summa:",
      
      propertyB_title = "Kinnisvara 2",
      labelB_label = "Nimetus B:",
      full_priceB_label = "Täishind (€):",
      down_paymentB_label = "Sissemakse (€):",
      mortgage_styleB_label = "Kodulaenu tüüp:",
      investB_label = "Igakuiselt investeeritud summa:",
      
      # Added translation key for the new "Initial Investment" field in EST
      initial_investAB_label = "Alginvesteering (€):",
      
      propertyC_title = "Üür & investeerimine",
      labelC_label = "Nimetus C:",
      start_investC_label = "Alginvesteering (€):",
      initial_rentC_label = "Igakuine üür (€):",
      annual_rent_increaseC_label = "Aasane üüri suurenemine (%):",
      investC_label = "Igakuiselt investeeritud summa:",
      
      tab_equity_table = "Omakapitali tabel",
      tab_total_equity = "Kogu omakapital aja jooksul",
      tab_principal_interest = "Põhiosa & intress",
      tab_property_value = "Kinnisvara väärtus",
      tab_detailed_gains = "Detailsem tootlikus & panus",
      tab_compare_mortgage = "Laenutüüpide võrdlus",
      
      equity_table_help = "<b>Veergude selgitus:</b><br>
                           <ul>
                             <li><b>Year:</b> Simulatsiooni konkreetne aasta.</li>
                             <li><b>PropertyValue:</b> Hinnanguline kinnisvara väärtus.</li>
                             <li><b>PropertyApp:</b> Kinnisvara hinna kasv võrreldes esialgse hinnaga.</li>
                             <li><b>TotalPrincipalPaid:</b> Seni makstud põhiosa väärtus.</li>
                             <li><b>%Owned:</b> Protsent kinnisvarast, mida omad põhiosa tagasi makstes.</li>
                             <li><b>TotalInterestPaid:</b> Seni makstud intressi väärtus.</li>
                             <li><b>InvContrib:</b> Investeerimisportfooliosse panustatud raha.</li>
                             <li><b>InvGains:</b> investeerimisportfoolio netokasv (InvBalance - InvContrib).</li>
                             <li><b>InvBalance:</b> Praegune investeerimisportfoolio kogusumma.</li>
                             <li><b>TotalEquity:</b> RealEstateEquity + InvBalance.</li>
                           </ul>",
      
      detailed_gains_help = "<b>Detailsem tootlikus & panus:</b><br>
                             <ul>
                               <li><b>Investeerimisportfooliosse panustatud raha. (InvContrib):</b> 
                                 Kogu summa, mis on investeeritud aktsiatesse/mujale, kuhu pole sisse arvestatud kodulaenu. 
                                 <br><i>Valem</i>: 
                                 <code>InvContrib[m] = sum(invested_this_month[1..m])</code>
                               </li>
                               <li><b>investeerimisportfoolio netokasv (InvGains):</b> 
                                 Vahe käesoleval hetkel portfoolio väärtusest ning panustatud väärtuse vahel. 
                                 <br><i>Valem</i>: 
                                 <code>InvGains[m] = InvBalance[m] - InvContrib[m]</code>
                               </li>
                               <li><b>Kinnisvara Kasv:</b> 
                                 Kinnisvara väärtuse kasv võrreldes esialgse ostuhinnaga. 
                                 <br><i>Valem</i>: 
                                 <code>PropertyApp[m] = PropertyValue[m] - InitialPropertyValue</code>
                               </li>
                               <li><b>Reaalmajanduslik Omakapital:</b> 
                                 Omandatud kinnisvara osa (PropertyValue - PrincipalOwed). 
                                 <br><i>Valem</i>:
                                 <code>RealEstateEquity[m] = PropertyValue[m] - PrincipalOwed[m]</code>
                               </li>
                             </ul>"
    )
  )
  
  # Helper function to safely access translations
  t <- function(key) {
    req(translations[[lang()]][[key]])
    translations[[lang()]][[key]]
  }
  
  # Generate dynamic UI components based on language
  # Titles
  output$app_title <- renderText({
    t("app_title")
  })
  
  output$gen_params_title <- renderUI({
    h3(t("gen_params_title"))
  })
  
  # Monthly Income
  output$monthly_income_ui <- renderUI({
    numericInput("monthly_income", t("monthly_income_label"), 
                 value = 2800, min = 0, step = 100)
  })
  
  output$monthly_income_help_ui <- renderUI({
    helpText(t("monthly_income_help"))
  })
  
  # Monthly Expenses
  output$monthly_expenses_ui <- renderUI({
    numericInput("monthly_expenses", t("monthly_expenses_label"), 
                 value = 670, min = 0, step = 50)
  })
  
  output$monthly_expenses_help_ui <- renderUI({
    helpText(t("monthly_expenses_help"))
  })
  
  # Annual Mortgage Rate
  output$annual_mort_rate_ui <- renderUI({
    numericInput("annual_mort_rate", t("annual_mort_rate_label"), 
                 value = 3.5, min = 0, step = 0.1)
  })
  
  output$annual_mort_rate_help_ui <- renderUI({
    helpText(t("annual_mort_rate_help"))
  })
  
  # Annual Investment Return
  output$annual_invest_return_ui <- renderUI({
    numericInput("annual_invest_return", t("annual_invest_return_label"), 
                 value = 9, min = 0, step = 0.1)
  })
  
  output$annual_invest_return_help_ui <- renderUI({
    helpText(t("annual_invest_return_help"))
  })
  
  # Annual Property Growth
  output$annual_property_growth_ui <- renderUI({
    numericInput("annual_property_growth", t("annual_property_growth_label"), 
                 value = 5, min = 0, step = 0.1)
  })
  
  output$annual_property_growth_help_ui <- renderUI({
    helpText(t("annual_property_growth_help"))
  })
  
  # Mortgage Term Help
  output$mortgage_term_help <- renderText({
    t("mortgage_term_help")
  })
  
  # Analysis Interval
  output$interval_years_ui <- renderUI({
    sliderInput("interval_years", t("interval_years_label"), 
                min = 1, max = 30, value = 1, step = 1)
  })
  
  # Show up to Year
  output$maxPlotYears_ui <- renderUI({
    sliderInput("maxPlotYears", t("maxPlotYears_label"), 
                min = 1, max = 30, value = 10, step = 1)
  })
  
  # Calculated Allocations Title
  output$calc_allocations_title <- renderUI({
    h4(t("calc_allocations_title"))
  })
  
  # Option A Outputs
  output$optionA_title <- renderUI({
    h4(t("optionA_title"))
  })
  
  output$optionA_mortgagePayment <- renderText({
    req(monthlyAllocA())
    paste0(t("optionA_mortgagePayment_label"), " €",
           formatC(round(monthlyAllocA()$mortgage, digits = 2), format="f", digits = 2))
  })
  
  output$optionA_investment <- renderText({
    req(monthlyAllocA())
    paste0(t("optionA_investment_label"), " €",
           formatC(round(monthlyAllocA()$invest, digits = 2), format="f", digits = 0))
  })
  
  # Option B Outputs
  output$optionB_title <- renderUI({
    h4(t("optionB_title"))
  })
  
  output$optionB_mortgagePayment <- renderText({
    req(monthlyAllocB())
    paste0(t("optionB_mortgagePayment_label"), " €",
           formatC(round(monthlyAllocB()$mortgage, digits = 2), format="f", digits = 2))
  })
  
  output$optionB_investment <- renderText({
    req(monthlyAllocB())
    paste0(t("optionB_investment_label"), " €",
           formatC(round(monthlyAllocB()$invest, digits = 2), format="f", digits = 0))
  })
  
  # Option C Outputs
  output$optionC_title <- renderUI({
    h4(t("optionC_title"))
  })
  
  output$optionC_rentPayment <- renderText({
    req(monthlyAllocC())
    paste0(t("optionC_rentPayment_label"), " €",
           formatC(round(monthlyAllocC()$rent, digits = 2), format="f", digits = 0))
  })
  
  output$optionC_investment <- renderText({
    req(monthlyAllocC())
    paste0(t("optionC_investment_label"), " €",
           formatC(round(monthlyAllocC()$invest, digits = 2), format="f", digits = 0))
  })
  
  # ---------------------------
  # PROPERTY / RENTING OPTIONS (A, B, C)
  # ---------------------------
  
  # 1) Option A Inputs
  output$propertyA_title <- renderUI({
    h3(t("propertyA_title"))
  })
  
  output$labelA_ui <- renderUI({
    textInput("labelA", t("labelA_label"), value = "A")
  })
  
  output$full_priceA_ui <- renderUI({
    numericInput("full_priceA", t("full_priceA_label"), 
                 value = 200000, min = 0, step = 1000)
  })
  
  output$down_paymentA_ui <- renderUI({
    numericInput("down_paymentA", t("down_paymentA_label"), 
                 value = 20000, min = 0, step = 1000)
  })
  
  output$mortgage_styleA_ui <- renderUI({
    selectInput("mortgage_styleA", t("mortgage_styleA_label"),
                choices = c(
                  "Annuity" = "annuity", 
                  "Equal Principal" = "equal_principal"
                ), 
                selected = "annuity")
  })
  
  # ADD this new UI for A
  output$initial_investA_ui <- renderUI({
    numericInput(
      "initial_investA",
      t("initial_investAB_label"),  # "Initial Investment (€):" in ENG, or "Alginvesteering (€):" in EST
      value = 0,  # default 0
      min = 0,
      step = 1000
    )
  })
  
  output$investA_ui <- renderUI({
    sliderInput("investA", t("investA_label"), 
                min = 0, max = 5000, value = 2130, step = 50)
  })
  
  # 2) Option B Inputs
  output$propertyB_title <- renderUI({
    h3(t("propertyB_title"))
  })
  
  output$labelB_ui <- renderUI({
    textInput("labelB", t("labelB_label"), value = "B")
  })
  
  output$full_priceB_ui <- renderUI({
    numericInput("full_priceB", t("full_priceB_label"), 
                 value = 120000, min = 0, step = 1000)
  })
  
  output$down_paymentB_ui <- renderUI({
    numericInput("down_paymentB", t("down_paymentB_label"), 
                 value = 12000, min = 0, step = 1000)
  })
  
  output$mortgage_styleB_ui <- renderUI({
    selectInput("mortgage_styleB", t("mortgage_styleB_label"),
                choices = c(
                  "Annuity" = "annuity", 
                  "Equal Principal" = "equal_principal"
                ), 
                selected = "annuity")
  })
  
  # ADD this new UI for B
  output$initial_investB_ui <- renderUI({
    numericInput(
      "initial_investB",
      t("initial_investAB_label"), 
      value = 0, 
      min = 0,
      step = 1000
    )
  })
  
  output$investB_ui <- renderUI({
    sliderInput("investB", t("investB_label"), 
                min = 0, max = 5000, value = 2130, step = 50)
  })
  
  # 3) Option C Inputs
  output$propertyC_title <- renderUI({
    h3(t("propertyC_title"))
  })
  
  output$labelC_ui <- renderUI({
    textInput("labelC", t("labelC_label"), value = "Rent & Invest")
  })
  
  output$start_investC_ui <- renderUI({
    numericInput("start_investC", t("start_investC_label"), 
                 value = 20000, min = 0, step = 1000)
  })
  
  output$initial_rentC_ui <- renderUI({
    numericInput("initial_rentC", t("initial_rentC_label"), 
                 value = 550, min = 0, step = 50)
  })
  
  output$annual_rent_increaseC_ui <- renderUI({
    numericInput("annual_rent_increaseC", t("annual_rent_increaseC_label"), 
                 value = 0, min = 0, step = 0.1)
  })
  
  output$investC_ui <- renderUI({
    sliderInput("investC", t("investC_label"), 
                min = 0, max = 5000, value = 2130, step = 50)
  })
  
  # Translated Tab Titles
  output$tab_equity_table <- renderText({
    t("tab_equity_table")
  })
  
  output$tab_total_equity <- renderText({
    t("tab_total_equity")
  })
  
  output$tab_principal_interest <- renderText({
    t("tab_principal_interest")
  })
  
  output$tab_property_value <- renderText({
    t("tab_property_value")
  })
  
  output$tab_detailed_gains <- renderText({
    t("tab_detailed_gains")
  })
  
  output$tab_compare_mortgage <- renderText({
    t("tab_compare_mortgage")
  })
  
  # Translated Help Texts in Tabs
  output$equity_table_help <- renderUI({
    HTML(t("equity_table_help"))
  })
  
  output$detailed_gains_help <- renderUI({
    HTML(t("detailed_gains_help"))
  })
  
  # -------------------------------------------------------------------
  # Data Calculations
  # -------------------------------------------------------------------
  
  totalTermYears <- 30
  
  # Pool: (income - expenses) - this is the total leftover for mortgage+invest or rent+invest
  common_pool <- reactive({
    req(input$monthly_income, input$monthly_expenses)
    pool <- input$monthly_income - input$monthly_expenses
    if (pool < 0) {
      showNotification(t("monthly_expenses_help"), type = "error")
      pool <- 0
    }
    pool
  })
  
  # -------------------------------------------------------------------
  # Option A: Mortgage Payment + Investment
  # -------------------------------------------------------------------
  monthlyAllocA <- reactive({
    req(input$full_priceA, input$down_paymentA)
    validate(
      need(!is.na(input$annual_mort_rate) && input$annual_mort_rate >= 0,
           "Please insert a valid annual mortgage rate.")
    )
    principalA <- input$full_priceA - input$down_paymentA
    validate(
      need(principalA >= 0, paste(t("down_paymentA_label"), "cannot exceed Full Price."))
    )
    
    # Calculate required mortgage payment
    if (input$mortgage_styleA == "annuity") {
      required_mort_payment <- annuity_monthly_payment(
        principalA, input$annual_mort_rate / 100, totalTermYears
      )
    } else {
      # equal_principal approach
      required_mort_payment <- (principalA / (totalTermYears * 12)) +
        (principalA * (input$annual_mort_rate / 100) / 12)
    }
    
    leftoverA <- common_pool() - required_mort_payment
    
    if (leftoverA < 0) {
      # Mortgage payment alone exceeds your monthly pool => zero invest
      showNotification(
        paste(t("optionA_mortgagePayment_label"), "exceeds your available pool. Setting investment to 0."),
        type="warning"
      )
      investsA <- 0
    } else {
      # user-chosen invests from slider
      investsA <- input$investA
      # do not exceed leftover
      investsA <- min(investsA, leftoverA)
      if (investsA < 0) investsA <- 0
    }
    
    list(mortgage = required_mort_payment, invest = investsA, leftover = leftoverA)
  })
  
  # Observe leftoverA and update the slider max
  observe({
    allocA <- monthlyAllocA()
    leftoverA <- allocA$leftover
    maxInvestA <- floor(leftoverA)
    updateSliderInput(session, "investA",
                      max = max(maxInvestA, 0),
                      value = min(input$investA, max(maxInvestA,0)))
  })
  
  dataA <- reactive({
    req(monthlyAllocA())
    total_budgetA <- monthlyAllocA()$mortgage + monthlyAllocA()$invest
    simulate_mortgage_and_investing(
      principal             = input$full_priceA - input$down_paymentA,
      annual_mort_rate      = input$annual_mort_rate / 100,
      total_monthly_budget  = total_budgetA,
      years                 = totalTermYears,
      annual_invest_return  = input$annual_invest_return / 100,
      annual_property_growth= input$annual_property_growth / 100,
      mortgage_style        = input$mortgage_styleA,
      initial_property_value= input$full_priceA,
      initial_investment    = input$initial_investA   # <-- Pass to simulation
    )
  })
  
  # -------------------------------------------------------------------
  # Option B: Mortgage Payment + Investment
  # -------------------------------------------------------------------
  monthlyAllocB <- reactive({
    req(input$full_priceB, input$down_paymentB)
    validate(
      need(!is.na(input$annual_mort_rate) && input$annual_mort_rate >= 0,
           "Please insert a valid annual mortgage rate.")
    )
    principalB <- input$full_priceB - input$down_paymentB
    validate(
      need(principalB >= 0, paste(t("down_paymentB_label"), "cannot exceed Full Price."))
    )
    
    if (input$mortgage_styleB == "annuity") {
      required_mort_payment <- annuity_monthly_payment(
        principalB, input$annual_mort_rate / 100, totalTermYears
      )
    } else {
      required_mort_payment <- (principalB / (totalTermYears * 12)) +
        (principalB * (input$annual_mort_rate / 100) / 12)
    }
    
    leftoverB <- common_pool() - required_mort_payment
    
    if (leftoverB < 0) {
      showNotification(
        paste(t("optionB_mortgagePayment_label"), "exceeds your available pool. Setting investment to 0."),
        type="warning"
      )
      investsB <- 0
    } else {
      investsB <- input$investB
      investsB <- min(investsB, leftoverB)
      if (investsB < 0) investsB <- 0
    }
    
    list(mortgage = required_mort_payment, invest = investsB, leftover = leftoverB)
  })
  
  # Observe leftoverB and update the slider max
  observe({
    allocB <- monthlyAllocB()
    leftoverB <- allocB$leftover
    maxInvestB <- floor(leftoverB)
    updateSliderInput(session, "investB",
                      max = max(maxInvestB, 0),
                      value = min(input$investB, max(maxInvestB,0)))
  })
  
  dataB <- reactive({
    req(monthlyAllocB())
    total_budgetB <- monthlyAllocB()$mortgage + monthlyAllocB()$invest
    simulate_mortgage_and_investing(
      principal             = input$full_priceB - input$down_paymentB,
      annual_mort_rate      = input$annual_mort_rate / 100,
      total_monthly_budget  = total_budgetB,
      years                 = totalTermYears,
      annual_invest_return  = input$annual_invest_return / 100,
      annual_property_growth= input$annual_property_growth / 100,
      mortgage_style        = input$mortgage_styleB,
      initial_property_value= input$full_priceB,
      initial_investment    = input$initial_investB   # <-- Pass to simulation
    )
  })
  
  # -------------------------------------------------------------------
  # Option C: Rent + Invest
  # -------------------------------------------------------------------
  monthlyAllocC <- reactive({
    req(input$initial_rentC)
    rent_first_month <- input$initial_rentC
    leftoverC <- common_pool() - rent_first_month
    
    if (leftoverC < 0) {
      # Rent payment alone exceeds your monthly pool => zero invest
      showNotification(
        paste(t("optionC_rentPayment_label"), "exceeds your available pool. Setting investment to 0."),
        type="warning"
      )
      investsC <- 0
    } else {
      investsC <- input$investC
      investsC <- min(investsC, leftoverC)
      if (investsC < 0) investsC <- 0
    }
    
    list(rent = rent_first_month, invest = investsC, leftover = leftoverC)
  })
  
  # Observe leftoverC and update the slider max
  observe({
    allocC <- monthlyAllocC()
    leftoverC <- allocC$leftover
    maxInvestC <- floor(leftoverC)
    updateSliderInput(session, "investC",
                      max = max(maxInvestC, 0),
                      value = min(input$investC, max(maxInvestC,0)))
  })
  
  dataC <- reactive({
    simulate_renting_and_investing(
      monthly_income       = input$monthly_income,
      monthly_expenses     = input$monthly_expenses,
      initial_rent         = input$initial_rentC,
      annual_rent_increase = input$annual_rent_increaseC / 100,
      annual_invest_return = input$annual_invest_return / 100,
      years               = totalTermYears,
      start_invest        = input$start_investC
    )
  })
  
  # -------------------------------------------------------------------
  # Equity Table
  # -------------------------------------------------------------------
  equity_at_year <- function(data_dict, year) {
    req(data_dict)
    m <- year * 12
    if (m > nrow(data_dict)) m <- nrow(data_dict)
    list(
      PropertyValue       = data_dict$property_value[m],
      PropertyApp         = data_dict$property_app[m],
      TotalInterestPaid   = data_dict$total_interest_paid[m],
      TotalPrincipalPaid  = data_dict$total_principal_paid[m],
      RealEstateEquity    = data_dict$real_estate_equity[m],
      InvContrib          = data_dict$inv_contrib[m],
      InvGains            = data_dict$inv_gains[m],
      InvBalance          = data_dict$inv_balance[m],
      TotalEquity         = data_dict$total_equity[m]
    )
  }
  
  equityTable <- reactive({
    req(input$maxPlotYears, input$interval_years)
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
    
    # Fill columns for A
    for (i in seq_along(yrs_vector)) {
      dfOut[[paste0("PropertyValue_", input$labelA)]][i]      <- round(get_val(eqA[[i]], "PropertyValue"))
      dfOut[[paste0("PropertyApp_", input$labelA)]][i]        <- round(get_val(eqA[[i]], "PropertyApp"))
      dfOut[[paste0("TotalPrincipalPaid_", input$labelA)]][i] <- round(get_val(eqA[[i]], "TotalPrincipalPaid"))
      
      propValA <- get_val(eqA[[i]], "PropertyValue")
      eqValA   <- get_val(eqA[[i]], "RealEstateEquity")
      pctA <- if (propValA > 0) (eqValA / propValA * 100) else 0
      dfOut[[paste0("PctOwned_", input$labelA)]][i]           <- round(pctA, 0)
      
      dfOut[[paste0("TotalInterestPaid_", input$labelA)]][i]  <- round(get_val(eqA[[i]], "TotalInterestPaid"))
      dfOut[[paste0("InvContrib_", input$labelA)]][i]         <- round(get_val(eqA[[i]], "InvContrib"))
      dfOut[[paste0("InvGains_", input$labelA)]][i]           <- round(get_val(eqA[[i]], "InvGains"))
      dfOut[[paste0("InvBalance_", input$labelA)]][i]         <- round(get_val(eqA[[i]], "InvBalance"))
      dfOut[[paste0("TotalEquity_", input$labelA)]][i]        <- round(get_val(eqA[[i]], "TotalEquity"))
    }
    
    # Fill columns for B
    for (i in seq_along(yrs_vector)) {
      dfOut[[paste0("PropertyValue_", input$labelB)]][i]      <- round(get_val(eqB[[i]], "PropertyValue"))
      dfOut[[paste0("PropertyApp_", input$labelB)]][i]        <- round(get_val(eqB[[i]], "PropertyApp"))
      dfOut[[paste0("TotalPrincipalPaid_", input$labelB)]][i] <- round(get_val(eqB[[i]], "TotalPrincipalPaid"))
      
      propValB <- get_val(eqB[[i]], "PropertyValue")
      eqValB   <- get_val(eqB[[i]], "RealEstateEquity")
      pctB <- if (propValB > 0) (eqValB / propValB * 100) else 0
      dfOut[[paste0("PctOwned_", input$labelB)]][i]           <- round(pctB, 0)
      
      dfOut[[paste0("TotalInterestPaid_", input$labelB)]][i]  <- round(get_val(eqB[[i]], "TotalInterestPaid"))
      dfOut[[paste0("InvContrib_", input$labelB)]][i]         <- round(get_val(eqB[[i]], "InvContrib"))
      dfOut[[paste0("InvGains_", input$labelB)]][i]           <- round(get_val(eqB[[i]], "InvGains"))
      dfOut[[paste0("InvBalance_", input$labelB)]][i]         <- round(get_val(eqB[[i]], "InvBalance"))
      dfOut[[paste0("TotalEquity_", input$labelB)]][i]        <- round(get_val(eqB[[i]], "TotalEquity"))
    }
    
    # Fill columns for C
    for (i in seq_along(yrs_vector)) {
      dfOut[[paste0("PropertyValue_", input$labelC)]][i]      <- NA_real_
      dfOut[[paste0("PropertyApp_", input$labelC)]][i]        <- NA_real_
      dfOut[[paste0("TotalPrincipalPaid_", input$labelC)]][i] <- NA_real_
      
      # For renting, we have 0 or NA for equity, so 0% owned
      dfOut[[paste0("PctOwned_", input$labelC)]][i]           <- 0
      
      dfOut[[paste0("TotalInterestPaid_", input$labelC)]][i]  <- NA_real_
      dfOut[[paste0("InvContrib_", input$labelC)]][i]         <- round(get_val(eqC[[i]], "InvContrib"))
      dfOut[[paste0("InvGains_", input$labelC)]][i]           <- round(get_val(eqC[[i]], "InvGains"))
      dfOut[[paste0("InvBalance_", input$labelC)]][i]         <- round(get_val(eqC[[i]], "InvBalance"))
      dfOut[[paste0("TotalEquity_", input$labelC)]][i]        <- round(get_val(eqC[[i]], "TotalEquity"))
    }
    
    # Convert Year to integer
    dfOut$Year <- as.integer(dfOut$Year)
    
    dfOut
  })
  
  output$equityTable <- renderTable({
    req(equityTable())
    equityTable()
  },
  striped = TRUE,
  hover = TRUE,
  align = "c",
  sanitize.text.function = function(x) x,
  include.rownames = FALSE,
  digits = 0  # Ensures no decimal places
  )
  
  # --------------------------------------------
  # Helper to remove decimals in Plotly hover axes
  # --------------------------------------------
  no_decimal_plotly <- function(g) {
    # Attempt to set all y-axes to .0f
    for(i in 1:6) {
      ax <- paste0("yaxis", ifelse(i==1, "", i))
      if(!is.null(g$x$layout[[ax]])) {
        g$x$layout[[ax]]$tickformat <- ".0f"
      }
    }
    g
  }
  
  reactivePlotMonths <- reactive({
    req(input$maxPlotYears)
    input$maxPlotYears * 12
  })
  
  # --------------------------------------------
  # 1) Total Equity Over Time
  # --------------------------------------------
  output$plotInvestmentBalances <- renderPlotly({
    req(dataA(), dataB(), dataC())
    dfA <- dataA()
    dfB <- dataB()
    dfC <- dataC()
    
    dA <- data.frame(month = dfA$month, value = dfA$total_equity, option = input$labelA)
    dB <- data.frame(month = dfB$month, value = dfB$total_equity, option = input$labelB)
    dC <- data.frame(month = dfC$month, value = dfC$total_equity, option = input$labelC)
    
    df <- rbind(dA, dB, dC)
    
    p <- ggplot(df, aes(x = month, y = value, color = option)) +
      geom_line(size=1.2) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 7),
        labels = function(x) paste0("€", comma(x))
      ) +
      labs(title=t("tab_total_equity"), x="Month", y="€", color="Option") +
      theme_bw()
    
    g <- ggplotly(p, tooltip = c("x","y")) %>% 
      layout(legend = list(x=1, y=1))
    no_decimal_plotly(g)
  })
  
  # --------------------------------------------
  # 2) Principal & Interest Paid
  # --------------------------------------------
  output$plotPrincipalOwed <- renderPlotly({
    req(dataA(), dataB())
    dfA <- dataA()
    dfB <- dataB()
    
    dA <- data.frame(
      month = dfA$month,
      totalInterestPaid = dfA$total_interest_paid,
      totalPrincipalPaid= dfA$total_principal_paid,
      option = input$labelA
    )
    dB <- data.frame(
      month = dfB$month,
      totalInterestPaid = dfB$total_interest_paid,
      totalPrincipalPaid= dfB$total_principal_paid,
      option = input$labelB
    )
    dAll <- rbind(dA, dB)
    
    dLong <- reshape2::melt(dAll, id.vars=c("month","option"), 
                            variable.name="Metric", value.name="Value")
    
    # Correct facet labels
    metric_labels <- list(
      "totalInterestPaid" = t("tab_principal_interest"),
      "totalPrincipalPaid" = t("tab_principal_interest")
    )
    
    p <- ggplot(dLong, aes(x=month, y=Value, color=option)) +
      geom_line(size=1.2) +
      facet_wrap(~Metric, scales="free_y",
                 labeller = as_labeller(metric_labels)) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 7),
        labels = function(x) paste0("€", comma(x))
      ) +
      labs(title=t("tab_principal_interest"), x="Month", y="€", color="Option") +
      theme_bw()
    
    g <- ggplotly(p, tooltip = c("x","y")) %>%
      layout(legend = list(x=1, y=1))
    no_decimal_plotly(g)
  })
  
  # --------------------------------------------
  # 3) Property Value
  # --------------------------------------------
  output$plotPropertyValue <- renderPlotly({
    req(dataA(), dataB())
    dfA <- dataA(); dfB <- dataB()
    dA <- data.frame(month=dfA$month, value=dfA$property_value, option=input$labelA)
    dB <- data.frame(month=dfB$month, value=dfB$property_value, option=input$labelB)
    
    df <- rbind(dA, dB)
    
    p <- ggplot(df, aes(x=month, y=value, color=option)) +
      geom_line(size=1.2) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 7),
        labels=function(x) paste0("€", comma(x))
      ) +
      labs(title=t("tab_property_value"), x="Month", y="€", color="Option") +
      theme_bw()
    
    g <- ggplotly(p, tooltip = c("x","y")) %>% 
      layout(legend = list(x=1, y=1))
    no_decimal_plotly(g)
  })
  
  # --------------------------------------------
  # 4) Detailed Gains & Contributions
  # --------------------------------------------
  output$plotAllDetailedGains <- renderPlotly({
    req(dataA(), dataB(), dataC())
    dfA <- dataA(); dfA$Option <- input$labelA
    dfB <- dataB(); dfB$Option <- input$labelB
    dfC <- dataC(); dfC$Option <- input$labelC
    
    dfAll <- rbind(
      dfA[, c("month","Option","inv_contrib","inv_gains","property_app","real_estate_equity")],
      dfB[, c("month","Option","inv_contrib","inv_gains","property_app","real_estate_equity")],
      dfC[, c("month","Option","inv_contrib","inv_gains","property_app","real_estate_equity")]
    )
    
    dLong <- melt(dfAll, id.vars=c("month","Option"))
    
    # Translate labels
    translated_metric_labels <- list(
      "inv_contrib"        = if(lang() == "ENG") "Cumulative Invested (InvContrib)" else "Kogunenud Investeering (InvContrib)",
      "inv_gains"          = if(lang() == "ENG") "Investment Gains (InvGains)" else "Investeeringu Kasv (InvGains)",
      "property_app"       = if(lang() == "ENG") "Property Appreciation" else "Kinnisvara Kasv",
      "real_estate_equity" = if(lang() == "ENG") "Real Estate Equity" else "Reaalmajanduslik Omakapital"
    )
    
    p <- ggplot(dLong, aes(x=month, y=value, color=Option)) +
      geom_line(size=1.2, na.rm=TRUE) +
      facet_wrap(~variable, scales="free_y",
                 labeller = as_labeller(translated_metric_labels)) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 7),
        labels = function(x) paste0("€", comma(x))
      ) +
      labs(title=t("tab_detailed_gains"),
           x="Month", y="€", color="Option") +
      theme_bw()
    
    g <- ggplotly(p, tooltip = c("x","y")) %>% 
      layout(legend=list(x=1,y=1))
    no_decimal_plotly(g)
  })
  
  # --------------------------------------------
  # Compare Mortgage Styles
  # --------------------------------------------
  # We'll forcibly simulate both styles for A & B, 
  # and plot their cumulative principal vs interest on one plot (faceted).
  
  # Data for comparison: A
  compareDataA_Annuity <- reactive({
    req(input$full_priceA, input$down_paymentA, monthlyAllocA())
    principalA <- input$full_priceA - input$down_paymentA
    # We'll reuse the final 'budget' that user actually invests for A.
    total_budgetA <- monthlyAllocA()$mortgage + monthlyAllocA()$invest
    simulate_mortgage_and_investing(
      principal             = principalA,
      annual_mort_rate      = input$annual_mort_rate / 100,
      total_monthly_budget  = total_budgetA,
      years                 = totalTermYears,
      annual_invest_return  = input$annual_invest_return / 100,
      annual_property_growth= input$annual_property_growth / 100,
      mortgage_style        = "annuity",
      initial_property_value= input$full_priceA,
      initial_investment    = input$initial_investA  # compare with same initial investment
    )
  })
  compareDataA_Equal <- reactive({
    req(input$full_priceA, input$down_paymentA, monthlyAllocA())
    principalA <- input$full_priceA - input$down_paymentA
    total_budgetA <- monthlyAllocA()$mortgage + monthlyAllocA()$invest
    simulate_mortgage_and_investing(
      principal             = principalA,
      annual_mort_rate      = input$annual_mort_rate / 100,
      total_monthly_budget  = total_budgetA,
      years                 = totalTermYears,
      annual_invest_return  = input$annual_invest_return / 100,
      annual_property_growth= input$annual_property_growth / 100,
      mortgage_style        = "equal_principal",
      initial_property_value= input$full_priceA,
      initial_investment    = input$initial_investA
    )
  })
  
  # Data for comparison: B
  compareDataB_Annuity <- reactive({
    req(input$full_priceB, input$down_paymentB, monthlyAllocB())
    principalB <- input$full_priceB - input$down_paymentB
    total_budgetB <- monthlyAllocB()$mortgage + monthlyAllocB()$invest
    simulate_mortgage_and_investing(
      principal             = principalB,
      annual_mort_rate      = input$annual_mort_rate / 100,
      total_monthly_budget  = total_budgetB,
      years                 = totalTermYears,
      annual_invest_return  = input$annual_invest_return / 100,
      annual_property_growth= input$annual_property_growth / 100,
      mortgage_style        = "annuity",
      initial_property_value= input$full_priceB,
      initial_investment    = input$initial_investB
    )
  })
  compareDataB_Equal <- reactive({
    req(input$full_priceB, input$down_paymentB, monthlyAllocB())
    principalB <- input$full_priceB - input$down_paymentB
    total_budgetB <- monthlyAllocB()$mortgage + monthlyAllocB()$invest
    simulate_mortgage_and_investing(
      principal             = principalB,
      annual_mort_rate      = input$annual_mort_rate / 100,
      total_monthly_budget  = total_budgetB,
      years                 = totalTermYears,
      annual_invest_return  = input$annual_invest_return / 100,
      annual_property_growth= input$annual_property_growth / 100,
      mortgage_style        = "equal_principal",
      initial_property_value= input$full_priceB,
      initial_investment    = input$initial_investB
    )
  })
  
  # 1) Principal & Interest (absolute)
  output$plotCompareMortgageStyles <- renderPlotly({
    req(compareDataA_Annuity(), compareDataA_Equal(), compareDataB_Annuity(), compareDataB_Equal())
    
    # Build data frames for each property + each style
    dAann  <- compareDataA_Annuity()
    dAeq   <- compareDataA_Equal()
    dBann  <- compareDataB_Annuity()
    dBeq   <- compareDataB_Equal()
    
    # Tag them
    dAann$label <- paste0(input$labelA, "-Annuity")
    dAeq$label  <- paste0(input$labelA, "-Equal")
    dBann$label <- paste0(input$labelB, "-Annuity")
    dBeq$label  <- paste0(input$labelB, "-Equal")
    
    # Build a single combined data
    combined <- rbind(
      data.frame(month = dAann$month, principal = dAann$total_principal_paid,
                 interest = dAann$total_interest_paid, style = dAann$label),
      data.frame(month = dAeq$month, principal = dAeq$total_principal_paid,
                 interest = dAeq$total_interest_paid, style = dAeq$label),
      data.frame(month = dBann$month, principal = dBann$total_principal_paid,
                 interest = dBann$total_interest_paid, style = dBann$label),
      data.frame(month = dBeq$month, principal = dBeq$total_principal_paid,
                 interest = dBeq$total_interest_paid, style = dBeq$label)
    )
    
    # Melt for faceting
    dfLong <- melt(combined, id.vars=c("month","style"),
                   variable.name="Metric", value.name="Value")
    
    # Manual color and linetype:
    color_values <- setNames(
      c("blue4", "blue", "red4", "red"),
      c(
        paste0(input$labelA, "-Annuity"),
        paste0(input$labelA, "-Equal"),
        paste0(input$labelB, "-Annuity"),
        paste0(input$labelB, "-Equal")
      )
    )
    linetype_values <- setNames(
      c("solid", "dashed", "solid", "dashed"),
      c(
        paste0(input$labelA, "-Annuity"),
        paste0(input$labelA, "-Equal"),
        paste0(input$labelB, "-Annuity"),
        paste0(input$labelB, "-Equal")
      )
    )
    
    # Define separate labels for each metric
    metric_labels <- list(
      "principal" = t("tab_compare_mortgage"),
      "interest"  = t("tab_compare_mortgage")
    )
    
    p <- ggplot(dfLong, aes(x=month, y=Value, color=style, linetype=style)) +
      geom_line(size=1.2) +
      facet_wrap(~Metric, scales="free_y",
                 labeller = as_labeller(metric_labels)) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 7),
        labels = function(x) paste0("€", comma(x))
      ) +
      scale_color_manual(values=color_values) +
      scale_linetype_manual(values=linetype_values) +
      labs(
        title=t("tab_compare_mortgage"),
        x="Month", y="€", color="Property-Style", linetype="Property-Style"
      ) +
      theme_bw()
    
    g <- ggplotly(p, tooltip=c("x","y")) %>%
      layout(legend = list(x=1, y=1))
    no_decimal_plotly(g)
  })
  
  # 2) Percentage of Ownership & Interest (relative to principal)
  output$plotCompareMortgageStylesPerc <- renderPlotly({
    req(compareDataA_Annuity(), compareDataA_Equal(), compareDataB_Annuity(), compareDataB_Equal())
    
    # We'll compute:
    #   - % of Principal Paid = total_principal_paid / initial_principal * 100
    #   - % of Interest vs. Principal = total_interest_paid / initial_principal * 100
    
    # Data A Annuity
    dAann  <- compareDataA_Annuity()
    principalA <- input$full_priceA - input$down_paymentA
    dAann$pctPrincipal <- (dAann$total_principal_paid / principalA) * 100
    dAann$pctInterest  <- (dAann$total_interest_paid  / principalA) * 100
    dAann$label        <- paste0(input$labelA, "-Annuity")
    
    # Data A Equal
    dAeq   <- compareDataA_Equal()
    dAeq$pctPrincipal <- (dAeq$total_principal_paid / principalA) * 100
    dAeq$pctInterest  <- (dAeq$total_interest_paid  / principalA) * 100
    dAeq$label        <- paste0(input$labelA, "-Equal")
    
    # Data B Annuity
    dBann  <- compareDataB_Annuity()
    principalB <- input$full_priceB - input$down_paymentB
    dBann$pctPrincipal <- (dBann$total_principal_paid / principalB) * 100
    dBann$pctInterest  <- (dBann$total_interest_paid  / principalB) * 100
    dBann$label        <- paste0(input$labelB, "-Annuity")
    
    # Data B Equal
    dBeq   <- compareDataB_Equal()
    dBeq$pctPrincipal <- (dBeq$total_principal_paid / principalB) * 100
    dBeq$pctInterest  <- (dBeq$total_interest_paid  / principalB) * 100
    dBeq$label        <- paste0(input$labelB, "-Equal")
    
    # Combine
    combined <- rbind(
      data.frame(month = dAann$month, pctPrincipal = dAann$pctPrincipal,
                 pctInterest = dAann$pctInterest, style = dAann$label),
      data.frame(month = dAeq$month, pctPrincipal = dAeq$pctPrincipal,
                 pctInterest = dAeq$pctInterest, style = dAeq$label),
      data.frame(month = dBann$month, pctPrincipal = dBann$pctPrincipal,
                 pctInterest = dBann$pctInterest, style = dBann$label),
      data.frame(month = dBeq$month, pctPrincipal = dBeq$pctPrincipal,
                 pctInterest = dBeq$pctInterest, style = dBeq$label)
    )
    
    # Melt
    dfLong <- melt(combined, id.vars=c("month","style"),
                   variable.name="Metric", value.name="Value")
    
    # Manual color + linetype
    color_values <- setNames(
      c("blue4", "blue", "red4", "red"),
      c(
        paste0(input$labelA, "-Annuity"),
        paste0(input$labelA, "-Equal"),
        paste0(input$labelB, "-Annuity"),
        paste0(input$labelB, "-Equal")
      )
    )
    linetype_values <- setNames(
      c("solid", "dashed", "solid", "dashed"),
      c(
        paste0(input$labelA, "-Annuity"),
        paste0(input$labelA, "-Equal"),
        paste0(input$labelB, "-Annuity"),
        paste0(input$labelB, "-Equal")
      )
    )
    
    # Define separate labels for each metric
    metric_labels <- list(
      "pctPrincipal" = if(lang() == "ENG") "% of Principal Paid" else "% Peamise Makstud",
      "pctInterest"  = if(lang() == "ENG") "% of Interest (Relative to Principal)" else "% Intress (Võrdlus Peamisega)"
    )
    
    p <- ggplot(dfLong, aes(x=month, y=Value, color=style, linetype=style)) +
      geom_line(size=1.2) +
      facet_wrap(~Metric, scales="free_y",
                 labeller = as_labeller(metric_labels)) +
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 10),
        labels = function(x) paste0(round(x,0),"%")
      ) +
      scale_color_manual(values=color_values) +
      scale_linetype_manual(values=linetype_values) +
      labs(
        title=t("tab_compare_mortgage"),
        x="Month", y="%", color="Property-Style", linetype="Property-Style"
      ) +
      theme_bw()
    
    g <- ggplotly(p, tooltip=c("x","y")) %>%
      layout(legend = list(x=1, y=1))
    
    no_decimal_plotly(g)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
