# -------------------------------------------------------
# Run this in your computer and find source code at:
# https://github.com/hjaan/kinnisvarakalkulaator/
# Author: Jaan Märten Huik 2025
# -------------------------------------------------------

library(shiny)
library(ggplot2)
library(reshape2)
library(scales)
library(plotly)
library(openxlsx)

# ------------------------------------------------
# Helper Functions
# ------------------------------------------------
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
    initial_investment = 0
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
  total_interest_paid     <- numeric(months)  # cumulative
  monthly_principal_paid  <- numeric(months)
  principal_owed          <- numeric(months)  # NEW: Track principal owed each month
  
  invested_this_month     <- numeric(months)
  property_value          <- numeric(months)
  inv_balance             <- numeric(months)
  
  inv_contrib             <- numeric(months)
  inv_gains               <- numeric(months)
  property_app            <- numeric(months)
  real_estate_equity      <- numeric(months)
  
  # Tracking variables
  curr_principal   <- principal
  investment       <- initial_investment
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
    
    principal_owed[m]          <- curr_principal  # store after update
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
    real_estate_equity    = real_estate_equity,
    principal_owed        = principal_owed    # NEW
  )
  
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
    real_estate_equity   = NA_real_,
    principal_owed       = NA_real_   # for renting, no mortgage => NA
  )
  
  # real_estate_equity is 0 for renting
  df$real_estate_equity[is.na(df$real_estate_equity)] <- 0
  df$total_equity <- df$inv_balance
  
  return(df)
}

# ------------------------------------------------
# UI
# ------------------------------------------------
ui <- fluidPage(
  
  # App Title
  titlePanel(textOutput("app_title")),
  
  sidebarLayout(
    sidebarPanel(
      # Language toggle
      actionButton("toggle_lang", "EST", class = "btn-primary"),
      br(), br(),
      
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
      # PROPERTY / RENTING OPTIONS (A, B, C)
      # ---------------------------
      hr(),
      uiOutput("propertyA_title"),
      uiOutput("labelA_ui"),
      uiOutput("full_priceA_ui"),
      uiOutput("down_paymentA_ui"),
      uiOutput("mortgage_styleA_ui"),
      uiOutput("initial_investA_ui"),
      uiOutput("investA_ui"),
      
      hr(),
      uiOutput("propertyB_title"),
      uiOutput("labelB_ui"),
      uiOutput("full_priceB_ui"),
      uiOutput("down_paymentB_ui"),
      uiOutput("mortgage_styleB_ui"),
      uiOutput("initial_investB_ui"),
      uiOutput("investB_ui"),
      
      hr(),
      uiOutput("propertyC_title"),
      uiOutput("labelC_ui"),
      uiOutput("start_investC_ui"),
      uiOutput("initial_rentC_ui"),
      uiOutput("annual_rent_increaseC_ui"),
      uiOutput("investC_ui"),
      
      hr(),
      # ---------------------------
      # Download Buttons
      # ---------------------------
      h4("Export"),
      downloadButton("downloadExcel", "Download Excel (Parameters + Tables)")
      
    ),
    
    mainPanel(
      # ---------------------------
      # Cards for Calculated Allocations (A, B, C)
      # ---------------------------
      fluidRow(
        # Option A
        column(4,
               wellPanel(
                 h4(uiOutput("optionA_title")),
                 textOutput("optionA_mortgagePayment"),
                 textOutput("optionA_investment")
               )
        ),
        # Option B
        column(4,
               wellPanel(
                 h4(uiOutput("optionB_title")),
                 textOutput("optionB_mortgagePayment"),
                 textOutput("optionB_investment")
               )
        ),
        # Option C
        column(4,
               wellPanel(
                 h4(uiOutput("optionC_title")),
                 textOutput("optionC_rentPayment"),
                 textOutput("optionC_investment")
               )
        )
      ),
      
      # ---------------------------
      # Tabs
      # ---------------------------
      tabsetPanel(
        tabPanel(textOutput("tab_equity_table_compact"),
                 br(),
                 htmlOutput("equity_table_compact_help"),
                 br(),
                 tableOutput("equityTableCompact")
        ),
        
        tabPanel(textOutput("tab_equity_table"),
                 br(),
                 htmlOutput("equity_table_help"),
                 br(),
                 tableOutput("equityTable")  
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
        
        tabPanel(textOutput("tab_compare_mortgage"),
                 br(),
                 plotlyOutput("plotCompareMortgageStyles", height = "600px"),
                 br(),
                 plotlyOutput("plotCompareMortgageStylesPerc", height = "600px")
        )
      ),
      
      # Footer
      br(), br(),
      fluidRow(
        column(12, align="center",
               htmlOutput("footer_text")
        )
      )
    )
  )
)

# ------------------------------------------------
# SERVER
# ------------------------------------------------
server <- function(input, output, session) {
  
  # To avoid scientific notation in tooltips:
  no_decimal_plotly <- function(g) {
    for(i in 1:6) {
      ax <- paste0("yaxis", ifelse(i==1, "", i))
      if(!is.null(g$x$layout[[ax]])) {
        g$x$layout[[ax]]$tickformat <- ",.0f"
      }
    }
    g
  }
  
  # Reactive value to store the language
  lang <- reactiveVal("ENG")
  
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
      monthly_expenses_help = "Enter your total monthly expenses (excluding mortgage/rent/invest).",
      annual_mort_rate_label = "Annual Mortgage Rate (%):",
      annual_mort_rate_help = "Long-term models might assume ~3%-4% for planning.",
      annual_invest_return_label = "Annual Investment Return (%):",
      annual_invest_return_help = "Long-term broad equity returns might be around 7-10%.",
      annual_property_growth_label = "Annual Property Growth (%):",
      annual_property_growth_help = "Estimate around 4-6% for property growth.",
      mortgage_term_help = "Mortgage term is 30 years.",
      interval_years_label = "Analysis Interval (Years):",
      maxPlotYears_label = "Show up to Year:",
      calc_allocations_title = "Calculated Allocations",
      optionA_title = "Property A Calculated Allocations",
      optionA_mortgagePayment_label = "Monthly Mortgage Payment:",
      optionA_investment_label = "Monthly Investment:",
      optionB_title = "Property B Calculated Allocations",
      optionB_mortgagePayment_label = "Monthly Mortgage Payment:",
      optionB_investment_label = "Monthly Investment:",
      optionC_title = "Rent & Invest Calculated Allocations",
      optionC_rentPayment_label = "Monthly Rent Payment (1st Month):",
      optionC_investment_label = "Monthly Investment (1st Month):",
      propertyA_title = "Property A",
      labelA_label = "Label A:",
      full_priceA_label = "Full Price (€):",
      down_paymentA_label = "Down Payment (€):",
      mortgage_styleA_label = "Mortgage Style:",
      investA_label = "Monthly Investment for A:",
      initial_investAB_label = "Initial Investment (€):",
      propertyB_title = "Property B",
      labelB_label = "Label B:",
      full_priceB_label = "Full Price (€):",
      down_paymentB_label = "Down Payment (€):",
      mortgage_styleB_label = "Mortgage Style:",
      investB_label = "Monthly Investment for B:",
      propertyC_title = "Rent & Invest",
      labelC_label = "Label C:",
      start_investC_label = "Initial Investment (€):",
      initial_rentC_label = "Initial Monthly Rent (€):",
      annual_rent_increaseC_label = "Annual Rent Increase (%):",
      investC_label = "Monthly Investment for C:",
      tab_equity_table_compact = "Compact Equity Table",
      tab_equity_table = "Full Equity Table",
      tab_total_equity = "Total Equity Over Time",
      tab_principal_interest = "Principal & Interest Paid",
      tab_property_value = "Property Value",
      tab_detailed_gains = "Detailed Gains & Contributions",
      tab_compare_mortgage = "Compare Mortgage Styles",
      equity_table_compact_help = "<b>Columns Explanation (Compact Table):</b><br>
                           <ul>
                             <li><b>Year:</b> The specific year in the simulation.</li>
                             <li><b>PropertyValue:</b> Estimated property value for option X.</li>
                             <li><b>InvBalance:</b> Current total investment balance for option X.</li>
                             <li><b>TotalEquity:</b> Combined equity for option X (Property + Investments).</li>
                           </ul>",
      equity_table_help = "<b>Columns Explanation (Full Table):</b><br>
                           <ul>
                             <li><b>Year:</b> The specific year in the simulation.</li>
                             <li><b>PropertyValue:</b> Estimated property value.</li>
                             <li><b>PropertyApp:</b> Property appreciation from start.</li>
                             <li><b>YearlyMortgagePaid:</b> Sum of mortgage payments for the year.</li>
                             <li><b>PrincipalOwed:</b> Remaining principal owed at year's end.</li>
                             <li><b>TotalPrincipalPaid:</b> Cumulative principal paid so far.</li>
                             <li><b>TotalInterestPaid:</b> Cumulative interest paid so far.</li>
                             <li><b>%Owned:</b> Percentage of property owned.</li>
                             <li><b>RealEstateEquity:</b> Current property equity (value - owed).</li>
                             <li><b>InvContrib:</b> Total investments contributed so far.</li>
                             <li><b>InvGains:</b> Net gains from investments.</li>
                             <li><b>InvBalance:</b> Current investment balance.</li>
                             <li><b>TotalEquity:</b> Sum of RealEstateEquity and InvBalance.</li>
                           </ul>",
      detailed_gains_help = "<b>Details & Formulas:</b><br>
                             <ul>
                               <li><b>InvContrib:</b> Sum of all monthly investments made so far.</li>
                               <li><b>InvGains:</b> Difference between current investment balance and total contributed.</li>
                               <li><b>PropertyApp:</b> Current property value minus initial purchase value.</li>
                               <li><b>RealEstateEquity:</b> PropertyValue - PrincipalOwed.</li>
                             </ul>"
    ),
    EST = list(
      app_title = "Kodulaenu ja investeerimisportfoolio simulatsioon",
      gen_params_title = "Üldised parameetrid",
      monthly_income_label = "Igakuine sissetulek (€):",
      monthly_income_help = "Sisesta oma igakuine netosissetulek.",
      monthly_expenses_label = "Igakuised kulud (€):",
      monthly_expenses_help = "Sisesta oma kogu igakuised kulutused (välja arvatud laen/üür/investeeringud).",
      annual_mort_rate_label = "Aastane kodulaenu intressimäär (%):",
      annual_mort_rate_help = "Pikas perspektiivis võib eeldada ~3%-4% planeerimiseks.",
      annual_invest_return_label = "Aastane investeeringute tootlus (%):",
      annual_invest_return_help = "Pikas perspektiivis võivad aktsiaturud anda 7-10% tootlust.",
      annual_property_growth_label = "Aastane kinnisvara kasv (%):",
      annual_property_growth_help = "Hinnanguliselt 4%-6% kinnisvara pikaajaline kasv.",
      mortgage_term_help = "Kodulaenu tähtaeg on 30 aastat.",
      interval_years_label = "Analüüsi samm (aastates):",
      maxPlotYears_label = "Kuva kuni (aastad):",
      calc_allocations_title = "Arvutatud jaotused",
      optionA_title = "Kinnisvara A arvutatud jaotused",
      optionA_mortgagePayment_label = "Kodulaenu igakuine makse:",
      optionA_investment_label = "Igakuine investeering:",
      optionB_title = "Kinnisvara B arvutatud jaotused",
      optionB_mortgagePayment_label = "Kodulaenu igakuine makse:",
      optionB_investment_label = "Igakuine investeering:",
      optionC_title = "Üür & Investeering arvutatud jaotused",
      optionC_rentPayment_label = "Igakuine üür (esimene kuu):",
      optionC_investment_label = "Igakuine investeering (esimene kuu):",
      propertyA_title = "Kinnisvara A",
      labelA_label = "Nimetus A:",
      full_priceA_label = "Täishind (€):",
      down_paymentA_label = "Sissemakse (€):",
      mortgage_styleA_label = "Laenu tüüp:",
      investA_label = "Igakuine investeering A:",
      initial_investAB_label = "Alginvesteering (€):",
      propertyB_title = "Kinnisvara B",
      labelB_label = "Nimetus B:",
      full_priceB_label = "Täishind (€):",
      down_paymentB_label = "Sissemakse (€):",
      mortgage_styleB_label = "Laenu tüüp:",
      investB_label = "Igakuine investeering B:",
      propertyC_title = "Üür & Investeerimine",
      labelC_label = "Nimetus C:",
      start_investC_label = "Alginvesteering (€):",
      initial_rentC_label = "Igakuine üür (€):",
      annual_rent_increaseC_label = "Üüri aastane tõus (%):",
      investC_label = "Igakuine investeering C:",
      tab_equity_table_compact = "Omakapitali kompaktne tabel",
      tab_equity_table = "Omakapitali suur tabel",
      tab_total_equity = "Kogu omakapital ajas",
      tab_principal_interest = "Põhiosa & intress",
      tab_property_value = "Kinnisvara väärtus",
      tab_detailed_gains = "Detailne kasv & panused",
      tab_compare_mortgage = "Laenutüüpide võrdlus",
      equity_table_compact_help = "<b>Veergude selgitus (kompaktne tabel):</b><br>
                           <ul>
                             <li><b>Aasta:</b> Simulatsiooni konkreetne aasta.</li>
                             <li><b>PropertyValue:</b> Hinnanguline kinnisvara väärtus variandi X jaoks.</li>
                             <li><b>InvBalance:</b> Praegune investeerimisportfoolio summa variandi X jaoks.</li>
                             <li><b>TotalEquity:</b> Kinnisvara ja investeeringute summavariandi X jaoks.</li>
                           </ul>",
      equity_table_help = "<b>Veergude selgitus (täielik tabel):</b><br>
                           <ul>
                             <li><b>Aasta:</b> Simulatsiooni konkreetne aasta.</li>
                             <li><b>PropertyValue:</b> Hinnanguline kinnisvara väärtus.</li>
                             <li><b>PropertyApp:</b> Kinnisvara väärtuse tõus algusest peale.</li>
                             <li><b>YearlyMortgagePaid:</b> Kogu laenumaksete summa selle aasta jooksul.</li>
                             <li><b>PrincipalOwed:</b> Laenus alles jäänud põhiosa aasta lõpus.</li>
                             <li><b>TotalPrincipalPaid:</b> Seni makstud põhiosa summa.</li>
                             <li><b>TotalInterestPaid:</b> Seni makstud intresside summa.</li>
                             <li><b>PctOwned:</b> Protsent kinnisvarast, mis on välja ostetud.</li>
                             <li><b>RealEstateEquity:</b> Hetkeline omakapital (väärtus - järelejäänud põhiosa).</li>
                             <li><b>InvContrib:</b> Kogunenud investeeringute summa.</li>
                             <li><b>InvGains:</b> Investeeringute hetkeväärtus miinus panustatud summa.</li>
                             <li><b>InvBalance:</b> Praegune investeerimisportfoolio summa.</li>
                             <li><b>TotalEquity:</b> Reaalmajanduslik omakapital ja investeerimisportfoolio summa.</li>
                           </ul>",
      detailed_gains_help = "<b>Detailsem info & valemid:</b><br>
                             <ul>
                               <li><b>InvContrib:</b> Kuude lõikes kogunenud investeering.</li>
                               <li><b>InvGains:</b> Investeeringute hetkeväärtuse ja panuse vahe.</li>
                               <li><b>PropertyApp:</b> Kinnisvara väärtuse kasv algsest ostuhinnast.</li>
                               <li><b>RealEstateEquity:</b> Kinnisvara väärtus - laenujääk.</li>
                             </ul>"
    )
  )
  
  t <- function(key) {
    req(translations[[lang()]][[key]])
    translations[[lang()]][[key]]
  }
  
  output$app_title <- renderText({ t("app_title") })
  output$gen_params_title <- renderUI({ h3(t("gen_params_title")) })
  output$monthly_income_ui <- renderUI({
    numericInput("monthly_income", t("monthly_income_label"), value = 2800, min = 0, step = 100)
  })
  output$monthly_income_help_ui <- renderUI({ helpText(t("monthly_income_help")) })
  output$monthly_expenses_ui <- renderUI({
    numericInput("monthly_expenses", t("monthly_expenses_label"), value = 700, min = 0, step = 50)
  })
  output$monthly_expenses_help_ui <- renderUI({ helpText(t("monthly_expenses_help")) })
  output$annual_mort_rate_ui <- renderUI({
    numericInput("annual_mort_rate", t("annual_mort_rate_label"), value = 3.5, min = 0, step = 0.1)
  })
  output$annual_mort_rate_help_ui <- renderUI({ helpText(t("annual_mort_rate_help")) })
  output$annual_invest_return_ui <- renderUI({
    numericInput("annual_invest_return", t("annual_invest_return_label"), value = 8, min = 0, step = 0.1)
  })
  output$annual_invest_return_help_ui <- renderUI({ helpText(t("annual_invest_return_help")) })
  output$annual_property_growth_ui <- renderUI({
    numericInput("annual_property_growth", t("annual_property_growth_label"), value = 5, min = 0, step = 0.1)
  })
  output$annual_property_growth_help_ui <- renderUI({ helpText(t("annual_property_growth_help")) })
  output$mortgage_term_help <- renderText({ t("mortgage_term_help") })
  output$interval_years_ui <- renderUI({
    sliderInput("interval_years", t("interval_years_label"), min = 1, max = 30, value = 1, step = 1)
  })
  output$maxPlotYears_ui <- renderUI({
    sliderInput("maxPlotYears", t("maxPlotYears_label"), min = 1, max = 30, value = 10, step = 1)
  })
  
  output$propertyA_title <- renderUI({ h3(t("propertyA_title")) })
  output$labelA_ui <- renderUI({ textInput("labelA", t("labelA_label"), value = "A") })
  output$full_priceA_ui <- renderUI({ numericInput("full_priceA", t("full_priceA_label"), value = 200000, min = 0, step = 1000) })
  output$down_paymentA_ui <- renderUI({ numericInput("down_paymentA", t("down_paymentA_label"), value = 20000, min = 0, step = 1000) })
  output$mortgage_styleA_ui <- renderUI({
    selectInput("mortgage_styleA", t("mortgage_styleA_label"),
                choices = c("Annuity" = "annuity", "Equal Principal" = "equal_principal"),
                selected = "annuity")
  })
  output$initial_investA_ui <- renderUI({ numericInput("initial_investA", t("initial_investAB_label"), value = 0, min = 0, step = 1000) })
  output$investA_ui <- renderUI({ sliderInput("investA", t("investA_label"), min = 0, max = 5000, value = 2000, step = 50) })
  
  output$propertyB_title <- renderUI({ h3(t("propertyB_title")) })
  output$labelB_ui <- renderUI({ textInput("labelB", t("labelB_label"), value = "B") })
  output$full_priceB_ui <- renderUI({ numericInput("full_priceB", t("full_priceB_label"), value = 120000, min = 0, step = 1000) })
  output$down_paymentB_ui <- renderUI({ numericInput("down_paymentB", t("down_paymentB_label"), value = 12000, min = 0, step = 1000) })
  output$mortgage_styleB_ui <- renderUI({
    selectInput("mortgage_styleB", t("mortgage_styleB_label"),
                choices = c("Annuity" = "annuity", "Equal Principal" = "equal_principal"),
                selected = "annuity")
  })
  output$initial_investB_ui <- renderUI({ numericInput("initial_investB", t("initial_investAB_label"), value = 0, min = 0, step = 1000) })
  output$investB_ui <- renderUI({ sliderInput("investB", t("investB_label"), min = 0, max = 5000, value = 1500, step = 50) })
  
  output$propertyC_title <- renderUI({ h3(t("propertyC_title")) })
  output$labelC_ui <- renderUI({ textInput("labelC", t("labelC_label"), value = "Rent & Invest") })
  output$start_investC_ui <- renderUI({ numericInput("start_investC", t("start_investC_label"), value = 20000, min = 0, step = 1000) })
  output$initial_rentC_ui <- renderUI({ numericInput("initial_rentC", t("initial_rentC_label"), value = 550, min = 0, step = 50) })
  output$annual_rent_increaseC_ui <- renderUI({ numericInput("annual_rent_increaseC", t("annual_rent_increaseC_label"), value = 0, min = 0, step = 0.1) })
  output$investC_ui <- renderUI({ sliderInput("investC", t("investC_label"), min = 0, max = 5000, value = 1800, step = 50) })
  
  output$optionA_title <- renderUI({ t("optionA_title") })
  output$optionB_title <- renderUI({ t("optionB_title") })
  output$optionC_title <- renderUI({ t("optionC_title") })
  
  output$tab_equity_table_compact <- renderText({ t("tab_equity_table_compact") })
  output$tab_equity_table <- renderText({ t("tab_equity_table") })
  output$tab_total_equity <- renderText({ t("tab_total_equity") })
  output$tab_principal_interest <- renderText({ t("tab_principal_interest") })
  output$tab_property_value <- renderText({ t("tab_property_value") })
  output$tab_detailed_gains <- renderText({ t("tab_detailed_gains") })
  output$tab_compare_mortgage <- renderText({ t("tab_compare_mortgage") })
  
  output$equity_table_compact_help <- renderUI({ HTML(t("equity_table_compact_help")) })
  output$equity_table_help <- renderUI({ HTML(t("equity_table_help")) })
  output$detailed_gains_help <- renderUI({ HTML(t("detailed_gains_help")) })
  
  output$footer_text <- renderUI({
    HTML(if(lang()=="ENG") {
      "Run this in your computer and find source code available from 
            <a href='https://github.com/hjaan/kinnisvarakalkulaator/'>https://github.com/hjaan/kinnisvarakalkulaator/</a><br>
             Author: <b>Jaan Märten Huik 2025</b>"
    } else {
      "Leia lähtekood ning jooksuta see enda arvutis siit 
            <a href='https://github.com/hjaan/kinnisvarakalkulaator/'>https://github.com/hjaan/kinnisvarakalkulaator/</a><br>
             Autor: <b>Jaan Märten Huik 2025</b>"
    })
  })
  
  totalTermYears <- 30
  
  common_pool <- reactive({
    req(input$monthly_income, input$monthly_expenses)
    pool <- input$monthly_income - input$monthly_expenses
    if (pool < 0) {
      showNotification(t("monthly_expenses_help"), type = "error")
      pool <- 0
    }
    pool
  })
  
  # A - Mortgage + Invest
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
    
    required_mort_payment <-
      if (input$mortgage_styleA == "annuity") {
        annuity_monthly_payment(principalA, input$annual_mort_rate/100, totalTermYears)
      } else {
        (principalA/(totalTermYears*12)) + (principalA*(input$annual_mort_rate/100)/12)
      }
    leftoverA <- common_pool() - required_mort_payment
    
    if (leftoverA < 0) {
      showNotification(
        paste(t("optionA_mortgagePayment_label"), "exceeds your available pool. Setting investment to 0."),
        type="warning"
      )
      investsA <- 0
    } else {
      investsA <- input$investA
      investsA <- min(investsA, leftoverA)
      if (investsA < 0) investsA <- 0
    }
    
    list(mortgage = required_mort_payment, invest = investsA, leftover = leftoverA)
  })
  
  observe({
    allocA <- monthlyAllocA()
    leftoverA <- allocA$leftover
    maxInvestA <- floor(leftoverA)
    updateSliderInput(session, "investA",
                      max = max(maxInvestA, 0),
                      value = min(input$investA, max(maxInvestA, 0)))
  })
  
  dataA <- reactive({
    req(monthlyAllocA())
    total_budgetA <- monthlyAllocA()$mortgage + monthlyAllocA()$invest
    simulate_mortgage_and_investing(
      principal = input$full_priceA - input$down_paymentA,
      annual_mort_rate = input$annual_mort_rate/100,
      total_monthly_budget = total_budgetA,
      years = totalTermYears,
      annual_invest_return = input$annual_invest_return/100,
      annual_property_growth= input$annual_property_growth/100,
      mortgage_style = input$mortgage_styleA,
      initial_property_value= input$full_priceA,
      initial_investment    = input$initial_investA
    )
  })
  
  output$optionA_mortgagePayment <- renderText({
    req(monthlyAllocA())
    paste0(t("optionA_mortgagePayment_label"), " €",
           formatC(round(monthlyAllocA()$mortgage, 2), format="f", digits=2))
  })
  output$optionA_investment <- renderText({
    req(monthlyAllocA())
    paste0(t("optionA_investment_label"), " €",
           formatC(round(monthlyAllocA()$invest, 2), format="f", digits=0))
  })
  
  # B - Mortgage + Invest
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
    
    required_mort_payment <-
      if (input$mortgage_styleB == "annuity") {
        annuity_monthly_payment(principalB, input$annual_mort_rate/100, totalTermYears)
      } else {
        (principalB/(totalTermYears*12)) + (principalB*(input$annual_mort_rate/100)/12)
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
  
  observe({
    allocB <- monthlyAllocB()
    leftoverB <- allocB$leftover
    maxInvestB <- floor(leftoverB)
    updateSliderInput(session, "investB",
                      max = max(maxInvestB, 0),
                      value = min(input$investB, max(maxInvestB, 0)))
  })
  
  dataB <- reactive({
    req(monthlyAllocB())
    total_budgetB <- monthlyAllocB()$mortgage + monthlyAllocB()$invest
    simulate_mortgage_and_investing(
      principal = input$full_priceB - input$down_paymentB,
      annual_mort_rate = input$annual_mort_rate/100,
      total_monthly_budget = total_budgetB,
      years = totalTermYears,
      annual_invest_return = input$annual_invest_return/100,
      annual_property_growth= input$annual_property_growth/100,
      mortgage_style = input$mortgage_styleB,
      initial_property_value= input$full_priceB,
      initial_investment    = input$initial_investB
    )
  })
  
  output$optionB_mortgagePayment <- renderText({
    req(monthlyAllocB())
    paste0(t("optionB_mortgagePayment_label"), " €",
           formatC(round(monthlyAllocB()$mortgage, 2), format="f", digits=2))
  })
  output$optionB_investment <- renderText({
    req(monthlyAllocB())
    paste0(t("optionB_investment_label"), " €",
           formatC(round(monthlyAllocB()$invest, 2), format="f", digits=0))
  })
  
  # C - Rent + Invest
  monthlyAllocC <- reactive({
    req(input$initial_rentC)
    rent_first_month <- input$initial_rentC
    leftoverC <- common_pool() - rent_first_month
    if (leftoverC < 0) {
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
      monthly_income = input$monthly_income,
      monthly_expenses= input$monthly_expenses,
      initial_rent = input$initial_rentC,
      annual_rent_increase = input$annual_rent_increaseC/100,
      annual_invest_return = input$annual_invest_return/100,
      years = totalTermYears,
      start_invest = input$start_investC
    )
  })
  
  output$optionC_rentPayment <- renderText({
    req(monthlyAllocC())
    paste0(t("optionC_rentPayment_label"), " €",
           formatC(round(monthlyAllocC()$rent, 2), format="f", digits=0))
  })
  output$optionC_investment <- renderText({
    req(monthlyAllocC())
    paste0(t("optionC_investment_label"), " €",
           formatC(round(monthlyAllocC()$invest, 2), format="f", digits=0))
  })
  
  # -------------------------------------------------------------------
  # Full Equity Table
  # -------------------------------------------------------------------
  # We also want to sum yearly mortgage payments, track principal owed etc.
  
  equity_at_year <- function(data_dict, year) {
    req(data_dict)
    m_start <- (year-1)*12 + 1
    m_end   <- min(year*12, nrow(data_dict))
    # sum mortgage for that year
    yearly_mort_paid <- sum(data_dict$mortgage_payment[m_start:m_end], na.rm=TRUE)
    
    # pick last month of that year
    m <- m_end
    list(
      PropertyValue       = data_dict$property_value[m],
      PropertyApp         = data_dict$property_app[m],
      TotalInterestPaid   = data_dict$total_interest_paid[m],
      TotalPrincipalPaid  = data_dict$total_principal_paid[m],
      RealEstateEquity    = data_dict$real_estate_equity[m],
      InvContrib          = data_dict$inv_contrib[m],
      InvGains            = data_dict$inv_gains[m],
      InvBalance          = data_dict$inv_balance[m],
      TotalEquity         = data_dict$total_equity[m],
      YearlyMortgagePaid  = yearly_mort_paid,
      PrincipalOwed       = data_dict$principal_owed[m]  # new
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
    if (length(yrs_vector) == 0) return(NULL)
    
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
      pctA <- if (propValA > 0) (eqValA/propValA*100) else 0
      dfOut[[paste0("PctOwned_", input$labelA)]][i]           <- round(pctA, 0)
      
      dfOut[[paste0("TotalInterestPaid_", input$labelA)]][i]  <- round(get_val(eqA[[i]], "TotalInterestPaid"))
      dfOut[[paste0("YearlyMortgagePaid_", input$labelA)]][i] <- round(get_val(eqA[[i]], "YearlyMortgagePaid"))
      dfOut[[paste0("PrincipalOwed_", input$labelA)]][i]      <- round(get_val(eqA[[i]], "PrincipalOwed"))
      dfOut[[paste0("RealEstateEquity_", input$labelA)]][i]   <- round(get_val(eqA[[i]], "RealEstateEquity"))
      
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
      pctB <- if (propValB > 0) (eqValB/propValB*100) else 0
      dfOut[[paste0("PctOwned_", input$labelB)]][i]           <- round(pctB, 0)
      
      dfOut[[paste0("TotalInterestPaid_", input$labelB)]][i]  <- round(get_val(eqB[[i]], "TotalInterestPaid"))
      dfOut[[paste0("YearlyMortgagePaid_", input$labelB)]][i] <- round(get_val(eqB[[i]], "YearlyMortgagePaid"))
      dfOut[[paste0("PrincipalOwed_", input$labelB)]][i]      <- round(get_val(eqB[[i]], "PrincipalOwed"))
      dfOut[[paste0("RealEstateEquity_", input$labelB)]][i]   <- round(get_val(eqB[[i]], "RealEstateEquity"))
      
            
      dfOut[[paste0("InvContrib_", input$labelB)]][i]         <- round(get_val(eqB[[i]], "InvContrib"))
      dfOut[[paste0("InvGains_", input$labelB)]][i]           <- round(get_val(eqB[[i]], "InvGains"))
      dfOut[[paste0("InvBalance_", input$labelB)]][i]         <- round(get_val(eqB[[i]], "InvBalance"))
      dfOut[[paste0("TotalEquity_", input$labelB)]][i]        <- round(get_val(eqB[[i]], "TotalEquity"))
    }
    
    # Fill columns for C (rent scenario => mostly zero or NA for mortgage stuff)
    for (i in seq_along(yrs_vector)) {
      dfOut[[paste0("PropertyValue_", input$labelC)]][i]      <- round(get_val(eqC[[i]], "PropertyValue"))
      dfOut[[paste0("PropertyApp_", input$labelC)]][i]        <- round(get_val(eqC[[i]], "PropertyApp"))
      dfOut[[paste0("TotalPrincipalPaid_", input$labelC)]][i] <- NA
      dfOut[[paste0("PctOwned_", input$labelC)]][i]           <- 0
      
      dfOut[[paste0("TotalInterestPaid_", input$labelC)]][i]  <- NA
      dfOut[[paste0("YearlyMortgagePaid_", input$labelC)]][i] <- 0
      dfOut[[paste0("PrincipalOwed_", input$labelC)]][i]      <- NA
      dfOut[[paste0("RealEstateEquity_", input$labelC)]][i]   <- 0
      
            
      dfOut[[paste0("InvContrib_", input$labelC)]][i]         <- round(get_val(eqC[[i]], "InvContrib"))
      dfOut[[paste0("InvGains_", input$labelC)]][i]           <- round(get_val(eqC[[i]], "InvGains"))
      dfOut[[paste0("InvBalance_", input$labelC)]][i]         <- round(get_val(eqC[[i]], "InvBalance"))
      dfOut[[paste0("TotalEquity_", input$labelC)]][i]        <- round(get_val(eqC[[i]], "TotalEquity"))
    }
    
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
  digits = 0)
  
  # ------------------------------------------------
  # Compact Equity Table
  # Only: PropertyValue, InvBalance, TotalEquity for each scenario
  # ------------------------------------------------
  equityTableCompact <- reactive({
    et <- equityTable()
    if (is.null(et)) return(NULL)
    
    # We pick columns: Year
    # Then "PropertyValue_A", "InvBalance_A", "TotalEquity_A"
    # same for B and C
    keepCols <- c("Year",
                  paste0("PropertyValue_", input$labelA),
                  paste0("InvBalance_", input$labelA),
                  paste0("TotalEquity_", input$labelA),
                  paste0("PropertyValue_", input$labelB),
                  paste0("InvBalance_", input$labelB),
                  paste0("TotalEquity_", input$labelB),
                  paste0("PropertyValue_", input$labelC),
                  paste0("InvBalance_", input$labelC),
                  paste0("TotalEquity_", input$labelC))
    
    keepCols <- keepCols[keepCols %in% names(et)]
    et[, keepCols, drop = FALSE]
  })
  
  output$equityTableCompact <- renderTable({
    req(equityTableCompact())
    equityTableCompact()
  },
  striped = TRUE,
  hover = TRUE,
  align = "c",
  sanitize.text.function = function(x) x,
  include.rownames = FALSE,
  digits = 0)
  
  # --------------------------------------------
  # Plot months
  # --------------------------------------------
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
    p <- ggplot(df, aes(x=month, y=value, color=option)) +
      geom_line(size=1.2) +
      scale_x_continuous(limits=c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks = pretty_breaks(n=7),
        labels = function(x) paste0("€", comma(x))
      ) +
      labs(title=t("tab_total_equity"), x="Month", y="€", color="Option") +
      theme_bw()
    
    g <- ggplotly(p, tooltip=c("x","y")) %>%
      layout(legend=list(x=1, y=1)) %>%
      config(toImageButtonOptions=list(filename="TotalEquityPlot"))
    
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
    
    dLong <- melt(dAll, id.vars=c("month","option"),
                  variable.name="Metric", value.name="Value")
    
    metric_labels <- list(
      "totalInterestPaid" = "Total Interest Paid",
      "totalPrincipalPaid" = "Total Principal Paid"
    )
    
    p <- ggplot(dLong, aes(x=month, y=Value, color=option)) +
      geom_line(size=1.2) +
      facet_wrap(~Metric, scales="free_y",
                 labeller = as_labeller(metric_labels)) +
      scale_x_continuous(limits=c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks=pretty_breaks(n=7),
        labels=function(x) paste0("€", comma(x))
      ) +
      labs(title=t("tab_principal_interest"), x="Month", y="€", color="Option") +
      theme_bw()
    
    g <- ggplotly(p, tooltip=c("x","y")) %>%
      layout(legend=list(x=1, y=1)) %>%
      config(toImageButtonOptions=list(filename="PrincipalInterestPlot"))
    
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
      scale_x_continuous(limits=c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks=pretty_breaks(n=7),
        labels=function(x) paste0("€", comma(x))
      ) +
      labs(title=t("tab_property_value"), x="Month", y="€", color="Option") +
      theme_bw()
    
    g <- ggplotly(p, tooltip=c("x","y")) %>%
      layout(legend=list(x=1, y=1)) %>%
      config(toImageButtonOptions=list(filename="PropertyValuePlot"))
    
    no_decimal_plotly(g)
  })
  
  # --------------------------------------------
  # 4) Detailed Gains & Contributions
  # --------------------------------------------
  output$plotAllDetailedGains <- renderPlotly({
    req(dataA(), dataB(), dataC())
    
    # Prepare your data
    dfA <- dataA(); dfA$Option <- input$labelA
    dfB <- dataB(); dfB$Option <- input$labelB
    dfC <- dataC(); dfC$Option <- input$labelC
    
    dfAll <- rbind(
      dfA[, c("month","Option","inv_contrib","inv_gains","property_app","real_estate_equity")],
      dfB[, c("month","Option","inv_contrib","inv_gains","property_app","real_estate_equity")],
      dfC[, c("month","Option","inv_contrib","inv_gains","property_app","real_estate_equity")]
    )
    
    dLong <- reshape2::melt(dfAll, id.vars = c("month", "Option"))
    
    # Translated facet labels
    translated_metric_labels <- list(
      "inv_contrib"        = if(lang()=="ENG") "Cumulative Invested" else "Kogunenud Investeering",
      "inv_gains"          = if(lang()=="ENG") "Investment Gains"     else "Investeeringu Kasv",
      "property_app"       = if(lang()=="ENG") "Property Appreciation" else "Kinnisvara Kasv",
      "real_estate_equity" = if(lang()=="ENG") "Real Estate Equity"    else "Kinnisvara omakapital"
    )
    
    p <- ggplot(dLong, aes(
      x     = month,
      y     = value,
      color = Option,
      group = Option,  # Important to connect lines per Option
      text  = paste("Month:", month, "<br>Value:", format(value, scientific=FALSE))
    )) +
      geom_line(size=1.2, na.rm=TRUE) +
      facet_wrap(
        ~ variable,
        scales = "free_y",
        labeller = as_labeller(translated_metric_labels)
      ) +
      # Use the same approach as your Property Value plot:
      scale_x_continuous(limits = c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n=7),
        # Show euro sign on the numeric labels:
        labels = function(x) paste0("€", format(x, scientific=FALSE, big.mark=""))
      ) +
      labs(
        title = t("tab_detailed_gains"),
        x     = "Month",
        y     = "",       # or "€" if you want a literal "€" on the axis label
        color = "Option"
      ) +
      theme_bw()
    
    # Convert to Plotly, using tooltip="text" 
    g <- ggplotly(p, tooltip="text") %>%
      layout(legend=list(x=1, y=1)) %>%
      config(toImageButtonOptions=list(filename="DetailedGainsPlot"))
    
    # Apply your custom no_decimal_plotly function to finalize formatting
    no_decimal_plotly(g)
  })
  
  
  # --------------------------------------------
  # Compare Mortgage Styles
  # --------------------------------------------
  compareDataA_Annuity <- reactive({
    req(input$full_priceA, input$down_paymentA, monthlyAllocA())
    principalA <- input$full_priceA - input$down_paymentA
    total_budgetA <- monthlyAllocA()$mortgage + monthlyAllocA()$invest
    simulate_mortgage_and_investing(
      principal = principalA,
      annual_mort_rate = input$annual_mort_rate/100,
      total_monthly_budget = total_budgetA,
      years = totalTermYears,
      annual_invest_return = input$annual_invest_return/100,
      annual_property_growth= input$annual_property_growth/100,
      mortgage_style = "annuity",
      initial_property_value= input$full_priceA,
      initial_investment    = input$initial_investA
    )
  })
  compareDataA_Equal <- reactive({
    req(input$full_priceA, input$down_paymentA, monthlyAllocA())
    principalA <- input$full_priceA - input$down_paymentA
    total_budgetA <- monthlyAllocA()$mortgage + monthlyAllocA()$invest
    simulate_mortgage_and_investing(
      principal = principalA,
      annual_mort_rate = input$annual_mort_rate/100,
      total_monthly_budget = total_budgetA,
      years = totalTermYears,
      annual_invest_return = input$annual_invest_return/100,
      annual_property_growth= input$annual_property_growth/100,
      mortgage_style = "equal_principal",
      initial_property_value= input$full_priceA,
      initial_investment    = input$initial_investA
    )
  })
  
  compareDataB_Annuity <- reactive({
    req(input$full_priceB, input$down_paymentB, monthlyAllocB())
    principalB <- input$full_priceB - input$down_paymentB
    total_budgetB <- monthlyAllocB()$mortgage + monthlyAllocB()$invest
    simulate_mortgage_and_investing(
      principal = principalB,
      annual_mort_rate = input$annual_mort_rate/100,
      total_monthly_budget = total_budgetB,
      years = totalTermYears,
      annual_invest_return = input$annual_invest_return/100,
      annual_property_growth= input$annual_property_growth/100,
      mortgage_style = "annuity",
      initial_property_value= input$full_priceB,
      initial_investment    = input$initial_investB
    )
  })
  compareDataB_Equal <- reactive({
    req(input$full_priceB, input$down_paymentB, monthlyAllocB())
    principalB <- input$full_priceB - input$down_paymentB
    total_budgetB <- monthlyAllocB()$mortgage + monthlyAllocB()$invest
    simulate_mortgage_and_investing(
      principal = principalB,
      annual_mort_rate = input$annual_mort_rate/100,
      total_monthly_budget = total_budgetB,
      years = totalTermYears,
      annual_invest_return = input$annual_invest_return/100,
      annual_property_growth= input$annual_property_growth/100,
      mortgage_style = "equal_principal",
      initial_property_value= input$full_priceB,
      initial_investment    = input$initial_investB
    )
  })
  
  # 1) Principal & Interest (absolute)
  output$plotCompareMortgageStyles <- renderPlotly({
    req(compareDataA_Annuity(), compareDataA_Equal(),
        compareDataB_Annuity(), compareDataB_Equal())
    
    dAann <- compareDataA_Annuity(); dAeq <- compareDataA_Equal()
    dBann <- compareDataB_Annuity(); dBeq <- compareDataB_Equal()
    
    dAann$label <- paste0(input$labelA, "-Annuity")
    dAeq$label  <- paste0(input$labelA, "-Equal")
    dBann$label <- paste0(input$labelB, "-Annuity")
    dBeq$label  <- paste0(input$labelB, "-Equal")
    
    combined <- rbind(
      data.frame(month=dAann$month, principal=dAann$total_principal_paid,
                 interest=dAann$total_interest_paid, style=dAann$label),
      data.frame(month=dAeq$month, principal=dAeq$total_principal_paid,
                 interest=dAeq$total_interest_paid, style=dAeq$label),
      data.frame(month=dBann$month, principal=dBann$total_principal_paid,
                 interest=dBann$total_interest_paid, style=dBann$label),
      data.frame(month=dBeq$month, principal=dBeq$total_principal_paid,
                 interest=dBeq$total_interest_paid, style=dBeq$label)
    )
    
    dfLong <- melt(combined, id.vars=c("month","style"),
                   variable.name="Metric", value.name="Value")
    
    color_values <- setNames(c("blue4","blue","red4","red"),
                             c(paste0(input$labelA, "-Annuity"),
                               paste0(input$labelA, "-Equal"),
                               paste0(input$labelB, "-Annuity"),
                               paste0(input$labelB, "-Equal")))
    linetype_values <- setNames(c("solid","dashed","solid","dashed"),
                                c(paste0(input$labelA, "-Annuity"),
                                  paste0(input$labelA, "-Equal"),
                                  paste0(input$labelB, "-Annuity"),
                                  paste0(input$labelB, "-Equal")))
    
    metric_labels <- list(
      "principal" = "Total Principal Paid",
      "interest"  = "Total Interest Paid"
    )
    
    p <- ggplot(dfLong, aes(x=month, y=Value, color=style, linetype=style)) +
      geom_line(size=1.2) +
      facet_wrap(~Metric, scales="free_y",
                 labeller=as_labeller(metric_labels)) +
      scale_x_continuous(limits=c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks=pretty_breaks(n=7),
        labels=function(x) paste0("€", comma(x))
      ) +
      scale_color_manual(values=color_values) +
      scale_linetype_manual(values=linetype_values) +
      labs(title=t("tab_compare_mortgage"),
           x="Month", y="€", color="Style", linetype="Style") +
      theme_bw()
    
    g <- ggplotly(p, tooltip=c("x","y")) %>%
      layout(legend=list(x=1, y=1)) %>%
      config(toImageButtonOptions=list(filename="CompareMortgageStyles"))
    
    no_decimal_plotly(g)
  })
  
  # 2) Percentage of Ownership & Interest (relative to principal)
  output$plotCompareMortgageStylesPerc <- renderPlotly({
    req(compareDataA_Annuity(), compareDataA_Equal(),
        compareDataB_Annuity(), compareDataB_Equal())
    
    dAann <- compareDataA_Annuity()
    dAeq  <- compareDataA_Equal()
    principalA <- input$full_priceA - input$down_paymentA
    dAann$pctPrincipal <- (dAann$total_principal_paid / principalA)*100
    dAann$pctInterest  <- (dAann$total_interest_paid  / principalA)*100
    dAann$label        <- paste0(input$labelA, "-Annuity")
    
    dAeq$pctPrincipal  <- (dAeq$total_principal_paid / principalA)*100
    dAeq$pctInterest   <- (dAeq$total_interest_paid  / principalA)*100
    dAeq$label         <- paste0(input$labelA, "-Equal")
    
    dBann <- compareDataB_Annuity()
    dBeq  <- compareDataB_Equal()
    principalB <- input$full_priceB - input$down_paymentB
    dBann$pctPrincipal <- (dBann$total_principal_paid / principalB)*100
    dBann$pctInterest  <- (dBann$total_interest_paid  / principalB)*100
    dBann$label        <- paste0(input$labelB, "-Annuity")
    
    dBeq$pctPrincipal  <- (dBeq$total_principal_paid / principalB)*100
    dBeq$pctInterest   <- (dBeq$total_interest_paid  / principalB)*100
    dBeq$label         <- paste0(input$labelB, "-Equal")
    
    combined <- rbind(
      data.frame(month=dAann$month, pctPrincipal=dAann$pctPrincipal,
                 pctInterest=dAann$pctInterest, style=dAann$label),
      data.frame(month=dAeq$month, pctPrincipal=dAeq$pctPrincipal,
                 pctInterest=dAeq$pctInterest, style=dAeq$label),
      data.frame(month=dBann$month, pctPrincipal=dBann$pctPrincipal,
                 pctInterest=dBann$pctInterest, style=dBann$label),
      data.frame(month=dBeq$month, pctPrincipal=dBeq$pctPrincipal,
                 pctInterest=dBeq$pctInterest, style=dBeq$label)
    )
    
    dfLong <- melt(combined, id.vars=c("month","style"),
                   variable.name="Metric", value.name="Value")
    
    color_values <- setNames(c("blue4","blue","red4","red"),
                             c(paste0(input$labelA, "-Annuity"),
                               paste0(input$labelA, "-Equal"),
                               paste0(input$labelB, "-Annuity"),
                               paste0(input$labelB, "-Equal")))
    linetype_values <- setNames(c("solid","dashed","solid","dashed"),
                                c(paste0(input$labelA, "-Annuity"),
                                  paste0(input$labelA, "-Equal"),
                                  paste0(input$labelB, "-Annuity"),
                                  paste0(input$labelB, "-Equal")))
    
    metric_labels <- list(
      "pctPrincipal" = "% of Principal Paid",
      "pctInterest"  = "% of Interest vs Principal"
    )
    
    p <- ggplot(dfLong, aes(x=month, y=Value, color=style, linetype=style)) +
      geom_line(size=1.2) +
      facet_wrap(~Metric, scales="free_y",
                 labeller=as_labeller(metric_labels)) +
      scale_x_continuous(limits=c(0, reactivePlotMonths())) +
      scale_y_continuous(
        breaks=pretty_breaks(n=10),
        labels=function(x) paste0(round(x,0),"%")
      ) +
      scale_color_manual(values=color_values) +
      scale_linetype_manual(values=linetype_values) +
      labs(title=t("tab_compare_mortgage"), x="Month", y="%", color="Style", linetype="Style") +
      theme_bw()
    
    g <- ggplotly(p, tooltip=c("x","y")) %>%
      layout(legend=list(x=1, y=1)) %>%
      config(toImageButtonOptions=list(filename="CompareMortgageStylesPercent"))
    
    no_decimal_plotly(g)
  })
  
  # ------------------------------------------------
  # Download Excel (Parameters + Tables)
  # ------------------------------------------------
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste0("Mortgage_Investment_Simulation_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      
      # 1) Compact Equity + Parameters on same sheet
      addWorksheet(wb, "Compact Equity & Params")
      
      # Prepare parameters as a small data frame
      paramDF <- data.frame(
        Parameter = c(
          "Monthly Income",
          "Monthly Expenses",
          "Annual Mortgage Rate (%)",
          "Annual Investment Return (%)",
          "Annual Property Growth (%)",
          "Property A FullPrice",
          "Property A DownPayment",
          "Property B FullPrice",
          "Property B DownPayment",
          "Rent C InitialRent"
        ),
        Value = c(
          input$monthly_income,
          input$monthly_expenses,
          input$annual_mort_rate,
          input$annual_invest_return,
          input$annual_property_growth,
          input$full_priceA,
          input$down_paymentA,
          input$full_priceB,
          input$down_paymentB,
          input$initial_rentC
        )
      )
      
      writeData(wb, "Compact Equity & Params", paramDF, startRow=1, startCol=1, rowNames=FALSE)
      # Write compact equity table below
      etc <- equityTableCompact()
      if (!is.null(etc)) {
        writeData(wb, "Compact Equity & Params", etc, startRow=nrow(paramDF)+3, startCol=1, rowNames=FALSE)
      }
      
      # 2) Full Equity + Parameters on same sheet
      addWorksheet(wb, "Full Equity & Params")
      writeData(wb, "Full Equity & Params", paramDF, startRow=1, startCol=1, rowNames=FALSE)
      etf <- equityTable()
      if (!is.null(etf)) {
        writeData(wb, "Full Equity & Params", etf, startRow=nrow(paramDF)+3, startCol=1, rowNames=FALSE)
      }
      
      saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
