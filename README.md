# Mortgage and Investment Simulator

This project is a financial simulation tool built in **R** using the Shiny framework. It allows users to compare three options for managing finances over a 30-year period:

1. **Option A**: Buy Property 1 (with a mortgage) and invest leftover funds.
2. **Option B**: Buy Property 2 (with a mortgage) and invest leftover funds.
3. **Option C**: Rent a property and invest leftover funds.

---

## Features

- **Dynamic Simulations**:
  - Mortgage payments (annuity or equal principal).
  - Property appreciation.
  - Investment growth (with customizable returns).
  - Rent increases for renting scenarios.

- **Customizable Inputs**:
  - Income, expenses, mortgage rates, property growth rates, and investment returns.
  - Options for property price, down payment, and mortgage type (annuity or equal principal).

- **Outputs**:
  - Equity table summarizing key metrics (e.g., total equity, property value, investment gains).
  - Interactive plots for:
    - Total equity over time.
    - Cumulative principal and interest paid.
    - Property value growth.
    - Investment and property gains.

---

## Formulas Used

### 1. Monthly Interest Rate

$$
r = \frac{\text{Annual Rate}}{12}
$$

### 2. Monthly Mortgage Payment (Annuity)

$$ 
M = P \times \frac{r}{1 - (1 + r)^{-n}} 
$$

**Where:**
- \( M \): Monthly payment  
- \( P \): Principal (loan amount)  
- \( r \): Monthly interest rate  
- \( n \): Total number of payments (\( \text{years} \times 12 \))

---

### 3. Investment Growth

$$ 
Investment Balance_m = Investment Balance_{m-1} * (1 + r) + Monthly Contribution
$$

**Where:**
 - \( Investment Balance_m \):  Investment balance at month m
 - \( r \):  Monthly investment return rate
 - \( Monthly Contribution \):  Amount added to the investment each month
---


### 4. Property Appreciation

$$ 
Property Value_m = Property Value_{m-1} * (1 + r)
$$

**Where:**
 - \( Property Value_m \):  Property value at month m
 - \( r \):  Monthly property appreciation rate


## Code Structure

### **1. Helper Functions**
- **`monthly_interest_rate`**: Converts annual rates to monthly rates.
- **`annuity_monthly_payment`**: Calculates monthly payments for annuity-style mortgages.
- **`simulate_mortgage_and_investing`**: Simulates financial metrics for buying a property and investing.
- **`simulate_renting_and_investing`**: Simulates financial metrics for renting and investing.

### **2. User Interface (UI)**
- Sidebar for input parameters:
  - Monthly income, expenses, mortgage rates, and terms.
  - Property-specific parameters (price, down payment, mortgage type).
  - Rent-specific parameters (initial rent, annual rent increases).
- Main panel displays:
  - Equity table (summarized at intervals).
  - Plots for equity, property value, and detailed gains.

### **3. Server Logic**
- **Reactive Values**:
  - `common_pool`: Monthly available budget.
  - Option-specific calculations for mortgage payments, rent, and investments.
- **Simulations**:
  - Uses helper functions to generate financial data for all options.
- **Output Rendering**:
  - Equity table and plots (using `ggplot2` and `plotly`).

---

## Example Interactive Features

1. Adjust the **annual mortgage rate** to see its impact on monthly payments and equity growth.
2. Compare total equity after 30 years for buying vs. renting under different **investment return rates**.
3. Explore how **annual property growth rates** affect long-term equity in Options A and B.

---

## Plots Included
1. **Total Equity Over Time**: Shows combined equity from property and investments.
2. **Principal & Interest Paid**: Tracks cumulative amounts paid toward the mortgage.
3. **Property Value Growth**: Visualizes the appreciation of properties.
4. **Detailed Gains & Contributions**: Breaks down contributions, gains, and equity components.

---

## How to Run
1. Install required libraries: `shiny`, `ggplot2`, `reshape2`, `scales`, `plotly`.
2. Save the script as `app.R`.
3. Run `shiny::runApp("app.R")` in RStudio or R.

---

This project provides a clear, interactive way to evaluate financial decisions for buying, renting, or investing. Adjust inputs, compare options, and visualize long-term impacts in real-time!
