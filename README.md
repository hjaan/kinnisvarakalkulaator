# Mortgage and Investment Simulator

This project is a comprehensive financial simulation tool developed in **R** using the Shiny framework. It allows users to compare three financial options over a 30-year timeline:

1. **Option A**: Purchase Property 1 (with a mortgage) and invest leftover funds.
2. **Option B**: Purchase Property 2 (with a mortgage) and invest leftover funds.
3. **Option C**: Rent a property and invest leftover funds.

---

## Features

### **Dynamic Simulations**:
- Mortgage payments using **annuity** or **equal principal** methods.
- Property value appreciation over time.
- Investment growth with customizable rates of return.
- Rent growth for renting scenarios.

### **Customizable Inputs**:
- User-defined income, expenses, mortgage interest rates, property appreciation rates, and investment returns.
- Flexibility in setting property prices, down payments, and mortgage types (annuity or equal principal).
- Adjustable rent parameters, including initial rent and annual rent growth.

### **Interactive Outputs**:
- Equity table summarizing key financial metrics for each option:
  - Total equity.
  - Investment balance and gains.
  - Property value and real estate equity.
  - Cumulative principal and interest payments.
- Graphical representations, including:
  - Total equity over time.
  - Cumulative principal and interest paid.
  - Property value growth.
  - Detailed gains from investments and property appreciation.

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


---

## Code Structure

### **1. Helper Functions**
- **`monthly_interest_rate`**: Calculates the monthly interest rate from an annual rate.
- **`annuity_monthly_payment`**: Computes the monthly payment for an annuity-style mortgage.
- **`simulate_mortgage_and_investing`**: Simulates financial outcomes for property purchase and investment scenarios.
- **`simulate_renting_and_investing`**: Simulates financial outcomes for renting and investing scenarios.

### **2. User Interface (UI)**
- **Input Parameters**:
  - Adjustable sliders and inputs for income, expenses, mortgage rates, property growth rates, and investment returns.
  - Specific parameters for property and rent options.
- **Outputs**:
  - Interactive equity tables and plots for financial comparisons.
  - Clear explanation of columns and metrics.

### **3. Server Logic**
- **Reactive Calculations**:
  - Allocates monthly income to mortgage, rent, and investments dynamically.
- **Simulations**:
  - Generates detailed financial data for each option using helper functions.
- **Output Rendering**:
  - Produces dynamic tables and interactive plots using `ggplot2` and `plotly`.

---

## Plots Included

1. **Total Equity Over Time**: Tracks the combined equity from property and investments for each option.
2. **Cumulative Principal and Interest Paid**: Visualizes the mortgage payments over time.
3. **Property Value Growth**: Displays the appreciation of property values.
4. **Detailed Gains**: Breaks down investment contributions, gains, and real estate equity.
5. **Compare Mortgage Styles**: Contrasts annuity and equal principal mortgage methods.

---

## Example Use Cases

1. Compare long-term equity outcomes for buying vs. renting under different economic conditions.
2. Analyze how varying property appreciation rates impact overall wealth.
3. Visualize differences in total interest paid between annuity and equal principal mortgages.

---

## How to Run

1. Install the required libraries: `shiny`, `ggplot2`, `reshape2`, `scales`, `plotly`.
2. Save the code as `app.R`.
3. Run the application using the command:  
   ```R
   shiny::runApp("app.R")
   ```

---

This tool offers a clear, interactive approach to evaluate financial decisions related to property, mortgages, and investments. With adjustable parameters and real-time visualizations, it provides valuable insights for long-term financial planning.
