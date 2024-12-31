Here's a detailed GitHub-style explanation of the code, including the key formulas, assumptions, and decisions made in the implementation:

---

# Mortgage and Investment Simulation App

This Shiny application simulates the financial outcomes of three scenarios: 
- **Option A**: Buying a 3-room property.
- **Option B**: Buying a 2-room property.
- **Option C**: Renting and investing.

The app calculates and visualizes the growth of investments, property values, mortgage balances, and total equity over time, enabling comparisons between options.

---

## Key Concepts and Formulas

### Mortgage Payments
The app uses two types of mortgage payment styles:
1. **Annuity Mortgage**: Equal monthly payments throughout the term.
   - **Formula**:  
     \[
     M = P \times \frac{r}{1 - (1 + r)^{-n}}
     \]
     Where:
     - \( M \): Monthly payment.
     - \( P \): Principal (loan amount).
     - \( r \): Monthly interest rate (\(\text{annual rate} / 12\)).
     - \( n \): Total number of payments (\( \text{years} \times 12 \)).

2. **Equal Principal Payment**: Fixed principal payment + decreasing interest.
   - Principal Payment: \( P / n \)
   - Monthly Payment: Principal Payment + Interest Payment (\( P_{\text{current}} \times r \)).

These formulas ensure accurate modeling of mortgage payments over time, accounting for varying interest and principal proportions.

---

### Investment Growth
Investments grow monthly, compounding with the assumed annual return rate:
\[
I_{\text{next}} = I_{\text{current}} \times (1 + r_i) + \text{monthly contribution}
\]
Where:
- \( r_i \): Monthly investment return (\(\text{annual rate} / 12\)).

Cumulative contributions and investment gains are tracked separately to distinguish between principal contributions and the growth due to compounding.

---

### Property Value Growth
Properties appreciate over time using the annual growth rate:
\[
V_{\text{next}} = V_{\text{current}} \times (1 + r_p)
\]
Where:
- \( r_p \): Monthly property growth rate (\(\text{annual growth rate} / 12\)).

---

### Total Equity
Equity is the sum of:
1. **Real Estate Equity**: \( \text{property value} - \text{principal owed} \).
2. **Investment Balance**: \( \text{investment contributions} + \text{investment gains} \).

---

## App Features

### UI (User Interface)
- **Input Parameters**: 
  - Interest rates, property growth rates, and investment returns.
  - Property prices, down payments, and investable income for each option.
  - Mortgage term and analysis intervals for equity comparisons.
- **Outputs**:
  - Equity Table: Tabular comparison of equity at selected intervals.
  - Plots:
    - Investment Balances.
    - Mortgage Principal Owed.
    - Property Value Growth.
    - Monthly Cash Flows.
    - Detailed Gains & Contributions.

---

## Server-Side Logic

### Simulations
1. **Buying Options (A & B)**:
   - Simulates the mortgage payments and investments made with leftover income.
   - Tracks monthly:
     - Mortgage payments (principal + interest).
     - Investment balances (cumulative contributions and gains).
     - Property values (with appreciation).
     - Real estate equity (\( \text{property value} - \text{principal owed} \)).

2. **Renting & Investing (Option C)**:
   - Models direct investments with the provided monthly contribution.
   - Tracks:
     - Investment balances.
     - Gains and contributions.

---

### Reactive Outputs
- **Equity Table**:
  - Compares the total equity for all options at specified intervals.
  - Separates real estate equity, investment balances, and total gains.
- **Plots**:
  - Visualize cash flow, balances, property values, and detailed gains over time.

---

## Visualization
Plots leverage **ggplot2** to create clean, visually appealing representations of data:
- **Line Charts** for continuous growth (e.g., investments, property value).
- **Faceted Charts** to show multiple metrics in a single view (e.g., gains, contributions).

---

## Assumptions
- Constant annual rates for mortgage, investment, and property growth.
- Down payments and contributions are fixed at the start.
- No tax, fees, or transaction costs considered.

---

## Code Design
- **Modular Functions**: Helper functions for recurring calculations (e.g., annuity payments, monthly interest).
- **Reactivity**: Allows real-time updates as inputs change.
- **Error Handling**: Alerts for invalid inputs (e.g., down payments exceeding property price).

---

## How to Use
1. Input the financial parameters and assumptions.
2. Adjust the sliders for mortgage term and analysis intervals.
3. View and compare results in tables and plots.

This tool enables users to make informed financial decisions about buying or renting.

--- 

## Future Enhancements
- Include fees, taxes, and insurance for realistic modeling.
- Add sensitivity analysis for variable interest or growth rates.
- Integrate downloadable reports for the results.

---

This code provides a solid foundation for evaluating mortgage and investment scenarios while being easy to extend for advanced use cases.
