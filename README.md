# UT Farm Stand Market Analytics Dashboard: Tracking Sales, Attendance, and Volunteer Engagement (2024–2025 Academic Year, Fall 2025 Semester)

## Project Overview
This project was developed as part of my work as **Market Team Manager for UT Farm Stand (UT Austin)**, a student-led organization providing the campus community with affordable, locally sourced produce, baked goods, sustainable living products, and more.

I engineered an **interactive Shiny dashboard** that integrates sales, attendance, and volunteer data across UT Farm Stand’s Fall 2024 - Spring 2025 markets. The tool enables the Market Team to monitor KPIs throughout the year, forecast this data for upcoming Fall 2025 markets, process incoming Fall 2025 market data (recent addition), make data-driven decisions to optimize profitability and affordability, adjust product sourcing and inventory levels, and enhance the overall market experience.

**Live App:** [UTFS Market Analytics Dashboard on shinyapps.io](https://quinnhungerford.shinyapps.io/utfs_app_deploy/)  

---

# Problem Statement

UT Farm Stand organizes biweekly farmers markets throughout the semester. Historically, decisions around ordering, staffing, and outreach relied on isolated, single-use spreadsheets which made it difficult to achieve our sustainability, __, and profitability goals. We needed a tool that could:
- **Monitor All Market KPIs:** Track sales performance, customer attendance, and volunteer engagement in one dashboard.
- **Summarize Trends:** Highlight category- and product-level sales, account for seasonal and time-based attendance shifts, and measure volunteer retention.
- **Support Future Market Decision-Making:** Generate forecasts of total items sold, revenue, volunteer support, attendance, and more for each future market, based on historical patterns, to guide important decisions about market purchasing, pricing, product quantity, community/vendor engagement, new product rollout, etc.
- **Enhance Efficiency:** Replace manual reporting with automated pipelines that reduce time spent on data entry and analysis.

---

# Tech Stack
- R (Core programming language)
- Shiny (Interactive dashboard framework)
- dplyr, tidyr (Data wrangling and cleaning)
- ggplot2, plotly (Data visualization and interactivity)
- DT (Interactive tables)
- readxl, lubridate (Excel data import and date handling)
- rsconnect (Deployment to shinyapps.io)

---

# Data Sources

The dashboard integrates UT Farm Stand’s internal Excel workbooks, which I designed in Fall 2024 based on both the key performance indicators we wanted to track moving forward and the historical data points recorded in earlier spreadsheets. **This public repo includes only dummy datasets for reproducibility.**
- Sales_Performance_Past.xlsx and Sales_Performance_2526.xlsx
  - Per-Market Sheets: Sales_YYYY-MM-DD (item-level rows: Item Number, Item Name, Category, Number Sold, Gross Sales)
  - Summary Sheet: Sales_Summary_2425 (Market Date, Total Items Sold, Total Gross Sales, Average Transaction Value)
- Market_Attendance_Past.xlsx and Market_Attendance_2526.xlsx
  - Single Sheet: Market Date, Total Customer Attendance, Average Temperature, Weather Condition, Average Temperature, Number of Tabling Organizations, Number of Vendors, Vendor Names, Notes
- Volunteer_Engagement_Past.xlsx and Volunteer_Engagement_2526.xlsx
  - Per-Market Sheets: Volunteer_Engagement_YYYY-MM-DD (Volunteer Name, Hours Worked)
  - *Privacy Note: Volunteer names are anonymized to initials in the app with safe_initials().*

---

# Approach & Dashboard Features

**Data Processing**
- Excel Integration: Integrates six workbooks (Sales_Performance_Past.xlsx, Sales_Performance_2526.xlsx, Market_Attendance_Past.xlsx, Market_Attendance_2526.xlsx, Volunteer_Engagement_Past.xlsx, Volunteer_Engagement_2526.xlsx).
- Automated ETL: Standardizes column names, formats dates and numbers, and anonymizes volunteer names into initials.
- Dynamic Updates: Automatically detects new sheets so the dashboard grows as new market data is added.

**Core Dashboard Functions/Tabs**
All components are interactive, with filtering, selection tools, and hover tooltips.
- **Table View**: Clean, searchable tables for each dataset with consistent formatting.
- **Summary Statistics**: Aggregated KPIs for single markets and/or across all markets:
  - Volunteer Engagement: Total volunteers and total hours worked per market and across all markets.
  - Market Attendance: Total/average customers, average temperature, total/average tabling organizations, and total/average vendors across all markets.
  - Sales Performance:
      - Sales Summary: Total/average items sold, total/average gross sales, average transaction value across all markets.
      - Per-Market (Sales YYYY-MM-DD): Breakdown of total items sold and gross sales by product category for that market.
- **Retention Analysis** (Volunteer Engagement only):
  - Pie chart showing the percentage of volunteers who returned across multiple markets.
- **Forecasting (Fall 2025 Markets)**: Generated by aligning each upcoming market date with the nearest historical market date from the prior year.
  - Volunteer Engagement: Expected volunteers and hours for each future market.
  - Market Attendance: Expected number of customers, tabling organizations, vendors + typical weather conditions for each future market.
  - Sales Performance:
      - Sales Summary: Expected total items sold, gross sales, and average transaction value for each future market.
      - Sales YYYY-MM-DD Market: Expected breakdown of total items sold and gross sales by product category for each future market.

---

# Repository Contents
- `code/`
    - `app.R` → Main Shiny app script
- `data/` (All dummy datasets)
  - `Sales_Performance_Past.xlsx` and `Sales_Performance_2526.xlsx` → Excel workbooks with per-market sales data sheets (Sales_YYYY-MM-DD) and a summary sheet (Sales_Summary_2425, Sales_Summary_2526) for the 25-26 academic year and most recent Fall 2025 semester
  - `Market_Attendance_Past.xlsx` and `Market_Attendance_2526.xlsx` → Single-sheet excel workbooks with market attendance information
  - `Volunteer_Engagement_Past.xlsx` and `Volunteer_Engagement_2526.xlsx` → Excel workbook with per-market volunteer data sheets (Volunteer_Engagement_YYYY-MM-DD)
- `presentation/`
  - `dashboard_demo.mp4` → Walkthrough video of the dashboard in use
  - `UTFS_Market_Summary_Report_2024_2025.pdf` → End-of-year summary report for 25-26 academic year of market KPIs written using this dashboard

---

# Reproducibility
To reproduce this dashboard:
- Clone this repository and install the required R packages:
  - install.packages(c("shiny", "readxl", "dplyr", "ggplot2", "plotly", "DT", "tidyr", "lubridate", "rsconnect"))
- Use the three dummy Excel workbooks provided in the /data folder.
- Launch the dashboard locally with shiny::runApp("code/app.R").
- Or view the live deployed app here: [UTFS Market Analytics Dashboard on shinyapps.io](https://quinnhungerford.shinyapps.io/utfs_market_data/)  

---

*This project was independently designed and developed by Quinn Hungerford as part of my work as Market Team Manager for UT Farm Stand (UT Austin). The dashboard actively supports the data-driven decision making of myself and my team, helping ensure the markets remain sustainable, financially feasible, and impactful so we can better serve the student community at UT Austin.*
