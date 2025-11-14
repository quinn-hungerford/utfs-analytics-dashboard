# Load libraries
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(lubridate)
library(rsconnect)

# List of upcoming markets
UPCOMING_MARKETS_2526 <- as.Date(c("2025-09-10","2025-09-24","2025-10-08","2025-10-29","2025-11-12"))

# Helper for loading in Excel workbooks
load_data <- function(file, sheet = NULL) {
  if (is.null(sheet)) read_excel(file) else read_excel(file, sheet = sheet)
}

# Helper for finding the nearest past market date to a future date
nearest_row <- function(df, date_col, target_date) {
  df %>%
    mutate(.abs_diff_days = abs(as.integer(difftime(.data[[date_col]], target_date, units = "days")))) %>%
    arrange(.abs_diff_days) %>%
    slice(1)
}

# Helper for converting names to initials (volunteer confidentiality in demo video)
safe_initials <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  sapply(x, function(n) {
    if (is.na(n)) return(NA_character_)
    parts <- unlist(strsplit(n, "\\s+"))
    first <- substr(parts[1], 1, 1)
    last <- if (length(parts) >= 2) substr(parts[length(parts)], 1, 1) else ""
    toupper(paste0(first, last))
  }, USE.NAMES = FALSE)
}

# Helper for combining all sheets (used for Volunteer Engagement stats)
read_all_sheets <- function(path) {
  sh <- readxl::excel_sheets(path)
  if (length(sh) == 0) return(tibble())
  bind_rows(lapply(sh, function(s) {
    df <- readxl::read_excel(path, sheet = s)
    # Standardize columns
    nm <- names(df)
    names(df)[match("Volunteer Name", nm, nomatch = 0)] <- "Volunteer Name"
    names(df)[match("Hours Worked", nm, nomatch = 0)] <- "Hours Worked"
    # Anonymize names with helper safe_initials() function
    if ("Volunteer Name" %in% names(df)) {
      df[["Volunteer Name"]] <- safe_initials(df[["Volunteer Name"]])
    }
    # Access market date from sheet name
    date_str <- sub(".*?(\\d{4})[-_](\\d{2})[-_](\\d{2}).*$", "\\1-\\2-\\3", s)
    market_dt <- suppressWarnings(as.Date(date_str))
    # Check that Market Date column exists and is a date type
    if (!"Market Date" %in% names(df)) {
      df[["Market Date"]] <- market_dt
    } else {
      df[["Market Date"]] <- as.Date(df[["Market Date"]])
      df[["Market Date"]][is.na(df[["Market Date"]])] <- market_dt
    }
    # Make sure that Hours Worked are all numeric
    if ("Hours Worked" %in% names(df)) {
      df[["Hours Worked"]] <- suppressWarnings(as.numeric(df[["Hours Worked"]]))
    }
    df
  }))
}

# Creating user interface
ui <- fluidPage(
  div(img(src = "UTFSPageHeader.png", style = "width: 100%;")),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "acadYear", "Academic Year:",
        choices = c("2024–25", "2025–26"),
        selected = "2025–26"
      ),
      selectInput("dataset", "Select a Dataset:",
                  choices = c("Volunteer Engagement", "Market Attendance", "Sales Performance")),
      uiOutput("salesDateSelector"),
      uiOutput("veDateSelector"),
      img(src = "UTFSFranny.png", style = "width: 100%; margin-top: 20px;")
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Table View", uiOutput("tableHeader"), DT::dataTableOutput("dataTable")),
                  tabPanel("Summary Statistics", uiOutput("summaryHeader"), DT::dataTableOutput("summaryStatsTable")),
                  tabPanel("Retention Analysis", tags$br(), plotlyOutput("retentionPlot")),
                  tabPanel("Forecasting", tags$br(), uiOutput("forecastHeader"), DT::dataTableOutput("forecastTable"))
      )
    )
  )
)

# Creating server
server <- function(input, output, session) {
  observe({
    if (input$dataset == "Volunteer Engagement") showTab("tabs", "Retention Analysis")
    else hideTab("tabs", "Retention Analysis")
  })
  
  # Reactive helper functions
  year_slug <- reactive({
    if (input$acadYear == "2024–25") "Past" else "2526"
  })
  ve_path <- reactive(paste0("Volunteer_Engagement_", year_slug(), ".xlsx"))
  attn_path <- reactive(paste0("Market_Attendance_", year_slug(), ".xlsx"))
  sales_path <- reactive(paste0("Sales_Performance_", year_slug(), ".xlsx"))
  
  prior_year_sales_summary <- reactive({
    # prior year is 24–25 for your Fall 25 forecasts
    load_data("Sales_Performance_Past.xlsx", sheet = "Sales_Summary_2425") |>
      mutate(`Market Date` = as.Date(`Market Date`))
  })
  prior_year_sales_sheet <- function(d) paste0("Sales_", format(d, "%Y-%m-%d"))
  
  # Route the data
  dataset <- reactive({
    if (input$dataset == "Sales Performance") {
      req(input$salesDate)
      load_data(sales_path(), sheet = input$salesDate)
    } else if (input$dataset == "Volunteer Engagement") {
      read_all_sheets(ve_path())
    } else {
      load_data(attn_path())
    }
  })
  
  # Create selector for Volunteer Engagement market date (changes table view only)
  output$veDateSelector <- renderUI({
    if (input$dataset == "Volunteer Engagement") {
      sheets <- tryCatch(readxl::excel_sheets(ve_path()), error = function(e) character(0))
      raw_labels <- sub(".*?(\\d{4})[-_](\\d{2})[-_](\\d{2}).*$", "\\1-\\2-\\3", sheets)
      df_choices <- data.frame(sheet = sheets, label = gsub("_","-", raw_labels), stringsAsFactors = FALSE)
      df_choices$label_date <- suppressWarnings(as.Date(df_choices$label))
      df_choices <- df_choices[!is.na(df_choices$label_date), ]
      df_choices <- df_choices[order(df_choices$label_date), ]
      choice_vec <- stats::setNames(df_choices$sheet, as.character(df_choices$label_date))
      default_choice <- if (nrow(df_choices)) df_choices$sheet[nrow(df_choices)] else NULL
      selectInput("veSheet", "Select a Market Date (Table View Only):",
                  choices = choice_vec, selected = default_choice)
    }
  })
  
  # Create selector for Sales Performance market date
  output$salesDateSelector <- renderUI({
    if (input$dataset == "Sales Performance") {
      sheets <- tryCatch(readxl::excel_sheets(sales_path()), error = function(e) character(0))
      date_sheets <- grep("^Sales_\\d{4}[-_]\\d{2}[-_]\\d{2}$", sheets, value = TRUE)
      raw_labels <- sub("^Sales_(\\d{4})[-_](\\d{2})[-_](\\d{2})$", "\\1-\\2-\\3", date_sheets)
      df_choices <- data.frame(sheet = date_sheets, label = gsub("_","-", raw_labels), stringsAsFactors = FALSE)
      df_choices$label_date <- suppressWarnings(as.Date(df_choices$label))
      df_choices <- df_choices[!is.na(df_choices$label_date), ]
      df_choices <- df_choices[order(df_choices$label_date), ]
      date_choice_vec <- stats::setNames(df_choices$sheet, as.character(df_choices$label_date))
      summary_key <- if (input$acadYear == "2024–25") "Sales_Summary_2425" else "Sales_Summary_2526"
      summary_lab <- if (input$acadYear == "2024–25") "24–25 Sales Summary" else "25–26 Sales Summary"
      selectInput("salesDate", "Select a Market Date or View Year Summary:",
                  choices = c(setNames(summary_key, summary_lab), date_choice_vec),
                  selected = summary_key)
    }
  })
  
  # Creating table view output
  output$dataTable <- DT::renderDataTable({
    # Volunteer Engagement table view
    if (input$dataset == "Volunteer Engagement") {
      req(input$veSheet)
      df <- load_data(ve_path(), sheet = input$veSheet)
      # Anonymizes names with helper function
      if ("Volunteer Name" %in% names(df)) {
        df[["Volunteer Name"]] <- safe_initials(df[["Volunteer Name"]])
      }
      if ("Date" %in% names(df)) df$Date <- as.Date(df$Date)
      if ("Market Date" %in% names(df)) df$`Market Date` <- format(as.Date(df$`Market Date`), "%Y-%m-%d")
      if ("Market Date Attended" %in% names(df)) df$`Market Date Attended` <- format(as.Date(df$`Market Date Attended`), "%Y-%m-%d")
      if ("Hours Worked" %in% names(df)) df$`Hours Worked` <- suppressWarnings(as.numeric(df$`Hours Worked`))
      datatable(
        df, class = "stripe hover compact",
        options = list(pageLength = 10, autoWidth = TRUE, dom = 'ftipr'),
        rownames = FALSE
      )
      # Market Attendance table view
    } else if (input$dataset == "Market Attendance") {
      attn_all <- load_data(attn_path()) %>% mutate(`Market Date` = as.Date(`Market Date`))
      if ("Other Factors" %in% names(attn_all)) {
        attn_all <- attn_all %>% rename(Notes = `Other Factors`)
      }
      wanted <- c("Market Date", "Total Customer Attendance", "Average Temperature",
                  "Weather Condition", "Number of Tabling Organizations",
                  "Number of Vendors", "Notes")
      cols <- intersect(wanted, names(attn_all))
      attn_all$`Market Date` <- format(attn_all$`Market Date`, "%Y-%m-%d")
      datatable(
        attn_all[cols], class = "stripe hover compact",
        options = list(dom = "tip", pageLength = 10, autoWidth = TRUE),
        rownames = FALSE
      )
      # Sales Performance, Sales Summary table view
    } else if (input$dataset == "Sales Performance" && grepl("Sales_Summary_", input$salesDate)) {
      df <- dataset() %>% mutate(`Market Date` = as.Date(`Market Date`))
      datatable(
        df, class = "stripe hover compact",
        options = list(dom = "tip", pageLength = 20, autoWidth = TRUE),
        rownames = FALSE
      ) %>%
        formatCurrency("Total Gross Sales", currency = "$", interval = 3, mark = ",", digits = 2) %>%
        formatCurrency("Average Transaction Value", currency = "$", interval = 3, mark = ",", digits = 2) %>%
        formatCurrency("Total Items Sold", currency = "", interval = 3, mark = ",", digits = 0)
      # Sales Performance, Specific Market Date table view
    } else {
      df <- dataset()
      if ("Date" %in% names(df)) df$Date <- as.Date(df$Date)
      if ("Market Date" %in% names(df)) df$`Market Date` <- format(as.Date(df$`Market Date`), "%Y-%m-%d")
      if ("Market Date Attended" %in% names(df)) df$`Market Date Attended` <- format(as.Date(df$`Market Date Attended`), "%Y-%m-%d")
      dt <- datatable(
        df, class = "stripe hover compact",
        options = list(
          dom = 'ftipr', pageLength = 10, autoWidth = TRUE
        ),
        rownames = FALSE
      )
      if ("Number Sold" %in% names(df)) {
        dt <- dt %>% DT::formatCurrency("Number Sold", currency = "", digits = 0, mark = ",") %>%
          DT::formatStyle("Number Sold", `text-align` = "right")
      }
      if ("Gross Sales" %in% names(df)) {
        dt <- dt %>% DT::formatCurrency("Gross Sales", currency = "$", digits = 2, mark = ",") %>%
          DT::formatStyle("Gross Sales", `text-align` = "right")
      }
      return(dt)
    }
  })
  
  # Creating descriptive headers for each of the forecasting sections
  output$forecastHeader <- renderUI({
    if (input$dataset == "Volunteer Engagement") {
      h4("Volunteer Engagement Forecast for Fall 2025 Markets")
    } else if (input$dataset == "Market Attendance") {
      h4("Market Attendance Forecast for Fall 2025 markets")
    } else {
      is_summary <- !is.null(input$salesDate) && grepl("Summary", input$salesDate)
      if (is_summary) {
        h4("Sales Forecast (Summary) for Fall 2025 Markets")
      } else {
        h4("Sales Forecast (Per-Market Category Sales) for Fall 2025 Markets")
      }
    }
  })
  
  # Creating descriptive headers for each of the table view sections
  output$tableHeader <- renderUI({
    if (input$dataset == "Volunteer Engagement") h4("Volunteer Engagement Per Market Table View")
    else if (input$dataset == "Market Attendance") h4("Market Attendance Per Market Table View")
    else h4("Sales Performance Per Market Table View")
  })
  
  # Creating descriptive headers for each of those summary statistics sections
  output$summaryHeader <- renderUI({
    if (input$dataset == "Volunteer Engagement") h4("Volunteer Engagement Across All Markets Summary Statistics")
    else if (input$dataset == "Market Attendance") h4("Market Attendance Across All Markets Summary Statistics")
    else h4("Sales Performance Across All Markets Summary Statistics")
  })
  
  # Creating the forecast tables
  output$forecastTable <- DT::renderDataTable({
    req(input$dataset)
    # Forecasting volunteer engagement for Fall 2025
    if (input$dataset == "Volunteer Engagement") {
      ve <- dataset() %>% mutate(`Market Date` = as.Date(`Market Date`))
      df <- lapply(UPCOMING_MARKETS_2526, function(target_date) {
        prior_year_target <- target_date %m-% years(1)
        row_near <- nearest_row(ve, "Market Date", prior_year_target)
        date_match <- row_near$`Market Date`[1]
        slice_day <- ve %>% filter(`Market Date` == date_match)
        tibble(
          `Future Market` = target_date,
          `Expected Volunteers` = n_distinct(slice_day$`Volunteer Name`),
          `Expected Hours` = sum(slice_day$`Hours Worked`, na.rm = TRUE)
        )
      }) %>% bind_rows()
      datatable(df, class = "stripe hover compact",
                options = list(pageLength = 10, autoWidth = TRUE, dom = 'tip'),
                rownames = FALSE)
      # Forecasting market attendance for Fall 2025
    } else if (input$dataset == "Market Attendance") {
      attn <- dataset() %>% mutate(`Market Date` = as.Date(`Market Date`))
      df <- lapply(UPCOMING_MARKETS_2526, function(target_date) {
        prior_year_target <- target_date %m-% years(1)
        row_near <- nearest_row(attn, "Market Date", prior_year_target)
        tibble(
          `Future Market` = target_date,
          `Expected Customers` = row_near$`Total Customer Attendance`,
          `Expected Tabling Organizations` = row_near$`Number of Tabling Organizations`,
          `Expected Vendors` = if ("Number of Vendors" %in% names(row_near)) row_near$`Number of Vendors` else NA_real_,
          `Expected Weather` = if ("Weather Condition" %in% names(row_near)) row_near$`Weather Condition` else NA,
          `Expected Avg Temp` = if ("Average Temperature" %in% names(row_near)) row_near$`Average Temperature` else NA
        )
      }) %>% bind_rows()
      datatable(df, class = "stripe hover compact",
                options = list(dom = "tip", pageLength = 10, autoWidth = TRUE),
                rownames = FALSE)
      # Forecasting sales performance for Fall 2025 (summary page, general forecast)
    } else if (input$dataset == "Sales Performance") {
      sales <- dataset()
      # Checking if it is the sales summary sheet
      is_summary <- !is.null(input$salesDate) && grepl("Summary", input$salesDate)
      if (is_summary) {
        sales_summary <- prior_year_sales_summary() %>% mutate(`Market Date` = as.Date(`Market Date`))
        df <- lapply(UPCOMING_MARKETS_2526, function(target_date) {
          prior_year_target <- target_date %m-% years(1)
          row_near <- nearest_row(sales_summary, "Market Date", prior_year_target)
          tibble(
            `Future Market` = target_date,
            `Expected Total Items Sold` = suppressWarnings(as.numeric(row_near$`Total Items Sold`)),
            `Expected Gross Sales` = suppressWarnings(as.numeric(row_near$`Total Gross Sales`)),
            `Expected Average Transaction Value` = suppressWarnings(as.numeric(row_near$`Average Transaction Value`))
          )
        }) %>% bind_rows()
        DT::datatable(
          df, class = "stripe hover compact",
          options = list(dom = "tip", pageLength = 10, autoWidth = TRUE),
          rownames = FALSE
        ) %>% DT::formatCurrency("Expected Gross Sales", "$", interval = 3, mark = ",", digits = 2) %>%
          DT::formatCurrency("Expected Average Transaction Value", "$", interval = 3, mark = ",", digits = 2) %>%
          DT::formatCurrency("Expected Total Items Sold", "", interval = 3, mark = ",", digits = 0)
        # Forecasting sales performance for Fall 2025 (market page, category forecast)
      } else {
        sales_summary <- prior_year_sales_summary() %>% mutate(`Market Date` = as.Date(`Market Date`))
        get_sheet_name_for_date <- function(d) paste0("Sales_", format(d, "%Y-%m-%d"))
        rows <- lapply(UPCOMING_MARKETS_2526, function(target_date) {
          prior_year_target <- target_date %m-% years(1)
          near_row <- nearest_row(sales_summary, "Market Date", prior_year_target)
          nearest_date <- near_row$`Market Date`[1]
          nearest_sheet <- get_sheet_name_for_date(nearest_date)
          per_market <- tryCatch(
            load_data("Sales_Performance_Past.xlsx", sheet = prior_year_sales_sheet(nearest_date)),
            error = function(e) NULL
          )
          if (is.null(per_market)) {
            return(tibble(
              `Future Market` = target_date,
              Category = NA_character_,
              `Expected Total Items Sold` = NA_real_,
              `Expected Gross Sales` = NA_real_
            ))
          }
          per_market %>% group_by(Category) %>%
            summarise(
              `Expected Total Items Sold` = sum(`Number Sold`, na.rm = TRUE),
              `Expected Gross Sales` = sum(`Gross Sales`, na.rm = TRUE),
              .groups = "drop"
            ) %>% mutate(`Future Market` = target_date) %>%
            select(`Future Market`, Category, `Expected Total Items Sold`, `Expected Gross Sales`)
        })
        df <- bind_rows(rows)
        requested_order <- c("Added-Value Products","Bargain Bag","Bread","Hibiscus Tea",
                             "Honey","Jester Produce","Merch","Pastry","Produce","Sustainable Living")
        df$Category <- factor(df$Category,
                              levels = unique(c(requested_order, setdiff(df$Category, requested_order))))
        df <- df %>% arrange(`Future Market`, Category)
        DT::datatable(
          df, extensions = "RowGroup",
          class = "stripe hover compact",
          options = list(
            dom = "tip", pageLength = 50,
            order = list(list(0, "asc"), list(1, "asc")),
            rowGroup = list(dataSrc = 0),
            columnDefs = list(list(visible = FALSE, targets = 0))
          ),
          rownames = FALSE
        ) |> DT::formatCurrency("Expected Gross Sales", currency = "$",
                                interval = 3, mark = ",", digits = 2) |>
          DT::formatRound("Expected Total Items Sold", digits = 0,
                          interval = 3, mark = ",")
      }
    }
  })
  
  # Creating the volunteer retention plot
  output$retentionPlot <- renderPlotly({
    data <- dataset()
    if (input$dataset == "Volunteer Engagement") {
      validate(
        need("Volunteer Name" %in% names(data), "Volunteer Name column not found"),
        need("Market Date" %in% names(data), "Market Date column not found")
      )
      data$`Market Date` <- as.Date(data$`Market Date`)
      retention_data <- data %>% group_by(`Volunteer Name`) %>%
        summarise(First_Market = min(`Market Date`), Last_Market = max(`Market Date`)) %>%
        mutate(Returned = ifelse(First_Market != Last_Market, "Returning", "Not Returning"))
      plot_data <- retention_data %>% count(Returned)
      utfs_colors <- c("Returning" = "#F28E2B", "Not Returning" = "#4E79A7")
      plot_ly(plot_data, labels = ~Returned, values = ~n, type = 'pie',
              textinfo = 'label+percent',
              marker = list(colors = utfs_colors[plot_data$Returned])) %>%
        layout(title = "Percentage of Returning Volunteers", showlegend = FALSE)
    }
  })
  
  # Creating the summary statistics table
  output$summaryStatsTable <- DT::renderDataTable({
    data <- dataset()
    
    # Summary statistics on volunteer engagement (24-25)
    if (input$dataset == "Volunteer Engagement") {
      validate(
        need("Market Date" %in% names(data), "Market Date column not found"),
        need("Volunteer Name" %in% names(data), "Volunteer Name column not found"),
        need("Hours Worked" %in% names(data), "Hours Worked column not found")
      )
      data$`Market Date` <- as.character(as.Date(data$`Market Date`))
      stats <- data %>%
        dplyr::group_by(`Market Date`) %>%
        dplyr::summarise(
          `Total Volunteers` = dplyr::n_distinct(`Volunteer Name`),
          `Total Hours`      = sum(`Hours Worked`, na.rm = TRUE),
          .groups = "drop"
        )
      
      all_row <- dplyr::summarise(
        data,
        `Market Date`      = "All Markets",
        `Total Volunteers` = dplyr::n_distinct(`Volunteer Name`),
        `Total Hours`      = sum(`Hours Worked`, na.rm = TRUE)
      )
      
      stats <- dplyr::bind_rows(all_row, stats)
      
      DT::datatable(stats, class = "stripe hover compact",
                    options = list(dom = "tip", pageLength = 10, autoWidth = TRUE),
                    rownames = FALSE)
      
      # Summary statistics on market attendance 
    } else if (input$dataset == "Market Attendance") {
      validate(
        need("Total Customer Attendance" %in% names(data), "Total Customer Attendance column not found"),
        need("Average Temperature" %in% names(data), "Average Temperature column not found"),
        need("Number of Tabling Organizations" %in% names(data), "Number of Tabling Organizations column not found"),
        need("Number of Vendors" %in% names(data), "Number of Vendors column not found")
      )
      
      stats <- c(
        paste("Total Customer Attendance:", format(sum(data$`Total Customer Attendance`, na.rm = TRUE), big.mark=",")),
        paste("Average Customer Attendance:", format(round(mean(data$`Total Customer Attendance`, na.rm = TRUE)), big.mark=",")),
        paste("Average Temperature:", round(mean(data$`Average Temperature`, na.rm = TRUE), 1)),
        paste("Total Tabling Organizations:", format(sum(data$`Number of Tabling Organizations`, na.rm = TRUE), big.mark=",")),
        paste("Average Tabling Organizations:", format(round(mean(data$`Number of Tabling Organizations`, na.rm = TRUE)), big.mark=",")),
        paste("Total Vendors:", format(sum(data$`Number of Vendors`, na.rm = TRUE), big.mark=",")),
        paste("Average Vendors:", format(round(mean(data$`Number of Vendors`, na.rm = TRUE)), big.mark=","))
      )
      
      summary_stats <- data.frame(Statistic = stats, stringsAsFactors = FALSE)
      
      DT::datatable(
        summary_stats,
        colnames = NULL,
        class   = "compact",
        options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
        rownames = FALSE
      )
      
      # Summary statistics on sales performance (24-25) for each market date
    } else if (input$dataset == "Sales Performance" && grepl("^Sales_\\d{4}-\\d{2}-\\d{2}$", input$salesDate)) {
      
      validate(
        need("Category" %in% names(data), "Category column not found"),
        need("Number Sold" %in% names(data), "Number Sold column not found"),
        need("Gross Sales" %in% names(data), "Gross Sales column not found")
      )
      
      stats <- data %>%
        group_by(Category) %>%
        summarise(
          `Total Number Sold`   = sum(`Number Sold`, na.rm = TRUE),
          `Total Gross Sales`  = sum(`Gross Sales`, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        bind_rows(
          summarise(
            data,
            Category            = "All Categories",
            `Total Number Sold`  = sum(`Number Sold`, na.rm = TRUE),
            `Total Gross Sales` = sum(`Gross Sales`, na.rm = TRUE)
          )
        ) %>%
        rename(`Product Category` = Category) %>%
        arrange(desc(`Product Category` == "All Categories"), `Product Category`)
      
      DT::datatable(
        stats,
        class   = "stripe hover compact",
        options = list(
          dom        = "t", 
          pageLength = nrow(stats), 
          paging     = FALSE, 
          autoWidth  = TRUE
        ),
        rownames = FALSE
      ) %>%
        # format numbers
        DT::formatCurrency("Total Number Sold", currency = "", digits = 0, mark = ",") %>%
        DT::formatCurrency("Total Gross Sales", currency = "$", digits = 2, mark = ",")
      
      # Summary statistics on sales performance (24-25) for all markets together
    } else if (input$dataset == "Sales Performance" && grepl("Sales_Summary_", input$salesDate)) {
      validate(
        need("Market Date" %in% names(data), "Market Date column not found"),
        need("Total Items Sold" %in% names(data), "Total Items Sold column not found"),
        need("Total Gross Sales" %in% names(data), "Total Gross Sales column not found"),
        need("Average Transaction Value" %in% names(data), "Average Transaction Value column not found")
      )
      
      stats <- c(
        paste("Total Items Sold:", format(sum(data$`Total Items Sold`, na.rm = TRUE), big.mark=",")),
        paste("Average Items Sold:", format(round(mean(data$`Total Items Sold`, na.rm = TRUE)), big.mark=",")),
        paste("Total Gross Sales:", paste0("$", format(round(sum(data$`Total Gross Sales`, na.rm = TRUE), 2), big.mark=",", nsmall=2))),
        paste("Average Gross Sales:", paste0("$", format(round(mean(data$`Total Gross Sales`, na.rm = TRUE), 2), big.mark=",", nsmall=2))),
        paste("Average Transaction Value:", paste0("$", format(round(mean(data$`Average Transaction Value`, na.rm = TRUE), 2), big.mark=",", nsmall=2)))
      )
      
      summary_stats <- data.frame(Statistic = stats, stringsAsFactors = FALSE)
      
      DT::datatable(
        summary_stats,
        colnames = NULL,
        class = "compact",
        options = list(dom = "t", paging = FALSE, ordering = FALSE, autoWidth = TRUE),
        rownames = FALSE
      )
    }
  })

}

# Run the shiny app
shinyApp(ui = ui, server = server)
