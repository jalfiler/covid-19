### Packages and setup
  
library(tidyverse) # the fundamentals
library(lubridate) # working with dates
library(timetk)    # time series Swiss Army Knife
library(tidyquant) # great finance functions and FRED importer
library(readxl)
library(scales)
library(fredr)    # meta data on Fred data sets
library(janitor)  # clean column names
library(reshape) # for rename
library(gt)
library(gtExtras) #FIX
library(downloadthis) # create downloaders
# fred_api_key <- Sys.getenv("FRED_API_KEY") 
knitr::opts_chunk$set(echo = TRUE)


  
### Project for today
  
# import CPI component data
# wrangle into long and wide format
# visualize with gt
# add some highlights with gtExtras

### Import and Wrangle the Data

# Our first goal is to import data on relative importance or weights of CPI components. '

#That data is available here: 
#https://www.bls.gov/cpi/tables/relative-importance/home.htm

# Let's use the `import data set` button to grab the spreadsheet.


url <- "https://www.bls.gov/cpi/tables/relative-importance/2021.xlsx"
destfile <- "X2021.xlsx"
curl::curl_download(url, destfile)
relative_importance <- 
  read_excel(destfile, skip = 9)  %>% 
  slice(-1) %>% 
  select(-4) %>% 
  rename(level = 1, item = 2, cpi_u = 3)


# Today we are going to work with `Level 1` components. 
# A bit tricky here because we don't want the aggregates 
# that lurk at the bottom of the spreadsheet, so not a simple filter.


relative_importance %>% 
  filter(level == 1)

# We want all the level 1's that appear above `Special aggregate`.


grepl("Special aggregate", relative_importance$item, fixed = TRUE)
cumsum(grepl("Special aggregate", relative_importance$item, fixed = TRUE))

# Place that in a call to `filter()` and pipe to `adorn_totals()` for a sanity check.


relative_importance %>% 
  filter(cumsum(grepl("Special aggregate", item, fixed = TRUE)) < 1) %>% 
  filter(level == 1) %>% 
  adorn_totals()


relative_importance %>% 
  filter(cumsum(grepl("Special aggregate", item, fixed = TRUE)) < 1) %>% 
  filter(level == 1) %>% 
  select(-level) %>% 
  arrange(-cpi_u) %>% 
  adorn_totals() %>% 
  tibble() %>% 
  gt() %>% 
  cols_label(
    item = "", 
    cpi_u = "CPI Weight"
  ) %>% 
  fmt_percent(
    columns = cpi_u,
    scale_values = F
  )


# We could keep going along this path of drilling down into levels. 


relative_importance %>% 
  filter(cumsum(grepl("Apparel", item, fixed = TRUE)) < 1) %>% 
  filter(cumsum(grepl("Housing", item, fixed = TRUE)) >= 1) %>% 
  # filter(level == 2) %>% 
  filter(level == 3) %>% 
  select(-level) %>% 
  arrange(-cpi_u) %>% 
  adorn_totals() %>% 
  tibble() %>% 
  gt() %>% 
  cols_label(
    item = "", 
    cpi_u = "CPI Weight"
  ) %>% 
  fmt_percent(
    columns = cpi_u,
    scale_values = F
  ) %>% 
  tab_header(title = "Housing Subcomponents")


# Interesting stuff but not really what most people want to see when they think of Inflation. 
# Let's head back to the `Level 1` components and import their index histories. 
# The weights give context to those indexes, but the indexes get all the headlines.

# Let's use `tribble()` to manually create a data frame with Fred codes and `item` labels. 
# We use `item` so we can `left_join()` with our `relative_importance` tibble.


level_1_cpi_components_manual <- 
  tribble(
    ~symbol, ~item,
    "CPIAPPSL", "Apparel",
    "CPIMEDSL", "Medical care",
    "CPIHOSSL", "Housing",
    "CPIFABSL", "Food and beverages", 
    "CPITRNSL", "Transportation",
    "CPIEDUSL", "Education and communication",
    "CPIRECSL", "Recreation",
    "CPIOGSSL", "Other goods and services"
  )
# alternate way of exploring with fredr
# need Fre API Key
# fredr::fredr_series_search_id("CPI") %>% 
#   filter(frequency == "Monthly",
#          str_detect(title, "Consumer Price Index for All Urban Consumers"),
#          seasonal_adjustment_short == "SA",
#          !str_detect(title, "All Items"))



level_1_cpi_fred_symbols <- 
  relative_importance %>% 
  filter(cumsum(grepl("Special", item, fixed = TRUE)) < 1) %>% 
  filter(level == 1) %>% 
  left_join(
    level_1_cpi_components_manual
  ) %>% 
  rename(cpi_weight = cpi_u) %>% 
  arrange(-cpi_weight) 


#Pass the Fred symbols to `tq_get()`


level_1_cpi_data <- 
  level_1_cpi_fred_symbols %>% 
  pull(symbol) %>% 
  tq_get(get = "economic.data", from =  "1979-01-01") %>% 
  left_join(
    level_1_cpi_fred_symbols %>% select(-level) , 
    by = "symbol"
  ) %>% 
  select(-symbol)


### Building a Table with gt

# First, let's look at percent change by month. 
# We'll do some transforming with mutate() and then pivot_wider()
# because people like dates running across columns. 


level_1_wide_data_for_gt <- 
  level_1_cpi_data %>% 
  group_by(item, cpi_weight) %>% 
  mutate(
    mom_change = price/lag(price, 12) - 1,
    date = as.yearmon(date)
  ) %>% 
  select(-price) %>% 
  filter(date >= "2021-09-01") %>% 
  arrange(-date) %>% 
  pivot_wider(names_from = date, values_from = mom_change) %>% 
  ungroup() 
level_1_wide_data_for_gt


# Let's painstakingly turn this into a table.


gt_table_1 <-
level_1_wide_data_for_gt %>% 
  gt() %>% 
    tab_header(title = "CPI Level 1 YoY % Changes") %>% 
  cols_label(
    item = "",
    cpi_weight = "Weight"
  ) %>% 
  fmt_percent(
    columns = is.numeric,
    decimals = 2,
  ) %>%
  fmt_percent(
    columns = cpi_weight,
    decimals = 2,
    # Important line next for scaling values
    scale_values = F
  )  %>% 
   data_color(
    # columns = cpi_weight, 
    columns = contains("20"), 
    colors = scales::col_numeric(
      colorspace::diverge_hcl(n = 20,  palette = "Blue-Red 3") %>% rev(),
      domain = c(-.15, .25)) 
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "darkgreen")
    ),
    locations = cells_body(
      columns = vars(cpi_weight),
      rows = cpi_weight > 10
    )
  ) 
gt_table_1



 gt_table_2 <- 
gt_table_1 %>% 
  tab_footnote(footnote = "Weights as of Feb 2022",
               locations = cells_column_labels(
               columns = 2
               )) %>% 
  tab_source_note(html("Data from BLS via <a href='https://fred.stlouisfed.org/'>FRED</a>"))  %>%  
  tab_options(
    row_group.border.top.width = px(3),
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.top.color = "white",
    table.border.top.width = px(3),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
  ) 
gt_table_2



#Two great packages to help us out: 
#gtExtras and downloadthis


library(gtExtras)
library(downloadthis)


# Perhaps we want to display weights as a bar chart. 
# We can use gt_plt_bar_pct() from gtExtras.


gt_table_3 <-
gt_table_2   %>% 
  gt_plt_bar_pct(
    column = cpi_weight,
    scaled = T,
    fill = "steelblue"
  ) %>% 
  cols_width(
    cpi_weight ~px(100)
  )
gt_table_3



### Add a sparkline

# We can add a `sparkline` to display a time series chart in the table. 
# First we need a list column - we need this data stored in one row.

cpi_data_for_sparkline <- 
level_1_cpi_data %>% 
  group_by(item, cpi_weight) %>% 
  filter(date > "1990-01-01") %>% 
  summarise(cpi = list(price))


# Next we join that list column to our original data and use gt_sparkline() from gtExtras.


level_1_wide_data_for_gt %>% 
  left_join(
    cpi_data_for_sparkline, 
    by = c("item", "cpi_weight")
  ) %>% 
  gt() %>% 
  cols_label(
    item = "",
    cpi_weight = "Weight",
    cpi = "CPI Since 1990"
  ) %>% 
  fmt_percent(
    columns = is.numeric,
    decimals = 2,
  ) %>%
  fmt_percent(
    columns = cpi_weight,
    decimals = 2,
    scale_values = F
  ) %>%
  tab_header(title = "CPI Level 1 YoY % Changes and Sparkline History") %>% 
   data_color(
    columns = contains("20"), 
    colors = scales::col_numeric(colorspace::diverge_hcl(n = 25,  palette = "Blue-Red 3") %>% rev(),
    domain = c(-.15, .25)) 
  ) %>% 
  cols_width(
   cpi  ~ px(120)
  ) %>% 
  cols_align(
    align = "center"
  ) %>% 
  gt_sparkline(cpi)  
  # gt_theme_538()



### Download the data

# Finally, users like to download raw data so let's add a download button.


cpi_download_excel <- 
  level_1_wide_data_for_gt %>%
  download_this(
    output_name = "cpi-data",
    output_extension = ".xlsx",  
    button_label = "Download xlsx",
    button_type = "primary", 
  )
gt_table_3 %>% 
  tab_source_note(cpi_download_excel)
