library(tidyverse)
library(tidyquant)
library(timetk)
library(readxl)
library(plotly)
library(scales)
library(formattable)
library(fredr)
library(broom)

core_cpi_w_lags <- 
  "CPILFESL" %>% 
  tq_get(get = "economic.data", from = "1979-01-01") %>% 
  select(date, cpi = price) %>% 
  mutate(core_cpi_yoy_change = cpi/lag(cpi, 12) - 1,
         lag_core_cpi_yoy_change = lag(core_cpi_yoy_change))

core_cpi_w_lags %>% 
  select(date, lag_core_cpi_yoy_change) %>% 
  left_join(ten_year_yield, by = "date") %>% 
  mutate(pre_post_covid = case_when(date <= "2020-03-01" ~ "pre-covid",
                                    TRUE ~ "post-covid")) %>% 
  drop_na() %>% 
  filter(date >= "1990-01-01") %>% 
  ggplot(aes(x = lag_core_cpi_yoy_change, y = monthly_yield, color = pre_post_covid)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = str_glue("{month(date, label = T, abbr = T)}
                                                {year(date)}")), 
                           data = . %>% 
                             filter(date >= "2021-01-01"),
                           size = 3,
                           show.legend = F,
                           color = "black") +
  geom_smooth(aes(y = monthly_yield,
                  x = lag_core_cpi_yoy_change,
                  linetype = pre_post_covid), 
              formula= y ~ x,
              method = "lm",
              size = .2,
              se = F,
              color = "black", 
              alpha = .5,
              show.legend = F) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  theme_minimal() +
  labs(title = "10-Year Yields v. Lag Core CPI YoY Change",
       x = "Lag Core CPI YoY",
       y = "10-Year Yields",
       color = "",
       caption = "Source: FRED Data") +
  theme(plot.title = element_text(hjust = .5),
        legend.position = "right") 


core_pce_w_lags <- 
  "PCEPILFE" %>% 
  tq_get(get = "economic.data", from = "1979-01-01") %>% 
  select(date, pce = price) %>% 
  mutate(core_pce_yoy_change = pce/lag(pce, 12) - 1,
         lag_core_pce_yoy_change = lag(core_pce_yoy_change))

core_pce_w_lags %>% 
  select(date, lag_core_pce_yoy_change) %>% 
  left_join(ten_year_yield, by = "date") %>% 
  mutate(pre_post_covid = case_when(date <= "2020-03-01" ~ "pre-covid",
                                    TRUE ~ "post-covid")) %>% 
  drop_na() %>% 
  filter(date >= "1990-01-01") %>% 
  ggplot(aes(x = lag_core_pce_yoy_change, y = monthly_yield, color = pre_post_covid)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = str_glue("{month(date, label = T, abbr = T)}
                                                {year(date)}")), 
                           data = . %>% 
                             filter(date >= "2021-01-01"),
                           size = 3,
                           show.legend = F,
                           color = "black") +
  geom_smooth(aes(y = monthly_yield,
                  x = lag_core_pce_yoy_change,
                  linetype = pre_post_covid), 
              formula= y ~ x,
              method = "lm",
              size = .2,
              se = F,
              color = "black", 
              alpha = .5,
              show.legend = F) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  theme_minimal() +
  labs(title = "10-Year Yields v. Lag Core PCE YoY Change",
       x = "Core PCE YoY",
       y = "10-Year Yields",
       color = "",
       caption = "Source: FRED Data") +
  theme(plot.title = element_text(hjust = .5),
        legend.position = "right") 

