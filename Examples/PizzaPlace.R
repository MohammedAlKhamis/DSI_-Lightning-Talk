# Today I did a nice table using PizzaPlace data set 

library(gt)
library(tidyverse)

# Create `sizes_order` and `types_order` to
# support the ordering of pizza sizes and types
sizes_order <- c("S", "M", "L", "XL", "XXL")
types_order <- c("Classic", "Chicken", "Supreme", "Veggie")
# info_currencies()

pizzaplace <- pizzaplace

 pizzaplace %>% 
  mutate(type = str_to_sentence(type)) %>% 
  mutate(size = factor(size, levels = sizes_order)) %>% 
  mutate(type = factor(type, levels = types_order)) %>% 
  group_by(type, size) %>% 
  summarise(pies = n(),
            Total_income = sum(price)) %>% 
  gt(rowname_col = "size",
     groupname_col = "type") %>% 
  fmt_currency(
    columns = c(Total_income),
    currency = "SAR"
  ) %>% 
  # Adding the number of pies of each type
  summary_rows(
    groups = TRUE,
    columns = "pies",
    fns = list(TOTAL = "sum")
  ) %>% 
  # Adding the total income of pies of each type
  
  summary_rows(
    groups = TRUE,
    columns = "Total_income",
    fns = list(TOTAL = "sum"),
    formatter = fmt_currency,
    currency = "USD",
    
  # coloring the summary and the subgroup sections 
  ) %>%
  tab_options(
    summary_row.background.color = "#f7ffee"
  ) %>% 
  tab_options(
    stub.background.color = "#E6EFFC"
  ) %>% 
  tab_header(
    title = "The pizza Sales in 2015",
    subtitle = "The data is splited by the type and the size of pizza"
  ) %>% 
  cols_label(
    pies = "Pizza",
    Total_income = "Income"
  )  %>% 
  # Adding Footnotes:
  
  tab_footnote(
    footnote = "The Greek Pizza is the onle class that comes with this size",
    locations = cells_stub(
      rows = c("XL","XXL"))
    )


