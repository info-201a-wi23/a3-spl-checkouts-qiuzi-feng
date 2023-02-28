# Load libraries
library("tidyr")
library("dplyr")
library("stringr")
library("ggplot2")
library("readr")
library("scales")
library("RColorBrewer")

# Prepare the sub-dataframe for building the chart
spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

harry_potter <- spl_df %>%
  filter(str_detect(Title, "Harry Potter"))

harry_potter <- harry_potter %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
harry_potter$date <- as.Date(harry_potter$date, format = "%Y-%m-%d")

checkout_difference <- harry_potter %>%
  group_by(date) %>% 
  summarize(sum_checkouts = sum(Checkouts)) %>% 
  mutate(new_checkouts = sum_checkouts - lag(sum_checkouts)) %>% 
  select("date", "new_checkouts")


# building the chart
ggplot(checkout_difference) +
  geom_col(mapping = aes(x = date, y = new_checkouts),
           position = "dodge") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  labs(title ="Harry Potter-Related Books Monthly Checkout Difference" , x = "Month of the Year",
       y = "Checkouts Difference From the Previous Month")
