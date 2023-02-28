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

sum_materialtype_checkout_monthly <- harry_potter %>%
  group_by(date, MaterialType) %>% 
  summarize(all_checkouts = sum(Checkouts))

material_types_checkout <- sum_materialtype_checkout_monthly %>%
  select("MaterialType", "all_checkouts", "date")

# building the chart
ggplot(material_types_checkout, aes(x = date, y = all_checkouts, color = MaterialType)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 1200, 100)) +
  labs(title = "Harry Potter Books Checkout Summary Overtime Organized by Book Material Types",
       x = "Month of the Year",
       y = "Number of Checkouts",
       color = "Book Material Types"
  )