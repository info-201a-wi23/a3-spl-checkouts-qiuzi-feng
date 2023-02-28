# Preparation Works

# Load libraries
library("dplyr")
library("stringr")

# Load the data
spl_df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# create a new dataset with all the books that are related to Harry Potter.
harry_potter <- spl_df %>%
  filter(str_detect(Title, "Harry Potter"))

# For spl_df: Create a new date column ("date") with the month AND year and a default first day of the month
# Format: 10-01-2020
# convert the new column to a date value
spl_df <- spl_df %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")

# For harry_potter: Create a new date column ("date") with the month AND year and a default first day of the month
# Format: 10-01-2020
# convert the new column to a date value
harry_potter <- harry_potter %>% mutate(date = paste0(CheckoutYear, "-", CheckoutMonth,  "-01" ))
harry_potter$date <- as.Date(harry_potter$date, format = "%Y-%m-%d")



# Calculated 5 values

# 1 - What is the Harry Potter related book that is checked out the most? What is its checked out times?
# Value: 196 times, "Harry Potter and the Chamber of Secrets: Harry Potter Series, Book 2 (unabridged)"
summary1_info <- list()
summary1_info$num_observations <- nrow(harry_potter)
summary1_info$most_checkoutname_hp <- harry_potter %>%
  filter(`Checkouts` == max(`Checkouts`, na.rm = T)) %>%
  select(`Title`)

summary11_info <- list()
summary11_info$num_observations <- nrow(harry_potter)
summary11_info$most_checkout_hp <- harry_potter %>%
  filter(`Checkouts` == max(`Checkouts`, na.rm = T)) %>%
  select(`Checkouts`)


# 2 - What is the month with the most checkouts for all Harry Potter books?
# Value: March of the year of 2022
summary2_info <- list()
summary2_info$num_observations <- nrow(harry_potter)
summary2_info$most_checkoutm_hp <- harry_potter %>%
  group_by(date) %>%
  summarize(max_checkouts = max(Checkouts)) %>% 
  filter(`max_checkouts` == max(`max_checkouts`, na.rm = T)) %>% 
  select(`date`)


# 3 - What is the month with the most checkouts for all Harry Potter related ebooks?
# Value: March of the year of 2022
summary3_info <- list()
summary3_info$num_observations <- nrow(harry_potter)
summary3_info$most_checkout_ebookmonth_hp <- harry_potter %>%
  filter(str_detect(MaterialType, "EBOOK")) %>% 
  filter(`Checkouts` == max(`Checkouts`, na.rm = T)) %>% 
  select(`date`)

# 4 - How has the number of book checkouts changed over time for all the books related to Harry Potter?
# Value: This is a list of values that shows the difference of checkouts from month to month. To view details please load the chart below.
# view data list in this chart
checkout_monthly_difference <- harry_potter %>%
  group_by(date) %>% 
  summarize(sum_checkouts = sum(Checkouts)) %>% 
  mutate(new_checkouts = sum_checkouts - lag(sum_checkouts))

summary4_info <- list()
summary4_info$num_observations <- nrow(harry_potter)
summary4_info$checkout_change_over_time_hp <- harry_potter %>%
  group_by(date) %>% 
  summarize(sum_checkouts = sum(Checkouts)) %>% 
  mutate(new_checkouts = sum_checkouts - lag(sum_checkouts))


# 5 - What is the average number of checkouts for books that are related to Harry Potter?
# Value: 10.30457
summary5_info <- list()
summary5_info$num_observations <- nrow(harry_potter)
summary5_info$avg_checkout_hp <- harry_potter %>%
  summarize(avg_checkouts = mean(Checkouts))
