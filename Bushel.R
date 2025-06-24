################## bushel ##################
# install.packages("readxl")
library(readxl)
library(MASS)
library(dplyr)

############ Background survey analysis ############
#survey25 <- read_excel("/Users/yanyuma/Downloads/0research/04 price setting/background/20250123 Bushel 2025 SOTF Raw Data.xlsx", sheet = 2)
#nrow(survey25) #1337
#ncol(survey25) #395
surveyhistory <- read_excel("/Users/yanyuma/Downloads/0research/04 price setting/background/20250123 Bushel 2025 SOTF Raw Data.xlsx", sheet = 4, col_types = "text")
nrow(surveyhistory) #4788
surveyhistory$Year <- as.integer(as.numeric(surveyhistory$Year))
sum(surveyhistory$Year == 2022) #1230 completes 1107
sum(surveyhistory$Year == 2023) #1351 completes 1046
sum(surveyhistory$Year == 2024) #871
sum(surveyhistory$Year == 2025) #1336
ncol(surveyhistory) #400

#surveyhistory
###### 30) How satisfied are you with your 2023/2024/2025 grain marketing results? ######
which(names(surveyhistory) == "How satisfied are you with your 2024 grain marketing results?Select one.") #at column 255
unique(surveyhistory$`How satisfied are you with your 2024 grain marketing results?Select one.`)
surveyhistory$satisfy <-factor(
  surveyhistory$`How satisfied are you with your 2024 grain marketing results?Select one.`,
  levels = c("Very Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very Satisfied"),
  ordered = TRUE
)
tapply(is.na(surveyhistory$satisfy), surveyhistory$Year,sum) [as.character(c(2023,2024,2025))]  # cancel 425 342 625

###### 1) Which of the following best describes your primary relationship to farming? ######
table(surveyhistory$`Which of the following best describes your primary relationship to farming?Select one.`)
surveyhistory$Relationship_clean <- ifelse(
  surveyhistory$`Which of the following best describes your primary relationship to farming?Select one.` %in% c(
    "Full-time farm owner / operator", "Full-time Farm Owner / Operator"),
  "Full-time farm owner / operator",
  surveyhistory$`Which of the following best describes your primary relationship to farming?Select one.`
)

surveyhistory$Relationship <- factor(
  surveyhistory$Relationship_clean,
  levels = c("Bookkeeper / accountant","Farm employee","Farm manager","Full-time farm owner / operator","Landowner","Other","Part-time"),
  ordered = FALSE
)
tapply(!is.na(surveyhistory$Relationship), surveyhistory$Year,sum) [as.character(c(2023,2024,2025))]  


###### 2) What is your age? ######
table(surveyhistory$`What is your age?Select one.`)
surveyhistory$Age <- as.character(surveyhistory$`What is your age?Select one.`)
surveyhistory$Age[is.na(surveyhistory$Age)] <- "Missing"

surveyhistory$Age <- factor(
  surveyhistory$Age,
  levels = c("18-30","31-40","41-50","51-60","61-70","71-80","Older than 80","Missing"),
  ordered = FALSE
  )
age_dummy <- model.matrix(~ Age - 1, data = surveyhistory)
surveyhistory <- cbind(surveyhistory, age_dummy)
tapply(!is.na(surveyhistory$`What is your age?Select one.`), surveyhistory$Year,sum) [as.character(c(2023,2024,2025))]  

###### 3) How many acres do you plan to plant in 2023/2024/2025, excluding hay and pasture? ######
table(surveyhistory$`How many acres do you plan to plant in 2025, excluding hay and pasture?Select one.`)
surveyhistory$Acre <- as.character(
  surveyhistory$`How many acres do you plan to plant in 2025, excluding hay and pasture?Select one.`
)
surveyhistory$Acre[is.na(surveyhistory$Acre)] <- "Missing"

surveyhistory$Acre <- factor(
  surveyhistory$Acre,
  levels = c("Fewer than 500 acres",
             "500-1999 acres",
             "2000-4999 acres",
             "5000-9999 acres",
             "10,000 or more acres",
             "Missing"),
  ordered = FALSE
)
acre_dummies <- model.matrix(~ Acre - 1, data = surveyhistory)
surveyhistory <- cbind(surveyhistory, acre_dummies)

tapply(!is.na(surveyhistory$`How many acres do you plan to plant in 2025, excluding hay and pasture?Select one.`), surveyhistory$Year,sum)[as.character(c(2023,2024,2025))]  

###### 6) Which of the following best describes your farm's growth plans over the next five years? 2024/2025 ######
table(surveyhistory$`Which of the following best describes your farm's growth plans over the next five years?Select one.`)
surveyhistory$Plan <- factor(
  surveyhistory$`Which of the following best describes your farm's growth plans over the next five years?Select one.`,
  levels = c("About the same size","Larger by 10%","Larger by 25%","Larger by 50%","Larger by more than double","Plan to exit or retire","Smaller"),
  ordered = FALSE
)
tapply(!is.na(surveyhistory$Plan), surveyhistory$Year,sum)[as.character(c(2023,2024,2025))]  

###### 24) Which of the following grain marketing practices do you use? ######
unique(surveyhistory$`Managed pricing (elevator pricing program, i.e., moving average):Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`Spot cash sale:Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`Futures hedge:Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`Production contract:Which of the following grain marketing practices do you use?Select all that apply.`)
# the first one is 2023 only 
unique(surveyhistory$`Store grain for sale at a later date:Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`Store grain on farm to price at a later date:Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`Store grain at elevator to price at a later date:Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`Lock in the "carry" (spread) for stored grain:Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`HTA contract:Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`Basis contract:Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`Options:Which of the following grain marketing practices do you use?Select all that apply.`)
unique(surveyhistory$`Forward cash contract:Which of the following grain marketing practices do you use?Select all that apply.`)

surveyhistory <- surveyhistory %>%
  mutate(Managed_pricing = ifelse(is.na(`Managed pricing (elevator pricing program, i.e., moving average):Which of the following grain marketing practices do you use?Select all that apply.`) | `Managed pricing (elevator pricing program, i.e., moving average):Which of the following grain marketing practices do you use?Select all that apply.` == "", 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(Spot_cash = ifelse(is.na(`Spot cash sale:Which of the following grain marketing practices do you use?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(Futures_hedge = ifelse(is.na(`Futures hedge:Which of the following grain marketing practices do you use?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(Production_contract = ifelse(is.na(`Production contract:Which of the following grain marketing practices do you use?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(Store_later = ifelse(
    (is.na(`Store grain for sale at a later date:Which of the following grain marketing practices do you use?Select all that apply.`)) &
      (is.na(`Store grain on farm to price at a later date:Which of the following grain marketing practices do you use?Select all that apply.`)) &
      (is.na(`Store grain at elevator to price at a later date:Which of the following grain marketing practices do you use?Select all that apply.`)),
    0, 1
  ))
surveyhistory <- surveyhistory %>%
  mutate(Lock = ifelse(is.na(`Lock in the "carry" (spread) for stored grain:Which of the following grain marketing practices do you use?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(HTA_contract = ifelse(is.na(`HTA contract:Which of the following grain marketing practices do you use?Select all that apply.`) , 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(Basis_contract = ifelse(is.na(`Basis contract:Which of the following grain marketing practices do you use?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(Options = ifelse(is.na(`Options:Which of the following grain marketing practices do you use?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(Forward_cash_contract = ifelse(is.na(`Forward cash contract:Which of the following grain marketing practices do you use?Select all that apply.`), 0, 1))

count_all_zero_for_year <- function(data, year, vars) {
  year_data <- subset(data, Year == year)
  sum(rowSums(year_data[, vars]) == 0)
}
marketing_vars <- c("Managed_pricing", "Spot_cash", "Futures_hedge", "Production_contract",
                    "Store_later", "Lock", "HTA_contract", "Basis_contract",
                    "Options", "Forward_cash_contract")
count_marketing_2023 <- count_all_zero_for_year(surveyhistory, 2023, marketing_vars)
count_marketing_2024 <- count_all_zero_for_year(surveyhistory, 2024, marketing_vars)
count_marketing_2025 <- count_all_zero_for_year(surveyhistory, 2025, marketing_vars)
cat("2023 no marketing practices:", count_marketing_2023, "\n")
cat("2024 no marketing practices:", count_marketing_2024, "\n")
cat("2025 no marketing practices:", count_marketing_2025, "\n")

###### 25) Which of the following pricing strategies do you rely on most frequently? ######
unique(surveyhistory$`Price mainly during seasonal price strength (spring/summer):Which of the following pricing strategies do you rely on most frequently?Select all that apply.`)
unique(surveyhistory$`Price as soon as I see a profit:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`)
unique(surveyhistory$`Sell most of my crop during or shortly after harvest:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`)
unique(surveyhistory$`Price when I need cash flow:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`)
unique(surveyhistory$`Price when my advisor says I should:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`)
unique(surveyhistory$`Price crop in small increments (scale up, selling as market rises):Which of the following pricing strategies do you rely on most frequently?Select all that apply.`)
unique(surveyhistory$`Price when technical analysis (charts) say it's time to sell:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`)
unique(surveyhistory$`Go for the "home run" and price a large portion at a time:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`)
unique(surveyhistory$`Price multiple years of crops when the opportunity arises:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`)

surveyhistory <- surveyhistory %>%
  mutate(seasonal_price = ifelse(is.na(`Price mainly during seasonal price strength (spring/summer):Which of the following pricing strategies do you rely on most frequently?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(see_profit = ifelse(is.na(`Price as soon as I see a profit:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(harvest = ifelse(is.na(`Sell most of my crop during or shortly after harvest:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(need_cash = ifelse(is.na(`Price when I need cash flow:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(advisor = ifelse(is.na(`Price when my advisor says I should:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(increments = ifelse(is.na(`Price crop in small increments (scale up, selling as market rises):Which of the following pricing strategies do you rely on most frequently?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(technical_analysis = ifelse(is.na(`Price when technical analysis (charts) say it's time to sell:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(home_run = ifelse(is.na(`Go for the "home run" and price a large portion at a time:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(opportunity_arises = ifelse(is.na(`Price multiple years of crops when the opportunity arises:Which of the following pricing strategies do you rely on most frequently?Select all that apply.`), 0, 1))

pricing_vars <- c("seasonal_price", "see_profit", "harvest", "need_cash",
                   "advisor", "increments", "technical_analysis", 
                   "home_run", "opportunity_arises")
count_pricing_2023 <- count_all_zero_for_year(surveyhistory, 2023, pricing_vars)
count_pricing_2024 <- count_all_zero_for_year(surveyhistory, 2024, pricing_vars)
count_pricing_2025 <- count_all_zero_for_year(surveyhistory, 2025, pricing_vars)
cat("2023 no pricing strategies :", count_pricing_2023, "\n")
cat("2024 no pricing strategies :", count_pricing_2024, "\n")
cat("2025 no pricing strategies :", count_pricing_2025, "\n")

###### 27) Please answer Yes or No to the following statements.######
unique(surveyhistory$`I have a very good understanding of my cost of production.:Please answer Yes or No to the following statements.`)
unique(surveyhistory$`I use my cost of production to set the price where I will begin pricing my crop.:Please answer Yes or No to the following statements.`)
unique(surveyhistory$`I use accrual-adjusted accounting for farm business analysis.:Please answer Yes or No to the following statements.`)
unique(surveyhistory$`I have a documented marketing plan.:Please answer Yes or No to the following statements.`)

surveyhistory <- surveyhistory %>%
  rename(understanding_cost = `I have a very good understanding of my cost of production.:Please answer Yes or No to the following statements.`) %>%
  mutate(understanding_cost = ifelse(understanding_cost == "Yes", 1,
                              ifelse(understanding_cost == "No", 0, NA)))
which(names(surveyhistory) == "understanding_cost")  # at column 239
surveyhistory %>%
  group_by(Year) %>%
  summarise(na_count = sum(is.na(understanding_cost))) %>%
  arrange(Year)
      
surveyhistory <- surveyhistory %>%
  rename(use_cost = `I use my cost of production to set the price where I will begin pricing my crop.:Please answer Yes or No to the following statements.`) %>%
  mutate(use_cost = ifelse(use_cost == "Yes", 1,
                    ifelse(use_cost == "No", 0, NA)))
surveyhistory %>%
  group_by(Year) %>%
  summarise(na_count = sum(is.na(use_cost))) %>%
  arrange(Year)

surveyhistory <- surveyhistory %>%
  rename(accrual_adjusted_accounting = `I use accrual-adjusted accounting for farm business analysis.:Please answer Yes or No to the following statements.`) %>%
  mutate(accrual_adjusted_accounting = ifelse(accrual_adjusted_accounting == "Yes", 1,
                                       ifelse(accrual_adjusted_accounting == "No", 0, NA)))
surveyhistory %>%
  group_by(Year) %>%
  summarise(na_count = sum(is.na(accrual_adjusted_accounting))) %>%
  arrange(Year)

surveyhistory <- surveyhistory %>%
  rename(documented_marketing_plan = `I have a documented marketing plan.:Please answer Yes or No to the following statements.`) %>%
  mutate(documented_marketing_plan = ifelse(documented_marketing_plan == "Yes", 1,
                                     ifelse(documented_marketing_plan == "No", 0, NA)))
surveyhistory %>%
  group_by(Year) %>%
  summarise(na_count = sum(is.na(documented_marketing_plan))) %>%
  arrange(Year)


###### 28) How do you calculate your cost of production? ######
unique(surveyhistory$`Work with an accountant:How do you calculate your cost of production?Select all that apply.`)
unique(surveyhistory$`Work with a lender/banker:How do you calculate your cost of production?Select all that apply.`)
unique(surveyhistory$`Work with advisory service:How do you calculate your cost of production?Select all that apply.`)
unique(surveyhistory$`Use farm software:How do you calculate your cost of production?Select all that apply.`)
unique(surveyhistory$`Use a spreadsheet:How do you calculate your cost of production?Select all that apply.`)
unique(surveyhistory$`Use university crop budgets:How do you calculate your cost of production?Select all that apply.`)

surveyhistory <- surveyhistory %>%
  mutate(cost_accountant = ifelse(is.na(`Work with an accountant:How do you calculate your cost of production?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(cost_lender = ifelse(is.na(`Work with a lender/banker:How do you calculate your cost of production?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(cost_advisory = ifelse(is.na(`Work with advisory service:How do you calculate your cost of production?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(cost_software = ifelse(is.na(`Use farm software:How do you calculate your cost of production?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(cost_spreadsheet = ifelse(is.na(`Use a spreadsheet:How do you calculate your cost of production?Select all that apply.`), 0, 1))
surveyhistory <- surveyhistory %>%
  mutate(cost_budgets = ifelse(is.na(`Use university crop budgets:How do you calculate your cost of production?Select all that apply.`), 0, 1))

cost_vars <- c("cost_accountant", "cost_lender", "cost_advisory", "cost_software",
                  "cost_spreadsheet", "cost_budgets")
count_cost_2023 <- count_all_zero_for_year(surveyhistory, 2023, cost_vars)
count_cost_2024 <- count_all_zero_for_year(surveyhistory, 2024, cost_vars)
count_cost_2025 <- count_all_zero_for_year(surveyhistory, 2025, cost_vars)
cat("2023 no pricing strategies :", count_cost_2023, "\n")
cat("2024 no pricing strategies :", count_cost_2024, "\n")
cat("2025 no pricing strategies :", count_cost_2025, "\n")



############ 2.Ordered Logistic Regression ############
model_data <- surveyhistory %>%
  filter(Year != 2022) %>%
  filter(AgeMissing != 1, AcreMissing != 1) %>%
  select(Year, satisfy, Relationship, 
         "Age18-30", "Age31-40", "Age41-50", "Age51-60", "Age61-70", "Age71-80", "AgeOlder than 80", "AgeMissing",
         "AcreFewer than 500 acres", "Acre500-1999 acres", "Acre2000-4999 acres", "Acre5000-9999 acres", "Acre10,000 or more acres", "AcreMissing",
         Managed_pricing, Spot_cash, Futures_hedge, Production_contract, Store_later, Lock, HTA_contract, Basis_contract, Options, Forward_cash_contract,
         seasonal_price, see_profit, harvest, need_cash, advisor, increments, technical_analysis, home_run, opportunity_arises,
         cost_accountant, cost_lender, cost_advisory, cost_software, cost_spreadsheet, cost_budgets,
         understanding_cost, use_cost, accrual_adjusted_accounting, documented_marketing_plan) %>%
  filter(complete.cases(.))

## count valid observation 
model_data %>%
  group_by(Year) %>%
  summarise(valid_count = n()) %>%
  arrange(Year)

## Summary of variable - understand cost
understandcost_data <- model_data %>%
  group_by(Year) %>%
  summarise(
    total_responses = sum(!is.na(understanding_cost)),
    count_yes = sum(understanding_cost == 1, na.rm = TRUE)
  ) %>%
  arrange(Year)

summary_long <- understandcost_data %>%
  pivot_longer(cols = c(total_responses, count_yes),
               names_to = "ResponseType",
               values_to = "Count")

ggplot(summary_long, aes(x = Year, y = Count, fill = ResponseType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(title = "I have a very good understanding of my cost of production.",
       x = "Year", y = "Number", fill = "ResponseType") +
  scale_fill_manual(values = c("total_responses" = "steelblue", "count_yes" = "darkorange"),
                    labels = c("total_responses" = "Total", "count_yes" = "Yes")) +
  theme_minimal()

## Summary of variable - use cost
usecost_data <- model_data %>%
  group_by(Year) %>%
  summarise(
    total_responses = sum(!is.na(use_cost)),
    count_yes = sum(use_cost == 1, na.rm = TRUE)
  ) %>%
  arrange(Year)

usecost_summary_long <- usecost_data %>%
  pivot_longer(cols = c(total_responses, count_yes),
               names_to = "ResponseType",
               values_to = "Count")

ggplot(usecost_summary_long, aes(x = Year, y = Count, fill = ResponseType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(title = "I use my cost of production to set the price where I will begin pricing my crop.",
       x = "Year", y = "Number", fill = "ResponseType") +
  scale_fill_manual(values = c("total_responses" = "steelblue", "count_yes" = "darkorange"),
                    labels = c("total_responses" = "Total", "count_yes" = "Yes")) +
  theme_minimal()

## Summary of variable - cost 
model_data_cost <- model_data %>%
  mutate(
    all_zero = if_else(
      cost_accountant == 0 &
        cost_lender == 0 &
        cost_advisory == 0 &
        cost_software == 0 &
        cost_spreadsheet == 0 &
        cost_budgets == 0, 1, 0),
    
    diy_only = if_else(
      (cost_software == 1 | cost_spreadsheet == 1 | cost_budgets == 1) &
        (cost_accountant == 0 & cost_lender == 0 & cost_advisory == 0), 1, 0),
    
    others_only = if_else(
      (cost_accountant == 1 | cost_lender == 1 | cost_advisory == 1) &
        (cost_software == 0 & cost_spreadsheet == 0 & cost_budgets == 0), 1, 0)
  ) %>%
  mutate(mixed = if_else(all_zero == 0 & diy_only == 0 & others_only == 0, 1, 0))

model_data_cost %>%
  group_by(Year) %>%
  summarise(
    count_all_zero = sum(all_zero),
    count_diy_only = sum(diy_only),
    count_others_only = sum(others_only),
    count_mixed = sum(mixed),
    total = n()
  ) %>%
  arrange(Year)

model_data_cost_zero <- model_data_cost %>%
  filter(all_zero == 1) %>% select(-all_zero,-diy_only,-others_only,-mixed)

model_data_cost_diy <- model_data_cost %>%
  filter(diy_only == 1) %>% select(-all_zero,-diy_only,-others_only,-mixed)

model_data_cost_others <- model_data_cost %>%
  filter(others_only == 1) %>% select(-all_zero,-diy_only,-others_only,-mixed)

model_data_cost_mixed <- model_data_cost %>%
  filter(mixed == 1) %>% select(-all_zero,-diy_only,-others_only,-mixed)


## result of Ordered Logistic Regression (2023,2024,2025)
model_data_total <-model_data %>% select(-Year)
r <- polr(satisfy ~ ., data = model_data_total, Hess = TRUE)
summary(r)

## year dummy with 2023 baseline 
model_data$Year <- as.factor(model_data$Year)
model_data$Year <- relevel(model_data$Year, ref = "2023")  
model_data_withoutcostcalculation <-model_data %>% select(-cost_accountant, -cost_lender, -cost_advisory, -cost_software, -cost_spreadsheet, -cost_budgets)
r_dummy <- polr(satisfy ~ . + Year, data = model_data_withoutcostcalculation, Hess = TRUE)
summary(r_dummy)

r_cost_zero <-  polr(satisfy ~ . + Year, data = model_data_cost_zero, Hess = TRUE)
summary(r_cost_zero)
# alias(satisfy ~ . + Year, data = model_data_cost_zero)
# Multicollinearity： `AgeOlder than 80` AgeMissing  `Acre10,000 or more acres` AcreMissing cost_accountant cost_lender cost_advisory cost_software cost_spreadsheet cost_budgets

r_cost_diy <-  polr(satisfy ~ . + Year, data = model_data_cost_diy, Hess = TRUE)
summary(r_cost_diy)
# Multicollinearity： `AgeOlder than 80` AgeMissing  `Acre10,000 or more acres` AcreMissing cost_accountant cost_lender cost_advisory

alias(satisfy ~ . + Year, data = model_data_cost_others)
r_cost_others <-  polr(satisfy ~ . + Year, data = model_data_cost_others, Hess = TRUE)
summary(r_cost_others)

r_cost_mixed <-  polr(satisfy ~ . + Year, data = model_data_cost_mixed, Hess = TRUE)
summary(r_cost_mixed)
alias(satisfy ~ . + Year, data = model_data_cost_mixed)


# Ordered Logistic Regression for each year
model_data_2023 <- model_data %>% 
  filter(Year == 2023) %>% 
  select(-Year)
r_2023 <- polr(satisfy ~ ., data = model_data_2023, Hess = TRUE)
summary(r_2023)

model_data_2024 <- model_data %>% 
  filter(Year == 2024)  %>% 
  select(-Year)
r_2024 <- polr(satisfy ~ ., data = model_data_2024, Hess = TRUE)
summary(r_2024)

model_data_2025 <- model_data %>% 
  filter(Year == 2025)  %>% 
  select(-Year)
r_2025 <- polr(satisfy ~ ., data = model_data_2025, Hess = TRUE)
summary(r_2025)

############ 3.K modes cluster + Ordered Logistic Regression ############
#install.packages("klaR")
#install.packages("FactoMineR")
#install.packages("factoextra")
library(klaR)
library(FactoMineR)
library(factoextra)
library(ggplot2)
str(model_data_total)

set.seed(123)
model_data_kmodes <- model_data_total %>%
  select(cost_accountant, cost_lender, cost_advisory, cost_software, cost_spreadsheet, cost_budgets) %>%
  mutate(across(everything(), ~ as.factor(as.character(.)))) %>%
  as.data.frame()
km_result <- kmodes(model_data_kmodes, modes = 3, iter.max = 10)
km_result$modes
model_data_kmodes$cluster <- as.factor(km_result$cluster)
model_data_total$cluster <- as.factor(km_result$cluster)

## result of Ordered Logistic Regression (2023,2024,2025) with k mode cluster 
olr_models <- list()
for (k in 1:3) {
  data_subset <- model_data_total[model_data_kmodes$cluster == k, ]
  model <- polr(satisfy ~ ., data = data_subset, Hess = TRUE)
  olr_models[[paste0("Cluster_", k)]] <- summary(model)
}
for (k in 1:3) {
  cat("\n===== Cluster", k, "=====\n")
  print(olr_models[[paste0("Cluster_", k)]])
}

## result of Ordered Logistic Regression (year dummy) with k mode cluster 
model_data$Year <- relevel(model_data$Year, ref = "2023")  
olr_cluster_year <- list()
for (k in 1:3) {
  data_subset <- model_data[model_data_kmodes$cluster == k, ]
  data_subset$Year <- relevel(factor(data_subset$Year), ref = "2023")
  model <- polr(satisfy ~ . + Year, data = data_subset, Hess = TRUE)
  olr_cluster_year[[paste0("Cluster_", k)]] <- summary(model)
}
for (k in 1:3) {
  cat("\n===== Cluster", k, "=====\n")
  print(olr_cluster_year[[paste0("Cluster_", k)]])
}
table(model_data_kmodes$cluster)
# 1   2   3 
# 690 741 392 
