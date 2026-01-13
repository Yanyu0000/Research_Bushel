########## expecatation project ##########
library(readxl)
library(timeDate)
library(dplyr)
library(lubridate)
library(purrr)
library(nnet)
library(MASS)
library(openxlsx)

# ======================
# survey data #
# ======================
setwd("/Users/yanyuma/Downloads/0research/05 Ag Barometer Data/Data")  
dat <- read.xlsx("expectation_survey.xlsx") 
dat <- dat %>%
  mutate(tradingDay = as.Date(Date, format = "%y%m%d")) %>%
  dplyr::select(-Date,-`Est.GFI`,-`GFI.Score`,-CSS_CellCode,-CSS_BadNpaNxx)

# ======================
# price contract data #
# ======================
# NOTE: change the path of contract data 
# cash contract
Corn <- read.csv("/Users/yanyuma/Downloads/0research/05 Ag Barometer Data/Data/cash contract/Corn_Y00.csv")
Cotton <- read.csv("/Users/yanyuma/Downloads/0research/05 Ag Barometer Data/Data/cash contract/Cotton_Y00.csv")
Hogs <- read.csv("/Users/yanyuma/Downloads/0research/05 Ag Barometer Data/Data/cash contract/Lean_Hogs_Y00.csv")
Cattle <- read.csv("/Users/yanyuma/Downloads/0research/05 Ag Barometer Data/Data/cash contract/Live_Cattle_Y00.csv")
Soybeans <- read.csv("/Users/yanyuma/Downloads/0research/05 Ag Barometer Data/Data/cash contract/Soybeans_Y00.csv")
Wheat <- read.csv("/Users/yanyuma/Downloads/0research/05 Ag Barometer Data/Data/cash contract/Wheat_Y00.csv")

Corn <- Corn %>%
  mutate(tradingDay = as.Date(tradingDay)) %>%
  rename(#corn_open = open,
         #corn_high = high,
         #corn_low = low,
         corn = close) %>%
  dplyr::select(-symbol,-timestamp,-open,-high,-low,-volume,-openInterest) %>%
  mutate(
    # corn_open_1 = lag(corn_open,1),
    # corn_high_1 = lag(corn_high,1),
    # corn_low_1 = lag(corn_low,1),
    corn_1 = lag(corn,1),
    
    # corn_open_2 = lag(corn_open,2),
    # corn_high_2 = lag(corn_high,2),
    # corn_low_2 = lag(corn_low,2),
    corn_2 = lag(corn,2),
    
    # corn_open_3 = lag(corn_open,3),
    # corn_high_3 = lag(corn_high,3),
    # corn_low_3 = lag(corn_low,3),
    corn_3 = lag(corn,3),
    
    # corn_open_4 = lag(corn_open,4),
    # corn_high_4 = lag(corn_high,4),
    # corn_low_4 = lag(corn_low,4),
    corn_4 = lag(corn,4),
    
    # corn_open_5 = lag(corn_open,5),
    # corn_high_5 = lag(corn_high,5),
    # corn_low_5 = lag(corn_low,5),
    corn_5 = lag(corn,5),
    
    # corn_open_6 = lag(corn_open,6),
    # corn_high_6 = lag(corn_high,6),
    # corn_low_6 = lag(corn_low,6),
    corn_6 = lag(corn,6),
         )

Cotton <- Cotton %>%
  mutate(tradingDay = mdy(Date)) %>%
  rename(#cotton_open = open,
         #cotton_high = high,
         #cotton_low = low,
         cotton = close) %>%
  dplyr::select(-symbol,-Date,-open,-high,-low) %>%
  mutate(
    # cotton_open_1 = lag(cotton_open,1),
    # cotton_high_1 = lag(cotton_high,1),
    # cotton_low_1 = lag(cotton_low,1),
    cotton_1 = lag(cotton,1),
    
    # cotton_open_2 = lag(cotton_open,2),
    # cotton_high_2 = lag(cotton_high,2),
    # cotton_low_2 = lag(cotton_low,2),
    cotton_2 = lag(cotton,2),
    
    # cotton_open_3 = lag(cotton_open,3),
    # cotton_high_3 = lag(cotton_high,3),
    # cotton_low_3 = lag(cotton_low,3),
    cotton_3 = lag(cotton,3),
    
    # cotton_open_4 = lag(cotton_open,4),
    # cotton_high_4 = lag(cotton_high,4),
    # cotton_low_4 = lag(cotton_low,4),
    cotton_4 = lag(cotton,4),
    
    # cotton_open_5 = lag(cotton_open,5),
    # cotton_high_5 = lag(cotton_high,5),
    # cotton_low_5 = lag(cotton_low,5),
    cotton_5 = lag(cotton,5),
    
    # cotton_open_6 = lag(cotton_open,6),
    # cotton_high_6 = lag(cotton_high,6),
    # cotton_low_6 = lag(cotton_low,6),
    cotton_6 = lag(cotton,6),
  )

Hogs <- Hogs %>%
  mutate(tradingDay = as.Date(tradingDay)) %>%
  rename(#hogs_open = open,
         #hogs_high = high,
         #hogs_low = low,
         hogs = close) %>%
  dplyr::select(-symbol,-timestamp,-open,-high,-low,-volume,-openInterest) %>%
  mutate(
    # hogs_open_1 = lag(hogs_open,1),
    # hogs_high_1 = lag(hogs_high,1),
    # hogs_low_1 = lag(hogs_low,1),
    hogs_1 = lag(hogs,1),
    
    # hogs_open_2 = lag(hogs_open,2),
    # hogs_high_2 = lag(hogs_high,2),
    # hogs_low_2 = lag(hogs_low,2),
    hogs_2 = lag(hogs,2),
    
    # hogs_open_3 = lag(hogs_open,3),
    # hogs_high_3 = lag(hogs_high,3),
    # hogs_low_3 = lag(hogs_low,3),
    hogs_3 = lag(hogs,3),
  
    # hogs_open_4 = lag(hogs_open,4),
    # hogs_high_4 = lag(hogs_high,4),
    # hogs_low_4 = lag(hogs_low,4),
    hogs_4 = lag(hogs,4),
    
    # hogs_open_5 = lag(hogs_open,5),
    # hogs_high_5 = lag(hogs_high,5),
    # hogs_low_5 = lag(hogs_low,5),
    hogs_5 = lag(hogs,5),
    
    # hogs_open_6 = lag(hogs_open,6),
    # hogs_high_6 = lag(hogs_high,6),
    # hogs_low_6 = lag(hogs_low,6),
    hogs_6 = lag(hogs,6),
  )

Cattle <- Cattle %>%
  mutate(tradingDay = as.Date(tradingDay)) %>%
  rename(#cattle_open = open,
         #cattle_high = high,
         #cattle_low = low,
         cattle = close) %>%
  dplyr::select(-symbol,-timestamp,-open,-high,-low,-volume,-openInterest) %>%
  mutate(
    # cattle_open_1 = lag(cattle_open,1),
    # cattle_high_1 = lag(cattle_high,1),
    # cattle_low_1 = lag(cattle_low,1),
    cattle_1 = lag(cattle,1),
    
    # cattle_open_2 = lag(cattle_open,2),
    # cattle_high_2 = lag(cattle_high,2),
    # cattle_low_2 = lag(cattle_low,2),
    cattle_2 = lag(cattle,2),
    
    # cattle_open_3 = lag(cattle_open,3),
    # cattle_high_3 = lag(cattle_high,3),
    # cattle_low_3 = lag(cattle_low,3),
    cattle_3 = lag(cattle,3),
    
    # cattle_open_4 = lag(cattle_open,4),
    # cattle_high_4 = lag(cattle_high,4),
    # cattle_low_4 = lag(cattle_low,4),
    cattle_4 = lag(cattle,4),
    
    # cattle_open_5 = lag(cattle_open,5),
    # cattle_high_5 = lag(cattle_high,5),
    # cattle_low_5 = lag(cattle_low,5),
    cattle_5 = lag(cattle,5),
    
    # cattle_open_6 = lag(cattle_open,6),
    # cattle_high_6 = lag(cattle_high,6),
    # cattle_low_6 = lag(cattle_low,6),
    cattle_6 = lag(cattle,6),
  ) 
  
Soybeans <- Soybeans %>%
  mutate(tradingDay = as.Date(tradingDay)) %>%
  rename(#soybeans_open = open,
         #soybeans_high = high,
         #soybeans_low = low,
         soybeans = close) %>%
  dplyr::select(-symbol,-timestamp,-open,-high,-low,-volume,-openInterest) %>%
  mutate(
    # soybeans_open_1 = lag(soybeans_open,1),
    # soybeans_high_1 = lag(soybeans_high,1),
    # soybeans_low_1 = lag(soybeans_low,1),
    soybeans_1 = lag(soybeans,1),
    
    # soybeans_open_2 = lag(soybeans_open,2),
    # soybeans_high_2 = lag(soybeans_high,2),
    # soybeans_low_2 = lag(soybeans_low,2),
    soybeans_2 = lag(soybeans,2),
    
    # soybeans_open_3 = lag(soybeans_open,3),
    # soybeans_high_3 = lag(soybeans_high,3),
    # soybeans_low_3 = lag(soybeans_low,3),
    soybeans_3 = lag(soybeans,3),
    
    # soybeans_open_4 = lag(soybeans_open,4),
    # soybeans_high_4 = lag(soybeans_high,4),
    # soybeans_low_4 = lag(soybeans_low,4),
    soybeans_4 = lag(soybeans,4),
    
    # soybeans_open_5 = lag(soybeans_open,5),
    # soybeans_high_5 = lag(soybeans_high,5),
    # soybeans_low_5 = lag(soybeans_low,5),
    soybeans_5 = lag(soybeans,5),
    
    # soybeans_open_6 = lag(soybeans_open,6),
    # soybeans_high_6 = lag(soybeans_high,6),
    # soybeans_low_6 = lag(soybeans_low,6),
    soybeans_6 = lag(soybeans,6),
  ) 

Wheat <- Wheat %>%
  mutate(tradingDay = as.Date(tradingDay)) %>%
  rename(#wheat_open = open,
         #wheat_high = high,
         #wheat_low = low,
         wheat = close) %>%
  dplyr::select(-symbol,-timestamp,-open,-high,-low,-volume,-openInterest)  %>%
  mutate(
    # wheat_open_1 = lag(wheat_open,1),
    # wheat_high_1 = lag(wheat_high,1),
    # wheat_low_1 = lag(wheat_low,1),
    wheat_1 = lag(wheat,1),
    
    # wheat_open_2 = lag(wheat_open,2),
    # wheat_high_2 = lag(wheat_high,2),
    # wheat_low_2 = lag(wheat_low,2),
    wheat_2 = lag(wheat,2),
    
    # wheat_open_3 = lag(wheat_open,3),
    # wheat_high_3 = lag(wheat_high,3),
    # wheat_low_3 = lag(wheat_low,3),
    wheat_3 = lag(wheat,3),
    
    # wheat_open_4 = lag(wheat_open,4),
    # wheat_high_4 = lag(wheat_high,4),
    # wheat_low_4 = lag(wheat_low,4),
    wheat_4 = lag(wheat,4),
    
    # wheat_open_5 = lag(wheat_open,5),
    # wheat_high_5 = lag(wheat_high,5),
    # wheat_low_5 = lag(wheat_low,5),
    wheat_5 = lag(wheat,5),
    
    # wheat_open_6 = lag(wheat_open,6),
    # wheat_high_6 = lag(wheat_high,6),
    # wheat_low_6 = lag(wheat_low,6),
    wheat_6 = lag(wheat,6),
  )  

# =======================
# future data 
# =======================
# NOTE: input the following future data by manual selection 
Corn_future <- read.csv("future/Corn_Continuous_Futures.csv")
Soybeans_future <- read.csv("future/Soybeans_Continuous_Futures.csv")
Wheat_future <- read.csv("future/Wheat_Continuous_Futures.csv")
Cotton_future <- read.csv("future/Cotton_Continuous_Futures.csv")
Hogs_future <- read.csv("future/Lean_Hogs_Continuous_Futures.csv")
Cattle_future <- read.csv("future/Live_Cattle_Continuous_Futures.csv")

Corn_future <- Corn_future %>%
  mutate(tradingDay = as.Date(Time)) %>%
  rename(corn_future = Last) %>%
  rename(corn_return = Returns) %>%
  dplyr::select(-Time,-CommodityCode,-ContractMonth,-YearCode,-Roll_Flag) %>%
  mutate(
    corn_future_1 = lag(corn_future,1),
    corn_return_1 = lag(corn_return,1),
    corn_future_2 = lag(corn_future,2),
    corn_return_2 = lag(corn_return,2),
    corn_future_3 = lag(corn_future,3),
    corn_return_3 = lag(corn_return,3),
    corn_future_4 = lag(corn_future,4),
    corn_return_4 = lag(corn_return,4),
    corn_future_5 = lag(corn_future,5),
    corn_return_5 = lag(corn_return,5),
    corn_future_6 = lag(corn_future,6),
    corn_return_6 = lag(corn_return,6),
    corn_future_22 = lag(corn_future,22),
    corn_delta_week = (corn_future_1-corn_future_6)/corn_future_6,
    corn_delta_month = (corn_future_1-corn_future_22)/corn_future_22
  )

Soybeans_future <- Soybeans_future %>%
  mutate(tradingDay = as.Date(Time)) %>%
  rename(soybeans_future = Last) %>%
  rename(soybeans_return = Returns) %>%
  dplyr::select(-Time,-CommodityCode,-ContractMonth,-YearCode,-Roll_Flag) %>%
  mutate(
    soybeans_future_1 = lag(soybeans_future,1),
    soybeans_return_1 = lag(soybeans_return,1),
    soybeans_future_2 = lag(soybeans_future,2),
    soybeans_return_2 = lag(soybeans_return,2),
    soybeans_future_3 = lag(soybeans_future,3),
    soybeans_return_3 = lag(soybeans_return,3),
    soybeans_future_4 = lag(soybeans_future,4),
    soybeans_return_4 = lag(soybeans_return,4),
    soybeans_future_5 = lag(soybeans_future,5),
    soybeans_return_5 = lag(soybeans_return,5),
    soybeans_future_6 = lag(soybeans_future,6),
    soybeans_return_6 = lag(soybeans_return,6),
    soybeans_future_22 = lag(soybeans_future,22),
    soybeans_delta_week = (soybeans_future_1-soybeans_future_6)/soybeans_future_6,
    soybeans_delta_month = (soybeans_future_1-soybeans_future_22)/soybeans_future_22
  )

Wheat_future <- Wheat_future  %>%
  mutate(tradingDay = as.Date(Time)) %>%
  rename(wheat_future = Last) %>%
  rename(wheat_return = Returns) %>%
  dplyr::select(-Time,-CommodityCode,-ContractMonth,-YearCode,-Roll_Flag) %>%
  mutate(
    wheat_future_1 = lag(wheat_future,1),
    wheat_return_1 = lag(wheat_return,1),
    wheat_future_2 = lag(wheat_future,2),
    wheat_return_2 = lag(wheat_return,2),
    wheat_future_3 = lag(wheat_future,3),
    wheat_return_3 = lag(wheat_return,3),
    wheat_future_4 = lag(wheat_future,4),
    wheat_return_4 = lag(wheat_return,4),
    wheat_future_5 = lag(wheat_future,5),
    wheat_return_5 = lag(wheat_return,5),
    wheat_future_6 = lag(wheat_future,6),
    wheat_return_6 = lag(wheat_return,6),
    wheat_future_22 = lag(wheat_future,22),
    wheat_delta_week = (wheat_future_1-wheat_future_6)/wheat_future_6,
    wheat_delta_month = (wheat_future_1-wheat_future_22)/wheat_future_22
  )

Cotton_future <- Cotton_future  %>%
  mutate(tradingDay = as.Date(Time)) %>%
  rename(cotton_future = Last) %>%
  rename(cotton_return = Returns) %>%
  dplyr::select(-Time,-CommodityCode,-ContractMonth,-YearCode,-Roll_Flag) %>%
  mutate(
    cotton_future_1 = lag(cotton_future,1),
    cotton_return_1 = lag(cotton_return,1),
    cotton_future_2 = lag(cotton_future,2),
    cotton_return_2 = lag(cotton_return,2),
    cotton_future_3 = lag(cotton_future,3),
    cotton_return_3 = lag(cotton_return,3),
    cotton_future_4 = lag(cotton_future,4),
    cotton_return_4 = lag(cotton_return,4),
    cotton_future_5 = lag(cotton_future,5),
    cotton_return_5 = lag(cotton_return,5),
    cotton_future_6 = lag(cotton_future,6),
    cotton_return_6 = lag(cotton_return,6),
    cotton_future_22 = lag(cotton_future,22),
    cotton_delta_week = (cotton_future_1-cotton_future_6)/cotton_future_6,
    cotton_delta_month = (cotton_future_1-cotton_future_22)/cotton_future_22
  )

Hogs_future <- Hogs_future  %>%
  mutate(tradingDay = as.Date(Time)) %>%
  rename(hogs_future = Last) %>%
  rename(hogs_return = Returns) %>%
  dplyr::select(-Time,-CommodityCode,-ContractMonth,-YearCode,-Roll_Flag) %>%
  mutate(
    hogs_future_1 = lag(hogs_future,1),
    hogs_return_1 = lag(hogs_return,1),
    hogs_future_2 = lag(hogs_future,2),
    hogs_return_2 = lag(hogs_return,2),
    hogs_future_3 = lag(hogs_future,3),
    hogs_return_3 = lag(hogs_return,3),
    hogs_future_4 = lag(hogs_future,4),
    hogs_return_4 = lag(hogs_return,4),
    hogs_future_5 = lag(hogs_future,5),
    hogs_return_5 = lag(hogs_return,5),
    hogs_future_6 = lag(hogs_future,6),
    hogs_return_6 = lag(hogs_return,6),
    hogs_future_22 = lag(hogs_future,22),
    hogs_delta_week = (hogs_future_1-hogs_future_6)/hogs_future_6,
    hogs_delta_month = (hogs_future_1-hogs_future_22)/hogs_future_22
  )

Cattle_future <- Cattle_future  %>%
  mutate(tradingDay = as.Date(Time)) %>%
  rename(cattle_future = Last) %>%
  rename(cattle_return = Returns) %>%
  dplyr::select(-Time,-CommodityCode,-ContractMonth,-YearCode,-Roll_Flag) %>%
  mutate(
    cattle_future_1 = lag(cattle_future,1),
    cattle_return_1 = lag(cattle_return,1),
    cattle_future_2 = lag(cattle_future,2),
    cattle_return_2 = lag(cattle_return,2),
    cattle_future_3 = lag(cattle_future,3),
    cattle_return_3 = lag(cattle_return,3),
    cattle_future_4 = lag(cattle_future,4),
    cattle_return_4 = lag(cattle_return,4),
    cattle_future_5 = lag(cattle_future,5),
    cattle_return_5 = lag(cattle_return,5),
    cattle_future_6 = lag(cattle_future,6),
    cattle_return_6 = lag(cattle_return,6),
    cattle_future_22 = lag(cattle_future,22),
    cattle_delta_week = (cattle_future_1-cattle_future_6)/cattle_future_6,
    cattle_delta_month = (cattle_future_1-cattle_future_22)/cattle_future_22
  )

# =======================
# USDA report / S&P
# =======================
library(dplyr)
report <- read_excel("/Users/yanyuma/Downloads/0research/05 Ag Barometer Data/Data/Copy of AG Report Dates 2018_2025_final.xlsx")
colnames(report)[1] <- "tradingDay"
report$tradingDay <- as.Date(report$tradingDay)
report <- report %>%
  dplyr::select(tradingDay, WASDE, CPZS, GS, CROP, `Cattle Combined`)
report <- report %>%
  mutate(WASDE_minus1 = lead(WASDE, n = 1, default = 0),  
         WASDE_plus1  = lag(WASDE, n = 1, default = 0), 
         CPZS_minus1  = lead(CPZS, n = 1, default = 0),  
         CPZS_plus1   = lag(CPZS, n = 1, default = 0),  
         GS_minus1    = lead(GS, n = 1, default = 0),  
         GS_plus1     = lag(GS, n = 1, default = 0),  
         CROP_minus1  = lead(CROP, n = 1, default = 0),  
         CROP_plus1   = lag(CROP, n = 1, default = 0),  
         `Cattle Combined_minus1` = lead(`Cattle Combined`, n = 1, default = 0),  
         `Cattle Combined_plus1`  = lag(`Cattle Combined`, n = 1, default = 0)
   )

SP <- read_excel("/Users/yanyuma/Downloads/0research/05 Ag Barometer Data/Data/PerformanceGraphExport (7).xls",
                 skip = 6)
colnames(SP)[1] <- "tradingDay"
SP$tradingDay <- as.Date(SP$tradingDay, origin = "1899-12-30")

# =======================
# merge data 
# =======================
price_list <- list(Corn,Cotton,Hogs,Cattle,Soybeans,Wheat,Corn_future,Cotton_future,Hogs_future,Cattle_future,Soybeans_future,Wheat_future)
dat_combined <- reduce(
  list(Corn,Cotton,Hogs,Cattle,Soybeans,Wheat,Corn_future,Cotton_future,Hogs_future,Cattle_future,Soybeans_future,Wheat_future),
  ~ left_join(.x, .y, by = "tradingDay"),
  .init = dat
)
dat_combined <- reduce(
  list(dat_combined, report, SP),
  ~ left_join(.x, .y, by = "tradingDay")
)
write.csv(dat_combined, "dat_combined.csv", row.names = FALSE) # NOTE:change the save path if you need 

# =============================================
# survey corn/soybean future price expectation 
# =============================================
nrow(dat_combined)
table(dat_combined$corn_future_exceed)
table(dat_combined$corn_future_below)
table(dat_combined$soybean_future_exceed)
table(dat_combined$soybean_future_below)

# Do you think July 2020 corn futures prices will exceed $4.50 per bushel, between now and July 1? Yes or No?
#	Do you think July 2020 corn futures prices will fall below $3.50 per bushel between now and July 1? Yes or No?

# 0: A-A uncertain with big change
# 1: A-B / A-C up(optimistic)
# 2: C-B / B-B / C-C / B-C uncertain
# 3: C-A / B-A down(pessimistic)
table(dat_combined$corn_future_exceed, dat_combined$corn_future_below)
table(dat_combined$soybean_future_exceed, dat_combined$soybean_future_below)
dat_combined <- dat_combined %>%
  mutate(corn_future_exp = case_when(
    corn_future_exceed == "A" & corn_future_below == "A" ~ 0,
    corn_future_exceed == "A" & corn_future_below %in% c("B", "C") ~ 1,
    corn_future_exceed == "B" & corn_future_below == "A" ~ 3,
    corn_future_exceed == "C" & corn_future_below == "A" ~ 3,
    corn_future_exceed %in% c("B", "C") & corn_future_below %in% c("B", "C") ~ 2),
    soybean_future_exp = case_when(
    soybean_future_exceed == "A" & soybean_future_below == "A" ~ 0,
    soybean_future_exceed == "A" & soybean_future_below %in% c("B", "C") ~ 1,
    soybean_future_exceed == "B" & soybean_future_below == "A" ~ 3,
    soybean_future_exceed == "C" & soybean_future_below == "A" ~ 3,
    soybean_future_exceed %in% c("B", "C") & soybean_future_below %in% c("B", "C") ~ 2)
  )
table(dat_combined$corn_future_exp, useNA = "ifany")
table(dat_combined$soybean_future_exp, useNA = "ifany")

dat_combined$corn_future_exp <- factor(dat_combined$corn_future_exp)
dat_combined$soybean_future_exp <- factor(dat_combined$soybean_future_exp)

# generate time series of agricultural products with the rate of change of price
commodities <- c("corn", "cotton", "hogs", "cattle", "soybeans", "wheat","corn_future", "cotton_future", "hogs_future", "cattle_future", "soybeans_future", "wheat_future")
for (var in commodities) {
  delta_name <- paste0("delta_", var, "_1")
  dat_combined[[delta_name]] <- (dat_combined[[var]] - dat_combined[[paste0(var, "_1")]]) / dat_combined[[paste0(var, "_1")]]
  
  for (k in 2:6) {
    delta_k_name <- paste0("delta_", var, "_", k)
    var_1 <- paste0(var, "_1")
    var_k <- paste0(var, "_", k)
    dat_combined[[delta_k_name]] <- (dat_combined[[var_1]] - dat_combined[[var_k]]) / dat_combined[[var_k]]
  }
}

# Create a month_year fixed-effect variable
dat_combined$month_year <- format(dat_combined$tradingDay, "%Y-%m")
dat_combined$month_year <- factor(dat_combined$month_year,
                                  levels = sort(unique(dat_combined$month_year)))

# generate new data set with days lag
# dat_set1: ignore high uncertainty
# dat_set2: combine low and high uncertainty (USE THIS)
dat_set1 <- dat_combined %>%
  filter(!(corn_future_exceed == "A" & corn_future_below == "A")) %>%
  filter(!(soybean_future_exceed == "A" & soybean_future_below == "A"))

dat_set2 <- dat_combined %>%
  mutate(
    corn_future_exp = case_when(
      corn_future_exceed == "A" & corn_future_below %in% c("B", "C") ~ 1,   # optimistic
      corn_future_exceed %in% c("B", "C") & corn_future_below %in% c("B", "C") ~ 2,  
      corn_future_exceed == "A" & corn_future_below == "A" ~ 2,             # uncertain
      corn_future_exceed %in% c("B", "C") & corn_future_below == "A" ~ 3    # pessimistic
    ),
    soybean_future_exp = case_when(
      soybean_future_exceed == "A" & soybean_future_below %in% c("B", "C") ~ 1,
      soybean_future_exceed %in% c("B", "C") & soybean_future_below %in% c("B", "C") ~ 2,
      soybean_future_exceed == "A" & soybean_future_below == "A" ~ 2,
      soybean_future_exceed %in% c("B", "C") & soybean_future_below == "A" ~ 3
    )
  )

# ==================================================
# ordered future + optimistic(1)/low + high uncertain(2)/pessimistic(3)
# report  +  SP
# ==================================================
dat_set2$corn_future_exp_ord <- factor(dat_set2$corn_future_exp,
                                       levels = c("3", "2", "1"),
                                       ordered = TRUE)

dat_set2$soybean_future_exp_ord <- factor(dat_set2$soybean_future_exp,
                                          levels = c("3", "2", "1"),
                                          ordered = TRUE)

# WARNING: design appears to be rank-deficient, so dropping some coefs
# JUST THREE COEFS LEFT 
# ord_corn_future_report2 <- polr(
#   corn_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 +
#                         delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
#                         corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week +
#                         cattle_delta_week + wheat_delta_week +
#                         corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month +
#                         cattle_delta_month + wheat_delta_month +
#                         WASDE + WASDE_minus1 + WASDE_plus1 + CPZS + CPZS_minus1 + CPZS_plus1 +
#                         GS + GS_minus1 + GS_plus1 + CROP + CROP_minus1 + CROP_plus1 +
#                         `Cattle Combined`+ `Cattle Combined_minus1` + `Cattle Combined_plus1` +
#                         `S&P 500` + factor(month_year),
#   data = dat_set2,
#   Hess = TRUE
# )
# summary(ord_corn_future_report2)

# ==============================================================
# Changes in corn, soybeans, cotton, hogs, cattle, wheat (baseline)
# ==============================================================
ord_corn_future_report_1<- polr(
  corn_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month,
  data = dat_set2,
  Hess = TRUE
)
summary(ord_corn_future_report_1)

ord_soybean_future_report_1 <- polr(
  soybean_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month,
  data = dat_set2,
  Hess = TRUE
)
summary(ord_soybean_future_report_1)

# ==============================================================
# Changes in corn, soybeans, cotton, hogs, cattle, wheat (baseline)
# + month-year (survey) fixed effects
# ==============================================================
ord_corn_future_report_2 <- polr(
  corn_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_corn_future_report_2)

ord_soybean_future_report_2 <- polr(
  soybean_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_soybean_future_report_2)

# ==============================================================
# Changes in corn, soybeans, cotton, hogs, cattle, wheat (baseline)
# + month-year (survey) fixed effects
# + WASDE dummy for day before, day of, day after
# ==============================================================
ord_corn_future_report_3 <- polr(
  corn_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    WASDE + WASDE_minus1 + WASDE_plus1 + 
    factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_corn_future_report_3)

ord_soybean_future_report_3 <- polr(
  soybean_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    WASDE + WASDE_minus1 + WASDE_plus1 + 
    factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_soybean_future_report_3)

# ==============================================================
# Changes in corn, soybeans, cotton, hogs, cattle, wheat (baseline)
# + month-year (survey) fixed effects
# + WASDE dummy for day before, day of, day after
# + S&P500 - daily price
# ==============================================================
ord_corn_future_report_4 <- polr(
  corn_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    WASDE + WASDE_minus1 + WASDE_plus1 + 
    `S&P 500` + factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_corn_future_report_4)

ord_soybean_future_report_4 <- polr(
  soybean_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    WASDE + WASDE_minus1 + WASDE_plus1 + 
    `S&P 500` + factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_soybean_future_report_4)






#################################################################
# Gradient Boosting for Ordinal Classification # 
#################################################################
library(dplyr)
library(xgboost)
library(caret)
library(Matrix)

# test set/ out of sample 
set.seed(123)
idx <- createDataPartition(dat_set2$corn_future_exp_ord,
                           p = 0.7, list = FALSE)

train <- dat_set2[idx, ]
test  <- dat_set2[-idx, ]

# Ordered Logit
ord_logit <- polr(
  corn_future_exp_ord ~
    delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    WASDE + WASDE_minus1 + WASDE_plus1 + 
    `S&P 500` + factor(month_year),
  data = train,
  Hess = TRUE
)

pred_ord <- predict(ord_logit, newdata = test, type = "class")
confusionMatrix(pred_ord, test$corn_future_exp_ord)

# XGBoost
## Constructing the design matrix
x_vars <- c(
  "delta_corn_future_1",
  "delta_soybeans_future_1",
  "delta_wheat_future_1",
  "WASDE", "WASDE_minus1", "WASDE_plus1",
  "S&P 500"
)

train_clean <- train %>% dplyr::select(all_of(x_vars), corn_future_exp_ord) %>% na.omit()
test_clean  <- test  %>% dplyr::select(all_of(x_vars), corn_future_exp_ord) %>% na.omit()

X_train <- model.matrix(~ . - 1, data = train_clean[, x_vars])
y_train <- as.numeric(train_clean$corn_future_exp_ord) - 1

X_test  <- model.matrix(~ . - 1, data = test_clean[, x_vars])
y_test  <- as.numeric(test_clean$corn_future_exp_ord) - 1

## train XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

params <- list(
  objective = "multi:softmax",
  num_class = 3,
  eval_metric = "mlogloss",
  max_depth = 4,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  verbose = 0
)

## predict
pred_xgb <- predict(xgb_model, dtest)
pred_xgb <- factor(pred_xgb + 1,
                   levels = 1:3,
                   labels = levels(test$corn_future_exp_ord))

confusionMatrix(pred_xgb, test_clean$corn_future_exp_ord)









#################################################################
##### FOLLOWING are attempts using other models and data #####
#################################################################
# ==================================================
# MNL future + optimistic/pessimistic/low uncertain
# ==================================================
model_corn_future_short_set1 <- multinom(
  as.factor(corn_future_exp) ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 
                            + corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + cattle_delta_week + wheat_delta_week
                            + corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + cattle_delta_month + wheat_delta_month
                            + factor(month_year),
  data = dat_set1
)
summary(model_corn_future_short_set1)

model_soybean_future_short_set1 <- multinom(
  as.factor(soybean_future_exp) ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 
                                + corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + cattle_delta_week + wheat_delta_week
                                + corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + cattle_delta_month + wheat_delta_month
                                + factor(month_year),
  data = dat_set1
)
summary(model_soybean_future_short_set1)

# ==================================================
# MNL future + optimistic/pessimistic/low + high uncertain
# ==================================================
model_corn_future_short_set2 <- multinom(
  as.factor(corn_future_exp) ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 
  + corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + cattle_delta_week + wheat_delta_week
  + corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + cattle_delta_month + wheat_delta_month
  + factor(month_year),
  data = dat_set2
)
summary(model_corn_future_short_set2)

model_soybean_future_short_set2 <- multinom(
  as.factor(soybean_future_exp) ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 
  + corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + cattle_delta_week + wheat_delta_week
  + corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + cattle_delta_month + wheat_delta_month
  + factor(month_year),
  data = dat_set2
)
summary(model_soybean_future_short_set2)

# ==================================================
# ordered future + optimistic/pessimistic/low uncertain
# ==================================================
dat_set1$corn_future_exp_ord <- factor(dat_set1$corn_future_exp,
                                       levels = c("3", "2", "1"),
                                       ordered = TRUE)

ord_corn_future_set1 <- polr(
  corn_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    factor(month_year),
  data = dat_set1,
  Hess = TRUE
)
summary(ord_corn_future_set1)

dat_set1$soybean_future_exp_ord <- factor(dat_set1$soybean_future_exp,
                                          levels = c("3", "2", "1"),
                                          ordered = TRUE)

ord_soybean_future_set1 <- polr(
  soybean_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    factor(month_year),
  data = dat_set1,
  Hess = TRUE
)
summary(ord_soybean_future_set1)

# ==================================================
# ordered future + optimistic/pessimistic/low + high uncertain
# ==================================================
# positive and negative 
make_pos_neg <- function(x, prefix) {
  out <- data.frame(
    setNames(list(ifelse(x >= 0, 1, 0)), paste0(prefix, "_pos")),
    setNames(list(ifelse(x < 0, 1, 0)),  paste0(prefix, "_neg"))
  )
  return(out)
}

vars_yesterday  <- c("delta_corn_future_1", "delta_soybeans_future_1",
                  "delta_cotton_future_1", "delta_hogs_future_1",
                  "delta_cattle_future_1", "delta_wheat_future_1")

vars_week    <- c("corn_delta_week", "soybeans_delta_week", "cotton_delta_week",
                  "hogs_delta_week", "cattle_delta_week", "wheat_delta_week")

vars_month   <- c("corn_delta_month", "soybeans_delta_month", "cotton_delta_month",
                  "hogs_delta_month", "cattle_delta_month", "wheat_delta_month")

for (v in c(vars_yesterday, vars_week, vars_month)) {
  dat_set2 <- cbind(dat_set2, make_pos_neg(dat_set2[[v]], v))
}

# for corn expectation
dat_set2$corn_future_exp_ord <- factor(dat_set2$corn_future_exp,
                                       levels = c("3", "2", "1"),
                                       ordered = TRUE)

all_vars <- c(
  paste0(vars_yesterday,  "_pos"),  paste0(vars_yesterday,  "_neg"),
  paste0(vars_week,    "_pos"),  paste0(vars_week,    "_neg"),
  paste0(vars_month,   "_pos"),  paste0(vars_month,   "_neg"),
  "factor(month_year)"
)

form <- as.formula(
  paste("corn_future_exp_ord ~", paste(all_vars, collapse = " + "))
)

ord_corn_future_posneg_set2 <- polr(
   form,
   data = dat_set2,
   Hess = TRUE
 )
summary(ord_corn_future_posneg_set2)

# for soybean expectation
dat_set2$soybean_future_exp_ord <- factor(
  dat_set2$soybean_future_exp,
  levels = c("3", "2", "1"),
  ordered = TRUE
)

form_soy <- as.formula(
  paste("soybean_future_exp_ord ~", paste(all_vars, collapse = " + "))
)

ord_soybean_future_posneg_set2 <- polr(
  form_soy,
  data = dat_set2,
  Hess = TRUE
)

summary(ord_soybean_future_posneg_set2)

# positive & zero range & negative 
library(ggplot2)

p40 <- quantile(dat_set2$delta_corn_future_1, 0.40, na.rm = TRUE)
p60 <- quantile(dat_set2$delta_corn_future_1, 0.60, na.rm = TRUE)

ggplot(dat_set2, aes(x = delta_corn_future_1)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = p40, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = p60, color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = p40, y = 0, label = "P40", vjust = -1, color = "red", size = 5) +
  annotate("text", x = p60, y = 0, label = "P60", vjust = -1, color = "blue", size = 5) +
  labs(
    title = "Density of delta_corn_future_1 with 40% and 60% Quantiles",
    x = "delta_corn_future_1",
    y = "Density"
  ) +
  theme_minimal()

vars_all <- c(
"delta_corn_future_1", "delta_soybeans_future_1", "delta_cotton_future_1",
"delta_hogs_future_1", "delta_cattle_future_1", "delta_wheat_future_1",
"corn_delta_week", "soybeans_delta_week", "cotton_delta_week",
"hogs_delta_week", "cattle_delta_week", "wheat_delta_week",
"corn_delta_month", "soybeans_delta_month", "cotton_delta_month",
"hogs_delta_month", "cattle_delta_month", "wheat_delta_month"
)

for (v in vars_all) {
  
  p40 <- quantile(dat_set2[[v]], 0.40, na.rm = TRUE)
  p60 <- quantile(dat_set2[[v]], 0.60, na.rm = TRUE)
  p40 <- min(p40, 0)
  
  dat_set2[[paste0(v, "_pos")]] <- ifelse(dat_set2[[v]] > p60, 1, 0)
  dat_set2[[paste0(v, "_neg")]] <- ifelse(dat_set2[[v]] < p40, 1, 0)
}

all_vars <- c(
  paste0(vars_all, "_pos"),
  paste0(vars_all, "_neg"),
  "factor(month_year)"
)

form <- as.formula(
  paste("corn_future_exp_ord ~", paste(all_vars, collapse = " + "))
)

ord_corn_future_mid20_set2 <- polr(
  form,
  data = dat_set2,
  Hess = TRUE
)

summary(ord_corn_future_mid20_set2)

# for soybean expectation
form_soy <- as.formula(
  paste("soybean_future_exp_ord ~", paste(all_vars, collapse = " + "))
)

ord_soybean_future_mid20_set2 <- polr(
  form_soy,
  data = dat_set2,
  Hess = TRUE
)
summary(ord_soybean_future_mid20_set2)

ord_corn_future_set2 <- polr(
  corn_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 +
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week +
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month +
    cattle_delta_month + wheat_delta_month +
    factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_corn_future_set2)


dat_set2$soybean_future_exp_ord <- factor(dat_set2$soybean_future_exp,
                                          levels = c("3", "2", "1"),
                                          ordered = TRUE)

ord_soybean_future_set2 <- polr(
  soybean_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_soybean_future_set2)

# ==================================================
# ordered future + optimistic/pessimistic/low + high uncertain
# report  +  SP
# ==================================================
dat_set2$corn_future_exp_ord <- factor(dat_set2$corn_future_exp,
                                       levels = c("3", "2", "1"),
                                       ordered = TRUE)

# WARNING: design appears to be rank-deficient, so dropping some coefs
# JUST THREE COEFS LEFT 
# ord_corn_future_report2 <- polr(
#   corn_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 +
#                         delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
#                         corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week +
#                         cattle_delta_week + wheat_delta_week +
#                         corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month +
#                         cattle_delta_month + wheat_delta_month +
#                         WASDE + WASDE_minus1 + WASDE_plus1 + CPZS + CPZS_minus1 + CPZS_plus1 +
#                         GS + GS_minus1 + GS_plus1 + CROP + CROP_minus1 + CROP_plus1 +
#                         `Cattle Combined`+ `Cattle Combined_minus1` + `Cattle Combined_plus1` +
#                         `S&P 500` + factor(month_year),
#   data = dat_set2,
#   Hess = TRUE
# )
# summary(ord_corn_future_report2)

# NOTES:DIFFERENT RESULT WITH DIFFERENT REPORT 
ord_corn_future_report <- polr(
  corn_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    WASDE + WASDE_minus1 + WASDE_plus1 + 
    `S&P 500` + factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_corn_future_report)

dat_set2$soybean_future_exp_ord <- factor(dat_set2$soybean_future_exp,
                                          levels = c("3", "2", "1"),
                                          ordered = TRUE)

ord_soybean_future_report <- polr(
  soybean_future_exp_ord ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + 
    delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 +
    corn_delta_week + soybeans_delta_week + cotton_delta_week + hogs_delta_week + 
    cattle_delta_week + wheat_delta_week +
    corn_delta_month + soybeans_delta_month + cotton_delta_month + hogs_delta_month + 
    cattle_delta_month + wheat_delta_month +
    WASDE + WASDE_minus1 + WASDE_plus1 + 
    `S&P 500` + factor(month_year),
  data = dat_set2,
  Hess = TRUE
)
summary(ord_soybean_future_report)


# ==================================================
# optimistic/pessimistic/low uncertain/high uncertain
# ==================================================
# ==================================================
# multinominal logistic regression for cash contract 
# ==================================================
# Each historical period has an equal share
# short term model: use one lag differences
model_corn_short <- multinom(
   as.factor(corn_future_exp) ~ delta_corn_1 + delta_soybeans_1 + delta_cotton_1 + delta_hogs_1 + delta_cattle_1 + delta_wheat_1 + factor(month_year),
   data = dat_combined
  )
summary(model_corn_short)

model_soybean_short <- multinom(
  as.factor(soybean_future_exp) ~ delta_corn_1 + delta_soybeans_1 + delta_cotton_1 + delta_hogs_1 + delta_cattle_1 + delta_wheat_1 + factor(month_year),
  data = dat_combined
)
summary(model_soybean_short )

# long-term model: use multiple lag differences (1~6)
model_corn_long <- multinom(
  as.factor(corn_future_exp) ~ 
    delta_corn_1 + delta_corn_2 + delta_corn_3 + delta_corn_4 + delta_corn_5 + delta_corn_6 +
    delta_soybeans_1 + delta_soybeans_2 + delta_soybeans_3 + delta_soybeans_4 + delta_soybeans_5 + delta_soybeans_6 +
    delta_cotton_1 + delta_cotton_2 + delta_cotton_3 + delta_cotton_4 + delta_cotton_5 + delta_cotton_6 +
    delta_hogs_1 + delta_hogs_2 + delta_hogs_3 + delta_hogs_4 + delta_hogs_5 + delta_hogs_6 +
    delta_cattle_1 + delta_cattle_2 + delta_cattle_3 + delta_cattle_4 + delta_cattle_5 + delta_cattle_6 +
    delta_wheat_1 + delta_wheat_2 + delta_wheat_3 + delta_wheat_4 + delta_wheat_5 + delta_wheat_6 +
    factor(month_year),
  data = dat_combined
)

summary(model_corn_long)

model_soybean_long <- multinom(
  as.factor(soybean_future_exp) ~ 
    delta_corn_1 + delta_corn_2 + delta_corn_3 + delta_corn_4 + delta_corn_5 + delta_corn_6 +
    delta_soybeans_1 + delta_soybeans_2 + delta_soybeans_3 + delta_soybeans_4 + delta_soybeans_5 + delta_soybeans_6 +
    delta_cotton_1 + delta_cotton_2 + delta_cotton_3 + delta_cotton_4 + delta_cotton_5 + delta_cotton_6 +
    delta_hogs_1 + delta_hogs_2 + delta_hogs_3 + delta_hogs_4 + delta_hogs_5 + delta_hogs_6 +
    delta_cattle_1 + delta_cattle_2 + delta_cattle_3 + delta_cattle_4 + delta_cattle_5 + delta_cattle_6 +
    delta_wheat_1 + delta_wheat_2 + delta_wheat_3 + delta_wheat_4 + delta_wheat_5 + delta_wheat_6 +
    factor(month_year),
  data = dat_combined
)

summary(model_soybean_long)

# yesterday VS moving average for history 
dat_combined <- dat_combined %>%
  mutate(
    # Corn moving average from lag 2 to lag 6
    delta_corn_MA = rowMeans(dplyr::select(., delta_corn_2, delta_corn_3, delta_corn_4, delta_corn_5, delta_corn_6), na.rm = TRUE),
    # Soybeans moving average
    delta_soy_MA = rowMeans(dplyr::select(., delta_soybeans_2, delta_soybeans_3, delta_soybeans_4, delta_soybeans_5, delta_soybeans_6), na.rm = TRUE),
    # Cotton moving average
    delta_cotton_MA = rowMeans(dplyr::select(., delta_cotton_2, delta_cotton_3, delta_cotton_4, delta_cotton_5, delta_cotton_6), na.rm = TRUE),
    # Hogs moving average
    delta_hogs_MA = rowMeans(dplyr::select(., delta_hogs_2, delta_hogs_3, delta_hogs_4, delta_hogs_5, delta_hogs_6), na.rm = TRUE),
    # Cattle moving average
    delta_cattle_MA = rowMeans(dplyr::select(., delta_cattle_2, delta_cattle_3, delta_cattle_4, delta_cattle_5, delta_cattle_6), na.rm = TRUE),
    # Wheat moving average
    delta_wheat_MA = rowMeans(dplyr::select(., delta_wheat_2, delta_wheat_3, delta_wheat_4, delta_wheat_5, delta_wheat_6), na.rm = TRUE)
  )

# long-term model: use multiple lag differences (1~6)
model_corn_long_MA <- multinom(
  as.factor(corn_future_exp) ~ 
    delta_corn_1 + delta_corn_MA +
    delta_soybeans_1 + delta_soy_MA +
    delta_cotton_1 + delta_cotton_MA +
    delta_hogs_1 + delta_hogs_MA +
    delta_cattle_1 + delta_cattle_MA +
    delta_wheat_1 + delta_wheat_MA +
    factor(month_year),
  data = dat_combined
)
summary(model_corn_long_MA)

model_soybean_long_MA <- multinom(
  as.factor(soybean_future_exp) ~ 
    delta_corn_1 + delta_corn_MA +
    delta_soybeans_1 + delta_soy_MA +
    delta_cotton_1 + delta_cotton_MA +
    delta_hogs_1 + delta_hogs_MA +
    delta_cattle_1 + delta_cattle_MA +
    delta_wheat_1 + delta_wheat_MA +
    factor(month_year),
  data = dat_combined
)
summary(model_soybean_long_MA)


# ==================================================
# multinominal logistic regression for future
# ==================================================

# Each historical period has an equal share
# short term model: use one lag differences
model_corn_future_short <- multinom(
  as.factor(corn_future_exp) ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 + factor(month_year),
  data = dat_combined
)
summary(model_corn_future_short)

model_soybean_future_short <- multinom(
  as.factor(soybean_future_exp) ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 + factor(month_year),
  data = dat_combined
)
summary(model_soybean_future_short )

# long-term model: use multiple lag differences (1~6)
model_corn_future_long <- multinom(
  as.factor(corn_future_exp) ~ 
    delta_corn_future_1 + delta_corn_future_2 + delta_corn_future_3 + delta_corn_future_4 + delta_corn_future_5 + delta_corn_future_6 +
    delta_soybeans_future_1 + delta_soybeans_future_2 + delta_soybeans_future_3 + delta_soybeans_future_4 + delta_soybeans_future_5 + delta_soybeans_future_6 +
    delta_cotton_future_1 + delta_cotton_future_2 + delta_cotton_future_3 + delta_cotton_future_4 + delta_cotton_future_5 + delta_cotton_future_6 +
    delta_hogs_future_1 + delta_hogs_future_2 + delta_hogs_future_3 + delta_hogs_future_4 + delta_hogs_future_5 + delta_hogs_future_6 +
    delta_cattle_future_1 + delta_cattle_future_2 + delta_cattle_future_3 + delta_cattle_future_4 + delta_cattle_future_5 + delta_cattle_future_6 +
    delta_wheat_future_1 + delta_wheat_future_2 + delta_wheat_future_3 + delta_wheat_future_4 + delta_wheat_future_5 + delta_wheat_future_6 +
    factor(month_year),
  data = dat_combined
)

summary(model_corn_future_long)

model_soybean_future_long <- multinom(
  as.factor(soybean_future_exp) ~ 
    delta_corn_future_1 + delta_corn_future_2 + delta_corn_future_3 + delta_corn_future_4 + delta_corn_future_5 + delta_corn_future_6 +
    delta_soybeans_future_1 + delta_soybeans_future_2 + delta_soybeans_future_3 + delta_soybeans_future_4 + delta_soybeans_future_5 + delta_soybeans_future_6 +
    delta_cotton_future_1 + delta_cotton_future_2 + delta_cotton_future_3 + delta_cotton_future_4 + delta_cotton_future_5 + delta_cotton_future_6 +
    delta_hogs_future_1 + delta_hogs_future_2 + delta_hogs_future_3 + delta_hogs_future_4 + delta_hogs_future_5 + delta_hogs_future_6 +
    delta_cattle_future_1 + delta_cattle_future_2 + delta_cattle_future_3 + delta_cattle_future_4 + delta_cattle_future_5 + delta_cattle_future_6 +
    delta_wheat_future_1 + delta_wheat_future_2 + delta_wheat_future_3 + delta_wheat_future_4 + delta_wheat_future_5 + delta_wheat_future_6 +
    factor(month_year),
  data = dat_combined
)

summary(model_soybean_future_long)

# yesterday VS moving average for history 
dat_combined <- dat_combined %>%
  mutate(
    # Corn future moving average from lag 2 to lag 6
    delta_corn_future_MA = rowMeans(dplyr::select(., delta_corn_future_2, delta_corn_future_3, delta_corn_future_4, delta_corn_future_5, delta_corn_future_6), na.rm = TRUE),
    # Soybeans future moving average
    delta_soy_future_MA = rowMeans(dplyr::select(., delta_soybeans_future_2, delta_soybeans_future_3, delta_soybeans_future_4, delta_soybeans_future_5, delta_soybeans_future_6), na.rm = TRUE),
    # Cotton future moving average
    delta_cotton_future_MA = rowMeans(dplyr::select(., delta_cotton_future_2, delta_cotton_future_3, delta_cotton_future_4, delta_cotton_future_5, delta_cotton_future_6), na.rm = TRUE),
    # Hogs future moving average
    delta_hogs_future_MA = rowMeans(dplyr::select(., delta_hogs_future_2, delta_hogs_future_3, delta_hogs_future_4, delta_hogs_future_5, delta_hogs_future_6), na.rm = TRUE),
    # Cattle future moving average
    delta_cattle_future_MA = rowMeans(dplyr::select(., delta_cattle_future_2, delta_cattle_future_3, delta_cattle_future_4, delta_cattle_future_5, delta_cattle_future_6), na.rm = TRUE),
    # Wheat future moving average
    delta_wheat_future_MA = rowMeans(dplyr::select(., delta_wheat_future_2, delta_wheat_future_3, delta_wheat_future_4, delta_wheat_future_5, delta_wheat_future_6), na.rm = TRUE)
  )

# long-term model: use multiple lag differences (1~6)
model_corn_future_long_MA <- multinom(
  as.factor(corn_future_exp) ~ 
    delta_corn_future_1 + delta_corn_future_MA +
    delta_soybeans_future_1 + delta_soy_future_MA +
    delta_cotton_future_1 + delta_cotton_future_MA +
    delta_hogs_future_1 + delta_hogs_future_MA +
    delta_cattle_future_1 + delta_cattle_future_MA +
    delta_wheat_future_1 + delta_wheat_future_MA +
    factor(month_year),
  data = dat_combined
)
summary(model_corn_future_long_MA)

model_soybean_future_long_MA <- multinom(
  as.factor(soybean_future_exp) ~ 
    delta_corn_future_1 + delta_corn_future_MA +
    delta_soybeans_future_1 + delta_soy_future_MA +
    delta_cotton_future_1 + delta_cotton_future_MA +
    delta_hogs_future_1 + delta_hogs_future_MA +
    delta_cattle_future_1 + delta_cattle_future_MA +
    delta_wheat_future_1 + delta_wheat_future_MA +
    factor(month_year),
  data = dat_combined
)
summary(model_soybean_future_long_MA)

# ==================================================
# ordered logit
# ==================================================
dat_ordered <- dat_combined %>%
  filter(corn_future_exp %in% c(1, 2, 3),
         soybean_future_exp %in% c(1, 2, 3)) %>% 
  mutate(
    corn_future_exp = factor(
      corn_future_exp,
      levels = c(3, 2, 1),     # order：3（down） < 2（uncertain） < 1（up）
      ordered = TRUE
    ),
    soybean_future_exp = factor(
      soybean_future_exp,
      levels = c(3, 2, 1),     # order：3（down） < 2（uncertain） < 1（up）
      ordered = TRUE
    )
  )

# ==================================================
# ordered logit regression for cash contract
# ==================================================
# Each historical period has an equal share
# short term model: use one lag differences
ord_corn_short <- polr(
  corn_future_exp ~ delta_corn_1 + delta_soybeans_1 + delta_cotton_1 + delta_hogs_1 + delta_cattle_1 + delta_wheat_1 + factor(month_year),
  data = dat_ordered,
  Hess = TRUE,
  method = "logistic"
)
summary(ord_corn_short)

ord_soybean_short <- polr(
  soybean_future_exp ~ delta_corn_1 + delta_soybeans_1 + delta_cotton_1 + delta_hogs_1 + delta_cattle_1 + delta_wheat_1 + factor(month_year),
  data = dat_ordered,
  Hess = TRUE,
  method = "logistic"
) 
summary(ord_soybean_short)
levels(dat_ordered$month_year)

# yesterday VS moving average for history 
dat_ordered <- dat_ordered %>%
  mutate(
    # Corn moving average from lag 2 to lag 6
    delta_corn_MA = rowMeans(dplyr::select(., delta_corn_2, delta_corn_3, delta_corn_4, delta_corn_5, delta_corn_6), na.rm = TRUE),
    # Soybeans moving average
    delta_soy_MA = rowMeans(dplyr::select(., delta_soybeans_2, delta_soybeans_3, delta_soybeans_4, delta_soybeans_5, delta_soybeans_6), na.rm = TRUE),
    # Cotton moving average
    delta_cotton_MA = rowMeans(dplyr::select(., delta_cotton_2, delta_cotton_3, delta_cotton_4, delta_cotton_5, delta_cotton_6), na.rm = TRUE),
    # Hogs moving average
    delta_hogs_MA = rowMeans(dplyr::select(., delta_hogs_2, delta_hogs_3, delta_hogs_4, delta_hogs_5, delta_hogs_6), na.rm = TRUE),
    # Cattle moving average
    delta_cattle_MA = rowMeans(dplyr::select(., delta_cattle_2, delta_cattle_3, delta_cattle_4, delta_cattle_5, delta_cattle_6), na.rm = TRUE),
    # Wheat moving average
    delta_wheat_MA = rowMeans(dplyr::select(., delta_wheat_2, delta_wheat_3, delta_wheat_4, delta_wheat_5, delta_wheat_6), na.rm = TRUE)
  )

# long-term model: use multiple lag differences (1~6)
ord_corn_long_MA <- polr(
  as.ordered(corn_future_exp) ~ 
    delta_corn_1 + delta_corn_MA +
    delta_soybeans_1 + delta_soy_MA +
    delta_cotton_1 + delta_cotton_MA +
    delta_hogs_1 + delta_hogs_MA +
    delta_cattle_1 + delta_cattle_MA +
    delta_wheat_1 + delta_wheat_MA +
    factor(month_year),
  data = dat_ordered,
  Hess = TRUE
)
summary(ord_corn_long_MA)

ord_soybean_long_MA <- polr(
  as.ordered(soybean_future_exp) ~ 
    delta_corn_1 + delta_corn_MA +
    delta_soybeans_1 + delta_soy_MA +
    delta_cotton_1 + delta_cotton_MA +
    delta_hogs_1 + delta_hogs_MA +
    delta_cattle_1 + delta_cattle_MA +
    delta_wheat_1 + delta_wheat_MA +
    factor(month_year),
  data = dat_ordered,
  Hess = TRUE
)
summary(ord_soybean_long_MA)

# ==================================================
# ordered logit regression for future
# ==================================================
# Each historical period has an equal share
# short term model: use one lag differences
ord_corn_future_short <- polr(
  corn_future_exp ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 + factor(month_year),
  data = dat_ordered,
  Hess = TRUE,
  method = "logistic"
)
summary(ord_corn_future_short)

ord_soybean_future_short <- polr(
  soybean_future_exp ~ delta_corn_future_1 + delta_soybeans_future_1 + delta_cotton_future_1 + delta_hogs_future_1 + delta_cattle_future_1 + delta_wheat_future_1 + factor(month_year),
  data = dat_ordered,
  Hess = TRUE,
  method = "logistic"
) 
summary(ord_soybean_future_short)

# yesterday VS moving average for history 
dat_ordered <- dat_ordered %>%
  mutate(
    # Corn moving average from lag 2 to lag 6
    delta_corn_future_MA = rowMeans(dplyr::select(., delta_corn_future_2, delta_corn_future_3, delta_corn_future_4, delta_corn_future_5, delta_corn_future_6), na.rm = TRUE),
    # Soybeans moving average
    delta_soy_future_MA = rowMeans(dplyr::select(., delta_soybeans_future_2, delta_soybeans_future_3, delta_soybeans_future_4, delta_soybeans_future_5, delta_soybeans_future_6), na.rm = TRUE),
    # Cotton moving average
    delta_cotton_future_MA = rowMeans(dplyr::select(., delta_cotton_future_2, delta_cotton_future_3, delta_cotton_future_4, delta_cotton_future_5, delta_cotton_future_6), na.rm = TRUE),
    # Hogs moving average
    delta_hogs_future_MA = rowMeans(dplyr::select(., delta_hogs_future_2, delta_hogs_future_3, delta_hogs_future_4, delta_hogs_future_5, delta_hogs_future_6), na.rm = TRUE),
    # Cattle moving average
    delta_cattle_future_MA = rowMeans(dplyr::select(., delta_cattle_future_2, delta_cattle_future_3, delta_cattle_future_4, delta_cattle_future_5, delta_cattle_future_6), na.rm = TRUE),
    # Wheat moving average
    delta_wheat_future_MA = rowMeans(dplyr::select(., delta_wheat_future_2, delta_wheat_future_3, delta_wheat_future_4, delta_wheat_future_5, delta_wheat_future_6), na.rm = TRUE)
  )

# long-term model: use multiple lag differences (1~6)
ord_corn_future_long_MA <- polr(
  as.factor(corn_future_exp) ~ 
    delta_corn_future_1 + delta_corn_future_MA +
    delta_soybeans_future_1 + delta_soy_future_MA +
    delta_cotton_future_1 + delta_cotton_future_MA +
    delta_hogs_future_1 + delta_hogs_future_MA +
    delta_cattle_future_1 + delta_cattle_future_MA +
    delta_wheat_future_1 + delta_wheat_future_MA +
    factor(month_year),
  data = dat_ordered,
  Hess = TRUE,
  method = "logistic"
)
summary(ord_corn_future_long_MA)

ord_soybean_future_long_MA <- polr(
  as.factor(soybean_future_exp) ~ 
    delta_corn_future_1 + delta_corn_future_MA +
    delta_soybeans_future_1 + delta_soy_future_MA +
    delta_cotton_future_1 + delta_cotton_future_MA +
    delta_hogs_future_1 + delta_hogs_future_MA +
    delta_cattle_future_1 + delta_cattle_future_MA +
    delta_wheat_future_1 + delta_wheat_future_MA +
    factor(month_year),
  data = dat_ordered,
  Hess = TRUE,
  method = "logistic"
)
summary(ord_soybean_future_long_MA)


