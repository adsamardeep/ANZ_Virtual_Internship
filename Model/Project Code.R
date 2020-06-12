# Required packages
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(outliers)

# Dataset Description

ANZ <- read_excel("C:/Users/Amar/Documents/GitHub/ANZ_Virtual_Internship/data/ANZ synthesised transaction dataset.xlsx")

head(ANZ)

dim(ANZ)

colnames(ANZ)

summary(ANZ$card_present_flag)

summary(ANZ$bpay_biller_code)

summary(ANZ$merchant_code)

#Understand the data

ANZ1 <- ANZ %>% select(-currency, -country, -bpay_biller_code,-card_present_flag, -merchant_code)

dim(ANZ1)

summary(ANZ1)

ANZ1$status <- factor(ANZ1$status, levels = c("authorized","posted"))

ANZ1$txn_description <- factor(ANZ1$txn_description, levels = c("INTER BANK", "PAY/SALARY","PAYMENT","PHONEBANK","POS","SALES-POS"))

ANZ1$date <- ymd(ANZ1$date)

class(ANZ1$date)

ANZ1$gender <- factor(ANZ1$gender, levels=c("M","F"))

ANZ$extraction[1:6]

ANZ1$extraction <- ymd_hms(ANZ1$extraction)

class(ANZ1$extraction)

ANZ1$extraction[1:6]

ANZ1$movement <- factor(ANZ1$movement, levels=c("credit","debit"))

#Tidy & Manipulate Data I

ANZ$long_lat[1:6]

ANZ$merchant_long_lat[1:6]

ANZ1 <- ANZ1 %>% separate(long_lat, into = c("longitude","latitude"), sep=6)

ANZ1 <- ANZ1 %>% separate(merchant_long_lat, into = c("m_longitude","m_latitude"), sep=6)

head(ANZ1$longitude)

head(ANZ1$latitude)

head(ANZ1$m_latitude)

head(ANZ1$m_longitude)

#Tidy & Manipulate Data II

agebreaks <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,500)

agelabels <- c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85+")

setDT(ANZ1)[ , agegroups := cut(age, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]

head(ANZ1$agegroups)

ANZ1<-  mutate(ANZ1,transaction= ifelse(txn_description=="INTER BANK" | txn_description=="PAYMENT" |
                                          txn_description=="PHONE BANK" | txn_description=="POS" | 
                                          txn_description == "SALES-POS", ANZ1$amount, 0))

summary(ANZ1$transaction)

IQR = 42.78 - 12.77

UpperLimit = 1.5 *(IQR) + 42.78

UpperLimit

boxplot(ANZ1$transaction)

ANZ1 <- ANZ1 %>%
  mutate(transaction_volume = 
           case_when(transaction >= 0 & transaction <= 12.87 ~ "Low",
                     transaction > 12.87 & transaction <= 24.44 ~ "Low-Medium",
                     transaction > 24.44 & transaction <= 48.72 ~ "Medium",
                     transaction > 48.72 & transaction <= 94.495 ~ "Medium-High",
                     transaction > 94.495 & transaction <= 7081.09 ~ "High"))

ANZ1$transaction_volume <- factor(ANZ1$transaction_volume)

summary(ANZ1$transaction_volume)

Salary_Table <- ANZ1 %>% 
  group_by(customer_id) %>% 
  filter(txn_description=="PAY/SALARY") %>%   
  mutate(monthlysalary = (sum(amount))/3) %>%  
  select(account, customer_id, merchant_id, txn_description,
         first_name, gender,balance, date, age, merchant_suburb,
         amount, agegroups, monthlysalary, transaction)

Salary_Table <- Salary_Table %>%
  mutate(month = 
           case_when(date >= "2018-08-01" & date <= "2018-08-31" ~ "August",
                     date >= "2018-09-01" & date <= "2018-09-30" ~ "September",
                     date >= "2018-10-01" & date <= "2018-10-31" ~ "October"))

Salary_Table$month <- factor(Salary_Table$month)

sum(is.na(ANZ1$txn_description))

Missing_Txn <- ANZ1 %>%  filter(is.na(txn_description)) 

dim(Missing_Txn)

Missing_Txn$txn_description <- as.character(Missing_Txn$txn_description)

Missing_Txn[68,] <- Missing_Txn[68,] %>% mutate(txn_description = replace(txn_description, 
                                                                          is.na(txn_description),
                                                                          "PossibleFroud"))

Missing_Txn[18,] <- Missing_Txn[68,] %>% mutate(txn_description = replace(txn_description, 
                                                                          is.na(txn_description),
                                                                          "PossibleFroud"))
Missing_Txn[58,] <- Missing_Txn[68,] %>% mutate(txn_description = replace(txn_description, 
                                                                          is.na(txn_description),
                                                                          "PossibleFroud"))

Missing_Txn <- Missing_Txn %>%   mutate(txn_description  = replace(txn_description, 
                                                                   is.na(txn_description),
                                                                   "PaymentT"))

table(Missing_Txn$txn_description)

sum(is.na(Missing_Txn$txn_description))

ANZ2 <- ANZ1 %>%  filter(!is.na(ANZ1$txn_description))

ANZ3 <- bind_rows(ANZ2, Missing_Txn)

table(ANZ3$txn_description)

summary(Salary_Table$age)

IQR_Age = 40 - 22

UpperLimit_Age = 1.5 *(IQR) + 22

UpperLimit_Age

boxplot(Salary_Table$age)

plot(Salary_Table$monthlysalary~Salary_Table$agegroups)

boxplot(Salary_Table$monthlysalary)

cap <- function(x){
  quantiles <- quantile( x, c(.05, 0.25, 0.75, .95 ) )
  x[ x < quantiles[2] - 1.5*IQR(x) ] <- quantiles[1]
  x[ x > quantiles[3] + 1.5*IQR(x) ] <- quantiles[4]
  x
}

Salary_Table$monthlysalary <-  Salary_Table$monthlysalary  %>%  cap()

boxplot(Salary_Table$monthlysalary)

par(mfrow=c(2,2))

hist(Salary_Table$monthlysalary, main="Original Salary Histogram")

hist(log(Salary_Table$monthlysalary), main="Log Transformed Salary Histogram")

hist((Salary_Table$monthlysalary)^-2, main="Square Root Transformed Salary Hist")

hist(diff(Salary_Table$monthlysalary), main="Differenced Salary Histogram")



