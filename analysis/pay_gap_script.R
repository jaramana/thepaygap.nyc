library(reshape2)
library(dplyr)
library(kableExtra)
library(tidyverse)
library(data.table)
library(DT)
library(scales)
library(jsonlite)





setwd("C:/Users/jaramana/Documents/Library/The Pay Gap/")


Citywide_Payroll = read.csv("Citywide_Payroll_Data.csv", header = TRUE)

Citywide_Payroll_2018_filtered <- filter(Citywide_Payroll, Fiscal.Year == 2018 & Pay.Basis == 'per Annum',	)

setnames(Citywide_Payroll_2018_filtered, old=c("Agency.Name"), new=c("AGENCY"))





Baby_Names = read.csv("NY.txt", header = FALSE)

setnames(Baby_Names, old=c("V1","V2","V3","V4","V5"), new=c("State","Sex","Year","Name","Count"))

Baby_Names[,"Name"] = toupper(Baby_Names[,"Name"])






Citywide_Payroll_2018_filtered$Base.Salary <- (Citywide_Payroll_2018_filtered$Base.Salary)





Citywide_Payroll_2018_filtered$Gender <- Baby_Names$Sex[match(Citywide_Payroll_2018_filtered$First.Name, Baby_Names$Name)]



Citywide_Payroll_2018_filtered <- filter(Citywide_Payroll_2018_filtered, !is.na(Gender))



Citywide_Payroll_2018_filtered <- setDT(Citywide_Payroll_2018_filtered)[,if(.N >100) .SD,by=AGENCY]




basic_summ = group_by(Citywide_Payroll_2018_filtered, AGENCY)


basic_summ = summarise(basic_summ,
                       FEMALE = 
                         paste('$',formatC(sprintf(mean(Base.Salary[Gender=="F"]),fmt = '%#.2f'), big.mark=',', format = 'f')),
                       MALE = 
                         paste('$',formatC(sprintf(mean(Base.Salary[Gender=="M"]),fmt = '%#.2f'), big.mark=',', format = 'f')),
                       GAP = 
                         percent(((mean(Base.Salary[Gender=="F"]) - mean(Base.Salary[Gender=="M"])) / ((mean(Base.Salary[Gender=="F"]) + mean(Base.Salary[Gender=="M"])))/2)))



write_json(basic_summ, "basic_summ.json", pretty = TRUE)
