##GenderGap_Agency_2019.R
##by Ahmad Shaibani, 2020

##Load Libraries
library(reshape2)
library(dplyr)
library(kableExtra)
library(tidyverse)
library(data.table)
library(DT)
library(scales)
library(jsonlite)
library(blscrapeR)

##Set working directory
setwd("C:/Users/jaramana/Documents/Library/Websites/thepaygap.nyc analysis")














#######################
##
##Load and filter data
##

##Citywide Payroll
Citywide_Payroll = read.csv("Citywide_Payroll_Data__Fiscal_Year_.csv", header = TRUE)
setnames(Citywide_Payroll, old=c("Agency.Name"), new=c("Agency"))
setnames(Citywide_Payroll, old=c("Title.Description"), new=c("Title"))

#Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == 2020 & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)

##Names Dataset for Gender Gap
Baby_Names = read.csv("NY.txt", header = FALSE)
setnames(Baby_Names, old=c("V1","V2","V3","V4","V5"), new=c("State","Sex","Year","Name","Count"))
Baby_Names[,"Name"] = toupper(Baby_Names[,"Name"])
##Prepare Names Dataset for aggregation
Baby_Names_pivot = group_by(Baby_Names, Name, Sex)
Baby_Names_pivot = summarise(Baby_Names_pivot,
                             Count = sum(Count))
Baby_Names_Spread = spread(Baby_Names_pivot, "Sex", "Count")
Baby_Names_Spread[is.na(Baby_Names_Spread)] <- 0
Baby_Names_Spread$M_weight <- Baby_Names_Spread$M/(Baby_Names_Spread$M + Baby_Names_Spread$F) 
Baby_Names_Spread$F_weight <- Baby_Names_Spread$F/(Baby_Names_Spread$M + Baby_Names_Spread$F) 
write.csv(Baby_Names_Spread, "baby_names_NY.csv", row.names=FALSE)
##Aggregate Names Dataset with Citywide Payroll
Citywide_Payroll$Male <- Baby_Names_Spread$M_weight[match(Citywide_Payroll$First.Name, Baby_Names_Spread$Name, nomatch = NA_integer_, incomparables = NULL)]
Citywide_Payroll$Female <- Baby_Names_Spread$F_weight[match(Citywide_Payroll$First.Name, Baby_Names_Spread$Name)]
Citywide_Payroll <- filter(Citywide_Payroll, !is.na(Male))
Citywide_Payroll <- filter(Citywide_Payroll, !is.na(Female))

##Inflation Data
df <- bls_api("CUSR0000SA0",
              startyear = 2014, endyear = 2020)
df = read.csv("bls_api.csv", header = TRUE)




#######################
##Analysis
##



#######################
##GENDER GAP BY AGENCY


#######################
##Aggregation for .csv
# Year list for loop
Years <-
  c(
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020"
  )

for (k in Years) {
  
  #Filter and do some math
  Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == k & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
  citywide_average <- mean(Citywide_Payroll_filtered$Base.Salary)
  citywide_average_m <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Male)
  citywide_average_f <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Female)
  
  #Aggregation for .csv
  basic_summ = group_by(Citywide_Payroll_filtered, Agency)
  
  #Summarize data
  summary = summarise(basic_summ,
                      Average = 
                        (mean(Base.Salary)),
                      Male = 
                        (weighted.mean(Base.Salary, Male)),
                      Female = 
                        (weighted.mean(Base.Salary, Female)))      
  
  #Run summary for entire City
  summary <- rbind(summary, data.frame(Agency="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Male=citywide_average_m, Female=citywide_average_f))
  
  #Adjust for inflation
  # Get CPI from base period (January 2014).
  base_cpi <- as.numeric(subset(df, year== k & periodName=="June", select = "value"))
  # Get the CPI for the new period (February 2015).
  new_cpi <- as.numeric(subset(df, year== 2020 & periodName=="June", select = "value"))
  # Calculate the updated value of our $100 investment.
  summary$Average <- (summary$Average / base_cpi) * new_cpi
  summary$Male <- (summary$Male / base_cpi) * new_cpi
  summary$Female <- (summary$Female / base_cpi) * new_cpi
  # Woops, looks like we lost a penny!
  
  #Summarize Gap
  summary$Gap <- percent((((summary$Male) - (summary$Female)) / ((summary$Male) + summary$Female)), .1)
  
  # Set column / dataframe names
  setnames(summary, old=c("Average"), new=c(paste0("Average_", k)))
  setnames(summary, old=c("Male"), new=c(paste0("Male_", k)))
  setnames(summary, old=c("Female"), new=c(paste0("Female_", k)))
  setnames(summary, old=c("Gap"), new=c(paste0("Gap_", k)))
  assign(paste0("summary_", k), data.frame(summary))
  
}

# Setup list of agencies/titles for reference
Citywide_Payroll_list <- 
  distinct(Citywide_Payroll_filtered %>%
             select(3))

# Merge dataframes into one
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2014,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2015,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2016,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2017,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2018,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2019,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2020,by="Agency", all=T)

# Write .csv
write.csv(Citywide_Payroll_list, "GenderGap_Agency.csv", row.names=FALSE)



#######################
##Aggregation for Chart
# Year list for loop
Years <-
  c(
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020"
  )

for (k in Years) {
  
  #Filter and do some math
  Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == k & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
  citywide_average <- mean(Citywide_Payroll_filtered$Base.Salary)
  citywide_average_m <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Male)
  citywide_average_f <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Female)
  Citywide_Payroll_filtered <- setDT(Citywide_Payroll_filtered)[,if(.N >200) .SD,by=Agency]
  
  #Aggregation for .csv
  basic_summ = group_by(Citywide_Payroll_filtered, Agency)
  
  #Summarize data
  summary = summarise(basic_summ,
                      Average = 
                        (mean(Base.Salary)),
                      Male = 
                        (weighted.mean(Base.Salary, Male)),
                      Female = 
                        (weighted.mean(Base.Salary, Female)))      
  
  #Run summary for entire City
  summary <- rbind(summary, data.frame(Agency="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Male=citywide_average_m, Female=citywide_average_f))
  
  #Adjust for inflation
  # Get CPI from base period (January 2014).
  base_cpi <- as.numeric(subset(df, year== k & periodName=="June", select = "value"))
  # Get the CPI for the new period (February 2015).
  new_cpi <- as.numeric(subset(df, year== 2020 & periodName=="June", select = "value"))
  # Calculate the updated value of our $100 investment.
  summary$Average <- (summary$Average / base_cpi) * new_cpi
  summary$Male <- (summary$Male / base_cpi) * new_cpi
  summary$Female <- (summary$Female / base_cpi) * new_cpi
  # Woops, looks like we lost a penny!
  
  #Summarize Gap
  summary$Gap <- percent((((summary$Male) - (summary$Female)) / ((summary$Male) + summary$Female)), .1)
  
  # Set column / dataframe names
  setnames(summary, old=c("Average"), new=c(paste0("Average_", k)))
  setnames(summary, old=c("Male"), new=c(paste0("Male_", k)))
  setnames(summary, old=c("Female"), new=c(paste0("Female_", k)))
  setnames(summary, old=c("Gap"), new=c(paste0("Gap_", k)))
  assign(paste0("summary_", k), data.frame(summary))
  
}

# Setup list of agencies/titles for reference
Citywide_Payroll_list <- 
  distinct(Citywide_Payroll_filtered %>%
             select(1))

# Merge dataframes into one
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2014,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2015,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2016,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2017,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2018,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2019,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2020,by="Agency", all=T)

# Write .csv
write.csv(Citywide_Payroll_list, "GenderGap_Agency_chart.csv", row.names=FALSE)



#######################
##Aggregation for Table
#Filter and do some math
Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == 2020 & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
citywide_average <- mean(Citywide_Payroll_filtered$Base.Salary)
citywide_average_m <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Male)
citywide_average_f <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Female)
Citywide_Payroll_filtered <- setDT(Citywide_Payroll_filtered)[,if(.N >200) .SD,by=Agency]

summary = summarise(basic_summ,
                    Average = 
                      (mean(Base.Salary)),
                    Male = 
                      (weighted.mean(Base.Salary, Male)),
                    Female = 
                      (weighted.mean(Base.Salary, Female))
)
summary <- rbind(summary, data.frame(Agency="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Male=citywide_average_m, Female=citywide_average_f))
summary$Average <- dollar(summary$Average)
summary$Gap <- percent((((summary$Male) - (summary$Female)) / ((summary$Male) + summary$Female)), .1)
summary$Male <- dollar(summary$Male)
summary$Female <- dollar(summary$Female)
write.csv(summary, "GenderGap_Agency_2020_table.csv", row.names=FALSE)






#######################
##GENDER GAP BY TITLE

#######################
##Aggregation for .csv
# Year list for loop
Years <-
  c(
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020"
  )

for (k in Years) {
  
  #Filter and do some math
  Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == k & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
  citywide_average <- mean(Citywide_Payroll_filtered$Base.Salary)
  citywide_average_m <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Male)
  citywide_average_f <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Female)
  
  #Aggregation for .csv
  basic_summ = group_by(Citywide_Payroll_filtered, Title)
  
  #Summarize data
  summary = summarise(basic_summ,
                      Average = 
                        (mean(Base.Salary)),
                      Male = 
                        (weighted.mean(Base.Salary, Male)),
                      Female = 
                        (weighted.mean(Base.Salary, Female)))      
  
  #Run summary for entire City
  summary <- rbind(summary, data.frame(Title="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Male=citywide_average_m, Female=citywide_average_f))
  
  #Adjust for inflation
  # Get CPI from base period (January 2014).
  base_cpi <- as.numeric(subset(df, year== k & periodName=="June", select = "value"))
  # Get the CPI for the new period (February 2015).
  new_cpi <- as.numeric(subset(df, year== 2020 & periodName=="June", select = "value"))
  # Calculate the updated value of our $100 investment.
  summary$Average <- (summary$Average / base_cpi) * new_cpi
  summary$Male <- (summary$Male / base_cpi) * new_cpi
  summary$Female <- (summary$Female / base_cpi) * new_cpi
  # Woops, looks like we lost a penny!
  
  #Summarize Gap
  summary$Gap <- percent((((summary$Male) - (summary$Female)) / ((summary$Male) + summary$Female)), .1)
  
  # Set column / dataframe names
  setnames(summary, old=c("Average"), new=c(paste0("Average_", k)))
  setnames(summary, old=c("Male"), new=c(paste0("Male_", k)))
  setnames(summary, old=c("Female"), new=c(paste0("Female_", k)))
  setnames(summary, old=c("Gap"), new=c(paste0("Gap_", k)))
  assign(paste0("summary_", k), data.frame(summary))
  
}

# Setup list of agencies/titles for reference
Citywide_Payroll_list <- 
  distinct(Citywide_Payroll_filtered %>%
             select(9))

# Merge dataframes into one
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2014,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2015,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2016,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2017,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2018,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2019,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2020,by="Title", all=T)

# Write .csv
write.csv(Citywide_Payroll_list, "GenderGap_Title.csv", row.names=FALSE)



#######################
##Aggregation for Chart
# Year list for loop
Years <-
  c(
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020"
  )

for (k in Years) {
  
  #Filter and do some math
  Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == k & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
  citywide_average <- mean(Citywide_Payroll_filtered$Base.Salary)
  citywide_average_m <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Male)
  citywide_average_f <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Female)
  Citywide_Payroll_filtered <- setDT(Citywide_Payroll_filtered)[,if(.N >200) .SD,by=Title]

  
  #Aggregation for .csv
  basic_summ = group_by(Citywide_Payroll_filtered, Title)
  
  #Summarize data
  summary = summarise(basic_summ,
                      Average = 
                        (mean(Base.Salary)),
                      Male = 
                        (weighted.mean(Base.Salary, Male)),
                      Female = 
                        (weighted.mean(Base.Salary, Female)))      
  
  #Run summary for entire City
  summary <- rbind(summary, data.frame(Title="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Male=citywide_average_m, Female=citywide_average_f))
  
  #Adjust for inflation
  # Get CPI from base period (January 2014).
  base_cpi <- as.numeric(subset(df, year== k & periodName=="June", select = "value"))
  # Get the CPI for the new period (February 2015).
  new_cpi <- as.numeric(subset(df, year== 2020 & periodName=="June", select = "value"))
  # Calculate the updated value of our $100 investment.
  summary$Average <- (summary$Average / base_cpi) * new_cpi
  summary$Male <- (summary$Male / base_cpi) * new_cpi
  summary$Female <- (summary$Female / base_cpi) * new_cpi
  # Woops, looks like we lost a penny!
  
  #Summarize Gap
  summary$Gap <- percent((((summary$Male) - (summary$Female)) / ((summary$Male) + summary$Female)), .1)
  
  # Set column / dataframe names
  setnames(summary, old=c("Average"), new=c(paste0("Average_", k)))
  setnames(summary, old=c("Male"), new=c(paste0("Male_", k)))
  setnames(summary, old=c("Female"), new=c(paste0("Female_", k)))
  setnames(summary, old=c("Gap"), new=c(paste0("Gap_", k)))
  assign(paste0("summary_", k), data.frame(summary))
  
}

# Setup list of agencies/titles for reference
Citywide_Payroll_list <- 
  distinct(Citywide_Payroll_filtered %>%
             select(1))

# Merge dataframes into one
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2014,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2015,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2016,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2017,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2018,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2019,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2020,by="Title", all=T)

# Write .csv
write.csv(Citywide_Payroll_list, "GenderGap_Title_chart.csv", row.names=FALSE)



#######################
##Aggregation for Table
#Filter and do some math
Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == 2020 & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
citywide_average <- mean(Citywide_Payroll_filtered$Base.Salary)
citywide_average_m <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Male)
citywide_average_f <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Female)
Citywide_Payroll_filtered <- setDT(Citywide_Payroll_filtered)[,if(.N >200) .SD,by=Title]

summary = summarise(basic_summ,
                    Average = 
                      (mean(Base.Salary)),
                    Male = 
                      (weighted.mean(Base.Salary, Male)),
                    Female = 
                      (weighted.mean(Base.Salary, Female))
)
summary <- rbind(summary, data.frame(Title="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Male=citywide_average_m, Female=citywide_average_f))
summary$Average <- dollar(summary$Average)
summary$Gap <- percent((((summary$Male) - (summary$Female)) / ((summary$Male) + summary$Female)), .1)
summary$Male <- dollar(summary$Male)
summary$Female <- dollar(summary$Female)
write.csv(summary, "GenderGap_Title_2020_table.csv", row.names=FALSE)






#######################
##OVERTIME BY AGENCY

#######################
##Aggregation for .csv
# Year list for loop
Years <-
  c(
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020"
  )

for (k in Years) {
  
  #Filter and do some math
  Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == k & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
  citywide_average <- mean(Citywide_Payroll_filtered$Total.OT.Paid)
  citywide_total <- sum(Citywide_Payroll_filtered$Total.OT.Paid)
  
  #Aggregation for .csv
  basic_summ = group_by(Citywide_Payroll_filtered, Agency)
  
  #Summarize data
  summary = summarise(basic_summ,
                      "Average" = 
                        (mean(Total.OT.Paid)),
                      "Total" = 
                        (sum(Total.OT.Paid)))  
  
  summary <- rbind(summary, data.frame(Agency="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Total=citywide_total))
  
  #Adjust for inflation
  # Get CPI from base period (January 2014).
  base_cpi <- as.numeric(subset(df, year== k & periodName=="June", select = "value"))
  # Get the CPI for the new period (February 2015).
  new_cpi <- as.numeric(subset(df, year== 2020 & periodName=="June", select = "value"))
  # Calculate the updated value of our $100 investment.
  summary$Average <- (summary$Average / base_cpi) * new_cpi
  summary$Total <- (summary$Total / base_cpi) * new_cpi
  
  summary$"Average" <- dollar(summary$"Average")
  summary$"Total" <- dollar(summary$"Total")
  
  # Set column / dataframe names
  setnames(summary, old=c("Average"), new=c(paste0("Average_", k)))
  setnames(summary, old=c("Total"), new=c(paste0("Total_", k)))
  assign(paste0("summary_", k), data.frame(summary))
  
}

# Setup list of agencies/titles for reference
Citywide_Payroll_list <- 
  distinct(Citywide_Payroll_filtered %>%
             select(3))

# Merge dataframes into one
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2014,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2015,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2016,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2017,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2018,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2019,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2020,by="Agency", all=T)

# Write .csv
write.csv(Citywide_Payroll_list, "Overtime_Agency.csv", row.names=FALSE)



#######################
##Aggregation for Chart
# Year list for loop
Years <-
  c(
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020"
  )

for (k in Years) {
  
  #Filter and do some math
  Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == k & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
  citywide_average <- mean(Citywide_Payroll_filtered$Total.OT.Paid)
  citywide_total <- sum(Citywide_Payroll_filtered$Total.OT.Paid)
  Citywide_Payroll_filtered <- setDT(Citywide_Payroll_filtered)[,if(.N >200) .SD,by=Agency]
  
  #Aggregation for .csv
  basic_summ = group_by(Citywide_Payroll_filtered, Agency)
  
  #Summarize data
  summary = summarise(basic_summ,
                      "Average" = 
                        (mean(Total.OT.Paid)),
                      "Total" = 
                        (sum(Total.OT.Paid)))  
  
  summary <- rbind(summary, data.frame(Agency="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Total=citywide_total))
  
  #Adjust for inflation
  # Get CPI from base period (January 2014).
  base_cpi <- as.numeric(subset(df, year== k & periodName=="June", select = "value"))
  # Get the CPI for the new period (February 2015).
  new_cpi <- as.numeric(subset(df, year== 2020 & periodName=="June", select = "value"))
  # Calculate the updated value of our $100 investment.
  summary$Average <- (summary$Average / base_cpi) * new_cpi
  summary$Total <- (summary$Total / base_cpi) * new_cpi
  
  summary$"Average" <- dollar(summary$"Average")
  summary$"Total" <- dollar(summary$"Total")
  
  # Set column / dataframe names
  setnames(summary, old=c("Average"), new=c(paste0("Average_", k)))
  setnames(summary, old=c("Total"), new=c(paste0("Total_", k)))
  assign(paste0("summary_", k), data.frame(summary))
  
}

# Setup list of agencies/titles for reference
Citywide_Payroll_list <- 
  distinct(Citywide_Payroll_filtered %>%
             select(1))

# Merge dataframes into one
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2014,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2015,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2016,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2017,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2018,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2019,by="Agency", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2020,by="Agency", all=T)

# Write .csv
write.csv(Citywide_Payroll_list, "Overtime_Agency_chart.csv", row.names=FALSE)



#######################
##Aggregation for Table
#Filter and do some math
Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == 2020 & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
citywide_average <- mean(Citywide_Payroll_filtered$Total.OT.Paid)
citywide_total <- sum(Citywide_Payroll_filtered$Total.OT.Paid)
Citywide_Payroll_filtered <- setDT(Citywide_Payroll_filtered)[,if(.N >200) .SD,by=Agency]

#Aggregation for .csv
basic_summ = group_by(Citywide_Payroll_filtered, Agency)

#Summarize data
summary = summarise(basic_summ,
                    "Average" = 
                      (mean(Total.OT.Paid)),
                    "Total" = 
                      (sum(Total.OT.Paid)))  

summary <- rbind(summary, data.frame(Agency="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Total=citywide_total))
summary$Average <- dollar(summary$Average)
summary$Total <- dollar(summary$Total)
write.csv(summary, "Overtime_Agency_2020_chart.csv", row.names=FALSE)






#######################
##OVERTIME BY TITLE

#######################
##Aggregation for .csv
# Year list for loop
Years <-
  c(
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020"
  )

for (k in Years) {
  
  #Filter and do some math
  Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == k & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
  citywide_average <- mean(Citywide_Payroll_filtered$Total.OT.Paid)
  citywide_total <- sum(Citywide_Payroll_filtered$Total.OT.Paid)
  
  #Aggregation for .csv
  basic_summ = group_by(Citywide_Payroll_filtered, Title)
  
  #Summarize data
  summary = summarise(basic_summ,
                      "Average" = 
                        (mean(Total.OT.Paid)),
                      "Total" = 
                        (sum(Total.OT.Paid)))  
  
  summary <- rbind(summary, data.frame(Title="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Total=citywide_total))
  
  #Adjust for inflation
  # Get CPI from base period (January 2014).
  base_cpi <- as.numeric(subset(df, year== k & periodName=="June", select = "value"))
  # Get the CPI for the new period (February 2015).
  new_cpi <- as.numeric(subset(df, year== 2020 & periodName=="June", select = "value"))
  # Calculate the updated value of our $100 investment.
  summary$Average <- (summary$Average / base_cpi) * new_cpi
  summary$Total <- (summary$Total / base_cpi) * new_cpi
  
  summary$"Average" <- dollar(summary$"Average")
  summary$"Total" <- dollar(summary$"Total")
  
  # Set column / dataframe names
  setnames(summary, old=c("Average"), new=c(paste0("Average_", k)))
  setnames(summary, old=c("Total"), new=c(paste0("Total_", k)))
  assign(paste0("summary_", k), data.frame(summary))
  
}

# Setup list of agencies/titles for reference
Citywide_Payroll_list <- 
  distinct(Citywide_Payroll_filtered %>%
             select(9))

# Merge dataframes into one
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2014,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2015,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2016,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2017,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2018,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2019,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2020,by="Title", all=T)

# Write .csv
write.csv(Citywide_Payroll_list, "Overtime_Title.csv", row.names=FALSE)



#######################
##Aggregation for Chart
# Year list for loop
Years <-
  c(
    "2014",
    "2015",
    "2016",
    "2017",
    "2018",
    "2019",
    "2020"
  )

for (k in Years) {
  
  #Filter and do some math
  Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == k & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
  citywide_average <- mean(Citywide_Payroll_filtered$Total.OT.Paid)
  citywide_total <- sum(Citywide_Payroll_filtered$Total.OT.Paid)
  Citywide_Payroll_filtered <- setDT(Citywide_Payroll_filtered)[,if(.N >200) .SD,by=Title]


  #Aggregation for .csv
  basic_summ = group_by(Citywide_Payroll_filtered, Title)
  
  #Summarize data
  summary = summarise(basic_summ,
                      "Average" = 
                        (mean(Total.OT.Paid)),
                      "Total" = 
                        (sum(Total.OT.Paid)))  
  
  summary <- rbind(summary, data.frame(Title="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Total=citywide_total))
  
  #Adjust for inflation
  # Get CPI from base period (January 2014).
  base_cpi <- as.numeric(subset(df, year== k & periodName=="June", select = "value"))
  # Get the CPI for the new period (February 2015).
  new_cpi <- as.numeric(subset(df, year== 2020 & periodName=="June", select = "value"))
  # Calculate the updated value of our $100 investment.
  summary$Average <- (summary$Average / base_cpi) * new_cpi
  summary$Total <- (summary$Total / base_cpi) * new_cpi
  
  summary$"Average" <- dollar(summary$"Average")
  summary$"Total" <- dollar(summary$"Total")
  
  # Set column / dataframe names
  setnames(summary, old=c("Average"), new=c(paste0("Average_", k)))
  setnames(summary, old=c("Total"), new=c(paste0("Total_", k)))
  assign(paste0("summary_", k), data.frame(summary))
  
}

# Setup list of agencies/titles for reference
Citywide_Payroll_list <- 
  distinct(Citywide_Payroll_filtered %>%
             select(1))

# Merge dataframes into one
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2014,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2015,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2016,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2017,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2018,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2019,by="Title", all=T)
Citywide_Payroll_list <- merge(Citywide_Payroll_list,summary_2020,by="Title", all=T)

# Write .csv
write.csv(Citywide_Payroll_list, "Overtime_Title_chart.csv", row.names=FALSE)



#######################
##Aggregation for Table
#Filter and do some math
Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == 2020 & Pay.Basis == 'per Annum' & Leave.Status.as.of.June.30 == 'ACTIVE',	)
citywide_average <- mean(Citywide_Payroll_filtered$Total.OT.Paid)
citywide_total <- sum(Citywide_Payroll_filtered$Total.OT.Paid)
Citywide_Payroll_filtered <- setDT(Citywide_Payroll_filtered)[,if(.N >200) .SD,by=Title]

#Aggregation for .csv
basic_summ = group_by(Citywide_Payroll_filtered, Title)

#Summarize data
summary = summarise(basic_summ,
                    "Average" = 
                      (mean(Total.OT.Paid)),
                    "Total" = 
                      (sum(Total.OT.Paid)))  

summary <- rbind(summary, data.frame(Title="CITY OF NEW YORK (ALL AGENCIES / TITLES)", Average=citywide_average, Total=citywide_total))
summary$Average <- dollar(summary$Average)
summary$Total <- dollar(summary$Total)
write.csv(summary, "Overtime_Title_2020_chart.csv", row.names=FALSE)