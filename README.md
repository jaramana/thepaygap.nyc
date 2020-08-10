# thepaygap.nyc

This project seeks to identify the gender pay gap and other possible disparities in the City of New York payroll. It uses publicly available data provided by the New York City **Office of Payroll Administration** and the **Social Security Administration**.

## Technologies 
### Web development
-   JavaScript
-   DataTables
-   HTML5
-   CSS3

### Data Analysis
-   R 3.6.1

### R Libraries
- reshape2
- dplyr
- kableExtra
- tidyverse
- data.table
- DT
- scales
- jsonlite

## Data Requirements

NYC Office of Payroll Administration, Citywide Payroll Data
https://data.cityofnewyork.us/City-Government/Citywide-Payroll-Data-Fiscal-Year-/k397-673e/data

Social Security Administration, Popular Baby Names
https://www.ssa.gov/oact/babynames/limits.html

## Methodology

Load library packages

    library(reshape2)
    library(dplyr)
    library(kableExtra)
    library(tidyverse)
    library(data.table)
    library(DT)
    library(scales)
    library(jsonlite)

Set working directory

    setwd("C:/PATH/TO/DIRECTORY")


Load, clean and aggregate Popular Baby Names data

    Baby_Names = read.csv("NY.txt", header = FALSE)
    setnames(Baby_Names, old=c("V1","V2","V3","V4","V5"), new=c("State","Sex","Year","Name","Count"))
    Baby_Names[,"Name"] = toupper(Baby_Names[,"Name"])
    
    #Set up data frame for by-group processing.  
    Baby_Names_pivot = group_by(Baby_Names, Name, Sex)
    
    #Calculate the summary metrics
    Baby_Names_pivot = summarise(Baby_Names_pivot,
                                      Count = sum(Count))
    
    Baby_Names_Spread = spread(Baby_Names_pivot, "Sex", "Count")
    Baby_Names_Spread[is.na(Baby_Names_Spread)] <- 0
    Baby_Names_Spread$M_weight <- Baby_Names_Spread$M/(Baby_Names_Spread$M + Baby_Names_Spread$F) 
    Baby_Names_Spread$F_weight <- Baby_Names_Spread$F/(Baby_Names_Spread$M + Baby_Names_Spread$F) 
    
    write.csv(Baby_Names_Spread, "baby_names_NY.csv", row.names=FALSE)

Load, clean and aggregate Citywide Payroll Data

    Citywide_Payroll = read.csv("Citywide_Payroll_Data.csv", header = TRUE)
    Citywide_Payroll_filtered <- filter(Citywide_Payroll, Fiscal.Year == 2019 & Pay.Basis == 'per Annum',	)
    setnames(Citywide_Payroll_filtered, old=c("Agency.Name"), new=c("Agency"))
    
    
    Citywide_Payroll_filtered$Male <- Baby_Names_Spread$M_weight[match(Citywide_Payroll_filtered$First.Name, Baby_Names_Spread$Name, nomatch = NA_integer_, incomparables = NULL)]
    Citywide_Payroll_filtered$Female <- Baby_Names_Spread$F_weight[match(Citywide_Payroll_filtered$First.Name, Baby_Names_Spread$Name)]
    
    Citywide_Payroll_filtered <- filter(Citywide_Payroll_filtered, !is.na(Male))
    Citywide_Payroll_filtered <- filter(Citywide_Payroll_filtered, !is.na(Female))
    
 Further aggregate Citywide Payroll Data by Agency
    
    citywide_average <- mean(Citywide_Payroll_filtered$Base.Salary)
    citywide_average_m <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Male)
    citywide_average_f <- weighted.mean(Citywide_Payroll_filtered$Base.Salary, Citywide_Payroll_filtered$Female)
    

    basic_summ = group_by(Citywide_Payroll_filtered, Agency)
    
    agency_summary = summarise(basic_summ,
                               Average = 
                                 (mean(Base.Salary)),
                               Male = 
                                 (weighted.mean(Base.Salary, Male)),
                               Female = 
                                 (weighted.mean(Base.Salary, Female))
    )
    
    agency_summary$Gap <- (((agency_summary$Male) - (agency_summary$Female)) / ((agency_summary$Male) + agency_summary$Female))
    
    write.csv(agency_summary, "agency_summary.csv", row.names=FALSE)
    
    Citywide_Payroll_filtered_agency <- setDT(Citywide_Payroll_filtered)[,if(.N >100) .SD,by=Agency]
    basic_summ = group_by(Citywide_Payroll_filtered_agency, Agency)
    
    agency_summary = summarise(basic_summ,
                           Average = 
                             (mean(Base.Salary)),
                           Male = 
                             (weighted.mean(Base.Salary, Male)),
                           Female = 
                             (weighted.mean(Base.Salary, Female))
                           )
    
    agency_summary$Gap <- percent((((agency_summary$Male) - (agency_summary$Female)) / ((agency_summary$Male) + agency_summary$Female)), .1)
    
    agency_summary$Average <- dollar(agency_summary$Average)
    agency_summary$Male <- dollar(agency_summary$Male)
    agency_summary$Female <- dollar(agency_summary$Female)
    
    write_json(agency_summary, "agency_summary.json", pretty = TRUE)

 Aggregate Citywide Payroll Data by Civil Service Title
 
    names(Citywide_Payroll_filtered)[9]<- 'Title'
    basic_summ = group_by(Citywide_Payroll_filtered, Title)
    
    title_summary = summarise(basic_summ,
                               Average = 
                                 (mean(Base.Salary)),
                               Male = 
                                 (weighted.mean(Base.Salary, Male)),
                               Female = 
                                 (weighted.mean(Base.Salary, Female))
    )
    
    title_summary$Gap <- (((title_summary$Male) - (title_summary$Female)) / ((title_summary$Male) + title_summary$Female))
    
    write.csv(title_summary, "title_summary.csv", row.names=FALSE)
    
    Citywide_Payroll_filtered_title <- setDT(Citywide_Payroll_filtered)[,if(.N >100) .SD,by=Title]
    basic_summ = group_by(Citywide_Payroll_filtered_title, Title)
    
    title_summary = summarise(basic_summ,
                               Average = 
                                 (mean(Base.Salary)),
                               Male = 
                                 (weighted.mean(Base.Salary, Male)),
                               Female = 
                                 (weighted.mean(Base.Salary, Female))
    )
    
    title_summary$Gap <- percent((((title_summary$Male) - (title_summary$Female)) / ((title_summary$Male) + title_summary$Female)), .1)
    
    title_summary$Average <- dollar(title_summary$Average)
    title_summary$Male <- dollar(title_summary$Male)
    title_summary$Female <- dollar(title_summary$Female)
    
    write_json(title_summary, "title_summary.json", pretty = TRUE)

## License
thepaygap.nyc is released under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).
