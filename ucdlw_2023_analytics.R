##########################################################
### UCDLW 2023 - Workshop Registration Analytics ###
##########################################################

# META

# Data source: UCDLW_2023_Registrations.csv
# Dates for analysis: February 2023
# Contributors: UCDLW Committee
# Questions? Script prepared by: Pamela Reynolds (plreynolds@ucdavis.edu)
##########################################################

# MANUAL PREPARATION

# Added column "Host", with levels being the UC code
# Moved UCSB entries for Data-DeID and Data Visualization workshops
# under Pronouns column to new column "Requests", as they were in the incorrect column
# Added NA for any row missing Email Address
# Completed Current Institution based on email address suffix for Reproducibility for Everyone workshop; 
# used NA for email addresses where suffix was not indicative 
# Host for Reproducibility for Everyone listed as Multiple (UCB + UCD graduate student led)
##########################################################

# CAVEATS

# Not all information captured across all workshops
# No registration data from 4 "informal" activities hosted by UCD (R, Python and Julia User Group Meetup, WiDS Datathon)
# Domain: some forms restricted to one entry, whereas others allowed multiple to be selected
# Registration counts can often be assumed to be greater than actual attendance data
##########################################################

# LIBRARIES

library(readxl) # if reading in from xls
library(dplyr)
library(stringr)
library(data.table)
library(psych)
library(tidyverse)
##########################################################

# DIRECTORY

# Set working directory and where you want output to be saved locally
setwd("~/Documents/Teaching/UCDLW/UCLDW2023")
path_out = "~/Documents/Teaching/UCDLW/UCLDW2023/output"
##########################################################

# READ IN DATA

my.data<-read.csv("UCLDW_2023_Registrations.csv")
df<-my.data
##########################################################

# CLEANING

# remove punctuation in column names
colnames(df)<-str_trim(colnames(df))
colnames(df)<-str_replace_all(colnames(df), "[:punct:]", "") # removes the ?'s
colnames(df)<-str_replace_all(colnames(df), "\\s+", ".") # the + tackles if there are one or more spaces to trim
colnames(df)<- str_to_lower(colnames(df))
colnames(df)

# changing column names to be more sensible; need to specify that rename is for dplyr if both dplyr and plyr are being used
df<-dplyr::rename(df, workshop=nameofworkshop, email=emailaddress, role = areyoua, location = currentinstitution, department = whatisyourhomedepartmentororganization, domain = withwhatdomaindoyoumostidentify, reason=whydoyouwanttotakethisworkshopselectallthatapply, pronouns = whatpronounswouldyoulikeustouseforyou, past.attendee=didyouattenduclovedataweek2022or2021, distribution=howdidyouhearaboutthisworkshop)

# create column for email domain (splitting at the @)
df$email<-str_to_lower(df$email)          # removes capitalization in some email addresses
df$email.entity<-gsub(".*@","",df$email)  # returns only after the @, proxy for entity (ie ucdavis, ucla)
df$email.org<-gsub(".*\\.","",df$email.entity)  # returns only after the . (need double escape, \\, because . is special)
# email.entity is @* and email.org is .*
df$email.org<-as.factor(df$email.org)  # returns edu, com, gov, etc.
df$email.entity<-as.factor(df$email.entity) # returns ucdavis.edu, etc.

##########################################################

# BASIC METRICS

df.table<-as.data.frame(df)

# Total Number Workshops
total.workshops<-length(unique(df$workshop))
total.events<-total.workshops+4
total.workshops # 21
total.events   # 25

# Total Registrations
total.registrants<-length(df$email)
total.registrants  # 2098

# Unique Registrants
length(unique(df$email))   # 1110

# Total Attended
#total.attended<-sum(df$attended == "yes", df$attended == "Yes", df$attended == "YES", df$attended == "attended", df$attended == "Attended", df$attended == "x", df$attended =="X", df$attended == "walk-on",df$attended == "walk-in", na.rm=TRUE)

# Unique "Departments"
  df$department<- as.factor(df$department)
  uniq.departments<-length(unique(df$department))

# Unique Current Institutions
  uniq.location=length(unique(df$location))

# Registrants per workshop by host
  reg.by.workshop<-df %>% count(host, workshop)

# Registrants per workshop by current institution
  reg.by.workshop.current<-df %>% count(workshop, location)

##########################################################
# CREATE SUMMARY VARIABLES
  
# Domain
  df$domain<- as.factor(df$domain)
  domain.math.cs<-sum(df$domain == "mathematical and computational sciences", na.rm=TRUE)
  domain.physical.sci<-sum(df$domain == "physical sciences", na.rm=TRUE)
  domain.life.sci<-sum(df$domain == "life sciences", na.rm=TRUE)
  domain.health.sci<-sum(df$domain == "health sciences", na.rm=TRUE)
  domain.soc.sci<-sum(df$domain == "social sciences", na.rm=TRUE)
  domain.humanities<-sum(df$domain == "humanities", na.rm=TRUE)

# Registrant Roles
  df$role<- as.factor(df$role)
  #levels:   
  role.undergrad<-sum(df$role== "Student, undergraduate", na.rm=TRUE)
  role.grad.postdoc<-sum(df$role== "Student, graduate" | df$role== "Postdoc" , na.rm=TRUE)
  role.faculty<-sum(df$role== "Faculty", na.rm=TRUE)
  role.staff<-sum(df$role== "Staff" | df$role=="Professional Researcher", na.rm=TRUE)
  role.other<-sum(df$role=="Alumni" | df$role=="Other" | df$role=="Medical Resident", na.rm=TRUE)

# Pronouns - NEEDS IMPROVEMENT
df$pronouns<- as.factor(df$pronouns)
pronoun.he<-sum(df$pronouns == "he/him/his", na.rm=TRUE)
pronoun.she<-sum(df$pronouns == "she/her/hers", na.rm=TRUE)
pronoun.they<-sum(df$pronouns == "they/them/theirs", na.rm=TRUE)

# Overall Summary
results<-data.frame(total.registrants, uniq.departments, email.academia, email.govt, email.other, domain.math.cs, domain.physical.sci, domain.life.sci, domain.health.sci, domain.soc.sci, domain.humanities, loc.davis, loc.sacramento, loc.other, role.undergrad, role.grad.postdoc, role.faculty, role.staff, role.other, pronoun.he, pronoun.she, pronoun.they, exp.none, exp.some, exp.extensive)
results

###############################################################

# BASIC DATA VISUALIZATION

ggplot(df, aes(x=workshop))+geom_bar()
ggplot(df, aes(x=host))+geom_bar()
ggplot(df, aes(x=location))+geom_bar()
ggplot(df, aes(x=pronouns))+geom_bar()

###############################################################

