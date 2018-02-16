#EES Comment Maker

#Import packages
library(readxl)
library(broom)

#set directory to GitHub repo
setwd("C:/Users/bjr21/Documents/GitHub/DunnEES")

#Import file
df <- read_excel("2017EESStatsClean.xlsx", sheet=3)

#Make Agency a factor variable
df$Agency.? <- factor(df$Agency)
is.factor(df$Agency.f)

names <- c("Abraham Lincoln Presidential Library and Museum","Aging, Department on","Agriculture, Department of","Arts Council","Capital Development Board",
           "Central Management Services, Department o?","Children and Family Services, Department of","Civil Service Commission, Illinois",
           "Commerce and Economic Opportunity, Department of","Commerce Commission, Illinois","Community College Board","Corrections, Department of",
           "Criminal?Justice Information Authority","Deaf and Hard of Hearing Commission, Illinois","Developmental Disabilities, Illinois Council on",
           "Education, Illinois State Board of","Educational Labor Relations Board","Emergency Management Agency","Employment ?ecurity, Department of",
           "Environmental Protection Agency","Financial and Professional Regulation, Department of","Gaming Board, Illinois","Guardianship and Advocacy Commission",
           "Healthcare and Family Services - Formerly known as Pub?ic Aid, Dept. of","Higher Education, Board of","Human Rights Commission","Human Rights, Department of",
           "Human Services, Department of","Innovation and Technology, Department of","Insurance Department of","Juvenile Justice, Department of","Labor?Relations Board",
           "Labor, Department of","Law Enforcement Training and Standards Board, Illinois","Liquor Control Commission","Lottery, Illinois",
           "Management and Budget, Office of","Medical District Commission","Natural Resources, De?artment of","Other (please specify)",
           "Pollution Control Board","Power Agency, Illinois","Prisoner Review Board, Illinois","Property Tax Appeal Board","Public Health, Department of",
           "Racing Board, Illinois","Revenue, Department of","?tate Fire Marshal","State Police, Illinois","State Retirement Systems",
           "Student Assistance Commission, Illinois","Toll Highway Authority, Illinois State","Transportation, Department of","Veterans Affairs, Department of",
           "Volunteeris? & Community Service, Governor\'s Commission on","Workers' Compensation Commission, Illinois")
abbrevs <- c("ALPLM","AGE","AG","AC","CDB","CMS","DCFS","CSC","DCEO","ICC","ICCB","DOC","ICJIA","DHHC","CDD","ISBE","ELRB","IEMA","IDES","IEPA","IDFPR","GAM","GA?",
             "HFS","IBHE","HRC","DHS","DHR","DoIT","DOI","DJJ","ILRB","DOL","LETSB","LCC","LOT","GOMB","MDC","IDNR","Other","PCB","IPA","PRB","PTAB","DPH","IRB","REV","OSFM","ISP","SRS",
             "ISAC","TOL","DOT","VA","CVCS","WCC")
Xagency <- data?frame(names, abbrevs)

