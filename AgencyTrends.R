#EES Agency Trends

#Import packages
library(readxl)
library(broom)
library(tibble)
library(ggplot2)
library(reshape2)

#set directory to GitHub repo
setwd("C:/Users/bjr21/Documents/GitHub/DunnEES")

#set number of years
years <- c("2015","2016","2017")

#Import file
df_2017 <- read_excel("2017EESStatsData.xlsx", sheet=1)
df_2016 <- read_excel("2016EESStats.xlsx", sheet=4)
df_2015 <- read_excel("2015EESStats.xlsx", sheet=2)

#Clean & combine
keeps_2017 <- c("SurveyYear", "Sex", "Race", "Tenure", "Union", "Agency", "RetentionComp", "TalentComp", "EnviroComp", "EvalComp", "CustomerComp", "UnitComp", "SuperComp", "LeaderComp", "StateComp")
comps_2017 <- df_2017[keeps_2017]
comps_2017 <- comps_2017[rowSums(is.na(comps_2017))!=ncol(comps_2017), ]

names(df_2016)[2] <- "Sex"
names(df_2016)[3] <- "Race"
names(df_2016)[4] <- "OtherRace"
names(df_2016)[5] <- "Tenure"
names(df_2016)[6] <- "Union"
names(df_2016)[7] <- "Agency"
names(df_2016)[8] <- "OtherAgency"
names(df_2016)[130] <- "RetentionComp"
names(df_2016)[135] <- "TalentComp"
names(df_2016)[144] <- "EnviroComp"
names(df_2016)[153] <- "EvalComp"
names(df_2016)[159] <- "CustomerComp"
names(df_2016)[166] <- "UnitComp"
names(df_2016)[173] <- "SuperComp"
names(df_2016)[181] <- "LeaderComp"
names(df_2016)[182] <- "StateComp"
keeps_2016 <- c("SurveyYear", "Sex", "Race", "Tenure", "Union", "Agency", "RetentionComp", "TalentComp", "EnviroComp", "EvalComp", "CustomerComp", "UnitComp", "SuperComp", "LeaderComp", "StateComp")
comps_2016 <- df_2016[keeps_2016]
comps_2016 <- comps_2016[rowSums(is.na(comps_2016))!=ncol(comps_2016), ]

names(df_2015)[2] <- "Sex"
names(df_2015)[3] <- "Race"
names(df_2015)[4] <- "Tenure"
names(df_2015)[6] <- "Union"
names(df_2015)[7] <- "Agency"
names(df_2015)[126] <- "RetentionComp"
names(df_2015)[131] <- "TalentComp"
names(df_2015)[140] <- "EnviroComp"
names(df_2015)[149] <- "EvalComp"
names(df_2015)[155] <- "CustomerComp"
names(df_2015)[162] <- "UnitComp"
names(df_2015)[169] <- "SuperComp"
names(df_2015)[177] <- "LeaderComp"
names(df_2015)[179] <- "StateComp"
keeps_2015 <- c("SurveyYear", "Sex", "Race", "Tenure", "Union", "Agency", "RetentionComp", "TalentComp", "EnviroComp", "EvalComp", "CustomerComp", "UnitComp", "SuperComp", "LeaderComp", "StateComp")
comps_2015 <- df_2015[keeps_2015]
comps_2015 <- comps_2015[rowSums(is.na(comps_2015))!=ncol(comps_2015), ]

fullset <- rbind(comps_2015, comps_2016, comps_2017)
fullset$Sex <- factor(fullset$Sex)
fullset$Race <- factor(fullset$Race)
fullset$Tenure <- factor(fullset$Tenure)
fullset$Union <- factor(fullset$Union)
fullset$Agency <- factor(fullset$Agency)
fullset$SurveyYear.f <- factor(fullset$SurveyYear)

#Calculate aggregate statistics
fullsetm <- melt(data=fullset, id.vars = c("SurveyYear", "SurveyYear.f", "Sex", "Race", "Tenure", "Union", "Agency"))

fullmean <- dcast(fullsetm, SurveyYear.f + variable ~ ., mean, na.rm=TRUE)
fullsd <- dcast(fullsetm, SurveyYear.f + variable ~ ., sd, na.rm=TRUE)

names(fullmean)[3] <- "Mean"
names(fullsd)[3] <- "SD"

SOI_results<- cbind(fullmean,fullsd$SD)
names(SOI_results)[4] <- "SD"
SOI_results <- na.omit(SOI_results)
SOI_subset_only <-subset(SOI_results, variable!="StateComp")

# Plot results - basic plot
p_SOI_subset_only<- ggplot(SOI_subset_only, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                position=position_dodge(.9))

# Cleaned up bar plot
p_SOI_subset_only+labs(title="Survey Section Composite Scores, Mean & 95% Confidence Interval", x="Survey Focus", y = "Average Composite Score")+theme_minimal()+scale_fill_discrete(name = "Survey Year") + scale_x_discrete(labels=c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership"))  + 
  theme(axis.text.x=element_text(angle=30, hjust=1))

print(p_SOI_subset_only)

thing

# Perform t-test
#use Welch's two-sided test to control for unequal sample sizes
delfull = subset(fullset,SurveyYear.f==2015 | SurveyYear.f==2017) #delta between start of survey and most recent year
delrecent = subset(fullset,SurveyYear.f==2016 | SurveyYear.f==2017) #delta between last two runs of the survey

full_ttest <- lapply(delfull[,7:15], function(i) t.test(i ~ delfull$SurveyYear.f))
recent_ttest <- lapply(delrecent[,7:15], function(i) t.test(i ~ delrecent$SurveyYear.f))

#NEXT: calculate % change and display using https://stackoverflow.com/questions/18700938/ggplot2-positive-and-negative-values-different-color-gradient 


#List of agency names and abbreviations, to be used in file creation.
names <- c("Abraham Lincoln Presidential Library and Museum","Aging, Department on","Agriculture, Department of","Arts Council","Capital Development Board",
           "Central Management Services, Department of","Children and Family Services, Department of","Civil Service Commission, Illinois",
           "Commerce and Economic Opportunity, Department of","Commerce Commission, Illinois","Community College Board","Corrections, Department of",
           "Criminal Justice Information Authority","Deaf and Hard of Hearing Commission, Illinois","Developmental Disabilities, Illinois Council on",
           "Education, Illinois State Board of","Educational Labor Relations Board","Emergency Management Agency","Employment Security, Department of",
           "Environmental Protection Agency","Financial and Professional Regulation, Department of","Gaming Board, Illinois","Guardianship and Advocacy Commission",
           "Healthcare and Family Services - Formerly known as Public Aid, Dept. of","Higher Education, Board of","Human Rights Commission","Human Rights, Department of",
           "Human Services, Department of","Innovation and Technology, Department of","Insurance Department of","Juvenile Justice, Department of","Labor Relations Board",
           "Labor, Department of","Law Enforcement Training and Standards Board, Illinois","Liquor Control Commission","Lottery, Illinois",
           "Management and Budget, Office of","Medical District Commission","Natural Resources, Department of","Other (please specify)",
           "Pollution Control Board","Power Agency, Illinois","Prisoner Review Board, Illinois","Property Tax Appeal Board","Public Health, Department of",
           "Racing Board, Illinois","Revenue, Department of","State Fire Marshal","State Police, Illinois","State Retirement Systems",
           "Student Assistance Commission, Illinois","Toll Highway Authority, Illinois State","Transportation, Department of","Veterans Affairs, Department of",
           "Volunteerism Community Service, Governor's Commission on","Workers' Compensation Commission, Illinois")
abbrevs <- c("ALPLM","AGE","AG","AC","CDB","CMS","DCFS","CSC","DCEO","ICC","ICCB","DOC","ICJIA","DHHC","CDD","ISBE","ELRB","IEMA","IDES","IEPA","IDFPR","GAM","GAC",
             "HFS","IBHE","HRC","DHR","DHS","DoIT","DOI","DJJ","ILRB","DOL","LETSB","LCC","LOT","GOMB","MDC","IDNR","Other","PCB","IPA","PRB","PTAB","DPH","IRB","REV","OSFM","ISP","SRS",
             "ISAC","TOL","DOT","VA","CVCS","WCC")
Xagency <- tibble(names, abbrevs)
