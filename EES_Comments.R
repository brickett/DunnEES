#EES Comment Maker

#Import packages
library(readxl)
library(broom)
library(tibble)
library(xlsx)

#set directory to GitHub repo
setwd("C:/Users/bjr21/Documents/GitHub/DunnEES")

#Import file
df <- read_excel("2017EESStatsClean.xlsx", sheet=3)

#Make Agency a factor variable
df$Agency.f <- factor(df$Agency)
is.factor(df$Agency.f)

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

questions <- tibble("Is there a feasible, low-cost change your agency could implement to improve employee retention among your co-workers?","Is there a feasible, low-cost change your agency could implement to improve job satisfaction among your co-workers?","What training or development opportunities would most improve your co-worker's performance?","Is there a feasible, low-cost change your agency could implement to improve your work environment?","What could be done to make the evaluation process more fair for your co-workers?", "If you could provide one piece of advice to your agency's director, what would it be?","The state's fiscal situation has limited its ability to issue raises for many employees. What non-monetary rewards could state agencies employ to improve employee compensation?")

#Initialize
j <- 1 #counter for stepping through the list of agencies 
last <- 1 #bookmark for the beginning of comments
fileend = "EES2017"
filext = ".xlsx" #end of file name
sheet = colnames(df)[3:9] #set of the column names in the dataset, to be used as excel sheet names

#file creation script
for (i in 1:nrow(df)){ #step through each survey response
  while (df[i,1]!=Xagency[j,1]) { #creates the file when the agency name changes
    comms = df[last:i-1,3:9]
    filename=paste(Xagency[j,2],fileend,sep="_") #uses the short abbrev for the file name
    for (k in 1:length(sheet)){
      if (sum(is.na(comms[,k]))==nrow(comms[,k])){
        sheetvals <- data.frame("No Comments Submitted")
      } else {
        sheetvals <- na.omit(comms[,k])
      }
      colnames(sheetvals) <- questions[k]
      write.xlsx(sheetvals, file=paste(filename,filext, sep = "" ),
                 sheetName=sheet[k], append=TRUE, col.names = TRUE)
    }
    last=i+1 #sets a new bookmark for the beginning of a new agency's comments
    j <- j+1 #steps to the next agency
  }
}

comms = df[last:i,3:9]
filename=paste(Xagency[j,2],fileend,sep="_") #uses the short abbrev for the file name
for (k in 1:length(sheet)){
  if (sum(is.na(comms[,k]))==nrow(comms[,k])){
    sheetvals <- data.frame("No Comments Submitted")
  } else {
    sheetvals <- na.omit(comms[,k])
  }
  colnames(sheetvals) <- questions[k]
  write.xlsx(sheetvals, file=paste(filename,filext, sep = "" ),
             sheetName=sheet[k], append=TRUE, col.names = FALSE)
}