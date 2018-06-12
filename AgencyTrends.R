### EES Agency Trends ###

## Setup ##
# Import packages
library(readxl)
library(broom)
library(tibble)
library(ggplot2)
library(reshape2)
library(scales)
library(stringi)
library(magrittr)
library(dplyr)
library(tidyr)

# set directory to GitHub repo
setwd("C:/Users/bjr21/Documents/GitHub/DunnEES")

# set number of years
years <- c("2015","2016","2017")

## Import & clean files ##
df_2017 <- read_excel("2017EESStats.xlsx", sheet=3)
df_2016 <- read_excel("2016EESStats.xlsx", sheet=4)
df_2015 <- read_excel("2015EESStats.xlsx", sheet=2)

# Clean & combine
#This code makes the column names R-friendly for later manipulation. It also selects a subset of the data in 'keeps' to use for analysis
#and converts 'keeps' into 'comps' (short for composites), a dataframe instead of a tibble. 'comps' will be combined into a full set of data later.
names(df_2017)[2] <- "Sex"
names(df_2017)[3] <- "Race"
names(df_2017)[4] <- "OtherRace"
names(df_2017)[5] <- "Tenure"
names(df_2017)[6] <- "Union"
names(df_2017)[7] <- "Agency"
names(df_2017)[8] <- "OtherAgency"
names(df_2017)[136] <- "RetentionComp"
names(df_2017)[141] <- "TalentComp"
names(df_2017)[150] <- "EnviroComp"
names(df_2017)[159] <- "EvalComp"
names(df_2017)[165] <- "CustomerComp"
names(df_2017)[172] <- "UnitComp"
names(df_2017)[179] <- "SuperComp"
names(df_2017)[187] <- "LeaderComp"
names(df_2017)[188] <- "StateComp"
keeps_2017 <- c("SurveyYear", "Sex", "Race", "Tenure", "Union", "Agency", "RetentionComp", "TalentComp", "EnviroComp", "EvalComp", "CustomerComp", "UnitComp", "SuperComp", "LeaderComp", "StateComp")
comps_2017 <- df_2017[keeps_2017]
comps_2017 <- comps_2017[rowSums(is.na(comps_2017))!=ncol(comps_2017), ] #drops the values where all responses are null (e.g. someone only answered the open responses)

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


# make into one large dataset
fullset <- rbind(comps_2015, comps_2016, comps_2017) #combines the subsets of each year into a subset of all years.
fullset$Sex <- factor(fullset$Sex) #makes factor variables for earier manipulation later
fullset$Race <- factor(fullset$Race)
fullset$Tenure <- factor(fullset$Tenure)
fullset$Union <- factor(fullset$Union)
fullset$Agency <- factor(fullset$Agency)
fullset$SurveyYear.f <- factor(fullset$SurveyYear)

# keep only the results that have at least some survey answers
fullset <- drop_na(fullset, "StateComp") #because StateComp is a sum of all the other composites, if it is N/A, they must have submitted no numeric answers

## Survey Results Analysis ##
# Calculate aggregate statistics
#melt the dataset so that each composite is on its own line, separated out by all of the ID variables below
fullsetm <- melt(data=fullset, id.vars = c("SurveyYear", "SurveyYear.f", "Sex", "Race", "Tenure", "Union", "Agency"))

#cast the dataset to get the mean and standard deviation across the entire population
fullmean <- dcast(fullsetm, SurveyYear.f + variable ~ ., mean, na.rm=TRUE)
fullsd <- dcast(fullsetm, SurveyYear.f + variable ~ ., sd, na.rm=TRUE)

names(fullmean)[3] <- "Mean"
names(fullsd)[3] <- "SD"

# Bind the mean and standard deviation into one dataset
SOI_results<- cbind(fullmean,fullsd$SD)
names(SOI_results)[4] <- "SD"
SOI_results <- na.omit(SOI_results) #clear out the glitch of the unknown years that melting introduces
SOI_subset_only <-subset(SOI_results, variable!="StateComp") #all of the composite scores except the statewide composite
SOI_state_only <-subset(SOI_results, variable=="StateComp") #only the statewide composite score

# Plot results - basic plot: composite questions
p_SOI_subset_only<- ggplot(SOI_subset_only, aes(x=variable, y=Mean, fill=SurveyYear.f, label = round(Mean, digits=2))) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                position=position_dodge(width = 0.9))

# Cleaned up bar plot
p_SOI_subset_only <- p_SOI_subset_only+labs(title="Survey Section Composite Scores, Mean & 95% Confidence Interval", x="Survey Focus", y = "Average Composite Score")+
  theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
  scale_x_discrete(labels=c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership"))  + 
  theme(axis.text.x=element_text(angle=30, hjust=1))+
  geom_label(data = SOI_subset_only, aes(x=variable, y=Mean-2, label=round(Mean, digits=2)), position = position_dodge(0.9), label.padding = unit(0.1, "lines"), size = 3)
  
print(p_SOI_subset_only)
picname <- "1State_Composites.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)

# Plot results - basic plot: Statewide Composite
p_SOI_state_only<- ggplot(SOI_state_only, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                position=position_dodge(.9))

# Cleaned up bar plot
p_SOI_state_only <- p_SOI_state_only+labs(title="Statewide Composite Scores, Mean & 95% Confidence Interval", x="Statewide Composite", y = "Average Composite Score")+
  theme_minimal()+scale_fill_discrete(name = "Survey Year")+
  geom_label(data = SOI_state_only, aes(x=variable, y=Mean-10, label=round(Mean, digits=2)), position = position_dodge(0.9), label.padding = unit(0.1, "lines"))

#print(p_SOI_state_only)
picname <- "1Statewide.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)

## Determine % change year to year, and whether it is statistically significant ##
# Perform t-test
# use Welch's two-sided test to control for unequal sample sizes
delfull <- subset(fullset,SurveyYear.f==2015 | SurveyYear.f==2017) #delta between start of survey and most recent year
delrecent <- subset(fullset,SurveyYear.f==2016 | SurveyYear.f==2017) #delta between last two runs of the survey

full_ttest <- lapply(delfull[,7:15], function(i) t.test(i ~ delfull$SurveyYear.f)) #lapply produces a vector that applies the function to each column of the input
recent_ttest <- lapply(delrecent[,7:15], function(i) t.test(i ~ delrecent$SurveyYear.f)) #the ttest function tells it that two populations are indicated by the two years

# set vectors for calculating % change - extracts the results from the vector
full_results_end <- c(full_ttest$RetentionComp$estimate[2], full_ttest$TalentComp$estimate[2], full_ttest$EnviroComp$estimate[2], full_ttest$EvalComp$estimate[2],
                      full_ttest$CustomerComp$estimate[2], full_ttest$UnitComp$estimate[2], full_ttest$SuperComp$estimate[2], full_ttest$LeaderComp$estimate[2],
                      full_ttest$StateComp$estimate[2])
full_results_beg <- c(full_ttest$RetentionComp$estimate[1], full_ttest$TalentComp$estimate[1], full_ttest$EnviroComp$estimate[1], full_ttest$EvalComp$estimate[1],
                      full_ttest$CustomerComp$estimate[1], full_ttest$UnitComp$estimate[1], full_ttest$SuperComp$estimate[1], full_ttest$LeaderComp$estimate[1],
                      full_ttest$StateComp$estimate[1])

rec_results_end <- c(recent_ttest$RetentionComp$estimate[2], recent_ttest$TalentComp$estimate[2], recent_ttest$EnviroComp$estimate[2], recent_ttest$EvalComp$estimate[2],
                     recent_ttest$CustomerComp$estimate[2], recent_ttest$UnitComp$estimate[2], recent_ttest$SuperComp$estimate[2], recent_ttest$LeaderComp$estimate[2],
                     recent_ttest$StateComp$estimate[2])
rec_results_beg <- c(recent_ttest$RetentionComp$estimate[1], recent_ttest$TalentComp$estimate[1], recent_ttest$EnviroComp$estimate[1], recent_ttest$EvalComp$estimate[1],
                     recent_ttest$CustomerComp$estimate[1], recent_ttest$UnitComp$estimate[1], recent_ttest$SuperComp$estimate[1], recent_ttest$LeaderComp$estimate[1],
                     recent_ttest$StateComp$estimate[1])

# calculate % change. full=2015 to most recent year, while recent should be the most recent year and the prior year
PC_full <- (full_results_end - full_results_beg)/full_results_beg
names(PC_full) <- c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership", "State Composite")
PC_rec <- (rec_results_end - rec_results_beg)/rec_results_beg
names(PC_rec) <- c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership", "State Composite")

# Extract statistical significance
full_results_pval <- c(full_ttest$RetentionComp$p.value, full_ttest$TalentComp$p.value, full_ttest$EnviroComp$p.value, full_ttest$EvalComp$p.value,
                       full_ttest$CustomerComp$p.value, full_ttest$UnitComp$p.value, full_ttest$SuperComp$p.value, full_ttest$LeaderComp$p.value,
                       full_ttest$StateComp$p.value)

# Translate statistical significance into un-significant, 95% confidence interval, 99% confidence interval
CInul_full <- full_results_pval>0.05
CI95_full <- full_results_pval<=0.05 & full_results_pval>0.01
CI99_full <- full_results_pval<=0.01

# Use the logical variables above to create graphical representations of significance
full_results_pval <- replace(full_results_pval,CI99_full,"**")
full_results_pval <- replace(full_results_pval,CI95_full,"*")
full_results_pval <- replace(full_results_pval,CInul_full," ")

# Repeat the translation of significance for the change in the 'recent' set
rec_results_pval <- c(recent_ttest$RetentionComp$p.value, recent_ttest$TalentComp$p.value, recent_ttest$EnviroComp$p.value, recent_ttest$EvalComp$p.value,
                       recent_ttest$CustomerComp$p.value, recent_ttest$UnitComp$p.value, recent_ttest$SuperComp$p.value, recent_ttest$LeaderComp$p.value,
                       recent_ttest$StateComp$p.value)

CInul_rec <- rec_results_pval>0.05
CI95_rec <- rec_results_pval<=0.05 & rec_results_pval>0.01
CI99_rec <- rec_results_pval<=0.01

rec_results_pval <- replace(rec_results_pval,CI99_rec,"**")
rec_results_pval <- replace(rec_results_pval,CI95_rec,"*")
rec_results_pval <- replace(rec_results_pval,CInul_rec," ")


## Show Percent Change - Timeframe of full survey ##
# Create a plot
PC_full_2plot <- data.frame(c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership", "State Composite"), unname(PC_full),
                            paste(round(unname(PC_full)*100,digits = 1),full_results_pval, sep = "%\n"))
names(PC_full_2plot) <- c("variable", "Change", "sig")
# set variables in proper order for plotting
PC_full_2plot$variable <- factor(PC_full_2plot$variable, levels = c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership", "State Composite"))

p_PC_full <- ggplot(PC_full_2plot, mapping=aes(x=variable, y=Change, fill=Change)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradient2(low = scales::muted("red"), mid = "white",
                       high = scales::muted("green"), midpoint = 0, space = "Lab",
                       na.value = "grey50", guide = "colourbar", labels = scales::percent) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_label(aes(label=sig), position = position_stack(vjust = 0.5), label.padding = unit(0.1,"lines"), fill = "white")

# Clean up plot
p_PC_full <- p_PC_full+labs(title="% Change, 2015-2017", x="Survey Focus", y = "% Change")+
  theme_minimal()+ 
  theme(axis.text.x=element_text(angle=30, hjust=1))

#print(p_PC_full)
picname <- "1Statewide_PercentChange_Overall.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)


## Show Percent Change - Timeframe of year-to-year change ##
# Create a plot
PC_rec_2plot <- data.frame(c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership", "State Composite"), unname(PC_rec),
                            paste(round(unname(PC_rec)*100,digits = 1),rec_results_pval, sep = "%\n"))
names(PC_rec_2plot) <- c("variable", "Change", "sig")
# set variables in proper order for plotting
PC_rec_2plot$variable <- factor(PC_rec_2plot$variable, levels = c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership", "State Composite"))

p_PC_rec <- ggplot(PC_rec_2plot, mapping=aes(x=variable, y=Change, fill=Change)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_gradient2(low = scales::muted("red"), mid = "white",
                       high = scales::muted("green"), midpoint = 0, space = "Lab",
                       na.value = "grey50", guide = "colourbar", labels = scales::percent) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  geom_label(aes(label=sig), position = position_stack(vjust = 0.5), label.padding = unit(0.1,"lines"), fill = "white")

# Clean up plot
p_PC_rec <- p_PC_rec+labs(title="% Change, 2016-2017", x="Survey Focus", y = "% Change", caption = "Significance: * indicates 95% certainty that the % change is due to actual shifts in responses, while ** indicates 99% certainty.")+
  theme_minimal()+ 
  theme(axis.text.x=element_text(angle=30, hjust=1))

#print(p_PC_rec)
picname <- "1Statewide_PercentChange_Recent.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)


## Plot out question-by-question comparisons ##
# Clean 2015 data
df_2015$`State Average` <- NULL #removes the state average column, aligning the number of question columns across years
names(df_2015) <- replace(names(df_2015),c(120:178),names(df_2017[130:188])) #cleans up different question intro (all the "my coworkers" replaced with "State of Illinois")

allq_fullset <- bind_rows(df_2015[,c(1:4,6:7,120:178)], df_2016[,c(1:3,5:7,124:182)], df_2017[,c(1:3,5:7, 130:188)])

# keep only the results that have at least some survey answers
allq_fullset <- drop_na(allq_fullset, "StateComp")

allq_fullset$`Retention and Satisfaction Average` <- NULL
allq_fullset$`Talent Development Average` <- NULL
allq_fullset$`Work Environment Average` <- NULL
allq_fullset$`Worker Evalutions Average` <- NULL
allq_fullset$`Work Unit Average` <- NULL
allq_fullset$`Customer Interactions Average` <- NULL
allq_fullset$`Supervison Average` <- NULL
allq_fullset$`Leadership Average` <- NULL
allq_fullset$`Worker Evaluations Average` <- NULL
allq_fullset$`Supervision Average` <- NULL

# make data into factors
allq_fullset$Sex <- factor(allq_fullset$Sex)
allq_fullset$Race <- factor(allq_fullset$Race)
allq_fullset$Tenure <- factor(allq_fullset$Tenure)
allq_fullset$Union <- factor(allq_fullset$Union)
allq_fullset$Agency <- factor(allq_fullset$Agency)
allq_fullset$SurveyYear.f <- factor(allq_fullset$SurveyYear)

# melt the dataset for aggregation
allq_fullsetm <- melt(data=allq_fullset, id.vars = c("SurveyYear", "SurveyYear.f", "Sex", "Race", "Tenure", "Union", "Agency"))

# aggregate by year and variable to get mean and standard deviation
allq_fullmean <- dcast(allq_fullsetm, SurveyYear.f + variable ~ ., mean, na.rm=TRUE) #mean
allq_fullsd <- dcast(allq_fullsetm, SurveyYear.f + variable ~ ., sd, na.rm=TRUE) #standard deviation

names(allq_fullmean)[3] <- "Mean"
names(allq_fullsd)[3] <- "SD"

# make into one dataset for plotting
SOI_allq<- cbind(allq_fullmean,allq_fullsd$SD)
names(SOI_allq)[4] <- "SD"
SOI_allq <- na.omit(SOI_allq)
brkpt <-round(stri_length(SOI_allq$variable)/2, digits=0) #add breakpoint for plotting - splits halfway through question
stri_sub(SOI_allq$variable,brkpt+1,brkpt) <- "-\n"

SOI_allq_Ret <- SOI_allq[c(1:5,52:56,103:107),]
SOI_allq_Tal <- SOI_allq[c(7:9,58:60,109:111),]
SOI_allq_Env <- SOI_allq[c(11:17,62:68,113:119),]
SOI_allq_Eval <- SOI_allq[c(19:25,70:76,121:127),]
SOI_allq_Cust <- SOI_allq[c(27:30,78:81,129:132),]
SOI_allq_Unit <- SOI_allq[c(32:36,83:87,134:138),]
SOI_allq_Sup <- SOI_allq[c(38:42,89:93,140:144),]
SOI_allq_Lead <- SOI_allq[c(44:49,95:100,146:151),]

allq_plot_fun <- function(data_in, label_shift, title_in){
  #Results - basic plot
  p_SOI_allq_gen<- ggplot(data_in, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
    geom_bar(stat="identity", color="black", 
             position=position_dodge()) +
    geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                  position=position_dodge(.9))
  
  # Cleaned up bar plot
  p_SOI_allq_gen <- p_SOI_allq_gen+labs(title=title_in, x="Survey Question", y = "Average Score")+
    theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
    theme(axis.text.x=element_text(angle=40, hjust=1, size = 7))+
    geom_label(data = data_in, aes(x=variable, y=Mean-label_shift, label=round(Mean, digits=2)), position = position_dodge(0.9), label.padding = unit(0.1, "lines"))
}

## PLOT QUESTION RESULTS ##
# Plot results - Retention & Satisfaction
p_SOI_allq_Ret <- allq_plot_fun(data_in = SOI_allq_Ret, label_shift = 1, title_in = "Retention & Satisfaction Average Scores, Mean & 95% Confidence Interval")
#print(p_SOI_allq_Ret)
picname <- "1Statewide_Retention.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)

# Plot results - Talent Development 
p_SOI_allq_Tal <- allq_plot_fun(data_in = SOI_allq_Tal, label_shift = 1, title_in = "Talent Development Average Scores, Mean & 95% Confidence Interval")
#print(p_SOI_allq_Tal)
picname <- "1Statewide_Talent.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)

# Plot results - Work Environment
p_SOI_allq_Env <- allq_plot_fun(data_in = SOI_allq_Env, label_shift = 1, title_in = "Work Environment Average Scores, Mean & 95% Confidence Interval")
#print(p_SOI_allq_Env)
picname <- "1Statewide_WorkEnvironment.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)

# Plot results - Worker Evaluations
p_SOI_allq_Eval <- allq_plot_fun(data_in = SOI_allq_Eval, label_shift = 1, title_in = "Worker Evaluations Average Scores, Mean & 95% Confidence Interval")
#print(p_SOI_allq_Eval)
picname <- "1Statewide_Evaluations.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)

# Plot results - Customer Interactions
p_SOI_allq_Cust <- allq_plot_fun(data_in = SOI_allq_Cust, label_shift = 1, title_in = "Customer Interactions Average Scores, Mean & 95% Confidence Interval")
#print(p_SOI_allq_Cust)
picname <- "1Statewide_Customer.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)

# Plot results - Work Unit
p_SOI_allq_Unit <- allq_plot_fun(data_in = SOI_allq_Unit, label_shift = 1, title_in = "Work Unit Average Scores, Mean & 95% Confidence Interval")
#print(p_SOI_allq_Unit)
picname <- "1Statwide_WorkUnit.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)

# Plot results - Supervision
p_SOI_allq_Sup <- allq_plot_fun(data_in = SOI_allq_Sup, label_shift = 1, title_in = "Supervision Average Scores, Mean & 95% Confidence Interval")
#print(p_SOI_allq_Sup)
picname <- "1Statewide_Supervisor.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)

# Plot results - Leadership
p_SOI_allq_Lead <- allq_plot_fun(data_in = SOI_allq_Lead, label_shift = 1, title_in = "Leadership Average Scores, Mean & 95% Confidence Interval")
#print(p_SOI_allq_Lead)
picname <- "1Statewide_Leadership.jpg"
ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)


## Agency Level Comparison ##
# prep dataset aggregating all agency responses - cast the melted questions into mean/sd by question, but also sort on agency
agency_allq_mean <- dcast(allq_fullsetm, SurveyYear.f + variable + Agency ~ ., mean, na.rm=TRUE) #mean
agency_allq_sd <- dcast(allq_fullsetm, SurveyYear.f + variable + Agency ~ ., sd, na.rm=TRUE) #standard deviation
agency_allq_count <- dcast(allq_fullsetm, SurveyYear.f + variable + Agency ~ .)

names(agency_allq_mean)[4] <- "Mean"
names(agency_allq_sd)[4] <- "SD"
names(agency_allq_count)[4] <- "Count"

# Combine the mean and SD into one dataset, along with the count of how many respondents (sorted by year, agency, and question)
Agency_allq <- cbind(agency_allq_mean,agency_allq_sd$SD,agency_allq_count$Count)
names(Agency_allq)[5] <- "SD"
names(Agency_allq)[6] <- "Count"

Agency_allq <- na.omit(Agency_allq) #drop the glitch blank years caused by melting & casting the data

# Gatekeeper dataset - used to determine if there were 10 or less responses in a year
Agency_2small <- subset(Agency_allq, variable == "StateComp")

# List of agency names and abbreviations, to be used in file creation.
names <- c("Abraham Lincoln Presidential Library and Museum","Aging, Department on","Agriculture, Department of","Arts Council","Capital Development Board",
           "Central Management Services, Department of","Children and Family Services, Department of","Civil Service Commission, Illinois",
           "Commerce and Economic Opportunity, Department of","Commerce Commission, Illinois","Community College Board","Corrections, Department of",
           "Criminal Justice Information Authority","Deaf and Hard of Hearing Commission, Illinois","Developmental Disabilities, Illinois Council on",
           "Education, Illinois State Board of","Educational Labor Relations Board","Emergency Management Agency","Employment Security, Department of",
           "Environmental Protection Agency","Financial and Professional Regulation, Department of","Gaming Board, Illinois","Guardianship and Advocacy Commission",
           "Healthcare and Family Services - Formerly known as Public Aid, Dept. of","Higher Education, Board of","Human Rights Commission","Human Rights, Department of",
           "Human Services, Department of","Innovation and Technology, Department of","Insurance Department of","Juvenile Justice, Department of","Labor Relations Board",
           "Labor, Department of","Law Enforcement Training and Standards Board, Illinois","Liquor Control Commission","Lottery, Illinois",
           "Management and Budget, Office of","Medical District Commission","Natural Resources, Department of",
           "Pollution Control Board","Power Agency, Illinois","Prisoner Review Board, Illinois","Property Tax Appeal Board","Public Health, Department of",
           "Racing Board, Illinois","Revenue, Department of","State Fire Marshal","State Police, Illinois","State Retirement Systems",
           "Student Assistance Commission, Illinois","Toll Highway Authority, Illinois State","Transportation, Department of","Veterans Affairs, Department of",
           "Volunteerism Community Service, Governor's Commission on","Workers' Compensation Commission, Illinois")
abbrevs <- c("ALPLM","AGE","AG","AC","CDB","CMS","DCFS","CSC","DCEO","ICC","ICCB","DOC","ICJIA","DHHC","CDD","ISBE","ELRB","IEMA","IDES","IEPA","IDFPR","GAM","GAC",
             "HFS","IBHE","HRC","DHR","DHS","DoIT","DOI","DJJ","ILRB","DOL","LETSB","LCC","LOT","GOMB","MDC","IDNR","PCB","IPA","PRB","PTAB","DPH","IRB","REV","OSFM","ISP","SRS",
             "ISAC","TOL","DOT","VA","CVCS","WCC")
Xagency <- tibble(names, abbrevs)



## For loop that creates each agency's file ##

for (i in seq(1, nrow(Xagency), 1)) {
  size <- subset(Agency_2small, Agency_2small$Agency == Xagency$names[i])
  
  test_col <- stri_cmp(Xagency$names[i], Agency_2small$Agency) #compare the current agency to the column of all agency names
  numyears <- sum(test_col == 0) #use the compare column, where 0 means a match, to determine how many years are in the dataset
  
  if (sum(size$Count<10)>0){
    print(paste(Xagency$names[i],"is too small to display."))
  }

  ### FOR AGENCIES WITH JUST 1 YEAR OF DATA ###  
  else if (numyears == 1){
    print(paste(Xagency$names[i],"has one (1) year."))
    # select the results that belong to the agency currently being tested
    curr_Agency <- subset(Agency_allq, Agency == Xagency$names[i])
    # create a subset that is just the composite data
    curr_Agency_comps <- subset(curr_Agency, variable == "RetentionComp" | variable =="TalentComp" | variable =="EnviroComp" | 
                                  variable == "EvalComp" | variable == "CustomerComp" | variable == "UnitComp" | variable == "SuperComp" |
                                  variable == "LeaderComp")
    curr_Agency_state <- subset(curr_Agency, variable == "StateComp")
    # remove the composite data to leave just the questions
    curr_Agency <- subset(curr_Agency, variable != "RetentionComp" & variable !="TalentComp" & variable !="EnviroComp" & 
                            variable != "EvalComp" & variable != "CustomerComp" & variable != "UnitComp" & variable != "SuperComp" &
                            variable != "LeaderComp" & variable != "StateComp")
    
    # Add the breakpoint into each question
    brkpt <-round(stri_length(curr_Agency$variable)/2, digits=0) #add breakpoint for plotting - splits halfway through question
    stri_sub(curr_Agency$variable,brkpt+1,brkpt) <- "-\n"
    
    # combine the agency's average data with the statewide average for plotting
    SOI_subset_only_rbind <- subset(SOI_subset_only, SurveyYear.f == 2017)
    SOI_subset_only_rbind$SurveyYear.f <- factor(SOI_subset_only_rbind$SurveyYear.f)
    SOI_subset_only_rbind$SurveyYear.f <- "Statewide"
    Agency_comps_2plot <- rbind(curr_Agency_comps[,c(1:2,4:5)],SOI_subset_only_rbind)
    SOI_state_only_rbind <- subset(SOI_state_only, SurveyYear.f == 2017)
    SOI_state_only_rbind$SurveyYear.f <- factor(SOI_state_only_rbind$SurveyYear.f)
    SOI_state_only_rbind$SurveyYear.f <- "Statewide"
    Agency_state_2plot <- rbind(curr_Agency_state[,c(1:2,4:5)],SOI_state_only_rbind)
    
    # get the agency's subset of the full set of composite data in order to t-test
    agevsst <- subset(fullset, SurveyYear.f==2017) #delta between agency in 2017 and statewide in 2017
    agevsst <- na.omit(agevsst)
    agevsst <- cbind(agevsst, data.frame(agevsst$Agency == Xagency$names[i]))
    names(agevsst)[17] <- "Flag" # setup t-test between agency's data and all other data (the statistically correct way to do this - not including themselves in the statewide)
    
    # t-test the two different samples
    agevsst_ttest <- lapply(agevsst[7:15], function(i) t.test(i ~ agevsst$Flag))
    
    # extract the p-values
    agency_ttest_results <- c(agevsst_ttest$RetentionComp$p.value, agevsst_ttest$TalentComp$p.value, agevsst_ttest$EnviroComp$p.value, agevsst_ttest$EvalComp$p.value,
                              agevsst_ttest$CustomerComp$p.value, agevsst_ttest$UnitComp$p.value, agevsst_ttest$SuperComp$p.value, agevsst_ttest$LeaderComp$p.value)
    
    # test signficance from p-values
    CInul_age <- agency_ttest_results>0.05 & agency_ttest_results != " "
    CI95_age <- agency_ttest_results<=0.05 & agency_ttest_results>0.01 & agency_ttest_results != " "
    CI99_age <- agency_ttest_results<=0.01 & agency_ttest_results != " "
    
    # convert to significance level
    agency_ttest_results <- replace(agency_ttest_results,CI99_age,"**")
    agency_ttest_results <- replace(agency_ttest_results,CI95_age,"*")
    agency_ttest_results <- replace(agency_ttest_results,CInul_age," ")
    
    agency_ttest_results<- c(" "," "," "," "," "," "," "," ",agency_ttest_results)
    
    # bind significance level to the plotting data
    Agency_comps_2plot <- cbind(Agency_comps_2plot,agency_ttest_results)
    names(Agency_comps_2plot)[5] <- "sig"
    
    # Plot results - basic plot: composite scores
    p_Agency_comps<- ggplot(Agency_comps_2plot, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))+
      geom_text(aes(label=sig), position = position_dodge(width = 1.1), vjust = -1)
    
    # Cleaned up bar plot
    p_Agency_comps <- p_Agency_comps+labs(title=paste(Xagency$abbrevs[i],"Survey Section Composite Scores, Mean & 95% Confidence Interval",sep = ", "), x="Survey Focus", y = "Average Composite Score",
                                          caption = "Significance: * indicates 95% certainty that the % change is due to actual shifts in responses, while ** indicates 99% certainty.\nAn * over Statewide indicates a significant difference from the statewide population.")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      scale_x_discrete(labels=c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership"))  + 
      theme(axis.text.x=element_text(angle=30, hjust=1))
    
    #print(p_Agency_comps)
    picname <- paste(Xagency$abbrevs[i],"_Composite.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # extract the p-values
    agency_ttest_state <- agevsst_ttest$StateComp$p.value
    
    # test signficance from p-values
    CInul_age_st <- agency_ttest_state>0.05 & agency_ttest_state != " "
    CI95_age_st <- agency_ttest_state<=0.05 & agency_ttest_state>0.01 & agency_ttest_state != " "
    CI99_age_st <- agency_ttest_state<=0.01 & agency_ttest_state != " "
    
    # convert to significance level
    agency_ttest_state <- replace(agency_ttest_state,CI99_age_st,"**")
    agency_ttest_state <- replace(agency_ttest_state,CI95_age_st,"*")
    agency_ttest_state <- replace(agency_ttest_state,CInul_age_st," ")
    
    agency_ttest_state <- c(" ", agency_ttest_state)
    
    # bind significance level to the plotting data
    Agency_state_2plot <- cbind(Agency_state_2plot,agency_ttest_state)
    names(Agency_state_2plot)[5] <- "sig"
    
    # Plot results - basic plot: statewide scores
    p_Agency_state<- ggplot(Agency_state_2plot, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))+
      geom_text(aes(label=sig), position = position_dodge(width = 1.1), vjust = -1)
    
    # Cleaned up bar plot
    p_Agency_state <- p_Agency_state+labs(title=paste(Xagency$abbrevs[i],"Survey Section Composite Scores, Mean & 95% Confidence Interval",sep = ", "), x="Statewide Composite Score", y = "Average Composite Score",
                                          caption = "Significance: * indicates 95% certainty that the % change is due to actual shifts in responses, while ** indicates 99% certainty.\nAn * over Statewide indicates a significant difference from the statewide population.")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      scale_x_discrete(labels="Statewide Composite Score")
    
    #print(p_Agency_state)
    picname <- paste(Xagency$abbrevs[i],"_StatewideComposite.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    ## Plot out the question results over time ##
    
    # Create filters based on variable names (flexible instead of using the numbering subsetting format)
    ### WILL NEED REDONE EVERY YEAR ###
    filter_allq_Ret <- SOI_allq$variable[c(103:107)]
    filter_allq_Tal <- SOI_allq$variable[c(109:111)]
    filter_allq_Env <- SOI_allq$variable[c(113:119)]
    filter_allq_Eval <- SOI_allq$variable[c(121:127)]
    filter_allq_Cust <- SOI_allq$variable[c(129:132)]
    filter_allq_Unit <- SOI_allq$variable[c(134:138)]
    filter_allq_Sup <- SOI_allq$variable[c(140:144)]
    filter_allq_Lead <- SOI_allq$variable[c(146:151)]
    
    # Get subset for plotting using dplyr functions
    age_allq_Ret <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Ret)
    age_allq_Tal <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Tal)
    age_allq_Env <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Env)
    age_allq_Eval <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Eval)
    age_allq_Cust <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Cust)
    age_allq_Unit <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Unit)
    age_allq_Sup <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Sup)
    age_allq_Lead <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Lead)
    
    # Plot results - Retention & Satisfaction - basic plot
    p_age_allq_Ret<- ggplot(age_allq_Ret, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Ret <- p_age_allq_Ret+labs(title="Retention & Satisfaction Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Ret)
    picname <- paste(Xagency$abbrevs[i],"_Retention.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Talent Development - basic plot
    p_age_allq_Tal<- ggplot(age_allq_Tal, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Tal <- p_age_allq_Tal+labs(title="Talent Development Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Tal)
    picname <- paste(Xagency$abbrevs[i],"_Talent.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Work Environment - basic plot
    p_age_allq_Env<- ggplot(age_allq_Env, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Env <- p_age_allq_Env+labs(title="Work Environment Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Env)
    picname <- paste(Xagency$abbrevs[i],"_WorkEnvironment.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Worker Evaluations - basic plot
    p_age_allq_Eval <- ggplot(age_allq_Eval, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Eval <- p_age_allq_Eval+labs(title="Worker Evaluations Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Eval)
    picname <- paste(Xagency$abbrevs[i],"_Evaluations.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Customer Interactions - basic plot
    p_age_allq_Cust<- ggplot(age_allq_Cust, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Cust <- p_age_allq_Cust+labs(title="Customer Interactions Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Cust)
    picname <- paste(Xagency$abbrevs[i],"_Customer.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Work Unit - basic plot
    p_age_allq_Unit<- ggplot(age_allq_Unit, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Unit <- p_age_allq_Unit+labs(title="Work Unit Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Unit)
    picname <- paste(Xagency$abbrevs[i],"_WorkUnit.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Supervision - basic plot
    p_age_allq_Sup<- ggplot(age_allq_Sup, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Sup <- p_age_allq_Sup+labs(title="Supervision Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Sup)
    picname <- paste(Xagency$abbrevs[i],"_Supervisor.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Leadership - basic plot
    p_age_allq_Lead <- ggplot(age_allq_Lead, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Lead <- p_age_allq_Lead+labs(title="Leadership Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Lead)
    picname <- paste(Xagency$abbrevs[i],"_Leadership.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
  }
 
  ### FOR AGENCIES WITH TWO YEARS OF DATA ### 
  else if (numyears == 2){
    print(paste(Xagency$names[i],"has two (2) years."))
    # select the results that belong to the agency currently being tested
    curr_Agency <- subset(Agency_allq, Agency == Xagency$names[i])
    # create a subset that is just the composite data
    curr_Agency_comps <- subset(curr_Agency, variable == "RetentionComp" | variable =="TalentComp" | variable =="EnviroComp" |
                                  variable == "EvalComp" | variable == "CustomerComp" | variable == "UnitComp" | variable == "SuperComp" |
                                  variable == "LeaderComp")
    curr_Agency_state <- subset(curr_Agency, variable == "StateComp")
    # remove the composite data to leave just the questions
    curr_Agency <- subset(curr_Agency, variable != "RetentionComp" & variable !="TalentComp" & variable !="EnviroComp" &
                            variable != "EvalComp" & variable != "CustomerComp" & variable != "UnitComp" & variable != "SuperComp" &
                            variable != "LeaderComp" & variable != "StateComp")
    
    # Add the breakpoint into each question
    brkpt <-round(stri_length(curr_Agency$variable)/2, digits=0) #add breakpoint for plotting - splits halfway through question
    stri_sub(curr_Agency$variable,brkpt+1,brkpt) <- "-\n"
    
    # combine the agency's average data with the statewide average for plotting
    SOI_subset_only_rbind <- subset(SOI_subset_only, SurveyYear.f == 2017)
    SOI_subset_only_rbind$SurveyYear.f <- factor(SOI_subset_only_rbind$SurveyYear.f)
    SOI_subset_only_rbind$SurveyYear.f <- "Statewide"
    Agency_comps_2plot <- rbind(curr_Agency_comps[,c(1:2,4:5)],SOI_subset_only_rbind)
    SOI_state_only_rbind <- subset(SOI_state_only, SurveyYear.f == 2017)
    SOI_state_only_rbind$SurveyYear.f <- factor(SOI_state_only_rbind$SurveyYear.f)
    SOI_state_only_rbind$SurveyYear.f <- "Statewide"
    Agency_state_2plot <- rbind(curr_Agency_state[,c(1:2,4:5)],SOI_state_only_rbind)
    
    # get the agency's subset of the full set of composite data in order to t-test
    agevsst <- subset(fullset, SurveyYear.f==2017) #delta between agency in 2017 and statewide in 2017
    agevsst <- na.omit(agevsst)
    agevsst <- cbind(agevsst, data.frame(agevsst$Agency == Xagency$names[i]))
    names(agevsst)[17] <- "Flag" # setup t-test between agency's data and all other data (the statistically correct way to do this - not including themselves in the statewide)
    
    # get the agency's data from the past 2 years to compare
    agevslast <- subset(fullset, SurveyYear.f==2017 | SurveyYear.f == 2016) #delta between agency in 2017 and agency in 2016
    agevslast <- subset(agevslast, Agency == Xagency$names[i])
    agevslast <- na.omit(agevslast)
    
    # t-test the two different samples
    agevsst_ttest <- lapply(agevsst[7:15], function(i) t.test(i ~ agevsst$Flag))
    agevslast_ttest <- lapply(agevslast[7:15], function(i) t.test(i ~ agevslast$SurveyYear.f))
    
    # extract the p-values
    agency_ttest_results <- c(agevsst_ttest$RetentionComp$p.value, agevsst_ttest$TalentComp$p.value, agevsst_ttest$EnviroComp$p.value, agevsst_ttest$EvalComp$p.value,
                              agevsst_ttest$CustomerComp$p.value, agevsst_ttest$UnitComp$p.value, agevsst_ttest$SuperComp$p.value, agevsst_ttest$LeaderComp$p.value,
                              agevslast_ttest$RetentionComp$p.value, agevslast_ttest$TalentComp$p.value, agevslast_ttest$EnviroComp$p.value, agevslast_ttest$EvalComp$p.value,
                              agevslast_ttest$CustomerComp$p.value, agevslast_ttest$UnitComp$p.value, agevslast_ttest$SuperComp$p.value, agevslast_ttest$LeaderComp$p.value)
    
    # test signficance from p-values
    CInul_age <- agency_ttest_results>0.05 & agency_ttest_results != " "
    CI95_age <- agency_ttest_results<=0.05 & agency_ttest_results>0.01 & agency_ttest_results != " "
    CI99_age <- agency_ttest_results<=0.01 & agency_ttest_results != " "
    
    # convert to significance level
    agency_ttest_results <- replace(agency_ttest_results,CI99_age,"**")
    agency_ttest_results <- replace(agency_ttest_results,CI95_age,"*")
    agency_ttest_results <- replace(agency_ttest_results,CInul_age," ")
    
    agency_ttest_results<- c(" "," "," "," "," "," "," "," ",agency_ttest_results)
    
    # bind significance level to the plotting data
    Agency_comps_2plot <- cbind(Agency_comps_2plot,agency_ttest_results)
    names(Agency_comps_2plot)[5] <- "sig"
    
    # Plot results - basic plot: composite scores
    p_Agency_comps<- ggplot(Agency_comps_2plot, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))+
      geom_text(aes(label=sig), position = position_dodge(width = 1.1), vjust = -1)
    
    # Cleaned up bar plot
    p_Agency_comps <- p_Agency_comps+labs(title=paste(Xagency$abbrevs[i],"Survey Section Composite Scores, Mean & 95% Confidence Interval",sep = ", "), x="Survey Focus", y = "Average Composite Score",
                                          caption = "Significance: * indicates 95% certainty that the % change is due to actual shifts in responses, while ** indicates 99% certainty.\nAn * over 2017 indicates significant change from 2016 to 2017, while over Statewide indicates a significant difference from the statewide population.")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      scale_x_discrete(labels=c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership"))  +
      theme(axis.text.x=element_text(angle=30, hjust=1))
    
    #print(p_Agency_comps)
    picname <- paste(Xagency$abbrevs[i],"_Composite.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # extract the p-values
    agency_ttest_state <- c(agevslast_ttest$StateComp$p.value, agevsst_ttest$StateComp$p.value)
    
    # test signficance from p-values
    CInul_age_st <- agency_ttest_state>0.05 & agency_ttest_state != " "
    CI95_age_st <- agency_ttest_state<=0.05 & agency_ttest_state>0.01 & agency_ttest_state != " "
    CI99_age_st <- agency_ttest_state<=0.01 & agency_ttest_state != " "
    
    # convert to significance level
    agency_ttest_state <- replace(agency_ttest_state,CI99_age_st,"**")
    agency_ttest_state <- replace(agency_ttest_state,CI95_age_st,"*")
    agency_ttest_state <- replace(agency_ttest_state,CInul_age_st," ")
    
    agency_ttest_state <- c(" ", agency_ttest_state)
    
    # bind significance level to the plotting data
    Agency_state_2plot <- cbind(Agency_state_2plot,agency_ttest_state)
    names(Agency_state_2plot)[5] <- "sig"
    
    # Plot results - basic plot: statewide scores
    p_Agency_state<- ggplot(Agency_state_2plot, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))+
      geom_text(aes(label=sig), position = position_dodge(width = 1.1), vjust = -1)
    
    # Cleaned up bar plot
    p_Agency_state <- p_Agency_state+labs(title=paste(Xagency$abbrevs[i],"Survey Section Composite Scores, Mean & 95% Confidence Interval",sep = ", "), x="Statewide Composite Score", y = "Average Composite Score",
                                          caption = "Significance: * indicates 95% certainty that the % change is due to actual shifts in responses, while ** indicates 99% certainty.\nAn * over 2017 indicates significant change from 2016 to 2017, while over * indicates a significant difference from the statewide population.")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      scale_x_discrete(labels="Statewide Composite Score")
    
    #print(p_Agency_state)
    picname <- paste(Xagency$abbrevs[i],"_StatewideComposite.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    ## Plot out the question results over time ##
    
    # Create filters based on variable names (flexible instead of using the numbering subsetting format)
    filter_allq_Ret <- SOI_allq$variable[c(52:56,103:107)]
    filter_allq_Tal <- SOI_allq$variable[c(58:60,109:111)]
    filter_allq_Env <- SOI_allq$variable[c(62:68,113:119)]
    filter_allq_Eval <- SOI_allq$variable[c(70:76,121:127)]
    filter_allq_Cust <- SOI_allq$variable[c(78:81,129:132)]
    filter_allq_Unit <- SOI_allq$variable[c(83:87,134:138)]
    filter_allq_Sup <- SOI_allq$variable[c(89:93,140:144)]
    filter_allq_Lead <- SOI_allq$variable[c(95:100,146:151)]
    
    # Get subset for plotting using dplyr functions
    age_allq_Ret <- curr_Agency %>%
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Ret)
    age_allq_Tal <- curr_Agency %>%
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Tal)
    age_allq_Env <- curr_Agency %>%
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Env)
    age_allq_Eval <- curr_Agency %>%
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Eval)
    age_allq_Cust <- curr_Agency %>%
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Cust)
    age_allq_Unit <- curr_Agency %>%
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Unit)
    age_allq_Sup <- curr_Agency %>%
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Sup)
    age_allq_Lead <- curr_Agency %>%
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Lead)
    
    # Plot results - Retention & Satisfaction - basic plot
    p_age_allq_Ret<- ggplot(age_allq_Ret, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Ret <- p_age_allq_Ret+labs(title="Retention & Satisfaction Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Ret)
    picname <- paste(Xagency$abbrevs[i],"_Retention.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Talent Development - basic plot
    p_age_allq_Tal<- ggplot(age_allq_Tal, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Tal <- p_age_allq_Tal+labs(title="Talent Development Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Tal)
    picname <- paste(Xagency$abbrevs[i],"_Talent.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Work Environment - basic plot
    p_age_allq_Env<- ggplot(age_allq_Env, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Env <- p_age_allq_Env+labs(title="Work Environment Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Env)
    picname <- paste(Xagency$abbrevs[i],"_WorkEnvironment.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Worker Evaluations - basic plot
    p_age_allq_Eval <- ggplot(age_allq_Eval, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Eval <- p_age_allq_Eval+labs(title="Worker Evaluations Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Eval)
    picname <- paste(Xagency$abbrevs[i],"_Evaluations.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Customer Interactions - basic plot
    p_age_allq_Cust<- ggplot(age_allq_Cust, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Cust <- p_age_allq_Cust+labs(title="Customer Interactions Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Cust)
    picname <- paste(Xagency$abbrevs[i],"_Customer.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Work Unit - basic plot
    p_age_allq_Unit<- ggplot(age_allq_Unit, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Unit <- p_age_allq_Unit+labs(title="Work Unit Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Unit)
    picname <- paste(Xagency$abbrevs[i],"_WorkUnit.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Supervision - basic plot
    p_age_allq_Sup<- ggplot(age_allq_Sup, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Sup <- p_age_allq_Sup+labs(title="Supervision Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Sup)
    picname <- paste(Xagency$abbrevs[i],"_Supervisor.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Leadership - basic plot
    p_age_allq_Lead <- ggplot(age_allq_Lead, aes(x=variable, y=Mean, fill=SurveyYear.f)) +
      geom_bar(stat="identity", color="black",
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Lead <- p_age_allq_Lead+labs(title="Leadership Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") +
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Lead)
    picname <- paste(Xagency$abbrevs[i],"_Leadership.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
  }
  
  else if (numyears == 3) {
    # select the results that belong to the agency currently being tested
    curr_Agency <- subset(Agency_allq, Agency == Xagency$names[i])
    # create a subset that is just the composite data
    curr_Agency_comps <- subset(curr_Agency, variable == "RetentionComp" | variable =="TalentComp" | variable =="EnviroComp" | 
                                  variable == "EvalComp" | variable == "CustomerComp" | variable == "UnitComp" | variable == "SuperComp" |
                                  variable == "LeaderComp")
    curr_Agency_state <- subset(curr_Agency, variable == "StateComp")
    # remove the composite data to leave just the questions
    curr_Agency <- subset(curr_Agency, variable != "RetentionComp" & variable !="TalentComp" & variable !="EnviroComp" & 
                            variable != "EvalComp" & variable != "CustomerComp" & variable != "UnitComp" & variable != "SuperComp" &
                            variable != "LeaderComp" & variable != "StateComp")
    
    # Add the breakpoint into each question
    brkpt <-round(stri_length(curr_Agency$variable)/2, digits=0) #add breakpoint for plotting - splits halfway through question
    stri_sub(curr_Agency$variable,brkpt+1,brkpt) <- "-\n"
    
    # combine the agency's average data with the statewide average for plotting
    SOI_subset_only_rbind <- subset(SOI_subset_only, SurveyYear.f == 2017)
    SOI_subset_only_rbind$SurveyYear.f <- factor(SOI_subset_only_rbind$SurveyYear.f)
    SOI_subset_only_rbind$SurveyYear.f <- "Statewide"
    Agency_comps_2plot <- rbind(curr_Agency_comps[,c(1:2,4:5)],SOI_subset_only_rbind)
    SOI_state_only_rbind <- subset(SOI_state_only, SurveyYear.f == 2017)
    SOI_state_only_rbind$SurveyYear.f <- factor(SOI_state_only_rbind$SurveyYear.f)
    SOI_state_only_rbind$SurveyYear.f <- "Statewide"
    Agency_state_2plot <- rbind(curr_Agency_state[,c(1:2,4:5)],SOI_state_only_rbind)
    
    # get the agency's subset of the full set of composite data in order to t-test
    agevsst <- subset(fullset, SurveyYear.f==2017) #delta between agency in 2017 and statewide in 2017
    agevsst <- na.omit(agevsst)
    agevsst <- cbind(agevsst, data.frame(agevsst$Agency == Xagency$names[i]))
    names(agevsst)[17] <- "Flag" # setup t-test between agency's data and all other data (the statistically correct way to do this - not including themselves in the statewide)
    
    # get the agency's data from the past 2 years to compare
    agevslast <- subset(fullset, SurveyYear.f==2017 | SurveyYear.f == 2016) #delta between agency in 2017 and agency in 2016
    agevslast <- subset(agevslast, Agency == Xagency$names[i])
    agevslast <- na.omit(agevslast)
    
    # t-test the two different samples
    agevsst_ttest <- lapply(agevsst[7:15], function(i) t.test(i ~ agevsst$Flag))
    agevslast_ttest <- lapply(agevslast[7:15], function(i) t.test(i ~ agevslast$SurveyYear.f))
    
    # extract the p-values
    agency_ttest_results <- c(agevsst_ttest$RetentionComp$p.value, agevsst_ttest$TalentComp$p.value, agevsst_ttest$EnviroComp$p.value, agevsst_ttest$EvalComp$p.value,
                              agevsst_ttest$CustomerComp$p.value, agevsst_ttest$UnitComp$p.value, agevsst_ttest$SuperComp$p.value, agevsst_ttest$LeaderComp$p.value,
                              agevslast_ttest$RetentionComp$p.value, agevslast_ttest$TalentComp$p.value, agevslast_ttest$EnviroComp$p.value, agevslast_ttest$EvalComp$p.value,
                              agevslast_ttest$CustomerComp$p.value, agevslast_ttest$UnitComp$p.value, agevslast_ttest$SuperComp$p.value, agevslast_ttest$LeaderComp$p.value)
    
    # test signficance from p-values
    CInul_age <- agency_ttest_results>0.05 & agency_ttest_results != " "
    CI95_age <- agency_ttest_results<=0.05 & agency_ttest_results>0.01 & agency_ttest_results != " "
    CI99_age <- agency_ttest_results<=0.01 & agency_ttest_results != " "
    
    # convert to significance level
    agency_ttest_results <- replace(agency_ttest_results,CI99_age,"**")
    agency_ttest_results <- replace(agency_ttest_results,CI95_age,"*")
    agency_ttest_results <- replace(agency_ttest_results,CInul_age," ")
    
    agency_ttest_results<- c(" "," "," "," "," "," "," "," "," "," "," "," "," "," "," "," ",agency_ttest_results)
    
    # bind significance level to the plotting data
    Agency_comps_2plot <- cbind(Agency_comps_2plot,agency_ttest_results)
    names(Agency_comps_2plot)[5] <- "sig"
    
    # Plot results - basic plot: composite scores
    p_Agency_comps<- ggplot(Agency_comps_2plot, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))+
      geom_text(aes(label=sig), position = position_dodge(width = 1.1), vjust = -1)
    
    # Cleaned up bar plot
    p_Agency_comps <- p_Agency_comps+labs(title=paste(Xagency$abbrevs[i],"Survey Section Composite Scores, Mean & 95% Confidence Interval",sep = ", "), x="Survey Focus", y = "Average Composite Score",
                                          caption = "Significance: * indicates 95% certainty that the % change is due to actual shifts in responses, while ** indicates 99% certainty.\nAn * over 2017 indicates significant change from 2016 to 2017, while over Statewide indicates a significant difference from the statewide population.")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      scale_x_discrete(labels=c("Retention & Satisfaction", "Talent Development", "Work Environment", "Worker Evaluations", "Customer Interactions", "Work Unit", "Supervision", "Leadership"))  + 
      theme(axis.text.x=element_text(angle=30, hjust=1))
    
    #print(p_Agency_comps)
    picname <- paste(Xagency$abbrevs[i],"_Composite.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # extract the p-values
    agency_ttest_state <- c(agevslast_ttest$StateComp$p.value, agevsst_ttest$StateComp$p.value)
    
    # test signficance from p-values
    CInul_age_st <- agency_ttest_state>0.05 & agency_ttest_state != " "
    CI95_age_st <- agency_ttest_state<=0.05 & agency_ttest_state>0.01 & agency_ttest_state != " "
    CI99_age_st <- agency_ttest_state<=0.01 & agency_ttest_state != " "
    
    # convert to significance level
    agency_ttest_state <- replace(agency_ttest_state,CI99_age_st,"**")
    agency_ttest_state <- replace(agency_ttest_state,CI95_age_st,"*")
    agency_ttest_state <- replace(agency_ttest_state,CInul_age_st," ")
    
    agency_ttest_state <- c(" ", " ", agency_ttest_state)
    
    # bind significance level to the plotting data
    Agency_state_2plot <- cbind(Agency_state_2plot,agency_ttest_state)
    names(Agency_state_2plot)[5] <- "sig"
    
    # Plot results - basic plot: statewide scores
    p_Agency_state<- ggplot(Agency_state_2plot, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))+
      geom_text(aes(label=sig), position = position_dodge(width = 1.1), vjust = -1)
    
    # Cleaned up bar plot
    p_Agency_state <- p_Agency_state+labs(title=paste(Xagency$abbrevs[i],"Survey Section Composite Scores, Mean & 95% Confidence Interval",sep = ", "), x="Statewide Composite Score", y = "Average Composite Score",
                                          caption = "Significance: * indicates 95% certainty that the % change is due to actual shifts in responses, while ** indicates 99% certainty.\nAn * over 2017 indicates significant change from 2016 to 2017, while over Statewide indicates a significant difference from the statewide population.")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      scale_x_discrete(labels="Statewide Composite Score")
    
    #print(p_Agency_state)
    picname <- paste(Xagency$abbrevs[i],"_StatewideComposite.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    ## Plot out the question results over time ##
    
    # Create filters based on variable names (flexible instead of using the numbering subsetting format)
    filter_allq_Ret <- SOI_allq$variable[c(1:5,52:56,103:107)]
    filter_allq_Tal <- SOI_allq$variable[c(7:9,58:60,109:111)]
    filter_allq_Env <- SOI_allq$variable[c(11:17,62:68,113:119)]
    filter_allq_Eval <- SOI_allq$variable[c(19:25,70:76,121:127)]
    filter_allq_Cust <- SOI_allq$variable[c(27:30,78:81,129:132)]
    filter_allq_Unit <- SOI_allq$variable[c(32:36,83:87,134:138)]
    filter_allq_Sup <- SOI_allq$variable[c(38:42,89:93,140:144)]
    filter_allq_Lead <- SOI_allq$variable[c(44:49,95:100,146:151)]
    
    # Get subset for plotting using dplyr functions
    age_allq_Ret <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Ret)
    age_allq_Tal <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Tal)
    age_allq_Env <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Env)
    age_allq_Eval <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Eval)
    age_allq_Cust <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Cust)
    age_allq_Unit <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Unit)
    age_allq_Sup <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Sup)
    age_allq_Lead <- curr_Agency %>% 
      select(SurveyYear.f, variable, Agency, Mean, SD) %>%
      filter(variable %in% filter_allq_Lead)
    
    # Plot results - Retention & Satisfaction - basic plot
    p_age_allq_Ret<- ggplot(age_allq_Ret, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Ret <- p_age_allq_Ret+labs(title="Retention & Satisfaction Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Ret)
    picname <- paste(Xagency$abbrevs[i],"_Retention.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Talent Development - basic plot
    p_age_allq_Tal<- ggplot(age_allq_Tal, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Tal <- p_age_allq_Tal+labs(title="Talent Development Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Tal)
    picname <- paste(Xagency$abbrevs[i],"_Talent.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Work Environment - basic plot
    p_age_allq_Env<- ggplot(age_allq_Env, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Env <- p_age_allq_Env+labs(title="Work Environment Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Env)
    picname <- paste(Xagency$abbrevs[i],"_WorkEnvironment.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Worker Evaluations - basic plot
    p_age_allq_Eval <- ggplot(age_allq_Eval, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Eval <- p_age_allq_Eval+labs(title="Worker Evaluations Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Eval)
    picname <- paste(Xagency$abbrevs[i],"_Evaluations.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Customer Interactions - basic plot
    p_age_allq_Cust<- ggplot(age_allq_Cust, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Cust <- p_age_allq_Cust+labs(title="Customer Interactions Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Cust)
    picname <- paste(Xagency$abbrevs[i],"_Customer.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Work Unit - basic plot
    p_age_allq_Unit<- ggplot(age_allq_Unit, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Unit <- p_age_allq_Unit+labs(title="Work Unit Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Unit)
    picname <- paste(Xagency$abbrevs[i],"_WorkUnit.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Supervision - basic plot
    p_age_allq_Sup<- ggplot(age_allq_Sup, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Sup <- p_age_allq_Sup+labs(title="Supervision Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Sup)
    picname <- paste(Xagency$abbrevs[i],"_Supervisor.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
    
    # Plot results - Leadership - basic plot
    p_age_allq_Lead <- ggplot(age_allq_Lead, aes(x=variable, y=Mean, fill=SurveyYear.f)) + 
      geom_bar(stat="identity", color="black", 
               position=position_dodge()) +
      geom_errorbar(aes(ymin=Mean-2*SD, ymax=Mean+2*SD), width=.2,
                    position=position_dodge(.9))
    
    # Cleaned up bar plot
    p_age_allq_Lead <- p_age_allq_Lead+labs(title="Leadership Average Scores, Mean & 95% Confidence Interval", x="Survey Question", y = "Average Score")+
      theme_minimal()+scale_fill_discrete(name = "Survey Year") + 
      theme(axis.text.x=element_text(angle=15, hjust=1))
    
    #print(p_age_allq_Lead)
    picname <- paste(Xagency$abbrevs[i],"_Leadership.jpg")
    ggsave(picname, plot = last_plot(), device = "jpeg", path = NULL, width = 12, height = 6, units = "in", dpi = 600, limitsize = TRUE)
  }
}



