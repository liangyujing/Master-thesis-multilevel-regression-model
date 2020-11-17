---
  title: "Aha and Heart Rate Variability"
output:
  pdf_document: default
word_document: default
---
  install.packages("BayesFactor")

```{r}
library(readxl)
library(naniar)
library(tidyverse) 
library(MESS)
library(lmerTest)
library(lme4)
library(cluster)
library(usdm)
library(outliers)
library(ggplot2)
library(GGally)
library(tidyr)
library(psych)
library(BayesFactor)

```



```{r}
########################################
#GET RAW BEHAVIORAL DATA FROM CSV FILES 
#######################################

setwd("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/MT/Data/data/data/")
PP <- c(1:62)

# Read 1st PP

data = read_excel("1_insight_PsychoPhysics.xlsx")

data<- data %>% 
  transmute(
    subject = participant,
    age = leeftijd,
    language = specificieer,
    trial_counter = CRAT_exp_trials.thisTrialN,
    solution_location = location,
    CRAT1 = CRAT1,
    correct_answer = corrAns,
    AlternativeCorrect_answer = second_correct,
    answer = inputText,
    ACC = correct,
    RT = CRAT_response.rt,
    confidence = confidence_rating.response,
    solution_type = solution_strategy_response.keys)


# Read remaining PPs

for (i in PP[-1]){
  
  datastr <- paste("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/MT/Data/data/data/",i,   "_insight_PsychoPhysics.xlsx", sep = "")
  
  data_n = read_excel(datastr)
  
  data_n<- data_n %>% 
    transmute(
      subject = participant,
      age = leeftijd,
      language = specificieer,
      trial_counter = CRAT_exp_trials.thisTrialN,
      solution_location = location,
      CRAT1 = CRAT1,
      correct_answer = corrAns,
      AlternativeCorrect_answer = second_correct,
      answer = inputText,
      ACC = correct,
      RT = CRAT_response.rt,
      confidence = confidence_rating.response,
      solution_type = solution_strategy_response.keys)
  
  data <- rbind(data, data_n)
  
}
```

```{r}
data$trial_counter <- as.integer(factor(data$trial_counter,levels=unique(data$trial_counter))) 
data$subject <- as.character(as.factor(data$subject))

```

```{r}
#######################################
#Delete all practice trials
######################################
experimental_df<-data%>%
  tidyr::drop_na(trial_counter)

#############################
#Replace all cells with None by NA
##############################
experimental_df<- experimental_df%>% dplyr::na_if("None")
experimental_df<- experimental_df%>% dplyr::na_if("")

```


```{R}
####################################
#Place all values on one line per trial/solved word puzzle
experimental_df<- experimental_df %>%
  group_by(subject, trial_counter)%>%
  summarise_all(funs(first(na.omit(.))))

experimental_df$solution_type<-as.factor(experimental_df$solution_type)

summary(experimental_df)
```

```{r}
#################################
#Delete all word puzzles that were not solved
###################################
experimental_df<-experimental_df%>%
  tidyr::drop_na(answer)
sum(is.na(experimental_df$answer))
summary(experimental_df$ACC)
summary(experimental_df)

```

```{r}
###########################################
#create factor of solution-type and convert RT and confidence to numeric values with a certain amount of decimal places left
experimental_df$solution_type<-as.factor(experimental_df$solution_type)
experimental_df$RT<-as.numeric(experimental_df$RT, digits = 3)
experimental_df$confidence<-as.numeric(experimental_df$confidence, digits = 2)
summary(experimental_df)

#Some preliminary descriptives for correct and incorrectly solved trials
describeBy(experimental_df$confidence, group=experimental_df$solution_type,mat=FALSE,type=3)
describeBy(experimental_df$RT, group=experimental_df$solution_type,mat=FALSE,type=3)
describeBy(experimental_df$ACC, group=experimental_df$solution_type,mat=FALSE,type=3)

#idem only for correctly solved trials
preliminary_data<-experimental_df%>%
  filter(ACC == 1)%>%
  group_by(solution_type)%>%
  summarise(mean_conf = mean(confidence), mean_RT = mean(RT))

summary(preliminary_data)

```

```{r}
############################################
#create number of correctly solved insight and non-insight word puzzles per participant 
###########################################
number_insight<-experimental_df%>%
  filter(solution_type == 1, ACC == 1)%>%
  group_by(subject)%>%
  count(ACC)

#number of correctly non-insight
number_NonInsight<-experimental_df%>%
  filter(solution_type == 2, ACC == 1)%>%
  group_by(subject)%>%
  count(ACC)


```

```{r}
###########################################################
#attach the number of trials solved with insight and non-insight to the df
#######################################################################
experimental_df$nrInsight <- NaN
for (s in unique(number_insight$subject)){
  experimental_df$nrInsight[experimental_df$subject==s] <- number_insight$n[number_insight$subject==s]
} 

experimental_df$nrNonInsight <- NaN
for (s in unique(number_NonInsight$subject)){
  experimental_df$nrNonInsight[experimental_df$subject==s] <- number_NonInsight$n[number_NonInsight$subject==s]
} 
```


```{r}
##############################
#Seperate Bilingual PP
################################
#Create a variable to seperate the natural Dutch speaking participants from the participants with another mother tongue
experimental_df$mother_tongue<- ifelse(experimental_df$language == "Nederlands" | experimental_df$language == "nederlands" | experimental_df$language == "dutch", 1, 0)

#check all subjects available
num_subj<-experimental_df%>%
  count(subject)

```

```{R}
###############################################
#add baseline RMSSD to data frame
#################################################
#create df with HRV data
artiifact_HRV_df = read_excel("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/MT/Data/data/data/artiifact_HRV.xlsx")

#replace NaN in df with real NaN R-wise
artiifact_HRV_df<- artiifact_HRV_df%>%dplyr::na_if("NaN")

#add to df with behavioral data RMSSD, High-frequency HRV, IBI reject
experimental_df$baseline_RMSSD <- NaN
for (s in unique(artiifact_HRV_df$subject)){
  experimental_df$baseline_RMSSD[experimental_df$subject==s] <- artiifact_HRV_df$Artiifact_RMSSD[artiifact_HRV_df$subject==s]
} 

experimental_df$HF_HRV <- NaN
for (s in unique(artiifact_HRV_df$subject)){
  experimental_df$HF_HRV[experimental_df$subject==s] <- artiifact_HRV_df$Artiifact_HF[artiifact_HRV_df$subject==s]
} 

experimental_df$num_reject_IBI <- NaN
for (s in unique(artiifact_HRV_df$subject)){
  experimental_df$num_reject_IBI[experimental_df$subject==s] <- artiifact_HRV_df$rejected_IBI[artiifact_HRV_df$subject==s]
} 

summary(experimental_df)
summary(experimental_df$ACC)
summary(preliminary_data)
```

```{R}
########################################
#Create data frames to perform analysis on
#####################################
#first create additional variable total number solved
experimental_df<-experimental_df%>%
  mutate(total_correct_solved = nrInsight + nrNonInsight)

#create two data frames one with correct and incorrect responses and one with only correct responses
mixedef_correct<-experimental_df %>%
  filter(ACC == 1) %>% 
  filter(solution_type != 3) %>% 
  mutate(baseline_RMSSD = as.numeric(baseline_RMSSD),
         HF_HRV = as.numeric(HF_HRV),
         num_reject_IBI = as.numeric(num_reject_IBI))

mixedef_correct_incorrect<-experimental_df %>%
  filter(solution_type != 3) %>% 
  mutate(baseline_RMSSD = as.numeric(baseline_RMSSD),
         HF_HRV = as.numeric(HF_HRV),
         num_reject_IBI = as.numeric(num_reject_IBI))
summary(mixedef_correct_incorrect)

sd(mixedef_correct_incorrect$age)

```
```{r}
#change solution-type to 0 = non-insight and 1 = insight
mixedef_correct<-mixedef_correct%>%
  mutate(solution_type = if_else(solution_type == 2, 0, 1))

#create factor of solution-type
mixedef_correct$solution_type<-factor(mixedef_correct$solution_type, levels = c(0, 1), labels = c("Non-Insight", "Insight"))
mixedef_correct<-mixedef_correct%>%
  filter(subject != "37669" & subject != "39823" & baseline_RMSSD > 0)
summary(mixedef_correct)

#correct incorrect
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  mutate(solution_type = if_else(solution_type == 2, 0, 1))

#create factor of solution-type
mixedef_correct_incorrect$solution_type<-factor(mixedef_correct_incorrect$solution_type, levels = c(0, 1), labels = c("Non-Insight", "Insight"))

#exclude participant number 37669 only 17% solved correct and only with insight, also bilangual with Turkish as mothertongue
#subject 39823 was by accident included but did not adhere to the instructions, she/he slept to little, sleep<6h
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  filter(subject != "37669" & subject != "39823" & baseline_RMSSD > 0)

#reverse ACC score
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  mutate(ACC_reverse = if_else(ACC == 0, 1, 0))

#create factor of the reverse ACC score
mixedef_correct_incorrect$ACC_reverse<-factor(mixedef_correct_incorrect$ACC_reverse, levels = c(0, 1), labels = c("correct", "Incorrect"))

```


```{r}
#check normality of predictor vaiables
library(car)
library(qualityTools)
#histogram to see how RMSSD is skewed
hist(mixedef_correct_incorrect$baseline_RMSSD,probability=T, main="Histogram of distributed RMSSD
     data",xlab="distributed RMSSD data")
lines(density(mixedef_correct_incorrect$baseline_RMSSD),col=2)
#qqplot RT
qqPlot(mixedef_correct_incorrect$baseline_RMSSD)
#not normal distributed what is to be expected based on the literature, right skewed

#histogram to see how RT is skewed
hist(mixedef_correct_incorrect$RT,probability=T, main="Histogram of distributed RT
     data",xlab="distributed RT data")
lines(density(mixedef_correct_incorrect$RT),col=2)
#qqplot RT
qqPlot(mixedef_correct_incorrect$RT)
#It is right skewed

#histogram to see how confidence is skewed
hist(mixedef_correct_incorrect$confidence,probability=T, main="Histogram of distributed confidence data",
     xlab="distributed confidence data")
lines(density(mixedef_correct_incorrect$confidence),col=2)
#qqplot RT
qqPlot(mixedef_correct_incorrect$confidence)
#It is left skewed
```

```{r}
#Transform HRV measures, to increase progressing to normal distribution, so that model estimation is beter afterwards
#correct incorrect
mixedef_correct_incorrect<-mixedef_correct_incorrect%>%
  mutate(lnRT = log10(RT),
         lnconfidence = 1/(max(confidence+1) - confidence), 
         lnRMSSD = log10(baseline_RMSSD))

#check normality again, with adjusted predictor variables
#histogram to see how RMSSD 
hist(mixedef_correct_incorrect$lnRMSSD,probability=T, main="Histogram of distributed RMSSD
     data",xlab="distributed RMSSD data")
lines(density(mixedef_correct_incorrect$lnRMSSD),col=2)
#qqplot RT
qqPlot(mixedef_correct_incorrect$lnRMSSD)
#not normal distributed what is to be expected based on the literature, right skewed

#histogram to see how RT
hist(mixedef_correct_incorrect$lnRT,probability=T, main="Histogram of distributed RT
     data",xlab="distributed RT data")
lines(density(mixedef_correct_incorrect$RT),col=2)
#qqplot RT
qqPlot(mixedef_correct_incorrect$lnRT)
#It is right skewed

#histogram to see how confidence 
hist(mixedef_correct_incorrect$lnconfidence,probability=T, main="Histogram of distributed confidence data",
     xlab="distributed confidence data")
lines(density(mixedef_correct_incorrect$lnconfidence),col=2)
#qqplot RT
qqPlot(mixedef_correct_incorrect$lnconfidence)
#It is left skewed
```

```{r}
#center variables
#lnRMSSD grand mean centered
mixedef_correct_incorrect$lnRMSSD_Zscore<-scale(mixedef_correct_incorrect$lnRMSSD, center = TRUE, scale = TRUE)

#center RT and confidence at the participant level
#for lnconfidence
mixedef_correct_incorrect<- mixedef_correct_incorrect %>%
  group_by(subject)%>%
  mutate(lnconfidence_mean = mean(lnconfidence),
         lnconfidence_sd = sd(lnconfidence),
         lnconfidence_zscore = (lnconfidence - lnconfidence_mean)/lnconfidence_sd)

#For RT
mixedef_correct_incorrect<- mixedef_correct_incorrect %>%
  group_by(subject)%>%
  mutate(lnRT_mean = mean(lnRT),
         lnRT_sd = sd(lnRT),
         lnRT_zscore = (lnRT - lnRT_mean)/lnRT_sd)
```



```{r}
##############################################
#mixed effect logistic regression
#############################################
model_empty<-glmer(solution_type ~ (1|CRAT1) + (1|subject),
                   data = mixedef_correct_incorrect, 
                   family = binomial("logit"), 
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model_empty)

#Calculate the IntraClass Correlation =ICC
#for word puzzles
ICC_WP <- 0.2349/(0.2349 + 0.6983 + 3.29)
ICC_WP
#this is 6%, small variation between word puzzles

ICC_subject <- 0.6983/(0.2349 + 0.6983 + 3.29)
ICC_subject
#this is 17%, sufficient variations between participants

#We do not test a constrained and augmented intermediate model because we are not going to include random slopes, so it is just a model with random intercepts

#create final model
#model with all variables in it, plus intra level first order interactionterms
model_full_1<-glmer(solution_type ~ 
                      ACC_reverse + 
                      lnRT_zscore +
                      lnconfidence_zscore +
                      lnRMSSD_Zscore +
                      (1|CRAT1) + (1|subject),
                    data = mixedef_correct_incorrect, 
                    family = binomial("logit"), 
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model_full_1)


#We are going to drop the first order (non-sign) intraclass interactionterms for which we don't have hypothesis. To keep a parsimonious model
model_full_3<-glmer(solution_type ~ 
                      ACC_reverse + 
                      lnRT_zscore +
                      lnconfidence_zscore +
                      lnRMSSD_Zscore +
                      lnRMSSD_Zscore*ACC_reverse +
                      lnRMSSD_Zscore*lnRT_zscore +
                      lnRMSSD_Zscore*lnconfidence_zscore + 
                      (1|CRAT1) + (1|subject),
                    data = mixedef_correct_incorrect, 
                    family = binomial("logit"), 
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summary(model_full_3)
summary(mixedef_correct_incorrect)
u<-unique(mixedef_correct_incorrect$age)

#Compare last model to empty model with anova
anova(model_full_3, model_empty)

#model is better than empty model.

#get confidence intervals for last model
exp(fixef(model_full_3))

exp(confint(model_full_3,parm="beta_",method="Wald"))
```


```{r}
##################################
#Model Diagnostics
#################################
#check multicollinearity
library(performance)
#check multicolinearity
check_collinearity(model_full_3)
#ok all less than 4

####################################
#Random intercept diagnostics
####################################
#Plot random effect with dotplot
library(lattice)
dotplot_model3<- dotplot(ranef(model_full_3, condVar = TRUE))
dotplot_model3

#distribution of random intercepts SDs CRAT1
Rand_model3_CRAT<-ranef(model_full_3, condVar = TRUE, whichel = "CRAT1")
Rand_model3_CRAT_df<-as.data.frame(Rand_model3_CRAT)

qplot(Rand_model3_CRAT_df$condval, geom = "histogram") +#standardized values of the slopes per word puzzle
  stat_bin(bins = 10)
#This looks like normal distributed

#distribution of random intercepts SDs subject
Rand_model3_subject<-ranef(model_full_3, condVar = TRUE, whichel = "subject")
Rand_model3_subject_df<-as.data.frame(Rand_model3_subject)

qplot(Rand_model3_subject_df$condval, geom = "histogram") +#standardized values of the slopes per word puzzle
  stat_bin(bins = 10)
#This looks, more or less, like normal distributed

###################################################
#main model diagnostics
####################################################
#residual deviance analysis
library(gridExtra)
plot_model3_1 <- plot(model_full_3,id=0.05,idLabels=~.obs)
plot_model3_2 <- plot(model_full_3,ylim=c(-1.5,1),type=c("p","smooth"))
grid.arrange(plot_model3_1,plot_model3_2,nrow=1)
#Seems some outliers in the model, 

#three step outlier deletion for three cut-offs 1.96/2.58/3
#deviation within 95% boundary = 1.96SD
new_model3_outlier_196<- subset(mixedef_correct_incorrect,
                                abs(resid(model_full_3,"pearson"))<1.96)

refit_model3__196 <- update(model_full_3, data = new_model3_outlier_196)
summary(refit_model3__196)
#Results remain the same, RT becomes sign

#deviation within 99% boundary = 2.58SD
new_model3_outlier_258<- subset(mixedef_correct_incorrect,
                                abs(resid(model_full_3,"pearson"))<2.58)

refit_model3_258 <- update(model_full_3, data = new_model3_outlier_258)
summary(refit_model3_258)
#results remain the same, in general

#deviation within 99.7% boundary = 3SD
new_model3_outlier_300<- subset(mixedef_correct_incorrect,
                                abs(resid(model_full_3,"pearson"))<3)

refit_model3_300 <- update(model_full_3, data = new_model3_outlier_300)
summary(refit_model3_300)
#results remain the same

#by-subject model influence
library(influence.ME)
fit_model3_BySubjectINfluence <-  influence(model_full_3, group="subject")

#Create data frame with cooks distance for each subject
cd_model3=cooks.distance(fit_model3_BySubjectINfluence)
#General rule-of-thumb value above 3*mean cooks distance can be considered influential cases
mean(cd_model3)*3

#Histogram of cook's distance:
qplot(cd_model3) + xlab("Histogram Cook's distance (subjects)")

#which subjects are potential influential cases
cd_model3[order(-cd_model3),,drop = FALSE]

#refit model without influential subject which 38455
fit_model3_MinSubject38455 <- update(model_full_3, . ~ ., data=filter(mixedef_correct_incorrect, subject!=38455))
summary(fit_model3_MinSubject38455)
#RT*RMSSD is changes somewhat to being almost borderline sign, results remain generally the same (seems to be something there for this interactioneffect)

#by-CRAT1 model influence
fit_model3_ByCRAT1INfluence <-  influence(model_full_3, group="CRAT1")

#Create data frame with cooks distance for each CRAT1
cd_model3_CRA=cooks.distance(fit_model3_ByCRAT1INfluence)
#General rule-of-thumb value above 3*mean cooks distance can be considered influential cases
mean(cd_model3_CRA)*3#0.0433

#Histogram of cook's distance:
qplot(cd_model3_CRA) + xlab("Histogram Cook's distance (CRAT)")

#which CRAT are potential influential cases
cd_model3_CRA[order(-cd_model3_CRA),,drop = FALSE]
#CRAT wandeling, regen, rust

#refit model without influential CRAT1
fit_model3_MinCRAT1 <- update(model_full_3, . ~ ., data=filter(mixedef_correct_incorrect,
                                                               CRAT1!= "wandeling" & CRAT1!= "regen" & CRAT1 != "rust"))
summary(fit_model3_MinCRAT1)
#result remains the same

#We maintain the initial model-full_3 because after being more conservative the estimates stay more or less the same. Thus the influence of outliers or influential cases does not alter the models predictions severly
```
```{r}
#visualize interaction effects
#Cross-level interaction between confidence and RMSSD
library(ggeffects)
m<-ggpredict(model_full_3, c("lnconfidence_zscore[all]", "lnRMSSD_Zscore")) 
plot(m, ci = FALSE)+
  labs(
    x = "Confidence", 
    y = "Probability of Insight", 
    title = " ",
    colour = "RMSSD"
  )+
  theme_classic()+
  scale_color_discrete(labels = c("-1 SD", "Mean", "+1 SD"))
```










