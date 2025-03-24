######################################
#                                    #
#   Mixed effects modelling in R
#   Amani Stream Inverts
#                                    #
######################################

#############
#libraries
library(tidyverse)
library(emmeans)
library(car)
library(agridat)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(performance)
library(MuMIn)
library(bbmle)
library(aods3)
library(boot)
library(lmerTest)
library(pbkrtest)
library(ggeffects)


## authors: James Vonesh based onGabriela K Hajduk, based on workshop developed by Liam Bailey
## A reanalysis of van Biervliet et al (2009)

##

###---- THE DATA-----###
## load the data and have a look at it
library(readr)
TeaStreams <- read_csv("~/Downloads/UsambaraStreamDataBiotropicaSIMPLE.csv")
View(TeaStreams)            
head(TeaStreams)

#How much data?
length(TeaStreams$Richness)
#168 lines of data - why?
#How does this differ from the last time we looked at this study

## Have a look at the data distribution:
## Let's look at Stone areas - recall the species area relationship -
## larger habitats, have more species. Do we need to have stone size as a covariate?

### RESPONSE VARIABLE - RICHNESS####
hist(TeaStreams$Richness)  
shapiro.test(TeaStreams$Richness)
#SW suggests "not normal"

# seems tuncated - count data? Another thing to hold in mind
LogRich<-log(TeaStreams$Richness+1)
hist(LogRich)
shapiro.test(LogRich)
## Didnt really help. Maybe a case for a generalized linear model 
##- perhaps poisson. Hold that thought, but for now lets focus on the heirarchical structure of the data

###THE COVARIATE - STONE SIZE####
#Log and Scale Stone_Area explanatory variable
hist(TeaStreams$Stone_Area)

# Seems left skewed, maybe log
logStoneArea<-log(TeaStreams$Stone_Area)
hist(logStoneArea)

#Improved - now scale the variable scale() centers the data (the column mean is subtracted from the values in the column) 
# and then scales it (the centered column values are divided by the column’s standard deviation).
Stone_Area1<-scale(logStoneArea)
hist(Stone_Area1)

##For now lets cbind these all into the TeaStreams dataframe
TeaStreams<-cbind(TeaStreams,Stone_Area1)
head(TeaStreams)
TeaStreams<-cbind(TeaStreams,LogRich)
head(TeaStreams)

### ANALYSIS####
### Lets build a basic model - IGNORE STONE AREA FOR NOW (LIKE THE PAPER DOES)
## LETS LOOK AT RICHNESS BY TREATMENT

library(ggplot2)
P<-ggplot(TeaStreams, aes(x = Treatment, y = Richness)) +
  geom_violin(trim=FALSE)
P
P+stat_summary(fun.y=mean,geo="point", shape=23, size=2)
P+geom_boxplot(width=0.1)

P+geom_boxplot(width=0.1)

# Basic model - Effects of Tea Type
basic.lm <- lm(Richness ~ Treatment, data = TeaStreams)
summary(basic.lm)

##Model fit/Assumptions
## Plot the residuals - the red line should be close to being flat, like the dashed grey line
plot(basic.lm, which = 1)  # not perfect, but look alright

## Have a quick look at the  qqplot too - point should ideally fall onto the diagonal dashed line
plot(basic.lm, which = 2)  # doesnt look great
plot(basic.lm, which = 3)  # ok
plot(basic.lm, which = 4)  # only 1 > 0.05

#Hypothesis test
anova(basic.lm)

##HOW DO THESE STATS MATCH UP WITH THE PAPER? WHAT'S THE CONCLUSION/RESULT?
## WHAT IS THE SAMPLE SIZE HERE? IS THAT APPROPRIATE?
##SEEMS LIKE THIS SHOULD BE SIMILAR TO FIG 1 IN PAPER - CHECK

### ANALYSIS BASIC WITH COVARIATE ####
### Lets add stone size as a covariate - Recall "SAR" of ecology - Species area relationship
##(which we will assume as linear for this example)

## recall the dragons example - could a bias in stone size between treatments be an issue here?
## DISCUSS

##LETS LOOK AT STONE SIZE DATA
## Stone size by Treatment
P<-ggplot(TeaStreams, aes(x = Treatment, y = Stone_Area1)) +
  geom_violin(trim=FALSE)
P
P+stat_summary(fun.y=mean,geo="point", shape=23, size=2)
P+geom_boxplot(width=0.1)

P+geom_boxplot(width=0.1)

## Stone size by Stream
P<-ggplot(TeaStreams, aes(x = Stream, y = Stone_Area1)) +
  geom_violin(trim=FALSE)
P
P+stat_summary(fun.y=mean,geo="point", shape=23, size=2)
P+geom_boxplot(width=0.1)

P+geom_boxplot(width=0.1)

## IS THERE A RELATIONSHIP BETWEEN STONE SIZE AND RICHNESS?
## VISUALIZE THAT RELATIONHSIP
(prelim_plot <- ggplot(TeaStreams, aes(x = Stone_Area1, y = Richness)) +
    geom_point() +
    geom_smooth(method = "lm")+
    geom_smooth(span =1, color= "red", se= FALSE)
)

##BASIC ANALYSIS WITH *ONLY* STONE AREA (IGNORING TREATMENTS HERE)
basic.lmSS <- lm(Richness ~ Stone_Area1, data = TeaStreams)
summary(basic.lmSS)

##Model fit/Assumptions
plot(basic.lmSS, which = 1)
plot(basic.lmSS, which = 2)
plot(basic.lmSS, which = 3)
plot(basic.lmSS, which = 4)

#Hypothesis test
anova(basic.lmSS)
# SO YES - SEEMS THERE IS A STRONG POSITIVE EFFECT OF STONE AREA - HOW DOES THIS COMPARE TO THE PAPER?
## WHAT IS THE SAMPLE SIZE HERE? IS THAT APPROPRIATE?

## However, what about observation independence? Are our data independent?
## We collected multiple samples from stones within riffles within streams
## It's perfectly plausible that the data from within each riffle are more similar to each other than the data from different riffles - they are correlated. Pseudoreplication isn't our friend.

## Have a look at the data to see if above is true
boxplot(Richness ~ Stream, data = TeaStreams)  # certainly looks like something is going on here

qplot(log(Stone_Area), Richness, data = TeaStreams, geom=c("point", "smooth"), method="lm", xlab = "Stone Area", ylab="Richness", facets=.~Treatment) +theme_bw()

qplot(log(Stone_Area), Richness, data = TeaStreams, geom=c("point", "smooth"), method="lm", xlab = "Stone Area", ylab="Richness", facets=.~Stream) +theme_bw()

# Add in Stone size across streams
(colour_plot <- ggplot(TeaStreams, aes(x = Stone_Area1, y = Richness, colour = Stream)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

#####

(split_plot <- ggplot(aes(Stone_Area1, Richness), data = TeaStreams) + 
   geom_point() + 
   facet_wrap(~ Stream) + # create a facet for each mountain range
   xlab("Stone size") + 
   ylab("Richness"))

###### MOVING BEYOND THE BASIC MODELS
##----- Modify the model ANOVA BUT NOT YET ACCOUNTING FOR HIERARCHICAL STRUCTURE (ie No random effects yet)-----###
## Same as previous examples

## We want to use all the data, but account for the data coming from different streams
## let's add streams as a fixed effect to our basic.lm

#Here we have the covriate 'main effect' but not the interaction
streams.lm <- lm(Richness ~ Treatment + Stone_Area1, data = TeaStreams)
summary(streams.lm)

#Here we have the covariate with with interaction term as well - usung the "*"
streams.lm2 <- lm(Richness ~ Treatment * Stone_Area1, data = TeaStreams)
summary(streams.lm2)

#Always good to look at model fit diagnostics - like we did above
plot(streams.lm2)

#Hypothesis test - is one model better than the other
anova(streams.lm,streams.lm2)

# So non-mixed effect suggests no interaction, marginal and significant treatment effects
## but this is still commiting pseudoreplication as stone are not independent
## i.e., df is too high, we are exaggerating our statistical power


################################################
###########----- FIRST MIXED MODEL -----########
#################################################

## DISCUSS THE DATA
## How is the data hieracrichally structured? Russian dolls, what is nested within what?
## Given the work you did with Dragons - which do you think are possible random effects?
## DISCUSS

#Lets use the lme4 package to build our first mixed model - RANDOM INTERCEPTS 
library(lme4)

mixed.lmer1 <- lmer(Richness ~ Treatment + Stone_Area1+ (1|Stream/Riffle), data = TeaStreams)
summary(mixed.lmer1)

##PAUSE TO LOOK AT THAT SUMMARY OUTPUT AT BREAK IT DOWN INTO ITS COMPONENTS
## REFER TO DRAGOND EXAMPLE - WORK IN GROUPS TO INTERPRET

## LOOK AT DIAGNOSTICS
plot(mixed.lmer1)
qqnorm(resid(mixed.lmer1))
qqline(resid(mixed.lmer1))
#THOUGHTS?

#EXTRACT THE EFFECTS
emmeans1<-emmeans(mixed.lmer1,specs="Treatment")
emmeans1


##########################################################
#### VISUALIZE THE EFFECTS OF TEA & STONE AREA ON RICHNESS
##########################################################

ggpredict(mixed.lmer1, terms = c("Stone_Area1", "Treatment"), type = "random") %>% 
  plot() +
  labs(x = "Stone Size", y = "Richness", title = "Effect of Stone size and Tea on Macroinverts") + 
  theme_minimal()

##########################################################


### model2 - Remove Stone_Area
mixed.lmer2 <- lmer(Richness ~ Treatment + (1|Stream/Riffle), data = TeaStreams)
summary(mixed.lmer2)

plot(mixed.lmer2)
qqnorm(resid(mixed.lmer2))
qqline(resid(mixed.lmer2))

emmeans2<-emmeans(mixed.lmer2,specs="Treatment")
emmeans2

#Test for significance of Stone_Area with LRT
anova(mixed.lmer2, mixed.lmer1) #This is a test of the significance of Stone_Area
#So this suggests our scales log stone area has an important effect and WE SHOULD KEEP STONE AREA

## Lets remove the Treatment effect now
mixed.lmer3 <- lmer(Richness ~ Stone_Area1+ (1|Stream/Riffle), data = TeaStreams)
summary(mixed.lmer3)

plot(mixed.lmer3)
qqnorm(resid(mixed.lmer3))
qqline(resid(mixed.lmer3))  

#Test parameter of Treatment
anova(mixed.lmer3, mixed.lmer1)
#So this suggests our scales Treatment area has an important effect and we should keep this term

## How does this change when we adjust random effects?

mixed.lmer4 <- lmer(Richness ~ Treatment + Stone_Area1+ (1|Stream), data = TeaStreams)
summary(mixed.lmer4)

emmeans4<-emmeans(mixed.lmer4,specs="Treatment")
emmeans4

mixed.lmer5 <- lmer(Richness ~ Treatment + (1|Stream), data = TeaStreams)
summary(mixed.lmer5)

emmeans5<-emmeans(mixed.lmer5,specs="Treatment")
emmeans5

anova(mixed.lmer4, mixed.lmer5) # essentially asking the same question as above - does stone area matter? but now with onky Stream as random effect
# makes a slight difference in p-values 0.04 vs 0.3

## Should we include the random effects?
#Ltes compare mixed.lmer1 (which includes "Riffle nested in Stream") with mixed.lmer4 were we drop the "Riffle" random level
anova(mixed.lmer1, mixed.lmer4)
#**The model with the nested Riffle layer within Stream is better - so we keep Riffle**
#BUT ALSO THIS IS APPROPRIATE TO ACCOUNT FOR NESTED STRUCTURE OF DATA ON FIRST PRINCIPLES

## What about any random effect?
#here we keep fixed effects but no Random at all
lm6 <- lm(Richness ~ Treatment + Stone_Area1, data = TeaStreams)
summary(lm6)
plot(lm6)

emmeans6<-emmeans(lm6,specs="Treatment")
emmeans6
emmeans1
#Note that treatment means are same - but df, SE, and CI change
#Talk about that

anova(mixed.lmer1,lm6)
#Mixed effects model is much better
#So seems the mixed model is much better - although parameter estimates and significance pretty similar

##So far we have kind of ignored that the data are not normally distributed - to address that we would need to do not just linear mixe model -
# but generlaized linear mixed model - glmm = the "whole enchilada"

#########################################
## So best model so far- MORE CAREFUL LOOK
###########################################

mixed.lmer1 <- lmer(Richness ~ Treatment + Stone_Area1+ (1|Stream/Riffle), data = TeaStreams)
summary(mixed.lmer1)

(fixef(mixed.lmer1) [1])
(fixef(mixed.lmer1) [2])
#EXTRACT THE EFFECTS
emmeans1<-emmeans(mixed.lmer1,specs="Treatment")
emmeans1

#Yup this matches what we pull from emeans

##Using emmeans to look at marginal treatment effects

##Can we check overdispersion
check_overdispersion(mixed.lmer1)
# seems okay

#Other diganostics 
plot(simulateResiduals(mixed.lmer1))
hist(simulateResiduals(mixed.lmer1))
#These seem okay too 

##########################################################################################################
###Plot the relationship between Stone size and richness for each riffle in each stream - random intercepts

(mm_plot <- ggplot(TeaStreams, aes(x = Stone_Area1, y = Richness, colour = Riffle)) +
    facet_wrap(~Stream, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(TeaStreams, pred = predict(mixed.lmer1)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

# End of Linear Mixed Model examoke

##########################################################################
# Modelling using glm b/c Richness could be consider count data
##########################################################################

#So we could stop here perhaps
#But for the sake of the lesson - lets say that number of taxa is a count type data - which it is
#Modeling count data is often not normally distributed nor its errors - so is often modeled using a poisson distribution
#i.e., a glm 

######
##https://entnemdept.ufl.edu/Hahn/generalized-linear-mixed-models.html
######

##Start with Poisson GLM - but here we have ignored the mixed effects we just worked hard to include
glm1 <- glm(Richness ~ Treatment + Stone_Area1, family = "poisson", data = TeaStreams)
summary(glm1)
Anova(glm1)
emmeans(glm1, ~Treatment, type='response') 
check_overdispersion(glm1)

plot(simulateResiduals(glm1))
hist(simulateResiduals(glm1)) 


#FYI Another way to check model fit wrt "over dispersion is to fit the Quasipoisson and look at the estimated dispersion parameter
#if it is not close to 1 could have issues
glm2 <- glm(Richness ~ Treatment + Stone_Area1, family = "quasipoisson", data = TeaStreams)
summary(glm2)
#Note the dispersion parameter in quasifit is close to 1 - so suggestions poisson is good fit



#######################################################################
#Adding the mixed effects avbove back in to this poisson glm - make glmm
#########################################################################
#We can use "glmer" - which is already in lme4 package

glmm1 <- glmer(Richness ~ Treatment+Stone_Area1  + (1 | Stream/Riffle), data = TeaStreams, family = "poisson")
summary(glmm1)
Anova(glmm1)
plot(glmm1)

Anova(glmm1)
emmeans(glmm1, ~Treatment, type='response') 
#compare with effects from best linear mixed model above
emmeans1
#Effect sizes are only slighly different 1.97 diff vs 1.92
#glmm allows for asyn CI - and bit more seperation b/w treatment effect, ie glmm CI dont overlap at all

#check diagnostics
check_overdispersion(glmm1)
plot(simulateResiduals(glmm1))
hist(simulateResiduals(glmm1)) 
#Seems like some conflicting tests wrt over dispersion (compare plot vs check test)
