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
library(pbkrtest)


## authors: James Vonesh based onGabriela K Hajduk, based on workshop developed by Liam Bailey

##

###---- Explore the data -----###

## load the data and have a look at it

library(readr)
TeaStreams <- read_csv("UsambaraStreamDataBiotropicaSIMPLE.csv", 
                       col_types = cols(Stone = col_character(), 
                                        Stone_Area = col_number(), Richness = col_number(), 
                                        Abundance = col_number()))
View(TeaStreams)            

head(TeaStreams)

## Have a look at the data distribution:
## Let's look at taxa richness as a response variable

## It is good practice to standardise your explanatory variables before proceeding so that they have a mean of zero (“centering”) and standard deviation of one (“scaling”). It ensures that the estimated coefficients are all on the same scale, making it easier to compare effect sizes. You can use scale() to do that:

#Log and Scale Stone_Area explanatory variable
logStoneArea<-log(TeaStreams$Stone_Area)
logStoneArea
Stone_Area1<-scale(logStoneArea)
Stone_Area1

hist(TeaStreams$Richness)  # seems tuncated - count data?
LogRich<-log(TeaStreams$Richness+1)
hist(LogRich) ## this is slightly better but still not great as it is count data

## Let's look at abundance as a response variable

hist(TeaStreams$Abundance) # seems tuncated - count data?
LogAbun<-log(TeaStreams$Abundance+1)
hist(LogAbun) ## this is better normality once log transformed

# Basic model

basic.lm <- lm(Richness ~ Treatment, data = TeaStreams)

summary(basic.lm)

## Let's plot the data with ggplot2

library(ggplot2)

P<-ggplot(TeaStreams, aes(x = Treatment, y = Richness)) +
  geom_violin(trim=FALSE)
P
P+stat_summary(fun.y=mean,geo="point", shape=23, size=2)
P+geom_boxplot(width=0.1)

P+geom_boxplot(width=0.1)

### Assumptions?

## Plot the residuals - the red line should be close to being flat, like the dashed grey line
plot(basic.lm, which = 1)  # not perfect, but look alright

## Have a quick look at the  qqplot too - point should ideally fall onto the diagonal dashed line
plot(basic.lm, which = 2)  # doesnt look great
plot(basic.lm, which = 3)  # ok
plot(basic.lm, which = 4)  # only 1 > 0.05

## However, what about observation independence? Are our data independent?
## We collected multiple samples from eight mountain ranges
## It's perfectly plausible that the data from within each mountain range are more similar to each other than the data from different mountain ranges - they are correlated. Pseudoreplication isn't our friend.

## Have a look at the data to see if above is true
boxplot(Richness ~ Stream, data = TeaStreams)  # certainly looks like something is going on here

qplot(log(Stone_Area), Richness, data = TeaStreams, geom=c("point", "smooth"), method="lm", xlab = "Stone Area", ylab="Richness", facets=.~Treatment) +theme_bw()

qplot(log(Stone_Area), Richness, data = TeaStreams, geom=c("point", "smooth"), method="lm", xlab = "Stone Area", ylab="Richness", facets=.~Stream) +theme_bw()


##----- Modify the model -----###

## We want to use all the data, but account for the data coming from different streams

## let's add streams as a fixed effect to our basic.lm

streams.lm <- lm(Richness ~ Treatment + Stone_Area1, data = TeaStreams)
summary(streams.lm)

# Full ANCOVA with interaction

streams.lm2 <- lm(Richness ~ Treatment * Stone_Area1, data = TeaStreams)
summary(streams.lm2)

anova(streams.lm,streams.lm2)

# So non-mixed effect suggests no interaction, marginal and significant treatment effects - but this is commiting pseudoreplication as stone are not independent

##----- First mixed model -----##

library(lme4)

### model1
mixed.lmer1 <- lmer(Richness ~ Treatment + Stone_Area1+ (1|Stream/Riffle), data = TeaStreams)
summary(mixed.lmer1)

plot(mixed.lmer1)
qqnorm(resid(mixed.lmer1))
qqline(resid(mixed.lmer1))

emmeans1<-emmeans(mixed.lmer1,specs="Treatment")
emmeans1

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
#So this suggests our scales log stone area has an important effect and we should keep this term

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
#The model with the nested Riffle layer within Stream is better - so we keep Riffle

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

## So best model so far

mixed.lmer1 <- lmer(Richness ~ Treatment + Stone_Area1+ (1|Stream/Riffle), data = TeaStreams)
summary(mixed.lmer1)

(fixef(mixed.lmer1) [1])
(fixef(mixed.lmer1) [2])

#Yup this matches what we pull from emeans

##Using emmeans to look at marginal treatment effects

##Can we check overdispersion
check_overdispersion(mixed.lmer1)
# seems okay

#Other diganostics 
plot(simulateResiduals(mixed.lmer1))
hist(simulateResiduals(mixed.lmer1))
#These seem okay too 

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
