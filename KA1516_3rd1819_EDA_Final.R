#Kindergarten Assessment 2015-16 to 3rd Grade 2018-19 Exploratory Analysis
##Exploratory Analysis Script



### - Part 1: Import and Clean Data -----------------------------


#Read Initial Data
require(readxl)
KA.3rd <- read_excel("KA15-16_3rdGr18-19.xlsx")

#Rename Variables
require(tidyverse)
KA.3rd <- rename(KA.3rd, Hub = `Hub Name`, HubID = `Hub ID`, District = `District Name`, DisID = `District ID`, School = `School Name`,
                 SchID = `School ID`, StudentID = `group(ssid)`, Econ.Dis = `Economically Disadvantaged`, Sp.ED = `Special Education`,
                 ELL = `English Language Learner`, OR.preK = `Oregon Pre-K`, E.Inter = `Early Intervention`, ATL.Inter = `Interpersonal Skills Score`,
                 ATL.SR = `Self-Regulation Score`, ATL.Total = `Approaches to Learning Total Score`, Nums.Opps = `Numbers & Operation Score`,
                 Eng.Names = `English Letter Names Score`, Eng.Sound = `English Letter Sounds Score`, Span.Sound = `Spanish Letter Sounds Score`,
                 Read.3rd = `OAKS 3rd Grade Reading Score`, Read.Lvl.3rd = `OAKS 3rd Grade Reading Performance Level`)

#1) Evaluate OAKs 3rd Grade Reading Scores#
ggplot(KA.3rd, aes(x=Read.3rd)) + geom_histogram() 
summary(KA.3rd$Read.3rd)
#Need to remove low reading level values, under 1941. These aren't on the scale.

#Manage Missing Values for OAKS 3rd Grade Reading Level
KA.3rd %>%
  filter(Read.3rd <= 1900) %>%
  select(StudentID, ATL.Total, Eng.Names, Read.Lvl.3rd, Read.3rd)
KA.3rd <- KA.3rd %>%
  mutate(Read.3rd = replace(Read.3rd, Read.3rd < 1900, NA))
ggplot(KA.3rd, aes(x=Read.3rd)) + geom_histogram() 

#2) Evaluate OAKs 3rd Grade Reading Level#
KA.3rd <- KA.3rd %>% mutate(Read.Lvl.3rd=as.factor(Read.Lvl.3rd))
summary(KA.3rd$Read.Lvl.3rd)


##Evaluate KA Scores - ATL, ELA, Math##
#1) ATL Total#
summary(KA.3rd$ATL.Total) #Within range
ggplot(KA.3rd, aes(x=ATL.Total)) + geom_histogram() 

#2) ATL Interpersonal#
summary(KA.3rd$ATL.Inter) #Within range
ggplot(KA.3rd, aes(x=ATL.Inter)) + geom_histogram() 

#3) ATL Self-Regulation#
summary(KA.3rd$ATL.SR) #Within range
ggplot(KA.3rd, aes(x=ATL.SR)) + geom_histogram() 

#4) KA English Letter Sound Recognition
summary(KA.3rd$Eng.Sound) 
ggplot(KA.3rd, aes(x=Eng.Sound)) + geom_histogram() #Outliers, correct this.

#Manage Outlying Values for KA Sound Recognition
KA.3rd %>%
  filter(Eng.Sound <= 0) %>%
  select(StudentID, Eng.Sound, Read.Lvl.3rd, Read.3rd)
#645 scores of 0. Assume correct entry.
KA.3rd %>%
  filter(Eng.Sound > 26) %>%
  select(StudentID, Eng.Sound, Read.Lvl.3rd, Read.3rd)
#120 scores above 26. Re-classify as "NA"
KA.3rd <- KA.3rd %>%
  mutate(Eng.Sound = replace(Eng.Sound, Eng.Sound >  26, NA))
ggplot(KA.3rd, aes(x=Eng.Sound)) + geom_histogram(bins = 20) 

#5) KA Letter Name Recognition#
summary(KA.3rd$Eng.Names)
ggplot(KA.3rd, aes(x=Eng.Names)) + geom_histogram() 

#Manage Outlying Values for KA English Letter Sound Recognition#
KA.3rd %>%
  filter(Eng.Names <= 0) %>%
  select(StudentID, Eng.Names, Read.Lvl.3rd, Read.3rd)
#212 scores of 0. Assume correct entry.
KA.3rd %>%
  filter(Eng.Names > 52) %>%
  select(StudentID, Eng.Names, Read.Lvl.3rd, Read.3rd)
#54 scores above 26. Re-classify as "NA"
KA.3rd <- KA.3rd %>%
  mutate(Eng.Names = replace(Eng.Names, Eng.Names > 52, NA))

ggplot(KA.3rd, aes(x=Eng.Names)) + geom_histogram(bins = 20) 

#6) KA Spanish Letter Sound Recognition
summary(KA.3rd$Span.Sound)
ggplot(KA.3rd, aes(x=Span.Sound)) + geom_histogram(bins = 10, binwidth = 1) 

#7) KA Numbers and Operations#
summary(KA.3rd$Nums.Opps)
ggplot(KA.3rd, aes(x=Nums.Opps)) + geom_histogram(bins = 20, binwidth = .5) 




### - Part 2: Exploratory Data Analysis - Regional Context Graphics --------------------------------

##1) Explore and graph Demographic Characteristics##
##Barplot of Gender Distribution in ELH Region##
ggplot(KA.3rd, aes(x=Gender, fill=Gender)) + geom_bar(width = .5)

##Race/Ethnicity Counts##
ggplot(KA.3rd, aes(x=Ethnicity, fill=Ethnicity)) + geom_bar(width = .7)

##Student Counts - By County##
ggplot(KA.3rd, aes(x=County, fill=County)) + geom_bar(width = .7)

##Econ Disadvantaged##
ggplot(KA.3rd, aes(x=County, fill = Econ.Dis)) + 
  geom_bar(position = "fill") #By County

ggplot(KA.3rd, aes(x=District, fill = Econ.Dis)) + 
  geom_bar(position = "fill") #By District

#Special Education, ELL Status, and Economically Disadvantaged Plots
#Want to just plot proportion of "Yes" responses.

#Econ Dis by County
KA.3rd %>% 
  group_by(County) %>% 
  count(Econ.Dis) %>% 
  pivot_wider(names_from = Econ.Dis, values_from = n) %>% 
  mutate(total = (N +Y)) %>% 
  mutate(prop.Y = ((Y/total)*100)) %>% 
  ggplot() + 
  geom_bar(aes(x=County, y = prop.Y, fill = County), 
           stat = "identity", width = .7) +
  geom_text(aes(County, prop.Y, label = round(prop.Y, digits = 1)), 
            vjust=-.5, size=4.25, family = "Century Gothic") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,100),
                     labels = function(x) paste0(x, "%")) +
  fill.ELH + ELH.Theme +
  theme(legend.position = "none") +
  labs(title= "Proportion of Students Considered Economically Disadvantaged, by County\n",
       y="\nPercentage of Students Economically Disadvantaged (%)", x = NULL)


#Econ Dis by District
KA.3rd %>% 
  group_by(District, County) %>% 
  count(Econ.Dis) %>% 
  pivot_wider(names_from = Econ.Dis, values_from = n) %>% 
  mutate(total = (N +Y)) %>% 
  mutate(prop.Y = ((Y/total)*100)) %>% 
  ggplot() + 
  geom_bar(aes(x=District, y = prop.Y, fill = County), 
           stat = "identity", width = .7) +
  geom_text(aes(District, prop.Y, label = round(prop.Y, digits = 1)), 
            hjust=-.25, size=4.25, family = "Century Gothic") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,105),
                     labels = function(x) paste0(x, "%")) +
  fill.ELH + ELH.Theme +
  theme(legend.position = "none") +
  labs(title= "Proportion of Students Considered Economically Disadvantaged, by District\n",
       y="\nPercentage of Students Economically Disadvantaged (%)", x = NULL) +
  coord_flip()



# Prop.EconDis <- KA.3rd %>% 
#   group_by(District, County) %>% 
#   count(Econ.Dis) %>% 
#   pivot_wider(names_from = Econ.Dis, values_from = n) %>% 
#   mutate(total = (N +Y)) %>% 
#   mutate(prop.Y = ((Y/total)*100)) %>% 
#   select(District, prop.Y)
# Prop.EconDis


# left_join(Prop.EconDis, KA.3rd, by = "District") %>% 
#   # select(District, County, prop.Y) %>% 
#   # summarise(mean = mean(prop.Y)) %>% 
#   ggplot() +
#   geom_bar(aes(x=District, y = prop.Y, fill = County), 
#            stat = "identity", width = .7) +
#   geom_text(aes(District, prop.Y, label = round(prop.Y, digits = 1)), 
#             hjust=-.25, size=4.25, family = "Century Gothic") + 
#   scale_y_continuous(expand = c(0,0), limits = c(0,100),
#                      labels = function(x) paste0(x, "%")) +
#   fill.ELH + ELH.Theme +
#   theme(legend.position = "none") +
#   labs(title= "Proportion of Students Considered Economically Disadvantaged, by District\n",
#        y="\nPercentage of Students Economically Disadvantaged (%)", x = NULL) +
#   coord_flip()

##Special Education##
ggplot(KA.3rd, aes(x=County, fill = Sp.ED)) + 
  geom_bar(position = "fill") #By County

ggplot(KA.3rd, aes(x=District, fill = Sp.ED)) + 
  geom_bar(position = "fill") #By District

##English Language Learner##
ggplot(KA.3rd, aes(x=County, fill = ELL)) + 
  geom_bar(position = "fill") + coord_cartesian(
    ylim = c(0, .3), expand = TRUE) 


##2) Explore and Graph 3rd Grade Reading Scores##

#3rd Grade Grade Reading Levels
table(KA.3rd$Read.Lvl.3rd)
ggplot(KA.3rd, aes(x=Read.Lvl.3rd, fill = Read.Lvl.3rd)) + 
  geom_bar() + coord_cartesian(ylim = c(0, 800), expand = TRUE) 

#ATL vs. 3rd Grade Reading#
ggplot(KA.3rd, aes(x=ATL.Total, y=Read.3rd, color = Read.Lvl.3rd)) + 
  geom_point()

#English Letter Sound Recognition and 3rd Grade Reading#
ggplot(KA.3rd, aes(x=Eng.Sound, y=Read.3rd, color = Read.Lvl.3rd)) + 
  geom_point() 

#English Letter Name Recognition and 3rd Grade Reading#
ggplot(KA.3rd, aes(x=Eng.Names, y=Read.3rd, color = Read.Lvl.3rd)) + 
  geom_point() 




### - Part 3: Exploratory Data Analysis - Key Variable Graphics & Modeling ---------------------------
##### - Part 3.1: ATL and 3rd Grade Reading --------------------------------

##1) ATL Scores and 3rd Grade Reading##

#Unscaled Model
m1 <- lm(Read.3rd ~ ATL.Total + Econ.Dis + Sp.ED + ELL, data = KA.3rd)
summary(m1)

#Basic Regression Plots
plot(KA.3rd$ATL.Total, KA.3rd$Read.3rd, abline(m1, col="red", lwd=2))

#Plotting Non-Econ Disadvantaged vs. Econ Disadvantaged
ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total, color=factor(Econ.Dis))) +
  geom_point(color = "black", alpha = 1/5)+stat_smooth(method="lm",se=FALSE) 
#Higher 3rd Grade reading scores vs. ATL Total scores in non Econ-Disadvantaged students.

ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total)) +
  geom_point() + facet_wrap(~Econ.Dis) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for EconDisadvantaged

#Plotting ELL vs Non-ELL
ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total, color=factor(ELL))) +
  geom_point(color = "black", alpha =1/5)+stat_smooth(method="lm",se=FALSE) 
#Substantial difference in means of ATL vs. 3rd Grade reading, by ELL status.

ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total)) +
  geom_point() + facet_wrap(~ELL) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for ELL

#Plotting Sp.Ed vs. non Sp.Ed
ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total, color=factor(Sp.ED))) +
  geom_point()+stat_smooth(method="lm",se=FALSE) #Difference in means of ATL vs. 
#3rd Reading, by Sp.Ed Status.

ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total)) +
  geom_point() + facet_wrap(~Sp.ED) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for Special Education. Large 
#difference in mean ATL scores vs. 3rd Reading.

#Racial Subgroups
ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total, color=factor(Ethnicity))) +
  geom_point(color = "black", fill = 1/5) + stat_smooth(method="lm",se=FALSE) 
#Same plot that was created in Katy's analysis

ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total)) +
  geom_point() + facet_wrap(~Ethnicity) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for Race

#Gender 
ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total, color=factor(Gender))) +
  geom_point(color = "black", fill = 1/5) + stat_smooth(method="lm",se=FALSE) 
#Not much of a difference here

ggplot(KA.3rd, aes(y=Read.3rd, x=ATL.Total)) +
  geom_point() + facet_wrap(~Gender) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for Gender, marginal difference


##### - Part 3.2: KA Letter Sound Recognition and 3rd Grade Reading --------------------------------

##2) KA Letter Sound Recognition and 3rd Grade Reading##

#Unscaled Model
sound.mod1 <- lm(Read.3rd ~ Eng.Sound + Econ.Dis + Sp.ED + ELL, data = KA.3rd)
summary(sound.mod1)


#Basic Regression Plots
plot(KA.3rd$Eng.Sound, KA.3rd$Read.3rd, 
     abline(sound.mod1, col="red", lwd=2))

p2 <- ggplot(KA.3rd, aes(x=Eng.Sound, y=Read.3rd)) +
  geom_point() +
  geom_smooth(method='lm')
p2

#Plotting Non-Econ Disadvantaged vs. Econ Disadvantaged
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound, color=factor(Econ.Dis))) +
  geom_point(color = "black", alpha = 1/5)+stat_smooth(method="lm",se=FALSE) 
#Higher mean 3rd grade reading vs. Eng Sound scores in non-Econ Disadvantaged students.

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound)) +
  geom_point() + facet_wrap(~Econ.Dis) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for EconDisadvantaged

#Plotting ELL vs Non-ELL
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound, color=factor(ELL))) +
  geom_point(color = "black", alpha = 1/5)+stat_smooth(method="lm",se=FALSE) 
#Difference in means, but not much data to go off of for ELL Group.

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound)) +
  geom_point() + facet_wrap(~ELL) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for ELL

#Plotting Sp.Ed vs. non Sp.Ed
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound, color=factor(Sp.ED))) +
  geom_point(color = "black", alpha = 1/5)+stat_smooth(method="lm",se=FALSE) 
#Significant difference in mean Letter Sounds vs. 3rd Reading.

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound)) +
  geom_point() + facet_wrap(~Sp.ED) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for Special Education. 
#Substantial difference in mean Letter Sounds vs. 3rd Reading.

#Racial Subgroups
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound, color=factor(Ethnicity))) +
  geom_point(color = "black", alpha = 1/5)+stat_smooth(method="lm",se=FALSE) 
#Slight difference in means, by race.

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound)) +
  geom_point() + facet_wrap(~Ethnicity) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for Race

#Gender
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound, color=factor(Gender))) +
  geom_point(color = "black", fill = 1/5) + stat_smooth(method="lm",se=FALSE) 
#Slightly more of a difference vs. 3rd.Read ~ ATL

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Sound)) +
  geom_point() + facet_wrap(~Gender) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for Gender, marginal visable difference


##### - Part 3.3: KA Letter Names and 3rd Grade Reading --------------------------------

##3) KA Letter Name Recognition  and 3rd Grade Reading##

#Unscaled Model
name.mod1 <- lm(Read.3rd ~ Eng.Names + Econ.Dis + Sp.ED + ELL, data = KA.3rd)
summary(name.mod1)

#Basic Regression Plots
plot(KA.3rd$Eng.Names, KA.3rd$Read.3rd, abline(name.mod1, col="red", lwd=2))

p2 <- ggplot(KA.3rd, aes(x=Eng.Names, y=Read.3rd)) +
  geom_point() +
  geom_smooth(method='lm')
p2

#Plotting Non-Econ Disadvantaged vs. Econ Disadvantaged
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names, color=factor(Econ.Dis))) +
  geom_point(color = "black", alpha = 1/5)+stat_smooth(method="lm",se=FALSE) 
#Elevated means for 3rd.Read vs Eng.Names for Non-Econ Disadvantaged Students.

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names)) +
  geom_point() + facet_wrap(~Econ.Dis) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for EconDisadvantaged

#Plotting ELL vs Non-ELL
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names, color=factor(ELL))) +
  geom_point(color = "black", alpha = 1/5)+stat_smooth(method="lm",se=FALSE) 
#Elevated means for 3rd grade reading scores vs. English Names for non-ELL students.

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names)) +
  geom_point() + facet_wrap(~ELL) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for ELL

#Plotting Sp.Ed vs. non Sp.Ed
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names, color=factor(Sp.ED))) +
  geom_point(color = "black", alpha = 1/5)+stat_smooth(method="lm",se=FALSE) 
#Substantial difference in Sp.ED vs. non-Sp.ED.

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names)) +
  geom_point() + facet_wrap(~Sp.ED) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for Special Education. 
#Substantial difference in mean Letter Name scores vs. 3rd Reading.

#Racial Subgroups
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names, color=factor(Ethnicity))) +
  geom_point(color = "black", alpha = 1/5)+stat_smooth(method="lm",se=FALSE) 
#Difference in means, by race.

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names)) +
  geom_point() + facet_wrap(~Ethnicity) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for Race. Too few of points 
#for Pacific Islander to make an inference.

#Gender
ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names, color=factor(Gender))) +
  geom_point(color = "black", fill = 1/5) + stat_smooth(method="lm",se=FALSE) 
#Marginal difference

ggplot(KA.3rd, aes(y=Read.3rd, x=Eng.Names)) +
  geom_point() + facet_wrap(~Gender) + 
  stat_smooth(method="lm",se=FALSE) #Facet Wrap for Gender, marginal difference


##### - Part 3.4: Summary of Un-scaled Models --------------------------------

##4) Overall Prediction of 3rd Grade Reading ##
summod1 <- lm(Read.3rd ~ ATL.Total + Eng.Names + Eng.Sound + 
                Econ.Dis + Sp.ED + ELL, data = KA.3rd)
summary(summod1)

## - Compare Models and Predictors of 3rd Grade Reading - ##
summary(m1) #For every additional ATL Point scored, 3rd grade reading scores increased 31.40 points (when adjusted for other variables)
summary(sound.mod1) #For every additional letter sound recognized, 3rd Grade reading scores increased 4.00 points (when adjusted for other variables)
summary(name.mod1) #For every additional letter name recognized, 3rd Grade reading score increases 2.22 points (when adjusted for other variables)
summary(summod1) #For every additional ATL Point scored, 3rd grade reading scores increased 22.34 points (when adjusted for all other covariates in full model)

par(mfrow = c(2,2))
plot(KA.3rd$ATL.Total, KA.3rd$Read.3rd, abline(m1, col="red", lwd=2))
plot(KA.3rd$Eng.Sound, KA.3rd$Read.3rd, abline(sound.mod1, col="red", lwd=2))
plot(KA.3rd$Eng.Names, KA.3rd$Read.3rd, abline(name.mod1, col="red", lwd=2))
plot(KA.3rd$ATL.Total, KA.3rd$Read.3rd, abline(summod1, col="red", lwd=2))





### - Part 4: Modeling and Plotting Scaled Models -----------------------
##### - Part 4.1: 1st Scaled Model (0-26) ---------------------------------


#Standardize (or scale) continuous variables in summary model (Eng.Names, Eng.Sounds, ATL.Total)#
#in order to better interpret this summary model.
summary(summod1)
#install.packages("arm") #Need this package for standardizing coefficients in model
arm::standardize(summod1) #Automatically scales data and reports coefficients in a standardized scale
#Results aren't easily interpretable, try with manual scaling.

#Need to scale ATL.Total to (0-26)
KA.3rd$scaledATL.Tot <- NA
KA.3rd$scaledATL.Tot <- (26/5)*KA.3rd$ATL.Total #Manually scale ATL.Total to (0-26)

#Need to scale Eng Names to (0-26) from (0-52)
KA.3rd$scaledEng.Names <- NA
KA.3rd$scaledEng.Names <- (1/2)*KA.3rd$Eng.Names #Manually scale Eng.Names to (0-26) from (0-52)

summod.scaled <- lm(Read.3rd ~ scaledATL.Tot + scaledEng.Names + Eng.Sound + Econ.Dis + Sp.ED + ELL, data = KA.3rd)
summary(summod.scaled) #Incorporates scaled ATL.Total Variable, and scaled Eng.Name Variable.
#English Names now has more of an effect on 3rd Gr Reading scores on a standardized scale.


## - Overall 3rd Grade Reading Score Prediction Graphics - ##
#1) English Letter Sounds as an Effect Modifier for Predicting 3rd Grade Reading Scores#
summary(summod.scaled) #Revisit model parameters to manually enter coefficients
#formula: Read.3rd ~ Bo + B1(scaledATL.Tot) + B2(Eng.Names) + B3(Eng.Sound) + B4(Econ.Dis) + B5(Sp.ED) + B6(ELL), 
summary(KA.3rd$Eng.Sound) #1st Quartile is 0, 3rd Quartile is 8. Enter these values in appropriate function below.
summary(KA.3rd$scaledEng.Names) #Mean is 8.65. Set parameter to this value in function below. 
summary(KA.3rd$scaledATL.Tot) #Mean is 18.91. Add vertical line to show this.
#Set Econ.Dis, Sp.ED, and ELL to 0 for now.
equation.0sound=function(x){coef(summod.scaled)[1]+coef(summod.scaled)[2]*x+(8.65*coef(summod.scaled)[3])}
equation.8sound=function(x){coef(summod.scaled)[1]+coef(summod.scaled)[2]*x+(8.65*coef(summod.scaled)[3])+(8*coef(summod.scaled)[4])}

soundplot1 <- ggplot(KA.3rd, aes(x=scaledATL.Tot, y=Read.3rd, color = Read.Lvl.3rd)) + geom_point(alpha = 3/5) +
  stat_function(fun=equation.0sound,geom="line", lwd=1.5, color=scales::hue_pal()(2)[1]) +
  stat_function(fun=equation.8sound, geom="line", lwd=1.5, color=scales::hue_pal()(2)[2]) +
  theme_bw() +
  geom_segment(aes(x = 18.91, y=2375, xend = 18.91, yend = 2475), size = 1.5, color = "black", linetype = "dashed") #added the mean of scaled ATL scores

#Minimum Letter Sound Recognition (0) vs. Maximum (26)#
equation.0sound=function(x){coef(summod.scaled)[1]+coef(summod.scaled)[2]*x+(8.65*coef(summod.scaled)[3])}
equation.26sound=function(x){coef(summod.scaled)[1]+coef(summod.scaled)[2]*x+(8.65*coef(summod.scaled)[3])+(26*coef(summod.scaled)[4])} #altered sound coefficient value to 26 (maximum)

soundplot2 <- ggplot(KA.3rd, aes(x=scaledATL.Tot, y=Read.3rd, color = Read.Lvl.3rd)) + geom_point() +
  scale_color_grey(start=.7, end=0.1) + #changed color palette to grey scale to sample, manually setting the range of colors
  stat_function(fun=equation.0sound,geom="line", lwd=1.5, color=scales::hue_pal()(2)[1]) +
  stat_function(fun=equation.26sound, geom="line", lwd=1.5, color=scales::hue_pal()(2)[2]) +
  theme_bw() +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_segment(aes(x = 18.91, y=2375, xend = 18.91, yend = 2500), size = 1.5, color = "navyblue",  linetype = "dashed") + #added the mean of scaled ATL scores in a vertical dashed line(18.91)
  labs(title="English Sound Recognition and 3rd Grade Reading Scores, 15-16' - 18-19'", x ="ATL Total Score - Scaled (0-26)", y = "3rd Grade Reading Score") +
  coord_cartesian(ylim = c(2100, 2700))


#2) English Letter Names as an Effect Modifier for predicting 3rd Grade Reading Scores#
summary(summod.scaled) #Revisit model parameters to manually enter coefficients
#formula: Read.3rd ~ Bo + B1(scaledATL.Tot) + B2(Eng.Names) + B3(Eng.Sound) + B4(Econ.Dis) + B5(Sp.ED) + B6(ELL), 
summary(KA.3rd$scaledEng.Names) #1st Quartile is 1.5, 3rd quartile is 14.5. Set parameter to these values in respective functions below.
summary(KA.3rd$Eng.Sound) #Mean is 5.344. Set this to the mean within equation below.
summary(KA.3rd$scaledATL.Tot) #Mean is 18.91. Added vertical line to show this.
#Set Econ.Dis, Sp.ED, and ELL to 0 for now.
quart1names=function(x){coef(summod.scaled)[1]+coef(summod.scaled)[2]*x+
    (1.5*coef(summod.scaled)[3])+(5.34*coef(summod.scaled)[4])} #function entering first quartile letter name score
quart3names=function(x){coef(summod.scaled)[1]+coef(summod.scaled)[2]*x+
    (14.5*coef(summod.scaled)[3])+(5.34*coef(summod.scaled)[4])} #function entering third quartile letter name score

nameplot1 <- ggplot(KA.3rd, aes(x=scaledATL.Tot, y=Read.3rd, color = Read.Lvl.3rd)) + geom_point(alpha = 3/5) +
  stat_function(fun=quart1names,geom="line", lwd=1.5, color=scales::hue_pal()(2)[1]) +
  stat_function(fun=quart3names, geom="line", lwd=1.5, color=scales::hue_pal()(2)[2]) +
  theme_bw() +
  geom_segment(aes(x = 18.91, y=2375, xend = 18.91, yend = 2475), 
               size = 1.5, color = "black", linetype = "dashed") #added the mean of scaled ATL scores


#Minimum Name Recognition Score (0) vs. Maximum (26)#
minnames=function(x){coef(summod.scaled)[1]+coef(summod.scaled)[2]*x+
    (0*coef(summod.scaled)[3])+(5.34*coef(summod.scaled)[4])} #function entering minimum name recognition score (0)
maxnames=function(x){coef(summod.scaled)[1]+coef(summod.scaled)[2]*x+
    (26*coef(summod.scaled)[3])+(5.34*coef(summod.scaled)[4])} #function entering maximum name recognition score (26)

nameplot2 <- ggplot(KA.3rd, aes(x=scaledATL.Tot, y=Read.3rd, color = Read.Lvl.3rd)) + geom_point() +
  scale_color_grey(start=.7, end=0.1) + #changed color palette to grey scale to sample, manually setting the range of colors
  stat_function(fun=minnames,geom="line", lwd=1.5, color=scales::hue_pal()(2)[1]) +
  stat_function(fun=maxnames, geom="line", lwd=1.5, color=scales::hue_pal()(2)[2]) +
  theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_segment(aes(x = 18.91, y=2375, xend = 18.91, yend = 2500), 
               size = 1.5, color = "navyblue", linetype = "dashed") + #added the mean of scaled ATL scores
  labs(title="English Name Recognition and 3rd Grade Reading Scores, 15-16' - 18-19'", 
       x ="ATL Total Score - Scaled (0-26)", y = "3rd Grade Reading Score") +
  coord_cartesian(ylim = c(2100, 2700))

## Compare English Letter Name vs. English Letter Sound Effect Modification of 3rd Grade Reading ##
#Max Difference
require(gridExtra)
grid.arrange(soundplot2, nameplot2, ncol = 2)  #Arrange side by side for comparison
#Here, we see that English Name Recognition has a slightly bigger influence in 3rd Grade Reading Scores 
#relative to English Letter Sound Recognition when assessing the difference between 0 letter names recognized and 26 letter names recognized. 

#1st and 3rd Quartile Difference
grid.arrange(soundplot1, nameplot1, ncol = 2)


#It doesn't make complete sense to be setting the continuous variables from 0-26
#just to be on the letter scale. Thus, I should adjust continuous variables from 
#a 0-100 scale. Also, since Letter Names and Letter Sounds are correlate, I should 
#aggregate these to form a "KA English Language Aggregate Score".



##### - Part 4.2: Build (0-100) Scaled Models, Aggregating KA Language --------

##Re-Standardize (re-scale) in order to have continuous and categorical variables on a 0-100 scale.
#Rescale ATL.Total to (0-100)
library(scales)
KA.3rd$ATL.Tot.100 <- NA
KA.3rd$ATL.Tot.100 <- scales:::rescale(KA.3rd$ATL.Total, to = c(0, 100)) #Manually scale ATL.Total to (0-100)

## - Create Aggregate Kindergarten Language Variable - ##
#Create Cumulative Kindergarten English Language Score (add together scaled Eng.Names
#and Eng Sounds, then scale to 100). Since Sound & Letter scores are strongly correlated, 
#it makes since to aggregate them to avoid diluting affect of other predictors.
summary(KA.3rd$scaledEng.Names) #Call from previous section
KA.3rd$KA.Eng.Lang.100 <- NA
KA.3rd$KA.Eng.Lang.100 <- (KA.3rd$Eng.Sound + KA.3rd$scaledEng.Names)*(100/52) #Manually scale the cumulative score from 0-100

#Run 0-100 Scaled Model (Only Continuous Variables Scaled) - Same Result
summod.scaledcont.100 <- lm(Read.3rd ~ ATL.Tot.100 + KA.Eng.Lang.100 + Econ.Dis + Sp.ED + ELL + OR.preK, data = KA.3rd)
summary(summod.scaledcont.100) #Same result, not necessary to scale categorical variables as done in model above.

#Compare with Properly Standardized Initial Model
KA.3rd.Standardized <- KA.3rd %>% mutate_if(is.numeric, scale) #Make new dataframe where only continuous variables are standardized
summod.standard <- lm(Read.3rd ~ ATL.Total + Eng.Names + Eng.Sound + Econ.Dis + Sp.ED + ELL, data = KA.3rd.Standardized)
summary(summod.standard) #Summary of Standardized Model, difficult to interpret. Need to incorporate aggregate Kinder Language Variable.

#Compare with Standardized & Scaled Model Using Adjusted Kindergarten Language Variable (KA.Eng.Lang.100)
summod.standard.scaled.100 <- lm(Read.3rd ~ ATL.Tot.100 + KA.Eng.Lang.100 + Econ.Dis + Sp.ED + ELL, data = KA.3rd.Standardized) #Incorporate Scaled ATL & KA Lang Variable
summary(summod.standard.scaled.100) #Again, not entirely interpretable here. Standardized 3rd Reading Makes interpretation difficult.


## Model of Choice: Read.3rd ~ ATL.Tot.100 + KA.Eng.Lang.100 + Econ.Dis + Sp.ED + ELL##
final.mod <- summod.scaledcont.100
summary(final.mod)

## Model of Choice with Interaction Term: Read.3rd ~ ATL.Tot.100 + 
#KA.Eng.Lang.100 + ATL.Tot.100:KA.Eng.Lang.100 + Econ.Dis + Sp.ED + ELL##
final.mod.inter <- lm(Read.3rd ~ ATL.Tot.100 + KA.Eng.Lang.100 + 
                        ATL.Tot.100*KA.Eng.Lang.100 + Econ.Dis + Sp.ED + ELL, data = KA.3rd)
summary(final.mod.inter)


##### - Part 4.3: Develop Draft Graphics for Final Model ---------------------------------

summary(final.mod) #Revisit final model parameters to manually enter coefficients
#formula = Read.3rd ~ ATL.Tot.100 + KA.Eng.Lang.100 + Econ.Dis + Sp.ED + ELL 
summary(KA.3rd$ATL.Tot.100) #Mean is 65.94. Set to "x" for now in function.
summary(KA.3rd$KA.Eng.Lang.100) #Mean is 25.85. Set parameter to this value in function below. 

#Set Econ.Dis, Sp.ED, and ELL to 0 for now.
equat.final.mod.ATL=function(x){coef(final.mod)[1]+coef(final.mod)[2]*x+(25.85*coef(final.mod)[3])}
equat.final.mod.Lang=function(x){coef(final.mod)[1]+(65.94*coef(final.mod)[2])+(coef(final.mod)[3]*x)}

#3rd Reading vs. ATL
final.mod.plot1 <- ggplot(KA.3rd, aes(x=ATL.Tot.100, y=Read.3rd, color = Read.Lvl.3rd)) + geom_point(alpha = 3/5) +
  stat_function(fun=equat.final.mod.ATL, geom ="line", lwd=1.5, color=scales::hue_pal()(2)[1]) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_segment(aes(x = 65.94, y=2375, xend = 65.94, yend = 2475), size = 1.5, color = "black", linetype = "dashed") + #added the mean of scaled ATL scores
  labs(title="ATL Total Scores and 3rd Grade Reading Scores, 2015-16' to 2018-19'", x ="ATL Total Score - Scaled (0-100)", y = "3rd Grade Reading Score")
final.mod.plot1

#Add Error Bars
final.mod.plot1.2 <- ggplot(KA.3rd, aes(x=ATL.Tot.100, y=Read.3rd)) + geom_point(alpha = 3/5) +
  stat_smooth(method="lm",se=TRUE) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_segment(aes(x = 65.94, y=2375, xend = 65.94, yend = 2475), size = 1.5, color = "green", linetype = "dashed") + #added the mean of scaled ATL scores
  labs(title="ATL Total Scores and 3rd Grade Reading Scores, 2015-16' to 2018-19'", x ="ATL Total Score - Scaled (0-100)", y = "3rd Grade Reading Score")
final.mod.plot1.2

#3rd Reading vs. KA Language Aggregate Score
final.mod.plot2 <- ggplot(KA.3rd, aes(x=KA.Eng.Lang.100, y=Read.3rd, color = Read.Lvl.3rd)) + geom_point(alpha = 3/5) +
  stat_function(fun=equat.final.mod.Lang, geom ="line", lwd=1.5, color=scales::hue_pal()(2)[1]) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_segment(aes(x = 25.85, y=2375, xend = 25.85, yend = 2475), size = 1.5, color = "black", linetype = "dashed") + #added the mean of scaled ATL scores
  labs(title="Kindrgarten Language Aggregate Scores and 3rd Grade Reading Scores, 2015-16' to 2018-19'", x ="Kindergarten Language Aggregate Score - Scaled (0-100)", y = "3rd Grade Reading Score")
final.mod.plot2

#Added Error Bars
final.mod.plot2.2 <- ggplot(KA.3rd, aes(x=KA.Eng.Lang.100, y=Read.3rd)) + geom_point(alpha = 3/5) +
  stat_smooth(method="lm",se=TRUE) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_segment(aes(x = 25.85, y=2375, xend = 25.85, yend = 2475), size = 1.5, color = "green", linetype = "dashed") + #added the mean of scaled ATL scores
  labs(title="Kindrgarten Language Aggregate Scores and 3rd Grade Reading Scores, 2015-16' to 2018-19'", x ="Kindergarten Language Aggregate Score - Scaled (0-100)", y = "3rd Grade Reading Score")
final.mod.plot2.2

### - Part 5: Assessing Interaction Between ATL & KA Language  --------
##### Part 5.1: Subsetting by 3rd Reading Scores Deciles ----------------------


#Need to subset KA.3rd to only include top 20% of 3rd Grade Reading Scores, and Bottom 20% of 3rd Grade Reading Scores.
table(KA.3rd$Read.Lvl.3rd) #Read Lvls aren't evenly distributed, sort data into 5 deciles.
KA.3rd <- KA.3rd %>% mutate(quintile = ntile(Read.3rd, 5)) #Added Quintile Variable Into 
KA.3rd %>% filter(quintile == 1) %>% select(StudentID, Read.Lvl.3rd, Read.3rd) #Filtered dataset to ensure that quintiles are accurate. This looks good!
KA.3rd.Quintiles <- KA.3rd %>% filter(quintile == 1 | quintile == 5) #Created subset that includes the top and bottom quintiles.
KA.3rd.Quintiles$quintile <- as.factor(KA.3rd.Quintiles$quintile)

#Simple Plot of Quintiles (3rd Grade Reading Scores vs. ATL)
ggplot(KA.3rd.Quintiles) + 
  geom_point(aes(x=ATL.Tot.100, y=Read.3rd, color = quintile)) +
  theme_minimal()

#Simple Plot of Quintiles (3rd Grade Reading Scores vs. Kinder Lang)
ggplot(KA.3rd.Quintiles) + 
  geom_point(aes(x=KA.Eng.Lang.100, y=Read.3rd, color = quintile)) +
  theme_minimal()

#3rd Reading vs. ATL: Interaction on Quintile Subset
iplot1 <- ggplot(KA.3rd.Quintiles, aes(x=ATL.Tot.100, y=Read.3rd, color = quintile)) + #Moved aesthetic specification to allow stat_smooth entry
  geom_point() +
  theme_minimal() + stat_smooth(method="lm", se=TRUE)
iplot1

#3rd Reading vs. KA.Eng.Lang: Interaction on Quintile Subset
iplot2 <- ggplot(KA.3rd.Quintiles, aes(x=KA.Eng.Lang.100, y=Read.3rd, color = quintile)) + #Moved aesthetic specification to allow stat_smooth entry
  geom_point() +
  theme_minimal() + stat_smooth(method="lm", se=TRUE)

grid.arrange(iplot1, iplot2, nrow=1, ncol=2)


#Comparing with Full Dataset (non-Quintiles)
#3rd Reading vs. ATL: Interaction on Quintile Subset
iplot1.full <- ggplot(KA.3rd, aes(x=ATL.Tot.100, y=Read.3rd)) + #Moved aesthetic specification to allow stat_smooth entry
  geom_point() +
  theme_minimal() + stat_smooth(method="lm", se=TRUE)

#3rd Reading vs. KA.Eng.Lang: Interaction on Quintile Subset
iplot2.full <- ggplot(KA.3rd, aes(x=KA.Eng.Lang.100, y=Read.3rd)) + #Moved aesthetic specification to allow stat_smooth entry
  geom_point() +
  theme_minimal() + stat_smooth(method="lm", se=TRUE)

grid.arrange(iplot1.full, iplot2.full, nrow=1, ncol=2)


#Plotting Basic Interaction Between ATL and English Language Variable
library(sjPlot) #Load package to visualize interaction
library(sjmisc) #Load package to visualize interaction
plot_model(final.mod.inter, type = "pred", terms = c("ATL.Tot.100", "KA.Eng.Lang.100[20,80]")) #Set KA Eng Lang at 1st and 4th Deciles
plot_model(final.mod.inter, type = "pred", terms = c("KA.Eng.Lang.100", "ATL.Tot.100[20,80]")) #Set ATL at 1st and 4th Deciles

#Assess interaction between ELL & ATL
plot_model(final.mod.inter, type = "pred", terms = c("ATL.Tot.100", "ELL"))

#Assess interaction between Sp.ED & ATL
plot_model(final.mod.inter, type = "pred", terms = c("ATL.Tot.100", "Sp.ED"))

#Assess interaction between Econ.Dis & ATL
plot_model(final.mod.inter, type = "pred", terms = c("ATL.Tot.100", "Econ.Dis"))



##### Part 5.2: Subsetting by Kinder English Language and ATL -----------------

#Need to subset KA.Eng Lang & ATL to only include top 20% of values.

##1st subset by KA.Eng.Lang
summary(KA.3rd$KA.Eng.Lang.100) #Evaluate quartile distribution of English Language Variable
KA.3rd <- KA.3rd %>% arrange(KA.Eng.Lang.100) %>% mutate(quintile.KA.Eng.Lang = ntile(KA.Eng.Lang.100, 5)) #Added Quintile Variable to seperate English Language
KA.3rd %>% filter(quintile.KA.Eng.Lang == 1) %>% select(StudentID, quintile.KA.Eng.Lang, KA.Eng.Lang.100) %>% 
  summary(KA.3rd$KA.Eng.Lang.100) #Filtered dataset to ensure that quintiles are accurate. This looks qood based on lowest 20% quintile!
KA.3rd.Quintile.EngLang <- KA.3rd %>% filter(quintile.KA.Eng.Lang == 1 | quintile.KA.Eng.Lang == 5) #Created subset that includes the top and bottom quintiles.
KA.3rd.Quintile.EngLang$quintile <- as.factor(KA.3rd.Quintile.EngLang$quintile.KA.Eng.Lang)

ka.el.plot1 <- ggplot(KA.3rd.Quintile.EngLang, aes(x=ATL.Tot.100, y=Read.3rd, color = quintile)) + #Moved aesthetic specification to allow stat_smooth entry
  geom_point() +
  theme_minimal() + stat_smooth(method="lm", se=TRUE)
ka.el.plot1 #Here, we see a slightly greater slope in the 5th quintile versus the 1st quintile. Though not statistically significant, we see that 
#this means that those who start with a higher KA English Language Aggregate score will, on average, see a greater resulting increase in their 3rd 
#grade reading score.



#Now subset by ATL
summary(KA.3rd$ATL.Tot.100) #Evaluate quartile distribution of ATL Variable
KA.3rd <- KA.3rd %>% arrange(ATL.Tot.100) %>% mutate(quintile.ATL.Tot.100 = ntile(ATL.Tot.100, 5)) #Added quintile argument to seperate ATL by 20% deciles
KA.3rd %>% filter(quintile.ATL.Tot.100 == 1) %>% select(StudentID, quintile.ATL.Tot.100, ATL.Tot.100) %>% 
  summary(KA.3rd$ATL.Tot.100) #Filtered dataset to ensure that quintiles are accurate. This looks qood based on lowest 20% quintile!
KA.3rd.Quintile.ATL <- KA.3rd %>% filter(quintile.ATL.Tot.100 == 1 | quintile.ATL.Tot.100 == 5) #Created subset that includes the top and bottom quintiles.
KA.3rd.Quintile.ATL$quintile <- as.factor(KA.3rd.Quintile.ATL$quintile.ATL.Tot.100) #Ensured new quintile variable is factored

ka.ATL.plot1 <- ggplot(KA.3rd.Quintile.ATL, aes(x=KA.Eng.Lang.100, y=Read.3rd, color = quintile)) + #Moved aesthetic specification to allow stat_smooth entry
  geom_point() +
  theme_minimal() + stat_smooth(method="lm", se=TRUE)
ka.ATL.plot1 #Here, we see a slightly lesser slope in the 5th quintile versus the 1st quintile. Though not statistically significant, we see that 
#this means that those who start with a higher ATL score will, on average, see a smaller resulting increase in their 3rd 
#grade reading scores relative to those who start in the lower quintile. 


grid.arrange(ka.el.plot1, ka.ATL.plot1, nrow=1, ncol=2) #Serve as dessert D&E
#Keep graphic on hold.


#Fitting Model to Trend
# lm(formula = read.3rd[1st and 5th quintile only] ~ atl.tot100 + dummyFor5thQuint+atl.tot100*dummFor5thQuint

#Complete Model With Quintile Seperation
Sub.Mod.1 <- lm(Read.3rd ~ ATL.Tot.100 + KA.Eng.Lang.100 + quintile + ATL.Tot.100*KA.Eng.Lang.100 + 
                  Econ.Dis + Sp.ED + ELL, data = KA.3rd.Quintile.ATL)
summary(Sub.Mod.1)

#Model to fit Subset (Not Including ATL as a predictor)
Sub.Mod.KA.EL <- lm(Read.3rd ~ KA.Eng.Lang.100 + quintile + quintile*KA.Eng.Lang.100 + 
                      Econ.Dis + Sp.ED + ELL, data = KA.3rd.Quintile.ATL)
summary(Sub.Mod.2) #Does this make sense not to include ATL as a predictor, however?

#Model to fit Subset (Not Including KA.Eng.Lang as a predictor)
Sub.Mod.ATL <- lm(Read.3rd ~ ATL.Tot.100 + quintile + quintile*ATL.Tot.100 + 
                    Econ.Dis + Sp.ED + ELL, data = KA.3rd.Quintile.EngLang)
summary(Sub.Mod.ATL)





### - Part 6: Draft Graphics & Mixed Modeling Using Scaled Variables ------------------
##### - Part 6.1: Plotting District-Level Effects on 3rd Gr Reading -----------------------

#Evaluating District-Level Effects on Third Grade Reading Scores

#Basic Plot of 3rd Grade Reading vs. ATL, by District
district.ATL <- ggplot(KA.3rd, aes(x=ATL.Tot.100, y=Read.3rd, color = factor(District))) + 
  geom_point(alpha = 3/5, color = "grey") +
  stat_smooth(method="lm", se=FALSE) +
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          axis.line = element_line(size = 1, color = "grey50")) +
  labs(title = "Kindergarten Approaches to Learning and 3rd Grade Reading Scores",
       x="Approaches to Learning Score (Scaled: 0-100)",
       y="3rd Grade Reading Score") +
  ylim(2150, 2650)
district.ATL

#Facet Plot of 3rd Grade Reading vs. ATL, showing District Data Points
district.ATL.facet <- ggplot(KA.3rd, aes(x=ATL.Tot.100, y=Read.3rd, group = District,
                                         color = factor(District))) + 
  geom_point(alpha = 3/5, color = "grey") +
  stat_smooth(method="lm", se=FALSE) +
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          legend.position = "none",
                          axis.line = element_line(size = 1, color = "grey50")) +
  labs(title = "Kindergarten Approaches to Learning and 3rd Grade Reading Scores",
       x="Approaches to Learning Score (Scaled: 0-100)",
       y="3rd Grade Reading Score") +
  ylim(2150, 2650) +
  facet_wrap(~District)
district.ATL.facet

#Basic Plot of 3rd Grade Reading vs. Kindergarten English Language, by District
district.KA.EL <- ggplot(KA.3rd, aes(x=KA.Eng.Lang.100, y=Read.3rd, color = factor(District))) + 
  geom_point(alpha = 3/5, color = "grey") +
  stat_smooth(method="lm", se=FALSE) +
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          axis.line = element_line(size = 1, color = "grey50")) +
  labs(title = "Kindergarten Language Aggregate Score and 3rd Grade Reading Scores",
       x="Kindergarten Language Aggregate Score (Scaled 0-100)",
       y="3rd Grade Reading Score") +
  ylim(2150, 2650)
district.KA.EL #Include this graphic on final report, to explain district level effects on 3rd Reading.

#Facet Plot of 3rd Grade Reading vs. Kinder Eng Lang, showing District Data Points
district.KA.EL.facet <- ggplot(KA.3rd, aes(x=KA.Eng.Lang.100, y=Read.3rd, group = District,
                                           color = factor(District))) + 
  geom_point(alpha = 3/5, color = "grey") +
  stat_smooth(method="lm", se=FALSE) +
  theme_minimal() + theme(panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank(),
                          legend.position = "none",
                          axis.line = element_line(size = 1, color = "grey50")) +
  labs(title = "Kindergarten Language Aggregate Score and 3rd Grade Reading Scores",
       x="Kindergarten Language Aggregate Score (Scaled 0-100)",
       y="3rd Grade Reading Score") +
  ylim(2150, 2650) +
  facet_wrap(~District)
district.KA.EL.facet #Include on Final Report

#Side by Side Factored District Plots
grid.arrange(district.ATL, district.KA.EL, nrow=1, ncol=2) 
#Side by Side District Factor Plot
grid.arrange(district.ATL.facet, district.KA.EL.facet, nrow=1, ncol=2) 


##### - Part 6.2: Mixed Model for 3rd Grade Reading ---------------------------------------

#Justification for running a mixed model: Since we have students clustered within 
#districts, we need to account for the effect that District may have on 3rd Grade 
#reading performance. In this model, we consider observations (student scores) to
#be clustered by district, and must set the district as a random effect.

#Source for guiding through running the mixed model:
#https://m-clark.github.io/mixed-models-with-R/random_intercepts.html#application

require(lme4) #require this package to run mixed models
?glmer #Generalized linear models that applies to binary response variables, we don't 
#use this here.
?lmer #Use this instead, for our linear response variable (3rd Gr Reading Scores)

summary(final.mod) #Need to set up mixed model with district effect

mixed.mod <- lmer(Read.3rd ~ ATL.Tot.100 + KA.Eng.Lang.100 + 
                    Econ.Dis + Sp.ED + ELL + OR.preK + (1|District), data = KA.3rd) 
#Added in OR.preK as a binary predictor, given suggestion from D&E Workgroup.
#Optimizer is automatically set to BOBYQA.By setting to (1|District), we are allowing 
#the intercept to vary by district.
summary(mixed.mod)
confint(mixed.mod) #Since lmer doesn't give P-values, compute CI's for coefficients

# Random effects:
#   Groups   Name        Variance Std.Dev.
# District (Intercept)   53.5    7.314  
# Residual             5188.9   72.034  
# Number of obs: 1798, groups:  District, 12 

#The total variance of the 12 intercepts (for each District) is around 54.
#Thus, it does matter which district you are, but it is not super significant in 
#explaining the overall variance in scores. The differences in reading scores are 
#not heavily reliant on the district.

#Check Intraclass Correlation
(53.5/(53.5 + 5188.89))*100
# 1.02% - District only explains about 1% of total variance in scores. This just means
#that we can't explain all variance in 3rd Grade Reading scores by District Effect.
#Doesn't mean it isn't still an interesting takeaway.

library(sjstats)
performance::icc(mixed.mod)
# # Intraclass Correlation Coefficient
# 
# Adjusted ICC: 0.010 #Very close to hand-computed ICC.
# Conditional ICC: 0.007


#Estimates of District Effects
##Calculate Parameter Estimates for Random Intercepts/Coefficients
coef(mixed.mod)$District

##Random Effects Coefficients
ranef(mixed.mod)$District #To check the random coefficients 

#Fixed Effects Parameter Estimates
fixef(mixed.mod) #What the original model outputs

#Prediction of District Effects
library(merTools)
REsim(mixed.mod) #Again, estimates random effect by district
plotREsim(REsim(mixed.mod)) #Plots interval estimates for random effects.

##Compute Variance Explained by Fixed Effects
#Method 1
#Source: https://stats.stackexchange.com/questions/7240/proportion-of-explained-variance-in-a-mixed-effects-model
library(MuMIn)
r.squaredGLMM(mixed.mod) 
#     R2m       R2c
# 0.2865201 0.2938013
#R2m represents the 'marginal' proportion of variance in 3rd Grade reading scores associated
  #with only the fixed effects in consideration.
#R2c represents the 'conditional' proportion of variance in 3rd grade reading scores
  #associated with the fixed effects plus the random effects.
#R2c only slightly higher (random effects only explain a bit more than the
  #marginal estimate)

#Method 2
#Source: https://stats.stackexchange.com/questions/490362/how-to-obtain-the-variance-of-fixed-effects
library(r2glmm)
r2 <- r2beta(model=mixed.mod,partial=TRUE,method='sgv')
print(r2)
# Effect   Rsq upper.CL lower.CL
# 1           Model 0.287    0.321    0.256
# 3 KA.Eng.Lang.100 0.107    0.135    0.082
# 2     ATL.Tot.100 0.052    0.074    0.034
# 6            ELLY 0.017    0.031    0.007
# 5          Sp.EDY 0.014    0.027    0.005
# 4       Econ.DisY 0.013    0.025    0.004
# 7        OR.preKY 0.005    0.013    0.000

#This result computes the proportion of variance explained by each fixed effect
#within the mixed model

