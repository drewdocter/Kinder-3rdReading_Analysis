#Final Report Visuals: Kindergarten Assessment 15-16' matched with 3rd Grade
#Reading Scores 18-19'


### - Part 1: Quality Checks and Data Cleaning (Copied from Initial Script) -----------------------------


##Read in Data File
require(readxl)
KA.3rd <- read_excel("KA15-16_3rdGr18-19.xlsx")

##Rename Variables
require(tidyverse)
KA.3rd <- rename(KA.3rd, Hub = `Hub Name`, HubID = `Hub ID`, District = `District Name`, DisID = `District ID`, School = `School Name`,
                 SchID = `School ID`, StudentID = `group(ssid)`, Econ.Dis = `Economically Disadvantaged`, Sp.ED = `Special Education`,
                 ELL = `English Language Learner`, OR.preK = `Oregon Pre-K`, E.Inter = `Early Intervention`, ATL.Inter = `Interpersonal Skills Score`,
                 ATL.SR = `Self-Regulation Score`, ATL.Total = `Approaches to Learning Total Score`, Nums.Opps = `Numbers & Operation Score`,
                 Eng.Names = `English Letter Names Score`, Eng.Sound = `English Letter Sounds Score`, Span.Sound = `Spanish Letter Sounds Score`,
                 Read.3rd = `OAKS 3rd Grade Reading Score`, Read.Lvl.3rd = `OAKS 3rd Grade Reading Performance Level`)

#Reorder and Factor County Variable to Prep for Graphics
KA.3rd$County <- factor(KA.3rd$County,
                        c("Linn", "Benton", "Lincoln"))

#Reorder and Factor District Variable to Prep for Graphics
KA.3rd$District <- factor(KA.3rd$District,
                          c("Central Linn SD", "Greater Albany Public SD", 
                            "Harrisburg SD", "Lebanon Community SD", 
                            "Santiam Canyon SD",
                            "Scio SD", "Sweet Home SD", "Alsea SD", 
                            "Corvallis SD", "Monroe SD", "Philomath SD",
                            "Lincoln County SD"))


##Quality Check 
#1) Evaluate OAKs 3rd Grade Reading Scores
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

#2) Evaluate OAKs 3rd Grade Reading Level
KA.3rd <- KA.3rd %>% mutate(Read.Lvl.3rd=as.factor(Read.Lvl.3rd))
summary(KA.3rd$Read.Lvl.3rd)


##Evaluate KA Scores - ATL, ELA, Math
#1) ATL Total
summary(KA.3rd$ATL.Total)
ggplot(KA.3rd, aes(x=ATL.Total)) + geom_histogram() 

#2) ATL Interpersonal
summary(KA.3rd$ATL.Inter)
ggplot(KA.3rd, aes(x=ATL.Inter)) + geom_histogram() 

#3) ATL Self-Regulation
summary(KA.3rd$ATL.SR)
ggplot(KA.3rd, aes(x=ATL.SR)) + geom_histogram() 

#4) KA English Letter Sound Recognition
summary(KA.3rd$Eng.Sound)
ggplot(KA.3rd, aes(x=Eng.Sound)) + geom_histogram() 

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

#5) KA Letter Name Recognition
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

#7) KA Numbers and Operations
summary(KA.3rd$Nums.Opps)
ggplot(KA.3rd, aes(x=Nums.Opps)) + geom_histogram(bins = 20, binwidth = .5) 




##Re-Standardize (re-scale) in order to have continuous and categorical 
##variables on a 0-100 scale.
#Rescale ATL.Total to (0-100)
library(scales)
KA.3rd$ATL.Tot.100 <- NA
KA.3rd$ATL.Tot.100 <- scales:::rescale(KA.3rd$ATL.Total, to = c(0, 100)) 
#Manually scale ATL.Total to (0-100)


## - Create Aggregate Kindergarten Language Variable - ##
#Create Cumulative Kindergarten English Language Score (add together scaled 
#Eng.Names and Eng Sounds, then scale to 100)
#Since Sound & Letter scores are strongly correlated, it makes since to 
#aggregate them to avoid diluting affect of other predictors.
#First, rescale Eng Names to (0-26) from (0-52)
KA.3rd$scaledEng.Names <- NA
KA.3rd$scaledEng.Names <- (1/2)*KA.3rd$Eng.Names #Manually scale Eng.Names to (0-26) from (0-52)

#Next, manually scale the cumulative score from 0-100
KA.3rd$KA.Eng.Lang.100 <- NA
KA.3rd$KA.Eng.Lang.100 <- (KA.3rd$Eng.Sound + KA.3rd$scaledEng.Names)*(100/52) 






### - Part 2: Graphic Themes -----------------------------------------------------

##Create General Theme to Copy for All Graphics
ELH.Theme <- theme(plot.title = element_text(family = "Century Gothic", 
                                             face = "bold", size = (16), 
                                             hjust = .5, color = "grey30"),
                   plot.subtitle = element_text(family = "Century Gothic", 
                                             size = (12),  hjust = .5, 
                                             color = "grey30"),
                   legend.title = element_text(color = "grey30",  face = "bold", 
                                               family = "Century Gothic",
                                               size = 13), 
                   legend.text = element_text(face = "bold", color="grey30", 
                                              size = (12), 
                                              family = "Century Gothic"), 
                   legend.background = element_rect(fill = "grey90"),
                   axis.title = element_text(family = "Century Gothic",
                                             face = "bold", size = (14), 
                                             color = "grey30"),
                   axis.text.y = element_text(family = "Century Gothic", 
                                            color = "grey30", size = (12)),
                   axis.text.x = element_text(family = "Century Gothic", 
                                              color = "grey30",
                                              size = (13)),
                   axis.ticks = element_blank(),
                   # panel.border = element_rect(colour = "grey64", fill = NA, 
                   #                             size = 1),
                   axis.line = element_line(size = 1, color = "grey50"),
                   panel.grid.major.y = element_line(color = "grey90"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank())


Flip.Theme <- theme(plot.title = element_text(family = "Century Gothic", 
                                              face = "bold", size = (16), 
                                              hjust = .5, color = "grey30"),
                    legend.position = "right",
                    axis.text.x = element_text(family = "Century Gothic", 
                                               color = "grey30", size = (13)),
                    axis.text.y = element_text(family = "Century Gothic", 
                                               color = "grey30", size = (12)),
                    axis.title = element_text(family = "Century Gothic", 
                                                color = "grey30",
                                                size = (14), face = "bold"),
                    panel.grid.major.y = element_blank(),
                    panel.grid.major.x = element_line(color = "grey90"))


#Custom Color Scale
fill.ELH <- scale_fill_manual(values=c("#2E8C9E","#5DC0A1","#F4BF26","#EF7822",
                                        "#DE3D3D","#262926",
                                        "#3599B8","#DFBFBF",
                                        "#FE9666","#A66999",
                                        "#4AC5BB","#5F6B6D",
                                        "#FB8281","#F4D25A"))

#Custom Color scale (for actual color function)
color.ELH <- scale_color_manual(values=c("#2E8C9E","#5DC0A1","#F4BF26","#EF7822",
                                        "#DE3D3D","#262926",
                                        "#3599B8","#DFBFBF",
                                        "#FE9666","#A66999",
                                        "#4AC5BB","#5F6B6D",
                                        "#FB8281","#F4D25A"))




### - Part 3: Contextual Plots --------------------------------------------------------



### - Part 3.1: Background Plots ---------------------------------------------------


##Plot Contextual Demographic Characteristics of Region##

#Barplot of Gender Distribution in ELH Region#
Bar.Gender <- ggplot(KA.3rd, aes(x=Gender, fill=Gender)) + geom_bar(width = .5) + 
  labs(title= "Number of Students in EL Hub Region, by Gender\n", 
       y="Number of Students\n", 
       x = NULL) +
  geom_text(stat='count', aes(label=..count..), vjust=-.5, size=4.25, 
            family = "Century Gothic") + #sets label to the count
  scale_y_continuous(expand = c(0,0), limits = c(0,1200)) +
  scale_x_discrete(labels = c("Female", "Male")) +
  ELH.Theme +
  theme(legend.position = "none") + #removes legend, not needed
  fill.ELH
Bar.Gender
  
#Barplot of Number of Students by County
Bar.County <- ggplot(KA.3rd, aes(x=County, fill=County)) + geom_bar(width = .5) +
  labs(title= "Number of Students in EL Hub Region, by County\n",
       y="Number of Students\n", x = NULL) +
  geom_text(stat='count', aes(label=..count..), vjust=-.5, size=4.25, 
            family = "Century Gothic") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1500)) +
  fill.ELH + ELH.Theme +
  theme(legend.position = "none")
Bar.County

#Barplot of Number of Students by District (Coordinates Flipped) - Re-Sorted
#Need to create sorted County Variable to match legend.
KA.3rd$County.Srt <- NA
KA.3rd$County.Srt[which(KA.3rd$County == "Linn")] <- 1
KA.3rd$County.Srt[which(KA.3rd$County == "Benton")] <- 2
KA.3rd$County.Srt[which(KA.3rd$County == "Lincoln")] <- 3

Bar.District.Flip.2 <- KA.3rd %>% 
  # arrange(County) %>% #Trying to sort the flipped axis in inverse order
  ggplot(aes(x=reorder(District, -County.Srt), fill=County)) + #reordered District 
  #by custom sorted County Variable to match the order of the legend.
  geom_bar(width = .7) +
  labs(title= "Number of Students in EL Hub Region, by District\n",
       y="\nNumber of Students", x = NULL) +
  geom_text(stat='count', aes(label=..count..), hjust=-.5, size=4.25, 
            family = "Century Gothic") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,650)) +
  fill.ELH + ELH.Theme +
  coord_flip() +
  Flip.Theme
Bar.District.Flip.2

#Barplot of Racial/Ethnic Makeup Distribution (Coordinates Flipped)
Bar.Race.Flip <-ggplot(KA.3rd, aes(x=Ethnicity, fill=Ethnicity)) + geom_bar(width = .5) + 
  labs(title= "Number of Students in EL Hub Region, by Race/Ethnicity\n",
       y="\nNumber of Students", x = NULL) +
  geom_text(stat='count', aes(label=..count..), hjust=-.5, size=4.25, 
            family = "Century Gothic") + 
  scale_x_discrete(labels = c("Asian", "Black", "Hispanic", 
                              "  Native American/\n   Alaska Native",
                              "Multi-Racial", 
                              "  Native Hawaiian/\n   Pacific Islander",
                              "White")) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1700)) +
  coord_flip() +
  fill.ELH + ELH.Theme +
  Flip.Theme +
  theme(legend.position = "none")
Bar.Race.Flip



#Special Education, ELL Status, and Economically Disadvantaged Plots
#Want to just plot proportion of "Yes" responses.
  
#Econ Disadvantaged by District
KA.3rd %>% 
  group_by(District, County, County.Srt) %>% #Added County.Srt into grouping, since
  #we want to inverse arrange the counties to line up legend with flipped bar order.
  count(Econ.Dis) %>% 
  pivot_wider(names_from = Econ.Dis, values_from = n) %>% 
  mutate(total = (N +Y)) %>% 
  mutate(prop.Y = ((Y/total))) %>% 
  ggplot() + 
  geom_bar(aes(x=reorder(District, -County.Srt), y = prop.Y, fill = County), 
           stat = "identity", width = .7) +
  geom_text(aes(District, prop.Y, label = scales::percent(prop.Y, 2)), 
            hjust=-.25, size=4.25, family = "Century Gothic") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,1.05),
                     labels = scales::percent) +
  fill.ELH + ELH.Theme +
  labs(title= "Proportion of All Children Considered Economically Disadvantaged, by District\n",
       y="\nPercentage of All Students Considered Economically Disadvantaged (%)", x = NULL) +
  coord_flip() +
  Flip.Theme

#English Language Learner Proportion by District
KA.3rd %>% 
  group_by(District, County, County.Srt) %>% #Grouped by County.Srt, as well, to align
  #legend order with bar order on flipped graphic
  count(ELL) %>% 
  pivot_wider(names_from = ELL, values_from = n) %>% 
  mutate(total = (N +Y)) %>% 
  mutate(prop.Y = ((Y/total))) %>% 
  ggplot() + 
  geom_bar(aes(x=reorder(District, -County.Srt), y = prop.Y, fill = County), #Reordered District by County.Srt variable
           stat = "identity", width = .7) +
  geom_text(aes(District, prop.Y, label = scales::percent(prop.Y, 2)), 
            hjust=-.25, size=4.25, family = "Century Gothic") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,.3),
                     labels = scales::percent) +
  fill.ELH + ELH.Theme +
  labs(title= "Proportion of All Children Identified as English Language Learners, by District\n",
       y="\nPercentage of All Children Identified as English Language Learners (%)", x = NULL) +
  coord_flip() +
  Flip.Theme

#Disability Status by District
KA.3rd %>% 
  group_by(District, County, County.Srt) %>% 
  count(Sp.ED) %>% 
  pivot_wider(names_from = Sp.ED, values_from = n) %>% 
  mutate(total = (N +Y)) %>% 
  mutate(prop.Y = ((Y/total))) %>% 
  ggplot() + 
  geom_bar(aes(x=reorder(District, -County.Srt), y = prop.Y, fill = County), #Reordered County by County.Srt variable
           stat = "identity", width = .7) +
  geom_text(aes(District, prop.Y, label = scales::percent(prop.Y, 2)), 
            hjust=-.25, size=4.25, family = "Century Gothic") + 
  scale_y_continuous(expand = c(0,0), limits = c(0,.3),
                     labels = scales::percent) +
  fill.ELH + ELH.Theme +
  labs(title= "Proportion of All Students With Disabilities (SWD), by District\n",
       y="\nPercentage of All Students With Disabilities (%)", x = NULL) +
  coord_flip() +
  Flip.Theme


### - Part 3.2: Key Variable Plots & Statistical Tests --------------------------------------------

## - Plot Key Variables of Interest for this Report - ##

#1) 3rd Grade Reading

#Boxplot of 3rd Grade Reading Scores (Outcome of Interest), by County
Box.3rdRead <- ggplot(KA.3rd) +
  geom_boxplot(aes(x=County, y=Read.3rd, fill=County)) +
  labs(title= "3rd Grade Reading Scores in EL Hub Region, by County\n",
       y="3rd Grade Reading Scores\n", x = NULL) +
  # geom_text(stat = "identity", aes(label=Read.3rd), vjust=-.5, size=4.25, 
  #           family = "Century Gothic") + 
  # scale_y_continuous(expand = c(0,0), limits = c(0,1500)) +
  fill.ELH + ELH.Theme +
  theme(legend.position = "none")
Box.3rdRead

#Run ANOVA to determine potential difference in means of 3rd Reading
#scores, by county.
library(stats)
anova.Rd.County <- aov(Read.3rd ~ County, data = KA.3rd)
summary(anova.Rd.County) #Significant different b/ween county scores

#Determine which Counties there is a significant difference in scores between
TukeyHSD(anova.Rd.County) #Diff between all counties


#Run ANOVA to determine potential difference in means of 3rd Reading
#scores, by District.
anova.Rd.Dist <- aov(Read.3rd ~ District, data = KA.3rd)
summary(anova.Rd.Dist) #Significant different b/ween District scores

#Determine which Counties there is a significant difference in scores between
TukeyHSD(anova.Rd.Dist) #Diff between only some of the districts - Lincoln 
#County and a couple of other relationships

#Boxplot of 3rd Grade Reading Scores, by District - Coordinates Flipped
Box.3rdRead.Dist.Flip <- ggplot(KA.3rd) +
  geom_boxplot(aes(x=reorder(District, -County.Srt), y=Read.3rd, fill=County)) + #Sorted District by County.Srt
  #to align legend order with boxplot (flipped) order
  labs(title= "3rd Grade Reading Scores in EL Hub Region, by District\n",
       y="\n3rd Grade Reading Scores", x = NULL) +
  fill.ELH + ELH.Theme +
  coord_flip() + #Flipped the coordinates to try a different mode of comparison
  Flip.Theme #Apply this theme when I want to invert boxplot coordinates.
Box.3rdRead.Dist.Flip



#2) Kindergarten Approaches to Learning

#Boxplot of Kindergarten Approaches to Learning Scores (Key Predictor), by County
Box.ATL <- ggplot(KA.3rd) +
  geom_boxplot(aes(x=County, y=ATL.Total, fill=County)) +
  labs(title= "Approaches to Learning Total Scores in EL Hub Region, by County\n",
       y="Approaches to Learning Score (1-5)\n", x = NULL) +
  # geom_text(stat = "identity", aes(label=Read.3rd), vjust=-.5, size=4.25, 
  #           family = "Century Gothic") + 
  # scale_y_continuous(expand = c(0,0), limits = c(0,1500)) +
  fill.ELH + ELH.Theme +
  theme(legend.position = "none")
Box.ATL

#Run ANOVA to determine potential difference in means of ATL Total
#scores, by county.
library(stats)
anova.ATL.County <- aov(ATL.Total ~ County, data = KA.3rd)
summary(anova.ATL.County) #Significant different b/ween county scores

#Determine which Counties there is a significant difference in scores between
TukeyHSD(anova.ATL.County) #Diff between all counties


#Run ANOVA to determine potential difference in means of ATL Total
#scores, by District.
anova.ATL.Dist <- aov(ATL.Total ~ District, data = KA.3rd)
summary(anova.ATL.Dist) #Significant different b/ween District scores

#Determine which Counties there is a significant difference in scores between
TukeyHSD(anova.ATL.Dist) #Diff between only some of the districts 


#Boxplot of Kindergarten Approaches to Learning Scores, by District - 
#Coordinates Flipped
Box.ATL.Dist.Flip <- ggplot(KA.3rd) +
  geom_boxplot(aes(x=reorder(District, -County.Srt), y=ATL.Total, fill=County)) + #Sorted 
  #District by County.Srt variable to align legend with flipped bars
  labs(title= "Approaches to Learning Total Scores in EL Hub Region, by District\n",
       y="\nApproaches to Learning Score (1-5)", x = NULL) +
  fill.ELH + ELH.Theme +
  coord_flip() + #Flipped the Coordinates to try a different mode of comparison
  Flip.Theme
Box.ATL.Dist.Flip



#3) Kindergarten English Language Composite Score

#Boxplot of Kindergarten English Language Composite Score (Key Predictor), by County
Box.EngLang <- ggplot(KA.3rd) +
  geom_boxplot(aes(x=County, y=KA.Eng.Lang.100, fill=County)) +
  labs(title= "Kindergarten English Language Composite Scores in EL Hub Region, by County\n",
       y="English Language Composite Score (0-100)\n", x = NULL) +
  # geom_text(stat = "identity", aes(label=Read.3rd), vjust=-.5, size=4.25, 
  #           family = "Century Gothic") + 
  # scale_y_continuous(expand = c(0,0), limits = c(0,1500)) +
  fill.ELH + ELH.Theme +
  theme(legend.position = "none")
Box.EngLang

#Run ANOVA to determine potential difference in means of Kinder Eng Lang
#scores, by county.
anova.KEL.County <- aov(KA.Eng.Lang.100 ~ County, data = KA.3rd)
summary(anova.KEL.County) #Significant different b/ween county scores

#Determine which Counties there is a significant difference in scores between
TukeyHSD(anova.KEL.County) #Diff between Benton and Linn, and Lincoln and Benton


#Run ANOVA to determine potential difference in means of Kinder Eng Lang
#scores, by District.
anova.KEL.Dist <- aov(KA.Eng.Lang.100 ~ District, data = KA.3rd)
summary(anova.KEL.Dist) #Significant different b/ween District scores

#Determine which Counties there is a significant difference in scores between
TukeyHSD(anova.KEL.Dist) #Diff between only some of the districts 

#Boxplot of Kindergarten English Language Aggregate Score, by District
  #Coordinates Flipped
Box.EngLang.Dist.Flip <- ggplot(KA.3rd) +
  geom_boxplot(aes(x=reorder(District, -County.Srt), y=KA.Eng.Lang.100, fill=County)) + #Sorted
  #District by -County.Srt to align legend with flipped bars
  labs(title= "Kindergarten English Language Composite Scores in EL Hub Region, by District\n",
       y="\nEnglish Language Composite Score (0-100)", x = NULL) +
  fill.ELH + ELH.Theme +
  coord_flip() +
  Flip.Theme
Box.EngLang.Dist.Flip




### - Part 4: Key Variables vs. 3rd Grade Reading -------------------------------


### - Part 4.1: KA ATL vs. 3rd Grade Reading & Statistical Tests -----------------------------

#ATL vs. 3rd Grade Reading#
ATL.3rdRd.Plot <- ggplot(KA.3rd, 
       aes(x=ATL.Total, 
           y=Read.3rd, alpha = 2/5)) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, color = "#2E8C9E") + #Changed color to fit EL Hub Theme
  labs(title="Approaches to Learning Total Scores and 3rd Grade Reading Scores\n", 
         x ="\nApproaches to Learning Score (1-5)", y = "3rd Grade Reading Score\n") +
  ylim(2150, 2650) +
  ELH.Theme + 
  theme(panel.grid.major.x = element_line(color = "grey90")) +
  scale_alpha(guide = 'none') #This removes the guide for alpha.
ATL.3rdRd.Plot

#Evaluating District-Level Effects on Third Grade Reading Scores
#Basic Plot of 3rd Grade Reading vs. ATL, by District
ATL.3rdRd.District <- ggplot(KA.3rd, aes(x=ATL.Total, y=Read.3rd, 
                                         color = factor(District))) + 
  geom_point(color = "grey") +
  stat_smooth(method="lm", se=FALSE) +
  labs(title = "Kindergarten Approaches to Learning and 3rd Grade Reading Scores\n",
       x="\nApproaches to Learning Score (1-5)",
       y="3rd Grade Reading Score\n", color = "District") +
  ylim(2150, 2650) +
  ELH.Theme + color.ELH +
  theme(panel.grid.major.x = element_line(color = "grey90"))
ATL.3rdRd.District

#Facet Plot of ATL and 3rd Grade Reading, showing District Data Points
ATL.3rdRd.facet <- ggplot(KA.3rd, aes(x=ATL.Total, y=Read.3rd, group = District,
                                         color = factor(District))) + 
  geom_point(color = "grey") +
  stat_smooth(method="lm", se=FALSE) +
  ELH.Theme+
  color.ELH +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(color = "grey90"),
        strip.text.x = element_text(size = 12,
              face = "bold", family = "Century Gothic",
              color = "grey30"),
        strip.text.y = element_text(size = 12,
              face = "bold", family = "Century Gothic",
              color = "grey30")) +
  labs(title = "Kindergarten Approaches to Learning and 3rd Grade Reading Scores\n",
       x="\nApproaches to Learning Score (1-5)",
       y="3rd Grade Reading Score\n") +
  ylim(2150, 2650) +
  facet_wrap(~District)
ATL.3rdRd.facet


### - Part 4.2: KA Eng Lang vs 3rd Grade Reading & Statistical Tests -------------------------

#KA English Language Aggregate Score vs. 3rd Grade Reading#
EngLang.3rdRd.Plot <- ggplot(KA.3rd, 
                             aes(x=KA.Eng.Lang.100, 
                                 y=Read.3rd, alpha = 2/5)) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, color = "#2E8C9E") + #Changed color to fit EL Hub Theme
  labs(title="Kindergarten English Language Composite Scores and 3rd Grade Reading Scores\n", 
       x ="\nEnglish Language Composite Scores (0-100)", 
       y = "3rd Grade Reading Score\n") +
  ylim(2150, 2650) +
  ELH.Theme + 
  theme(panel.grid.major.x = element_line(color = "grey90")) +
  scale_alpha(guide = 'none') #This removes the guide for alpha.
EngLang.3rdRd.Plot


#Evaluating District-Level Effects on Third Grade Reading Scores
#Basic Plot of English Language Composite Scores vs. ATL, by District
EngLang.3rdRd.District <- ggplot(KA.3rd, aes(x=KA.Eng.Lang.100, y=Read.3rd, 
                                         color = factor(District))) + 
  geom_point(color = "grey") +
  stat_smooth(method="lm", se=FALSE) +
  labs(title = "Kindergarten English Language Composite Scores and 3rd Grade Reading Scores\n",
       x="\nEnglish Language Composite Score (0-100)",
       y="3rd Grade Reading Score\n", color = "District") +
  ylim(2150, 2650) +
  ELH.Theme + color.ELH +
  theme(panel.grid.major.x = element_line(color = "grey90"))
EngLang.3rdRd.District

#Facet Plot of English Language Composite Scores and 3rd Gr Reading, showing District Data Points
EngLang.3rdRd.facet <- ggplot(KA.3rd, aes(x=KA.Eng.Lang.100, y=Read.3rd, group = District,
                                      color = factor(District))) + 
  geom_point(color = "grey") +
  stat_smooth(method="lm", se=FALSE) +
  ELH.Theme+
  color.ELH +
  theme(legend.position = "none",
        panel.grid.major.x = element_line(color = "grey90"),
        strip.text.x = element_text(size = 12,
                                    face = "bold", family = "Century Gothic",
                                    color = "grey30"),
        strip.text.y = element_text(size = 12,
                                    face = "bold", family = "Century Gothic",
                                    color = "grey30")) +
  labs(title = "Kindergarten English Language Composite Scores and 3rd Grade Reading Scores\n",
       x="\nEnglish Language Composite Score (0-100)",
       y="3rd Grade Reading Score\n") +
  ylim(2150, 2650) +
  facet_wrap(~District)
EngLang.3rdRd.facet









