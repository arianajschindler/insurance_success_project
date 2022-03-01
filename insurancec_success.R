ls()
rm(list = ls())

library(tidyverse)
library(dplyr)
library(ggplot2)

hospital <- read.csv("/Users/arian/Documents/Brown Fall 2021/PHP1510/final/hospital.csv")

# RESEARCH QUESTION:
# Test whether one of the two insurance companies has been more successful at 
# minimizing the length of hospitalization

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EDA: SUMMARY STATS AND VARIABLE TRANSFORMATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# checking variable types
str(hospital)

# basic summary statistics
summary(hospital)

# creating new variable of age factor groups
age.grp <- hospital%>%
  mutate(age = 
           ifelse(age == 2, "2-5", 
           ifelse(age == 3, "2-5", 
           ifelse(age == 4, "2-5", 
           ifelse(age == 5, "2-5",
           ifelse(age == 6, "6-9", 
           ifelse(age == 7, "6-9", 
           ifelse(age == 8, "6-9", 
           ifelse(age == 9, "6-9",
           ifelse(age == 10, "10-13", 
           ifelse(age == 11, "10-13", 
           ifelse(age == 12, "10-13", 
           ifelse(age == 13, "10-13",
           ifelse(age == 14, "14-17", 
           ifelse(age == 15, "14-17", 
           ifelse(age == 16, "14-17", 
           ifelse(age == 17, "14-17", NA)))))))))))))))))

# changing mutated age group variable to factor
hospital$age.grp <- as.factor(age.grp$age)

# changing the age variable to factor in hospital df
hospital$age.f <- as.factor(hospital$age)

# changing the hospital id variable to factor in hospital df
hospital$hosp.id.f <- as.factor(hospital$hosp.id)

# changing the los variable to factor in hospital df
hospital$los.f <- as.factor(hospital$los)

str(hospital)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EDA: VISUALIZING VARIABLES I WANT TO EXPLORE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# might impact los: hosp.id, complic, type, company
# might be impacted by company: los, hosp.id, type, complic

# color libraries
library(RColorBrewer)
coul <- brewer.pal(9, "Set3") 
set2 <- brewer.pal(8, "Set2")

library(grDevices)
rain <- grDevices::terrain.colors(9)

library(colorspace)
color <- colorspace::rainbow_hcl(9)

# bar graph to see the distribution of length of stay
ggplot(data = hospital) +
  geom_bar(aes(x = los.f), stat = "count", fill=color) +
  labs(title = "Distribution of Length of Stay Observations") +
  theme( plot.title = element_text(size = 14))

# company and length of sta
ggplot(hospital, aes(x=company, fill=los.f)) + geom_bar() +
  labs(title = "Count of Observations for each LOS by Insurer") +
  theme(plot.title = element_text(size = 14))

# table for proportions of type
type_prop <- data.frame(prop.table(table(hospital$type)))
type_prop

# visualizing the proportions of type
barplot(height=type_prop$Freq, names=type_prop$Var1, col=set2, xlab="Hospital Type", ylab="Proportion", main="Proportion of Hospital Type")

# table for proportions of company
comp_prop <- data.frame(prop.table(table(hospital$company)))
comp_prop

# visualizing the proportions of company
all_bar <- barplot(height=comp_prop$Freq, names=comp_prop$Var1, col=set2, xlab="Company", ylab="Proportion", main="All Hospitals: Proportion of Company")
text(all_bar, comp_prop$Freq, paste(comp_prop$Freq, sep=""), pos=1, cex=1)

ggplot(data = hospital) +
  geom_bar(aes(x = los.f), stat = "count", fill=color) +
  labs(title = "Distribution of Length of Stay Observations") +
  theme( plot.title = element_text(size = 14))

ggplot(type_prop, aes(x=Var1, y=Freq)) + geom_col() + 
  scale_fill_brewer(palette = "Set2")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### EXPLORING COMPANY'S EFFECT ###

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMPANY AND LOS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# company and los
t.test(hospital$los~hospital$company, data=hospital, var.equal=F)

# proportions of company and los
company_los <- hospital%>%
  group_by(los, company)%>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))

company_los

# changing the los variable to factor in hosp_pro df
company_los$los.f <- as.factor(company_los$los)
boxplot(los~company, data=company_los, col = color, xlab="Insurance Company", ylab="Length of Stay in Days", main="All Hospitals: LOS for Each Insurer")

# proportion bar chart ALL HOSPITALS
ggplot(company_los, aes(fill=company, y=freq, x=los.f)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Accent") +
  ggtitle("All Hospitals: Proportion of Company per LOS") +
  xlab("Length of Stay") +
  ylab("Proportion")
  theme(plot.title = element_text(size = 8))

# visualizing this
boxplot(los~company, data=hospital, col = color, xlab="Insurance Company", ylab="Length of Stay in Days", main="All Hospitals: LOS for Each Insurer")

# company and length of stay
ggplot(hospital, aes(x=company, fill=los.f)) + geom_bar() +
  labs(title = "Length of Stay for each Insurer") +
  theme(plot.title = element_text(size = 10))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMPANY AND COMPLIC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# chi-squared for complications and company 
complic_chi <- table(hospital$complic, hospital$company)
chisq.test(complic_chi, correct=F) # NOT SIG

t.test(hospital$los~hospital$complic, data=hospital, var.equal=F)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMPANY AND TYPE 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# proportion significance for type
table(hospital$type)%>%
  prop.test()

# chi-squared for type and company 
type_comp_chi <- table(hospital$type, hospital$company)
chisq.test(type_comp_chi, correct=F)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# COMPANY AND HOSP.ID
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# chi-squared for hospital id and company 
id_chi <- table(hospital$hosp.id.f, hospital$company)
chisq.test(id_chi, correct=F) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# COMPANY AND LOS: NEW DF FOR EACH TYPE OF HOSPITAL

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PRIVATE HOSPITALS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hosp_priv <- hospital%>%
  filter(hospital$type == "private")

summary(hosp_priv)

# hypothesis: company and los for private hospital
t.test(los~company, data=hosp_priv, var.equal=F)

# visualizing this
boxplot(los~company, data=hosp_priv)

# proportion significance of company in private hospitals
table(hosp_priv$company)%>%
  prop.test() 

# proportion of company
hosp_priv_prop <- hosp_priv%>%
  group_by(los, company)%>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))

hosp_priv_prop

# trying to drop single company hospitals
priv2 <- hosp_priv[!(hosp_priv$hosp.id == 3 | hosp_priv$hosp.id == 8 | hosp_priv$hosp.id == 19 | hosp_priv$hosp.id == 22),]
priv2

# company and los for private hospital DROPPED SINGLE
t.test(los~company, data=priv2, var.equal=F)

boxplot(los~company, data=hosp_priv, col = color, xlab="Insurance Company", ylab="Length of Stay in Days", main="Private Hospitals: LOS for Each Insurer")

boxplot(los~company, data=priv2, col = coul, xlab="Insurance Company", ylab="Length of Stay in Days", main="Filtered Private Hospitals: LOS for Each Insurer")

priv_prop <- data.frame(prop.table(table(hosp_priv$company)))
priv_prop 

# visualizing the proportions of company
priv_bar <- barplot(height=priv_prop$Freq, names=priv_prop$Var1, col=set2, xlab="Company", ylab="Proportion", main="Private Hospitals: Proportion of Company")
text(priv_bar, priv_prop$Freq, paste(priv_prop$Freq, sep=""), pos=1, cex=1)

pri <- hosp_priv%>%
  group_by(hosp.id.f)

length(unique(hosp_priv$hosp.id))

hosp_priv_prop$los.f <- as.factor(hosp_priv_prop$los)
# proportion bar chart PRIVATE HOSPITALS
ggplot(hosp_priv_prop, aes(fill=company, y=freq, x=los.f)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Accent") +
  ggtitle("Private Hospitals: Proportion of Company per LOS") +
  xlab("Length of Stay") +
  ylab("Proportion") +
  theme(plot.title = element_text(size = 12))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PUBLIC HOSPITALS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hosp_pub <- hospital%>%
  filter(hospital$type == "public")

# hypothesis: company and los for public hospital
t.test(los~company, data=hosp_pub, var.equal=F)

# visualizing this
boxplot(los~company, data=hosp_pub)

# proportion significance of company in public hospitals
table(hosp_pub$company)%>%
  prop.test() 

# proportion of company
hosp_pub_prop <- hosp_pub%>%
  group_by(los, company)%>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))

hosp_pub_prop

# trying to drop single company hospitals
pub2 <- hosp_pub[!(hosp_pub$hosp.id == 42 | hosp_pub$hosp.id == 54),]
pub2

# company and los for public hospital DROPPED SINGLE
t.test(los~company, data=pub2, var.equal=F)

# all single company hospitals out
hosp_doub <- hospital[!(hospital$hosp.id == 3 | hospital$hosp.id == 8 | hospital$hosp.id == 19 | hospital$hosp.id == 22 | hospital$hosp.id == 42 | hospital$hosp.id == 54),]
hosp_doub
t.test(los~company, data=hosp_doub, var.equal=F)

# public boxplots
boxplot(los~company, data=hosp_pub, col = color, xlab="Insurance Company", ylab="Length of Stay in Days", main="Public Hospitals: LOS for Each Insurer")

boxplot(los~company, data=pub2, col = coul, xlab="Insurance Company", ylab="Length of Stay in Days", main="Filtered Public Hospitals: LOS for Each Insurer")

# all hospitals filtered
boxplot(los~company, data=hosp_doub, col = coul, xlab="Insurance Company", ylab="Length of Stay in Days", main="Filtered Hospitals: LOS for Each Insurer")

pub_prop <- data.frame(prop.table(table(hosp_pub$company)))
pub_prop # what does this data look like?

# visualizing the proportions of company
pub_bar <- barplot(height=pub_prop$Freq, names=pub_prop$Var1, col=set2, xlab="Company", ylab="Proportion", main="Public Hospitals: Proportion of Company")
text(pub_bar, pub_prop$Freq, paste(pub_prop$Freq, sep=""), pos=1, cex=1)

hosp_pub_prop$los.f <- as.factor(hosp_pub_prop$los)
# proportion bar chart PRIVATE HOSPITALS
ggplot(hosp_pub_prop, aes(fill=company, y=freq, x=los.f)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Accent") +
  ggtitle("Public Hospitals: Proportion of Company per LOS") +
  xlab("Length of Stay") +
  ylab("Proportion") +
  theme(plot.title = element_text(size = 12))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# chi-squared for hospital location and company
id_comp_chi <- table(hospital$hosp.id.f, hospital$company)
chisq.test(id_comp_chi, correct=F)

# proportion hosp.id.f and company
hosp_id_comp <- hospital%>%
  group_by(hosp.id, company)%>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))

hosp_id_comp

# changing the hospital id variable to factor in hospital_id_comp df
hosp_id_comp$hosp.id.f <- as.factor(hosp_id_comp$hosp.id)

# visualizing hosp_id_comp
ggplot(hosp_id_comp, aes(fill=company, y=freq, x=hosp.id.f)) + geom_bar(position="stack", stat="identity")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# los and hosp.id.f
# more than one group being tested so we need anova
id_anova <- aov(formula=los~hosp.id.f, data = hospital)
summary(id_anova) # anova says VERY significant
#TukeyHSD(id_anova, conf.level = 0.95) # this is WAY TOO LONG

# proportion hosp.id.f and los
hosp_id_los <- hospital%>%
  group_by(los, hosp.id)%>%
  summarise(n = n())%>%
  mutate(freq = n/sum(n))

# changing the hospital id variable to factor in hospital_id_los df
hosp_id_los$hosp.id.f <- as.factor(hosp_id_los$hosp.id)

hosp_id_los

# visualizing hosp_id_los
ggplot(hosp_id_los, aes(fill=n, y=los, x=hosp.id.f)) + geom_boxplot(aes(group = cut_width(hosp.id.f, 1)), varwidth=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EDA: Insurer A
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
comp_a <- hospital%>%
  filter(hospital$company == "Insurer A")

summary(comp_a)

table(comp_a$type)%>%
  prop.test()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EDA: Insurer B
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
comp_b <- hospital%>%
  filter(hospital$company == "Insurer B")

summary(comp_b)

table(comp_b$type)%>%
  prop.test()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# T-TESTS FOR NUM VARS AND COMPANY
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# hospital type and length of stay
t.test(hospital$los~hospital$type, data=hospital, var.equal=F) # not sig