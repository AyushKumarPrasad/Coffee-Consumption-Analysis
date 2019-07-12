setwd()

getwd()

setwd("/Users/ayushkumar/Desktop/PGBABI/Four")

getwd()

library(openxlsx)

datagiven <- read.xlsx("Coffee.xlsx")

datagiven

View(datagiven)

summary(datagiven)

boxplot(datagiven$No_of_Packet , horizontal = TRUE)

sum(datagiven$No_of_Packet)

range(datagiven$No_of_Packet)

names(which.max(table(datagiven$No_of_Packet)))

sum(datagiven$No_of_Packet == 1)

# No of packets bought is "1" which is almost 105022

sum(datagiven$Price_per_Packet)

range(datagiven$Price_per_Packet)

names(which.max(table(datagiven$Price_per_Packet)))

sum(datagiven$Price_per_Packet == 2 )

# Selling-price per packet of coffee is "2" which is 6,50 DM to 8,50 DM.

sum(datagiven$IDNo)

range(datagiven$IDNo)

names(which.max(table(datagiven$IDNo))) 

sum(datagiven$IDNo == 560)

# The maximum number in ID Column is "560" which is 251 times

ayushdata <- read.xlsx("Ayush.xlsx")

ayushdata

names(which.max(table(ayushdata$Engine)))

sum(datagiven$Brand)

range(datagiven$Brand )

names(which.max(table(datagiven$Brand)))

sum(datagiven$Brand == 9)

sum(datagiven$Brand == 1)

sum(datagiven$Brand == 2)

sum(datagiven$Brand == 3)

sum(datagiven$Brand == 4)

sum(datagiven$Brand == 5)

sum(datagiven$Brand == 6)

sum(datagiven$Brand == 7)

sum(datagiven$Brand == 8)

# The most trusted brand is 9 which is "Andere Kaffeemarken" . 

sum(datagiven$Days_between_Purchase)

range(datagiven$Days_between_Purchase)

names(which.max(table(datagiven$Days_between_Purchase)))

sum(datagiven$Days_between_Purchase == 61)

# Most period between two coffee purchases is "7" days which is 16369.

# Median of period between two coffee purchases is 9. 

sd(datagiven$Days_between_Purchase)

summary(datagiven$Days_between_Purchase)

days <- datagiven$Days_between_Purchase < 31

days

# Most people buying coffee within 30 days are 117890.

kool <- 13096 + 117890

kool

xc <- table(datagiven$Days_between_Purchase)

View(xc)

summary(xc)

sum(datagiven$Age)

range(datagiven$Age)

names(which.max(table(datagiven$Age)))

sum(datagiven$Age == 4)

sum(datagiven$Age == 2)

sum(datagiven$Age == 3)

sum(datagiven$Age == 1)

sum(datagiven$Age == 5)

# Age of most people are 4 which is "50 to 59 years".

sum(datagiven$SEC)

range(datagiven$SEC)

names(which.max(table(datagiven$SEC)))

sum(datagiven$SEC == 3)

sum(datagiven$SEC == 1)

sum(datagiven$SEC == 2)

sum(datagiven$SEC == 4)

sum(datagiven$SEC == 5)

# The highest number of socioeconomiclevel is 3 which is "Middle Class" people.

sum(datagiven$Income)

range(datagiven$Income)

names(which.max(table(datagiven$Income)))

sum(datagiven$Income == 1)

sum(datagiven$Income == 2)

sum(datagiven$Income == 3)

sum(datagiven$Income == 4)

# Monthly Income is more of 3rd number which lies between "2500 to 3499 DM".

sum(datagiven$Household_Sz)

range(datagiven$Household_Sz)

names(which.max(table(datagiven$Household_Sz)))

sum(datagiven$Household_Sz == 2)

sum(datagiven$Household_Sz == 1)

sum(datagiven$Household_Sz == 3)

sum(datagiven$Household_Sz == 4)

sum(datagiven$Household_Sz == 5)

# Most sold household size is 2.

sum(datagiven$Price_Conscious)

range(datagiven$Price_Conscious)

names(which.max(table(datagiven$Price_Conscious)))

sum(datagiven$Price_Conscious == 1)

sum(datagiven$Price_Conscious == 2)

sum(datagiven$Price_Conscious == 3)

sum(datagiven$Price_Conscious == 4)

totalPrice <- sum(datagiven$Price_Conscious == 1) + sum(datagiven$Price_Conscious == 2) + sum(datagiven$Price_Conscious == 3) + sum(datagiven$Price_Conscious == 4)

totalPrice

# Most Price-Consciousness people are 1 which is "Not at all" however 4 is also near only 
# which is "distinctly price-conscious".

sum(datagiven$Education)

range(datagiven$Education)

names(which.max(table(datagiven$Education)))

sum(datagiven$Education == 3) 

sum(datagiven$Education == 2) 

sum(datagiven$Education == 1 ) 

str(datagiven)

aaa <- 55237 + 38641 + 37108

aaa

dim(datagiven)

# In Education column "high-school / university" are the most likely to consume coffee.

sum(datagiven$Loyalty)

range(datagiven$Loyalty)

names(which.max(table(datagiven$Loyalty)))

sum(datagiven$Loyalty == 2)

# Most "loyal" people are more which is 75057.

loyal <- 75057 + 55929

loyal

packets <- 105022 + 6675 + 19289

packets

attach(datagiven)

ak <- factor(Days_between_Purchase)

ak

table(Age, Loyalty)

# Brand 2 "Jacobs other" and 5 "Eduscho Gala" have more "not loyal" observations than "loyal" observations.

table(Brand , Loyalty)

table(Brand , Age)

# Age group 4 which is "50 to 59 years" is the highest buyer of the coffee brand 9 which is "Andere Kaffeemarken".
# and age group 1 which is "less than 24 years" is the lowest buyer.

table(No_of_Packet , Age)

# The age group 4 which is "50 to 59 years" is the highest buyer of coffe packets "one packet" that is 1.

table(Price_Conscious , Age)

# The highest number of Age Group who are price conscious is 4 and also highest number of Age Group 
# who are not at all price conscious is also four.

table(Brand , Income)

# Since the brand 9 which is "Andere Kaffeemarken" is the highest selling brand but the income of people who 
# are earning 1 which is "less than 1499 DM" is the second buyer after 3 which is "2500 to 3499 DM".

summary(datagiven$Days_between_Purchase)

sd(Days_between_Purchase)

table(Days_between_Purchase , Age)

table(Days_between_Purchase , Household_Sz)

by(datagiven , INDICES = Brand , FUN = summary)

table(Price_Conscious , Loyalty)

boxplot(Education ~ Loyalty , horizontal = TRUE)

summary(Days_between_Purchase)

names(which.max(table(datagiven$Days_between_Purchase)))

sd(Days_between_Purchase)

pnorm(1 , mean = 15.14 , sd = 20.58186 , lower.tail=FALSE)

library(rpivotTable)

rpivotTable(datagiven)

one <- 33548 + 20055 + 15662

one1 <- 29888/130986

one1 * 100

summary(Days_between_Purchase)

names(which.max(table(Days_between_Purchase)))

sd(Days_between_Purchase)

var(Days_between_Purchase)

barplot(Days_between_Purchase)

plot(Days_between_Purchase)

plot(Loyalty , SEC)



