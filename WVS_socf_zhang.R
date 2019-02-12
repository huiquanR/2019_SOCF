#### 2018.6.13 Attach all the packages in R ####

library(readstata13)
library(qdap)
library(questionr)
library(data.table)
library(foreign)
library(ggrepel)
library(boot)
library(MASS)
library(plyr)
library(dplyr)
library(reshape)
library(mi)
library(mitools)
library(mice)
library(car)
library(effects)
library(ggplot2)
library(lme4)
library(lmerTest)
library(psych)
library(cplm)
library(laeken)
library(timeDate)
library(lubridate)
# set fonts for plotting #
windowsFonts()
windowsFonts(Arial = windowsFont("TT Arial"))
windowsFonts(Century = windowsFont("TT Century"))
windowsFonts(Times = windowsFont("TT Times New Roman"))
windowsFonts(Centaur = windowsFont("TT Centaur"))
windowsFonts(Palatino = windowsFont("TT Palatino"))
windowsFonts(Garamond = windowsFont("TT Garamond"))

# 2016.6.13/2018.6.10/2018.11.1 Read in Data #

# this data is downloaded from World Values Survey #
WVSdata = read.dta13("..../WVS6waves_20150418.dta")

# weight variable #
dist_tab(WVSdata$S017)
table(is.na(WVSdata$S017) == F)
dist_tab(WVSdata$S002, weights = WVSdata$S017)
wtd.table(WVSdata$S002, weights = WVSdata$S017, normwt = T)
mean(WVSdata$X003, na.rm = T)
wtd.mean(WVSdata$X003, weights = WVSdata$S017)
#
myvars = c(
  "S002",
  "S003",
  "S020",
  "S017",
  "S018",
  "S019",
  "X001",
  "X003",
  "X007",
  "X011",
  "X025",
  "X028",
  "X036",
  "X045",
  "X045B",
  "X047",
  "X049",
  "Y001",
  "A006",
  "F050",
  # requested by Reviewer 2, Nov 2018.
  "A025",
  "A042",
  "A008",
  "A124_02",
  "A124_06",
  "A124_09",
  "A165",
  "A170",
  "B001",
  "B002",
  "B003",
  "B004",
  "B008",
  "B009",
  "C001",
  "C002",
  "D018",
  "D019",
  "D022",
  "D023",
  "D024",
  "D054",
  "D056",
  "D057",
  "D058",
  "D059",
  "D060",
  "E001",
  "E002",
  "E001_HK",
  "E002_HK",
  "E003",
  "E004",
  "E005",
  "E006",
  "E012",
  "E017",
  "E018",
  "E023",
  "E025",
  "E026",
  "E027",
  "E028",
  "E029",
  "F051",
  "F052",
  "F053",
  "F054",
  "F063",
  "F064",
  "F118",
  "F119",
  "F120",
  "F121",
  "F122",
  "F123",
  "G006",
  "G019",
  "G032",
  "X023",
  "X023R",
  "X024",
  "X024B",
  "X025",
  "X025A",
  "X025B",
  "X025C",
  "X025CS",
  "X025CSWVS",
  "X025LIT",
  "X025R",
  "A029",
  "A035",
  "A040",
  "E224",
  "E225",
  "E226",
  "E227",
  "E228",
  "E229",
  "E230",
  "E231",
  "E232",
  "E233",
  "E233A",
  "E233B",
  "E234",
  "E235",
  "F025",
  # 2016.6.30 Religoin, Frequency
  "F028",
  "E143",
  "E114",
  "E117",
  "A173"
)
wvsdf = WVSdata[, myvars]

# [DC] 1.WAVES #
table(wvsdf$S002)
wvsdf$S002_1 = as.character(wvsdf$S002)
table(wvsdf$S002_1)
wvsdf$wave = 0
wvsdf = within(wvsdf, {
  wave [S002_1 == "1981-1984"] = 1
  wave [S002_1 == "1989-1993"] = 2
  wave [S002_1 == "1994-1998"] = 3
  wave [S002_1 == "1999-2004"] = 4
  wave [S002_1 == "2005-2009"] = 5
  wave [S002_1 == "2010-2014"] = 6
})
wvsdf$wave = as.factor(as.character(wvsdf$wave))
table(wvsdf$wave)

# [DC] 2. Year of Survey: S020 # 3. Country: S003 & S003_1 #

wvsdf$S020_1 = as.character(wvsdf$S020)
wvsdf$year = wvsdf$S020_1
wvsdf$S003_1 = as.factor(as.character(wvsdf$S003))
wvsdf$country = wvsdf$S003_1
wvsdf$country = as.character(wvsdf$country)
wvsdf$country = gsub(" ", '', wvsdf$country, fixed = TRUE)
wvsdf$country = gsub(" ", '', wvsdf$country, fixed = TRUE)
wvsdf$country = gsub(".", '', wvsdf$country, fixed = TRUE)

# [wvsdf] 2016.6.21  Country-Year & Country-Wave #

wvsdf$CY = NA # correct some countries with numeric tags. #
wvsdf$country = as.character(wvsdf$country)
wvsdf = within(wvsdf, {
  country [country == "Bosnia_(70)"] = "Bosnia"
  country [country == "Bosnia_(914)"] = "Bosnia"
  country [country == "GreatBritain_(826)"] = "GreatBritain"
  country [country == "GreatBritain_(826)"] = "GreatBritain"
  country [country == "Montenegro_(499)"] = "Montenegro"
  country [country == "Montenegro_(499)"] = "Montenegro"
  country [country == "Serbia_(688)"] = "Serbia"
  country [country == "Serbia_(688)"] = "Serbia"
})
wvsdf$year = as.character(wvsdf$year)
wvsdf$CY = as.character(paste(wvsdf$country, wvsdf$year, sep = "_"))
CYlist = data.frame(dist_tab(wvsdf$CY))
L1 = CYlist$interval

#### [wvsdf and CYdata]  2016.6.6 READ CYdata and describe it: ####
# header = TRUE makes the first line the variable name.
CYdata = read.table(
  "..../CYdata_GDP_20151020_reculture.csv", # Can be requested from the author
  header = TRUE,
  sep = ","
)
L2 = CYdata$CY
# COMPARE AND CHECK CYdata vs WVSDF$CY #
intersect(L1, L2)
setdiff(L1, intersect(L1, L2))
setdiff(L2, intersect(L1, L2))
wvsdf$CW = NA
wvsdf$CW = as.character(paste(wvsdf$country, wvsdf$wave, sep = "_"))

## 4. Gender  ##
table(wvsdf$X001)
wvsdf$X001_1 = as.character(wvsdf$X001)
table(wvsdf$X001_1)
wvsdf$male = NA
wvsdf = within(wvsdf, {
  male [X001_1 == "Male"] = 1
  male [X001_1 == "Female"] = 0
})
table(wvsdf$male)

## 5. Age ##
table(wvsdf$X003)
wvsdf$X003_1 = as.numeric(as.character(wvsdf$X003))
fivenum(wvsdf$X003_1)
wvsdf$age = wvsdf$X003_1
wvsdf = within(wvsdf, {
  age [X003_1 < 13] = NA
})
table(wvsdf$age)
fivenum(wvsdf$age)
dist_tab(wvsdf$age)

# [DC] 6. Marital & KIDS  # [DC]
wvsdf$X007_1 = as.character(wvsdf$X007)
dist_tab(wvsdf$X007)
dist_tab(wvsdf$X007_1)
wvsdf$marital = NA
wvsdf = within(wvsdf, {
  marital [X007_1 == "Married" |
             X007_1 == "Living together as married"] = "1.Married_Cohab"
  marital [X007_1 == "Divorced, Separated or Widow" |
             X007_1 == "Divorced" |
             X007_1 == "Widowed" |
             X007_1 == "Separated"] = "2.Divorce_Sep_Widowed"
  marital [X007_1 == "Single/Never married"] = "0.Single_Never"
})
dist_tab(wvsdf$marital)

#
wvsdf$X011_1 = as.character(wvsdf$X011)
dist_tab(wvsdf$X011)
dist_tab(wvsdf$X011_1)
wvsdf$kids = NA
wvsdf = within(wvsdf, {
  kids [X011_1 == "No child"] = 0
  kids [X011_1 == "1 child"] = 1
  kids [X011_1 == "2 children"] = 2
  kids [X011_1 == "3 children"] = 3
  kids [X011_1 == "4 children"] = 4
  kids [X011_1 == "5 children"] = 5
  kids [X011_1 == "6 children"] = 6
  kids [X011_1 == "7 children"] = 7
  kids [X011_1 == "8 or more children"] = 8
})
dist_tab(wvsdf$kids)

# [DC] 7. Edu.level #
wvsdf$X025_1 = as.character(wvsdf$X025)
wvsdf$X025_1 = as.numeric(as.factor(wvsdf$X025_1))
dist_tab(wvsdf$X025)
dist_tab(wvsdf$X025_1)
dist_tab(wvsdf$X023)
dist_tab(as.numeric(wvsdf$X023R))
dist_tab(as.numeric(wvsdf$X025CSWVS))
dist_tab(as.numeric(wvsdf$X025LIT))
wvsdf$X023R1 = as.numeric(wvsdf$X023R)
wvsdf$edulevel = NA

wvsdf = within(wvsdf, {
  edulevel [as.numeric(wvsdf$X025LIT) == 6] = "1.Elementary"
  edulevel [as.numeric(wvsdf$X025LIT) == 7] = "0.None"
  edulevel [as.numeric(wvsdf$X025CSWVS) > 0] = "0.None"
  edulevel [X023 >= 1 & X023 <= 5] = "0.None"
  edulevel [X023 >= 6 & X023 <= 12] = "1.Elementary"
  edulevel [X023 >= 13 & X023 <= 15] = "2.Secondary"
  edulevel [X023 >= 16 & X023 <= 18] = "3.High_Sch"
  edulevel [X023 >= 19] = "4.College&Above"
  edulevel [X023R1 == 1] = "1.Elementary"
  edulevel [X023R1 >= 2 & X023R1 <= 4] = "2.Secondary"
  edulevel [X023R1 >= 5 & X023R1 <= 7] = "3.High_Sch"
  edulevel [X023R1 >= 8] = "4.College&Above"
  edulevel [X025_1 == "10" |
              X025_1 == "5" |
              X025_1 == "4" | X025_1 == "8"] = "0.None"
  edulevel [X025_1 == "3" | X025_1 == "6"] = "1.Elementary"
  edulevel [X025_1 == "1" | X025_1 == "7"] = "2.Secondary"
  edulevel [X025_1 == "2" | X025_1 == "12"] = "3.High_Sch"
  edulevel [X025_1 == "13"] = "4.College&Above"
})
dist_tab(wvsdf$edulevel)

# [DC] 8. Occupation #
wvsdf$job = NA
wvsdf$X028 = as.character(wvsdf$X028)
wvsdf$X036 = as.character(wvsdf$X036)
dist_tab(wvsdf$X028)
dist_tab(wvsdf$X036)
wvsdf$X036 = gsub('"', '', wvsdf$X036, fixed = TRUE)
dist_tab(wvsdf$X036) # please note that X036 it self has ", remove it.

wvsdf$job = NA
wvsdf = within(wvsdf, {
  job [X028 == "Housewife" |
         X028 == "Unemployed" |
         X036 == "Never had a job"] = "0.Not Employed"
  job [X028 == "Students"]  = "1.Student"
  job [X028 == "Retired"] = "2.Retired"
  job [X036 == "Agricultural worker"
       | X036 == "Unskilled manual"] = "3.Unskilled_Manual"
  job [X036 == "Semi-skilled manual worker"
       | X036 == "Skilled manual"] = "4.Skilled_Manual"
  job [X036 == "Junior level non manual"
       | X036 == "Non manual -office worker"
       | X036 == "Foreman and supervisor"] = "5.Non-Manual_Office"
  job [X036 == "Middle level non-manual office worker"
       | X036 == "Professional worker"
       | X036 == "Member of armed forces"] = "6.Professional"
  job [X036 == "Supervisory Non manual -office worker" |
         X036 == "Employer/manager of establishment with 10 or more employed"
       | X036 == "Farmer: has own farm"
       |
         X036 == "Employer/manager of establishment with less than 10 employe"] = "7.Manager_Owner"
})
dist_tab(wvsdf$job)


# [wvsdf] 9.Size of Town #
wvsdf$X049_1 = as.character(wvsdf$X049)
dist_tab(wvsdf$X049)
dist_tab(wvsdf$X049_1)
wvsdf$townsize = NA
wvsdf = within(wvsdf, {
  townsize [X049_1 == "2,000 and less"] = "A.<2K"
  townsize [X049_1 == "2,000-5,000"]   = "B.2-5K"
  townsize [X049_1 == "5,000-10,000"]  = "C.5-10K"
  townsize [X049_1 == "10,000-20,000/10,000-25,000; EVS81:10M-25M"] = "D.10-20K"
  townsize [X049_1 == "20,000-50,000"]  = "E.20-50K"
  townsize [X049_1 == "50,000-100,000"] = "F.50-100K"
  townsize [X049_1 == "100,000-500,000"] = "G.100-500K"
  townsize [X049_1 == "500,000 and more"] = "H.>500K"
})
dist_tab(wvsdf$townsize)

#### [wvsdf] 2016.6.30 Religion ####

dist_tab(wvsdf$F025)
wvsdf$F025_1 = as.character(wvsdf$F025)
dist_tab(wvsdf$F025)
dist_tab(wvsdf$F025_1)
wvsdf$religion = NA
wvsdf = within(wvsdf, {
  religion [F025_1 == "Aglipayan"] = "2.Catholic"
  religion [F025_1 == "Al-Hadis"]  = "4.Islamic"
  religion [F025_1 == "Alliance"]  = "1.Protestant"
  religion [F025_1 == "Ancestral worshipping"]  = "7.other"
  religion [F025_1 == "Anglican"]  = "1.Protestant"
  religion [F025_1 == "Armenian Apostolic Church"]  = "3.Orthodox"
  religion [F025_1 == "Assembly of God"] = "1.Protestant"
  religion [F025_1 == "AU: Uniting Church"]  = "1.Protestant"
  #1-8#
  religion [F025_1 == "Bahai"]  = "7.other"
  religion [F025_1 == "Baptist"]  = "1.Protestant"
  religion [F025_1 == "Born again"]  = "1.Protestant"
  religion [F025_1 == "Brgy. Sang Birhen"]  = "2.Catholic"
  religion [F025_1 == "Buddhist"]  = "6.Buddhist,Taoist,otherAsian"
  #9-13#
  religion [F025_1 == "C & S Celestial"]  = "1.Protestant"
  religion [F025_1 == "Cao dai"]  = "6.Buddhist,Taoist,otherAsian"
  religion [F025_1 == "Catholic: doesn´t follow rules"]  = "2.Catholic"
  religion [F025_1 == "Charismatic"]  = "1.Protestant"
  religion [F025_1 == "Christian"]  = "1.Protestant"
  religion [F025_1 == "Christian Fellowship"]  = "1.Protestant"
  religion [F025_1 == "Christian Reform"]  = "1.Protestant"
  religion [F025_1 == "Church of Christ / Church of Christ / Church of Christ of La"]  = "1.Protestant"
  religion [F025_1 == "Confucianism"]  = "6.Buddhist,Taoist,otherAsian"
  #14-22#
  
  religion [F025_1 == "Don´t know"]  = "0.None"
  religion [F025_1 == "Druse"]  = "4.Islamic"
  religion [F025_1 == "Dutch Reformed (Nederlands Hervormd)"]  = "1.Protestant"
  religion [F025_1 == "DZ: Christian (Quakers, Jehovah's Witnesses, Evangelical, Pr"]  = "1.Protestant"
  religion [F025_1 == "El Shaddai"]  = "2.Catholic"
  religion [F025_1 == "Essid"]  = "7.other"
  religion [F025_1 == "Evangelical"]  = "1.Protestant"
  religion [F025_1 == "Faith in god"]  = "2.Catholic" # only one person in Philippines
  religion [F025_1 == "Filipinista"]  = "2.Catholic"
  religion [F025_1 == "Free church/Non denominational church"]  = "1.Protestant"
  religion [F025_1 == "Greek Catholic"]  = "2.Catholic"
  religion [F025_1 == "Gregorian"]  = "3.Orthodox"
  religion [F025_1 == "Hindu"]  = "5.Hindu_OtherSouthAsian"
  religion [F025_1 == "Hoa hao"]  = "6.Buddhist,Taoist,otherAsian"
  religion [F025_1 == "Hussite"]  = "1.Protestant"
  religion [F025_1 == "Iglesia ni Cristo (INC)"]  = "2.Catholic"
  religion [F025_1 == "Independent African Church (e.g. ZCC, Shembe, etc.)"]  = "1.Protestant"
  religion [F025_1 == "Independent Church"]  = "1.Protestant"
  religion [F025_1 == "Israelita Nuevo Pacto Universal (FREPAP)"]  = "2.Catholic"
  religion [F025_1 == "Jain"]  = "6.Buddhist,Taoist,otherAsian" # india, buddhist.
  religion [F025_1 == "Jehovah witnesses"]  = "1.Protestant"
  religion [F025_1 == "Jesus is Lord (JIL)"]  = "1.Protestant"
  religion [F025_1 == "Jesus Miracle Crusade"]  = "1.Protestant"
  religion [F025_1 == "Jew"]  = "7.other"
  religion [F025_1 == "Ka-a Elica"]  = "2.Catholic"  # philipine.
  religion [F025_1 == "Lutheran"]  = "1.Protestant"
  religion [F025_1 == "Mennonite"]  = "1.Protestant"
  religion [F025_1 == "Methodists"]  = "1.Protestant"
  religion [F025_1 == "Missing"]  = "7.other"
  religion [F025_1 == "Mita"]  = "1.Protestant"
  religion [F025_1 == "Mormon"]  = "7.other"
  religion [F025_1 == "Muslim"]  = "4.Islamic"
  religion [F025_1 == "Native"]  = "7.other"
  religion [F025_1 == "New Apostolic Church"]  = "2.Catholic"
  religion [F025_1 == "New Testament Christ/Biblist"]  = "1.Protestant"
  religion [F025_1 == "No answer"]  = "0.None" # no answer happens all over, make it None.
  religion [F025_1 == "No religious denomination"]  = "0.None"
  religion [F025_1 == "Not asked in survey"]  = NA
  religion [F025_1 == "Orthodox"]  = "3.Orthodox"
  religion [F025_1 == "Other"]  = "7.other"
  religion [F025_1 == "Other: Brasil: Espirit,candomblé,umbanda,esoterism,occultism"]  = "7.other"
  religion [F025_1 == "Other: Christian com"]  = "7.other"
  religion [F025_1 == "Other: Oriental"]  = "7.other"
  religion [F025_1 == "Other: Philippines (less 0.5%)"]  = "7.other"
  religion [F025_1 == "Other: Taiwan (taoism, protestant fundam., ancient cults)"]  = "6.Buddhist,Taoist,otherAsian"
  religion [F025_1 == "Paganism"]  = "7.other"
  religion [F025_1 == "Pentecostal"] = "1.Protestant"
  religion [F025_1 == "Presbyterian"]  = "1.Protestant"
  religion [F025_1 == "Protestant"]  = "1.Protestant"
  religion [F025_1 == "Qadiani"]  = "4.Islamic"
  religion [F025_1 == "Ratana"]  = "7.other"
  religion [F025_1 == "Reformed Churches in the Netherlands (Gereformeerd)"]  = "1.Protestant"
  religion [F025_1 == "Roman Catholic"]  = "2.Catholic"
  religion [F025_1 == "Salvation Army"]  = "1.Protestant"
  religion [F025_1 == "Self Lealisation Fellowship"]  = "6.Buddhist,Taoist,otherAsian"
  religion [F025_1 == "Seven Day Adventist"]  = "1.Protestant"
  religion [F025_1 == "Shenism (Chinese Religion)"]  = "6.Buddhist,Taoist,otherAsian"
  religion [F025_1 == "Shia"]  = "4.Islamic"
  religion [F025_1 == "Sikh"]  = "5.Hindu_OtherSouthAsian"
  religion [F025_1 == "Sisewiss"]  = "7.other"
  religion [F025_1 == "Spiritista"]  = "7.other"
  religion [F025_1 == "Spiritualists"]  = "7.other"
  religion [F025_1 == "Sunni"]  = "4.Islamic"
  religion [F025_1 == "Tac"]  = "2.Catholic"
  religion [F025_1 == "Taoist"]  = "6.Buddhist,Taoist,otherAsian"
  religion [F025_1 == "The Church of Sweden"]  = "1.Protestant"
  religion [F025_1 == "The Worldwide Church of God"]  = "1.Protestant"
  religion [F025_1 == "Theosofists"]  = "7.other"
  religion [F025_1 == "Unitarian"]  = "1.Protestant"
  religion [F025_1 == "United"]  = "1.Protestant"
  religion [F025_1 == "United Church of Christ in the Philippines (UCCP)"]  = "1.Protestant"
  religion [F025_1 == "Wicca"]  = "7.other"
  religion [F025_1 == "Yiguan Dao"]  = "6.Buddhist,Taoist,otherAsian"
  religion [F025_1 == "ZA: African Traditional Religion"]  = "7.other"
  religion [F025_1 == "ZA: Evangelical/Apostolic Faith Mission"]  = "1.Protestant"
  religion [F025_1 == "Zionist"]  = "7.other"
  religion [F025_1 == "Zoroastrian"]  = "7.other"
})
dist_tab(wvsdf$religion)
dist_tab(wvsdf$F025_1)
names(wvsdf)

#
dist_tab(wvsdf$F028)
wvsdf$F028_1 = as.character(wvsdf$F028)
dist_tab(wvsdf$F028)
dist_tab(wvsdf$F028_1)
wvsdf$relig_freq = NA
wvsdf = within(wvsdf, {
  relig_freq [F028_1 == "Never practically never"] = "0.Never"
  relig_freq [F028_1 == "Not applicable"] = "0.Never"
  relig_freq [F028_1 == "Less often"] = "1.Less often than Yearly"
  relig_freq [F028_1 == "More than once a week"] = "5.Daily"
  relig_freq [F028_1 == "Once a week"] = "4.Weekly"
  relig_freq [F028_1 == "Once a month"] = "3.Monthly"
  relig_freq [F028_1 == "Only on special holy days/Christmas/Easter days"] = "2.Yearly"
  relig_freq [F028_1 == "Once a year"] = "2.Yearly"
  relig_freq [F028_1 == "Other specific holy days"] = "2.Yearly"
  
})
dist_tab(wvsdf$relig_freq)
names(wvsdf)

#
# tolerance of neighbor, homosexuals #
wvsdf$A124_09_1 = as.character(wvsdf$A124_09)
dist_tab(wvsdf$A124_09)
dist_tab(wvsdf$A124_09_1)
wvsdf$NB_HOM = NA
wvsdf = within(wvsdf, {
  NB_HOM [A124_09_1 == "Not mentioned"] = 10
  NB_HOM [A124_09_1 == "Mentioned"]   = 0
})
dist_tab(wvsdf$NB_HOM)

#### [wvsdf] Data Cleaning: F question: justify. ####

# Justify Homosexuality #
wvsdf$F118_1 = as.character(wvsdf$F118)
dist_tab(wvsdf$F118)
dist_tab(wvsdf$F118_1)
wvsdf$JUSTIFY_HOM = NA
wvsdf = within(wvsdf, {
  JUSTIFY_HOM [F118_1 == "Never justifiable"] = 1
  JUSTIFY_HOM [F118_1 == "Always justifiable"] = 10
  JUSTIFY_HOM [F118_1 == "2"]        = 2
  JUSTIFY_HOM [F118_1 == "3"]        = 3
  JUSTIFY_HOM [F118_1 == "4"]        = 4
  JUSTIFY_HOM [F118_1 == "5"]        = 5
  JUSTIFY_HOM [F118_1 == "6"]        = 6
  JUSTIFY_HOM [F118_1 == "7"]        = 7
  JUSTIFY_HOM [F118_1 == "8"]        = 8
  JUSTIFY_HOM [F118_1 == "9"]        = 9
})
dist_tab(wvsdf$JUSTIFY_HOM)

# Believe in: F 050-054
wvsdf$F050_1 = as.character(wvsdf$F050)
dist_tab(wvsdf$F050)
dist_tab(wvsdf$F050_1)
wvsdf$believe_god = NA
wvsdf = within(wvsdf, {
  believe_god [F050_1 == "Yes"] = 0
  believe_god [F050_1 == "No"] = 10
})
dist_tab(wvsdf$believe_god)

wvsdf$F051_1 = as.character(wvsdf$F051)
dist_tab(wvsdf$F051)
dist_tab(wvsdf$F051_1)
wvsdf$believe_afterlife = NA
wvsdf = within(wvsdf, {
  believe_afterlife [F051_1 == "Yes"] = 0
  believe_afterlife [F051_1 == "No"] = 10
})
dist_tab(wvsdf$believe_afterlife)

wvsdf$F052_1 = as.character(wvsdf$F052)
dist_tab(wvsdf$F052)
dist_tab(wvsdf$F052_1)
wvsdf$believe_soul = NA
wvsdf = within(wvsdf, {
  believe_soul [F052_1 == "Yes"] = 0
  believe_soul [F052_1 == "No"] = 10
})
dist_tab(wvsdf$believe_soul)

wvsdf$F053_1 = as.character(wvsdf$F053)
dist_tab(wvsdf$F053)
dist_tab(wvsdf$F053_1)
wvsdf$believe_hell = NA
wvsdf = within(wvsdf, {
  believe_hell [F053_1 == "Yes"] = 0
  believe_hell [F053_1 == "No"] = 10
})
dist_tab(wvsdf$believe_hell)

wvsdf$F054_1 = as.character(wvsdf$F054)
dist_tab(wvsdf$F054)
dist_tab(wvsdf$F054_1)
wvsdf$believe_heaven = NA
wvsdf = within(wvsdf, {
  believe_heaven [F054_1 == "Yes"] = 0
  believe_heaven [F054_1 == "No"] = 10
})
dist_tab(wvsdf$believe_heaven)

# Please be aware of this variable's direction, and 10 levels.#
wvsdf$F063_1 = as.character(wvsdf$F063)
dist_tab(wvsdf$F063)
dist_tab(wvsdf$F063_1)
wvsdf$godimportant = NA

wvsdf = within(wvsdf, {
  godimportant [F063_1 == "Very important"] = 1
  godimportant [F063_1 == "Not at all important"] = 10
  godimportant [F063_1 == "2"]        = 9
  godimportant [F063_1 == "3"]        = 8
  godimportant [F063_1 == "4"]        = 7
  godimportant [F063_1 == "5"]        = 6
  godimportant [F063_1 == "6"]        = 5
  godimportant [F063_1 == "7"]        = 4
  godimportant [F063_1 == "8"]        = 3
  godimportant [F063_1 == "9"]        = 2
})
dist_tab(wvsdf$godimportant)

wvsdf$F064_1 = as.character(wvsdf$F064)
dist_tab(wvsdf$F064)
dist_tab(wvsdf$F064_1)
wvsdf$religionhelps = NA
wvsdf = within(wvsdf, {
  religionhelps [F064_1 == "Yes"] = 0
  religionhelps [F064_1 == "No"] = 10
})
dist_tab(wvsdf$religionhelps)
#
#### [wvsdf] 2018.11.1 weights+neighbours 2018.11.5 IMPORTANCE A006 F050 ####
wvsdf$WT = wvsdf$S017
table(wvsdf$A006)
wvsdf$A006 = trimws(as.character(wvsdf$A006))
wvsdf$IMPORTANT = NA
wvsdf = within(wvsdf, {
  IMPORTANT [A006 == "Not at all important"] = 1
  IMPORTANT [A006 == "Not very important"]   = 2
  IMPORTANT [A006 == "Rather important"]     = 3
  IMPORTANT [A006 == "Very important"]       = 4
})
dist_tab(wvsdf$IMPORTANT)

table(wvsdf$F050)
dist_tab(wvsdf$believe_god)
dist_tab(wvsdf$NB_HOM)

#### [wvsdf] 2016.6.21 Homosexuality : Select Relevant Variables in WVSDF into WVSDF1 ####
dist_tab(wvsdf$JUSTIFY_HOM)
names(wvsdf)
myvars = c(
  "wave",
  "country",
  "CY",
  "CW",
  "year",
  "male",
  "age",
  "marital",
  "kids",
  "edulevel",
  "job",
  "JUSTIFY_HOM",
  "religion",
  "relig_freq",
  "WT",
  "NB_HOM",
  "believe_god",
  "IMPORTANT"
)
wvsdf = wvsdf[, myvars]
fivenum(wvsdf$WT)
dist_tab(wvsdf$NB_HOM)
dist_tab(wvsdf$believe_god)
dist_tab(wvsdf$IMPORTANT)
wvsdf$NEIGHBOR_HOM = NA
wvsdf$NEIGHBOR_HOM = ifelse(wvsdf$NB_HOM == 10, 1, wvsdf$NEIGHBOR_HOM)
wvsdf$NEIGHBOR_HOM = ifelse(wvsdf$NB_HOM == 0,  0, wvsdf$NEIGHBOR_HOM)
names(wvsdf)
#

#### [CA-1990] 2016.6.17 [-] Merge CYdata, to WVSDF1, and Canada 1990 ####
#
wvsdf1 = merge(CYdata, wvsdf, by = "CY")
Canada_1990 = read.csv("....../wvs-canada-1990.csv",
                       # can be requested from The Research Data Centres (RDC) in Canada
                       header = TRUE)
names(Canada_1990)
dist_tab(Canada_1990$V9)
dist_tab(Canada_1990$V166)
dist_tab(Canada_1990$V80)
dist_tab(Canada_1990$S017)
dist_tab(Canada_1990$WEIGHT)
CA1990vars1 = c(
  "V353",
  "V355",
  "V181",
  "V356",
  "V358",
  "V359",
  "V307",
  "V144",
  "V147",
  "WEIGHT",
  "V80",
  "V9",
  "V166"
)

CA1990 = Canada_1990[, CA1990vars1]
CA1990$NEIGHBOR_HOM = NA
CA1990$NEIGHBOR_HOM = ifelse(CA1990$V80 == "no",       1, CA1990$NEIGHBOR_HOM)
CA1990$NEIGHBOR_HOM = ifelse(CA1990$V80 == "mention",  0, CA1990$NEIGHBOR_HOM)
CA1990$WT = CA1990$WEIGHT
fivenum(CA1990$WT)

#
table(CA1990$V9)
CA1990$V9 = trimws(as.character(CA1990$V9))
CA1990$IMPORTANT = NA
CA1990 = within(CA1990, {
  IMPORTANT [V9 == "notatall"] = 1
  IMPORTANT [V9 == "not very"] = 2
  IMPORTANT [V9 == "quite"]    = 3
  IMPORTANT [V9 == "very"]     = 4
})
dist_tab(CA1990$IMPORTANT)

#
table(CA1990$V166)
CA1990$V166 = trimws(as.character(CA1990$V166))
CA1990$believe_god = NA
CA1990 = within(CA1990, {
  believe_god [V166 == "yes"] = 0
  believe_god [V166 == "no"]  = 10
})
dist_tab(CA1990$believe_god)
#
CA1990$GDPPC = 27870.9 #2005 constant usd, noPPP
CA1990$CY = "Canada_1990"
CA1990$year = "1990"
CA1990$GINI = 27.63563728
CA1990$CultureZone = "1.Western_WestEuroNorthAmerica"
CA1990$wave = "2"
CA1990$FREESCORE = 1
CA1990$country = "Canada"

dist_tab(CA1990$V144)
dist_tab(CA1990$V147)
CA1990$V144_1 = as.character(CA1990$V144)
dist_tab(CA1990$V144)
dist_tab(CA1990$V144_1)
CA1990$religion = NA
CA1990 = within(CA1990, {
  religion [V144_1 == "buddhist"] = "6.Buddhist,Taoist,otherAsian"
  religion [V144_1 == "catholic"]  = "2.Catholic"
  religion [V144_1 == "hindu"]  = "5.Hindu_OtherSouthAsian"
  religion [V144_1 == "jew"]  = "7.other"
  religion [V144_1 == "muslim"]  = "4.Islamic"
  religion [V144_1 == "na"]  = NA
  religion [V144_1 == "never"] = "0.None"
  religion [V144_1 == "non-conf"]  = "0.None"
  religion [V144_1 == "other"]  = "7.other"
  religion [V144_1 == "protestant"]  = "1.Protestant"
})
dist_tab(CA1990$religion)
dist_tab(CA1990$V144_1)
names(CA1990)

dist_tab(CA1990$V147)
CA1990$V147_1 = as.character(CA1990$V147)
dist_tab(CA1990$V147)
dist_tab(CA1990$V147_1)
CA1990$relig_freq = NA
CA1990 = within(CA1990, {
  relig_freq [V147_1 == "holidays"] = "2.Yearly"
  relig_freq [V147_1 == "less"] = "1.Less often than Yearly"
  relig_freq [V147_1 == "na"] = "0.Never"
  relig_freq [V147_1 == "never"] = "0.Never"
  relig_freq [V147_1 == "once/mo"] = "3.Monthly"
  relig_freq [V147_1 == "once/wk"] = "4.Weekly"
  relig_freq [V147_1 == "once/yr"] = "2.Yearly"
  relig_freq [V147_1 == "once+/wk"] = "5.Daily"
  relig_freq [V147_1 == "xmas/eas"] = "2.Yearly"
})
dist_tab(CA1990$relig_freq)
names(CA1990)


CA1990$male = NA
CA1990 = within(CA1990, {
  male [V353 == "male"] = 1
  male [V353 == "female"] = 0
})
dist_tab(CA1990$male)
CA1990 = subset(CA1990, select = -c(V353))
# JOBS
dist_tab(CA1990$V358)
dist_tab(CA1990$V359)
class(CA1990$V359)
CA1990$job = NA
CA1990$V358 = as.character(CA1990$V358)
CA1990$V359 = as.character(CA1990$V359)
CA1990$V358 = gsub('"', '', CA1990$V358, fixed = TRUE)
CA1990$V359 = gsub('"', '', CA1990$V359, fixed = TRUE)
dist_tab(CA1990$V358)
dist_tab(CA1990$V359)

CA1990 = within(CA1990, {
  job [V358 == "hwife" |
         V358 == "unemp" | V359 == "nevr wrk"] = "0.Not Employed"
  job [V358 == "student"]  = "1.Student"
  job [V358 == "retired"] = "2.Retired"
  job [V359 == "farm wk"
       | V359 == "unskill"] = "3.Unskilled_Manual"
  job [V359 == "semi-sk"
       | V359 == "skilled"] = "4.Skilled_Manual"
  job [V359 == "junior"
       | V359 == "superv"] = "5.Non-Manual_Office"
  job [V359 == "middle"
       | V359 == "prof"
       | V359 == "arm forc"] = "6.Professional"
  job [V359 == "superv"
       | V359 == "farmer"
       | V359 == "<10emp" | V359 == ">10emp"] = "7.Manager_Owner"
})
dist_tab(CA1990$job)
dist_tab(CA1990$V358 [is.na(CA1990$job) == T])
dist_tab(CA1990$V359 [is.na(CA1990$job) == T])


# AGE
dist_tab(CA1990$V355)
CA1990$age = as.numeric(CA1990$V355)
CA1990 = within(CA1990, {
  age [age == 0] = NA
})
dist_tab(CA1990$age)
CA1990 = subset(CA1990, select = -c(V355))

#MARITAL
dist_tab(CA1990$V181)
CA1990$V181 = as.character(CA1990$V181)
CA1990$marital = NA
CA1990 = within(CA1990, {
  marital [V181 == "married" | V181 == "living"] = "1.Married_Cohab"
  marital [V181 == "separate" |
             V181 == "divorced" |
             V181 == "widow"] = "2.Divorce_Sep_Widowed"
  marital [V181 == "single"] = "0.Single_Never"
})
dist_tab(CA1990$marital)
CA1990 = subset(CA1990, select = -c(V181))

# eduyear -> categorical
dist_tab(CA1990$V356)
CA1990$edulevel = NA
CA1990 = within(CA1990, {
  edulevel [V356 == "<12yrs"] = "1.Elementary"
  edulevel [V356 == "13yrs"] = "2.Secondary"
  edulevel [V356 == "14yrs"] = "2.Secondary"
  edulevel [V356 == "15yrs"] = "3.High_Sch"
  edulevel [V356 == "16yrs"] = "3.High_Sch"
  edulevel [V356 == "17yrs"] = "3.High_Sch"
  edulevel [V356 == "18yrs"] = "4.College&Above"
  edulevel [V356 == "19yrs"] = "4.College&Above"
  edulevel [V356 == "20 yrs"] = "4.College&Above"
  edulevel [V356 == "21+yrs"] = "4.College&Above"
})
dist_tab(CA1990$edulevel)
CA1990 = subset(CA1990, select = -c(V356))

#
dist_tab(CA1990$V307)
CA1990$V307 = as.character(CA1990$V307)
CA1990$JUSTIFY_HOM = NA

CA1990 = within(CA1990, {
  JUSTIFY_HOM [V307 == "2"] = 2
  JUSTIFY_HOM [V307 == "3"] = 3
  JUSTIFY_HOM [V307 == "4"] = 4
  JUSTIFY_HOM [V307 == "5"] = 5
  JUSTIFY_HOM [V307 == "6"] = 6
  JUSTIFY_HOM [V307 == "7"] = 7
  JUSTIFY_HOM [V307 == "8"] = 8
  JUSTIFY_HOM [V307 == "9"] = 9
  JUSTIFY_HOM [V307 == "always"] = 10
  JUSTIFY_HOM [V307 == "never"] = 1
})
dist_tab(CA1990$JUSTIFY_HOM)
CA1990 = subset(CA1990, select = -c(V307))
#
#### [CA-1990] 2016.6.18 DV-protest, weighted. ####

fivenum(wvsdf$JUSTIFY_HOM)
fivenum(CA1990$JUSTIFY_HOM)
dist_tab(CA1990$male)

#
CA1990_clean = subset(
  CA1990,
  select = c(
    JUSTIFY_HOM,
    age,
    WT,
    male,
    edulevel,
    marital,
    GDPPC,
    CY,
    year,
    GINI,
    CultureZone,
    wave,
    FREESCORE,
    country,
    job,
    religion,
    relig_freq,
    NEIGHBOR_HOM,
    believe_god,
    IMPORTANT
  )
)
wvsdf3       = subset(
  wvsdf1,
  select = c(
    JUSTIFY_HOM,
    age,
    WT,
    male,
    edulevel,
    marital,
    GDPPC,
    CY,
    year,
    GINI,
    CultureZone,
    wave,
    FREESCORE,
    country,
    job,
    religion,
    relig_freq,
    NEIGHBOR_HOM,
    believe_god,
    IMPORTANT
  )
)
#
#### [CA-1981] 2016.6.30 Canada 1981 ####
#
Canada_1981 = read.csv("..../wvs-canada-1981.csv",
                       # can be requested from The Research Data Centres (RDC) in Canada
                       header = TRUE)
names(Canada_1981)
fivenum(Canada_1981$S017)

CA1981vars1 = c("X001",
                "X003",
                "X007",
                "X023",
                "X028",
                "X036",
                "F025",
                "F028",
                "F118",
                "S017")
CA1981 = Canada_1981[, CA1981vars1]
#
CA1981$NEIGHBOR_HOM = NA # this is not available in wave 1 in Canada.
CA1981$WT = CA1981$S017
CA1981$IMPORTANT = NA
CA1981$believe_god = NA

#
table(CA1981$V183)
CA1990$V166 = trimws(as.character(CA1990$V166))
CA1990$believe_god = NA
CA1990 = within(CA1990, {
  believe_god [V166 == "yes"] = 0
  believe_god [V166 == "no"]  = 10
})
dist_tab(CA1990$believe_god)
#
CA1981$GDPPC = 24788.6 #2005 constant usd, noPPP
CA1981$FREESCORE = 1
CA1981$GINI = 28.03 #20160603added
CA1981$CultureZone = "1.Western_WestEuroNorthAmerica"
CA1981$wave = "1"
CA1981$country = "Canada"
CA1981$CY = "Canada_1981"
CA1981$year = "1981"

# 4. Gender  #
table(CA1981$X001)
CA1981$X001_1 = as.character(CA1981$X001)
table(CA1981$X001_1)
CA1981$male = NA
CA1981 = within(CA1981, {
  male [X001_1 == "Male"] = 1
  male [X001_1 == "Female"] = 0
})
table(CA1981$male)

# 5. Age #
table(CA1981$X003)
CA1981$X003_1 = as.numeric(as.character(CA1981$X003))
fivenum(CA1981$X003_1)
CA1981$age = CA1981$X003_1
table(CA1981$age)
fivenum(CA1981$age)

# [DC] 6. Marital & KIDS  # [DC]
CA1981$X007_1 = as.character(CA1981$X007)
dist_tab(CA1981$X007)
dist_tab(CA1981$X007_1)
CA1981$marital = NA
CA1981 = within(CA1981, {
  marital [X007_1 == "Married" |
             X007_1 == "Living together as married"] = "1.Married_Cohab"
  marital [X007_1 == "Divorced" |
             X007_1 == "Widowed" |
             X007_1 == "Separated"] = "2.Divorce_Sep_Widowed"
  marital [X007_1 == "Single/Never married"] = "0.Single_Never"
})
dist_tab(CA1981$marital)

# [DC] 7. Edulevel  # [DC]
CA1981$X023_1 = as.character(CA1981$X023)
dist_tab(CA1981$X023)
dist_tab(CA1981$X023_1)
CA1981$edulevel = NA
CA1981 = within(CA1981, {
  edulevel [X023_1 == "No answer"] = "0.None"
  edulevel [X023_1 == "12"] = "1.Elementary"
  edulevel [X023_1 == "13"] = "2.Secondary"
  edulevel [X023_1 == "14"] = "2.Secondary"
  edulevel [X023_1 == "15"] = "2.Secondary"
  edulevel [X023_1 == "16"] = "3.High_Sch"
  edulevel [X023_1 == "17"] = "3.High_Sch"
  edulevel [X023_1 == "18"] = "3.High_Sch"
  edulevel [X023_1 == "19"] = "4.College&Above"
  edulevel [X023_1 == "20"] = "4.College&Above"
  edulevel [X023_1 == "21"] = "4.College&Above"
})
dist_tab(CA1981$edulevel)

# [DC] 8. Occupation #
dist_tab(CA1981$X028)
dist_tab(CA1981$X036)
CA1981$job = NA
CA1981$X028 = as.character(CA1981$X028)
CA1981$X036 = as.character(CA1981$X036)
dist_tab(CA1981$X028)
dist_tab(CA1981$X036)

CA1981 = within(CA1981, {
  job [X028 == "Housewife" | X028 == "Unemployed"] = "0.Not Employed"
  job [X028 == "Students"]  = "1.Student"
  job [X028 == "Retired"] = "2.Retired"
  job [X036 == "Agricultural worker"
       | X036 == "Unskilled manual"] = "3.Unskilled_Manual"
  job [X036 == "Semi-skilled manual worker"
       | X036 == "Skilled manual"] = "4.Skilled_Manual"
  job [X036 == "Junior level non manual"
       | X036 == "Non manual -office worker"
       | X036 == "Foreman and supervisor"] = "5.Non-Manual_Office"
  job [X036 == "Middle level non-manual office worker"
       | X036 == "Professional worker"
       | X036 == "Member of armed forces"] = "6.Professional"
  job [X036 == "Supervisory Non manual -office worker" |
         X036 == "Employer/manager of establishment with 10 or more employed"
       | X036 == "Farmer: has own farm"
       |
         X036 == "Employer/manager of establishment with less than 10 employe"
       |
         X036 == "Employer/manager of establishment w. less than 500 employed"
       |
         X036 == "Employer/manager of establishment with 500 or more employed"] = "7.Manager_Owner"
})
dist_tab(CA1981$job)
dist_tab(CA1981$X028 [is.na(CA1981$job) == T])
dist_tab(CA1981$X036 [is.na(CA1981$job) == T])

# [dv] F118#
dist_tab(CA1981$F118)
CA1981$JUSTIFY_HOM = NA
CA1981$F118 = as.character(CA1981$F118)
dist_tab(CA1981$F118)

CA1981 = within(CA1981, {
  JUSTIFY_HOM [F118 == "2"] = 2
  JUSTIFY_HOM [F118 == "3"] = 3
  JUSTIFY_HOM [F118 == "4"] = 4
  JUSTIFY_HOM [F118 == "5"] = 5
  JUSTIFY_HOM [F118 == "6"] = 6
  JUSTIFY_HOM [F118 == "7"] = 7
  JUSTIFY_HOM [F118 == "8"] = 8
  JUSTIFY_HOM [F118 == "9"] = 9
  JUSTIFY_HOM [F118 == "Always justifiable"] = 10
  JUSTIFY_HOM [F118 == "Never justifiable"] = 1
})
dist_tab(CA1981$JUSTIFY_HOM)

#### [CA-1981] 2016.7.14, redo-HOMO project - 1981 religion, relig_freq ####
dist_tab(CA1981$F025)
CA1981$F025_1 = as.character(CA1981$F025)
dist_tab(CA1981$F025)
dist_tab(CA1981$F025_1)
CA1981$religion = NA
CA1981 = within(CA1981, {
  religion [F025_1 == "Roman Catholic"]  = "2.Catholic"
  religion [F025_1 == "Hindu"]  = "5.Hindu_OtherSouthAsian"
  religion [F025_1 == "Jew"]  = "7.other"
  religion [F025_1 == "Muslim"]  = "4.Islamic"
  religion [F025_1 == "Not applicable"] = "0.None"
  religion [F025_1 == "Other"]  = "7.other"
  religion [F025_1 == "Protestant"]  = "1.Protestant"
})
dist_tab(CA1981$religion)
dist_tab(CA1981$F025_1)
names(CA1981)

dist_tab(CA1981$F028)
CA1981$F028_1 = as.character(CA1981$F028)
dist_tab(CA1981$F028)
dist_tab(CA1981$F028_1)
CA1981$relig_freq = NA
CA1981 = within(CA1981, {
  relig_freq [F028_1 == "Only on special holy days/Christmas/Easter days"] = "2.Yearly"
  relig_freq [F028_1 == "Less often"] = "1.Less often than Yearly"
  relig_freq [F028_1 == "Never practically never"] = "0.Never"
  relig_freq [F028_1 == "Once a month"] = "3.Monthly"
  relig_freq [F028_1 == "Once a week"] = "4.Weekly"
  relig_freq [F028_1 == "Once a year"] = "2.Yearly"
  relig_freq [F028_1 == "More than once a week"] = "5.Daily"
  relig_freq [F028_1 == "Other specific holy days"] = "2.Yearly"
})
dist_tab(CA1981$relig_freq)
names(CA1981)
#

#### [CA-1981] 2016.6.30 rbind, CA1990, CA1981 WVSDF4 ####

#
CA1981_clean = subset(
  CA1981,
  select = c(
    JUSTIFY_HOM,
    age,
    WT,
    male,
    edulevel,
    marital,
    GDPPC,
    CY,
    year,
    GINI,
    CultureZone,
    wave,
    FREESCORE,
    country,
    job,
    religion,
    relig_freq,
    NEIGHBOR_HOM,
    believe_god,
    IMPORTANT
  )
)
#
L1 = names(CA1990_clean)
L2 = names(wvsdf3)
setdiff(L1, intersect(L1, L2))
setdiff(L2, intersect(L1, L2))
L1 = names(wvsdf3_5)
L2 = names(CA1981_clean)
setdiff(L1, intersect(L1, L2))
setdiff(L2, intersect(L1, L2))
wvsdf3_5 = rbind(CA1990_clean, wvsdf3)
wvsdf4 = rbind(CA1981_clean, wvsdf3_5)
fivenum(wvsdf4$JUSTIFY_HOM)
#
wvsdf4$edulevel    = as.factor(as.character(wvsdf4$edulevel))
wvsdf4$marital     = as.factor(as.character(wvsdf4$marital))
wvsdf4$CY          = as.factor(as.character(wvsdf4$CY))
wvsdf4$year        = as.numeric(as.character(wvsdf4$year))
wvsdf4$wave        = as.factor(as.character(wvsdf4$wave))
wvsdf4$job         = as.factor(as.character(wvsdf4$job))
wvsdf4$country     = as.factor(as.character(wvsdf4$country))
wvsdf4$CultureZone = as.factor(as.character(wvsdf4$CultureZone))
wvsdf4$male        = as.factor(wvsdf4$male)
wvsdf4$religion    = as.factor(wvsdf4$religion)
wvsdf4$relig_freq  = as.factor(wvsdf4$relig_freq)
wvsdf4$age         = as.factor(as.character(wvsdf4$age))
#
dist_tab(wvsdf4$country)
dist_tab(wvsdf4$CY)
dist_tab(wvsdf4$NEIGHBOR_HOM)
wvsdf4$NEIGHBOR_HOM = as.factor(wvsdf4$NEIGHBOR_HOM)
sapply(wvsdf4, class)
names(wvsdf)
dist_tab(wvsdf$wave)
dist_tab(wvsdf$country)
dist_tab(wvsdf$CY)
dist_tab(wvsdf$CW)
dist_tab(wvsdf$year)
dist_tab(wvsdf$male)
dist_tab(wvsdf$age)
dist_tab(wvsdf$marital)
dist_tab(wvsdf$kids)
dist_tab(wvsdf$edulevel)
dist_tab(wvsdf$job)
dist_tab(wvsdf$JUSTIFY_HOM)
dist_tab(wvsdf$religion)
dist_tab(wvsdf$relig_freq)
dist_tab(wvsdf$WT)
dist_tab(wvsdf$NB_HOM)
dist_tab(wvsdf$believe_god)
dist_tab(wvsdf$IMPORTANT)
dist_tab(wvsdf$NEIGHBOR_HOM)

#### [2016.6.21] Amelia and Modeling ####
library(Amelia)
set.seed(1234)
myvars = c(
  "CY",
  "year",
  "wave",
  "CultureZone",
  "age",
  "FREESCORE",
  "country",
  "GDPPC",
  "GINI",
  "JUSTIFY_HOM",
  "edulevel",
  "marital",
  "job",
  "male",
  "religion",
  "relig_freq",
  "WT",
  "NEIGHBOR_HOM",
  "believe_god",
  "IMPORTANT"
)
a.out = amelia(
  wvsdf4[, myvars],
  m = 5,
  idvars = c(
    "CY",
    "year",
    "wave",
    "CultureZone",
    "FREESCORE",
    "country",
    "GDPPC",
    "GINI",
    "JUSTIFY_HOM",
    "WT",
    "NEIGHBOR_HOM",
    "believe_god",
    "IMPORTANT"
  ),
  noms = c("edulevel", "marital", "job", "male",  "religion", "relig_freq"),
  max.resample = 10,
  tolerance = 0.001,
  incheck = F
)
wvsdf10 = na.exclude(a.out$imputations$imp1)
dist_tab(wvsdf10$religion)
list(unique(as.character(wvsdf10$country)))

# 2018.11.04 NB#
myvars = c(
  "CY",
  "year",
  "wave",
  "CultureZone",
  "FREESCORE",
  "country",
  "GDPPC",
  "GINI",
  "age",
  "JUSTIFY_HOM",
  "edulevel",
  "marital",
  "job",
  "male",
  "religion",
  "relig_freq"
)
a.out = amelia(
  wvsdf4[, myvars],
  m = 5,
  idvars = c(
    "CY",
    "year",
    "wave",
    "CultureZone",
    "FREESCORE",
    "country",
    "GDPPC",
    "GINI",
    "JUSTIFY_HOM"
  ),
  noms = c("edulevel", "marital", "job", "male", "religion", "relig_freq"),
  #max.resample = 10,
  tolerance = 0.0002,
  incheck = F
)
wvsdf9 = na.exclude(a.out$imputations$imp1)
dist_tab(wvsdf9$religion)
dist_tab(wvsdf9$age)

wvsdf9$age = as.numeric(as.character(wvsdf9$age))
wvsdf10$age = as.numeric(as.character(wvsdf10$age))

#### Table 1 summary - 2016.6.19 Descriptives ####
L1 = unique(as.character(wvsdf4$country)) # L1
L2 = unique(as.character(wvsdf9$country)) # L2
intersect(L1, L2)
setdiff(L1, intersect(L1, L2))
setdiff(L2, intersect(L1, L2))

sapply(wvsdf9, dist_tab)
mean(wvsdf9$GDPPC)
SD(wvsdf9$GDPPC)
mean(wvsdf9$JUSTIFY_HOM, na.rm = T)
mean(wvsdf9$age)
SD(wvsdf9$age)
dist_tab(wvsdf9$job)
fivenum(wvsdf9$WT)

#
rm(list = setdiff(ls(), c(
  "WVSdata", "wvsdf", "wvsdf4",
  "wvsdf9", "wvsdf10"
)))

#### 2018.11.6 Reviewer 2: F050 God believe, A006 Important####
#
Countrymean_1 = wvsdf10 %>%
  group_by(CY) %>%
  summarise_at(vars(believe_god),
               funs(mean(., na.rm = TRUE)))
Countrymean_2 = wvsdf10 %>%
  group_by(CY) %>%
  summarise_at(vars(IMPORTANT),
               funs(mean(., na.rm = TRUE)))
#
Countrymean_3 = merge(Countrymean_1, Countrymean_2, by = "CY")
#
names(Countrymean_3)
names(Countrymean_3) = c("CY", "CY_GOD", "CY_IMP")
wvsdf11 = merge(Countrymean_3, wvsdf10, by = "CY")
rm(list = setdiff(
  ls(),
  c("WVSdata", "wvsdf", "wvsdf4",
    "wvsdf9", "wvsdf10", "wvsdf11")
))
names(wvsdf11)

#### 2018.3.14/2018.11.7 [CYdata] ####

CYdata_2 = aggregate(GDPPC ~ CY, data = wvsdf9, FUN = mean)
CYdata_3 = aggregate(GINI ~ CY, data = wvsdf9, FUN = mean)
CYdata_4 = aggregate(CultureZone ~ CY, data = wvsdf9, FUN = unique)
CYdata_5 = aggregate(FREESCORE ~ CY, data = wvsdf9, FUN = mean)
CYdata_6 = merge(CYdata_2, CYdata_3, all.x = T)
CYdata_7 = merge(CYdata_6, CYdata_4, all.x = T)
CYdata_8 = merge(CYdata_7, CYdata_5, all.x = T)
CYdata_8
rm(CYdata_2, CYdata_3, CYdata_4, CYdata_5, CYdata_6, CYdata_7)

#
table(wvsdf$wave, wvsdf$year)
CYdata_8$year = substr(as.character(CYdata_8$CY),
                       nchar(as.character(CYdata_8$CY)) - 3,
                       nchar(as.character(CYdata_8$CY)))
#
fivenum(CYdata_8$GDPPC)
fivenum(CYdata_8$GINI)
fivenum(CYdata_8$FREESCORE)
mean(CYdata_8$GDPPC)
mean(CYdata_8$GINI)
mean(CYdata_8$FREESCORE)
sd(CYdata_8$GDPPC)
sd(CYdata_8$GINI)
sd(CYdata_8$FREESCORE)
dist_tab(CYdata_8$CultureZone)

#### [Figure 1] 2016.7.6 ggplot2 effectplot####

#
theme_WVS = theme_grey() +
  theme(
    plot.title = element_text(
      lineheight = 2,
      hjust = .5,
      vjust = 1.5
    ),
    title = element_text(size = rel(1.5), family = "Century"),
    axis.text.x = element_text(size = rel(1.5), family = "Century"),
    axis.text.y = element_text(size = rel(1.5), family = "Century"),
    legend.text = element_text(size = rel(1.3), family = "Century"),
    legend.direction = "vertical",
    legend.key = element_rect(size = 1.3, color = 'white'),
    legend.position = c(0.19, 0.88),
    legend.key.size = unit(0.6, "cm")
  )

#### 2018.3.13 interaction/AIC/BIC ####

m1 = lmer(
  JUSTIFY_HOM ~ wave + male + marital
  + age + religion + relig_freq + job + edulevel + FREESCORE
  + (1 | country) + (1 | CY),
  REML = T,
  data = wvsdf9
)

m2 = lmer(
  JUSTIFY_HOM ~ wave + male + marital
  + age + religion + relig_freq + job + edulevel * FREESCORE
  + (1 | country) + (1 | CY),
  REML = T,
  data = wvsdf9
)

m3 = lmer(
  JUSTIFY_HOM ~ wave + male + marital
  + age + religion + relig_freq + job + edulevel * FREESCORE
  + log(GDPPC) + GINI + CultureZone
  + (1 | country) + (edulevel | CY),
  REML = T,
  data = wvsdf9
)
eff2 = effect(
  'edulevel* FREESCORE',
  mod = m3,
  x.var = "edulevel",
  confidence.level = 0.95,
  xlevels = list(
    edulevel = c(
      "0.None",
      "1.Elementary",
      "2.Secondary",
      "3.High_Sch",
      "4.College&Above"
    ),
    FREESCORE = c(1, 4, 7)
  )
)
x = as.data.frame(eff2)
x

ggplot(x, aes(edulevel, fit, group = factor(FREESCORE))) +
  labs(y = 'Tolerance of homosexuality',
       x = 'Level of education') +
  # geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.12) +
  geom_line(aes(size = 1, linetype = factor(FREESCORE))) +
  geom_point(aes(size = 1.2, shape = factor(FREESCORE))) +
  scale_shape_manual(
    values = c(18, 3, 2),
    name = "Freedom House Index",
    labels = c("1=freest", "4=mid-level freedom", "7=least free")
  )  +
  scale_linetype_manual(
    values = c("dotdash", "solid", "dotted"),
    name = "Freedom House Index",
    labels = c("1=freest", "4=mid-level freedom", "7=least free")
  )  +
  scale_x_discrete(labels = c(
    "No or little",
    "Elementary",
    "Secondary",
    "High school",
    "College and above"
  )) +
  guides(
    size = "none",
    linetype = guide_legend(override.aes = list(size = 1.04)),
    shape = guide_legend(override.aes = list(size = 1.15))
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(
      lineheight = 2,
      hjust = .5,
      vjust = 1.5
    ),
    title = element_text(size = rel(1.5), family = "Century"),
    axis.text.x = element_text(size = rel(1.5), family = "Century"),
    axis.text.y = element_text(size = rel(1.5), family = "Century"),
    legend.direction = "vertical",
    legend.position = c(0.22, 0.82),
    legend.title = element_text(
      size = 14,
      lineheight = 1,
      hjust = .5,
      vjust = 1.5
    ),
    legend.text = element_text(size = rel(1.4), family = "Century"),
    legend.key = element_rect(size = 4, color = 'white'),
    legend.key.size = unit(1.29, "cm")
  )

#### 2018.7.26 descriptive table by country ####

#
Countrymean_tol  = wvsdf9 %>%
  group_by(country) %>%
  summarise_at(vars(JUSTIFY_HOM),
               funs(mean(., na.rm = TRUE)))
#
Countrymean_free = wvsdf9 %>%
  group_by(country) %>%
  summarise_at(vars(FREESCORE),
               funs(mean(., na.rm = TRUE)))

Countrymean = merge(Countrymean_free, Countrymean_tol, by = "country")
scatterplot(Countrymean$JUSTIFY_HOM, 8 - Countrymean$FREESCORE)
scatterplot(Countrymean$JUSTIFY_HOM, log(8 - Countrymean$FREESCORE))
scatterplot(log(Countrymean$JUSTIFY_HOM), 8 - Countrymean$FREESCORE)
dist_tab(Countrymean$country)
Countrymean$country = as.character(Countrymean$country)
Countrymean$country = ifelse(
  Countrymean$country == "GreatBritain",
  "UnitedKingdom",
  as.character(Countrymean$country)
)

#
library(ggrepel)
ggplot(Countrymean) +
  geom_point(aes(8 - Countrymean$FREESCORE, JUSTIFY_HOM)) +
  scale_x_continuous(
    name = "Freedom",
    limits = c(0.99, 7.2),
    breaks = seq(1, 7, 1),
    labels = c("least free", "6", "5", "4", "3", "2", "freest")
  ) +
  scale_y_continuous(name = "Tolerance",
                     limits = c(0, 10),
                     breaks = seq(0, 10, 2)) +
  geom_text_repel(aes(8 - Countrymean$FREESCORE, JUSTIFY_HOM, label = country),
                  segment.alpha = 0.2) +
  theme_bw() +
  theme(
    plot.title = element_text(
      lineheight = 2,
      hjust = .5,
      vjust = 1.5
    ),
    title = element_text(size = rel(1.5), family = "Century"),
    axis.text.x = element_text(size = rel(1.5), family = "Century"),
    axis.text.y = element_text(size = rel(1.5), family = "Century"),
    legend.text = element_text(size = rel(1.3), family = "Century"),
    legend.direction = "vertical",
    legend.key = element_rect(size = 1.3, color = 'white'),
    legend.position = c(0.19, 0.88),
    legend.key.size = unit(0.6, "cm")
  )
#
NBm3b = glmer(
  as.numeric(NEIGHBOR_HOM) - 1 ~ male + marital
  + religion + relig_freq + job + edulevel * FREESCORE
  + log(GDPPC) + GINI
  + (1 | country) + (1 | CY),
  family = binomial,
  control = glmerControl(optCtrl = list(maxfun = 5),
                         boundary.tol = 1e-2),
  data = wvsdf11
)

#### 2018.11.8 [final publication plot generating codes] ####
#
windowsFonts()
windowsFonts(Arial = windowsFont("TT Arial"))
windowsFonts(Century = windowsFont("TT Century"))
windowsFonts(Times = windowsFont("TT Times New Roman"))
windowsFonts(Centaur = windowsFont("TT Centaur"))
windowsFonts(Palatino = windowsFont("TT Palatino"))
windowsFonts(Garamond = windowsFont("TT Garamond"))
library(ggplot2)
library(ggthemes)
library(ggeffects)

#
jpeg(
  "d:/SOCF-figure01.jpg",
  height = 28,
  width = 30,
  res = 600,
  units = 'cm'
)
ggplot(Countrymean) +
  geom_point(aes(8 - Countrymean$FREESCORE, JUSTIFY_HOM)) +
  scale_x_continuous(
    name = "Freedom",
    limits = c(0.99, 7.2),
    breaks = seq(1, 7, 1),
    labels = c("least free", "6", "5", "4", "3", "2", "freest")
  ) +
  scale_y_continuous(name = "Tolerance",
                     limits = c(0.5, 9.5),
                     breaks = seq(1, 9, 2)) +
  geom_text_repel(aes(8 - Countrymean$FREESCORE, JUSTIFY_HOM, label = country),
                  segment.alpha = 0.2) +
  theme_bw() +
  theme(
    plot.title = element_text(
      lineheight = 2,
      hjust = .5,
      vjust = 1.5
    ),
    title = element_text(size = rel(1.5), family = "Century"),
    axis.text.x = element_text(size = rel(1.5), family = "Century"),
    axis.text.y = element_text(size = rel(1.5), family = "Century"),
    legend.text = element_text(size = rel(1.3), family = "Century"),
    legend.direction = "vertical",
    legend.key = element_rect(size = 1.3, color = 'white'),
    legend.position = c(0.19, 0.88),
    legend.key.size = unit(0.6, "cm")
  )
dev.off()

jpeg(
  "d:/SOCF-figure02.jpg",
  height = 28,
  width = 30,
  res = 600,
  units = 'cm'
)
eff2 = effect(
  'edulevel * FREESCORE',
  mod = m3,
  x.var = "edulevel",
  confidence.level = 0.95,
  xlevels = list(
    edulevel = c(
      "0.None",
      "1.Elementary",
      "2.Secondary",
      "3.High_Sch",
      "4.College&Above"
    ),
    FREESCORE = c(1, 4, 7)
  )
)
x = as.data.frame(eff2)
x

# note 1, + has to be at the end of each line.
ggplot(x, aes(edulevel, fit, group = factor(FREESCORE))) +
  labs(y = 'Tolerance of homosexuality',
       x = 'Level of education') +
  geom_line(aes(size = 1, linetype = factor(FREESCORE))) +
  geom_point(aes(size = 1.2, shape = factor(FREESCORE))) +
  scale_shape_manual(
    values = c(18, 3, 2),
    name = "Freedom House Index",
    labels = c("1=freest", "4=mid-level freedom", "7=least free")
  )  +
  scale_linetype_manual(
    values = c("dotdash", "solid", "dotted"),
    name = "Freedom House Index",
    labels = c("1=freest", "4=mid-level freedom", "7=least free")
  )  +
  scale_x_discrete(labels = c(
    "No or little",
    "Elementary",
    "Secondary",
    "High school",
    "College and above"
  )) +
  guides(
    size = "none",
    linetype = guide_legend(override.aes = list(size = 1.04)),
    shape = guide_legend(override.aes = list(size = 1.15))
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(
      lineheight = 2,
      hjust = .5,
      vjust = 1.5
    ),
    title = element_text(size = rel(1.5), family = "Century"),
    axis.text.x = element_text(size = rel(1.5), family = "Century"),
    axis.text.y = element_text(size = rel(1.5), family = "Century"),
    legend.direction = "vertical",
    legend.position = c(0.22, 0.82),
    legend.title = element_text(
      size = 14,
      lineheight = 1,
      hjust = .5,
      vjust = 1.5
    ),
    legend.text = element_text(size = rel(1.4), family = "Century"),
    legend.key = element_rect(size = 4, color = 'white'),
    legend.key.size = unit(1.29, "cm")
  )
dev.off()

#
jpeg(
  "d:/SOCF-figure03.jpg",
  height = 28,
  width = 30,
  res = 600,
  units = 'cm'
)

eff2 = effect(
  'edulevel* FREESCORE',
  mod = NBm3b,
  x.var = "edulevel",
  confidence.level = 0.95,
  xlevels = list(
    edulevel = c(
      "0.None",
      "1.Elementary",
      "2.Secondary",
      "3.High_Sch",
      "4.College&Above"
    ),
    FREESCORE = c(1, 4, 7)
  )
)
x = as.data.frame(eff2)
x

#
ggplot(x, aes(edulevel, fit, group = factor(FREESCORE))) +
  labs(y = 'Tolerance of homosexuality',
       x = 'Level of education') +
  geom_line(aes(size = 1, linetype = factor(FREESCORE))) +
  geom_point(aes(size = 1.2, shape = factor(FREESCORE))) +
  scale_shape_manual(
    values = c(18, 3, 2),
    name = "Freedom House Index",
    labels = c("1=freest", "4=mid-level freedom", "7=least free")
  )  +
  scale_linetype_manual(
    values = c("dotdash", "solid", "dotted"),
    name = "Freedom House Index",
    labels = c("1=freest", "4=mid-level freedom", "7=least free")
  )  +
  scale_x_discrete(labels = c(
    "No or little",
    "Elementary",
    "Secondary",
    "High school",
    "College and above"
  )) +
  guides(
    size = "none",
    linetype = guide_legend(override.aes = list(size = 1.04)),
    shape = guide_legend(override.aes = list(size = 1.15))
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(
      lineheight = 2,
      hjust = .5,
      vjust = 1.5
    ),
    title = element_text(size = rel(1.5), family = "Century"),
    axis.text.x = element_text(size = rel(1.5), family = "Century"),
    axis.text.y = element_text(size = rel(1.5), family = "Century"),
    legend.direction = "vertical",
    legend.position = c(0.22, 0.82),
    legend.title = element_text(
      size = 14,
      lineheight = 1,
      hjust = .5,
      vjust = 1.5
    ),
    legend.text = element_text(size = rel(1.4), family = "Century"),
    legend.key = element_rect(size = 4, color = 'white'),
    legend.key.size = unit(1.29, "cm")
  )
dev.off()
