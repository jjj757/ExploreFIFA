
library(readxl)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(readcsv)
library(forcats)
###################################
#DATA COLLECTION, CLEANING, and Manipulation section
###################################


FIFA21 <- read_excel("/Users/jeremymassaquoi/Data project/FIFA/FIFA21.xlsx")

str(FIFA21)


FIFA21_var <- FIFA21%>%
  select(short_name, long_name, age, height_cm, weight_kg, nationality, club_name, league_name, 
         league_rank, overall, preferred_foot, team_position, team_jersey_number, pace)
#adding countries and GDP data

continents <- read_excel("/Users/jeremymassaquoi/Data project/FIFA/continents2 2.xlsx") ##continents csv from Kaggle

GDP <- read_excel("/Users/jeremymassaquoi/Data project/FIFA/API_NY.GDP.MKTP.CD_DS2_en_excel_v2_3605637.xlsx") ##from the World Bank


continent_var <- continents%>%
  select(name, `alpha-3`, region, `sub-region`)

GDP_var <- GDP%>%
  select(`Country Name`, `Country Code`, `2019`)#out of millions

#checking for NAs 

apply(FIFA21_var, 2, function(x) any(is.na(x)))
#club_name, league_name, league_rank, team_position, team_jersey_number
#fine
#some players may be national team only. some players may be classified as a bench player

apply(continent_var, 2, function(x) any(is.na(x)))
#sub-region, region
#not fine
#check 
b <- continent_var%>%
  filter(is.na(continent_var$region))
#Antarctica; so fine. drop
c <- continent_var%>%
  filter(is.na(continent_var$`sub-region`))
#Antarctica; so fine. drop

#dropping
continent_var <- continent_var%>%
  filter(!is.na(continent_var$region)|!is.na(continent_var$`sub-region`))
apply(continent_var, 2, function(x) any(is.na(x)))#recheck
  

apply(GDP_var, 2, function(x) any(is.na(x)))
#2019 
#not fine
#check
d<- GDP_var%>%
  filter(is.na(GDP_var$`2019`))
#many countries on here are not reporting do to various reason 
#example: North Korea will not report due to obvious reasons, South Sudan is a newer country
#many of other countries are smaller country. We do not want any NAs but many have NAs spanning decades
#We will just have to settle with assigning NAs to said countries  

###############
#JOIN
###############

#joining nationality on country name
join<-FIFA21_var%>%
  left_join(continent_var, by = c("nationality"= "name"))

join2<- join%>%
  left_join(GDP_var, by = c("alpha-3" = "Country Code"))
#check for NA's
apply(join2, 2, function(x) any(is.na(x)))# there are NA's on region, alpha-3, sub-region and country name,2019
#not fine
#check
e<- join2%>%
  filter(is.na(join2$`Country Name`))%>%
  group_by(nationality)
f<- join2%>%
  filter(is.na(join2$`Country Name`))%>%
  group_by(nationality)%>%
  count(nationality,sort= TRUE)%>%
  print()
#England, China PR, Korea Republic, Republic of Ireland , Scotland,  Wales, Ivory Coast, Northern Ireland,
#Bosnia Herzegovina, DR Congo,Kosovo, North Macedonia, Cape Verde, Curacao, Trinidad & Tobago, Antigua & Barbuda,
#Montserrat, Chinese Taipei, Palestine, Korea DPR, Macau, S√£o Tom√© & Pr√≠ncipe 

#England = United Kingdom (will add England to Continent) #A
#China PR =China
#Korea Republic = South Korea
#Republic of Ireland = Ireland (change ROI to Ireland)
#Scotland = part of United Kingdom (will add scotland to continent)#A
#Wales = Kingdom (will add Wales to con)#A
#Ivory Coast = Côte D'Ivoire
#Northern Ireland = part of UK (will add to continent)#A
#Bosnia  Herzegovina = Bosnia And Herzegovina
#DR Congo =Congo, Dem. Rep.
#kosovo = add Kosovo in continents #A
#North Macedonia = Macedonia
#cape varde =  Cabo Verde
#Curacao = Curaçao
#trinidad & tobago = Trinidad and Tobago
#Antigua & Barbuda = Antigua and Barbuda
#Montserrat =Montserrat
#Chinese Taipei = Taiwan #will have to be add to China for political reason
#Palestine = does not exist in GDP or continent data. Only two players; add in continents #A
#Korea DPR = Korea, Republic of = North Korea
#Macau = Macao (considered China too?will keep seperate)
#S√£o Tom√© & Pr√≠ncipe = São Tomé and Príncipe = Sao Tome and Principe

#Creating Data Frame of multiple variable/column using Vector  

#New Country     
Df.NewCountries <- data.frame(c("England", "Scotland", "Wales", "Northern Ireland",  "Kosovo", "Palestine"),
                                    c("GBR","GBR","GBR","GBR", "XKX", NA),
                                    c("Europe","Europe","Europe","Europe","Europe", "Asia"),
                                      c("Western Europe", "Western Europe", "Western Europe", "Western Europe", "Southern Europe", "Western Asia"))                                 

#Naming the above Data Frame                               
names(Df.NewCountries) <- c("name","alpha-3", "region", "sub-region")
?names

#Adding observations using rbind() function  
continent_varfinal <- rbind(continent_var, Df.NewCountries)

#renaming
FIFA21_var[FIFA21_var == "China PR"] <- "China"
FIFA21_var[FIFA21_var == "Korea Republic"] <- "South Korea"
FIFA21_var[FIFA21_var == "Republic of Ireland"] <- "Ireland"
FIFA21_var[FIFA21_var == "Ivory Coast"] <- "Côte D'Ivoire"
FIFA21_var[FIFA21_var == "DR Congo"]<- "Congo (Democratic Republic Of The)"
FIFA21_var[FIFA21_var == "North Macedonia"] <- "Macedonia"

FIFA21_var[FIFA21_var == "Trinidad & Tobago"] <- "Trinidad and Tobago"
FIFA21_var[FIFA21_var == "Antigua & Barbuda"] <- "Antigua and Barbuda"
FIFA21_var[FIFA21_var == "Chinese Taipei"] <- "China" #will have to be add to China for political reason
FIFA21_var[FIFA21_var == "Korea DPR"] <- "Korea, Republic of"
FIFA21_var[FIFA21_var == "Macau"] <- "Macao"
FIFA21_var[FIFA21_var == "S√£o Tom√© & Pr√≠ncipe"] <- "Sao Tome and Principe"
FIFA21_var[FIFA21_var == "Bosnia Herzegovina"] <- "Bosnia And Herzegovina"
FIFA21_var[FIFA21_var == "Curacao"] <- "Curaçao"
FIFA21_var[FIFA21_var == "Cape Verde"] <- "Cabo Verde"

#####
#redo join
#####
rejoin<-FIFA21_var%>%
  left_join(continent_varfinal, by = c("nationality"= "name"))
rejoin2<- rejoin%>%
  left_join(GDP_var, by = c("alpha-3" = "Country Code"))
apply(rejoin2, 2, function(x) any(is.na(x)))
#confirming there are no NAs for regions and etc
#NA in Country Name= fine
#NA in 2019 GDP = fine
#NA in alpha-3 (palestine) = fine
#good
g<- rejoin2%>%
  filter(is.na(rejoin2$`region`))%>%
  group_by(nationality)
h<- rejoin2%>%
  filter(is.na(rejoin2$`region`))%>%
  group_by(nationality)%>%
  count(nationality,sort= TRUE)%>%
  print()

FIFA21_clean<-rejoin2





###############################
#some fun analysis before exporting 
###############################


##########players 

#top 10 player by rating,overall
TOP10 <- FIFA21_clean %>%
  arrange(desc(overall))%>%
  head(10)


#top 100 player; group by nationality
TOP100 <- FIFA21_clean%>%
  arrange(desc(overall))%>%
  head(100)


#By League
#premier league
TOP_premier<- FIFA21_clean%>%
  filter(league_name == "English Premier League")%>%
  arrange(desc(overall))%>%
    head(1)
#top german league
TOP_bund<- FIFA21_clean%>%
  filter(league_name == "German 1. Bundesliga")%>%
  arrange(desc(overall))%>%
  head(1)
#top ligue 1
TOP_ligue1<- FIFA21_clean%>%
  filter(league_name == "French Ligue 1")%>%
  arrange(desc(overall))%>%
  head(1)
#top la liga player
TOP_laliga<- FIFA21_clean%>%
  filter(league_name == "Spain Primera Division")%>%
  arrange(desc(overall))%>%
  head(1)
#top serie a player
TOP_seriea<- FIFA21_clean%>%
  filter(league_name == "Italian Serie A")%>%
  arrange(desc(overall))%>%
  head(1)
 #bonus top MLS  
TOP_MLS<- FIFA21_clean%>%
  filter(league_name == "USA Major League Soccer")%>%
  arrange(desc(overall))%>%
  head(1)




#########
#by region

national <- TOP100 %>%
  group_by(nationality)%>%
 count(nationality, sort = TRUE)

national500<- FIFA21_clean%>%
  arrange(desc(overall))%>%
  head(500)%>%
  count(nationality, sort = TRUE)
  

#by league

league <- TOP100%>%
  group_by(league_name)%>%
  count(league_name, sort = TRUE)

league500 <- FIFA21_clean%>%
  arrange(desc(overall))%>%
  head(500)%>%
  count(league_name, sort = TRUE)


  
ggplot(national, aes(x= n, y = nationality))+
  geom_col()+
  theme_update()+
  labs(title = "Countries with a TOP 100 player")+
  xlab("amount of players")


############
#age
###########

#top 100 player under 23; group by nationality
TOP100YNG <- FIFA21_clean%>%
  arrange(desc(overall))%>%
  filter(age <= 23)%>%
  head(100)%>%
  group_by(nationality)%>%
  count(nationality,sort = TRUE)


TOP100YNGnames <- FIFA21_clean%>%
  arrange(desc(overall))%>%
  filter(age <= 23)%>%
  head(100)
  


TOP100YNG%>%
  mutate(nationality = as.factor(nationality))%>%
  ggplot(aes(x= n , y= fct_reorder(nationality,n)))+
  geom_col()+
  theme_update()+
  labs(title = "Countries with a TOP 50 young player")+
  xlab("amount of players")

TOP100YNG <- FIFA21_clean%>%
  arrange(desc(overall))%>%
  filter(age <= 23)%>%
  head(100)%>%
  group_by(nationality)%>%
  count(nationality,sort = TRUE)





#top subs
TOP100SUB<-FIFA21_clean%>%
  select(overall, short_name, long_name, team_position, club_name, nationality, age)%>%
  arrange(desc(overall))%>%
  filter(team_position == "SUB")%>%
  head(100)



###############
#position
################

position_rank<- FIFA21_clean%>%
  group_by(team_position)%>%
  arrange(desc(overall))%>%
  summarize(mean(overall))


height_rank <- FIFA21_clean%>%
  group_by(height_cm)%>%
  summarize(overall = mean(overall))%>%
  arrange(desc(overall))
weight_rank<- FIFA21_clean%>%
  group_by(weight_kg)%>%
  summarize(overall = mean(overall))%>%
  arrange(desc(overall))  
#
speed_rank <- FIFA21_clean%>%
  group_by(pace)%>%
  summarize(overall= mean(overall))%>%
  arrange(desc(overall))


###
GDP_rank <- FIFA21_clean%>%
  group_by(`2019`)%>%
  summarize(overall = mean(overall))%>%
  arrange(desc(overall))

ggplot(GDP_rank, aes(x= `2019`,y =overall))+
  geom_point()
cor(GDP_rank$`2019`, GDP_rank$overall)

Countriesss<- FIFA21_clean%>%
  group_by(nationality)%>%
  summarize(overall = mean(overall))%>%
  arrange(desc(overall))
  
Countriesss2<- FIFA21_clean%>%
  group_by(nationality)%>%
  summarize(overall = quantile(overall, probs=0.95, na.rm=TRUE))%>%
  arrange(desc(overall))

############
foot<- FIFA21_clean%>%
  filter(team_position != 'GK')%>%
  group_by(preferred_foot)%>%
  summarize(overall = quantile(overall, probs=0.95, na.rm=TRUE))%>%
  arrange(desc(overall))
###############

?filter
cor(FIFA21_clean$pace, FIFA21_clean$overall)



#for export

FIFAvis <- FIFA21_clean
write.csv(FIFAvis, file = '~/Desktop/CLEANFIFADATA.csv')


aa<-FIFA21%>%
  filter(grepl('Korea', nationality))%>%
  count(nationality)
