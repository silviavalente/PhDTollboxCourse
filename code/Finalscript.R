
# update di Rstudio. 
#With this computer I'm using R version 3.2.5, beacuse the newest version give me a fatal error(I'm using windows xp)
#Then I've modified the script on an other computer (because with xp rmarkdown doesn't work) and now I'm using R 3.3.3
# download GIT

# install packages 
install.packages("tidyverse")
library("tidyverse")

install.packages("dplyr")
library("dplyr")

install.packages("ggplot2")
library("ggplot2")


#I modified the excell file, creating a file csv called prova.csv
# Using read.csv I took the csv file, I used the function sep because the file has the ";" as separator

ecology <- read.csv("data/raw/ecologydata3.csv", sep = ";")

#I want to see the file, and change colnames
View(ecology)
ncol(ecology)
colnames(ecology)
coltochange <- c(3,4,5,6,7)
names(ecology)[coltochange] <- c("Time_of_day", "Temperature", "Plant_density", "Abundance_of_insects", "Taxonomic_units")
colnames(ecology)

#I want to see if the plant density influence the abundance of insects

ggplot(data = ecology)
provaplantdensity <- ggplot(data = ecology, aes(x = Plant_density, y = Abundance_of_insects)) 
provaplantdensity + geom_bar(aes(fill = Plant_density), stat = "identity", position = "dodge") + facet_grid(~Time_of_day) + theme_classic() +xlab("Plant density") + ylab("Abundance of insects")

#I want to understand also if the temperature influence the abundance of insects

ggplot(data = ecology)
provatemperature <- ggplot(data = ecology, aes(x = Temperature, y = Abundance_of_insects, group = Time_of_day, colour = Time_of_day)) 
provatemperature + geom_smooth() + theme_classic() +xlab("Temperatures") + ylab("Abundance of insects") 

#Finally I want to understand the influence of the time of day
#There are 210 replicates and 7 days, (30 replicates for each day,  15 for morning and 15 for night)
nrow(ecology)
table(ecology$Days)
#I need the mean of each sample, made of 15 replicates

#I try to do this for one day, I've seen in the manual it is easier to use slice than filter, because I can select both the day and the time of day

tbl_df(ecology)
day1n <- ecology %>%
  slice(1:15) %>% 
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

#I can't find how to repeat this function (I've seen there is the way with a loop, using repeat and {}, but this method doesn't work)
#so I paste and copy, changing the name and the number of row

day1m <- ecology %>%
  slice(16:30) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day2n <- ecology %>%
  slice(31:45) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day2m <- ecology %>%
  slice(46:60) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day3n <- ecology %>%
  slice(61:75) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day3m <- ecology %>%
  slice(76:90) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day4n <- ecology %>%
  slice(91:105) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day4m <- ecology %>%
  slice(106:120) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day5n <- ecology %>%
  slice(121:135) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day5m <- ecology %>%
  slice(136:150) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day6n <- ecology %>%
  slice(151:165) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day6m <- ecology %>%
  slice(166:180) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day7n <- ecology %>%
  slice(181:195) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

day7m <- ecology %>%
  slice(196:210) %>%
  mutate(mean_Abundance = mean(Abundance_of_insects)) %>%
  mutate(mean_Taxonomy_unit = mean(Taxonomic_units)) %>%
  select(Days, Time_of_day, mean_Abundance, mean_Taxonomy_unit) %>%
  slice(1)

#I want to bind all these raw
daytogheter <- bind_rows(day1n, day1m, day2n, day2m, day3n, day3m, day4n, day4m, day5n, day5m, day6n, day6m, day7n, day7m)
colnames(daytogheter)
View(daytogheter)

#I try to get two different plot, and to combine them
#I've seen there are some ways to combine plots, with the grid function

ggplot(data = daytogheter)
primaprova <- ggplot(data = daytogheter, aes(x = Days, y = mean_Abundance, group = Time_of_day, colour = Time_of_day))
one <- primaprova + geom_bar(stat = "identity", position = "dodge", aes(fill= Time_of_day)) + theme_classic() +xlab("Days") + ylab("Abundace of insects")


ggplot(data = daytogheter)
terzaprova <- ggplot(data = daytogheter, aes(x = Days, y = mean_Taxonomy_unit, group = Time_of_day, colour = Time_of_day))  
two <- terzaprova + geom_bar(stat = "identity", position = "dodge", aes(fill= Time_of_day)) + theme_classic() +xlab("Days") + ylab("Taxonomy units recongnizable") 

install.packages("gridExtra")
library("gridExtra")

grid.arrange(one, two, newpage = FALSE)

#At day 1-5-6 there are more insects and more kind of insects during the day, while in the other days we have the opposite situation.


