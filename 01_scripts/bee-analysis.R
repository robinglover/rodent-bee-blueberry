######SETUP#######

#load packages
library(ggplot2) #for plots
library(dplyr) #for working with dataframes
library(tidyr) #for working with datasets
library(lme4) #for mixed effects models
library(ggpubr) #for adding significance asterisks to graph

#read csv
bee <- read.csv("00_rawdata/bee-data.csv") #bee data
site_data <- read.csv("00_rawdata/site-info.csv") #site data

#add column to round data for site type (either grass or bare) by joining 
#rodent dataframe to site dataframe
bee1 <- bee %>%
  left_join(site_data, by = c("site_id" = "name")) %>%
  rename (field.type = grass) #column listing field condition (either bare or 
#grass)  to "field.type" 

#add new column for location (field for p1 and p2 or ditch)
bee1 <- mutate(bee1, location = if_else(plot_id == "p1" | plot_id == "p2",
                                                  "field", 
                                                  if_else(plot_id == "ditch", 
                                                          "ditch", 
                                                          NA)))

#####FILTERED DATAFRAMES####

#filter to bees foraging
bees_foraging <- filter(bee1, behaviour == "foraging")

#filter to bees foraging on blueberry
bees_foraging_vaco <- filter(bee1, behaviour == "foraging" & flower_code == "vaco")

#filter to bees nest searching
bees_nestsearching <- filter(bee1, behaviour == "nest_searching")

#filter to bees flying
bees_flying <- filter(bee1, behaviour == "flying")

#####Influence of field type####

#plot number of bees found in each field type
ggplot(bee1, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (all behaviours)")

#plot number of bee found in each field type, grouped by area
ggplot(bee1, aes(x=area, fill = field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Area")+
  ylab("Bee abundance (all behaviours)")

#plot number of bees found in each field type, grouped by behaviour
ggplot(bee1, aes(fill=field.type, x=behaviour))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Behaviour")+
  ylab("Bee abundance")

#BOX PLOT: number of bees found in each field type, grouped by behaviour
ggplot(bee2, aes(fill=field.type, x=behaviour, y = n))+ 
  geom_boxplot() +
  theme_classic(base_size = 15)+
  xlab("Behaviour")+
  ylab("Bee abundance")

##stats:
#create datasheet with transect code, round #, web type, and count number of each web type
bee2 <- bee1 %>% count(site_id, round, plot_id, sample_code, behaviour, field.type)
View(bee2)

#set field.type as a factor
bee2$field.type <- factor(bee2$field.type, levels = c("bare", "grass"))

# Change "NS" to "nest-searching" in the 'round' column
bee2 <- bee2 %>%
  mutate(round = recode(round, "NS" = "nest-searching"))

#run anova on mixed linear model
anova.behaviour <- aov(n ~ field.type + behaviour, data = bee2)
summary(anova.behaviour)

#run tukey test

TukeyHSD(anova.behaviour)

#run 3 separate t-tests for each web type
##flying
onlyFlying <- filter(bee2, behaviour == "flying")
t.test(n ~ field.type, data = onlyFlying, var.equal = FALSE)

##foraging
onlyForaging <- filter(bee2, behaviour == "foraging")
t.test(n ~ field.type, data = onlyForaging, var.equal = FALSE)

##nest_searching
onlyNestSearching <- filter(bee2, behaviour == "nest_searching")
t.test(n ~ field.type, data = onlyNestSearching, var.equal = FALSE)

##resting
onlyResting <- filter(bee2, behaviour == "resting")
t.test(n ~ field.type, data = onlyResting, var.equal = FALSE)

#plot number of bees found in each field type, grouped by round
ggplot(bee1, aes(fill=field.type, x=factor(round, levels=c("NS", "1", "2"))))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Round")+
  ylab("Bee abundance")

#BOX PLOT: plot number of bees found in each field type, grouped by round
ggplot(bee2, aes(fill=field.type, x=factor(round, levels=c("nest-searching", "1", "2")), y = n))+ 
  geom_boxplot() +
  theme_classic(base_size = 12)+
  xlab("Round")+
  ylab("Bee abundance") +
  scale_fill_manual(values = c("bare" = "chocolate4", "grass" = "darkgreen")) +
  stat_compare_means(
    aes(group = field.type),  # group by field.type
    method = "t.test",  # or "wilcox.test" if you're doing a non-parametric test
    label = "p.signif",  # Display p-value or asterisks (significance stars)
    size = 7  # Adjust this value to make the asterisks larger
    )

##stats:

####run anova on mixed linear model
a <- aov(n ~ field.type + round, data = bee2)
summary(a)

####run tukey test

TukeyHSD(a)

####run 3 separate t-tests for each web type
#######NS
onlyNS <- filter(bee2, round == "NS")
t.test(n ~ field.type, data = onlyNS, var.equal = FALSE)

#######1
onlyR1 <- filter(bee2, round == "1")
t.test(n ~ field.type, data = onlyR1, var.equal = FALSE)

#######2
onlyR2 <- filter(bee2, round == "2")
t.test(n ~ field.type, data = onlyR2, var.equal = FALSE)

#plot number of bees (foraging on blueberry only) found in each field type
ggplot(bees_foraging_vaco, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (foraging on blueberry only)")

####behaviour-specific analyses#####

#plot number of bees (nest searching only) found in each field type
ggplot(bees_nestsearching, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (nest searching only)")

#plot number of bees (nest searching only) found at each plot (p1, p2, d)
ggplot(bees_nestsearching, aes(x=plot_id))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Plot Type")+
  ylab("Bee abundance (nest searching only)")

#plot number of bees found at each plot (p1, p2, d)
ggplot(bee1, aes(x=plot_id))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Plot Type")+
  ylab("Bee abundance (all behaviours)")

#plot number of bees (flying only) found in each field type
ggplot(bees_flying, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (flying only)")






