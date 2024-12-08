######SETUP#######

#load packages
library(ggplot2) #for plots
library(dplyr) #for working with dataframes
library(tidyr) #for working with datasets

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

#####PLOTS#####

##influence of field type

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

#plot number of bees found in each field type, grouped by round
ggplot(bee1, aes(fill=field.type, x=factor(round, level=c("NS", "1", "2"))))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Round")+
  ylab("Bee abundance")

#plot number of bees (foraging on blueberry only) found in each field type
ggplot(bees_foraging_vaco, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (foraging on blueberry only)")

##behaviour-specific analyses

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
  xlab("Field Type")+
  ylab("Bee abundance (nest searching only)")

#plot number of bees (flying only) found in each field type
ggplot(bees_flying, aes(x=field.type))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Field Type")+
  ylab("Bee abundance (flying only)")


###For farmers:

#plot number of bees found in each site
ggplot(bee1, aes(x = site_id))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Site")+
  ylab("Bee abundance")

#plot number of bees (nest searching only) found at each site
ggplot(bees_nestsearching, aes(x=site_id))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Site")+
  ylab("Bee abundance (nest searching only)")

#plot number of bees (nest searching only) found at each plot within each plot
ggplot(bees_nestsearching, aes(x=plot_id, fill=location))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic(base_size = 15)+
  xlab("Plot")+
  ylab("Bee abundance (nest searching only)")+
  facet_wrap(~site_id)

#plot number of bees found at each site, grouped by behaviour
ggplot(bee1, aes(x=behaviour, fill=plot_id))+ 
  geom_bar(position="dodge", stat="count") +
  theme_classic()+
  xlab("Plot")+
  ylab("Bee abundance (nest searching only)")+
  facet_wrap(~site_id)
