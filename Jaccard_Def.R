# ---- Chapter 1: installing packages ---- 
 # installing packages required 
install.packages("vegan")
install.packages("tidyverse")
install.packages("plot.matrix")
install.packages("stringr")
install.packages("gdm")
# ---- Chapter 2: recalling packages ----
library(vegan)
library(tidyverse)
library(readxl)
library(plot.matrix)

# ---- Reading the file ----

# taking the excel file with all the data needed and read it in R
tbl_aufnahme <- read_excel("tbl_aufnahme.xlsx")

# I will take away the harvest number 1 (September 1996) for the plot 41
# I am going to do this because at the beginning some plots were characterized
# by bare ground, these plots were 12, 29, 41, 61.
# For the plots 12, 29 and 61 the first 5 harvests were missing, since till
# September 1998 the plots were managed, so no species were allowed to be there
# The presence of a first harvest inplot 41 looks then like a typo in the data
# management, reason why I will not consider that line of the data
tbl_aufnahme <-tbl_aufnahme %>% 
  filter(!(id_harvest == 1 & id_plot == "B1P041"))



# warnings()
# View(tbl_aufnahme)
# str(tbl_aufnahme)
# dim(tbl_aufnahme)
# glimpse(tbl_aufnahme)
# names(tbl_aufnahme)


# we also read the table containing the information about the different harvests
tbl_harvest <- read_excel("tbl_harvest_period.xlsx")

# view(tbl_harvest)
# let's see which harvests are available
id_harvest <- tbl_harvest$id_harvest

# i want to divide the harvests in the spring ones and the autumn ones
# i want to add columns to the main table
# one column with the month and one column with the year
fall <- str_extract(tbl_harvest$harvest_Bezeichnung, "September")
spring <- str_extract(tbl_harvest$harvest_Bezeichnung, "Juni")
year <- str_extract(tbl_harvest$harvest_Bezeichnung, "\\d{4}")

month <- coalesce(fall, spring)

# i can create a data frame with all the information i need
# they are going to match since the order in which they were obtained is the same one
# so the one contained in the table about the harvest period

data_f <- data.frame(id_harvest,
                     month,
                     year)
# View(data_f)

# now i join the month and the year based on the id harvest and i add a species column
tbl_data <- tbl_aufnahme %>% 
  left_join(data_f, by = "id_harvest") %>% 
  mutate(species = 1) %>%
  arrange(id_harvest) %>% 
  arrange(id_plot)
  
# view(tbl_data)
harvest_numbers <- unique(tbl_data$id_harvest)

mutate(id_plot_numeric = as.numeric(as.factor(id_plot)))

# now i can see that some harvests are missing for all the plots: 7, 9, 11

#---- Species Count ----

# I want to see what is the species count trend over the harvests/years
# I will consider the sum of all the species present in each plot and then
# sum all the plots together
# This will mean that at the end in the final sum the species are not going
# to be unique
# This means that if a species is present both in plot 1 and 5, the final sum
# is going to be 2 and not 1
# I want to take the sum of all species for each harvest and plot it

tbl_data_sp <- tbl_data %>% 
  arrange(id_harvest) %>% 
  select(id_harvest,
         species)

harvest_numbers <- unique(tbl_data_sp$id_harvest)
species <- list()

for(i in harvest_numbers){
  species[[i]] <- tbl_data_sp[which(tbl_data_sp$id_harvest == i),]
}

# View(species)

species_sum <- list()

for(i in harvest_numbers){
  species_sum[[i]] <- species[[i]] %>% 
    count(species)
}

# View(species_sum)

# I notice that for harvests 7, 9 and 11 there is no data collected
# I am going to remove the undesired element of the list

# I need to remember then the order of the harvests is going to be different

species_sum_n <- species_sum[-c(7, 9, 11)]

# View(species_sum_n)

# I create a data frame with all the dates corresponding to the harvests
# I then leave an empty column for the sum of the number of the species

species_count <- data.frame(year = c(19960601, 19970601, 19970901, 19980601,
                                     19980901, 19990601, 20000601, 20010601,
                                     20020601, 20020901, 20030601, 20030901,
                                     20040601, 20040901, 20050601, 20050901,
                                     20060601, 20060901, 20070601, 20070901,
                                     20080601, 20080901, 20090601, 20090901,
                                     20100601, 20100901, 20110601, 20110901,
                                     20120601, 20120901, 20130601, 20130901,
                                     20140601, 20140901, 20150601, 20150901,
                                     20160601, 20160901, 20170601, 20170901,
                                     20180601, 20180901, 20190601, 20190901,
                                     20200601, 20200901, 20210601, 20210901,
                                     20220601, 20220901, 20230601, 20230901,
                                     20240601, 20240901),
                            n_species = NA)

# and I transform the data sequence into actual date format with lubridate
species_count$year <- ymd(species_count$year)

# I then proceed to transform my list into a data frame
# I will join the date column with the values of the total number of species
# present at each harvest for all the plots summed together

for(j in 1:nrow(species_count)){
    species_count$n_species[j] <- species_sum_n[[j]]$n
}

species_count

# max(species_count$n_species)

# I will plot it

ggplot(species_count,
       aes(year,
           n_species)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  labs(title = "Species count overall",
       x = "Year",
       y = "Number of Species")

# look on the notebook the comment on the graph

# now we just considered also the repetition of species in the plot
# let's see how it looks without recalling the same species which are 
# present in more than one plot

#---- Mean species number per plot ----

# Here I want to consider the mean of species present in each plot
# This means that the same species are not going

tbl_data_sp_plot <- tbl_data %>%
  select(id_harvest,
         id_plot,
         species) %>%
  arrange(id_plot)

harvest_numbers
species_mean <- list()

for(i in harvest_numbers){
  species_mean[[i]] <- tbl_data_sp_plot[which(tbl_data_sp_plot$id_harvest == i),]
}

# View(species_mean)

# I want now to consider every harvest
# I want then to group by plot, in order to have for each harvest the amount 
# of species present in each plot
species_mean_plot <- list()

for(i in harvest_numbers){
  species_mean_plot[[i]] <- species_mean[[i]] %>% 
      group_by(id_plot) %>% 
      count(species) %>% 
      rename(total = n) %>% 
      select(-species)
}

# View(species_mean_plot)

# I can see that for the first 5 harvests we have 60 plots available 
# instead of 64
# This is because of the fact that 4 plots were left empty for the first 2 years
# Plots 12, 29, 41, 61 are in fact empty during the first 5 harvests 

# I now want to calculate the mean for each harvest of the amount of species
# present in every single plot
# I then want to extract the values from the list and have them in a vector

harvest_species_mean <- vector()

for(i in harvest_numbers){
  harvest_species_mean[i] <- species_mean_plot[[i]] %>%
    pull(total) %>% 
    mean(na.rm = TRUE)
}

# View(harvest_species_mean)
# str(harvest_species_mean)

# I want to calculate the standard error, in order to be able to plot
# the error together with the actual data

species_sd <- list()

for(i in harvest_numbers){
species_sd[[i]] <- species_mean_plot[[i]] %>%
  pull(total) %>% 
  sd(na.rm = TRUE) 
}

# View(species_sd)

harvest_species_sd <- vector()

for(i in harvest_numbers){
  harvest_species_sd[i] <- species_sd[[i]] %>%
    mean(na.rm = TRUE)
}

harvest_species_sd

# I am going to eliminate my NAs from the dataset
harvest_species_sd <- harvest_species_sd[-c(7,9,11)]
harvest_species_mean_n <- harvest_species_mean[-c(7,9,11)]

# I create a dataframe with all the actual dates of the harvests,
# the means of the harvests and the standard deviations

species_mean_plots <- data.frame(year = c(19960601, 19970601, 19970901, 19980601,
                                     19980901, 19990601, 20000601, 20010601,
                                     20020601, 20020901, 20030601, 20030901,
                                     20040601, 20040901, 20050601, 20050901,
                                     20060601, 20060901, 20070601, 20070901,
                                     20080601, 20080901, 20090601, 20090901,
                                     20100601, 20100901, 20110601, 20110901,
                                     20120601, 20120901, 20130601, 20130901,
                                     20140601, 20140901, 20150601, 20150901,
                                     20160601, 20160901, 20170601, 20170901,
                                     20180601, 20180901, 20190601, 20190901,
                                     20200601, 20200901, 20210601, 20210901,
                                     20220601, 20220901, 20230601, 20230901,
                                     20240601, 20240901),
                            n_species = harvest_species_mean_n,
                            sd = harvest_species_sd)

# str(species_mean_plots)

species_mean_plots$year <- ymd(species_mean_plots$year)
species_mean_plots

species_mean_plots <- species_mean_plots %>%
  mutate(
    lower = n_species - sd,  
    upper = n_species + sd
  )

species_mean_plots


ggplot(species_mean_plots,
       aes(year, n_species)) +
  geom_point() +
  geom_line() +
  geom_smooth()

ggplot(species_mean_plots, aes(year, n_species)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Mean Species Number per Plot",
       x = "Year",
       y = "Number of Species")+
  theme_minimal()

#---- Species Trend ----

# now I want to actually observe the trend of the number of unique species
# during the sequent harvests/years

tbl_data_sp_unique <- tbl_data %>% 
  arrange(id_harvest) %>% 
  select(id_harvest,
         id_plant)

harvest_numbers <- unique(tbl_data_sp_unique$id_harvest)
spp <- list()

# I organize the data in different harvests

for(i in harvest_numbers){
  spp[[i]] <- tbl_data_sp_unique[which(tbl_data_sp_unique$id_harvest == i),]
}

# View(spp)

# I get rid of the empty elements

spp_n <- spp[-c(7, 9, 11)]

# View(spp_n)

# I want now to count all the unique names of the species for each harvest
# in this way I can count the single species a sa single entity
# without counting the same species more times if it is present in different plots
spp_sum <- list()
for(i in 1:length(spp_n)){
  spp_sum[[i]] <- spp_n[[i]] %>%
    distinct(id_plant) %>%
    count(id_plant) %>% 
    rename(presence = n) %>% 
    count(presence) %>% 
    rename(sum = n)
}

# View(spp_sum)

spp_count <- data.frame(year = c(19960601, 19970601, 19970901, 19980601,
                                        19980901, 19990601, 20000601, 20010601,
                                        20020601, 20020901, 20030601, 20030901,
                                        20040601, 20040901, 20050601, 20050901,
                                        20060601, 20060901, 20070601, 20070901,
                                        20080601, 20080901, 20090601, 20090901,
                                        20100601, 20100901, 20110601, 20110901,
                                        20120601, 20120901, 20130601, 20130901,
                                        20140601, 20140901, 20150601, 20150901,
                                        20160601, 20160901, 20170601, 20170901,
                                        20180601, 20180901, 20190601, 20190901,
                                        20200601, 20200901, 20210601, 20210901,
                                        20220601, 20220901, 20230601, 20230901,
                                        20240601, 20240901),
                        species = NA)

spp_count$year <- ymd(spp_count$year)

for(i in 1:nrow(spp_count)){
  spp_count$species[i] <- spp_sum[[i]]$sum
}

# View(spp_count)

ggplot(spp_count,
       aes(year,
           species)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  labs(title = "Species trend",
       x = "Year",
       y = "Number of Species")+
  theme_minimal()

max(spp_count$species)


#---- Species gain spring ----

# I want to have a look to species which are gained or lost each year/harvest
# I start with looking at the spring dataset

tbl_changes <- tbl_data %>% 
  arrange(id_harvest) %>% 
  arrange(id_plot) %>% 
  select(id_harvest,
         id_plot,
         id_plant,
         year,
         month)
  
# View(tbl_changes)
# View(tbl_data)

unique(tbl_changes$id_harvest)
harvest_numbers <- unique(tbl_changes$id_harvest)
plot_numbers <- unique(tbl_changes$id_plot)
year_numbers <- unique(tbl_changes$year)

# I so then filter for the spring season

tbl_changes_s <- tbl_changes %>% 
  filter(month == "Juni")

# View(tbl_changes_s)

id_harvest_spring <- unique(tbl_changes_s$id_harvest)
year_spring <- unique(tbl_changes_s$year)

# I proceed creating a data frame where I will then store the values
# of species lost and gained for the spring season

dataf_sp_spring <- data.frame(id_harvest = id_harvest_spring,
           year = year_spring)

# View(dataf_sp_spring)

# starting the loops in order to get the species gained harvest by harvest
# for each plot
# i so then loop for the different plots (that I filter)
# to then select the harvest and the species, to then order based on the
# harvest, so to have a time order in the dataset
# I then group by harvest inside each plot and i calculate the species
# gained from harvest to harvest and I count them

plot_data_s <- list()
species_gained_s <- list()

for(i in plot_numbers){
  
plot_data_s[[i]] <- tbl_changes_s %>%
  filter(id_plot == i) %>% 
  select(id_harvest, id_plant) %>%
  arrange(id_harvest)             

species_gained_s[[i]] <- plot_data_s[[i]] %>%
  group_by(id_harvest) %>%
  summarise(species_list = list(id_plant)) %>% 
  mutate(
    gained = map2(species_list, lag(species_list), ~ setdiff(.x, .y)), 
    n_gained = map_int(gained, length) 
  ) %>% 
  ungroup() %>%
  select(id_harvest, gained, n_gained)
}

# View(species_gained_s)

# I then select just the sum of the species gained and not the actual species

species_gained_s_essential <- list()

for(i in plot_numbers){
  species_gained_s_essential[[i]] <- species_gained_s[[i]] %>%
  select(id_harvest, n_gained)
}


# View(species_gained_s_essential)

# we need to correct the data for the plots for the ones 
# that were empty at the beginning they were in fact having no species,
# reason why harvests have not been completed there
# I proceed adding empty lines for those plots

missing_plots <- c("B1P012", "B1P029", "B1P041", "B1P061")

for (plot in missing_plots) {
  
  missing_rows <- tibble(
    id_harvest = c(2, 4),
    n_gained = c(0, 0)
  )
  
  species_gained_s_essential[[plot]] <- bind_rows(
    missing_rows,
    species_gained_s_essential[[plot]]
  )
}

# Check the updated list for one of the modified plots
print(species_gained_s_essential[["B1P012"]])
print(species_gained_s_essential[["B1P029"]])
print(species_gained_s_essential[["B1P041"]])
print(species_gained_s_essential[["B1P061"]])

# View(species_gained_s_essential)

# we then select just the sum we are intrested in
species_gained_s_values <- list()

for(i in plot_numbers){
species_gained_s_values[[i]] <- species_gained_s_essential[[i]] %>% 
  select(n_gained)
}

# View(species_gained_s_values)

# I can now transform my values into a data frame structure
spec_gain_s_df <- as.data.frame(species_gained_s_values)

# to then assign them to my initial data frame
# where I want to have all the species gained and lost for the spring season
# now I add the gained ones

dataf_sp_spring$species_gain_s <- rowMeans(spec_gain_s_df)

dataf_sp_spring


#---- Species loss spring ----

# I want to do the same I just did but considering the lost species

plot_data_s <- list()
species_lost_s <- list()

# I proceed to use the same loop used before but now I modify it in order
# to get as a result the lost species and not the gained ones

for(i in plot_numbers){

  plot_data_s[[i]] <- tbl_changes_s %>%
  filter(id_plot == i) %>% 
  select(id_harvest, id_plant) %>% 
  arrange(id_harvest)            

species_lost_s[[i]] <- plot_data_s[[i]] %>%
  group_by(id_harvest) %>%
  summarise(species_list = list(id_plant)) %>% 
  mutate(
    lost = map2(species_list, lead(species_list), ~ setdiff(.x, .y)), 
    n_lost = map_int(lost, length)
  ) %>%
  ungroup() %>%
  select(id_harvest, lost, n_lost)
}

# View(species_lost_s)

# I want know to select just the columns I am interested in
species_lost_s_essential <- list()

for(i in plot_numbers){
  species_lost_s_essential[[i]] <- species_lost_s[[i]] %>%
    select(id_harvest, n_lost)
}

# and Is till want to correct the plots were the first harvests are missing
missing_plots <- c("B1P012", "B1P029", "B1P041", "B1P061")

for (plot in missing_plots) {
  
  missing_rows <- tibble(
    id_harvest = c(2, 4),
    n_lost = c(0, 0)
  )
  
  species_lost_s_essential[[plot]] <- bind_rows(
    missing_rows,
    species_lost_s_essential[[plot]]
  )
}

# Check the updated list for one of the modified plots
print(species_lost_s_essential[["B1P012"]])
print(species_lost_s_essential[["B1P029"]])
print(species_lost_s_essential[["B1P041"]])
print(species_lost_s_essential[["B1P061"]])

# View(species_lost_s_essential)


# I want to select the actual count of my lost species
species_lost_s_values <- list()

for(i in plot_numbers){
  species_lost_s_values[[i]] <- species_lost_s_essential[[i]] %>% 
    select(n_lost)
}

# View(species_lost_s_values)

# I want my lost species to be similar to a data frame set
spec_loss_s_df <- as.data.frame(species_lost_s_values)

# to then be added to the dataframe considering the spring
dataf_sp_spring$species_loss_s <- rowMeans(spec_loss_s_df)

# I then would like to have the lost species as a negative value in order to 
# then plot them with the gained species and visualze them clearly
# in the same plot
dataf_sp_spring <- dataf_sp_spring %>% 
  mutate(species_loss_s = species_loss_s*(-1))

dataf_sp_spring
# str(dataf_sp_spring)

# dataf_s_plot <- dataf_sp_spring %>% 
#   select(year,
#          species_gain)

# I can get the data ready to be plot with pivot_longer, having all my values
# in the same columns
data_plot_sp_s <- dataf_sp_spring %>%
  pivot_longer(cols = c(species_gain_s, species_loss_s), 
               names_to = "species_type", 
               values_to = "value_s")

View(data_plot_sp_s)

# I can try to have a first plot showing how spring species gain/loss
# is behaving
ggplot(data_plot_sp_s, aes(x = year, y = value_s, color = species_type, group = species_type)) +
  geom_point() + 
  geom_line() +
  labs(title = "Species Gain and Loss Over Years",
       x = "Year",
       y = "Value",
       color = "Species Type") +
  theme_minimal()

# We can see that the last values, mostly of the lost species are probably
# not correct
# this is because R is calculating the comparison between the last harvest and
# a sequent one that doesn't exist, which is then considered to be 0,
# reason why the decrease is really high
# I still do not understand why there are gained species in the last harvest
# if the comparison is basically done with null values

#---- Species gain autumn ----

# Now I would like to do the exact same but with the September harvest,
# so witht he autumn season

unique(tbl_changes$id_harvest)
harvest_numbers <- unique(tbl_changes$id_harvest)
plot_numbers <- unique(tbl_changes$id_plot)

tbl_changes_a <- tbl_changes %>% 
  filter(month == "September")

# View(tbl_changes_s)

id_harvest_autumn <- unique(tbl_changes_a$id_harvest)
year_autumn <- unique(tbl_changes_a$year)

dataf_sp_autumn <- data.frame(id_harvest = id_harvest_autumn,
                              year = year_autumn)


# View(dataf_sp_autumn)

# strting the loops in order to get the species gained harvest by harvest
# for each plot

plot_data_a <- list()
species_gained_a <- list()

for(i in plot_numbers){
  
  plot_data_a[[i]] <- tbl_changes_a %>%
    filter(id_plot == i) %>% 
    select(id_harvest, id_plant) %>%
    arrange(id_harvest)             
  
  species_gained_a[[i]] <- plot_data_a[[i]] %>%
    group_by(id_harvest) %>%
    summarise(species_list = list(id_plant)) %>% 
    mutate(
      gained = map2(species_list, lag(species_list), ~ setdiff(.x, .y)), 
      n_gained = map_int(gained, length) 
    ) %>% 
    ungroup() %>%
    select(id_harvest, gained, n_gained)
}

# View(species_gained_a)

species_gained_a_essential <- list()

for(i in plot_numbers){
  species_gained_a_essential[[i]] <- species_gained_a[[i]] %>%
    select(id_harvest, n_gained)
}


# View(species_gained_a_essential)

# we need to correct the data for the plots for the ones that were empty at the beginning
# they were in fact having no species, reason why harvests have not been completed there

missing_plots <- c("B1P012", "B1P029", "B1P041", "B1P061")

for (plot in missing_plots) {
  
  missing_rows <- tibble(
    id_harvest = c(1, 3, 5),
    n_gained = c(0, 0, 0)
  )
  
  species_gained_a_essential[[plot]] <- bind_rows(
    missing_rows,
    species_gained_a_essential[[plot]]
  )
}

# species_gained_a_essential

# Check the updated list for one of the modified plots
print(species_gained_a_essential[["B1P012"]])
print(species_gained_a_essential[["B1P029"]])
print(species_gained_a_essential[["B1P041"]])
print(species_gained_a_essential[["B1P061"]])

# I can observe that now there are less harvests because for years 
# 1999, 2000, 2001 the autumn harvest has not been completed

# View(species_gained_s_essential)

species_gained_a_values <- list()

for(i in plot_numbers){
  species_gained_a_values[[i]] <- species_gained_a_essential[[i]] %>% 
    select(n_gained)
}

# View(species_gained_a_values)

spec_gain_a_df <- as.data.frame(species_gained_a_values)


dataf_sp_autumn$species_gain_a <- rowMeans(spec_gain_a_df)

dataf_sp_autumn


#---- Species loss autumn  ----

# same for the species lost in autumn

plot_data_a <- list()
species_lost_a <- list()

for(i in plot_numbers){
  
  plot_data_a[[i]] <- tbl_changes_a %>%
    filter(id_plot == i) %>% 
    select(id_harvest, id_plant) %>% 
    arrange(id_harvest)            
  
  species_lost_a[[i]] <- plot_data_a[[i]] %>%
    group_by(id_harvest) %>%
    summarise(species_list = list(id_plant)) %>% 
    mutate(
      lost = map2(species_list, lead(species_list), ~ setdiff(.x, .y)), 
      n_lost = map_int(lost, length)
    ) %>%
    ungroup() %>%
    select(id_harvest, lost, n_lost)
}

# View(species_lost_a)

species_lost_a_essential <- list()

for(i in plot_numbers){
  species_lost_a_essential[[i]] <- species_lost_a[[i]] %>%
    select(id_harvest, n_lost)
}

missing_plots_a <- c("B1P012", "B1P029", "B1P041", "B1P061")

for (plot in missing_plots_a) {
  
  missing_rows <- tibble(
    id_harvest = c(1, 3, 5),
    n_lost = c(0, 0, 0)
  )
  
  species_lost_a_essential[[plot]] <- bind_rows(
    missing_rows,
    species_lost_a_essential[[plot]]
  )
}


# Check the updated list for one of the modified plots
print(species_lost_a_essential[["B1P012"]])
print(species_lost_a_essential[["B1P029"]])
print(species_lost_a_essential[["B1P041"]])
print(species_lost_a_essential[["B1P061"]])

# View(species_lost_a_essential)

species_lost_a_values <- list()

for(i in plot_numbers){
  species_lost_a_values[[i]] <- species_lost_a_essential[[i]] %>% 
    select(n_lost)
}

# View(species_lost_a_values)

spec_loss_df_a <- as.data.frame(species_lost_a_values)


dataf_sp_autumn$species_loss_a <- rowMeans(spec_loss_df_a)

dataf_sp_autumn <- dataf_sp_autumn %>% 
  mutate(species_loss_a = species_loss_a*(-1))


dataf_sp_autumn
# str(dataf_sp_autumn)

data_plot_sp_a <- dataf_sp_autumn %>%
  pivot_longer(cols = c(species_gain_a, species_loss_a), 
               names_to = "species_type_a", 
               values_to = "value_a")

# Create the plot
ggplot(data_plot_sp_a, aes(x = year, y = value_a, color = species_type_a, group = species_type_a)) +
  geom_point() + 
  geom_line() +
  labs(title = "Species Gain and Loss Over Years",
       x = "Year",
       y = "Value",
       color = "Species Type") +
  theme_minimal()


# now I want to plot everything together
# both the data coming from the spring and the data coming from autumn

dataf_sp_spring # spring dataframe
dataf_sp_autumn # autumn dataframe

# since we can see that the last value comparing the most recent harvest with
# a sequent value looks really wrong I am going to remove it from both datasets

dataf_sp_spring <- dataf_sp_spring %>%
  mutate(season = "spring") %>% 
  slice(-n())

dataf_sp_autumn <- dataf_sp_autumn %>%
  mutate(season = "autumn")%>% 
  slice(-n())

# Combine the two dataframes together in order to then plot them
combined_data <- bind_rows(dataf_sp_spring, dataf_sp_autumn)

# View(combined_data)

combined_data <- combined_data %>%
  rename(
    species_gain_spring = species_gain_s,
    species_loss_spring = species_loss_s,
    species_gain_autumn = species_gain_a,
    species_loss_autumn = species_loss_a
  )

# I want to make the data ready to be plotted
plot_data_gain_loss <- combined_data %>%
  pivot_longer(
    cols = starts_with("species_"),
    names_to = "metric", 
    values_to = "value"
  ) %>%
  drop_na(value) %>% 
  mutate(
    type = if_else(str_detect(metric, "gain"), "gain", "loss"),
    season = if_else(str_detect(metric, "spring"), "spring", "autumn")
  ) %>%
  select(-metric)

ggplot(plot_data_gain_loss, aes(x = year, y = value, shape = type, fill = season)) +
  geom_point(size = 3, aes(color = season)) +
  geom_line(aes(linetype = season, group = interaction(season, type))) +
  scale_shape_manual(values = c("gain" = 21, "loss" = 24)) + 
  scale_fill_manual(values = c("spring" = "white", "autumn" = "black")) + 
  scale_color_manual(values = c("spring" = "blue", "autumn" = "red")) +
  scale_linetype_manual(values = c("spring" = "dotted", "autumn" = "solid")) + 
  labs(
    title = "Species Gain and Loss Trends by Season",
    x = "Year",
    y = "Species Count",
    shape = "Type",
    fill = "Season",
    color = "Season",
    linetype = "Season"
  ) +
  theme_minimal()

#---- Biomass Evolution ----

# let's see how the biomass evolved over time

tbl_data_bio <- tbl_data %>% 
  arrange(id_harvest) %>% 
  select(id_harvest,
         biomass,
         year)

harvest_numbers <- unique(tbl_data_bio$id_harvest)
bio <- list()

for(i in harvest_numbers){
  bio[[i]] <- tbl_data_bio[which(tbl_data_bio$id_harvest == i),]
}

# View(bio)
bio_n <- list()

bio_n <- bio[-c(1, 2, 3, 4, 5, 7, 9, 11)]

# View(bio_n)

bio_sum <- list()


for (i in 1:length(bio_n)) {
  bio_sum[[i]] <- bio_n[[i]] %>%
    summarise(total_biomass = sum(biomass, na.rm = TRUE)) %>%
    pull(total_biomass)  
}

# View(bio_sum)

biomass_count <- data.frame(number = 1:49,
                            year = c(19990601, 20000601, 20010601,
                                     20020601, 20020901, 20030601, 20030901,
                                     20040601, 20040901, 20050601, 20050901,
                                     20060601, 20060901, 20070601, 20070901,
                                     20080601, 20080901, 20090601, 20090901,
                                     20100601, 20100901, 20110601, 20110901,
                                     20120601, 20120901, 20130601, 20130901,
                                     20140601, 20140901, 20150601, 20150901,
                                     20160601, 20160901, 20170601, 20170901,
                                     20180601, 20180901, 20190601, 20190901,
                                     20200601, 20200901, 20210601, 20210901,
                                     20220601, 20220901, 20230601, 20230901,
                                     20240601, 20240901),
                            sum_bio = NA)

biomass_count$year <- ymd(biomass_count$year)

for(j in 1:nrow(biomass_count)){
    biomass_count$sum_bio[j] <- bio_sum[[j]]
}

biomass_count

ggplot(biomass_count,
       aes(year,
           sum_bio)) +
  geom_point() +
  geom_line(na.rm = FALSE) +
  geom_smooth(na.rm = FALSE) +
  labs(title = "Sum of Biomass",
       x = "Year",
       y = "Grams of Biomass")

#---- Mean Biomass per plot ----

# Here I want to consider the mean of species present in each plot
# This means that the same species are not going

tbl_data_bio_m <- tbl_data %>% 
  arrange(id_harvest) %>%
  mutate(id_plot_numeric = as.numeric(as.factor(id_plot))) %>% 
  select(id_harvest,
         id_plot_numeric,
         biomass)

# View(tbl_data_bio_m)

harvest_numbers_bio_m <- unique(tbl_data_bio_m$id_harvest)
bio_m <- list()

for(i in harvest_numbers){
  bio_m[[i]] <- tbl_data_bio_m[which(tbl_data_bio_m$id_harvest == i),]
}

# View(bio_m)

bio_mean <- list()

bio_mean <- bio_m[-c(1, 2, 3, 4, 5, 7, 9, 11)]

# View(bio_mean)

# I want now to consider every harvest singularly
# I want then to group by plot, in order to have for each harvest the amount 
# of species present in each plot

biomass_mean_plot <- list()

for(i in 1:length(bio_mean)){
  biomass_mean_plot[[i]] <- bio_mean[[i]] %>%
    group_by(id_plot_numeric) %>% 
    summarise(biomass_sum = sum(biomass, na.rm = TRUE)) %>% 
    select(biomass_sum)
}

# View(biomass_mean_plot)

# I now want to calculate the mean for each harvest of the amount of biomass
# present in every single plot
# I then want to extract the values from the list and have them in a vector

harvest_biomass_mean <- sapply(biomass_mean_plot, function(df) {
  sum(df$biomass_sum, na.rm = TRUE)
})

# View(harvest_biomass_mean)
# str(harvest_biomass_mean)

# I want to calculate the standard error, in order to be able to plot
# the error together with the actual data

standard_error <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x[!is.na(x)]))
}

# Apply the function to calculate the standard error for each element
harvest_biomass_se <- sapply(biomass_mean_plot, function(df) {
  standard_error(df$biomass_sum)
})



# I create a dataframe with all the actual dates of the harvests,
# the means of the harvests and the standard deviations

biomass_mean_plots <- data.frame(year = c(19990601, 20000601, 20010601, 20020601,
                                          20020901, 20030601, 20030901, 20040601,
                                          20040901, 20050601, 20050901, 20060601,
                                          20060901, 20070601, 20070901, 20080601,
                                          20080901, 20090601, 20090901, 20100601, 
                                          20100901, 20110601, 20110901, 20120601, 
                                          20120901, 20130601, 20130901, 20140601, 
                                          20140901, 20150601, 20150901, 20160601, 
                                          20160901, 20170601, 20170901, 20180601,
                                          20180901, 20190601, 20190901, 20200601,
                                          20200901, 20210601, 20210901, 20220601,
                                          20220901, 20230601, 20230901, 20240601,
                                          20240901),
                                 biomass_sum = harvest_biomass_mean,
                                 sd = harvest_biomass_se)

# str(biomass_mean_plots)

biomass_mean_plots$year <- ymd(biomass_mean_plots$year)
biomass_mean_plots

biomass_mean_plots <- biomass_mean_plots %>%
  mutate(
    lower = biomass_sum - sd,  
    upper = biomass_sum + sd
  )

biomass_mean_plots


ggplot(biomass_mean_plots,
       aes(year, biomass_sum)) +
  geom_point() +
  geom_line() +
  geom_smooth()

ggplot(biomass_mean_plots, aes(year, biomass_sum)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Mean Biomass sum per Plot",
       x = "Year",
       y = "Grams of Biomass")+
  theme_minimal()

#---- Starting loops with Jaccard ----

# firstly i want to consider the whole dataset together

plot_numbers <- unique(tbl_data$id_plot)
data_w <- list()

# we can start with the first loop
for(i in plot_numbers){
  data_w[[i]] <- tbl_data[which(tbl_data$id_plot == i),]
}

# we continue with arranging the data based on the harvest
# and we start the loops in order to get the specific id harvests
# and to filter the data from the 6th harvest on
# and to select just the columns we need
data_wh <- list()

# data_w_arranged <- tbl_data %>% 
# arrange(id_harvest)

# id_harvest_w <- unique(data_w_arranged$id_harvest) #1  3  5 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57

for(i in plot_numbers){
  data_wh[[i]] <- data_w[[i]] %>%
    arrange(id_harvest) %>% 
    filter(6 <= id_harvest)%>% 
    select(id_harvest,
           id_plant,
           species)
}

# View(data_wh)
# data_wh[[1]]

# we proceed with changing the structure of the data
# i want to have the species as columns
# the harvest as rows
# and the values as the species, so if either they are present or not
# if the value of species is 1 means that the species is present
# if the value is 0 it means that the species is not present

data_who <- list()

for(i in plot_numbers){
  data_who[[i]] <- data_wh[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = species,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest")
}

# View(data_who)
# View(data_who[[1]])


j_similarity_w <- list()

for (i in plot_numbers) {
  j_dissimilarity_w <- data_who[[i]] %>%
    vegdist(method = "jaccard", binary = TRUE)
  
  j_similarity_w[[i]] <- 1 - as.matrix(j_dissimilarity_w) 
}

j_similarity_w


# let's check if my harvests are the correct ones
num_har_w <- as.numeric(rownames(j_similarity_w[[1]]))

length(num_har_w) #49


# and we can create two dataframes where to then store the values we are intrested in
table_w <- data.frame(number = 1:48,
                      rname = NA,
                      col_name = NA)

# we can now assign the values of the cells lying over the main diagonal
# in this diagonal we can find the values of the jaccard index which are
# comparing consecutive harvests for the same plot
# in this way we want to visualise how the similarity changed over the years 
# in the same plot
x_w <- list()

for(j in plot_numbers){
  for (i in 1:48){ #49 harvests
    x_w[[j]][i] <- j_similarity_w[[j]][i,i+1]
    table_w$rname[i] <- rownames(j_similarity_w[[j]])[i]
    table_w$col_name[i] <- colnames(j_similarity_w[[j]])[i + 1]
  }
}
x_w
# View(x_w)

# we want to be able to have a data.frame to then have a plot with ggplot
plot_data_w <- do.call(rbind, lapply(seq_along(x_w), function(j) {
  data.frame(
    Plot = names(x_w)[j],               # Correctly assign plot names
    Harvest = 1:length(x_w[[j]]),       # Harvest numbers
    Jaccard = unlist(x_w[[j]])          # Jaccard values
  )
})) %>%
  as.data.frame()

# view(plot_data_w)

# Compute the mean Jaccard index for each harvest in every plot
mean_data_w <- plot_data_w %>%
  group_by(Harvest) %>%
  summarize(MeanJaccard = mean(Jaccard))

# Plot with ggplot2 the mean of each harvest considering all plots 
ggplot(plot_data_w, aes(x = Harvest, y = Jaccard, color = Plot)) +
  geom_line(data = mean_data_w, aes(x = Harvest, y = MeanJaccard), 
            color = "red", linewidth = 1.2) +
  labs(
    title = "Jaccard Index Trends Across Plots, Total",
    x = "Harvest",
    y = "Jaccard Similarity Index"
  ) +
  theme_minimal()


ggplot(plot_data_w, aes(x = Harvest, y = Jaccard)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Jaccard Index Trends Across Plots, Total",
    x = "Harvest",
    y = "Jaccard Similarity Index"
  ) +
  theme_minimal()



# i want to try to look at all the different plots together
plot_data_w %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Jaccard))+
  geom_line(aes(Harvest,Jaccard, color = Plot))

# not great



# then i want to start sub setting the data based on the season the data is belonging to
data_a <- tbl_data %>% 
  filter(month == "September") 

unique(data_a$id_harvest)
# view(data_a)

data_s <- tbl_data %>% 
  filter(month == "Juni")

unique(data_s$id_harvest)
# view(data_s)

# now we want to differentiate the data for the different plots
plot_numbers <- unique(tbl_data$id_plot)
data_au <- list()

# and we can start with loops
# for autumn
for(i in plot_numbers){
  data_au[[i]] <- data_a[which(data_a$id_plot == i),]
}

# and spring
data_sp <- list()

for(i in plot_numbers){
  data_sp[[i]] <- data_s[which(data_s$id_plot == i),]
}

# View(data_sp)

# data_sp[[1]]
# data[[58]]



# we continue with arranging the data based on the harvest
# and we start the loops in order to get the specific id harvests
# and to filter the data from the 6th harvest on
# and to select just the columns we need
data_aut <- list()

data_ar <- data_a %>% 
  arrange(id_harvest)
  
id_harvest_a <- unique(data_ar$id_harvest) #1  3  5 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57

for(i in plot_numbers){
  data_aut[[i]] <- data_au[[i]] %>%
    mutate(id_harvest_a = id_harvest) %>% 
    arrange(id_harvest_a) %>% 
    filter(6 <= id_harvest_a)%>% 
    select(id_harvest_a,
           id_plant,
           species)
}

# View(data_aut)

data_spr <- list()

data_sr <- data_s %>%
  arrange(id_harvest)

id_harvest_s <- unique(data_sr$id_harvest) #2  4  6  8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56

for(i in plot_numbers){
  data_spr[[i]] <- data_sp[[i]] %>% 
    mutate(id_harvest_s = id_harvest) %>% 
    arrange(id_harvest_s) %>% 
    filter(6 <= id_harvest_s)%>% 
    select(id_harvest_s,
           id_plant,
           species)
}

# View(data_spr)
# View(data_spr[["B1P043"]])

# we proceed with changing the structure of the data
# i want to have the species as columns
# the harvest as rows
# and the values as the species, so if either they are present or not
# if thevalue of species is 1 means that the species is present
# if the value is 0 it means that the species is not present

data_autu <- list()

for(i in plot_numbers){
  data_autu[[i]] <- data_aut[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = species,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest_a")
}

# View(data_autu)

data_spri <- list()

for(i in plot_numbers){
  data_spri[[i]] <- data_spr[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = species,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest_s")
}

# View(data_spri)

# now i want to calculate jaccard index with the vegdist function
# the vegdist function is actually calculating a distance, so a dissimilarity value
# that is why i want then to obtain 1 - vegdist() in order to get the actual jaccard index

j_similarity_a <- list()

for (i in plot_numbers) {
  
  j_dissimilarity_a <- data_autu[[i]] %>%
    vegdist(method = "jaccard", binary = TRUE)
  
  j_similarity_a[[i]] <- 1 - as.matrix(j_dissimilarity_a) 
}

j_similarity_a

# View(j_similarity_a)
# view(j_similarity_a[["B1P001"]])
# plot(j_similarity_a[["B1P001"]])
# lapply(j_similarity_a, summary)

j_similarity_s <- list()

for(i in plot_numbers){
  
  j_dissimilarity_s <- data_spri[[i]] %>% 
  vegdist(method = "jaccard", binary = TRUE)

  j_similarity_s[[i]] <- 1 - as.matrix(j_dissimilarity_s)  
}

# View(j_similarity_s)
# view(j_similarity_s[["B1P001"]])
# plot(j_similarity_s[["B1P001"]])
# lapply(j_similarity_s, summary)


# let's check if my harvests are the correct ones
num_har_a <- as.numeric(rownames(j_similarity_a[[1]]))
num_har_s <- as.numeric(rownames(j_similarity_s[[1]]))

length(num_har_a) #23
length(num_har_s) #26


# and we can create two dataframes where to then store the values we are intrested in
table_a <- data.frame(number = 1:22,
                      rname = NA,
                      col_name = NA)

table_s <- data.frame(number = 1:25,
                      rname = NA,
                      col_name = NA)

# we can now assign the values of the diagonal lying over the main one
# in this diagonal we can find the values of the jaccard index which are
# comparing consecutive harvests for the same plot
# in this way we want to visualise how the similarity changed over the years 
# in the same plot
x_a <- list()

x_s <- list()

for(j in plot_numbers){
  for (i in 1:22){ #49 harvests
    x_a[[j]][i] <- j_similarity_a[[j]][i,i+1]
    table_a$rname[i] <- rownames(j_similarity_a[[j]])[i]
    table_a$col_name[i] <- colnames(j_similarity_a[[j]])[i + 1]
  }
}
x_a
# view(x_a)
# x_a[[1]]
# 
# plot(x_a$B1P001)
# plot(x_a$B1P002)
# plot(x_a$B1P006)

for(j in plot_numbers){
  for(i in 1:25){
    x_s[[j]][i] <- j_similarity_s[[j]][i,i+1]
    table_s$rname[i] <- rownames(j_similarity_s[[j]])[i]
    table_s$col_name[i] <- colnames(j_similarity_s[[j]])[i + 1]
  }
}

x_s
# view(x_s)
# x_s[[1]]
# 
# plot(x_s$B1P001)
# plot(x_s$B1P002)
# plot(x_s$B1P006)

# we want to be able to have a data.frame to then have a plot with ggplot
plot_data_a <- do.call(rbind, lapply(seq_along(x_a), function(j) {
  data.frame(
    Plot = names(x_a)[j],               # Correctly assign plot names
    Harvest = 1:length(x_a[[j]]),       # Harvest numbers
    Jaccard = unlist(x_a[[j]])          # Jaccard values
  )
})) %>%
  as.data.frame()

# view(plot_data_a)

plot_data_s <- do.call(rbind, lapply(seq_along(x_s), function(j) {
  data.frame(
    Plot = names(x_s)[j],
    Harvest = 1:length(x_s[[j]]),
    Jaccard = unlist(x_s[[j]])
  )
})) %>% 
  as.data.frame()

# view(plot_data_s)


# Compute the mean Jaccard index for each harvest in every plot
mean_data_a <- plot_data_a %>%
  group_by(Harvest) %>%
  summarize(MeanJaccard = mean(Jaccard))

mean_data_s <- plot_data_s %>% 
  group_by(Harvest) %>% 
  summarize(MeanJaccard = mean(Jaccard))


# Plot with ggplot2 the mean of each harvest considering all plots 
ggplot(plot_data_a, aes(x = Harvest, y = Jaccard)) +
  geom_line(data = mean_data_a,
            aes(x = Harvest, y = MeanJaccard), 
            color = "red", size = 1.2) +
  geom_smooth(method = lm) +
  labs(
    title = "Jaccard Index Trends Across Plots, Autumn",
    x = "Harvest",
    y = "Jaccard Similarity Index"
  ) +
  theme_minimal()

ggplot(plot_data_a, aes(x = Harvest, y = Jaccard)) +
  # geom_line(data = mean_data_a,
  #          aes(x = Harvest, y = MeanJaccard), 
  #          color = "red", size = 1.2) +
  geom_point(aes(x = Harvest, y = Jaccard)) +
  geom_smooth() +
  labs(
    title = "Jaccard Index Trends Across Plots, Autumn",
    x = "Harvest",
    y = "Jaccard Similarity Index"
  ) +
  theme_minimal()

ggplot(plot_data_s, aes(x = Harvest, y = Jaccard)) +
  # geom_line(data = mean_data_s,
  #           aes(Harvest, MeanJaccard),
  #           color = "red", size = 1.2) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Jaccard Index Trends Across Plots, Spring",
    x = "Harvest",
    y = "Jaccard Similarity Index"
  ) +
  theme_minimal()

# i want to try to look at all the different plots together
plot_data_a %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Jaccard))+
  geom_line(aes(Harvest,Jaccard, color = Plot))

# not great

plot_data_s %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Jaccard))+
  geom_line(aes(Harvest,Jaccard, color = Plot))


#---- Bray-Curtis Index Cover 2x2 ----
# bray curtis index

# we continue with adding the column for the species of the bray curtis index
# and arranging based on the harvest, selecting only from the 6th harvest on

# we first are going to consider the 2x2 cover
# then the biomass
# then the two of them together

bray_c_w <- list()
# View(data_w)

for(i in plot_numbers){
  bray_c_w[[i]] <- data_w[[i]] %>%
    arrange(id_harvest) %>% 
    filter(6 <= id_harvest)%>% 
    drop_na(cover_2x2) %>% 
    select(id_harvest,
           id_plant,
           cover_2x2)
}

View(bray_c_w)

# unique(bray_c_w[[1]]$id_harvest)


# we proceed with changing the structure of the data
# i want to have the species as columns
# the harvest as rows
# and the values as the cover_2x2, so what the cover of each species is

bray_c_wh <- list()

for(i in plot_numbers){
  bray_c_wh[[i]] <- bray_c_w[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = cover_2x2,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest")
}

# View(bray_c_wh)

b_similarity_c_w <- list()

for (i in plot_numbers) {
  b_dissimilarity_c_w <- bray_c_wh[[i]] %>%
  vegdist(method = "bray")
  
  b_similarity_c_w[[i]] <- 1 - as.matrix(b_dissimilarity_c_w) 
}

b_similarity_c_w

# View(b_similarity_c_w)
# view(b_similarity_c_w[["B1P001"]])
# plot(b_similarity_c_w[["B1P001"]])
# lapply(b_similarity_c_w, summary)

bray_har_c_w <- as.numeric(rownames(b_similarity_c_w[[1]]))

length(bray_har_c_w) #47

table_bray_c_w <- data.frame(number = 1:46,
                           rname = NA,
                           col_name = NA)


x_b_c_w <- list()

for(j in plot_numbers){
  for (i in 1:46){ #47 harvests
    x_b_c_w[[j]][i] <- b_similarity_c_w[[j]][i,i+1]
    table_bray_c_w$rname[i] <- rownames(b_similarity_c_w[[j]])[i]
    table_bray_c_w$col_name[i] <- colnames(b_similarity_c_w[[j]])[i + 1]
  }
}
x_b_c_w
# view(x_b_c_w)
# x_b_c_w[[1]]
# 
# plot(x_b_c_w$B1P001)
# plot(x_b_c_w$B1P002)
# plot(x_b_c_w$B1P006)

# set.seed(19991225)
# MDS_1 <- metaMDS(x_b_c_w[[1]])
# 
# plot(MDS_1)

plot_bray_c_w <- do.call(rbind, lapply(seq_along(x_b_c_w), function(j) {
  data.frame(
    Plot = names(x_b_c_w)[j],               # Correctly assign plot names
    Harvest = 1:length(x_b_c_w[[j]]),       # Harvest numbers
    Bray = unlist(x_b_c_w[[j]])          # Jaccard values
  )
})) %>%
  as.data.frame()

# view(plot_bray_c_w)

# compute the mean Bray index for each harvest
mean_bray_c_w <- plot_bray_c_w %>%
  group_by(Harvest) %>%
  summarize(MeanBray = mean(Bray))

# Plot with ggplot2
ggplot(plot_bray_c_w, aes(x = Harvest, y = Bray)) +
  # geom_line(data = mean_bray_c_w,
  #           aes(x = Harvest, y = MeanBray), 
  #           color = "red", size = 1.2) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Bray Index Trends Across Plots, Cover, Total",
    x = "Harvest",
    y = "Bray-Curtis Similarity Index"
  ) +
  theme_minimal()

plot_bray_c_w %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Bray))+
  geom_line(aes(Harvest,Bray, color = Plot))

# View(plot_bray_c_w)

# considering the 2x2 cover while subsetting

bray_c_aut <- list()

data_ar <- data_a %>% 
  arrange(id_harvest)

id_harvest_a <- unique(data_ar$id_harvest)

for(i in plot_numbers){
  bray_c_aut[[i]] <- data_au[[i]] %>%
    mutate(id_harvest_a = id_harvest) %>% 
    arrange(id_harvest_a) %>% 
    filter(6 <= id_harvest_a)%>% 
    drop_na(cover_2x2) %>% 
    select(id_harvest_a,
           id_plant,
           cover_2x2)
}


length(id_harvest_a)
# View(bray_aut)

bray_c_spr <- list()

data_sr <- data_s %>% 
  arrange(id_harvest)

id_harvest_s <- unique(data_sr$id_harvest)

for(i in plot_numbers){
  bray_c_spr[[i]] <- data_sp[[i]] %>% 
    mutate(id_harvest_s = id_harvest) %>% 
    arrange(id_harvest_s) %>% 
    filter(6 <= id_harvest_s) %>% 
    drop_na(cover_2x2) %>% 
    select(id_harvest_s,
           id_plant,
           cover_2x2)
}

length(id_harvest_s)

# View(bray_c_spr)
# View(bray_c_spr[["B1P043"]])

# we proceed with changing the structure of the data
# i want to have the species as columns
# the harvest as rows
# and the values as the species, so if either they are present or not

bray_c_autu <- list()

for(i in plot_numbers){
  bray_c_autu[[i]] <- bray_c_aut[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = cover_2x2,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest_a")
}

# View(bray_c_autu)

bray_c_spri <- list()

for(i in plot_numbers){
  bray_c_spri[[i]] <- bray_c_spr[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = cover_2x2,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest_s")
}

# View(bray_c_spri)


b_similarity_c_a <- list()

for (i in plot_numbers) {
  b_dissimilarity_c_a <- bray_c_autu[[i]] %>%
    vegdist(method = "bray")
  
  b_similarity_c_a[[i]] <- 1 - as.matrix(b_dissimilarity_c_a) 
}

b_similarity_c_a

# View(b_similarity_c_a)
# view(b_similarity_c_a[["B1P001"]])
# plot(b_similarity_c_a[["B1P001"]])
# lapply(b_similarity_c_a, summary)

b_similarity_c_s <- list()

for(i in plot_numbers){
  b_dissimilarity_c_s <- bray_c_spri[[i]] %>% 
  vegdist(method = "bray")
  
  b_similarity_c_s[[i]] <- 1 - as.matrix(b_dissimilarity_c_s)  
}


# View(b_similarity_c_s)
# view(b_similarity_c_s[["B1P001"]])
# plot(b_similarity_c_s[["B1P001"]])
# lapply(b_similarity_c_s, summary)

bray_har_c_a <- as.numeric(rownames(b_similarity_c_a[[1]]))
bray_har_c_s <- as.numeric(rownames(b_similarity_c_s[[1]]))

length(bray_har_c_a) #23
length(bray_har_c_s) #24


table_bray_c_a <- data.frame(number = 1:22,
                      rname = NA,
                      col_name = NA)

table_bray_c_s <- data.frame(number = 1:23,
                      rname = NA,
                      col_name = NA)

x_b_c_a <- list()

x_b_c_s <- list()

for(j in plot_numbers){
  for (i in 1:22){ #23 harvests
    x_b_c_a[[j]][i] <- b_similarity_c_a[[j]][i,i+1]
    table_bray_c_a$rname[i] <- rownames(b_similarity_c_a[[j]])[i]
    table_bray_c_a$col_name[i] <- colnames(b_similarity_c_a[[j]])[i + 1]
  }
}
x_b_c_a
# view(x_b_c_a)
# x_b_c_a[[1]]
# 
# plot(x_b_c_a$B1P001)
# plot(x_b_c_a$B1P002)
# plot(x_b_c_a$B1P006)

for(j in plot_numbers){
  for(i in 1:23){
    x_b_c_s[[j]][i] <- b_similarity_c_s[[j]][i,i+1]
    table_bray_c_s$rname[i] <- rownames(b_similarity_c_s[[j]])[i]
    table_bray_c_s$col_name[i] <- colnames(b_similarity_c_s[[j]])[i + 1]
  }
}

x_b_c_s
# view(x_b_c_s)
# x_b_c_s[[1]]
# view(x_b_c_s$B1P009)
# plot(x_b_c_s$B1P001)
# plot(x_b_c_s$B1P002)
# plot(x_b_c_s$B1P006)

plot_bray_c_a <- do.call(rbind, lapply(seq_along(x_b_c_a), function(j) {
  data.frame(
    Plot = names(x_b_c_a)[j],               # Correctly assign plot names
    Harvest = 1:length(x_b_c_a[[j]]),       # Harvest numbers
    Bray = unlist(x_b_c_a[[j]])          # Jaccard values
  )
})) %>%
  as.data.frame()

# view(plot_bray_c_a)

plot_bray_c_s <- do.call(rbind, lapply(seq_along(x_b_c_s), function(j) {
  data.frame(
    Plot = names(x_b_c_s)[j],
    Harvest = 1:length(x_b_c_s[[j]]),
    Bray = unlist(x_b_c_s[[j]])
  )
})) %>% 
  as.data.frame()

# view(plot_bray_c_s)


# compute the mean Bray index for each harvest
mean_bray_c_a <- plot_bray_c_a %>%
  group_by(Harvest) %>%
  summarize(MeanBray = mean(Bray))

mean_bray_c_s <- plot_bray_c_s %>% 
  group_by(Harvest) %>% 
  summarize(MeanBray = mean(Bray))


# Plot with ggplot2
ggplot(plot_bray_c_a, aes(x = Harvest, y = Bray)) +
  # geom_line(data = mean_bray_c_a,
  #           aes(x = Harvest, y = MeanBray), 
  #           color = "red", size = 1.2) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Bray Index Trends Across Plots, Cover, Autumn",
    x = "Harvest",
    y = "Bray-Curtis Similarity Index"
  ) +
  theme_minimal()

ggplot(plot_bray_c_s, aes(x = Harvest, y = Bray)) +
  # geom_line(data = mean_bray_c_s,
  #           aes(Harvest, MeanBray),
  #           color = "red", size = 1.2) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Bray Index Trends Across Plots, Cover, Spring",
    x = "Harvest",
    y = "Bray-Curtis Similarity Index"
  ) +
  theme_minimal()

plot_bray_c_a %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Bray))+
  geom_line(aes(Harvest,Bray, color = Plot))

# View(plot_bray_c_a)

plot_bray_c_s %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Bray))+
  geom_line(aes(Harvest, Bray, color = Plot))


#---- Bray-Curtis Index Biomass ----

# considering the biomass

bray_b_w <- list()

for(i in plot_numbers){
  bray_b_w[[i]] <- data_w[[i]] %>% 
    arrange(id_harvest) %>% 
    filter(6 <= id_harvest)%>%
    select(id_harvest,
           id_plant,
           biomass)
}

# View(bray_b_w)

# we proceed with changing the structure of the data
# i want to have the species as columns
# the harvest as rows
# and the values as the biomass

bray_b_wh <- list()

for(i in plot_numbers){
  bray_b_wh[[i]] <- bray_b_w[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = biomass,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest")
}

# View(bray_b_wh)

b_similarity_b_w <- list()

for (i in plot_numbers) {
  b_dissimilarity_b_w <- bray_b_wh[[i]] %>%
  vegdist(method = "bray",
           na.rm = TRUE)
  
  b_similarity_b_w[[i]] <- 1 - as.matrix(b_dissimilarity_b_w) 
}

b_similarity_b_w

# View(b_similarity_b_w)
# view(b_similarity_b_w[["B1P001"]])
# plot(b_similarity_b_w[["B1P001"]])
# lapply(b_similarity_b_w, summary)

bray_har_b_w <- as.numeric(rownames(b_similarity_b_w[[1]]))

length(bray_har_b_w) #49


table_bray_b_w <- data.frame(number = 1:48,
                               rname = NA,
                               col_name = NA)

x_b_b_w <- list()

for(j in plot_numbers){
  for (i in 1:48){ #49 harvests
    x_b_b_w[[j]][i] <- b_similarity_b_w[[j]][i,i+1]
    table_bray_b_w$rname[i] <- rownames(b_similarity_b_w[[j]])[i]
    table_bray_b_w$col_name[i] <- colnames(b_similarity_b_w[[j]])[i + 1]
  }
}
x_b_b_w
# view(x_b_b_w)
# x_b_b_w[[1]]
# 
# plot(x_b_b_w$B1P001)
# plot(x_b_b_w$B1P002)
# plot(x_b_b_w$B1P006)

plot_bray_b_w <- do.call(rbind, lapply(seq_along(x_b_b_w), function(j) {
  data.frame(
    Plot = names(x_b_b_w)[j],               # Correctly assign plot names
    Harvest = 1:length(x_b_b_w[[j]]),       # Harvest numbers
    Bray = unlist(x_b_b_w[[j]])          # Jaccard values
  )
})) %>%
  as.data.frame()

# view(plot_bray_b_w)

# compute the mean Bray index for each harvest
mean_bray_b_w <- plot_bray_b_w %>%
  group_by(Harvest) %>%
  summarize(MeanBray = mean(Bray))

# Plot with ggplot2
ggplot(plot_bray_b_w, aes(x = Harvest, y = Bray)) +
  # geom_line(data = mean_bray_b_w,
  #           aes(x = Harvest, y = MeanBray), 
  #           color = "red", size = 1.2) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Bray Index Trends Across Plots, Total, Biomass",
    x = "Harvest",
    y = "Bray-Curtis Similarity Index"
  ) +
  theme_minimal()

plot_bray_b_w %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Bray))+
  geom_line(aes(Harvest,Bray, color = Plot))

# View(plot_bray_a_bio)


# considering the biomass and sub setting
bray_b_aut <- list()

data_ar <- data_a %>% 
  arrange(id_harvest)

id_harvest_a <- unique(data_ar$id_harvest)

for(i in plot_numbers){
  bray_b_aut[[i]] <- data_au[[i]] %>%
    mutate(id_harvest_a = id_harvest) %>% 
    arrange(id_harvest_a) %>% 
    filter(6 <= id_harvest_a)%>%
    select(id_harvest_a,
           id_plant,
           biomass)
}


length(id_harvest_a)
# View(bray_aut_bio)

bray_b_spr <- list()

data_sr <- data_s %>% 
  arrange(id_harvest)

id_harvest_s <- unique(data_sr$id_harvest)

for(i in plot_numbers){
  bray_b_spr[[i]] <- data_sp[[i]] %>% 
    mutate(id_harvest_s = id_harvest) %>% 
    arrange(id_harvest_s) %>% 
    filter(6 <= id_harvest_s) %>% 
    select(id_harvest_s,
           id_plant,
           biomass)
}

length(id_harvest_s)

# View(bray_spr_bio)
# View(bray_spr_bio[["B1P043"]])
# we proceed with changing the structure of the data
# i want to have the biomass as columns
# the harvest as rows
# and the values as the biomass

bray_b_autu <- list()

for(i in plot_numbers){
  bray_b_autu[[i]] <- bray_b_aut[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = biomass,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest_a")
}

# View(bray_autu_bio)

bray_b_spri <- list()

for(i in plot_numbers){
  bray_b_spri[[i]] <- bray_b_spr[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = biomass,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest_s")
}

# View(bray_spri_bio)


b_similarity_b_a <- list()

for (i in plot_numbers) {
  
  b_dissimilarity_b_a <- bray_b_autu[[i]] %>%
    vegdist(method = "bray",
            na.rm = TRUE)
  
  b_similarity_b_a[[i]] <- 1 - as.matrix(b_dissimilarity_b_a) 
}

b_similarity_b_a

# View(b_similarity_a_bio)
# view(b_similarity_a_bio[["B1P001"]])
# plot(b_similarity_a_bio[["B1P001"]])
# lapply(b_similarity_a_bio, summary)

b_similarity_b_s <- list()

for(i in plot_numbers){
  
  b_dissimilarity_b_s <- bray_b_spri[[i]] %>% 
    vegdist(method = "bray",
            na.rm = TRUE)
  
  b_similarity_b_s[[i]] <- 1 - as.matrix(b_dissimilarity_b_s)  
}


# View(b_similarity_b_s)
# view(b_similarity_b_s[["B1P001"]])
# plot(b_similarity_b_s[["B1P001"]])
# lapply(b_similarity_b_s, summary)

bray_har_b_a <- as.numeric(rownames(b_similarity_b_a[[1]]))
bray_har_b_s <- as.numeric(rownames(b_similarity_b_s[[1]]))

length(bray_har_b_a) #23
length(bray_har_b_s) #26


table_bray_b_a <- data.frame(number = 1:22,
                           rname = NA,
                           col_name = NA)

table_bray_b_s <- data.frame(number = 1:25,
                           rname = NA,
                           col_name = NA)

x_b_b_a <- list()

x_b_b_s <- list()

for(j in plot_numbers){
  for (i in 1:22){ #23 harvests
    x_b_b_a[[j]][i] <- b_similarity_b_a[[j]][i,i+1]
    table_bray_b_a$rname[i] <- rownames(b_similarity_b_a[[j]])[i]
    table_bray_b_a$col_name[i] <- colnames(b_similarity_b_a[[j]])[i + 1]
  }
}
x_b_b_a
# view(x_b_b_a)
# x_b_b_a[[1]]
# 
# plot(x_b_b_a$B1P001)
# plot(x_b_b_a$B1P002)
# plot(x_b_b_a$B1P006)

for(j in plot_numbers){
  for(i in 1:25){
    x_b_b_s[[j]][i] <- b_similarity_b_s[[j]][i,i+1]
    table_bray_b_s$rname[i] <- rownames(b_similarity_b_s[[j]])[i]
    table_bray_b_s$col_name[i] <- colnames(b_similarity_b_s[[j]])[i + 1]
  }
}

x_b_b_s
# view(x_b_b_s)
# x_b_b_s[[1]]
# view(x_b_b_s$B1P009)
# plot(x_b_b_s$B1P001)
# plot(x_b_b_s$B1P002)
# plot(x_b_b_s$B1P006)

plot_bray_b_a <- do.call(rbind, lapply(seq_along(x_b_b_a), function(j) {
  data.frame(
    Plot = names(x_b_b_a)[j],               # Correctly assign plot names
    Harvest = 1:length(x_b_b_a[[j]]),       # Harvest numbers
    Bray = unlist(x_b_b_a[[j]])          # Jaccard values
  )
})) %>%
  as.data.frame()

# view(plot_bray_b_a)

plot_bray_b_s <- do.call(rbind, lapply(seq_along(x_b_b_s), function(j) {
  data.frame(
    Plot = names(x_b_b_s)[j],
    Harvest = 1:length(x_b_b_s[[j]]),
    Bray = unlist(x_b_b_s[[j]])
  )
})) %>% 
  as.data.frame()

# view(plot_bray_b_s)


# compute the mean Bray index for each harvest
mean_bray_b_a <- plot_bray_b_a %>%
  group_by(Harvest) %>%
  summarize(MeanBray = mean(Bray))

#View(mean_bray_b_a)

mean_bray_b_s <- plot_bray_b_s %>% 
  group_by(Harvest) %>% 
  summarize(MeanBray = mean(Bray))


# Plot with ggplot2
ggplot(plot_bray_b_a, aes(x = Harvest, y = Bray)) +
  # geom_line(data = mean_bray_b_a,
  #           aes(x = Harvest, y = MeanBray), 
  #           color = "red", size = 1.2) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Bray Index Trends Across Plots, Autumn, Biomass",
    x = "Harvest",
    y = "Bray-Curtis Similarity Index"
  ) +
  theme_minimal()

ggplot(plot_bray_b_s, aes(x = Harvest, y = Bray)) +
  # geom_line(data = mean_bray_b_s,
  #           aes(Harvest, MeanBray),
  #           color = "red", size = 1.2) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Bray Index Trends Across Plots, Spring, Biomass",
    x = "Harvest",
    y = "Bray-Curtis Similarity Index"
  ) +
  theme_minimal()

plot_bray_b_a %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Bray))+
  geom_line(aes(Harvest,Bray, color = Plot))


plot_bray_b_s %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Bray))+
  geom_line(aes(Harvest, Bray, color = Plot))




#---- Bray-Curtis Index mixed ----
?vegdist

# considering the cover 2x2 and the biomass
bray_aut_bio <- list()

data_ar <- data_a %>% 
  arrange(id_harvest)

id_harvest_a <- unique(data_ar$id_harvest)

for(i in plot_numbers){
  bray_aut_bio[[i]] <- data_au[[i]] %>%
    mutate(id_harvest_a = id_harvest) %>% 
    arrange(id_harvest_a) %>% 
    filter(6 <= id_harvest_a)%>% 
    drop_na(biomass) %>% 
    select(id_harvest_a,
           id_plant,
           biomass)
}


length(id_harvest_a)
# View(bray_aut_bio)

bray_spr_bio <- list()

data_sr <- data_s %>% 
  arrange(id_harvest)

id_harvest_s <- unique(data_sr$id_harvest)

for(i in plot_numbers){
  bray_spr_bio[[i]] <- data_sp[[i]] %>% 
    mutate(id_harvest_s = id_harvest) %>% 
    arrange(id_harvest_s) %>% 
    filter(6 <= id_harvest_s) %>% 
    drop_na(biomass) %>% 
    select(id_harvest_s,
           id_plant,
           biomass)
}

length(id_harvest_s)

# View(bray_spr_bio)
# View(bray_spr_bio[["B1P043"]])
# we proceed with changing the structure of the data
# i want to have the biomass as columns
# the harvest as rows
# and the values as the biomass

bray_autu_bio <- list()

for(i in plot_numbers){
  bray_autu_bio[[i]] <- bray_aut_bio[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = biomass,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest_a")
}

View(bray_autu_bio)

bray_spri_bio <- list()

for(i in plot_numbers){
  bray_spri_bio[[i]] <- bray_spr_bio[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = biomass,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_harvest_s")
}

View(bray_spri_bio)


b_similarity_a_bio <- list()

for (i in plot_numbers) {
  
  b_dissimilarity_a_bio <- bray_autu_bio[[i]] %>%
    vegdist(method = "bray")
  
  b_similarity_a_bio[[i]] <- 1 - as.matrix(b_dissimilarity_a_bio) 
}

b_similarity_a_bio

# View(b_similarity_a_bio)
# view(b_similarity_a_bio[["B1P001"]])
# plot(b_similarity_a_bio[["B1P001"]])
# lapply(b_similarity_a_bio, summary)

b_similarity_s_bio <- list()

for(i in plot_numbers){
  
  b_dissimilarity_s_bio <- bray_spri_bio[[i]] %>% 
    vegdist(method = "bray")
  
  b_similarity_s_bio[[i]] <- 1 - as.matrix(b_dissimilarity_s_bio)  
}


# View(b_similarity_s_bio)
# view(b_similarity_s_bio[["B1P001"]])
# plot(b_similarity_s_bio[["B1P001"]])
# lapply(b_similarity_s_bio, summary)

bray_har_a_bio <- as.numeric(rownames(b_similarity_a_bio[[1]]))
bray_har_s_bio <- as.numeric(rownames(b_similarity_s_bio[[1]]))

length(bray_har_a_bio) #23
length(bray_har_s_bio) #24


table_bray_a_bio <- data.frame(number = 1:22,
                               rname = NA,
                               col_name = NA)

table_bray_s_bio <- data.frame(number = 1:23,
                               rname = NA,
                               col_name = NA)

x_b_a_bio <- list()

x_b_s_bio <- list()

for(j in plot_numbers){
  for (i in 1:22){ #23 harvests
    x_b_a_bio[[j]][i] <- b_similarity_a_bio[[j]][i,i+1]
    table_bray_a_bio$rname[i] <- rownames(b_similarity_a_bio[[j]])[i]
    table_bray_a_bio$col_name[i] <- colnames(b_similarity_a_bio[[j]])[i + 1]
  }
}
x_b_a_bio
# view(x_b_a_bio)
# x_b_a_bio[[1]]
# 
# plot(x_b_a_bio$B1P001)
# plot(x_b_a_bio$B1P002)
# plot(x_b_a_bio$B1P006)

for(j in plot_numbers){
  for(i in 1:23){
    x_b_s_bio[[j]][i] <- b_similarity_s_bio[[j]][i,i+1]
    table_bray_s_bio$rname[i] <- rownames(b_similarity_s_bio[[j]])[i]
    table_bray_s_bio$col_name[i] <- colnames(b_similarity_s_bio[[j]])[i + 1]
  }
}

x_b_s_bio
# view(x_b_s_bio)
# x_b_s_bio[[1]]
# view(x_b_s$B1P009)
# plot(x_b_s_bio$B1P001)
# plot(x_b_s_bio$B1P002)
# plot(x_b_s_bio$B1P006)

plot_bray_a_bio <- do.call(rbind, lapply(seq_along(x_b_a_bio), function(j) {
  data.frame(
    Plot = names(x_b_a_bio)[j],               # Correctly assign plot names
    Harvest = 1:length(x_b_a_bio[[j]]),       # Harvest numbers
    Bray = unlist(x_b_a_bio[[j]])          # Jaccard values
  )
})) %>%
  as.data.frame()

view(plot_bray_a_bio)

plot_bray_s_bio <- do.call(rbind, lapply(seq_along(x_b_s_bio), function(j) {
  data.frame(
    Plot = names(x_b_s_bio)[j],
    Harvest = 1:length(x_b_s_bio[[j]]),
    Bray = unlist(x_b_s_bio[[j]])
  )
})) %>% 
  as.data.frame()

view(plot_bray_s_bio)


# compute the mean Bray index for each harvest
mean_bray_a_bio <- plot_bray_a_bio %>%
  group_by(Harvest) %>%
  summarize(MeanBray = mean(Bray))

mean_bray_s_bio <- plot_bray_s_bio %>% 
  group_by(Harvest) %>% 
  summarize(MeanBray = mean(Bray))


# Plot with ggplot2
ggplot(plot_bray_a_bio, aes(x = Harvest, y = Bray, color = Plot)) +
  geom_line(data = mean_bray_a_bio,
            aes(x = Harvest, y = MeanBray), 
            color = "red", size = 1.2) +
  labs(
    title = "Bray Index Trends Across Plots, Autumn, Biomass",
    x = "Harvest",
    y = "Bray-Curtis Similarity Index"
  ) +
  theme_minimal()

ggplot(plot_bray_s_bio, aes(x = Harvest, y = Bray)) +
  geom_line(data = mean_bray_s_bio,
            aes(Harvest, MeanBray),
            color = "red", size = 1.2) +
  labs(
    title = "Bray Index Trends Across Plots, Spring, Biomass",
    x = "Harvest",
    y = "Bray-Curtis Similarity Index"
  ) +
  theme_minimal()

plot_bray_a_bio %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Bray))+
  geom_line(aes(Harvest,Bray, color = Plot))

View(plot_bray_a_bio)

plot_bray_s_bio %>% 
  group_by(Plot) %>% 
  ggplot(aes(Harvest,Bray))+
  geom_line(aes(Harvest, Bray, color = Plot))





#---- Jaccard index all plots against all plots ----

tbl_j_all <- tbl_aufnahme %>% 
  mutate(species = 1) %>%
  filter(6 <= id_harvest) %>% 
  select(id_harvest,
         id_plot,
         id_plant,
         species) %>% 
  mutate(id_plot_numeric = as.numeric(as.factor(id_plot))) %>% 
  arrange(id_plot) %>% 
  arrange(id_harvest)

# View(tbl_j_all)
harvest_numbers <- unique(tbl_j_all$id_harvest)
single_harvests_j <- list()

for(i in harvest_numbers){
  single_harvests_j[[i]] <- tbl_j_all %>% 
    filter(id_harvest == i) %>% 
    select(id_plot_numeric,
           id_plant,
           species)
    
}

# View(single_harvests_j)

single_harvests_j_corrected <- single_harvests_j[-c(1, 2, 3, 4, 5, 7, 9, 11)]

# View(single_harvests_j_corrected)

single_harvests_prep <- list()

for(i in 1:length(single_harvests_j_corrected)){
  single_harvests_prep[[i]] <- single_harvests_j_corrected[[i]] %>% 
    pivot_wider(names_from = id_plant,
                values_from = species,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_plot_numeric")
}

# View(single_harvests_prep)

single_harvests_j_similarity <- list()

for (i in 1:length(single_harvests_prep)) {
  single_harvests_j_dissimilarity <- single_harvests_prep[[i]] %>%
    vegdist(method = "jaccard", binary = TRUE)
  
  single_harvests_j_similarity[[i]] <- 1 - as.matrix(single_harvests_j_dissimilarity) 
}

single_harvests_j_similarity
# View(single_harvests_j_similarity)
plot(single_harvests_j_similarity[[1]])
plot(single_harvests_j_similarity[[49]])

mean_values <- lapply(single_harvests_j_similarity, function(matrix) mean(matrix, na.rm = TRUE))

# Convert the result to a numeric vector (optional)
mean_values <- unlist(mean_values)

dataf_all_plots_j <- data.frame(harvest = 1:49,
                                j_index = mean_values)
plot(mean_values)

ggplot(dataf_all_plots_j,
       aes(harvest,
           mean_values))+
  geom_point() +
  geom_smooth() +
  labs(title = "Jaccard Index All Plots",
       x = "Harvest",
       y = "Jaccard Index")





min_max_values <- lapply(single_harvests_j_similarity, function(matrix) {
  list(
    min = min(matrix, na.rm = TRUE),
    max = max(matrix, na.rm = TRUE)
  )
})

# Convert to a data frame for easier viewing (optional)
min_max_df <- do.call(rbind, lapply(seq_along(min_max_values), function(i) {
  data.frame(
    Matrix = paste("Matrix", i),
    Min = min_max_values[[i]]$min,
    Max = min_max_values[[i]]$max
  )
}))

print(min_max_df)


# Step 1: Compute mean and flatten matrices for box whiskers
harvest_data <- lapply(seq_along(single_harvests_j_similarity), function(i) {
  matrix <- single_harvests_j_similarity[[i]]
  data.frame(
    harvest = i,
    mean_jaccard = mean(matrix, na.rm = TRUE), # Mean for the harvest
    all_values = as.vector(matrix) # Flattened values for box whiskers
  )
})

# Combine into a single data frame
harvest_df <- do.call(rbind, harvest_data)

# Step 2: Prepare for plotting
plot_data <- harvest_df %>%
  group_by(harvest) %>%
  summarise(
    mean_jaccard = mean(mean_jaccard), # Mean for the harvest
    all_values = list(all_values) # Collect all flattened values
  ) %>%
  unnest(all_values) # Unnest for box whiskers

# Step 3: Plot with ggplot
ggplot(plot_data, aes(x = factor(harvest), y = all_values)) +
  geom_boxplot() + # Box whiskers based on all Jaccard values
  geom_point(aes(y = mean_jaccard), color = "red", size = 2) + # Mean points
  labs(
    title = "Mean Jaccard Index and Distribution per Harvest",
    x = "Harvest",
    y = "Jaccard Index"
  ) +
  theme_minimal()


#---- Bray Curtis Index all plots against all plots, Cover ----

tbl_b_all <- tbl_data %>% 
  filter(6 <= id_harvest) %>% 
  select(id_harvest,
         id_plot,
         id_plant,
         cover_2x2) %>% 
  mutate(id_plot_numeric = as.numeric(as.factor(id_plot))) %>% 
  arrange(id_plot) %>% 
  arrange(id_harvest)
  
# View(tbl_b_all)
harvest_mnmbers_b_all <- unique(tbl_b_all$id_harvest)
b_c_all_plots <- list()
# View(data_w)

for(i in harvest_mnmbers_b_all){
  b_c_all_plots[[i]] <- tbl_b_all %>%
    filter(id_harvest == i) %>% 
    drop_na(cover_2x2) %>% 
    select(id_plot_numeric,
           id_plant,
           cover_2x2)
}

# View(b_c_all_plots)

b_c_all_plots_corrected <- b_c_all_plots[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)]

# View(b_c_all_plots_corrected)

# we proceed with changing the structure of the data
# i want to have the species as columns
# the plots as rows
# and the values as the cover_2x2, so what the cover of each species is

b_c_all_data <- list()

for(i in 1:length(b_c_all_plots_corrected)){
  b_c_all_data[[i]] <- b_c_all_plots_corrected[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = cover_2x2,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_plot_numeric")
}

# View(b_c_all_data)

b_c_similarity_all <- list()

for (i in 1:length(b_c_all_data)) {
  b_c_dissimilarity_all <- b_c_all_data[[i]] %>%
    vegdist(method = "bray")
  
  b_c_similarity_all[[i]] <- 1 - as.matrix(b_c_dissimilarity_all) 
}

b_c_similarity_all

# View(b_c_similarity_all)
plot(b_c_similarity_all[[1]])
plot(b_c_similarity_all[[47]])

mean_values_bray_c <- lapply(b_c_similarity_all, function(matrix) mean(matrix, na.rm = TRUE))

# Convert the result to a numeric vector (optional)
mean_values_bray_c <- unlist(mean_values_bray_c)

dataf_all_plots_b_c <- data.frame(harvest = 1:47,
                                j_index = mean_values_bray_c)
plot(mean_values_bray_c)

ggplot(dataf_all_plots_b_c,
       aes(harvest,
           mean_values_bray_c))+
  geom_point() +
  geom_smooth() +
  labs(title = "Bray-Curtis Index All Plots against all Plots, Cover",
       x = "Harvest",
       y = "Bray-Curtis Index")





min_max_values_b_c <- lapply(b_c_similarity_all, function(matrix) {
  list(
    min = min(matrix, na.rm = TRUE),
    max = max(matrix, na.rm = TRUE)
  )
})

# Convert to a data frame for easier viewing (optional)
min_max_df_b_c <- do.call(rbind, lapply(seq_along(min_max_values_b_c), function(i) {
  data.frame(
    Matrix = paste("Matrix", i),
    Min = min_max_values_b_c[[i]]$min,
    Max = min_max_values_b_c[[i]]$max
  )
}))

print(min_max_df_b_c)


# Step 1: Compute mean and flatten matrices for box whiskers
harvest_data_b_c <- lapply(seq_along(b_c_similarity_all), function(i) {
  matrix <- b_c_similarity_all[[i]]
  data.frame(
    harvest = i,
    mean_bray = mean(matrix, na.rm = TRUE), # Mean for the harvest
    all_values = as.vector(matrix) # Flattened values for box whiskers
  )
})

# Combine into a single data frame
harvest_df_b_c <- do.call(rbind, harvest_data_b_c)

# Step 2: Prepare for plotting
plot_data_b_c <- harvest_df_b_c %>%
  group_by(harvest) %>%
  summarise(
    mean_bray = mean(mean_bray), # Mean for the harvest
    all_values = list(all_values) # Collect all flattened values
  ) %>%
  unnest(all_values) # Unnest for box whiskers

# Step 3: Plot with ggplot
ggplot(plot_data_b_c, aes(x = factor(harvest), y = all_values)) +
  geom_boxplot() + # Box whiskers based on all Jaccard values
  geom_point(aes(y = mean_bray), color = "red", size = 2) + # Mean points
  labs(
    title = "Mean Bray-Curtis Index, Cover",
    x = "Harvest",
    y = "Bray-Curtis Index"
  ) +
  theme_minimal() 

#---- Bray Curtis Index all plots against all plots biomass ----

tbl_b_all_b <- tbl_data %>% 
  filter(6 <= id_harvest) %>% 
  select(id_harvest,
         id_plot,
         id_plant,
         biomass) %>% 
  mutate(id_plot_numeric = as.numeric(as.factor(id_plot))) %>% 
  arrange(id_plot) %>% 
  arrange(id_harvest)

# View(tbl_b_all_b)
harvest_mnmbers_b_all_b <- unique(tbl_b_all_b$id_harvest)
b_b_all_plots <- list()
# View(data_w)

for(i in harvest_mnmbers_b_all){
  b_b_all_plots[[i]] <- tbl_b_all_b %>%
    filter(id_harvest == i) %>% 
    drop_na(biomass) %>% 
    select(id_plot_numeric,
           id_plant,
           biomass)
}

# View(b_b_all_plots)

b_b_all_plots_corrected <- b_b_all_plots[-c(1, 2, 3, 4, 5, 7, 9, 11)]

# View(b_b_all_plots_corrected)

# we proceed with changing the structure of the data
# i want to have the species as columns
# the plots as rows
# and the values as the biomass, so what the cover of each species is

b_b_all_data <- list()

for(i in 1:length(b_b_all_plots_corrected)){
  b_b_all_data[[i]] <- b_b_all_plots_corrected[[i]] %>%
    pivot_wider(names_from = id_plant,
                values_from = biomass,
                values_fill = 0) %>% 
    column_to_rownames (var = "id_plot_numeric")
}

# View(b_b_all_data)

b_b_similarity_all <- list()

for (i in 1:length(b_b_all_data)) {
  b_b_dissimilarity_all <- b_b_all_data[[i]] %>%
    vegdist(method = "bray")
  
  b_b_similarity_all[[i]] <- 1 - as.matrix(b_b_dissimilarity_all) 
}

b_b_similarity_all

# View(b_b_similarity_all)
plot(b_b_similarity_all[[1]])
plot(b_b_similarity_all[[49]])

mean_values_bray_b <- lapply(b_b_similarity_all, function(matrix) mean(matrix, na.rm = TRUE))

# Convert the result to a numeric vector (optional)
mean_values_bray_b <- unlist(mean_values_bray_b)

dataf_all_plots_b_b <- data.frame(harvest = 1:49,
                                  j_index = mean_values_bray_b)
plot(mean_values_bray_b)

ggplot(dataf_all_plots_b_b,
       aes(harvest,
           mean_values_bray_b))+
  geom_point() +
  geom_smooth() +
  labs(title = "Bray-Curtis Index All Plots against all Plots, Biomass",
       x = "Harvest",
       y = "Bray-Curtis Index")





min_max_values_b_b <- lapply(b_b_similarity_all, function(matrix) {
  list(
    min = min(matrix, na.rm = TRUE),
    max = max(matrix, na.rm = TRUE)
  )
})

# Convert to a data frame for easier viewing (optional)
min_max_df_b_b <- do.call(rbind, lapply(seq_along(min_max_values_b_b), function(i) {
  data.frame(
    Matrix = paste("Matrix", i),
    Min = min_max_values_b_b[[i]]$min,
    Max = min_max_values_b_b[[i]]$max
  )
}))

print(min_max_df_b_b)


# Step 1: Compute mean and flatten matrices for box whiskers
harvest_data_b_b <- lapply(seq_along(b_b_similarity_all), function(i) {
  matrix <- b_b_similarity_all[[i]]
  data.frame(
    harvest = i,
    mean_bray_b = mean(matrix, na.rm = TRUE), # Mean for the harvest
    all_values_b = as.vector(matrix) # Flattened values for box whiskers
  )
})

# Combine into a single data frame
harvest_df_b_b <- do.call(rbind, harvest_data_b_b)

# Step 2: Prepare for plotting
plot_data_b_b <- harvest_df_b_b %>%
  group_by(harvest) %>%
  summarise(
    mean_bray_b = mean(mean_bray_b), # Mean for the harvest
    all_values_b = list(all_values_b) # Collect all flattened values
  ) %>%
  unnest(all_values_b) # Unnest for box whiskers

# Step 3: Plot with ggplot
ggplot(plot_data_b_b, aes(x = factor(harvest), y = all_values_b)) +
  geom_boxplot() + # Box whiskers based on all Jaccard values
  geom_point(aes(y = mean_bray_b), color = "red", size = 2) + # Mean points
  labs(
    title = "Mean Bray-Curtis Index all plots against all plots, Biomass",
    x = "Harvest",
    y = "Bray-Curtis Index"
  ) +
  theme_minimal() 

#---- GGplot2 ----

data()
BOD

ggplot(data = BOD,
       mapping = aes(x = Time,
                     y = demand)) +
  geom_point(size = 5) +
  geom_line(colour = "red")

ggplot(BOD, aes(Time, demand))+
  geom_point(size = 3)+
  geom_line(colour = "red")

CO2

CO2 %>% 
  ggplot(aes(conc, uptake, 
             colour = Treatment))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = lm, se = F)+
  facet_wrap(~Type)+
  labs(title = "Concentration of CO2")+
  theme_bw()

?ggplot



































# i have a large list of 64 tibbles

str(data[["B1P001"]])
View(data[["B1P001"]])
View(data[["B1P001"]]$id_plant)


transform_tibble_to_df <- function(tibble, row_col, col_col, value_col) {
  # Transform the tibble to a data frame with new rows and columns
  new_df <- tibble %>%
    pivot_wider(
      names_from = {{ col_col }},
      values_from = {{ value_col }},
      values_fill = 0  # You can specify a value to fill in missing entries, like 0
    )
  return(new_df)
}

new_df <- transform_tibble_to_df(data[["B1P001"]], data[["B1P001"]]$id_harvest,
                                 data[["B1P001"]]$id_plant,
                                 data[["B1P001"]]$species)
print(new_df)

view_df <- function(x){
  View(data[["B1P001"]]$ x)
  }
view_df(id_plant)

tbl_data <- subset(tbl_aufnahme, select = -c(id_plantcover, id_plot,
                                             cover_rahmen, cover_1x1,
                                             cover_2x2, biomass, bemerkung, scode_cov,
                                             scode_biom))
tbl_data


j_data <- data[["B1P001"]] +
  pivot_wider(names_from = data[["B1P001"]]$id_plant,
              values_from = data[["B1P001"]]$id_plant) + 
  
