## Code for reproducing results in Magrach et al. (2024):
# "Stability in plant-pollinator communities across 
#organizational levels: present, gaps, and future"
# Ainhoa Magrach, Bilbao, Bizkaia, Spain, 24-01-2024

library(tibble)
library(dplyr)

full.data <- as_tibble(
  read.csv("Input_data/full.data.csv", T))

full.data <- as_tibble(
  summary_df3)


# I will use a list from now on as I think it is easier to use lapply 

full.data2 <- split(full.data, f = full.data$System_var, drop = T)

length(full.data2)
names(full.data2) # here, each element is a unique combination of 
# a metacommunity and trophic group




## Arrange it the way Wang's function requires

full.data2 <- lapply(full.data2, function (x){
  arrange(x, Site_troph, Time_step, Sps)})

## Create the arrays

all.array <- lapply(full.data2, function (x){ 
  array(data = x$Abundance,
        dim = c(length(unique(x$Sps)), 
                length(unique(x$Time_step)), 
                length(unique(x$Site_troph))),
        dimnames = list(unique(x$Sps),
                        unique(x$Time_step),
                        unique(x$Site_troph)))
})

## Check if data is well organized as array
all.array[[1]][,,1] # this is site 1 along time


#===============================
## Run partitioning analysis following Wang et al. 2019  

part.W <- lapply(all.array, function (x) var.partition(x))
part.W



## Organize results
resul.all <- do.call(rbind, part.W) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "System_var") %>% 
  as_tibble() 
resul.all

# Organize per metacom
df.info.all <- lapply(full.data2, function (x) {
  data.frame(Meta = unlist(unlist(as.character(unique(na.omit(x$Meta))))),
             Meta_troph = unlist(unlist(as.character(unique(na.omit(x$Meta_troph))))),
             Freq = unlist(unlist(as.character(unique(na.omit(x$Freq))))),
             Time_step = unlist(unlist(as.numeric(unique(max(na.omit(x$Time_step)))))),
             System_var = unlist(unlist(as.character(unique(na.omit(x$System_var))))),
             nSites = unlist(unlist(as.numeric(length(unique(x$Site))))))})

df.info.all

resul.all2.metac <- Reduce(rbind, df.info.all) %>% 
  left_join(resul.all, by = "System_var") %>% 
  as_tibble()

resul.all2.metac




#================================
## Extract site level stability metrics

# This function was provided by Lise Comte
site_level_vars <- function (metacomm_tsdata) {
  
  ###calculate community synchrony (species synchrony in Wang & Loreau 2019)
  #
  ts_patch <- apply(metacomm_tsdata,c(2,3),sum) #total biomass of local comms per time
  sd_patch_k <- apply(ts_patch,2,sd) #temporal var. in comm biomass 
  sd_species_patch_ik <- apply(metacomm_tsdata,c(1,3),sd) #temporal var. in spp. biomass per comm.
  summed_sd_sp <- apply(sd_species_patch_ik,2,sum)  #summed SD across species
  synchrony_comm_site <- sd_patch_k/summed_sd_sp
  
  ###calculate mean species variability
  mean_species_patch_ik <-apply(metacomm_tsdata,c(1,3),mean)
  cv_species_site <- sd_species_patch_ik/mean_species_patch_ik
  mean_cv_species_site <- apply(cv_species_site, 2, mean, na.rm=T)
  
  ###calculate community variability
  #
  mean_patch_k <- apply(ts_patch,2,mean)
  cv_comm_site <- sd_patch_k/mean_patch_k
  
  return (tibble(Site_troph = names(cv_comm_site), synchrony_comm_site, 
                 mean_cv_species_site, cv_comm_site))
}

# Run the function
resul.site.metrics <- Reduce(rbind,
                             lapply(all.array, site_level_vars))

# Organize results
df.site.info <- Reduce(rbind,
                       lapply(full.data2, function (x) 
                         na.omit(distinct(x, Meta, Meta_troph, Site, Site_troph))))

resul.site.metrics2 <- left_join(resul.site.metrics, df.site.info, 
                                 by = "Site_troph")

# This data table should have the same number of rows as the number of sites in
# your study

resul.site.metrics2

#================================





#================================
## Extract biodiversity predictors

# I will use Simpson diversity index and observed species richness
# Create a list of lists where each element is a list of sites 

list.of.lists1 <- lapply(full.data2, function (x) split(x, f = x$Site_troph))

# e.g.,
with(list.of.lists1$`Flower availability`$`Pinar Hinojos Flower availability`, 
     table(Site, Time_step)) 
# 379 is the number of species names, including those with zero abundance
# and that's why all time_steps have same number

# Function to estimate Simpson diversity
# This will result in a measure of div per site per time step
# I am using the median of values across time steps as alpha diversity
# This is not computationally efficient, but I want to keep the same data 
# structure I used to estimate variability and synchrony

library(tidyr)

func_simpson <- function (x) {
  unlist(lapply(
    lapply(x, function (xx) {
      xx %>%
        pivot_wider(names_from = Sps, values_from = Abundance) %>%
        select(-Meta_troph:-Meta) %>%
        vegan::diversity(index = "simpson")}), 
    median))}

# Run it
alpha.resul1 <- lapply(list.of.lists1, func_simpson)

# Test if the function worked was expected
alpha.resul1$`Flower availability` 

median(apply(X = list.of.lists1$`Flower availability`$`Pinar Aznalcazar Flower availability` %>% 
               pivot_wider(names_from = Sps, values_from = Abundance) %>% 
               select(-Meta_troph:-Meta),
             MARGIN = 1,
             FUN = function (x) vegan::diversity(x, index = "simpson")))
#

## Now a function to calculate observed raw species richness
func_s <- function (x) {
  unlist(lapply(
    lapply(x, function (xx) {
      xx %>%
        pivot_wider(names_from = Sps, values_from = Abundance) %>%
        select(-Meta_troph:-Meta) %>%
        vegan::specnumber()}), 
    median))}

alpha.resul1.2 <- lapply(list.of.lists1, func_s)

# Test if the function worked was expected
alpha.resul1.2$`Flower availability` 

median(apply(X = list.of.lists1$`Flower availability`$`Pinar Aznalcazar Flower availability` %>% 
               pivot_wider(names_from = Sps, values_from = Abundance) %>% 
               select(-Meta_troph:-Meta),
             MARGIN = 1,
             FUN = function (x) vegan::specnumber(x)))
#

# Organize this so that it matches the format of the stability metric tibble
alpha.resul2 <- lapply(alpha.resul1, function (x) {
  tibble (Site_troph = names(x), Simp = x)
})

alpha.resul2.1 <- lapply(alpha.resul1.2, function (x) {
  tibble (Site_troph = names(x), S = x)
})

# Join this with site metrics data

resul.final.site <- do.call(rbind, alpha.resul2) %>%
  right_join(resul.site.metrics2, by = "Site_troph") %>% 
  right_join(do.call(rbind, alpha.resul2.1), by = "Site_troph") 

## Gamma diversity
# Again I will use Simpson index and species richness

# Prepare data
list1.gamma <- lapply(lapply(full.data2, function (x) { 
  x %>%
    group_by(Time_step, Sps) %>%
    summarise(Abundance_meta = sum(Abundance)) %>%
    pivot_wider(id_cols = Sps, names_from = Time_step, 
                values_from = Abundance_meta) %>%
    select(-Sps) %>%
    vegan::decostand(method = "pa")}), function (z) z[rowSums(z)>0,])

# Estimates
resul.gamma <- as_tibble(do.call(rbind, lapply(list1.gamma, function(x) {
  median(vegan::diversity(t(x), index = "simp"))}))) %>%
  mutate (Meta_troph = resul.all2.metac$Meta_troph, Simp_gamma = V1) %>%
  select(-V1)

resul.gamma2 <- as_tibble(do.call(rbind, lapply(list1.gamma, function(x) {
  median(vegan::specnumber(t(x)))}))) %>%
  mutate (Meta_troph = resul.all2.metac$Meta_troph, S_gamma = V1) %>%
  select(-V1)

# Create a Metacom data for this
resul.final.metac <- resul.final.site %>%
  select(Simp, S, Meta_troph) %>%
  group_by(Meta_troph) %>%
  summarise(across(.fns = mean)) %>%
  ungroup() %>%
  right_join(resul.all2.metac, by = "Meta_troph") %>%
  right_join(resul.gamma, by = "Meta_troph") %>% 
  right_join(resul.gamma2, by = "Meta_troph")

#=============================================================================
## Final organization and file export
# Bind the rows of the Lepas data sets

resul.final.site1 <- full.data %>% 
  select(Site_troph, System_var) %>% 
  distinct() %>% 
  left_join(resul.final.site, by = "Site_troph")

write_csv(resul.final.site1, "Input_data/site_stab_metrics.csv")

resul.final.metac %>% 
  write_csv("Input_data/meta_stab_metrics.csv")

## End
#=============================================================================




