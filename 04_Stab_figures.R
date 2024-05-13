# Here we prepare the figures and table we show in the main text. 

# Packages
library(tidyverse)
library(patchwork)
library(rcartocolor)
library(emmeans)
source("R_codes/Split_viol_plot.R")

## Code to partition variability and synchrony (Wang et al. 2019; Ecography)
source ("R_codes/var_part_wang.R") 

#===============================
## Data preparation
# Import data frame with results (stability partitions and diversity metrics)
# These were created with code: 
# "03_Stab_metrics.R"

meta.metr <- read_csv("Input_data/meta_stab_metrics.csv", T)
site.metr <- read_csv("Input_data/site_stab_metrics.csv", T)

## Order levels
meta.metr$System_var <- factor(meta.metr$System_var, 
                             levels = c("Flower availability","Pollinator visitation rate",
                                        "Pollinator interaction frequencies", "Fruit set"))

site.metr$System_var <- factor(site.metr$System_var, 
                               levels = c("Flower availability","Pollinator visitation rate",
                                          "Pollinator interaction frequencies", "Fruit set"))





#===============================

## Figures
## Prep figure with metacom partitions

# Move to long format
meta.metr.long <- 
  pivot_longer(meta.metr,
               cols = c(CV_S_L, CV_C_L, CV_S_R, CV_C_R, phi_S_L2R, phi_C_L2R,
                        phi_S2C_L, phi_S2C_R),
               names_to = "Partition", values_to = "Value_part")
#
meta.metr.long$System_var <- 
  factor(meta.metr.long$System_var, 
         levels = c("Flower availability","Pollinator visitation rate",
                    "Pollinator interaction frequencies", "Fruit set"))

with(meta.metr.long, unique(Partition))

# Choose colors
display_carto_all(colorblind_friendly = T)

# e.g.:
carto_pal(12, "Safe")

[1] "#88CCEE" "#CC6677" "#DDCC77" "#117733" "#332288" "#AA4499" "#44AA99" "#999933"
[9] "#882255" "#661100" "#6699CC" "#888888"

col_meta_cv <- "#882255"
col_com_cv <- "#6699CC"
col_pop_cv <- "#999933"

col_syn_pop <- "#117733"
col_syn_com <- "#888888"

#======
# Plots

a1 <- meta.metr.long %>%
  filter (Partition == "CV_S_L" | Partition == "CV_C_L" | Partition == "CV_C_R") %>%
  mutate (Partition = factor(Partition, levels = c("CV_S_L", "CV_C_L", "CV_C_R"))) %>%
  ggplot(aes(y=1/Value_part, x=System_var, fill = Partition)) +
  geom_dotplot(alpha = 0.7, binaxis = "y", stackdir = "center", 
               position = "dodge", dotsize = 1, binwidth = 1/17, aes(color = Partition)) +
  geom_violin(trim = F, draw_quantiles = c(0.5), alpha = 0.7, scale = "count",
              bw = 0.2) +
  scale_fill_manual(name = "", 
                    labels = c("Population", "Community", "Metacommunity"),
                    values = c(col_pop_cv, col_com_cv, col_meta_cv)) +
  scale_color_manual(name = "", 
                     labels = c("Population", "Community", "Metacommunity"),
                     values = c(col_pop_cv, col_com_cv, col_meta_cv)) +
  ylab("Invariability (1/CV)") + xlab ("") +
  ggtitle("(a)") +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12), 
        legend.position = c(x=0.1, y=0.95),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.text = element_text(size = 11),
        legend.key.size = unit(0.75,"line")) 





a2 <- meta.metr.long %>%   
  filter (Partition == "phi_S2C_L" | Partition == "phi_C_L2R") %>%
  mutate (Partition = factor(Partition, levels = c("phi_S2C_L", "phi_C_L2R"))) %>%
  ggplot(aes(y=Value_part, x=System_var, fill = Partition)) +
  geom_dotplot(alpha = 0.7, binaxis = "y", stackdir = "center", 
               position = "nudge", dotsize = 4, binwidth = 1/50, aes(color = Partition)) +
  geom_violin(trim = F, draw_quantiles = c(0.5), alpha = 0.7, 
              scale = "count") +
  scale_fill_manual(name = "", 
                    labels = c("Species", "Spatial"),
                    values = c(col_syn_pop, col_syn_com)) +
  scale_color_manual(name = "", 
                     labels = c("Species", "Spatial"),
                     values = c(col_syn_pop, col_syn_com)) +
  ylab("Synchrony")  + xlab ("Trophic levels") +
  scale_x_discrete(labels=c("Flower availability","Pollinator visitation rate",
                            "Pollinator interaction frequencies", "Fruit set")) +
  ylim(c(0, 1.25)) +
  ggtitle("(b)") +
  theme_classic() +
  theme(plot.title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12), 
        legend.position = c(x=0.1, y=0.3),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.text = element_text(size = 11),
        legend.key.size = unit(0.75,"line")) 







a1/a2

ggsave("figures/Fig5.jpeg", width=27,height=18, units = "cm")



# Test the differences suggested in the plot
#cannot really do this with points

df.cvs <- meta.metr.long %>%
  filter (Partition == "CV_S_L" | Partition == "CV_C_L" | Partition == "CV_C_R") %>%
  mutate (Partition = factor(Partition, levels = c("CV_S_L", "CV_C_L", "CV_C_R")))

mod.cvs <- lm(log(Value_part) ~ Partition*System_var, data = df.cvs)
plot(mod.cvs) # They look OK
anova(mod.cvs) # No interaction
summary(mod.cvs)




# Packages
library(tidyverse)
library(patchwork)
library(ggdist)
library(sf)
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
library(tmap)
library(nlme)

#===============================
# Site metrics
site.metr1.supp <- site.metr %>%
  mutate(New_ecos_tr = paste0(Ecosys, "_", New_tr_g)) %>%
  mutate(New_ecos_tr = case_when(
    New_ecos_tr == "Stream_Primary consumer" ~ "Lot_PriCon",
    New_ecos_tr == "Stream_Secondary consumer" ~ "Lot_SecCon",
    New_ecos_tr == "Stream_Tertiary consumer" ~ "Lot_TerCon",
    New_ecos_tr == "Stream_Producer" ~ "Lot_Prod",
    New_ecos_tr == "Lake_Producer" ~ "Len_Prod",
    New_ecos_tr == "Lake_Primary consumer" ~ "Len_PriCon")) %>% 
  mutate(New_ecos_tr = factor(New_ecos_tr,
                              levels = rev(c("Lot_TerCon", "Lot_SecCon",
                                             "Lot_PriCon", "Lot_Prod",
                                             "Len_PriCon", "Len_Prod"))))


ps6 <-  site.metr %>% 
  ggplot(aes(y = System_var, x = log(cv_comm_site), fill = System_var)) +
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.3) +
  stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA,
                    dotsize = 5, alpha = 0.75) +
  ylab ("") +
  xlab (expression("Community variability, CV" [italic("C,L")])) +
  ggtitle ("A") +
  theme_classic() +
  theme(axis.text = element_text(size = 17),
        axis.title = element_text(size = 20),
        legend.position = "none",
        plot.title = element_text(vjust = -8, hjust = 0.025, size = 18))
#
#

ps7 <- site.metr %>% 
  ggplot(aes(y = System_var, x = log(mean_cv_species_site), fill = System_var)) +
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA,
                    dotsize = 5, alpha = 0.75) +
  ylab ("") +
  xlab (expression("Population variability, CV" [italic("S,L")])) +
  theme_classic() +
  ggtitle ("B") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_blank(),
        plot.title = element_text(vjust = -8, hjust = 0.025, size = 18))
#

ps8 <- site.metr %>% 
  ggplot(aes(y = System_var, x = log(synchrony_comm_site), fill = System_var)) +
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.5) +
  stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA,
                    dotsize = 5, alpha = 0.75) +
  ylab ("") +
  xlab (expression("Population Synchrony " * (psi[italic("S C,L")]))) + 
  theme_classic() +
  ggtitle ("C") +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size = 17),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_blank(),
        plot.title = element_text(vjust = -8, hjust = 0.025, size = 18)) 
#
ps6+ps7+ps8


ggsave("figures/Fig6.jpeg", width=47,height=18, units = "cm")





meta.metr 
site.metr 



