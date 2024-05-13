##############################
#knowledge gaps and clusters##
##############################
rm(list=ls())

library(tidyverse)
library(cowplot)

#load in data
df<-read.csv("data/SYS_rev_article_list2.csv")
head(df)
colnames(df)
df2<-df[,1:26]

head(df2)
unique(df2$Type.of.study..empirical..theoretical..experimental..simulation.)

df3<-subset(df2, df2$Type.of.study..empirical..theoretical..experimental..simulation.!="")
unique(df3$Type.of.study..empirical..theoretical..experimental..simulation.)


unique(df3$Organizational.level)
df3$Organizational.level[df3$Organizational.level=="Pollinator visits"]<-"Visitation rates"
df3$Organizational.level[df3$Organizational.level=="Visitation rate (bumblebees and ground-nesting bees)"]<-"Visitation rates"
df3$Organizational.level[df3$Organizational.level=="Species and functions"]<-"Multiple"
df3$Organizational.level[df3$Organizational.level=="Ecosystem and species"]<-"Multiple"
df3$Organizational.level[df3$Organizational.level=="Flowers"]<-"Resource production"
df3$Organizational.level[df3$Organizational.level=="Interactions, species and functions"]<-"Multiple"
df3$Organizational.level[df3$Organizational.level=="Function (Fruit set)"]<-"Functions"
df3$Organizational.level[df3$Organizational.level=="Function (Gross Primary Productivity)"]<-"Functions"
df3$Organizational.level[df3$Organizational.level=="Function"]<-"Functions"
df3$Organizational.level[df3$Organizational.level=="Pollinator species and guild"]<-"Multiple"
df3$Organizational.level[df3$Organizational.level=="Species and interactions"]<-"Multiple"
df3$Organizational.level[df3$Organizational.level=="Resources, Species, community and function"]<-"Multiple"
df3$Organizational.level[df3$Organizational.level=="Species and community"]<-"Multiple"
df3$Organizational.level[df3$Organizational.level=="Nectar production"]<-"Resource production"
df3$Organizational.level[df3$Organizational.level=="Crop production (function)"]<-"Functions"
df3$Organizational.level[df3$Organizational.level=="Interactions and function (crop yield)"]<-"Multiple"
df3$Organizational.level[df3$Organizational.level=="Network"]<-"Community"

library(dplyr)
library(tidyr)
df4<-df3%>%
  
  group_by(Type.of.study..empirical..theoretical..experimental..simulation.,Organizational.level,.drop = FALSE)%>%
  summarise(heat_count=n_distinct(Article.ID))%>%
  ungroup %>%
  complete(Organizational.level, Type.of.study..empirical..theoretical..experimental..simulation., fill = list(heat_count = 0))

str(df4)
colnames(df4)<-c("Organizational.level", "Type.study", "heat_count")


df4$Organizational.level<-ordered(df4$Organizational.level, levels =c("Resource production", "Species", "Species roles",
                                                                      "Visitation rates","Interactions", "Community", "Functions", 
                                                                      "Multiple"))
df4b<-subset(df4, df4$Organizational.level!="NA")

ggplot(data=df4b,aes(x=Type.study,y=reorder(Organizational.level,desc(Organizational.level)),
                    fill=heat_count))+
  geom_tile(colour = "black")+
  theme_minimal()+
  scale_fill_continuous("No. of studies",low ="white",high="#1b9e77",na.value = "white",trans="sqrt")+
  theme(text=element_text(size=12),axis.text = element_text(size = 12))+
  ylab("Organizational level")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Study type")+
  theme(axis.text=element_text(size=16),
        text=element_text(size=16))+
  theme(legend.key.height = unit(1,"cm"))


ggsave(filename = "figures/organizational.level_study.type_heatmap.png",height=17,width=30,dpi = 300,units = "cm")

p1<-ggplot(data=df4b,aes(x=Type.study,y=reorder(Organizational.level,desc(Organizational.level)),
                    fill=heat_count))+
  geom_tile(colour = "black")+
  theme_minimal()+
  scale_fill_continuous("No. of studies",low ="white",high="#332288",na.value = "white",trans="sqrt")+
  theme(text=element_text(size=12),axis.text = element_text(size = 12))+
  ylab("Organizational level")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Study type")+
  theme(axis.text=element_text(size=16),
        text=element_text(size=16))+
  theme(legend.key.height = unit(1,"cm"))


unique(df3$Temporal.scale)
library(forcats)
df5<-df3%>%
  mutate(Temporal.scale=fct_recode(Temporal.scale,"Intra-annual"="Intra-annual Robustness to invasion measured in invaded vs un invaded plots",
                                   "Intra-annual"="Intra-annual: Weekly sampling for one year (47 sampling days)" ,
                                   "Simulation"="Before after simulated species removal",
                                   "Simulation"="Before after species removal",
                                   "Simulation"="Before after simulated perturbation",
                                   "Multiple"="Intra-annual & inter-annual. CV calculated for 6 periods of time within two consecutive years"))%>%
  
  group_by(Temporal.scale,Number.of.different.stability.metrics.calculated,.drop = FALSE)%>%
  summarise(heat_count=n_distinct(Article.ID))%>%
  ungroup %>%
  complete(Number.of.different.stability.metrics.calculated, Temporal.scale, fill = list(heat_count = 0))

unique(df5$Temporal.scale)
unique(df5$Number.of.different.stability.metrics.calculated)

df6<-subset(df5, df5$Temporal.scale!="")
df6<-subset(df6, df6$Number.of.different.stability.metrics.calculated!="NA")

ggplot(data=df6,aes(x=Temporal.scale,y=reorder(Number.of.different.stability.metrics.calculated,desc(Number.of.different.stability.metrics.calculated)),
                    fill=heat_count))+
  geom_tile(colour = "black")+
  theme_minimal()+
  scale_fill_continuous("No. of studies",low ="white",high="#1b9e77",na.value = "white",trans="sqrt")+
  theme(text=element_text(size=12),axis.text = element_text(size = 12))+
  ylab("Number of stability metrics")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Temporal scale")+
  theme(axis.text=element_text(size=16),
        text=element_text(size=16))+
  theme(legend.key.height = unit(1,"cm"))


ggsave(filename = "figures/temporal.scale_numer.metrics_heatmap.png",height=17,width=30,dpi = 300,units = "cm")


p2<-ggplot(data=df6,aes(x=Temporal.scale,y=reorder(Number.of.different.stability.metrics.calculated,desc(Number.of.different.stability.metrics.calculated)),
                    fill=heat_count))+
  geom_tile(colour = "black")+
  theme_minimal()+
  scale_fill_continuous("No. of studies",low ="white",high="#332288",na.value = "white",trans="sqrt")+
  theme(text=element_text(size=12),axis.text = element_text(size = 12))+
  ylab("Number of stability metrics")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Temporal scale")+
  theme(axis.text=element_text(size=16),
        text=element_text(size=16))+
  theme(legend.key.height = unit(1,"cm"))
#type of stability metric against spatial scale

unique(df$Stability.metric)
unique(df$Spatial.scale)

df$Spatial.scale[df$Spatial.scale=="SImulation"]<-"Simulation" 

df.na<-subset(df, df$Spatial.scale!="")

df.na$Spatial.scale <- factor(df.na$Spatial.scale, levels=c("Site", "Landscape", "National", "Supra-national", "Simulation"))
unique(df.na$Stability.metric)


df.na2<-df.na%>%
  group_by(Spatial.scale,Stability.metric,.drop = FALSE)%>%
  summarise(heat_count=n_distinct(Article.ID))%>%
  ungroup %>%
  complete(Stability.metric, Spatial.scale, fill = list(heat_count = 0))



ggplot(data=df.na2,aes(x=Spatial.scale,y=Stability.metric,
                    fill=heat_count))+
  geom_tile(colour = "black")+
  theme_minimal()+
  scale_fill_continuous("No. of studies",low ="white",high="#1b9e77",na.value = "white",trans="sqrt")+
  theme(text=element_text(size=12),axis.text = element_text(size = 12))+
  ylab("Stability dimension measured")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Spatial scale")+
  theme(axis.text=element_text(size=16),
        text=element_text(size=16))+
  theme(legend.key.height = unit(1,"cm"))

ggsave(filename = "figures/spatial.scale_type.metrics_heatmap.png",height=17,width=30,dpi = 300,units = "cm")


p3<-ggplot(data=df.na2,aes(x=Spatial.scale,y=Stability.metric,
                       fill=heat_count))+
  geom_tile(colour = "black")+
  theme_minimal()+
  scale_fill_continuous("No. of studies",low ="white",high="#1b9e77",na.value = "white",trans="sqrt")+
  theme(text=element_text(size=12),axis.text = element_text(size = 12))+
  ylab("Stability dimension measured")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Spatial scale")+
  theme(axis.text=element_text(size=16),
        text=element_text(size=16))+
  theme(legend.key.height = unit(1,"cm"))




head(df)
unique(df$Perturbation.type)

df$Perturbation.type2<-df$Perturbation.type

df$Perturbation.type2[df$Perturbation.type2=="Underlying variability"]<-"Underlying variation"
df$Perturbation.type2[df$Perturbation.type2=="Fragmentation"]<-"Landscape heterogeneity"
df$Perturbation.type2[df$Perturbation.type2=="Habitat restoration"]<-"Landscape heterogeneity"
df$Perturbation.type2[df$Perturbation.type2=="Introduction of field boundaries"]<-"Landscape heterogeneity"
df$Perturbation.type2[df$Perturbation.type2=="Distance to rainforest" ]<-"Distance to natural areas" 
df$Perturbation.type2[df$Perturbation.type2=="Pollinator exclusion" ]<-"Changing sps diversity"
df$Perturbation.type2[df$Perturbation.type2=="Species removal" ]<-"Changing sps diversity"
df$Perturbation.type2[df$Perturbation.type2=="Decreased pollinator diversity" ]<-"Changing sps diversity"
df$Perturbation.type2[df$Perturbation.type2=="SO"  ]<-"Changing sps diversity"
df$Perturbation.type2[df$Perturbation.type2=="Differing sps diversity" ]<-"Changing sps diversity"
df$Perturbation.type2[df$Perturbation.type2=="Small perturbation" ]<-"Other"
df$Perturbation.type2[df$Perturbation.type2=="Assembly" ]<-"Other"

df.b<-df
unique(df.b$Perturbation.type2)

df.b<-subset(df, df$Perturbation.type2!="")
df.b$Press_pulse<-df.b$Perturbation.type2
df.b$Press_pulse[df.b$Press_pulse=="Invasive species"]<-"Pulse"
df.b$Press_pulse[df.b$Press_pulse=="Changing sps diversity"]<-"Pulse"
df.b$Press_pulse[df.b$Press_pulse=="Changes in precipitation"]<-"Pulse"
df.b$Press_pulse[df.b$Press_pulse=="Other"]<-"Pulse"

df.b$Press_pulse[df.b$Press_pulse=="Landscape heterogeneity"]<-"Press"
df.b$Press_pulse[df.b$Press_pulse=="Distance to natural areas" ]<-"Press"
df.b$Press_pulse[df.b$Press_pulse=="Underlying variation" ]<-" "

unique(df.b$Press_pulse)



df.b2<-df.b%>%
  group_by(Perturbation.type2,Press_pulse,.drop = FALSE)%>%
  summarise(heat_count=n_distinct(Article.ID))%>%
  ungroup %>%
  complete(Press_pulse, Perturbation.type2, fill = list(heat_count = 0))



ggplot(data=df.b2,aes(x=Press_pulse,y=Perturbation.type2,
                       fill=heat_count))+
  geom_tile(colour = "black")+
  theme_cowplot()+
  scale_fill_continuous("No. of studies",low ="white",high="#1b9e77",na.value = "white",trans="sqrt")+
  theme(text=element_text(size=12),axis.text = element_text(size = 12))+
  ylab("Perturbation type")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Scale")+
  theme(axis.text=element_text(size=16),
        text=element_text(size=16))+
  theme(legend.key.height = unit(1,"cm"))

ggsave(filename = "figures/perturbation_type_heatmap.png",height=17,width=30,dpi = 300,units = "cm")


p4<-ggplot(data=df.b2,aes(x=Press_pulse,y=Perturbation.type2,
                          fill=heat_count))+
  geom_tile(colour = "black")+
  theme_cowplot()+
  scale_fill_continuous("No. of studies",low ="white",high="#1b9e77",na.value = "white",trans="sqrt")+
  theme(text=element_text(size=12),axis.text = element_text(size = 12))+
  ylab("Perturbation type")+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  xlab("Scale")+
  theme(axis.text=element_text(size=16),
        text=element_text(size=16))+
  theme(legend.key.height = unit(1,"cm"))

# library(cowplot)
# 
 Fig4<-plot_grid(
   p2, p1,
   labels = c("A)", "B)"), 
   #label_size = label_size,
   align = "h",  # Align the plots vertically
   axis = "l",   # Keep left axes aligned
   nrow = 1,
   ncol = 2
 ) 
 
# p3
# 
# 
# Fig2
p2-p1

ggsave("figures/Fig4.jpeg", width=45,height=14, units = "cm")

save(Fig4, p1,p2,p3, p4, file = "RData/figures_heatmaps.RData")


#create map of location of studies



# Load required libraries
library(tmap)
library(dplyr)
library(sf)


head(df)
colnames(df)

df.coun<-df[,c(1,10)]

unique(df.coun$Country)

df.coun$Country[df.coun$Country=="Brazil "]<-"Brazil" 
df.coun$Country[df.coun$Country=="UK"]<-"United Kingdom" 
df.coun$Country[df.coun$Country=="USA" ]<-"United States" 

# Count the number of studies per country
study_counts <- df.coun %>%
  group_by(Country) %>%
  summarize(NumStudies = n())

# Load world map data from tmap package
data("World")

# Merge study counts with world map data
world_map <- left_join(World, study_counts, by = c("name" = "Country"))


#install.packages("wesanderson")
library(wesanderson)


# Create a thematic map
pl.countr<-  tm_shape(world_map) +
  tm_borders() +
  tm_fill(col = "NumStudies", title = "Number of Studies", style = "cont", palette = wes_palettes$FantasticFox1)+
  tm_bubbles(size = "NumStudies", col = "#117733", border.col = "white", title.size = "Studies") +
  tm_text("NumStudies", size = 0.7)


save(pl.countr, file = "RData/figures_distribution_countries.RData")

tmap_save(pl.countr, filename = "figures/thematic_map.png", width = 10, height = 8, units = "in", dpi = 300)

#check number of simulation studies

sub<-df[,c(1,18)]
counts <- sub %>%
  group_by(Spatial.scale) %>%
  summarize(NumStudies = n())

head(df)
colnames(df)


#create rank abundance for habitat types
df.hab<-df[,c(1,12)]

unique(df.hab$Habitat.type)
df.hab$Habitat.type[df.hab$Habitat.type=="heathland"]<-"Heathland"
df.hab$Habitat.type[df.hab$Habitat.type=="Forest fragments"]<-"Forest"
df.hab$Habitat.type[df.hab$Habitat.type=="Forests"]<-"Forest"
df.hab$Habitat.type[df.hab$Habitat.type=="Tropical forest"]<-"Forest"
df.hab$Habitat.type[df.hab$Habitat.type=="Dry grasslands"]<-"Grassland"
df.hab$Habitat.type[df.hab$Habitat.type=="Agricultural landscapes"]<-"Agriculture"
df.hab$Habitat.type[df.hab$Habitat.type=="Coffee agroforests"]<-"Agriculture"
df.hab$Habitat.type[df.hab$Habitat.type=="Commercial watermelon and blueberry fields" ]<-"Agriculture"
df.hab$Habitat.type[df.hab$Habitat.type=="Calcareous grasslands"]<-"Grassland"
df.hab$Habitat.type[df.hab$Habitat.type=="oak savanna (12 networks), shrub–steppe\n(eight networks), and restored hedgerows (13 networks)."]<-"Multiple"
df.hab$Habitat.type[df.hab$Habitat.type=="Xeric grassland"]<-"Grassland"
df.hab$Habitat.type[df.hab$Habitat.type=="Almond cropland" ]<-"Agriculture"
df.hab$Habitat.type[df.hab$Habitat.type=="Olea europaea plots" ]<-"Agriculture"
df.hab$Habitat.type[df.hab$Habitat.type=="Olive orchards"  ]<-"Agriculture"
df.hab$Habitat.type[df.hab$Habitat.type=="Tropical dry forest" ]<-"Forest"
df.hab$Habitat.type[df.hab$Habitat.type=="4 Natural chaparral, 5 diversified organic farms, 7 organic monoculture farms"   ]<-"Agriculture"
df.hab$Habitat.type[df.hab$Habitat.type=="Cerrado"  ]<-"Forest"
df.hab$Habitat.type[df.hab$Habitat.type=="Temperate"   ]<-"Dryland"
df.hab$Habitat.type[df.hab$Habitat.type=="Varied"  ]<-"Multiple"
df.hab$Habitat.type[df.hab$Habitat.type=="Secondary forest in islands and mainland"  ]<-"Forest"
df.hab$Habitat.type[df.hab$Habitat.type=="Norwood Farm"   ]<-"Agriculture"
df.hab$Habitat.type[df.hab$Habitat.type=="Agricultural landscapes (21 crop species across the world)"   ]<-"Agriculture"
df.hab$Habitat.type[df.hab$Habitat.type=="Residential gardens"   ]<-"Urban"
df.hab$Habitat.type[df.hab$Habitat.type=="Urbanization gradient from cities to farmland"  ]<-"Urban"
df.hab$Habitat.type[df.hab$Habitat.type=="Burch forests"  ]<-"Forest"
df.hab$Habitat.type[df.hab$Habitat.type=="Neotropical Atlantic Rainforest"  ]<-"Forest"
df.hab$Habitat.type[df.hab$Habitat.type=="Deciduous forests in New Jersey and agricultural landscapes in California"        ]<-"Multiple"
df.hab$Habitat.type[df.hab$Habitat.type=="Agricultural landscape"]<-"Agriculture"


# Count the number of studies per habitat type
study_counts <- df.hab %>%
  group_by(Habitat.type) %>%
  summarize(NumStudies = n())



library(ggplot2)

# Sort the data by abundance in descending order
abundance_data <- study_counts[order(-study_counts$NumStudies), ]
abundance_data <- abundance_data[-1, ]
abundance_data <- abundance_data[-10, ]

# Create the ggplot
pl.habitat<-ggplot(abundance_data, aes(x = NumStudies, y = factor(Habitat.type, levels = unique(Habitat.type)))) +
  geom_segment(aes(xend = NumStudies, yend = factor(Habitat.type)), color = "darkgrey", size = 2) +
  geom_point(color = "#1b9e77", size = 3) +
  geom_segment(aes(x = 0, xend = NumStudies, y = factor(Habitat.type), yend = factor(Habitat.type)), 
               color = "black") +
  labs(x = "Number of Studies", y = "Habitat Type") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed")
  )
# To save the plot as an image file (e.g., PNG), you can use the ggsave function
# ggsave("rank_abundance_plot.png", width = 8, height = 6, units = "in")


#create rank abundance for different dimensions measured

# Count the number of studies per habitat type
study_counts <- df %>%
  group_by(Stability.metric) %>%
  summarize(NumStudies = n())



library(ggplot2)

# Sort the data by abundance in descending order
abundance_data <- study_counts[order(-study_counts$NumStudies), ]


# Create the ggplot
pl.metric<-ggplot(abundance_data, aes(x = NumStudies, y = factor(Stability.metric , levels = unique(Stability.metric )))) +
  geom_segment(aes(xend = NumStudies, yend = factor(Stability.metric )), color = "darkgrey", size = 2) +
  geom_point(color = "#1b9e77", size = 3) +
  geom_segment(aes(x = 0, xend = NumStudies, y = factor(Stability.metric ), yend = factor(Stability.metric )), 
               color = "black") +
  labs(x = "Number of Studies", y = "Stability dimension") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed")
  )

Fig3<-plot_grid(
  pl.habitat,pl.metric,
  labels = c("A)", "B)"), 
  #label_size = label_size,
  align = "h",  # Align the plots vertically
  axis = "l",   # Keep left axes aligned
  nrow = 1,
  ncol = 2
) 

ggsave("figures/Fig3.jpeg", width=45,height=14, units = "cm")

save(Fig3, pl.metric, pl.habitat, file = "RData/figures_rank_habitat.RData")

