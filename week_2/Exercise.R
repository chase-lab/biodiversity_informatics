rm(list = ls())

library(plyr)
library(dbplyr)
library(ggplot2)
library(ggfortify)
library(ggpubr)
library(devtools)
library(cowplot)
library(hypervolume)
library(dplyr)

# https://www.try-db.org/TryWeb/Home.php

# setwd(" ")
# 
# try     <- read.csv("TryAccSpecies_fromTRY_29Sep2021.csv")
# my_spec <- read.csv("OP_target_species.csv")
# 
# class(spec) # check what type of data it is.
# head(try$AccSpeciesName)
# 
# tmp = setdiff(spec$Scientific_name, try$AccSpeciesName) # find the species that are not in TRY
# 
# spec$AccSpeciesName = spec$Scientific_name
# tmp1 <- dplyr::semi_join(try, spec, by = "AccSpeciesName") # return all rows from x with a match in y
# write.csv(tmp1, file = " ") # here you get a csv file

#############
# ORGANIZING GLOBAL DATA AND ISLAND DATA

setwd("C:/_Teaching/Lecture_intro_Biodiver_Informatics_April2022/R_code")

GSPFF <- read.csv("Diaz_GSPFF_2016_traits.csv")
GSPFF <- dplyr::select(GSPFF, 
                       Species1 = Species.name.standardized.TPL.2016,
                          LA               = Leaf.Area..mm2.,                     # mm²
                          LMA              = LMA..g.m2.,                          # g/m²
                          Nmass            = Nmass..mg.g. ,                       # mg/g
                          SM               = Seed.Mass..mg.,                      # mg
                          SSD              = SSD.combined..mg.mm3.,               # mg/mm³
                          H                = Plant.Height..m.)                    # m
GSPFF$ori <- 'Global_data'
GSPFF[duplicated(GSPFF$Species1), ]
GSPFF   <- GSPFF[!duplicated(GSPFF$Species1), ]

data_Global <- data.frame(
  Species1      = GSPFF$Species1,
  Leaf_area     = scale(log10(GSPFF$LA)),
  LMA           = scale(log10(GSPFF$LMA)),
  Leaf_N        = scale(log10(GSPFF$Nmass)),
  Seed_mass     = scale(log10(GSPFF$SM)),
  Stem_density  = scale(log10(GSPFF$SSD)),
  Height        = scale(log10(GSPFF$H)),
  ori           = GSPFF$ori)

row.names(data_Global) <- data_Global$Species1

# Tenerife data

Tenerife_endemics <- read.csv("Barajas_Island_2022_traits1.csv")  
Tenerife_endemics$Species1 <- gsub("_", " ", Tenerife_endemics$Species1)
row.names(Tenerife_endemics) <- Tenerife_endemics$Species1

Tenerife_endemics$ori <- 'Island_data'

data_Island <- data.frame(
  Species1      = Tenerife_endemics$Species1,
  Leaf_area     = scale(log10(Tenerife_endemics$LA)),
  LMA           = scale(log10(Tenerife_endemics$LMA)),
  Leaf_N        = scale(log10(Tenerife_endemics$Nmass)),
  Seed_mass     = scale(log10(Tenerife_endemics$SM)),
  Stem_density  = scale(log10(Tenerife_endemics$SSD)),
  Height        = scale(log10(Tenerife_endemics$H)),
  ori           = Tenerife_endemics$ori)

row.names(Tenerife_endemics) <- Tenerife_endemics$Species1

# Join Island data with Global data
comparison <- rbind(data_Island, data_Global) # 2649 spp. First Tenerife data to be able to delete dupes from Diaz Global data.
comparison$dupes<-duplicated(comparison$Species1)# Creates a column to check for dupes
dplyr::filter(comparison, dupes == "TRUE")  # Diaz et al have 1 species from Tenerife
comparison   <- comparison[!duplicated(comparison$Species1), ] # 2535 spp. remove the 1 dupe from Diaz
anyDuplicated(comparison$Species1)

unique(comparison$ori) # makes sure Tenerife data is plotted on top of global one
te <- dplyr::filter (comparison, ori == "Island_data")
gl <- dplyr::filter (comparison, ori == "Global_data" )

comparison_1 <- rbind(gl, te) 

# FIGURE 1 ----

PCA         <- prcomp(comparison_1[,2:7])
summary(PCA) # check how much variance each PC explains : PC1 46%, PC2 25%
PCAvalues   <- data.frame(Species = comparison_1$Species1, ori = comparison_1$ori, PCA$x)# Extract PC axes for plotting
PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)          # Extract loading of the variables

p_my_theme <-  theme( axis.title.x= element_text(colour="black", size=7),
                      axis.title.y= element_text(colour="black", size=7),
                      axis.text.x= element_text(colour= "black", size=7),
                      axis.text.y= element_text(colour= "black", size=7),
                      panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank(),
                      panel.border=element_rect(fill=NA,colour="grey50"))

# Island versus global trait space  

Fig = ggplot(PCAvalues) +   
  geom_point(size = 2, alpha=0.9, 
             
             aes(x = PC1, y = PC2, group = ori, colour = ori, size = ori),
             
             show.legend = FALSE) +
  coord_fixed() + 
  scale_colour_manual(values=c("gray65","blue")) +
  scale_y_reverse()   +
  geom_segment(data = PCAloadings, size = 0.25,
               aes(x = 0, xend = PC1*3.5, y = 0, yend = PC2*3.5),
               arrow = arrow(length = unit(0.1, "cm")),colour = "black")   +
  geom_text(data = PCAloadings, aes(x = PC1*3.6, y = PC2*3.6, label = Variables), size = 2.3,
            hjust=c(0, 0, 0, 0, 0, 0) , vjust=c(0, 0, 0, 0, 0, 1))    + 
  xlab("PC1 (46%)") + ylab("PC2 (25%)")   +
  p_my_theme 

Fig

# Marginal density distribution island and global PCA  
xplot <- ggdensity(PCAvalues, "PC1", fill = "ori", color = "ori", palette = c("gray65","blue"))
yplot <- ggdensity(PCAvalues, "PC2", fill = "ori", color = "ori", palette = c("gray65","blue")) + rotate()

cowplot::plot_grid(xplot, NULL, Fig, yplot, ncol = 2, align = "hv", 
                   rel_widths = c(2, 1), rel_heights = c(1, 2))

# Island and global trait space overlap
overall_bandwidth <- estimate_bandwidth(PCAvalues[,3:5])

Tenerife  <- dplyr::filter(PCAvalues, ori== "Island_data")
HV_Tenerife <-hypervolume_gaussian(Tenerife[,3:5], name = "Island volume",
                                   kde.bandwidth=overall_bandwidth, quantile.requested=0.95)

Global  <- dplyr::filter(PCAvalues, ori== "Global_data")
HV_Global <-hypervolume_gaussian(Global[, 3:5], name = "Global volume",
                                 kde.bandwidth=overall_bandwidth, quantile.requested=0.95)

# Calculating Sørensen / overlap statistics
HVs <- hypervolume_join (HV_Tenerife, HV_Global)

HV_set <- hypervolume_set(HV_Tenerife, HV_Global, num.points.max = NULL,
                          verbose = TRUE, check.memory = F, distance.factor = 1)

HV_global_island_stats <- hypervolume_overlap_statistics(HV_set)
HV_global_island_stats
