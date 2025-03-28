
#=========================================================================================#

### UNIVERISTY OF CHINESE ACADEMY OF SCIENCE (UCAS)
### INSTITUTE OF URBAN ENVIRONMENT (IUE)
### NINGBO OBSERVATION AND RESEARCH STATION (NORS)
### project Name: EFFECTS OF URBANIZATION ON SOIL AND GRASS MICROBIOME AND RESISTOME
### Date: 28th, OCTOBER, 2023

#=======================================================================================#
#-rm(list=ls())
getwd()
#setwd ("C:/Users/Administrator/Desktop/Jack/Jack_Urbanization_on_soil-and_grass_Microbiome_and_resistome/data")
#setwd ("/Users/jacku/Desktop/Projects/PhD_Courses_Project/Research-Project/Jack_Urbanization_On_Soil_grass_Microbiome_and_resistome/data_in_use")
setwd("~/Desktop/Desktop/Projects/PhD_Courses_Project/Research-Project/Jack_Urbanization_On_Soil_grass_Microbiome_and_resistome/R-Scripts")
sessionInfo()  ## For nowing more information of R Version and Packages you have in your own Datasets

#========== Install and Loading Packages from CRAN ==========
#======= Loading required packages ========#
suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
  library(reshape2)
  library(dplyr)
  library(tidyr)
  library(MASS)
  library(vegan)
  library(gplots)
  library(ggplot2)
  library(ggthemes)
  library(colorspace)
  library(colorRamps)
  library(RColorBrewer)
  library(gridExtra)
  library(fitdistrplus)
  library(logspline)
  library(car)
  library(lme4) 
  library(multcomp)
  library(scales)
  library(ggridges)
  libraries <- c("ggridges", "viridis")
  library(corrplot)
  library(psych)
})
#=========================== Loading datasets ==============================#
Metadata <- read_excel("Urban_Ecosystems.xlsx", sheet = "metadata")
SoilProp<-read_excel("Urban_Ecosystems.xlsx", sheet =  "soil_properties")
EnzyAct<-read_excel("Urban_Ecosystems.xlsx", sheet =  "enzy_act")
ARGs <- read_excel("Urban_Ecosystems.xlsx", sheet = "ARGs")
ARGs_1 <- read_excel("Urban_Ecosystems.xlsx", sheet = "ARGs_1")
ARGs_taxa <- read_excel("Urban_Ecosystems.xlsx", sheet = "ARGs_taxa")
VGFs<-read_excel("Urban_Ecosystems.xlsx", sheet =  "VGFs")
VGFs_1<-read_excel("Urban_Ecosystems.xlsx", sheet =  "VGFs_1")
VGFs_Taxa<-read_excel("Urban_Ecosystems.xlsx", sheet =  "VGFs_Taxa")
Bact <- read_excel("Urban_Ecosystems.xlsx", sheet = "bacteria")
Bact_Taxa <- read_excel("Urban_Ecosystems.xlsx", sheet = "bacteria_taxa")
Fungi <- read_excel("Urban_Ecosystems.xlsx", sheet = "fungi")
Fungi_Taxa <- read_excel("Urban_Ecosystems.xlsx", sheet = "fungi_taxa")
Prot <- read_excel("Urban_Ecosystems.xlsx", sheet = "protists")
Prot_Taxa <- read_excel("Urban_Ecosystems.xlsx", sheet = "protist_taxa")
Bact_Phylum <- read_excel("Urban_Ecosystems.xlsx", sheet = "Bacteria_Phylum") 
#--- Soil_Properties <-read.csv("Soil_properties.csv",row.names=1,check.names=FALSE)

#=====================================================================================#
#.       ========= ANALYSIS OF ARGs & VGFs Abundance And Detected Numbers============#
#.       ========= USINGNG ARGs, ARGs_Taxa, VGFs, and VGFs_Taxa ======================#
#=====================================================================================#

##-----------------------------------SET WORKING DIRECTORIES FOR FIGURES _-----------------------------------------------------------#
setwd("~/Desktop/Desktop/Projects/PhD_Courses_Project/Research-Project/Jack_Urbanization_On_Soil_grass_Microbiome_and_resistome/R-Scripts/Z.Figures")

#       ==========     ARGs.    ================#

set.seed(123)
#-------------ORGANIZING MY DATASETS------------#
#---- Setting the variable to order
Order1 <- c("UP1","UP2","UP3","UP4","UP5","UP6","UP7","UP8","UP9","UP10","UP11","UP12","UP13","UP14","UP15","UP16","UP17","UP18","UP19","UP20","UP21","UP22","UP23","UP24","UP25","UP26","UP27","UP28","UP29","UP30","UP31","UP32","UP33","UP34","UP35","UP36","UP37","UP38","UP39","UP40","UP41","UP42","UP43","UP44","UP45","UP46","UP47","UP48","UP49","UP50","UP51","UP52","UP53","UP54","UP55","UP56","UP57","UP58","UP59","UP60")
order0 <- c("A1",	"A2",	"A3",	"A4",	"A5",	"A6",	"A7",	"A8",	"B1",	"B2",	"B3",	"B4",	"B5",	"B6",	"B7",	"B8",	"C1",	"C2",	"C3",	"C4",	"C5",	"C6",	"C7",	"C8",	"D1",	"D2",	"D3",	"D4",	"D5",	"D6",	"D7",	"D8",	"E1",	"E2",	"E3",	"E4",	"E5",	"E6",	"E7",	"E8")

#---- Setting the variable to remove
UP_remove_1 <- c("UP25","UP26","UP27","UP28","UP29","UP30","UP31","UP33","UP35","UP37","UP39","UP52","UP53","UP54","UP55","UP56","UP57","UP58","UP59","UP60")
UP_remove_1

#----------- DATA PREPARATION---- REMOVE SOME COLUMN NAMES UNDESIRED.

ARGs_1 <- ARGs_1[, c(paste0("ARGs"), paste0("A", 1:8), paste0("B", 1:8), paste0("C", 1:8), paste0("D", 1:8), paste0("E", 1:8), Order1), drop = FALSE]
ARGs_1 <- ARGs_1[, !(colnames(ARGs_1) %in% UP_remove_1)]
ARGs_1
names(ARGs_1)
##------------------------------------------------------##
VGFs_1 <- VGFs_1[, c(paste0("VGFs"), paste0("A", 1:8), paste0("B", 1:8), paste0("C", 1:8), paste0("D", 1:8), paste0("E", 1:8), Order1), drop = FALSE]
VGFs_1 <- VGFs_1[, !(colnames(VGFs_1) %in% UP_remove_1)]
names(VGFs_1)
VGFs_1
##------------------------------------------------------##
Bact <- Bact[, c(paste0("ASV"), paste0("A", 1:8), paste0("B", 1:8), paste0("C", 1:8), paste0("D", 1:8), paste0("E", 1:8), Order1), drop = FALSE] # Order Variables
Bact <- Bact[, !(colnames(Bact) %in% UP_remove_1)]
names(Bact)
##------------------------------------------------------##
Fungi <- Fungi[, c(paste0("ASV"), paste0("A", 1:8), paste0("B", 1:8), paste0("C", 1:8), paste0("D", 1:8), paste0("E", 1:8), Order1), drop = FALSE]
Fungi <- Fungi[, !(colnames(Fungi) %in% UP_remove_1)]
names(Fungi)
##------------------------------------------------------##
Prot <- Prot[, c(paste0("ASV"), paste0("A", 1:8), paste0("B", 1:8), paste0("C", 1:8), paste0("D", 1:8), paste0("E", 1:8), Order1), drop = FALSE]
Prot <- Prot[, !(colnames(Prot) %in% UP_remove_1)]
names(Prot)
##------Removing the addition up-id in Metadata----------##
Metadata <- Metadata [!(Metadata$SampleID %in%UP_remove_1), ]
#Metadata <- subset (Metadata, !(SampleID %in%UP_remove_1)) 
Metadata
#------------------- WORKING WITH ARG AND VGFs DATASETS NO TRANSPOSED -----------#
##------ ARGs -------##

# Create a new column for ordering
ARGs$order_column <- ifelse(grepl("^A", ARGs$SampleID), 
                            as.numeric(gsub("[^0-9]", "", ARGs$SampleID)),
                            match(ARGs$SampleID, Order1))
ARGs
#------------------- ARGs <- ARGs[order(ARGs$order_column), ]   # Order the data frame
#------------------- ARGs <- ARGs[, -ncol(ARGs)]   # Remove the temporary order column

#------------ REMOVE UNDESIRED phylosphere SAMPLES -------##
ARGs <- ARGs [!(ARGs$SampleID %in%UP_remove_1), ]
ARGs                            
##------ VGFs -------##
prefix <- substr(VGFs$SampleID, 1, 1)
numeric_part <- as.numeric(substr(VGFs$SampleID, 2, nchar(VGFs$SampleID)))
VGFs$order_column <- ifelse(prefix %in% c("A", "B", "C", "D", "E"),     # Create a new column for ordering
                            numeric_part + (as.numeric(prefix) - 1) * 8,
                            match(VGFs$SampleID, Order1))
VGFs <- VGFs[order(VGFs$order_column), ]   # Order the data frame
VGFs <- VGFs[, -ncol(VGFs)]   # Remove the temporary order column
VGFs
#--------------------------------------------------------------------------------------------
VGFs <- VGFs %>%
  mutate(SampleID = factor(SampleID, levels = c(order0, Order1))) 


#------------ REMOVE UNDESIRED phylosphere SAMPLES -------##
VGFs <- VGFs [!(VGFs$SampleID %in%UP_remove_1), ]
VGFs

#####============================== STARTING THE ANALYSIS =======================================##

#Soil_Properties <-read.csv("Soil_properties.csv",row.names=1,check.names=FALSE)
##===== We need to think back to the originali figure and reformat our data to have one column for the x-axix and another for the y-axis
ARGs_Abund <- ARGs_1 %>%
  pivot_longer(-ARGs, names_to = "SampleID", values_to = "sum")
ARGs_Abund

ARGs_Abund_1 <- ARGs_1 %>%
  pivot_longer(-ARGs, names_to = "Sample_id", values_to = "count")

##===== Merge with ARGs_Taxa Dataset
ARGs_Abund <- ARGs_Abund %>%
  left_join (ARGs_taxa, by = "ARGs")
ARGs_Abund

##===== Merge with Metadata Dataset
ARGs_Abund <- ARGs_Abund %>%
  left_join (Metadata, by = "SampleID")

####-------------------------------------------------------------------------------------------------

######################----------------------- WORKING WITH THE VFGs ---------------------#####
##===== We need to think back to the originali figure and reformat our data to have one column for the x-axix and another for the y-axis
VGFs_Abund <- VGFs_1 %>%
  pivot_longer(-VGFs, names_to = "SampleID", values_to = "sum")
VGFs_Abund

VGFs_Abund_1 <- VGFs_1 %>%
  pivot_longer(-VGFs, names_to = "Sample_id", values_to = "count")

##===== Merge with VGFs_Taxa Dataset
VGFs_Abund <- VGFs_Abund %>%
  left_join (VGFs_Taxa, by = "VGFs")
VGFs_Abund

##===== Merge with Metadata Dataset
VGFs_Abund <- VGFs_Abund %>%
  left_join (Metadata, by = "SampleID")

################--------------- 1st OPTION: SET THE DATASET THAT WILL BE USED FOR FOR GLAMMARS ------------------------------####

names(VGFs_Abund)
Urban_Resistomes_Soil1_VFGs<-subset(VGFs_Abund, Substrate!="phyllosphere")
Urban_Resistomes_Phyllo1_VFGs<-subset(VGFs_Abund, Substrate!="soil")

#######================ PREPARATION OF GAMMAS' DATASETS WITHIN ARGs ===========================#####

################--------------- 1st OPTION: SET THE DATASET THAT WILL BE USED FOR FOR GLAMMARS ------------------------------####
##---- K
##---------1. ARGs in SOIL
names(ARGs_Abund)
Urban_Resistomes_Soil1<-subset(ARGs_Abund, Substrate!="phyllosphere")
Urban_Resistomes_Phyllo1<-subset(ARGs_Abund, Substrate!="soil")

names(Urban_Resistomes_Soil1)
Urban_Resistomes_Soil1<- dplyr::rename(Urban_Resistomes_Soil1, Values=sum) # Renaming a new variables

##---- STEP 1: COUNTING THE ARGs which have value greater than 0

ARGs_GLAMMS_Soil <- Urban_Resistomes_Soil1 %>%
  group_by(SampleID, ARGs, Block) %>%
  summarise(pos_count = sum(Values > 0))
ARGs_GLAMMS_Soil 

library(dplyr)

# Ensure the "Values" column is correctly renamed if necessary
#----Urban_Resistomes_Soil1 <- dplyr::rename(Urban_Resistomes_Soil1, Values = sum)

# Step 1: Summarize ARGs, SampleID, and Block for non-zero Values
ARGs_GLAMMS_Soil <- Urban_Resistomes_Soil1 %>%
  group_by(SampleID, ARGs, Block) %>%
  summarise(
    pos_count = sum(Values > 0),   # Counts where Values are > 0
    .groups = 'drop'               # Drop groups after summarization to avoid collapsing data
  )

# Print the full summary
ARGs_GLAMMS_Soil

##---- STEP 2: FILTER THE ARGs WITH AT LEAST 3 VALUE WHICH ARE GREATER THAN 0 IN EACH SAMPLE
library(dplyr)
# Step 1: Filter ARGs with at least 3 occurrences where pos_count > 0 per SampleID
ARGs_GLAMMS_Soil2 <- ARGs_GLAMMS_Soil %>%
  group_by(ARGs) %>%
  filter(sum(pos_count > 0) >= 3) %>%
  ungroup()

##---- STEP 3: FILTER THE ORIGINAL DATASETS TO KEEP THE ARGs with the values which are greater than 0 IN AT LEAST 3 SAMPLES

ARGs_GLAMMS_Soil3 <- Urban_Resistomes_Soil1 %>%
  semi_join(ARGs_GLAMMS_Soil2 , by = "ARGs")
ARGs_GLAMMS_Soil3
names(ARGs_GLAMMS_Soil3)

##---------1. ARGs in PHYLLOSPHERE

Urban_Resistomes_Phyllo1<- dplyr::rename(Urban_Resistomes_Phyllo1, Values=sum) # Renaming a new variables

##---- STEP 1: COUNTING THE ARGs which have value greater than 0

ARGs_GLAMMS_Phyllo <- Urban_Resistomes_Phyllo1 %>%
  group_by(SampleID, ARGs, Block) %>%
  summarise(pos_count = sum(Values > 0))
ARGs_GLAMMS_Phyllo 
##---- STEP 2: FILTER THE ARGs WITH AT LEAST 3 VALUE WHICH ARE GREATER THAN 0 IN EACH SAMPLE

ARGs_GLAMMS_Phyllo2 <- ARGs_GLAMMS_Phyllo %>%
  group_by(ARGs) %>%
  filter(sum(pos_count > 0) >= 3) %>%
  ungroup() 
ARGs_GLAMMS_Phyllo2

##---- STEP 3: FILTER THE ORIGINAL DATASETS TO KEEP THE ARGs with the values which are greater than 0 IN AT LEAST 3 SAMPLES

ARGs_GLAMMS_Phyllo3 <- Urban_Resistomes_Phyllo1 %>%
  semi_join(ARGs_GLAMMS_Phyllo2 , by = "ARGs")
ARGs_GLAMMS_Phyllo3

names(ARGs_GLAMMS_Phyllo3)

####--------------------------------------------------------------------------------------------------

# Loading
library(readxl)
library(dplyr)

temp_Soil <- Urban_Resistomes_Soil1[Urban_Resistomes_Soil1$Values>0,]
###--detach(temp_Soil)
##--attach(temp_Soil)
##---- STEP 2: COUNTING NUMBER OF VALUES >0 BY ARGs and BLOCK
temp_Soil1<- Urban_Resistomes_Soil1  %>%
  group_by(ARGs,Block) %>%
  summarize(cnt_by_blk = n_distinct(Block))

##---- STEP 3: CHECKING WHICH ONES HAVE SAMPLES IN 2 OR MORE BLOCKS
temp_Soil2<- temp_Soil1  %>%
  group_by(ARGs) %>%
  summarize(ARG_by_blk = n_distinct(Block))
temp_Soil3<- temp_Soil2[temp_Soil2$ARG_by_blk>=2,]
##---- STEP 5: MERGE TEMP 5 WITH ORIGINAL DATASET TO GET BACK ALL ORIGINAL SAMPLES FOR THE ARGs THAT MEET THE CRITERIA

Urban_Resistomes_Soil4<- merge(x = Urban_Resistomes_Soil1, y = temp_Soil3, by = "ARGs", all.y = TRUE) ##---- Merge and keep All variables from Using DATASETS
Urban_Resistomes_Soil4<- subset(Urban_Resistomes_Soil4,select=-c(ARG_by_blk))
#######-----------------------------------------------------------------------------------------------------------------------######
###############=================================================================================================================#########
# Convert 'value' column to numeric
Urban_Resistomes_Soil1$Values <- as.numeric(Urban_Resistomes_Soil1$Values)

# Count the number of unique S.type categories for each Var2 where value > 0
counts <- aggregate(Block ~ ARGs, data = Urban_Resistomes_Soil1[Urban_Resistomes_Soil1$Values > 0, ], FUN = function(x) length(unique(x)))

# Filter Var2 based on the condition of at least 4 different S.type categories
filtered_Var2 <- counts[counts$Block >= 3, "ARGs"]

# Filter the original dataset based on the filtered Var2
Urban_Resistomes_Soil5 <- Urban_Resistomes_Soil1[Urban_Resistomes_Soil1$ARGs %in% filtered_Var2, ]
###############=================================================================================================================#########
Urban_Resistomes_Soil5
colSums(Urban_Resistomes_Soil5 != 0) ###--- Column with NON-ZERO Value: Total ARGs with NON-ZERO Value
Urban_Resistomes_Soil6 <- filter_if(Urban_Resistomes_Soil5, is.numeric, all_vars((.) != 0))  ##---// Remove raw with Zero
Urban_Resistomes_Soil6

####------- 3rd OPTION IS SOIL ------------------------------------

#####---------------count number of ARGs in each BLOCK in abundance are >0 ( INSHURO ARGs zigarukamo by BLOCK).
ARGs_GLAMMS_SoilKK <- Urban_Resistomes_Soil1 %>%
  group_by(ARGs, Block) %>%
  summarise(ncount = n_distinct(ifelse(Values > 0, SampleID, NA)))
##------------------------------------------------------------
####------- 4th Option OPTION IS SOIL ------------------------------------
#####---------------How many each ARGS appeared in Each SampleID with the Values appeared in EVERY BLOCK >0 
library(dplyr)
# STEP A: MAKE TOTAL SAMPLES WITH ABUNDANCE WHICH ARE GREATER THAN 0 (SUM BY SAMPLEID)
ARGs_GLAMMS_Soil11 <- Urban_Resistomes_Soil1 %>%
  group_by(ARGs, SampleID) %>%
  mutate(ncount = ifelse(Values > 0 & lead(Block) != Block, 1, 0)) %>%
  group_by(ARGs) %>%
  summarise(pos_count = sum(Values > 0),
            ncount = sum(ncount))
ARGs_GLAMMS_Soil11
# STEP B: IDENTIFY THE ARGs by BLOCK THAT DID NO have any values greater than 0.

library(dplyr)

# STEP B: IDENTIFY THE ARGs by BLOCK THAT DID NOT HAVE ANY VALUES GREATER THAN 0.
ARGs_GLAMMS_Soil111 <- Urban_Resistomes_Soil1 %>%
  arrange(ARGs, SampleID, Block) %>%  # Ensure data is ordered correctly
  group_by(ARGs) %>%
  summarise(
    pos_count = sum(Values > 0, na.rm = TRUE),  # Count non-zero occurrences
    in_A = as.integer(any(Values > 0 & Block == "A")),
    in_B = as.integer(any(Values > 0 & Block == "B")),
    in_C = as.integer(any(Values > 0 & Block == "C")),
    in_D = as.integer(any(Values > 0 & Block == "D")),
    in_E = as.integer(any(Values > 0 & Block == "E"))
  ) %>%
  mutate(ncount = in_A + in_B + in_C + in_D + in_E)  # Compute ncount as sum of blocks

# Print the first few rows
print(ARGs_GLAMMS_Soil111)
names (ARGs_GLAMMS_Soil111)

ARGs_GLAMMS_Soil111_2 <- ARGs_GLAMMS_Soil111 %>%
  filter((in_A != 0) + (in_B != 0) +(in_C != 0) + (in_D != 0) + (in_E != 0) >=3 & pos_count>3)
ARGs_GLAMMS_Soil111_9 <- ARGs_GLAMMS_Soil111 %>%
  filter((in_A != 0) + (in_B != 0) +(in_C != 0) + (in_D != 0) + (in_E != 0) <=3 & pos_count<3)

# Ensure the dataset is a data frame
ARGs_GLAMMS_Soil111_2 <- as.data.frame(ARGs_GLAMMS_Soil111_2)
# Check column names to verify correctness
print(colnames(ARGs_GLAMMS_Soil111_2))
# STEP C: KEEP DATASETS THAT ARE ABUNDANT AND FOUND IN MORE THAN ONE URBAN GRADIENT
ARGs_GLAMMS_Soil111_3 <- ARGs_GLAMMS_Soil111_2 %>%
  dplyr::filter(ncount >= 3) %>%  # Keep only ARGs found in at least 3 urban gradients
  dplyr::select(ARGs, pos_count, ncount, in_A, in_B, in_C, in_D, in_E)  # Select relevant columns
# Print the filtered dataset
print(ARGs_GLAMMS_Soil111_3)

# STEP D:KEEP THE FINAL DATASETS---------THAT CONTAINS NO MISSING VALUES
ARGs_GLAMMS_Soil111_4 <- merge(x = Urban_Resistomes_Soil1, y = ARGs_GLAMMS_Soil111_3, by = "ARGs", all.y = TRUE) ##---- Merge and keep All variables from Using DATASETS

ARGs_GLAMMS_Soil111_4 <- ARGs_GLAMMS_Soil111_4 %>%
  arrange(ARGs, SampleID, Block)

####------- 4th Option OPTION PHYLLOSPHERE ------------------------------------
names(Urban_Resistomes_Phyllo1)
# STEP A: MAKE TOTAL SAMPLES WITH ABUNDANCE WHICH ARE GREATER THAN 0 (SUM BY SAMPLEID)
ARGs_GLAMMS_Phyllo11 <- Urban_Resistomes_Phyllo1 %>%
  group_by(ARGs, SampleID, SampleID1) %>%
  mutate(ncount = ifelse(Values > 0 & lead(Block) != Block, 1, 0)) %>%
  group_by(ARGs) %>%
  summarise(pos_count = sum(Values > 0),
            ncount = sum(ncount))
Urban_Resistomes_Phyllo1
# STEP B: IDENTIFY THE ARGs by BLOCK THAT DID NO have any values greater than 0.
ARGs_GLAMMS_Phyllo111 <- Urban_Resistomes_Phyllo1 %>%
  arrange(ARGs, SampleID, Block) %>%  # Ensure data is ordered correctly
  group_by(ARGs) %>%
  summarise(
    pos_count = sum(Values > 0, na.rm = TRUE),  # Count non-zero occurrences
    in_A = as.integer(any(Values > 0 & Block == "A")),
    in_B = as.integer(any(Values > 0 & Block == "B")),
    in_C = as.integer(any(Values > 0 & Block == "C")),
    in_D = as.integer(any(Values > 0 & Block == "D")),
    in_E = as.integer(any(Values > 0 & Block == "E"))
  ) %>%
  mutate(ncount = in_A + in_B + in_C + in_D + in_E)  # Compute ncount as sum of blocks

# Print the first few rows
print(ARGs_GLAMMS_Phyllo111)
names (ARGs_GLAMMS_Phyllo111)

ARGs_GLAMMS_Phyllo111_2 <- ARGs_GLAMMS_Phyllo111 %>%
  filter((in_A != 0) + (in_B != 0) +(in_C != 0) + (in_D != 0) + (in_E != 0) >=3 & pos_count>3)
ARGs_GLAMMS_Phyllo111_9 <- ARGs_GLAMMS_Phyllo111 %>%
  filter((in_A != 0) + (in_B != 0) +(in_C != 0) + (in_D != 0) + (in_E != 0) <=3 & pos_count<3)

# Ensure the dataset is a data frame
ARGs_GLAMMS_Phyllo111_2 <- as.data.frame(ARGs_GLAMMS_Phyllo111_2)
# Check column names to verify correctness
print(colnames(ARGs_GLAMMS_Phyllo111_2))
# STEP C: KEEP DATASETS THAT ARE ABUNDANT AND FOUND IN MORE THAN ONE URBAN GRADIENT
ARGs_GLAMMS_Phyllo111_3 <- ARGs_GLAMMS_Phyllo111_2 %>%
  dplyr::filter(ncount >= 3) %>%  # Keep only ARGs found in at least 3 urban gradients
  dplyr::select(ARGs, pos_count, ncount, in_A, in_B, in_C, in_D, in_E)  # Select relevant columns
# Print the filtered dataset
print(ARGs_GLAMMS_Phyllo111_3)

# STEP D:KEEP THE FINAL DATASETS---------THAT CONTAINS NO MISSING VALUES
ARGs_GLAMMS_Phyllo111_4 <- merge(x = Urban_Resistomes_Phyllo1, y = ARGs_GLAMMS_Phyllo111_3, by = "ARGs", all.y = TRUE) ##---- Merge and keep All variables from Using DATASETS

ARGs_GLAMMS_Phyllo111_4 <- ARGs_GLAMMS_Phyllo111_4 %>%
  arrange(ARGs, SampleID, Block)
#######================ PREPARATION OF GAMMAS' DATASETS WITHIN VFGs ===========================#####

Urban_Resistomes_Soil1_VFGs
Urban_Resistomes_Phyllo1_VFGs
names(Urban_Resistomes_Soil1_VFGs)
names(Urban_Resistomes_Phyllo1_VFGs)

#####---------------How many each VGFs appeared in Each SampleID with the sum appeared in EVERY BLOCK >0 
library(dplyr)
Urban_Resistomes_Soil1_VFGs <- Urban_Resistomes_Soil1_VFGs %>%
  arrange(VGFs, SampleID1, Block)
# STEP A: MAKE TOTAL SAMPLES WITH ABUNDANCE WHICH ARE GREATER THAN 0 (SUM BY SAMPLEID)
VGFs_GLAMMS_Soil11 <- Urban_Resistomes_Soil1_VFGs %>%
  group_by(VGFs, SampleID1) %>%
  mutate(ncount = ifelse(sum > 0 & lead(Block) != Block, 1, 0)) %>%
  group_by(VGFs) %>%
  summarise(pos_count = sum(sum > 0),
            ncount = sum(ncount))
# STEP B: IDENTIFY THE ARGs by BLOCK THAT DID NOT HAVE ANY VALUES GREATER THAN 0.
VGFs_GLAMMS_Soil111 <- Urban_Resistomes_Soil1_VFGs %>%
  arrange(VGFs, SampleID1, Block) %>%  # Ensure data is ordered correctly
  group_by(VGFs) %>%
  summarise(
    pos_count = sum(sum > 0, na.rm = TRUE),  # Count non-zero occurrences
    in_A = as.integer(any(sum > 0 & Block == "A")),
    in_B = as.integer(any(sum > 0 & Block == "B")),
    in_C = as.integer(any(sum > 0 & Block == "C")),
    in_D = as.integer(any(sum > 0 & Block == "D")),
    in_E = as.integer(any(sum > 0 & Block == "E"))
  ) %>%
  mutate(ncount = in_A + in_B + in_C + in_D + in_E)  # Compute ncount as sum of blocks
# Print the first few rows
print(VGFs_GLAMMS_Soil111)
names (VGFs_GLAMMS_Soil111)

VGFs_GLAMMS_Soil111_2 <- VGFs_GLAMMS_Soil111 %>%
  filter((in_A != 0) + (in_B != 0) +(in_C != 0) + (in_D != 0) + (in_E != 0) >=3 & pos_count>3)
VGFs_GLAMMS_Soil111_9 <- VGFs_GLAMMS_Soil111 %>%
  filter((in_A != 0) + (in_B != 0) +(in_C != 0) + (in_D != 0) + (in_E != 0) <=3 & pos_count<3)

# Ensure the dataset is a data frame
VGFs_GLAMMS_Soil111_2 <- as.data.frame(VGFs_GLAMMS_Soil111_2)
# Check column names to verify correctness
print(colnames(VGFs_GLAMMS_Soil111_2))
# STEP C: KEEP DATASETS THAT ARE ABUNDANT AND FOUND IN MORE THAN ONE URBAN GRADIENT
VGFs_GLAMMS_Soil111_3 <- VGFs_GLAMMS_Soil111_2 %>%
  dplyr::filter(ncount >= 3) %>%  # Keep only VGFs found in at least 3 urban gradients
  dplyr::select(VGFs, pos_count, ncount, in_A, in_B, in_C, in_D, in_E)  # Select relevant columns
# Print the filtered dataset
print(VGFs_GLAMMS_Soil111_3)

# STEP D:KEEP THE FINAL DATASETS---------THAT CONTAINS NO MISSING sum
VGFs_GLAMMS_Soil111_4 <- merge(x = Urban_Resistomes_Soil1_VFGs, y = VGFs_GLAMMS_Soil111_3, by = "VGFs") ##---- Merge and keep All variables from Using DATASETS

VFGs_GLAMMS_Soil111_4 <- VGFs_GLAMMS_Soil111_4 %>%
  arrange(VGFs, SampleID, Block)

####------- 4th Option OPTION PHYLLOSPHERE ------------------------------------
# STEP A: MAKE TOTAL SAMPLES WITH ABUNDANCE WHICH ARE GREATER THAN 0 (SUM BY SAMPLEID)
VGFs_GLAMMS_Phyllo11 <- Urban_Resistomes_Phyllo1_VFGs %>%
  group_by(VGFs, SampleID, SampleID1) %>%
  mutate(ncount = ifelse(sum > 0 & lead(Block) != Block, 1, 0)) %>%
  group_by(VGFs) %>%
  summarise(pos_count = sum(sum > 0),
            ncount = sum(ncount))

# STEP B: IDENTIFY THE ARGs by BLOCK THAT DID NOT HAVE ANY VALUES GREATER THAN 0.
VGFs_GLAMMS_Phyllo111 <- Urban_Resistomes_Phyllo1_VFGs %>%
  arrange(VGFs, SampleID1, Block) %>%  # Ensure data is ordered correctly
  group_by(VGFs) %>%
  summarise(
    pos_count = sum(sum > 0, na.rm = TRUE),  # Count non-zero occurrences
    in_A = as.integer(any(sum > 0 & Block == "A")),
    in_B = as.integer(any(sum > 0 & Block == "B")),
    in_C = as.integer(any(sum > 0 & Block == "C")),
    in_D = as.integer(any(sum > 0 & Block == "D")),
    in_E = as.integer(any(sum > 0 & Block == "E"))
  ) %>%
  mutate(ncount = in_A + in_B + in_C + in_D + in_E)  # Compute ncount as sum of blocks
# Print the first few rows
print(VGFs_GLAMMS_Phyllo111)
names (VGFs_GLAMMS_Phyllo111)

VGFs_GLAMMS_Phyllo111_2 <- VGFs_GLAMMS_Phyllo111 %>%
  filter((in_A != 0) + (in_B != 0) +(in_C != 0) + (in_D != 0) + (in_E != 0) >=3 & pos_count>3)
VGFs_GLAMMS_Phyllo111_9 <- VGFs_GLAMMS_Phyllo111 %>%
  filter((in_A != 0) + (in_B != 0) +(in_C != 0) + (in_D != 0) + (in_E != 0) <=3 & pos_count<3)

# Ensure the dataset is a data frame
VGFs_GLAMMS_Phyllo111_2 <- as.data.frame(VGFs_GLAMMS_Phyllo111_2)
# Check column names to verify correctness
print(colnames(VGFs_GLAMMS_Phyllo111_2))
# STEP C: KEEP DATASETS THAT ARE ABUNDANT AND FOUND IN MORE THAN ONE URBAN GRADIENT
VGFs_GLAMMS_Phyllo111_3 <- VGFs_GLAMMS_Phyllo111_2 %>%
  dplyr::filter(ncount >= 3) %>%  # Keep only VGFs found in at least 3 urban gradients
  dplyr::select(VGFs, pos_count, ncount, in_A, in_B, in_C, in_D, in_E)  # Select relevant columns
# Print the filtered dataset
print(VGFs_GLAMMS_Phyllo111_3)

# STEP D:KEEP THE FINAL DATASETS---------THAT CONTAINS NO MISSING sum
VGFs_GLAMMS_Phyllo111_4 <- merge(x = Urban_Resistomes_Phyllo1_VFGs, y = VGFs_GLAMMS_Phyllo111_3, by = "VGFs") ##---- Merge and keep All variables from Using DATASETS

VFGs_GLAMMS_Phyllo111_4 <- VGFs_GLAMMS_Phyllo111_4 %>%
  arrange(VGFs, SampleID, Block)


#################===DATASETS TO USE FOR FIGURE OF VGFs AND VFGs MOVEMENENTS across DIFFERENTS URBAN GRADIENTS ======================================#########

ARGs_GLAMMS_Soil111_4<- dplyr::rename(ARGs_GLAMMS_Soil111_4, sum=Values) # Rename our variable values as it is with pivot-TABLE 
ARGs_GLAMMS_Phyllo111_4<- dplyr::rename(ARGs_GLAMMS_Phyllo111_4, sum=Values) # Rename our variable values as it is with pivot-TABLE 

ARGs_GLAMMS_Soil111_4
ARGs_GLAMMS_Phyllo111_4
VFGs_GLAMMS_Soil111_4
VFGs_GLAMMS_Phyllo111_4

###########################=================================== VANN DIAGRAMMS PLOTS ==============================##################################
####################################### ================== ARGs SOIL ====================########################

Urban_Resistomes_Soil1
Urban_Resistomes_Soil1_1 <- Urban_Resistomes_Soil1 %>%
  dplyr::select(SampleID, ARGs, Values, Urban_Gradient)
#Lets convert the data frame back into matrix:

Urban_Resistomes_Soil1_Vann <- dcast(Urban_Resistomes_Soil1_1, Urban_Gradient+SampleID~ARGs, value.var="Values")
rownames(Urban_Resistomes_Soil1_Vann)<-interaction(Urban_Resistomes_Soil1_Vann$Urban_Gradient,Urban_Resistomes_Soil1_Vann$SampleID, sep="_")
Urban_Resistomes_Soil1_Vann<-as.matrix(Urban_Resistomes_Soil1_Vann[,3:ncol(Urban_Resistomes_Soil1_Vann)])

# Core_Urban_ARGs
Core_Urban_ARGs<-as.matrix(Urban_Resistomes_Soil1_Vann[grep("A:Core_Urban_Areas",rownames(Urban_Resistomes_Soil1_Vann)),]
                   [, which(colSums(Urban_Resistomes_Soil1_Vann[grep("A:Core_Urban_Areas",rownames(Urban_Resistomes_Soil1_Vann)),]) >0)])
str(Core_Urban_ARGs)
Core_Urban_ARGs1<-colnames(Core_Urban_ARGs)
str(Core_Urban_ARGs1)
length(Core_Urban_ARGs1)
#215

Inner_Suburbs_ARGs<-as.matrix(Urban_Resistomes_Soil1_Vann[grep("B:Inner_Suburbs",rownames(Urban_Resistomes_Soil1_Vann)),]
                     [, which(colSums(Urban_Resistomes_Soil1_Vann[grep("B:Inner_Suburbs",rownames(Urban_Resistomes_Soil1_Vann)),]) >0)])
str(Inner_Suburbs_ARGs)
Inner_Suburbs_ARGs1<-colnames(Inner_Suburbs_ARGs)
str(Inner_Suburbs_ARGs1)
length(Inner_Suburbs_ARGs1)
#[1] 215

Outer_Surburbs_ARGs <-as.matrix(Urban_Resistomes_Soil1_Vann[grep("C:Outer_Surburbs",rownames(Urban_Resistomes_Soil1_Vann)),]
                    [, which(colSums(Urban_Resistomes_Soil1_Vann[grep("C:Outer_Surburbs",rownames(Urban_Resistomes_Soil1_Vann)),]) >0)])
str(Outer_Surburbs_ARGs)
Outer_Surburbs_ARGs1<-colnames(Outer_Surburbs_ARGs)
str(Outer_Surburbs_ARGs1)
length(Outer_Surburbs_ARGs1)
#[1] 215

Exurban_ARGs<-as.matrix(Urban_Resistomes_Soil1_Vann[grep("D:Exurban_Areas",rownames(Urban_Resistomes_Soil1_Vann)),]
                      [, which(colSums(Urban_Resistomes_Soil1_Vann[grep("D:Exurban_Areas",rownames(Urban_Resistomes_Soil1_Vann)),]) >0)])
str(Exurban_ARGs)
Exurban_ARGs1<-colnames(Exurban_ARGs)
str(Exurban_ARGs1)
length(Exurban_ARGs1)
#[1] 215

Rural_Fringe_ARGs<-as.matrix(Urban_Resistomes_Soil1_Vann[grep("E:Rural_Fringe",rownames(Urban_Resistomes_Soil1_Vann)),]
                        [, which(colSums(Urban_Resistomes_Soil1_Vann[grep("E:Rural_Fringe",rownames(Urban_Resistomes_Soil1_Vann)),]) >0)])
str(Rural_Fringe_ARGs)
Rural_Fringe_ARGs1<-colnames(Rural_Fringe_ARGs)
str(Rural_Fringe_ARGs1)
length(Rural_Fringe_ARGs1)
#[1] 215

library(VennDiagram)
input_ARGs_Soil<-list(Core_Urban= Core_Urban_ARGs1, Inner_Suburbs=Inner_Suburbs_ARGs1, Outer_Surburbs=Outer_Surburbs_ARGs1, Exurban= Exurban_ARGs1, Rural_Fringe= Rural_Fringe_ARGs1)

# Define the colors for the sets
set_colors <- c("#801818", "#E34234", "#FFD700", "#8F9779", "#03AC13")

# Define the intensity of colors for the intersection areas
int_colors <- list(
  Core_Urban_Inner_Suburbs = adjustcolor(set_colors[1], alpha.f = 0.5), # Adjust alpha value for transparency
  Core_Urban_Outer_Surburbs = adjustcolor(set_colors[1], alpha.f = 0.5),
  Core_Urban_Exurban = adjustcolor(set_colors[1], alpha.f = 0.5),
  Core_Urban_Rural_Fringe = adjustcolor(set_colors[1], alpha.f = 0.5),
  Inner_Suburbs_Outer_Surburbs = adjustcolor(set_colors[5], alpha.f = 0.5),
  Inner_Suburbs_Exurban = adjustcolor(set_colors[2], alpha.f = 0.5),
  Inner_Suburbs_Rural_Fringe = adjustcolor(set_colors[2], alpha.f = 0.5),
  Outer_Surburbs_Exurban = adjustcolor(set_colors[3], alpha.f = 0.5),
  Outer_Surburbs_Rural_Fringe = adjustcolor(set_colors[3], alpha.f = 0.5),
  Exurban_Rural_Fringe = adjustcolor(set_colors[4], alpha.f = 0.5)
)
# Create the Venn diagram
venn.plot <- venn.diagram(
  x = input_ARGs_Soil,
  category.names = names(input_ARGs_Soil),
  fill = set_colors,
  cat.fontfamily = "sans",
  cat.col = set_colors,
  cat.cex = 1.2,
  cat.pos = 0,
  margin = 0.05,
  main = "Venn Diagram of ARGs Soil Data",
  filename = NULL
)


# Plot the Venn diagram
grid.draw(venn.plot)

# Adjust the margin to control the boundary of the figure
margin_size <- 0.01  # Adjust this value as needed

# Create the Venn diagram with adjusted margin
venn.plot <- venn.diagram(
  x = input_ARGs_Soil,
  category.names = names(input_ARGs_Soil),
  fill = set_colors,
  cat.fontfamily = "sans",
  cat.col = set_colors,
  cat.cex = 1.2,
  cat.pos = 0,
  margin = margin_size,  # Adjusted margin size
  main = "Venn Diagram of the shared ARGs by different urban gradients soil",
  filename = NULL,
  euler.d = int_colors
)
# Plot the Venn diagram
grid.draw(venn.plot)
ggsave(filename = "Vann_Diagram_ARGs_In_Urban_Soil.png", plot = venn.plot, width = 20, height = 25, dpi = 2500, units = "cm")
ggsave(filename = "Vann_Diagram_ARGs_In_Urban_Soil.pdf", plot = venn.plot,width = 20, height = 25, dpi = 2500, units = "cm")
dev.off()
#####---------------------------------------------------------------------------------------------------------------------####
####----------- EXTRACT SUMMARY TABLE OF Vann_Diagram_ARGs_In_Urban_Soil
Summary_Table_Vann_Diagram_ARGs_In_Urban_Soil <- venn(input_ARGs_Soil, category.names= names(input_ARGs_Soil), show.plot = FALSE)
write.csv(Summary_Table_Vann_Diagram_ARGs_In_Urban_Soil, "Vann_Diagram_ARGs_In_Urban_Soil.csv", row.names = FALSE)
#####---------------------------------------------------------------------------------------------------------------------------######


####################################### ================== ARGs Phyllo ====================########################

Urban_Resistomes_Phyllo1
Urban_Resistomes_Phyllo1$sum <- as.numeric(Urban_Resistomes_Phyllo1$Values)
Urban_Resistomes_Phyllo1_1 <- Urban_Resistomes_Phyllo1 %>%
  dplyr::select(SampleID, ARGs, sum, Urban_Gradient)
#Lets convert the data frame back into matrix:

Urban_Resistomes_Phyllo1_Vann <- dcast(Urban_Resistomes_Phyllo1_1, Urban_Gradient+SampleID~ARGs, value.var="sum")
rownames(Urban_Resistomes_Phyllo1_Vann)<-interaction(Urban_Resistomes_Phyllo1_Vann$Urban_Gradient,Urban_Resistomes_Phyllo1_Vann$SampleID, sep="_")
Urban_Resistomes_Phyllo1_Vann<-as.matrix(Urban_Resistomes_Phyllo1_Vann[,3:ncol(Urban_Resistomes_Phyllo1_Vann)])

# Core_Urban_ARGs
Core_Urban_ARGs<-as.matrix(Urban_Resistomes_Phyllo1_Vann[grep("A:Core_Urban_Areas",rownames(Urban_Resistomes_Phyllo1_Vann)),]
                           [, which(colSums(Urban_Resistomes_Phyllo1_Vann[grep("A:Core_Urban_Areas",rownames(Urban_Resistomes_Phyllo1_Vann)),]) >0)])
str(Core_Urban_ARGs)
Core_Urban_ARGs1<-colnames(Core_Urban_ARGs)
str(Core_Urban_ARGs1)
length(Core_Urban_ARGs1)
#215

Inner_Suburbs_ARGs<-as.matrix(Urban_Resistomes_Phyllo1_Vann[grep("B:Inner_Suburbs",rownames(Urban_Resistomes_Phyllo1_Vann)),]
                              [, which(colSums(Urban_Resistomes_Phyllo1_Vann[grep("B:Inner_Suburbs",rownames(Urban_Resistomes_Phyllo1_Vann)),]) >0)])
str(Inner_Suburbs_ARGs)
Inner_Suburbs_ARGs1<-colnames(Inner_Suburbs_ARGs)
str(Inner_Suburbs_ARGs1)
length(Inner_Suburbs_ARGs1)
#[1] 215

Outer_Surburbs_ARGs <-as.matrix(Urban_Resistomes_Phyllo1_Vann[grep("C:Outer_Surburbs",rownames(Urban_Resistomes_Phyllo1_Vann)),]
                                [, which(colSums(Urban_Resistomes_Phyllo1_Vann[grep("C:Outer_Surburbs",rownames(Urban_Resistomes_Phyllo1_Vann)),]) >0)])
str(Outer_Surburbs_ARGs)
Outer_Surburbs_ARGs1<-colnames(Outer_Surburbs_ARGs)
str(Outer_Surburbs_ARGs1)
length(Outer_Surburbs_ARGs1)
#[1] 215

Exurban_ARGs<-as.matrix(Urban_Resistomes_Phyllo1_Vann[grep("D:Exurban_Areas",rownames(Urban_Resistomes_Phyllo1_Vann)),]
                        [, which(colSums(Urban_Resistomes_Phyllo1_Vann[grep("D:Exurban_Areas",rownames(Urban_Resistomes_Phyllo1_Vann)),]) >0)])
str(Exurban_ARGs)
Exurban_ARGs1<-colnames(Exurban_ARGs)
str(Exurban_ARGs1)
length(Exurban_ARGs1)
#[1] 215

Rural_Fringe_ARGs<-as.matrix(Urban_Resistomes_Phyllo1_Vann[grep("E:Rural_Fringe",rownames(Urban_Resistomes_Phyllo1_Vann)),]
                             [, which(colSums(Urban_Resistomes_Phyllo1_Vann[grep("E:Rural_Fringe",rownames(Urban_Resistomes_Phyllo1_Vann)),]) >0)])
str(Rural_Fringe_ARGs)
Rural_Fringe_ARGs1<-colnames(Rural_Fringe_ARGs)
str(Rural_Fringe_ARGs1)
length(Rural_Fringe_ARGs1)
#[1] 215

library(VennDiagram)
input_ARGs_Phyllo<-list(Core_Urban= Core_Urban_ARGs1, Inner_Suburbs=Inner_Suburbs_ARGs1, Outer_Surburbs=Outer_Surburbs_ARGs1, Exurban= Exurban_ARGs1, Rural_Fringe= Rural_Fringe_ARGs1)

# Define the colors for the sets
set_colors <- c("#801818", "#E34234", "#FFD700", "#8F9779", "#03AC13")

# Define the intensity of colors for the intersection areas
int_colors <- list(
  Core_Urban_Inner_Suburbs = adjustcolor(set_colors[1], alpha.f = 0.5), # Adjust alpha value for transparency
  Core_Urban_Outer_Surburbs = adjustcolor(set_colors[1], alpha.f = 0.5),
  Core_Urban_Exurban = adjustcolor(set_colors[1], alpha.f = 0.5),
  Core_Urban_Rural_Fringe = adjustcolor(set_colors[1], alpha.f = 0.5),
  Inner_Suburbs_Outer_Surburbs = adjustcolor(set_colors[5], alpha.f = 0.5),
  Inner_Suburbs_Exurban = adjustcolor(set_colors[2], alpha.f = 0.5),
  Inner_Suburbs_Rural_Fringe = adjustcolor(set_colors[2], alpha.f = 0.5),
  Outer_Surburbs_Exurban = adjustcolor(set_colors[3], alpha.f = 0.5),
  Outer_Surburbs_Rural_Fringe = adjustcolor(set_colors[3], alpha.f = 0.5),
  Exurban_Rural_Fringe = adjustcolor(set_colors[4], alpha.f = 0.5)
)
# Create the Venn diagram
venn.plot <- venn.diagram(
  x = input_ARGs_Phyllo,
  category.names = names(input_ARGs_Phyllo),
  fill = set_colors,
  cat.fontfamily = "sans",
  cat.col = set_colors,
  cat.cex = 1.2,
  cat.pos = 0,
  margin = 0.05,
  main = "Venn Diagram of ARGs Phyllo Data",
  filename = NULL
)

# Adjust the margin to control the boundary of the figure
margin_size <- 0.01  # Adjust this value as needed

# Create the Venn diagram with adjusted margin
venn.plot <- venn.diagram(
  x = input_ARGs_Phyllo,
  category.names = names(input_ARGs_Phyllo),
  fill = set_colors,
  cat.fontfamily = "sans",
  cat.col = set_colors,
  cat.cex = 1.2,
  cat.pos = 0,
  margin = margin_size,  # Adjusted margin size
  main = "Venn Diagram of the shared ARGs by different urban gradients grass phyllosphere",
  filename = NULL,
  euler.d = int_colors
)
# Plot the Venn diagram
grid.draw(venn.plot)

ggsave(filename = "Vann_Diagram_ARGs_In_Urban_Phyllo.pdf", plot = venn.plot,width = 20, height = 25, dpi = 2500, units = "cm")
ggsave(filename = "Vann_Diagram_ARGs_In_Urban_Phyllo.png", plot = venn.plot, width = 20, height = 25, dpi = 2500, units = "cm")
dev.off()
#####---------------------------------------------------------------------------------------------------------------------####
####----------- EXTRACT SUMMARY TABLE OF Vann_Diagram_ARGs_In_Urban_Phyllo
Summary_Table_Vann_Diagram_ARGs_In_Urban_Phyllo <- venn(input_ARGs_Phyllo, category.names= names(input_ARGs_Phyllo), show.plot = FALSE)
write.csv(Summary_Table_Vann_Diagram_ARGs_In_Urban_Phyllo, "Vann_Diagram_ARGs_In_Urban_Phyllo.csv", row.names = FALSE)
#####---------------------------------------------------------------------------------------------------------------------------######

###########################=================================== VANN DIAGRAMMS PLOTS ==============================##################################

####################################### ================== VGFs SOIL ====================########################

Urban_Resistomes_Soil1_VFGs
Urban_Resistomes_Soil1_VFGs_1 <- Urban_Resistomes_Soil1_VFGs %>%
  dplyr::select(SampleID, VGFs, sum, Urban_Gradient)
#Lets convert the data frame back into matrix:

Urban_Resistomes_Soil1_VFGs_Vann <- dcast(Urban_Resistomes_Soil1_VFGs_1, Urban_Gradient+SampleID~VGFs, value.var="sum")
rownames(Urban_Resistomes_Soil1_VFGs_Vann)<-interaction(Urban_Resistomes_Soil1_VFGs_Vann$Urban_Gradient,Urban_Resistomes_Soil1_VFGs_Vann$SampleID, sep="_")
Urban_Resistomes_Soil1_VFGs_Vann<-as.matrix(Urban_Resistomes_Soil1_VFGs_Vann[,3:ncol(Urban_Resistomes_Soil1_VFGs_Vann)])

# Core_Urban_VGFs
Core_Urban_VGFs<-as.matrix(Urban_Resistomes_Soil1_VFGs_Vann[grep("A:Core_Urban_Areas",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]
                           [, which(colSums(Urban_Resistomes_Soil1_VFGs_Vann[grep("A:Core_Urban_Areas",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]) >0)])
str(Core_Urban_VGFs)
Core_Urban_VGFs1<-colnames(Core_Urban_VGFs)
str(Core_Urban_VGFs1)
length(Core_Urban_VGFs1)
#215

Inner_Suburbs_VGFs<-as.matrix(Urban_Resistomes_Soil1_VFGs_Vann[grep("B:Inner_Suburbs",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]
                              [, which(colSums(Urban_Resistomes_Soil1_VFGs_Vann[grep("B:Inner_Suburbs",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]) >0)])
str(Inner_Suburbs_VGFs)
Inner_Suburbs_VGFs1<-colnames(Inner_Suburbs_VGFs)
str(Inner_Suburbs_VGFs1)
length(Inner_Suburbs_VGFs1)
#[1] 215

Outer_Surburbs_VGFs <-as.matrix(Urban_Resistomes_Soil1_VFGs_Vann[grep("C:Outer_Surburbs",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]
                                [, which(colSums(Urban_Resistomes_Soil1_VFGs_Vann[grep("C:Outer_Surburbs",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]) >0)])
str(Outer_Surburbs_VGFs)
Outer_Surburbs_VGFs1<-colnames(Outer_Surburbs_VGFs)
str(Outer_Surburbs_VGFs1)
length(Outer_Surburbs_VGFs1)
#[1] 215

Exurban_VGFs<-as.matrix(Urban_Resistomes_Soil1_VFGs_Vann[grep("D:Exurban_Areas",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]
                        [, which(colSums(Urban_Resistomes_Soil1_VFGs_Vann[grep("D:Exurban_Areas",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]) >0)])
str(Exurban_VGFs)
Exurban_VGFs1<-colnames(Exurban_VGFs)
str(Exurban_VGFs1)
length(Exurban_VGFs1)
#[1] 215

Rural_Fringe_VGFs<-as.matrix(Urban_Resistomes_Soil1_VFGs_Vann[grep("E:Rural_Fringe",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]
                             [, which(colSums(Urban_Resistomes_Soil1_VFGs_Vann[grep("E:Rural_Fringe",rownames(Urban_Resistomes_Soil1_VFGs_Vann)),]) >0)])
str(Rural_Fringe_VGFs)
Rural_Fringe_VGFs1<-colnames(Rural_Fringe_VGFs)
str(Rural_Fringe_VGFs1)
length(Rural_Fringe_VGFs1)
#[1] 215

library(VennDiagram)
input_VGFs_Soil<-list(Core_Urban= Core_Urban_VGFs1, Inner_Suburbs=Inner_Suburbs_VGFs1, Outer_Surburbs=Outer_Surburbs_VGFs1, Exurban= Exurban_VGFs1, Rural_Fringe= Rural_Fringe_VGFs1)

# Define the colors for the sets
set_colors <- c("#801818", "#E34234", "#FFD700", "#8F9779", "#03AC13")

# Define the intensity of colors for the intersection areas
int_colors <- list(
  Core_Urban_Inner_Suburbs = adjustcolor(set_colors[1], alpha.f = 0.5), # Adjust alpha value for transparency
  Core_Urban_Outer_Surburbs = adjustcolor(set_colors[1], alpha.f = 0.5),
  Core_Urban_Exurban = adjustcolor(set_colors[1], alpha.f = 0.5),
  Core_Urban_Rural_Fringe = adjustcolor(set_colors[1], alpha.f = 0.5),
  Inner_Suburbs_Outer_Surburbs = adjustcolor(set_colors[5], alpha.f = 0.5),
  Inner_Suburbs_Exurban = adjustcolor(set_colors[2], alpha.f = 0.5),
  Inner_Suburbs_Rural_Fringe = adjustcolor(set_colors[2], alpha.f = 0.5),
  Outer_Surburbs_Exurban = adjustcolor(set_colors[3], alpha.f = 0.5),
  Outer_Surburbs_Rural_Fringe = adjustcolor(set_colors[3], alpha.f = 0.5),
  Exurban_Rural_Fringe = adjustcolor(set_colors[4], alpha.f = 0.5)
)
# Create the Venn diagram
venn.plot <- venn.diagram(
  x = input_VGFs_Soil,
  category.names = names(input_VGFs_Soil),
  fill = set_colors,
  cat.fontfamily = "sans",
  cat.col = set_colors,
  cat.cex = 1.2,
  cat.pos = 0,
  margin = 0.05,
  main = "Venn Diagram of VGFs Soil Data",
  filename = NULL
)


# Plot the Venn diagram
grid.draw(venn.plot)

# Adjust the margin to control the boundary of the figure
margin_size <- 0.01  # Adjust this value as needed

# Create the Venn diagram with adjusted margin
venn.plot <- venn.diagram(
  x = input_VGFs_Soil,
  category.names = names(input_VGFs_Soil),
  fill = set_colors,
  cat.fontfamily = "sans",
  cat.col = set_colors,
  cat.cex = 1.2,
  cat.pos = 0,
  margin = margin_size,  # Adjusted margin size
  main = "Venn Diagram of the shared VGFs by different urban gradients soil",
  filename = NULL,
  euler.d = int_colors
)
# Plot the Venn diagram
grid.draw(venn.plot)
ggsave(filename = "Vann_Diagram_VGFs_In_Urban_Soil.png", plot = venn.plot, width = 20, height = 25, dpi = 2500, units = "cm")
ggsave(filename = "Vann_Diagram_VGFs_In_Urban_Soil.pdf", plot = venn.plot,width = 20, height = 25, dpi = 2500, units = "cm")
dev.off()
#####---------------------------------------------------------------------------------------------------------------------####
####----------- EXTRACT SUMMARY TABLE OF Vann_Diagram_VGFs_In_Urban_Soil
Summary_Table_Vann_Diagram_VGFs_In_Urban_Soil <- venn(input_VGFs_Soil, category.names= names(input_VGFs_Soil), show.plot = FALSE)
write.csv(Summary_Table_Vann_Diagram_VGFs_In_Urban_Soil, "Vann_Diagram_VGFs_In_Urban_Soil.csv", row.names = FALSE)
#####---------------------------------------------------------------------------------------------------------------------------######

####################################### ================== VGFs Phyllo ====================########################

Urban_Resistomes_Phyllo1_VFGs
Urban_Resistomes_Phyllo1_VFGs$sum <- as.numeric(Urban_Resistomes_Phyllo1_VFGs$sum)
Urban_Resistomes_Phyllo1_VFGs_1 <- Urban_Resistomes_Phyllo1_VFGs %>%
  dplyr::select(SampleID, VGFs, sum, Urban_Gradient)
#Lets convert the data frame back into matrix:

Urban_Resistomes_Phyllo1_VFGs_Vann <- dcast(Urban_Resistomes_Phyllo1_VFGs_1, Urban_Gradient+SampleID~VGFs, value.var="sum")
rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)<-interaction(Urban_Resistomes_Phyllo1_VFGs_Vann$Urban_Gradient,Urban_Resistomes_Phyllo1_VFGs_Vann$SampleID, sep="_")
Urban_Resistomes_Phyllo1_VFGs_Vann<-as.matrix(Urban_Resistomes_Phyllo1_VFGs_Vann[,3:ncol(Urban_Resistomes_Phyllo1_VFGs_Vann)])

# Core_Urban_VGFs
Core_Urban_VGFs<-as.matrix(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("A:Core_Urban_Areas",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]
                           [, which(colSums(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("A:Core_Urban_Areas",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]) >0)])
str(Core_Urban_VGFs)
Core_Urban_VGFs1<-colnames(Core_Urban_VGFs)
str(Core_Urban_VGFs1)
length(Core_Urban_VGFs1)
#215

Inner_Suburbs_VGFs<-as.matrix(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("B:Inner_Suburbs",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]
                              [, which(colSums(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("B:Inner_Suburbs",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]) >0)])
str(Inner_Suburbs_VGFs)
Inner_Suburbs_VGFs1<-colnames(Inner_Suburbs_VGFs)
str(Inner_Suburbs_VGFs1)
length(Inner_Suburbs_VGFs1)
#[1] 215

Outer_Surburbs_VGFs <-as.matrix(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("C:Outer_Surburbs",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]
                                [, which(colSums(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("C:Outer_Surburbs",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]) >0)])
str(Outer_Surburbs_VGFs)
Outer_Surburbs_VGFs1<-colnames(Outer_Surburbs_VGFs)
str(Outer_Surburbs_VGFs1)
length(Outer_Surburbs_VGFs1)
#[1] 215

Exurban_VGFs<-as.matrix(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("D:Exurban_Areas",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]
                        [, which(colSums(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("D:Exurban_Areas",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]) >0)])
str(Exurban_VGFs)
Exurban_VGFs1<-colnames(Exurban_VGFs)
str(Exurban_VGFs1)
length(Exurban_VGFs1)
#[1] 215

Rural_Fringe_VGFs<-as.matrix(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("E:Rural_Fringe",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]
                             [, which(colSums(Urban_Resistomes_Phyllo1_VFGs_Vann[grep("E:Rural_Fringe",rownames(Urban_Resistomes_Phyllo1_VFGs_Vann)),]) >0)])
str(Rural_Fringe_VGFs)
Rural_Fringe_VGFs1<-colnames(Rural_Fringe_VGFs)
str(Rural_Fringe_VGFs1)
length(Rural_Fringe_VGFs1)
#[1] 215

library(VennDiagram)
input_VGFs_Phyllo<-list(Core_Urban= Core_Urban_VGFs1, Inner_Suburbs=Inner_Suburbs_VGFs1, Outer_Surburbs=Outer_Surburbs_VGFs1, Exurban= Exurban_VGFs1, Rural_Fringe= Rural_Fringe_VGFs1)

# Define the colors for the sets
set_colors <- c("#801818", "#E34234", "#FFD700", "#8F9779", "#03AC13")

# Define the intensity of colors for the intersection areas
int_colors <- list(
  Core_Urban_Inner_Suburbs = adjustcolor(set_colors[1], alpha.f = 0.5), # Adjust alpha value for transparency
  Core_Urban_Outer_Surburbs = adjustcolor(set_colors[1], alpha.f = 0.5),
  Core_Urban_Exurban = adjustcolor(set_colors[1], alpha.f = 0.5),
  Core_Urban_Rural_Fringe = adjustcolor(set_colors[1], alpha.f = 0.5),
  Inner_Suburbs_Outer_Surburbs = adjustcolor(set_colors[5], alpha.f = 0.5),
  Inner_Suburbs_Exurban = adjustcolor(set_colors[2], alpha.f = 0.5),
  Inner_Suburbs_Rural_Fringe = adjustcolor(set_colors[2], alpha.f = 0.5),
  Outer_Surburbs_Exurban = adjustcolor(set_colors[3], alpha.f = 0.5),
  Outer_Surburbs_Rural_Fringe = adjustcolor(set_colors[3], alpha.f = 0.5),
  Exurban_Rural_Fringe = adjustcolor(set_colors[4], alpha.f = 0.5)
)
# Create the Venn diagram
venn.plot <- venn.diagram(
  x = input_VGFs_Phyllo,
  category.names = names(input_VGFs_Phyllo),
  fill = set_colors,
  cat.fontfamily = "sans",
  cat.col = set_colors,
  cat.cex = 1.2,
  cat.pos = 0,
  margin = 0.05,
  main = "Venn Diagram of VGFs Phyllo Data",
  filename = NULL
)

# Adjust the margin to control the boundary of the figure
margin_size <- 0.01  # Adjust this value as needed

# Create the Venn diagram with adjusted margin
venn.plot <- venn.diagram(
  x = input_VGFs_Phyllo,
  category.names = names(input_VGFs_Phyllo),
  fill = set_colors,
  cat.fontfamily = "sans",
  cat.col = set_colors,
  cat.cex = 1.2,
  cat.pos = 0,
  margin = margin_size,  # Adjusted margin size
  main = "Venn Diagram of the shared VGFs by different urban gradients grass phyllosphere",
  filename = NULL,
  euler.d = int_colors
)
# Plot the Venn diagram
grid.draw(venn.plot)

ggsave(filename = "Vann_Diagram_VGFs_In_Urban_Phyllo.pdf", plot = venn.plot,width = 20, height = 25, dpi = 2500, units = "cm")
ggsave(filename = "Vann_Diagram_VGFs_In_Urban_Phyllo.png", plot = venn.plot, width = 20, height = 25, dpi = 2500, units = "cm")
dev.off()
#####---------------------------------------------------------------------------------------------------------------------####
####----------- EXTRACT SUMMARY TABLE OF Vann_Diagram_VGFs_In_Urban_Phyllo
Summary_Table_Vann_Diagram_VGFs_In_Urban_Phyllo <- venn(input_VGFs_Phyllo, category.names= names(input_VGFs_Phyllo), show.plot = FALSE)
write.csv(Summary_Table_Vann_Diagram_VGFs_In_Urban_Phyllo, "Vann_Diagram_VGFs_In_Urban_Phyllo.csv", row.names = FALSE)
#####---------------------------------------------------------------------------------------------------------------------------######

############################---------------THE MOST ABUNDANT ARGS AT GENES LEVELS --------------------------################

#----------------------------------------------------------------------------------------------------------------------------#
ARGs_1
ARGs_1_1 <- data.frame(ARGs_1)
ARGs_1_1
t(ARGs_1_1)                                     ## Transpose
data.frame(t(ARGs_1_1))                         ## Convert the matrix after transposing to dataframe
data.frame(t(ARGs_1_1[ , - 1]))                 ## Transpose and remove the first column (Because first Column is X1, X2, etc,....)
ARGs_1_1t <- setNames(data.frame(t(ARGs_1_1[ , - 1])), ARGs_1_1[ , 1]) ## Transpose and name the first column as column name
head(ARGs_1_1t)
setNames(data.frame(t(ARGs_1_1[ , - 1])), ARGs_1_1[ , 1]) %>%
  mutate(SampleID=rownames(ARGs_1_1t))                   ## Set the rowname as a variable "Parameters"

setNames(data.frame(t(ARGs_1_1[ , - 1])), ARGs_1_1[ , 1]) %>%
  mutate(SampleID=rownames(ARGs_1_1t), .before="AAC(3)-Ia")         ## Set a variable "Parameters" after rowname
head(ARGs_1_1t)
#--------------------------------------------------------------------------------------------------------------------
levels(Metadata$Urban_Gradient)
Metadata$Urban_Gradient<- factor(Metadata$Urban_Gradient, levels=c("A:Core_Urban_Areas","B:Inner_Suburbs",
                                                           "C:Outer_Surburbs", "D:Exurban_Areas","E:Rural_Fringe"))

levels(Metadata$Urban_Gradient)          
Metadata$color<-rep(10, nrow( Metadata))  
Metadata <- within(Metadata, color[Urban_Gradient == "A:Core_Urban_Areas"] <- "#801818")
Metadata <- within(Metadata, color[Urban_Gradient == "B:Inner_Suburbs"] <- "#E34234")
Metadata <- within(Metadata, color[Urban_Gradient == "C:Outer_Surburbs"] <- "#FFD700")
Metadata <- within(Metadata, color[Urban_Gradient == "D:Exurban_Areas"] <- "#85E085")
Metadata <- within(Metadata, color[Urban_Gradient == "E:Rural_Fringe"] <- "#248f24")


ARGs_Only_NMDS<-metaMDS(ARGs_1_1t)

ordiplot(ARGs_Only_NMDS,type="n", xlim=c(-1.2,2.2),
         ylim=c(-1,1),cex.axis = 1.5, cex.lab = 1.5, 
         main = "ARGs and MGEs")
with(Metadata, ordiellipse(ARGs_Only_NMDS, Urban_Gradient, 
                               kind = "se", conf = 0.95, col=c("#801818", "#E34234", "#FFD700",
                                                               "#85E085", "#248f24")))

with(Metadata, points(ARGs_Only_NMDS$points,pch=17, cex=1.4, col=Metadata$color))
with(Metadata, legend(-2.8,-0.7, legend= levels(Urban_Gradient), cex=0.9, bty= "n", 
                          col=c("#801818", "#E34234", "#FFD700",
                                "#85E085", "#248f24"), pch=17))


adonis2(ARGs_1_1t ~ Urban_Gradient, data=Metadata, permutations=9999)



levels(Metadata$Urban_Gradient)


pairwise.adonis <- function(x,factors, sim.method, p.adjust.m)
{
  library(vegan)
  co = as.matrix(combn(unique(factors),2))
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  for(elem in 1:ncol(co)){
    ad = adonis(x[factors %in% c(as.character(co[1,elem]),as.character(co[2,elem])),] ~
                  factors[factors %in% c(as.character(co[1,elem]),as.character(co[2,elem]))] , method =sim.method, permutations = 9999);
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted)
  return(pairw.res)
}

pairwise.adonis(ARGs_1_1t, Metadata$Urban_Gradient, sim.method = "bray", p.adjust.m = "BH")
 #p.adjust.m = "BH" is False discovery rate
 
 # Large p-values due to small group size....
 # all not significant 
 
 #And heatmap is also a good tool for understanding data. 
 #Some transformation is probably needed, here we used square root (sqrt()).
 #You can also do it without transormation. Depends on your data.
 
 #with transformation
 heatmap.2(as.matrix(sqrt(ARGs_1_1t)), col = rev(heat.colors(100)), 
           trace = "none", density.info = "none",  srtCol=45, margins=c(5,8))
 #without
 heatmap.2(as.matrix(ARGs_1_1t), col = rev(heat.colors(100)), 
           trace = "none", density.info = "none",  srtCol=45, margins=c(5,8))
 
 #Heatmap from only the most abundant genes is usually more helpful. 
 #For example the ones where the maximum abundance is over 0.0001. 
 ARGs_abund00001 <- ARGs_1_1t[,apply(ARGs_1_1t, 2, max)>0.000001]
 ARGs_abund0000001 <- ARGs_1_1t[,apply(ARGs_1_1t, 2, max)>0.00000000001]
 ARGs_abund0000001
 ARGs_abund001 <- ARGs_1_1t[,apply(ARGs_1_1t, 2, max)>0.01]
 ARGs_abund001
 heatmap.2(as.matrix(sqrt(ARGs_abund00001)), col = rev(heat.colors(100)), 
           trace = "none", density.info = "none",  srtCol=45, margins=c(5,8))
 
 heatmap.2(as.matrix(sqrt(ARGs_abund00001)), col = rev(heat.colors(100)), #Colv=FALSE, 
           trace = "none", density.info = "none",  srtCol=45, margins=c(5,8))
 
 #Lets put the samples in order and forget the clustering:
 heatmap.2(as.matrix(sqrt(ARGs_abund00001)), col = rev(heat.colors(100)),
           dendrogram="none",Rowv=FALSE, Colv=FALSE, 
           trace = "none", density.info = "none",  srtCol=45, margins=c(5,8))
 
 heatmap.2(as.matrix(sqrt(ARGs_abund00001)), col = rev(heat.colors(100)),
           dendrogram="none",Rowv=FALSE, Colv=FALSE, 
           trace = "none", density.info = "none",  srtCol=45, margins=c(5,8))
 #clustering by rows and columns separately:
 
 heatmap.2(as.matrix(sqrt(ARGs_abund00001)), col = rev(heat.colors(100)),
           dendrogram="row",Colv=FALSE, 
           trace = "none", density.info = "none",  srtCol=45, margins=c(5,8))
 
 heatmap.2(as.matrix(sqrt(ARGs_abund00001)), col = rev(heat.colors(100)),
           dendrogram="column",Rowv=FALSE, 
           trace = "none", density.info = "none",  srtCol=45, margins=c(5,8))
 
 # We can also make a heatmap with ggplot2. 
 # The data needs to be in a long format.
 
 library(reshape2)  # Make sure reshape2 package is loaded
 # Melt the data frame
 ARGs_abund00001.m <- melt(ARGs_abund00001)
 # Add row names as the first variable
 ARGs_abund00001.m$RowNames <- rownames(ARGs_abund00001)
 # Move the RowNames column to the first position
 ARGs_abund00001.m <- ARGs_abund00001.m[, c("RowNames", names(ARGs_abund00001.m)[-ncol(ARGs_abund00001.m)])]
 # Print the modified data frame
 print(ARGs_abund00001.m)
 
 #lets check what genes are among the most abundant ones:
 
 levels(ARGs_abund00001.m$variable)
 sheet_name <- "ARGs_Genes_Abundant"  # You can change "Sheet1" to whatever name you prefer
 #-----write.xlsx(ARGs_abund00001.m, file = "ARGs_Genes_Abundant.xlsx", rowNames = FALSE, sheetName = sheet_name)
  
 #Then lets order the assays we have according to the antibiotic they give resistance
 Gene_ord<-c( "AAC(3)-Ia",	"AAC(3)-Ib",	"AAC(3)-Iic",	"Aac(3)-iid_iia_iie",	"AAC(3)-IV",	"AAC(3)-Via",	"AAC(3)-Xa",	"AAC(6')-Ib",	"aac(6')-ie-aph(2'')-ia",	"AAC(6')-IIa",
              "AAC(6')-Im",	"AAC(6')-Ir",	"aacA_aphD",	"aacA43",	"aadA",	"aadA10",	"aadA16",	"aadA17",	"aadA2",	"aadA21",	"aadA5",	"aadA7",	"aadA9",	"ANT(2'')-Ia",	"ANT(4')",
              "ANT(6)",	"APH(3')-Ia",	"APH(3'')-Ia",	"APH(3')-Ib",	"APH(4)-Ib",	"APH(6)-Ia",	"APH(6)-Ic",	"APH(6)-Id",	"APH(9)-Ib",	"APH3-III",	"APHA3",	"str",	"strA",	
              "ACT beta-lac",	"ADC beta-lac",	"AmpC beta-lac",	"Bla1",	"blaSFO",	"CARB beta-lac",	"CcrA beta-lac",	"cepA beta-lac",	"class C beta-lac",	"CMY beta-lac",	"CphA beta-lac",
              "CTX-M beta-lac",	"DHA beta-lac",	"FOX beta-lac",	"IMI beta-lac",	"IMIR beta-lac",	"L1 beta-lac",	"MIR beta-lac",	"OCH beta-lac",	"OXA-10",	"OXY-1-1",	"PDC beta-lac",	
              "penA",	"ROB-1",	"SHV-11",	"TEM beta-lac",	"VEB beta-lac",	"VIM beta-lac",	"dfrA1",	"dfrA21",	"dfrG",	"dfrK",	"mdtH",	"qacA_B",	"QepA_1_2",	"QnrB4",	"QnrD",	
              "Qnrvc1_vc3_vc6",	"vanA",	"vanB",	"vanC",	"vanC2_vanC3",	"vanHB",	"vanHD",	"vanRA",	"vanRB",	"vanSB",	"vanTC",	"vanYB",	"vanYD",	"IncI1_repI1",	"IncP_oriT",	
              "intI1_337old",	"IS1247",	"IS200-2",	"IS21-ISAs29",	"IS256",	"IS26",	"IS6/257",	"IS6100",	"IS613",	"IS630",	"IS91",	"ISEcp1",	"ISPps1-pseud",	"ISSm2-Xanthob",	
              "Tn3",	"TN5403",	"tnpA-1",	"tnpA-2",	"tnpA-3",	"tnpA-4",	"tnpA-5",	"tnpA-6",	"tnpA-7",	"Tp614",	"traN",	"trb-C",	"EreA",	"EreB",	"Erm(35)",	"Erm(36)",	"erm(O)",	
              "ErmB",	"ErmD",	"ErmE",	"ErmF",	"ErmG",	"ErmQ",	"ErmX",	"lnuA",	"lnuB",	"lnuC",	"lnuF",	"mef(B)",	"mphA",	"oleC",	"pica",	"pikR2",	"vatA",	"vatE",	"acrA",	"acrB",	"AcrF",	
              "acrR",	"adeA",	"adeI",	"cefa_qacelta",	"ceoA",	"emrD",	"marR",	"mdtA",	"mdtE",	"mdtG",	"MdtK",	"mel_1",	"mepA",	"MexA",	"MexB",	"MexE",	"msrE",	"nimE",	"OprD",	"optrA",	
              "oqxA",	"qacF_H",	"qacH_351",	"sugE",	"tolC",	"ttgA",	"ttgB",	"bacA",	"MCR-1.1",	"catB3",	"catQ",	"cmlA1",	"cmlA5",	"cmlv",	"cmx",	"floR",	"sul1",	"sul2",	"sulA_folP",	
              "tet(44)",	"tet32",	"tetA",	"tetB(P)",	"tetD",	"tetG",	"tetL",	"tetM",	"tetO",	"tetPB",	"tetQ",	"tetR",	"tetT",	"tetU",	"tetW",	"tetX")  
 
 ARGs_abund00001.m$ARGs<-factor(ARGs_abund00001.m$variable, levels =Gene_ord)
 length( Gene_ord)
 # Assuming you want to rename multiple variables
 new_names <- c("SampleID", "ARGs1", "Value", "ARGs" )
 old_names <- c("RowNames", "variable", "Value", "ARGs")
 
 # Use a loop to rename multiple variables
 for (i in seq_along(old_names)) {
   names(ARGs_abund00001.m)[names(ARGs_abund00001.m) == old_names[i]] <- new_names[i]
 }

 ARGs_abund00001.m <- merge(Metadata, ARGs_abund00001.m, by = "SampleID") 
 ARGs_abund00001.m 
 
 Ind_HM001 <- ggplot(ARGs_abund00001.m , aes(x=Urban_Gradient, y=ARGs, fill=value))+
   geom_tile(color = "black", size=0.005)+
  scale_fill_continuous_sequential(palette =  "Oslo",
   begin = 0.9, end = 0, na.value="white", name="Relative abundance")+
   theme(panel.background=element_rect(fill="black", colour="black")) +
   theme(axis.text.y=element_text(colour= "black",size=10))+
   theme(axis.text.x = element_text(colour= "black",angle = 90, hjust=0, vjust=0, size=10))+
   #theme(axis.text.x = element_text(colour= IndHMxcol,angle = 90, hjust=0, vjust=0, size=10))+
   theme(panel.border=element_blank())+
   theme(axis.title.x = element_blank()) + 
   theme(axis.title.y = element_blank()) + 
   theme(legend.position="bottom")+
   theme(legend.key.size=unit(0.2, "cm"))+
   theme(legend.key.width=unit(0.5, "cm"))
 Ind_HM001
#--------------------------------------------------------------------------------------------- 
 
 ARGs_abund00001.m$categories <- ARGs_abund00001.m%>% .$value %>% cut(c(9.536743e-09,1.227083e-05, 8.180552e-05,
                                                                                          1.227083e-04,8.180552e-04, 1.227083e-03,8.180552e-03,  1.227083e-02, 8.180552e-02, 1.227083e-01, 8.180552e-01))
 b <- ARGs_abund00001.m%>% .$value %>% cut(c(9.536743e-09,  1.227083e-05, 8.180552e-05,
                                                      1.227083e-04,8.180552e-04, 1.227083e-03,8.180552e-03,  1.227083e-02, 8.180552e-02, 1.227083e-01, 8.180552e-01))
 levels(b) <- 1:11
 ARGs_abund00001.m$categories2 <- b
 
 Indheatcol<-heat.colors(10)
 
 IndHMcolors<- sequential_hcl(n=10, palette="Mako")
#--------------------------------------------------------------------------------------
#------ ARGs_abund00001.m <- merge(Metadata, ARGs_abund00001.m, by = "SampleID") 
 #----ARGs_abund00001.m 
 
 
 ARGs_abund00001.m_SOIL1 <-subset(ARGs_abund00001.m, Substrate  == "soil")
 ARGs_abund00001.mL_Phyllo1 <-subset(ARGs_abund00001.m, Substrate  == "phyllosphere")
 
 ARGs_abund00001.m_SOIL1$ARGs <- factor(ARGs_abund00001.m_SOIL1$ARGs, levels = Gene_ord)
 ARGs_Soil_Abundant<-ggplot(ARGs_abund00001.m_SOIL1 , aes(x=Urban_Gradient, y=ARGs, fill=value))+
   geom_tile(aes(fill=categories2))+
   scale_fill_manual(values= rev(IndHMcolors), na.value="white", name= "Relative Abundance") +
   theme(axis.text.x = element_text(colour= "black", angle = 30, hjust = 1, vjust = 0.5, size=4), axis.text.y= element_text(colour="black", size=7))+
   theme(axis.title.x = element_blank(), axis.title.y =  element_blank())+
   theme(axis.text.x = element_text(colour= IndHMcolors,angle = 30, hjust=0, vjust=0, size=6))+
   theme(panel.border=element_blank())+
   theme(axis.title.x = element_blank()) + 
   theme(axis.title.y = element_blank()) + 
   #theme(legend.position="bottom")+
   theme(legend.key.size=unit(0.2, "cm"))+
   theme(legend.key.width=unit(0.5, "cm"))
 
 ARGs_Soil_Abundant
 ggsave("ARGs_Genes_abundance_in_soil.pdf", width = 20, height = 20, units = "cm", dpi = 300)
#-------------------------------------------------- 
 
 IndHMcolors2 <- sequential_hcl(n = 10, palette = "Viridis")
 
 ARGs_abund00001.mL_Phyllo1$ARGs <- factor(ARGs_abund00001.mL_Phyllo1$ARGs, levels = Gene_ord)
 ARGs_Phyllo_Abundant<-ggplot(ARGs_abund00001.mL_Phyllo1 , aes(x=Urban_Gradient, y=ARGs, fill=value))+
   geom_tile(aes(fill=categories2))+
   scale_fill_manual(values= rev(IndHMcolors2), na.value="white", name= "Relative Abundance") +
   theme(axis.text.x = element_text(colour= "black", angle = 30, hjust = 1, vjust = 0.5, size=4), axis.text.y= element_text(colour="black", size=4))+
   theme(axis.title.x = element_blank(), axis.title.y =  element_blank())+
   theme(axis.text.x = element_text(colour= IndHMcolors,angle = 30, hjust=0, vjust=0, size=6))+
   theme(panel.border=element_blank())+
   theme(axis.title.x = element_blank()) + 
   theme(axis.title.y = element_blank()) + 
   #theme(legend.position="bottom")+
   theme(legend.key.size=unit(0.2, "cm"))+
   theme(legend.key.width=unit(0.5, "cm"))
 ARGs_Phyllo_Abundant 
 ggsave("ARGs_Genes_abundance_in_Phylo.pdf", width = 12, height = 20, units = "cm", dpi = 300)
#---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
 ARGsQ <- group_by(ARGs_abund00001.m, categories2) %>%
   summarise(Mean=mean(value), sd = sd(value)) %>%
   arrange(desc(Mean))
 ARGsQ
#-------------------- WORKING WITH CLINICAL GENES ONLY --------------------------# 
 ARGs_Abund
 
 ARGs_abund00001.m1 <- ARGs_abund00001.m %>%
   left_join (ARGs_taxa, by = "ARGs")
 ARGs_abund00001.m1
 ARGs_abund00001.m11 <- subset(ARGs_abund00001.m1, RankI != 5)
 ARGs_abund00001.m_SOIL1 <-subset(ARGs_abund00001.m11, Substrate  == "soil")
 ARGs_abund00001.mL_Phyllo1 <-subset(ARGs_abund00001.m11, Substrate  == "phyllosphere")
 sheet_name <- "Soil"  # You can change "Sheet1" to whatever name you prefer
 write.xlsx(ARGs_abund00001.m_SOIL1, file = "ARGs_abund00001.m_SOIL.xlsx", rowNames = FALSE, sheetName = sheet_name)
 sheet_name <- "Phyllo"  # You can change "Sheet1" to whatever name you prefer
 write.xlsx(ARGs_abund00001.mL_Phyllo1, file = "ARGs_abund00001.m_SOIL.xlsx", rowNames = FALSE, sheetName = sheet_name)
 ##------------------------ sUMMARY TABLE -----------------------------------------##
 
 #---------------------IN URBAN SOIL-------------------------##
 # Step 1: Summarize Total ARGs
 High_Risk_ARGs_in_Soil <- ARGs_abund00001.m_SOIL1 %>%
   group_by(Substrate, Urban_Gradient, Classification, ARGs, RankI) %>%
   summarise(Total_ARGs = sum(value), .groups = "drop")
 # Step 2: Pivot the table to make Urban_Gradient a wide format column
 High_Risk_ARGs_in_Soil_wide <- High_Risk_ARGs_in_Soil %>%
   pivot_wider(names_from = Urban_Gradient, values_from = Total_ARGs, values_fill = 0)
 # Step 3: Export to Excel
 write.xlsx(High_Risk_ARGs_in_Soil_wide, file = "High_Risk_ARGs_in_Soil.xlsx", rowNames = FALSE)
 print(High_Risk_ARGs_in_Soil_wide)
 
#---------------------IN URBAN GRASS PHYLLOSPHERE-------------------------## 
 # Step 1: Summarize Total ARGs
 High_Risk_ARGs_in_Grass_Phyllosphere <- ARGs_abund00001.mL_Phyllo1 %>%
   group_by(Substrate, Urban_Gradient, Classification, ARGs, RankI) %>%
   summarise(Total_ARGs = sum(value),.groups = "drop")
 # Step 2: Pivot the table to make Urban_Gradient a wide format column
 High_Risk_ARGs_in_Grass_Phyllosphere_wide <- High_Risk_ARGs_in_Grass_Phyllosphere %>%
   pivot_wider(names_from = Urban_Gradient, values_from = Total_ARGs, values_fill = 0)
 # Step 3: Export to Excel
 High_Risk_ARGs_in_Grass_Phyllosphere_wide
 write.xlsx(High_Risk_ARGs_in_Grass_Phyllosphere_wide, file = "High_Risk_ARGs_in_Grass_Phyllosphere.xlsx", rowNames = FALSE)
 
 #-------------------
 #-----------------------------------------------------------------------------------------
 ARGs_abund00001.m_SOIL1$ARGs <- factor(ARGs_abund00001.m_SOIL1$ARGs, levels = Gene_ord)
 ARGs_Soil_Abundant<-ggplot(ARGs_abund00001.m_SOIL1 , aes(x=Urban_Gradient, y=ARGs, fill=value))+
   geom_tile(aes(fill=categories2))+
   scale_fill_manual(values= rev(IndHMcolors), na.value="white", name= "Relative Abundance") +
   theme(axis.text.x = element_text(colour= "black", angle = 30, hjust = 1, vjust = 0.5, size=4), axis.text.y= element_text(colour="black", size=7))+
   theme(axis.title.x = element_blank(), axis.title.y =  element_blank())+
   theme(axis.text.x = element_text(colour= IndHMcolors,angle = 30, hjust=0, vjust=0, size=6))+
   theme(panel.border=element_blank())+
   theme(axis.title.x = element_blank()) + 
   theme(axis.title.y = element_blank()) + 
   #theme(legend.position="bottom")+
   theme(legend.key.size=unit(0.2, "cm"))+
   theme(legend.key.width=unit(0.5, "cm"))
 
 ARGs_Soil_Abundant
 ggsave("ARGs_Clinical_Genes_abundance_in_soil.pdf", width = 8, height = 10, units = "cm", dpi = 300)
 
 
 # Convert ARGs to a factor based on a specific order
 
 ARGs_abund00001.m_SOIL1$ARGs <- factor(ARGs_abund00001.m_SOIL1$ARGs, levels = Gene_ord)
 
 # Create the heatmap with RED for higher abundance and BLUE for lower abundance
 ARGs_Soil_Abundant <- ggplot(ARGs_abund00001.m_SOIL1, aes(x = Urban_Gradient, y = ARGs, fill = value)) +
   geom_tile() +
   
   # Use scale_fill_gradient2 to create a gradient from blue (low) to red (high)
   scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = median(ARGs_abund00001.m_SOIL1$value, na.rm = TRUE),
                        na.value = "white", name = "Relative Abundance") +
   
   # Customize the axis text and appearance
   theme(axis.text.x = element_text(colour = "black", angle = 30, hjust = 1, vjust = 0.5, size = 4),
         
         axis.text.y = element_text(colour = "black", size = 7)) +
   
   # Remove axis titles and panel borders
   theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
         panel.border = element_blank()) +
   
   # Customize the legend key size and position
   theme(legend.key.size = unit(0.2, "cm"),
         legend.key.width = unit(0.5, "cm")) +
   
   # Optional: Adjust color of x-axis labels (if needed)
   theme(axis.text.x = element_text(angle = 30, hjust = 0, vjust = 0, size = 6))
 
 # Display the heatmap
 ARGs_Soil_Abundant
 
 
 
 #-------------------------------------------------- 
 IndHMcolors2 <- sequential_hcl(n = 10, palette = "Viridis")
 ARGs_abund00001.mL_Phyllo1$ARGs <- factor(ARGs_abund00001.mL_Phyllo1$ARGs, levels = Gene_ord)
 ARGs_Phyllo_Abundant<-ggplot(ARGs_abund00001.mL_Phyllo1 , aes(x=Urban_Gradient, y=ARGs, fill=value))+
   geom_tile(aes(fill=categories2))+
   scale_fill_manual(values= rev(IndHMcolors2), na.value="white", name= "Relative Abundance") +
   theme(axis.text.x = element_text(colour= "black", angle = 30, hjust = 1, vjust = 0.5, size=4), axis.text.y= element_text(colour="black", size=7))+
   theme(axis.title.x = element_blank(), axis.title.y =  element_blank())+
   theme(axis.text.x = element_text(colour= IndHMcolors,angle = 30, hjust=0, vjust=0, size=6))+
   theme(panel.border=element_blank())+
   theme(axis.title.x = element_blank()) + 
   theme(axis.title.y = element_blank()) + 
   #theme(legend.position="bottom")+
   theme(legend.key.size=unit(0.2, "cm"))+
   theme(legend.key.width=unit(0.5, "cm"))
 ARGs_Phyllo_Abundant 
 ggsave("ARGs_Clinical_Genes_abundance_in_Phylo.pdf", width = 12, height = 20, units = "cm", dpi = 300)
 #---------------------------------------------------------------------------------------------------------------------------------------------------------------- 
  library(gridExtra)
 grid.arrange(ARGs_Soil_Abundant, ARGs_Phyllo_Abundant, nrow = 2)
 ggsave("ARGs_Clinical_Genes_abundance.pdf", width = 12, height = 20, units = "cm", dpi = 300)

 #-------------------------- WORKING WITH VGFs ---------------------------------------------##
 
 VGFs_1
 VGFs_1_1 <- data.frame(VGFs_1)
 VGFs_1_1
 t(VGFs_1_1)                                     ## Transpose
 data.frame(t(VGFs_1_1))                         ## Convert the matrix after transposing to dataframe
 data.frame(t(VGFs_1_1[ , - 1]))                 ## Transpose and remove the first column (Because first Column is X1, X2, etc,....)
 VGFs_1_1t <- setNames(data.frame(t(VGFs_1_1[ , - 1])), VGFs_1_1[ , 1]) ## Transpose and name the first column as column name
 head(VGFs_1_1t)
 setNames(data.frame(t(VGFs_1_1[ , - 1])), VGFs_1_1[ , 1]) %>%
   mutate(SampleID=rownames(VGFs_1_1t))                   ## Set the rowname as a variable "Parameters"
 
 setNames(data.frame(t(VGFs_1_1[ , - 1])), VGFs_1_1[ , 1]) %>%
   mutate(SampleID=rownames(VGFs_1_1t), .before="aafA")         ## Set a variable "Parameters" after rowname
 head(VGFs_1_1t)
 VGFs_1No1 <- as.data.frame(specnumber(VGFs_1_1t))
 VGFs_1No1
 #------------------------------------------------------------------
 
 VGFs_abund00001 <- VGFs_1_1t[,apply(VGFs_1_1t, 2, max)>0.00001]
 VGFs_abund0000001 <- VGFs_1_1t[,apply(VGFs_1_1t, 2, max)>0.000000001]
 
#------------------------------------------------------------------------------ 
 # Melt the data frame
 VGFs_abund00001.m <- melt(VGFs_abund00001)
 # Add row names as the first variable
 VGFs_abund00001.m$RowNames <- rownames(VGFs_abund00001)
 # Move the RowNames column to the first position
 VGFs_abund00001.m <- VGFs_abund00001.m[, c("RowNames", names(VGFs_abund00001.m)[-ncol(VGFs_abund00001.m)])]
 # Print the modified data frame
 print(VGFs_abund00001.m)
 
 #lets check what genes are among the most abundant ones:

 levels(VGFs_abund00001.m$variable)
 sheet_name <- "VGFs_Genes_Abundant"  # You can change "Sheet1" to whatever name you prefer
 write.xlsx(VGFs_abund00001.m$variable, file = "VGFs_Genes_Abundant.xlsx", rowNames = FALSE, sheetName = sheet_name)
 
 Gene_ord<-c ("abaI",	"abaR",	"abaR2(d)",	"Ab-ITS",	"adeG",	"bap",	"basA",	"basG",	"bauA1",	"bauA2(d)",	"bfmR",	"csuA/B",	"csuE(d)",	"paa(d)",	"pgi",	"pgi(d)",	"phoQ",	"pic",	"aafA",	
              "aatA",	"pgaA(d)",	"prgH",	"shuA",	"espF",	"ibeA(d)",	"pefA",	"tviA",	"lpfC(d)",	"tir3(d)",	"astA(d)",	"eltA",	"eco-resv",	"allS",	"clpV/tssH",	"entB",	"entB(d)",	"fepB",	
              "fimA",	"fimA(d)[Klebs]",	"gtrA",	"iutA2",	"kp",	"ler",	"shdA(d)",	"wbbM(d)",	"wzm1",	"ybtS",	"icmF/tssM",	"icsA/virG1",	"csgA",	"csgG(d)",	"flmB",	"flmE",	"hilA",	"iroN",	
              "lpfE",	"lpxM",	"mark-AB1",	"mig-14(d)",	"mrkA",	"mrkD",	"pgaA",	"rck1",	"rck3(d)",	"set1B",	"shuT",	"wbbM",	"sigA",	"astA",	"iutA1(d)",	"nleA",	"ompA",	"sinH(d)",	"spa40",	
              "spvB",	"drap",	"focG",	"grvA",	"ivoN",	"pefA(d)")
 
 #--- VGFs_abund00001.m$VGFs<-factor(VGFs_abund00001.m$variable, levels =Gene_ord)
 #--- length( Gene_ord)
 # Assuming you want to rename multiple variables
 new_names <- c("SampleID", "VGFs", "Value")
 old_names <- c("RowNames", "variable", "Value")
 
 # Use a loop to rename multiple variables
 for (i in seq_along(old_names)) {
   names(VGFs_abund00001.m)[names(VGFs_abund00001.m) == old_names[i]] <- new_names[i]
 }
 
 VGFs_abund00001.m <- merge(Metadata, VGFs_abund00001.m, by = "SampleID") 
 VGFs_abund00001.m 
 #------------------------------------------------------------------------------------------------------
 dev.off()
 Ind_HM001 <- ggplot(VGFs_abund00001.m , aes(x=Urban_Gradient, y=VGFs, fill=value))+
   geom_tile(color = "black", size=0.005)+
   scale_fill_continuous_sequential(palette =  "Oslo",
                                    begin = 0.9, end = 0, na.value="white", name="Relative abundance")+
   theme(panel.background=element_rect(fill="black", colour="black")) +
   theme(axis.text.y=element_text(colour= "black",size=10))+
   theme(axis.text.x = element_text(colour= "black",angle = 90, hjust=0, vjust=0, size=10))+
   #theme(axis.text.x = element_text(colour= IndHMxcol,angle = 90, hjust=0, vjust=0, size=10))+
   theme(panel.border=element_blank())+
   theme(axis.title.x = element_blank()) + 
   theme(axis.title.y = element_blank()) + 
   theme(legend.position="bottom")+
   theme(legend.key.size=unit(0.2, "cm"))+
   theme(legend.key.width=unit(0.5, "cm"))
 Ind_HM001
 #--------------------------------------------------------------------------------------------- 
 
 VGFs_abund00001.m$categories <- VGFs_abund00001.m%>% .$value %>% cut(c(9.536743e-09,  1.227083e-05, 8.180552e-05,
                                                                        1.227083e-04,8.180552e-04, 1.227083e-03,8.180552e-03,  1.227083e-02, 8.180552e-02, 1.227083e-01, 3.242323e-01))
 b <- VGFs_abund00001.m%>% .$value %>% cut(c(9.536743e-09,  1.227083e-05, 8.180552e-05,
                                             1.227083e-04,8.180552e-04, 1.227083e-03,8.180552e-03,  1.227083e-02, 8.180552e-02, 1.227083e-01, 3.242323e-01))
 levels(b) <- 1:11
 VGFs_abund00001.m$categories2 <- b
 
 Indheatcol<-heat.colors(10)
 
 IndHMcolors<- sequential_hcl(n=10, palette="Mako")
 #--------------------------------------------------------------------------------------
 #------ ARGs_abund00001.m <- merge(Metadata, ARGs_abund00001.m, by = "SampleID") 
 #----ARGs_abund00001.m 
 IndHMcolors <- sequential_hcl(n = 10, palette = "SunsetDark")
 VGFs_abund00001.m_SOIL1 <-subset(VGFs_abund00001.m, Substrate  == "soil")
 sheet_name <- "Soil"  # You can change "Sheet1" to whatever name you prefer
 write.xlsx(VGFs_abund00001.m_SOIL1, file = "VGFs_abund00001.m_SOIL.xlsx", rowNames = FALSE, sheetName = sheet_name)
 
 VGFs_abund00001.mL_Phyllo1 <-subset(VGFs_abund00001.m, Substrate  == "phyllosphere")
 sheet_name <- "Phyllo"  # You can change "Sheet1" to whatever name you prefer
 write.xlsx(VGFs_abund00001.mL_Phyllo1, file = "VGFs_abund00001.mL_Phyllo.xlsx", rowNames = FALSE, sheetName = sheet_name)
 
 
 
 VGFs_abund00001.m_SOIL1$VGFs <- factor(VGFs_abund00001.m_SOIL1$VGFs, levels = Gene_ord)
 VGFs_Soil_Abundant<-ggplot(VGFs_abund00001.m_SOIL1 , aes(x=Urban_Gradient, y=VGFs, fill=value))+
   geom_tile(aes(fill=categories2))+
   scale_fill_manual(values= rev(IndHMcolors), na.value="white", name= "Relative Abundance") +
   theme(axis.text.x = element_text(colour= "black", angle = 30, hjust = 1, vjust = 0.5, size=4), axis.text.y= element_text(colour="black", size=7))+
   theme(axis.title.x = element_blank(), axis.title.y =  element_blank())+
   theme(axis.text.x = element_text(colour= IndHMcolors,angle = 30, hjust=0, vjust=0, size=6))+
   theme(panel.border=element_blank())+
   theme(axis.title.x = element_blank()) + 
   theme(axis.title.y = element_blank()) + 
   #theme(legend.position="bottom")+
   theme(legend.key.size=unit(0.2, "cm"))+
   theme(legend.key.width=unit(0.5, "cm"))
 
 VGFs_Soil_Abundant
 ggsave("VGFs_Genes_abundance_in_soil.pdf", width = 20, height = 20, units = "cm", dpi = 300)
 #-------------------------------------------------- 
 
 IndHMcolors2 <- sequential_hcl(n = 10, palette = "Dark Mint")
 
 VGFs_abund00001.mL_Phyllo1$VGFs <- factor(VGFs_abund00001.mL_Phyllo1$VGFs, levels = Gene_ord)
 VGFs_Phyllo_Abundant<-ggplot(VGFs_abund00001.mL_Phyllo1 , aes(x=Urban_Gradient, y=VGFs, fill=value))+
   geom_tile(aes(fill=categories2))+
   scale_fill_manual(values= rev(IndHMcolors2), na.value="white", name= "Relative Abundance") +
   theme(axis.text.x = element_text(colour= "black", angle = 30, hjust = 1, vjust = 0.5, size=4), axis.text.y= element_text(colour="black", size=4))+
   theme(axis.title.x = element_blank(), axis.title.y =  element_blank())+
   theme(axis.text.x = element_text(colour= IndHMcolors,angle = 30, hjust=0, vjust=0, size=6))+
   theme(panel.border=element_blank())+
   theme(axis.title.x = element_blank()) + 
   theme(axis.title.y = element_blank()) + 
   #theme(legend.position="bottom")+
   theme(legend.key.size=unit(0.2, "cm"))+
   theme(legend.key.width=unit(0.5, "cm"))
 VGFs_Phyllo_Abundant 
 ggsave("VGFs_Genes_abundance_in_Phylo.pdf", width = 12, height = 20, units = "cm", dpi = 300)
 ##--------------------------------------------------------------------------------------------------------------
 
 #------------------------------------------------------------------------------ 
 # Melt the data frame
 VGFs_abund0000001.m <- melt(VGFs_abund0000001)
 # Add row names as the first variable
 VGFs_abund0000001.m$RowNames <- rownames(VGFs_abund00001)
 # Move the RowNames column to the first position
 VGFs_abund0000001.m <- VGFs_abund0000001.m[, c("RowNames", names(VGFs_abund0000001.m)[-ncol(VGFs_abund0000001.m)])]
 # Print the modified data frame
 print(VGFs_abund0000001.m)
 
 #lets check what genes are among the most abundant ones:
 
 levels(VGFs_abund0000001.m$variable)
 sheet_name <- "VGFs_Genes_Abundant"  # You can change "Sheet1" to whatever name you prefer
 write.xlsx(VGFs_abund0000001.m$variable, file = "VGFs_Genes_Abundant.xlsx", rowNames = FALSE, sheetName = sheet_name)
 
 Gene_ord<-c ("abaI",	"abaR",	"abaR2(d)",	"Ab-ITS",	"adeG",	"bap",	"basA",	"basG",	"bauA1",	"bauA2(d)",	"bfmR",	"csuA/B",	"csuE(d)",	"paa(d)",	"pgi",	"pgi(d)",	"phoQ",	"pic",	"aafA",	
              "aatA",	"pgaA(d)",	"prgH",	"shuA",	"espF",	"ibeA(d)",	"pefA",	"tviA",	"lpfC(d)",	"tir3(d)",	"astA(d)",	"eltA",	"eco-resv",	"allS",	"clpV/tssH",	"entB",	"entB(d)",	"fepB",	
              "fimA",	"fimA(d)[Klebs]",	"gtrA",	"iutA2",	"kp",	"ler",	"shdA(d)",	"wbbM(d)",	"wzm1",	"ybtS",	"icmF/tssM",	"icsA/virG1",	"csgA",	"csgG(d)",	"flmB",	"flmE",	"hilA",	"iroN",	
              "lpfE",	"lpxM",	"mark-AB1",	"mig-14(d)",	"mrkA",	"mrkD",	"pgaA",	"rck1",	"rck3(d)",	"set1B",	"shuT",	"wbbM",	"sigA",	"astA",	"iutA1(d)",	"nleA",	"ompA",	"sinH(d)",	"spa40",	
              "spvB",	"drap",	"focG",	"grvA",	"ivoN",	"pefA(d)")
 
 #--- VGFs_abund0000001.m$VGFs<-factor(VGFs_abund0000001.m$variable, levels =Gene_ord)
 #--- length( Gene_ord)
 # Assuming you want to rename multiple variables
 new_names <- c("SampleID", "VGFs", "Value")
 old_names <- c("RowNames", "variable", "Value")
 
 # Use a loop to rename multiple variables
 for (i in seq_along(old_names)) {
   names(VGFs_abund0000001.m)[names(VGFs_abund0000001.m) == old_names[i]] <- new_names[i]
 }
 
 
 
 VGFs_abund0000001.m <- merge(Metadata, VGFs_abund0000001.m, by = "SampleID") 
 VGFs_abund0000001.m 
 VGFs_abund0000001.m <- merge(VGFs_Taxa, VGFs_abund0000001.m, by = "VGFs") 
 VGFs_abund0000001.m 
 names(VGFs_abund0000001.m )
 

 ##------------------------ sUMMARY TABLE -----------------------------------------##
 
 #---------------------IN URBAN SOIL-------------------------##
 Pathogenic_Bacteria_Abundance <- VGFs_abund0000001.m %>%
   group_by(Substrate, Urban_Gradient, Attached.Bacteria_1) %>%
   summarise(VFGs_Abund = sum(value))
 Pathogenic_Bacteria_Abundance
 
 write.xlsx(Pathogenic_Bacteria_Abundance, file = "Pathogenic_Bacteria_Abundance.xlsx", rowNames = FALSE)

 
 

 ####################
 ARGs_Abund
 ARGs_Abund_1 <- ARGs_Abund
 names(ARGs_Abund_1)
 ARGs_Abund_1$sum[ARGs_Abund_1$sum ==0] <- 1.99999e-06 ##-- Replace 0 value by the lowest value more than the one any value revealed in all samples
 ARGs_Abund_1
 colSums(ARGs_Abund_1 != 0) ###--- Column with NON-ZERO Value: Total ARGs with NON-ZERO Value
 #####ARGs_Abund_new <- filter_if(ARGs_Abund, is.numeric, all_vars((.) != 0))  ##---// Remove raw with Zero
 ##---ARGs_Abund_new


 ARGs_Abund_in_urban_Soil_Phyllo<-ARGs_Abund_1
 dim(ARGs_Abund_in_urban_Soil_Phyllo)
 ARGs_Abund_in_urban_Soil_Phyllo
 
 str(ARGs_Abund_in_urban_Soil_Phyllo)
 #lets check this
 table(subset(ARGs_Abund_in_urban_Soil_Phyllo$ARGs,ARGs_Abund_in_urban_Soil_Phyllo$sum>9.536743e-07))
 #yes there are several ARGs that have detected in only one sample.
 
 library(fitdistrplus)
 library(logspline)
 
 ?fitdist
 ARGs_Abund_in_urban_Soil_Phyllo <- as.data.frame(ARGs_Abund_in_urban_Soil_Phyllo)
 ##---write.table(ARGs_Abund_in_urban_Soil_Phyllo, file="ARGs_Abund_in_urban_Soil_Phyllo.txt",sep="\t")

 #All values:
 fit_gammaARGs_1<-fitdist(ARGs_Abund_in_urban_Soil_Phyllo$sum,"gamma")
 plot(fit_gammaARGs_1)
 summary(fit_gammaARGs_1)
 ##-----------------------------------------------------
 qqp(ARGs_Abund_in_urban_Soil_Phyllo$sum, "gamma", 
     shape = fit_gammaARGs_1$estimate[[1]], rate = fit_gammaARGs_1$estimate[[2]])
 #poor fit
 
 #With only detected ARGs:
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$sum>9.536743e-09), "gamma", 
     shape = fit_gammaARGs_1$estimate[[1]], rate = fit_gammaARGs_1$estimate[[2]])
 
 #Let's try with some individual genes:
 unique(ARGs_Abund_in_urban_Soil_Phyllo$ARGs)
 
 fit_gamma_tetD_1<-fitdist(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="tetD"),"gamma")
 plot(fit_gamma_tetD_1)
 summary(fit_gamma_tetD_1)
 
 View(ARGs_Abund_in_urban_Soil_Phyllo)
 unique(ARGs_Abund_in_urban_Soil_Phyllo$ARGs)
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="tetD"), "gamma", 
     shape = fit_gamma_tetD_1$estimate[[1]], rate = fit_gamma_tetD_1$estimate[[2]])
 
 
 #hmmm...
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="aadA17"), "gamma", 
     shape = fit_gamma_tetD_1$estimate[[1]], rate = fit_gamma_tetD_1$estimate[[2]])
 
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="aadA9"), "gamma", 
     shape = fit_gamma_tetD_1$estimate[[1]], rate = fit_gamma_tetD_1$estimate[[2]])
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="cmx"), "gamma", 
     shape = fit_gamma_tetD_1$estimate[[1]], rate = fit_gamma_tetD_1$estimate[[2]])
 # This ARGs might cause problems
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="ErmB"), "gamma", 
     shape = fit_gamma_tetD_1$estimate[[1]], rate = fit_gamma_tetD_1$estimate[[2]])
 
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="mobA"), "gamma", 
     shape = fit_gamma_tetD_1$estimate[[1]], rate = fit_gamma_tetD_1$estimate[[2]])
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="vanB"), "gamma", 
     shape = fit_gamma_tetD_1$estimate[[1]], rate = fit_gamma_tetD_1$estimate[[2]])
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="tetQ"), "gamma", 
     shape = fit_gamma_tetD_1$estimate[[1]], rate = fit_gamma_tetD_1$estimate[[2]])
 
 
 # with many ARGss the fit might be ok but with some not. 
 # Fit is poorer with those ARGss that are highly abundant in certain samples.
 
 # I wonder if I could use some other distribution...
 
 qqp(log10(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$sum>9.536743e-07)), "norm")
 # this could be a possibility but I dont like the transformation
 
 qqp(log10(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="aadA17")), "norm")
 # Does'nt look so nice
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="aadA17"), "norm")
 # nope
 
 qqp(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="aadA17"), "lnorm")
 # nope
 
 qqp(ARGs_Abund_in_urban_Soil_Phyllo$sum, "lnorm")
 # nope
 
 descdist(ARGs_Abund_in_urban_Soil_Phyllo$sum, discrete = FALSE)
 
 descdist(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$ARGs=="aadA17"), discrete = FALSE)
 
 descdist(subset(ARGs_Abund_in_urban_Soil_Phyllo$sum, ARGs_Abund_in_urban_Soil_Phyllo$sum>9.536743e-09), discrete = FALSE)
 
 #beta distribution..? I'll consider: https://cran.r-project.org/web/packages/betareg/index.html
 #https://stats.stackexchange.com/questions/169391/beta-distribution-glm-with-categorical-independents-and-proportional-response
 # https://rcompanion.org/handbook/J_02.html
 
 
 var(ARGs_Abund_in_urban_Soil_Phyllo$sum)
 #[1] 1.846776e-05
 mean(ARGs_Abund_in_urban_Soil_Phyllo$sum)
 #[1] 0.0008195115
 2^(mean(ARGs_Abund_in_urban_Soil_Phyllo$sum))
 #[1] 1.000568
 
 
 library(plyr)
 library(dplyr)
 Ind_mvtab_1 <- ddply(ARGs_Abund_in_urban_Soil_Phyllo,.(Urban_Gradient),
                    summarise,value.mean = mean(sum),value.var = var(sum))
 Ind_mvtab_1
 plot(Ind_mvtab_1$value.mean,Ind_mvtab_1$value.var)
 
 # so I would say close enough! 
 # (Proportional = having the same or a constant ratio)
 # Core_Urban_Areas has a high variance but that is due to a biological reason
 
 # So I'll start fitting gamma model
 graphics.off()
 fit.I_ARGgamma1_1 <- glm(sum ~ Urban_Gradient, data = ARGs_Abund_in_urban_Soil_Phyllo, family = Gamma(link = "log"))
 fit.I_ARGgamma1_1
 
 ARGs_Abund_in_urban_Soil_PhylloNOtetD<-subset(ARGs_Abund_in_urban_Soil_Phyllo, ARGs!="tetD")
 fit.I_ARGgamma1_1 <-glm(sum ~ Urban_Gradient, data=ARGs_Abund_in_urban_Soil_PhylloNOtetD, family=Gamma (link="log"))
 fit.I_ARGgamma1_1
 

 
 # First I'll try with some individual ARGs
 
 AACglm<-subset(ARGs_Abund_in_urban_Soil_Phyllo, ARGs=="AAC(3)-Ia")
 
 fit.AACglmglmgamma1<-glm(sum ~ Block, data=AACglm, family=Gamma (link="log"))
 summary(fit.AACglmglmgamma1)
 # Works
 
 aadA9glm<-subset(ARGs_Abund_in_urban_Soil_Phyllo, ARGs=="aadA9")
 
 aadA9glmgamma1<-glm(sum ~ Block, data=aadA9glm, family=Gamma (link="log"))
 summary(aadA9glmgamma1)
 # Works
 
 table(subset(ARGs_Abund_in_urban_Soil_Phyllo$ARGs,ARGs_Abund_in_urban_Soil_Phyllo$sum>0))
 
 IncP_oriTglm<-subset(ARGs_Abund_in_urban_Soil_Phyllo, ARGs=="IncP_oriT")
 
 IncP_oriTglmgamma1<-glm(sum ~ Block, data=IncP_oriTglm, family=Gamma (link="log"))
 summary(IncP_oriTglmgamma1)
 # Works
 
 
 aadA7glm <- subset(ARGs_Abund_in_urban_Soil_Phyllo, ARGs == "aadA7")
 
 aadA7glmgamma1 <- glm(sum ~ Block, data = aadA7glm, family = Gamma(link = "log"))
 summary(aadA7glmgamma1)
 
 
 tetRglm<-subset(ARGs_Abund_in_urban_Soil_Phyllo, ARGs=="tetR")
 
 tetRglmgamma1<-glm(sum ~ Block, data=tetRglm, family=Gamma (link="log"))
 summary(tetRglmgamma1)
 # Works
 
 unique(ARGs_Abund_in_urban_Soil_Phyllo$SampleID)

 
 ######---------------- SUBSET SOIL and PHYLLOSPHERE ---------------------####
 
 ARGs_Abund_in_urban_Soil<-subset(ARGs_Abund_in_urban_Soil_Phyllo, Substrate!="phyllosphere")
 ARGs_Abund_in_urban_Phyllo<-subset(ARGs_Abund_in_urban_Soil_Phyllo, Substrate!="soil")
 ARGs_Abund_in_urban_Phyllo$Block<-factor(ARGs_Abund_in_urban_Phyllo$Block, levels=c("A","B","C","D","E"))
 ####-------------------------------------------------------------------------------------------------------#######

 ########### --------------------- TRY THE MAIN URBAN GRADIENTS THAT MAY BE THE SOURCES OF MISMATCHING --------##
 ARGs_GLAMMS_Soil111_4
 ARGs_GLAMMS_Phyllo111_4
 VFGs_GLAMMS_Soil111_4
 VFGs_GLAMMS_Phyllo111_4
########--------------------------------------------------------------------------------------------###### 
 ARGs_GLAMMS_Soil111_4$sum[ARGs_GLAMMS_Soil111_4$sum ==0] <- 1.99999e-06 ##-- Replace 0 value by the lowest value more than the one any value revealed in all samples
 ARGs_GLAMMS_Soil111_4$Block<-as.factor(ARGs_GLAMMS_Soil111_4$Block)
 ARGs_GLAMMS_Soil111_4$Block<-factor(ARGs_GLAMMS_Soil111_4$Block, levels=c("A","B","C","D","E"))
 str(ARGs_GLAMMS_Soil111_4)
 
 library(multcomp)
############################################################################################################################ 
#########----------------- GAMMAS ANALYSIS IN SOIL ---------------------------------#####
 ARGs_GLAMMS_Soil111_4<-droplevels(ARGs_GLAMMS_Soil111_4)
 levels(ARGs_GLAMMS_Soil111_4$Block)
 str(ARGs_GLAMMS_Soil111_4)
 names(ARGs_GLAMMS_Soil111_4)
 library(multcomp)
 
 JackIsTheBest_Soil <- function(ARGx) {
   TEMP <- subset(ARGs_GLAMMS_Soil111_4, ARGs == ARGx)
   
   # Create the gamma fit model
   gammafit <- tryCatch(
     {
       glm(sum ~ Block, data = TEMP, family = Gamma(link = "log"), maxit = 10000)
     },
     error = function(e) {
       message("Error creating gammafit model: ", e$message)
       return(NULL)
     }
   )
   
   # Check if gammafit object was created successfully
   if (!is.null(gammafit)) {
     # Create the multiple comparisons object
     mcp_obj <- mcp(Block = "Tukey")
     
     glht.gamma <- glht(gammafit, mcp_obj)
     gamma_glht_df <- as.data.frame(summary(glht(glht.gamma))$test[-(1:2)])
     return(gamma_glht_df)
   } else {
     message("Unable to create gammafit model for ARGx: ", ARGx)
     return(NULL)
   }
 }
 
 JackIsTheBest_Soil("mdtG") 
 
 gamma_tmpARGs_soil <- unique(ARGs_GLAMMS_Soil111_4$ARGs)
 gamma_tmpARGs_soil_test <- lapply(gamma_tmpARGs_soil, JackIsTheBest_Soil)
 #There were 50 or more warnings (use warnings() to see the first 50)
 # IT WORKS!!!!! So Spring water was my problem!!
 head(gamma_tmpARGs_soil_test)
 names(gamma_tmpARGs_soil_test)
 names(gamma_tmpARGs_soil_test) <- gamma_tmpARGs_soil
 head(gamma_tmpARGs_soil_test)
 gamma_tmpARGs_soil_test.m<- melt(gamma_tmpARGs_soil_test)
 head(gamma_tmpARGs_soil_test.m)
 dim(gamma_tmpARGs_soil_test.m)
 #[1] 2244    4
 
 levels(gamma_tmpARGs_soil_test.m$variable)
 coefDF<-subset(gamma_tmpARGs_soil_test.m, variable=="coefficients")
 stderDF<-subset(gamma_tmpARGs_soil_test.m, variable=="sigma")
 tstatDF<-subset(gamma_tmpARGs_soil_test.m, variable=="tstat")
 pvalDF<-subset(gamma_tmpARGs_soil_test.m, variable=="pvalues")
 coefDF<-coefDF[,-c(1,2)]
 colnames(coefDF)<-c("coefficients","ARG_c")
 stderDF<-stderDF[,-c(1,2)]
 colnames(stderDF)<-c("std_error","ARG_s")
 tstatDF<-tstatDF[,-c(1,2)]
 colnames(tstatDF)<-c("tstat","ARG_t")
 pvalDF<-pvalDF[,-c(1,2)]
 colnames(pvalDF)<-c("p-value","ARG_p")
 
 Gamma_ARGs_results_soil<-cbind(coefDF,stderDF,tstatDF,pvalDF)
 
 #I need the comparisons LIST
 df_ARGcomp_gamma_Soil<-as.data.frame(lapply(gamma_tmpARGs_soil, JackIsTheBest_Soil))
 getARGcomparisons_Soil<-rownames(df_ARGcomp_gamma_Soil)
 
 getARGcomparisons_Soil
 
 #CHECKING IF MY COMPARISONS MEET MATCH WITH THE COMPARISONS LIST 2,150/215=10##
 dim(Gamma_ARGs_results_soil)
 length(unique(ARGs_GLAMMS_Soil111_4$ARGs))
 1360 /136
 
################## ======AADDINNG THE COMPARISONS COLUMNS ON MY RESULTS ============================######## 
 

 
 #I will add a column that has the comparison
 Gamma_ARGs_results_soil$comparison<-rep(getARGcomparisons_Soil,136)
 #And I want to remove columns that were used for checking the data
 Gamma_ARGs_results_soil$ARGs<-Gamma_ARGs_results_soil$ARG_c
 Gamma_ARGs_results_soil<-Gamma_ARGs_results_soil[,-c(2,4,6,8)]
 #Renaming columns
 colnames(Gamma_ARGs_results_soil)<-c("Delta.Estimate", "Std.error", "z-value", "p.value","Comparison", "ARGs")
 #And finally the rownames
 rownames(Gamma_ARGs_results_soil)<-interaction(Gamma_ARGs_results_soil$Comparison,Gamma_ARGs_results_soil$ARGs, sep="_")
 head(rownames(Gamma_ARGs_results_soil))
 
 ###############------------------- IDENTIFYING THE ARGs which are statistically significance------------------------------------###
 Gamma_ARGs_results_soil_sigP<-subset(Gamma_ARGs_results_soil, p.value <= 0.05)
 #sig.PC_NC$p.adjFDR<-p.adjust(sig.PC_NC$p.value,method="BH")
 Gamma_ARGs_results_soil_sigP$p.adj<-p.adjust(Gamma_ARGs_results_soil_sigP$p.value, method="BH")
 
 #Gamma_ARGs_results_soil_sigP$p.adj<-p.adjust(Gamma_ARGs_results_soil_sigP$p.value, method="bonferroni")
 
 Gamma_ARGs_results_soil_sigP.a<-subset(Gamma_ARGs_results_soil_sigP, p.adj <= 0.05)
 dim(Gamma_ARGs_results_soil_sigP.a)
 dim(Gamma_ARGs_results_soil_sigP)
 ########------------------------------CALCULATE THE UNIQUE ARGs IN EACH SOIL samples -----------------------------######
 
 unique(Gamma_ARGs_results_soil_sigP.a$ARGs)
 
 #######------------- CHECKING THE FOLD CHANGE EFFICIENCY ---------------------------------####
 
 # I can calculate the fold changes:
 Gamma_ARGs_results_soil_sigP.a$Fold.Change<-exp(Gamma_ARGs_results_soil_sigP.a$Delta.Estimate)
 
 # About the fold changes:
 JackIsTheBest_Soil("mdtG")
 
 #------>  JackIsTheBest_Soil("mdtG")
 #------ coefficients     sigma      tstat   pvalues        type
 #------ B - A  -0.43482817 0.2495497 -1.7424510 0.4077993 single-step
 #------C - A  -0.58104156 0.2495497 -2.3283598 0.1358733 single-step
 #------ D - A  -0.31557142 0.2495497 -1.2645633 0.7130353 single-step
 #------E - A  -0.52326777 0.2495497 -2.0968477 0.2212650 single-step
 #------C - B  -0.14621338 0.2495497 -0.5859088 0.9772052 single-step
 #------D - B   0.11925676 0.2495497  0.4778878 0.9893682 single-step
 #------E - B  -0.08843960 0.2495497 -0.3543967 0.9966293 single-step
 #------D - C   0.26547014 0.2495497  1.0637966 0.8251154 single-step
 #------E - C   0.05777378 0.2495497  0.2315121 0.9993652 single-step
 #------E - D  -0.20769636 0.2495497 -0.8322845 0.9206643 single-step
 #------Warning messages:
 #------  1: In chkdots(...) : Argument(s) ‘complete’ passed to ‘...’ are ignored
 #------ 2: In chkdots(...) : Argument(s) ‘complete’ passed to ‘...’ are ignored
 
 
 # The output above shows "delta estimates" under the column coefficients. 
 # By these I mean for example that the estimate of  "iNNER URBAN [B]" minus the estimate of "cORE URBAN [A]" is -0.43482817 
 #we can check this: 
 
 mdtGglm<-subset(ARGs_GLAMMS_Soil111_4, ARGs=="mdtG")
 mdtGglmgamma1<-glm(sum ~ Block, data=mdtGglm, family=Gamma (link="log"))
 summary(mdtGglmgamma1)
 
 #------Coefficients:
 #------  Estimate Std. Error t value Pr(>|t|)    
 #------ (Intercept)  -5.4698     0.1765 -30.998   <2e-16 ***
 #------  BlockB       -0.4348     0.2495  -1.742   0.0902 .  
 #------  BlockC       -0.5810     0.2495  -2.328   0.0258 *  
 #------  BlockD       -0.3156     0.2495  -1.265   0.2144    
 #------  BlockE       -0.5233     0.2495  -2.097   0.0433 *  
 #------  ---
 #------  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 #------(Dispersion parameter for Gamma family taken to be 0.2491003)

 -0.4348  -0.5810
 
 #[1] -1.0158
 
 # Gamma distribution is a member of the exponential family and I used the log link. 
 # in other words, to backtransform the "delta estimate",
 # I need to do this:
 exp(-1.0158)
 #[1] 0.3621126
 #This number is the fold change. BUT WE ARE NOT get the appropriate Scripts that could allow us to calculate it appropriately. 
 
 # to understand what is going on, I will do some calculations with the data: 
 mean(with (ARGs_GLAMMS_Soil111_4, subset(sum, ARGs=="mdtG" & Block=="B")))
 #[1] 0.00272675
 mean(with (ARGs_GLAMMS_Soil111_4, subset(sum, ARGs=="mdtG" & Block=="A")))
 #[1] 0.001898514 
 
 # iN PREVIOUS aNALYSIS WE SSE GOT INNER URBAN-CORE URBAN =  -1.0158. This was in log scale.
 # the corresponding in normal scale would be: 
 0.00272675/0.004212005
 
 #[1] 0.6473758
 # It Is NOT the same number that we got with exp(-1.0158)!
 # Here gene mdtG is ~ 0.6473758 times less abundant in  INNER URBAN versus CORE URBAN
 
 
 # to understand what is going on, I will do some calculations with the data: 
 mean(with (ARGs_GLAMMS_Soil111_4, subset(sum, ARGs=="aadA9" & Block=="B")))
 #[1] 2.564969e-06
 mean(with (ARGs_GLAMMS_Soil111_4, subset(sum, ARGs=="aadA9" & Block=="A")))
 #[1] 1.902084e-05
 2.564969e-06/1.902084e-05
 #0.1348505  
 
 
 # to understand what is going on, I will do some calculations with the data: 
 mean(with (ARGs_GLAMMS_Soil111_4, subset(sum, ARGs=="IncP_oriT" & Block=="B")))
 #[1] 6.913914e-05
 mean(with (ARGs_GLAMMS_Soil111_4, subset(sum, ARGs=="IncP_oriT" & Block=="A")))
 #[1] 8.23952e-05 
 
 6.913914e-05/8.23952e-05
 #[1] 0.8391161
 
 
 # Also the standard error needs to be transformed back
 Gamma_ARGs_results_soil_sigP.a$expStd.error<-exp(Gamma_ARGs_results_soil_sigP.a$Std.error)
 
 # log 2 transformation is handy for plotting
 
 Gamma_ARGs_results_soil_sigP.a$log2Fold.Change<-log2(Gamma_ARGs_results_soil_sigP.a$Fold.Change)
 
 Gamma_ARGs_results_soil_sigP.a$log2expStd.e<-log2(Gamma_ARGs_results_soil_sigP.a$expStd.error)
 
 Gamma_ARGs_results_soil_sigP.a[["change"]] = ifelse(Gamma_ARGs_results_soil_sigP.a[["log2Fold.Change"]] < 0, "decrease", "increase")
 
 
 ####---- NOW WE PLOTS THE RESULTS, WE NEED TO GET FIRST OF AALL THE GENES THAT HAD SIGNIFICANT DIFFERENCES IN EACH OF 10 COMPARISONS
 # I want to plot the results  
 # I will need to get the genes that had significant differences in each 
 # comparison.
 
 str(Gamma_ARGs_results_soil_sigP.a)
 getARGcomparisons_Soil
 Gamma_ARGs_results_sigP_soil_CoreInner<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="B - A")
 Gamma_ARGs_results_sigP_soil_CoreInner<-droplevels(Gamma_ARGs_results_sigP_soil_CoreInner)
 length(unique(Gamma_ARGs_results_sigP_soil_CoreInner$ARGs))
 #[1] 19
 Gamma_ARGs_results_sigP_soil_CoreOuter<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="C - A")
 Gamma_ARGs_results_sigP_soil_CoreOuter<-droplevels(Gamma_ARGs_results_sigP_soil_CoreOuter)
 length(unique(Gamma_ARGs_results_sigP_soil_CoreOuter$ARGs))
 #[1] 27
 
 Gamma_ARGs_results_sigP_soil_CoreExurban<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="D - A")
 Gamma_ARGs_results_sigP_soil_CoreExurban<-droplevels(Gamma_ARGs_results_sigP_soil_CoreExurban)
 length(unique(Gamma_ARGs_results_sigP_soil_CoreExurban$ARGs))
 #[1] 16
 
 Gamma_ARGs_results_sigP_soil_CoreR.Fringe<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="E - A")
 Gamma_ARGs_results_sigP_soil_CoreR.Fringe<-droplevels(Gamma_ARGs_results_sigP_soil_CoreR.Fringe)
 length(unique(Gamma_ARGs_results_sigP_soil_CoreR.Fringe$ARGs))
 #[1] 47
 
 Gamma_ARGs_results_sigP_soil_InnerOuter<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="C - B")
 Gamma_ARGs_results_sigP_soil_InnerOuter<-droplevels(Gamma_ARGs_results_sigP_soil_InnerOuter)
 length(unique(Gamma_ARGs_results_sigP_soil_InnerOuter$ARGs))
 #[1] 10
 
 Gamma_ARGs_results_sigP_soil_InnerExurban<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="D - B")
 Gamma_ARGs_results_sigP_soil_InnerExurban<-droplevels(Gamma_ARGs_results_sigP_soil_InnerExurban)
 length(unique(Gamma_ARGs_results_sigP_soil_InnerExurban$ARGs))
 #[1] 14
 
 Gamma_ARGs_results_sigP_soil_InnerR.Fringe<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="E - B")
 Gamma_ARGs_results_sigP_soil_InnerR.Fringe<-droplevels(Gamma_ARGs_results_sigP_soil_InnerR.Fringe)
 length(unique(Gamma_ARGs_results_sigP_soil_InnerR.Fringe$ARGs))
 #[1] 20 
 
 Gamma_ARGs_results_sigP_soil_OuterExurban<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="D - C")
 Gamma_ARGs_results_sigP_soil_OuterExurban<-droplevels(Gamma_ARGs_results_sigP_soil_OuterExurban)
 length(unique(Gamma_ARGs_results_sigP_soil_OuterExurban$ARGs))
 #[1] 9 
 
 Gamma_ARGs_results_sigP_soil_OuterR.Fringe<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="E - C")
 Gamma_ARGs_results_sigP_soil_OuterR.Fringe<-droplevels(Gamma_ARGs_results_sigP_soil_OuterR.Fringe)
 length(unique(Gamma_ARGs_results_sigP_soil_OuterR.Fringe$ARGs))
 #[1] 21
 
 Gamma_ARGs_results_sigP_soil_ExurbanR.Fringe<-subset(Gamma_ARGs_results_soil_sigP.a, Comparison=="E - D")
 Gamma_ARGs_results_sigP_soil_ExurbanR.Fringe<-droplevels(Gamma_ARGs_results_sigP_soil_ExurbanR.Fringe)
 length(unique(Gamma_ARGs_results_sigP_soil_ExurbanR.Fringe$ARGs))
 #[1] 29
 
 
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change >0))
 #[1] 6 12
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change <0))
 #[1] 13  12
 log2(1)
 log(2)
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change >2))
 #[1] 6 12
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change < -2))
 #[1] 13  12
 
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change >4))
 #[1] 6 12
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change < -4))
 #[1] 13  12
 
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change < -9.965784))
 #[1] 19 12
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change > 9.965784))
 #[1] 19 12
 
 log2(50)
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change < -5.643856))
 #[1]  9 11
 dim(subset(Gamma_ARGs_results_sigP_soil_CoreInner, log2Fold.Change > 5.643856))

 #####--------------------1
 CoreInner<- Gamma_ARGs_results_sigP_soil_CoreInner %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Inner-Urban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-4,3), breaks=seq(-4,3,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreInner <- CoreInner + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreInner
 #####--------------------2 
 CoreOuter<- Gamma_ARGs_results_sigP_soil_CoreOuter %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Outer-Surburban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-4,4), breaks=seq(-6,4,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreOuter <- CoreOuter + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreOuter
 #####--------------------3 
 CoreExurban<- Gamma_ARGs_results_sigP_soil_CoreExurban %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,5), breaks=seq(-5,5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreExurban <- CoreExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreExurban
 #####--------------------4 
 CoreR.Fringe<- Gamma_ARGs_results_sigP_soil_CoreR.Fringe %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,4.5), breaks=seq(-5,4.5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreR.Fringe <- CoreR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreR.Fringe
 #####--------------------5 
 InnerOuter<- Gamma_ARGs_results_sigP_soil_InnerOuter %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to Outer-Suburban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,4.5), breaks=seq(-5,4.5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerOuter <- InnerOuter + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerOuter
 #####--------------------6 
 InnerExurban<- Gamma_ARGs_results_sigP_soil_InnerExurban %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,6), breaks=seq(-5,6,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerExurban <- InnerExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerExurban
 #####--------------------7
 InnerR.Fringe<- Gamma_ARGs_results_sigP_soil_InnerR.Fringe %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-7,6), breaks=seq(-7,6,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerR.Fringe <- InnerR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerR.Fringe
 #####--------------------8
 OuterExurban<- Gamma_ARGs_results_sigP_soil_OuterExurban %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from OuterSuburban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,6), breaks=seq(-5,6,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 OuterExurban <- OuterExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 OuterExurban
 #####--------------------9
 OuterR.Fringe<- Gamma_ARGs_results_sigP_soil_OuterR.Fringe %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from OuterSuburban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,4), breaks=seq(-5,4,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 OuterR.Fringe <- OuterR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 OuterR.Fringe
 #####--------------------10
 ExurbanR.Fringe<- Gamma_ARGs_results_sigP_soil_ExurbanR.Fringe %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#999999", "decrease" = "red")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Exurban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold.change",limits=c(-5,5), breaks=seq(-5,5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 ExurbanR.Fringe
 ExurbanR.Fringe <- ExurbanR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 ExurbanR.Fringe
 
 library(patchwork)# to compile the plots together 
 ARGscomparisons_Soil<-(CoreInner|CoreOuter|CoreExurban|CoreR.Fringe|InnerOuter)/(InnerExurban|InnerR.Fringe|OuterExurban|OuterR.Fringe|ExurbanR.Fringe)
 ARGscomparisons_Soil
 ggsave(filename = "ARGscomparisons_Soil_Gammafit.png", plot = ARGscomparisons_Soil, width = 20, height = 25, dpi = 2500, units = "cm")
 ggsave(filename = "ARGscomparisons_Soil_Gammafit.pdf", plot = ARGscomparisons_Soil, width = 20, height = 25, dpi = 2500, units = "cm")
 ########-----------------------------------------------------------------------------------------------------------------------------------------------------###########
 
 
 
 ########### --------------------- ANALYZING THE ARGs IN PHYLLOSPHERE --------##
 
 ARGs_GLAMMS_Phyllo111_4
 VFGs_GLAMMS_Soil111_4
 VFGs_GLAMMS_Phyllo111_4
 ########--------------------------------------------------------------------------------------------###### 
 ARGs_GLAMMS_Phyllo111_4$sum[ARGs_GLAMMS_Phyllo111_4$sum ==0] <- 1.99999e-06 ##-- Replace 0 value by the lowest value more than the one any value revealed in all samples
 ARGs_GLAMMS_Phyllo111_4$Block<-as.factor(ARGs_GLAMMS_Phyllo111_4$Block)
 ARGs_GLAMMS_Phyllo111_4$Block<-factor(ARGs_GLAMMS_Phyllo111_4$Block, levels=c("A","B","C","D","E"))
 str(ARGs_GLAMMS_Phyllo111_4)
 
 library(multcomp)
 ############################################################################################################################ 
 #########----------------- GAMMAS ANALYSIS IN SOIL ---------------------------------#####
 ARGs_GLAMMS_Phyllo111_4<-droplevels(ARGs_GLAMMS_Phyllo111_4)
 levels(ARGs_GLAMMS_Phyllo111_4$Block)
 str(ARGs_GLAMMS_Phyllo111_4)
 names(ARGs_GLAMMS_Phyllo111_4)
 library(multcomp)
 
 JackIsTheBest_Phyllo <- function(ARGx) {
   TEMP <- subset(ARGs_GLAMMS_Phyllo111_4, ARGs == ARGx)
   
   # Create the gamma fit model
   gammafit <- tryCatch(
     {
       glm(sum ~ Block, data = TEMP, family = Gamma(link = "log"), maxit = 10000)
     },
     error = function(e) {
       message("Error creating gammafit model: ", e$message)
       return(NULL)
     }
   )
   
   # Check if gammafit object was created successfully
   if (!is.null(gammafit)) {
     # Create the multiple comparisons object
     mcp_obj <- mcp(Block = "Tukey")
     
     glht.gamma <- glht(gammafit, mcp_obj)
     gamma_glht_df <- as.data.frame(summary(glht(glht.gamma))$test[-(1:2)])
     return(gamma_glht_df)
   } else {
     message("Unable to create gammafit model for ARGx: ", ARGx)
     return(NULL)
   }
 }
 
 JackIsTheBest_Phyllo("mdtG") 
 
 gamma_tmpARGs_Phyllo <- unique(ARGs_GLAMMS_Phyllo111_4$ARGs)
 gamma_tmpARGs_Phyllo_test <- lapply(gamma_tmpARGs_Phyllo, JackIsTheBest_Phyllo)
 #There were 50 or more warnings (use warnings() to see the first 50)
 # IT WORKS!!!!! So Spring water was my problem!!
 head(gamma_tmpARGs_Phyllo_test)
 names(gamma_tmpARGs_Phyllo_test)
 names(gamma_tmpARGs_Phyllo_test) <- gamma_tmpARGs_Phyllo
 head(gamma_tmpARGs_Phyllo_test)
 gamma_tmpARGs_Phyllo_test.m<- melt(gamma_tmpARGs_Phyllo_test)
 head(gamma_tmpARGs_Phyllo_test.m)
 dim(gamma_tmpARGs_Phyllo_test.m)
 #[1] [1] 6080    4
 
 levels(gamma_tmpARGs_Phyllo_test.m$variable)
 coefDF<-subset(gamma_tmpARGs_Phyllo_test.m, variable=="coefficients")
 stderDF<-subset(gamma_tmpARGs_Phyllo_test.m, variable=="sigma")
 tstatDF<-subset(gamma_tmpARGs_Phyllo_test.m, variable=="tstat")
 pvalDF<-subset(gamma_tmpARGs_Phyllo_test.m, variable=="pvalues")
 coefDF<-coefDF[,-c(1,2)]
 colnames(coefDF)<-c("coefficients","ARG_c")
 stderDF<-stderDF[,-c(1,2)]
 colnames(stderDF)<-c("std_error","ARG_s")
 tstatDF<-tstatDF[,-c(1,2)]
 colnames(tstatDF)<-c("tstat","ARG_t")
 pvalDF<-pvalDF[,-c(1,2)]
 colnames(pvalDF)<-c("p-value","ARG_p")
 
 Gamma_ARGs_results_Phyllo<-cbind(coefDF,stderDF,tstatDF,pvalDF)
 
 #I need the comparisons LIST
 df_ARGcomp_gamma_Phyllo<-as.data.frame(lapply(gamma_tmpARGs_Phyllo, JackIsTheBest_Phyllo))
 getARGcomparisons_Phyllo<-rownames(df_ARGcomp_gamma_Phyllo)
 
 getARGcomparisons_Phyllo
 
 #CHECKING IF MY COMPARISONS MEET MATCH WITH THE COMPARISONS LIST 2,150/215=10##
 dim(Gamma_ARGs_results_Phyllo)
 length(unique(ARGs_GLAMMS_Phyllo111_4$ARGs))
 1520 /152
 
 ################## ======AADDINNG THE COMPARISONS COLUMNS ON MY RESULTS ============================######## 
 
 
 
 #I will add a column that has the comparison
 Gamma_ARGs_results_Phyllo$comparison<-rep(getARGcomparisons_Phyllo,152)
 #And I want to remove columns that were used for checking the data
 Gamma_ARGs_results_Phyllo$ARGs<-Gamma_ARGs_results_Phyllo$ARG_c
 Gamma_ARGs_results_Phyllo<-Gamma_ARGs_results_Phyllo[,-c(2,4,6,8)]
 #Renaming columns
 colnames(Gamma_ARGs_results_Phyllo)<-c("Delta.Estimate", "Std.error", "z-value", "p.value","Comparison", "ARGs")
 #And finally the rownames
 rownames(Gamma_ARGs_results_Phyllo)<-interaction(Gamma_ARGs_results_Phyllo$Comparison,Gamma_ARGs_results_Phyllo$ARGs, sep="_")
 head(rownames(Gamma_ARGs_results_Phyllo))
 
 ###############------------------- IDENTIFYING THE ARGs which are statistically significance------------------------------------###
 Gamma_ARGs_results_Phyllo_sigP<-subset(Gamma_ARGs_results_Phyllo, p.value <= 0.05)
 #sig.PC_NC$p.adjFDR<-p.adjust(sig.PC_NC$p.value,method="BH")
 Gamma_ARGs_results_Phyllo_sigP$p.adj<-p.adjust(Gamma_ARGs_results_Phyllo_sigP$p.value, method="BH")
 
 #Gamma_ARGs_results_Phyllo_sigP$p.adj<-p.adjust(Gamma_ARGs_results_Phyllo_sigP$p.value, method="bonferroni")
 
 Gamma_ARGs_results_Phyllo_sigP.a<-subset(Gamma_ARGs_results_Phyllo_sigP, p.adj <= 0.05)
 dim(Gamma_ARGs_results_Phyllo_sigP.a)
 dim(Gamma_ARGs_results_Phyllo_sigP)
 ########------------------------------CALCULATE THE UNIQUE ARGs IN EACH Phyllo samples -----------------------------######
 
 unique(Gamma_ARGs_results_Phyllo_sigP.a$ARGs)
 
 #######------------- CHECKING THE FOLD CHANGE EFFICIENCY ---------------------------------####
 
 # I can calculate the fold changes:
 Gamma_ARGs_results_Phyllo_sigP.a$Fold.Change<-exp(Gamma_ARGs_results_Phyllo_sigP.a$Delta.Estimate)
 
 # About the fold changes:
 JackIsTheBest_Phyllo("mdtG")
 
 mdtGglm<-subset(ARGs_GLAMMS_Phyllo111_4, ARGs=="mdtG")
 mdtGglmgamma1<-glm(sum ~ Block, data=mdtGglm, family=Gamma (link="log"))
 summary(mdtGglmgamma1)
 ######============= MAKE REVISION TO SEE HOW MY RESULTS WILL BE LOOK LIKE
 mean(with (ARGs_GLAMMS_Phyllo111_4, subset(sum, ARGs=="mdtG" & Block=="B")))
 #[1] 0.00272675
 mean(with (ARGs_GLAMMS_Phyllo111_4, subset(sum, ARGs=="mdtG" & Block=="A")))
 # to understand what is going on, I will do some calculations with the data: 
 mean(with (ARGs_GLAMMS_Phyllo111_4, subset(sum, ARGs=="aadA9" & Block=="B")))
 #[1] 2.564969e-06
 mean(with (ARGs_GLAMMS_Phyllo111_4, subset(sum, ARGs=="aadA9" & Block=="A")))
 
 # to understand what is going on, I will do some calculations with the data: 
 mean(with (ARGs_GLAMMS_Phyllo111_4, subset(sum, ARGs=="IncP_oriT" & Block=="B")))
 #[1] 6.913914e-05
 mean(with (ARGs_GLAMMS_Phyllo111_4, subset(sum, ARGs=="IncP_oriT" & Block=="A")))
 #[1] 8.23952e-05 

 # Also the standard error needs to be transformed back
 Gamma_ARGs_results_Phyllo_sigP.a$expStd.error<-exp(Gamma_ARGs_results_Phyllo_sigP.a$Std.error)
 
 # log 2 transformation is handy for plotting
 
 Gamma_ARGs_results_Phyllo_sigP.a$log2Fold.Change<-log2(Gamma_ARGs_results_Phyllo_sigP.a$Fold.Change)
 
 Gamma_ARGs_results_Phyllo_sigP.a$log2expStd.e<-log2(Gamma_ARGs_results_Phyllo_sigP.a$expStd.error)
 
 Gamma_ARGs_results_Phyllo_sigP.a[["change"]] = ifelse(Gamma_ARGs_results_Phyllo_sigP.a[["log2Fold.Change"]] < 0, "decrease", "increase")
 
 ####---- NOW WE PLOTS THE RESULTS, WE NEED TO GET FIRST OF AALL THE GENES THAT HAD SIGNIFICANT DIFFERENCES IN EACH OF 10 COMPARISONS
 # I want to plot the results  
 # I will need to get the genes that had significant differences in each 
 # comparison.
 
 str(Gamma_ARGs_results_Phyllo_sigP.a)
 getARGcomparisons_Phyllo
 Gamma_ARGs_results_sigP_Phyllo_CoreInner<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="B - A")
 Gamma_ARGs_results_sigP_Phyllo_CoreInner<-droplevels(Gamma_ARGs_results_sigP_Phyllo_CoreInner)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_CoreInner$ARGs))
 #[1] 14
 Gamma_ARGs_results_sigP_Phyllo_CoreOuter<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="C - A")
 Gamma_ARGs_results_sigP_Phyllo_CoreOuter<-droplevels(Gamma_ARGs_results_sigP_Phyllo_CoreOuter)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_CoreOuter$ARGs))
 #[1] 23
 
 Gamma_ARGs_results_sigP_Phyllo_CoreExurban<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="D - A")
 Gamma_ARGs_results_sigP_Phyllo_CoreExurban<-droplevels(Gamma_ARGs_results_sigP_Phyllo_CoreExurban)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_CoreExurban$ARGs))
 #[1] 11
 
 Gamma_ARGs_results_sigP_Phyllo_CoreR.Fringe<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="E - A")
 Gamma_ARGs_results_sigP_Phyllo_CoreR.Fringe<-droplevels(Gamma_ARGs_results_sigP_Phyllo_CoreR.Fringe)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_CoreR.Fringe$ARGs))
 #[1] 39
 
 Gamma_ARGs_results_sigP_Phyllo_InnerOuter<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="C - B")
 Gamma_ARGs_results_sigP_Phyllo_InnerOuter<-droplevels(Gamma_ARGs_results_sigP_Phyllo_InnerOuter)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_InnerOuter$ARGs))
 #[1] 10
 
 Gamma_ARGs_results_sigP_Phyllo_InnerExurban<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="D - B")
 Gamma_ARGs_results_sigP_Phyllo_InnerExurban<-droplevels(Gamma_ARGs_results_sigP_Phyllo_InnerExurban)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_InnerExurban$ARGs))
 #[1] 11
 
 Gamma_ARGs_results_sigP_Phyllo_InnerR.Fringe<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="E - B")
 Gamma_ARGs_results_sigP_Phyllo_InnerR.Fringe<-droplevels(Gamma_ARGs_results_sigP_Phyllo_InnerR.Fringe)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_InnerR.Fringe$ARGs))
 #[1] 17
 
 Gamma_ARGs_results_sigP_Phyllo_OuterExurban<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="D - C")
 Gamma_ARGs_results_sigP_Phyllo_OuterExurban<-droplevels(Gamma_ARGs_results_sigP_Phyllo_OuterExurban)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_OuterExurban$ARGs))
 #[1] 7 
 
 Gamma_ARGs_results_sigP_Phyllo_OuterR.Fringe<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="E - C")
 Gamma_ARGs_results_sigP_Phyllo_OuterR.Fringe<-droplevels(Gamma_ARGs_results_sigP_Phyllo_OuterR.Fringe)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_OuterR.Fringe$ARGs))
 #[1] 17
 
 Gamma_ARGs_results_sigP_Phyllo_ExurbanR.Fringe<-subset(Gamma_ARGs_results_Phyllo_sigP.a, Comparison=="E - D")
 Gamma_ARGs_results_sigP_Phyllo_ExurbanR.Fringe<-droplevels(Gamma_ARGs_results_sigP_Phyllo_ExurbanR.Fringe)
 length(unique(Gamma_ARGs_results_sigP_Phyllo_ExurbanR.Fringe$ARGs))
 #[1] 23
 
 
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change >0))
 #[1] 6 12
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change <0))
 #[1] 13  12
 log2(1)
 log(2)
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change >2))
 #[1] 6 12
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change < -2))
 #[1] 13  12
 
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change >4))
 #[1] 6 12
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change < -4))
 #[1] 13  12
 
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change < -9.965784))
 #[1] 19 12
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change > 9.965784))
 #[1] 19 12
 
 log2(50)
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change < -5.643856))
 #[1]  9 11
 dim(subset(Gamma_ARGs_results_sigP_Phyllo_CoreInner, log2Fold.Change > 5.643856))
 
 #####--------------------1
 CoreInner<- Gamma_ARGs_results_sigP_Phyllo_CoreInner %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Inner-Urban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,3), breaks=seq(-5,3,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreInner <- CoreInner + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreInner
 #####--------------------2 
 CoreOuter<- Gamma_ARGs_results_sigP_Phyllo_CoreOuter %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Outer-Surburban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,4), breaks=seq(-5,4,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreOuter <- CoreOuter + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreOuter
 #####--------------------3 
 CoreExurban<- Gamma_ARGs_results_sigP_Phyllo_CoreExurban %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,4), breaks=seq(-5,4,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreExurban <- CoreExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreExurban
 #####--------------------4 
 CoreR.Fringe<- Gamma_ARGs_results_sigP_Phyllo_CoreR.Fringe %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,4.5), breaks=seq(-5,4.5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreR.Fringe <- CoreR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreR.Fringe
 #####--------------------5 
 InnerOuter<- Gamma_ARGs_results_sigP_Phyllo_InnerOuter %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to Outer-Suburban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,4.5), breaks=seq(-5,4.5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerOuter <- InnerOuter + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerOuter
 #####--------------------6 
 InnerExurban<- Gamma_ARGs_results_sigP_Phyllo_InnerExurban %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-3,4), breaks=seq(-3,4,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerExurban <- InnerExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerExurban
 #####--------------------7
 InnerR.Fringe<- Gamma_ARGs_results_sigP_Phyllo_InnerR.Fringe %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-8,0), breaks=seq(-7,0,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerR.Fringe <- InnerR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerR.Fringe
 #####--------------------8
 OuterExurban<- Gamma_ARGs_results_sigP_Phyllo_OuterExurban %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from OuterSuburban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-4,3), breaks=seq(-4,3,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 OuterExurban <- OuterExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 OuterExurban
 #####--------------------9
 OuterR.Fringe<- Gamma_ARGs_results_sigP_Phyllo_OuterR.Fringe %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from OuterSuburban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-6,0), breaks=seq(-6,0,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 OuterR.Fringe <- OuterR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 OuterR.Fringe
 #####--------------------10
 ExurbanR.Fringe<- Gamma_ARGs_results_sigP_Phyllo_ExurbanR.Fringe %>% 
   ggplot( aes(x = ARGs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#556B2F", "decrease" = "#C41E3A")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Exurban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold.change",limits=c(-5,0), breaks=seq(-5,0,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 ExurbanR.Fringe
 ExurbanR.Fringe <- ExurbanR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 ExurbanR.Fringe
 
 library(patchwork)# to compile the plots together 
 ARGscomparisons_Phyllo<-(CoreInner|CoreOuter|CoreExurban|CoreR.Fringe|InnerOuter)/(InnerExurban|InnerR.Fringe|OuterExurban|OuterR.Fringe|ExurbanR.Fringe)
 ARGscomparisons_Phyllo
 ggsave(filename = "ARGscomparisons_Phyllo_Gammafit.png", plot = ARGscomparisons_Phyllo, width = 20, height = 25, dpi = 2500, units = "cm")
 ggsave(filename = "ARGscomparisons_Phyllo_Gammafit.pdf", plot = ARGscomparisons_Phyllo, width = 20, height = 25, dpi = 2500, units = "cm")
 ########-----------------------------------------------------------------------------------------------------------------------------------------------------###########
 
 
#==================================================================================================================================================================# 
 ######-------------------------------- ANALYSIS OF VIRULANCE FACTORS GENES ALONG DIFFERENT URBAN GRADIENTS -------------------#######
#==================================================================================================================================================================# 
                                              ######--------   VFGS IN URBAN SOIL -------------------### 
 VFGs_GLAMMS_Soil111_4
 names(VFGs_GLAMMS_Soil111_4)
 VFGs_GLAMMS_Phyllo111_4
 ########--------------------------------------------------------------------------------------------###### 
 VFGs_GLAMMS_Soil111_4$sum[VFGs_GLAMMS_Soil111_4$sum ==0] <- 1.99999e-06 ##-- Replace 0 value by the lowest value more than the one any value revealed in all samples
 VFGs_GLAMMS_Soil111_4$Block<-as.factor(VFGs_GLAMMS_Soil111_4$Block)
 VFGs_GLAMMS_Soil111_4$Block<-factor(VFGs_GLAMMS_Soil111_4$Block, levels=c("A","B","C","D","E"))
 str(VFGs_GLAMMS_Soil111_4)
 
 library(multcomp)
 ############################################################################################################################ 
 #########----------------- GAMMAS ANALYSIS IN SOIL ---------------------------------#####
 VFGs_GLAMMS_Soil111_4<-droplevels(VFGs_GLAMMS_Soil111_4)
 levels(VFGs_GLAMMS_Soil111_4$Block)
 str(VFGs_GLAMMS_Soil111_4)
 names(VFGs_GLAMMS_Soil111_4)
 library(multcomp)
 
 JackIsTheBest_Soil <- function(VGFx) {
   TEMP <- subset(VFGs_GLAMMS_Soil111_4, VGFs == VGFx)
   
   # Create the gamma fit model
   gammafit <- tryCatch(
     {
       glm(sum ~ Block, data = TEMP, family = Gamma(link = "log"), maxit = 10000)
     },
     error = function(e) {
       message("Error creating gammafit model: ", e$message)
       return(NULL)
     }
   )
   
   # Check if gammafit object was created successfully
   if (!is.null(gammafit)) {
     # Create the multiple comparisons object
     mcp_obj <- mcp(Block = "Tukey")
     
     glht.gamma <- glht(gammafit, mcp_obj)
     gamma_glht_df <- as.data.frame(summary(glht(glht.gamma))$test[-(1:2)])
     return(gamma_glht_df)
   } else {
     message("Unable to create gammafit model for VFGs: ", VGFx)
     return(NULL)
   }
 }
 
 JackIsTheBest_Soil("phoQ") 
 
 gamma_tmpVGFs_Soil <- unique(VFGs_GLAMMS_Soil111_4$VGFs)
 gamma_tmpVGFs_Soil_test <- lapply(gamma_tmpVGFs_Soil, JackIsTheBest_Soil)
 #There were 50 or more warnings (use warnings() to see the first 50)
 # IT WORKS!!!!! So Spring water was my problem!!
 head(gamma_tmpVGFs_Soil_test)
 names(gamma_tmpVGFs_Soil_test)
 names(gamma_tmpVGFs_Soil_test) <- gamma_tmpVGFs_Soil
 head(gamma_tmpVGFs_Soil_test)
 gamma_tmpVGFs_Soil_test.m<- melt(gamma_tmpVGFs_Soil_test)
 head(gamma_tmpVGFs_Soil_test.m)
 dim(gamma_tmpVGFs_Soil_test.m)
 #[1] 3280    4
 
 levels(gamma_tmpVGFs_Soil_test.m$variable)
 coefDF<-subset(gamma_tmpVGFs_Soil_test.m, variable=="coefficients")
 stderDF<-subset(gamma_tmpVGFs_Soil_test.m, variable=="sigma")
 tstatDF<-subset(gamma_tmpVGFs_Soil_test.m, variable=="tstat")
 pvalDF<-subset(gamma_tmpVGFs_Soil_test.m, variable=="pvalues")
 coefDF<-coefDF[,-c(1,2)]
 colnames(coefDF)<-c("coefficients","VFGs_c")
 stderDF<-stderDF[,-c(1,2)]
 colnames(stderDF)<-c("std_error","VFGs_s")
 tstatDF<-tstatDF[,-c(1,2)]
 colnames(tstatDF)<-c("tstat","VFGs_t")
 pvalDF<-pvalDF[,-c(1,2)]
 colnames(pvalDF)<-c("p-value","VFGs_p")
 
 Gamma_VGFs_results_Soil<-cbind(coefDF,stderDF,tstatDF,pvalDF)
 
 #I need the comparisons LIST
 df_VFGscomp_gamma_Soil<-as.data.frame(lapply(gamma_tmpVGFs_Soil, JackIsTheBest_Soil))
 getVFGscomparisons_Soil<-rownames(df_VFGscomp_gamma_Soil)
 
 getVFGscomparisons_Soil
 
 #CHECKING IF MY COMPARISONS MEET MATCH WITH THE COMPARISONS LIST 2,150/215=10##
 dim(Gamma_VGFs_results_Soil)
 length(unique(VFGs_GLAMMS_Soil111_4$VGFs))
 820 /82
 
 ################## ======AADDINNG THE COMPARISONS COLUMNS ON MY RESULTS ============================######## 

 #I will add a column that has the comparison
 Gamma_VGFs_results_Soil$comparison<-rep(getVFGscomparisons_Soil,82)
 #And I want to remove columns that were used for checking the data
 Gamma_VGFs_results_Soil$VGFs<-Gamma_VGFs_results_Soil$VFGs_c
 Gamma_VGFs_results_Soil<-Gamma_VGFs_results_Soil[,-c(2,4,6,8)]
 #Renaming columns
 colnames(Gamma_VGFs_results_Soil)<-c("Delta.Estimate", "Std.error", "z-value", "p.value","Comparison", "VGFs")
 #And finally the rownames
 rownames(Gamma_VGFs_results_Soil)<-interaction(Gamma_VGFs_results_Soil$Comparison,Gamma_VGFs_results_Soil$VGFs, sep="_")
 head(rownames(Gamma_VGFs_results_Soil))
 
 ###############------------------- IDENTIFYING THE VGFs which are statistically significance------------------------------------###
 Gamma_VGFs_results_Soil_sigP<-subset(Gamma_VGFs_results_Soil, p.value <= 0.1)
 #sig.PC_NC$p.adjFDR<-p.adjust(sig.PC_NC$p.value,method="BH")
 Gamma_VGFs_results_Soil_sigP$p.adj<-p.adjust(Gamma_VGFs_results_Soil_sigP$p.value, method="BH")
 
 #Gamma_VGFs_results_Soil_sigP$p.adj<-p.adjust(Gamma_VGFs_results_Soil_sigP$p.value, method="bonferroni")
 
 Gamma_VGFs_results_Soil_sigP.a<-subset(Gamma_VGFs_results_Soil_sigP, p.adj <= 0.1)
 dim(Gamma_VGFs_results_Soil_sigP.a)
 dim(Gamma_VGFs_results_Soil_sigP)
 ########------------------------------CALCULATE THE UNIQUE VGFs IN EACH Soil samples -----------------------------######
 
 unique(Gamma_VGFs_results_Soil_sigP.a$VGFs)
 
 #######------------- CHECKING THE FOLD CHANGE EFFICIENCY ---------------------------------####
 
 # I can calculate the fold changes:
 Gamma_VGFs_results_Soil_sigP.a$Fold.Change<-exp(Gamma_VGFs_results_Soil_sigP.a$Delta.Estimate)
 
 # About the fold changes:
 JackIsTheBest_Soil("phoQ")
 
 phoQglm<-subset(VFGs_GLAMMS_Soil111_4, VGFs=="phoQ")
 phoQglmgamma1<-glm(sum ~ Block, data=phoQglm, family=Gamma (link="log"))
 summary(phoQglmgamma1)
 ######============= MAKE REVISION TO SEE HOW MY RESULTS WILL BE LOOK LIKE
 mean(with (VFGs_GLAMMS_Soil111_4, subset(sum, VGFs=="phoQ" & Block=="B")))
 #[1] 0.00272675
 mean(with (VFGs_GLAMMS_Soil111_4, subset(sum, VGFs=="phoQ" & Block=="A")))
 # to understand what is going on, I will do some calculations with the data: 
 mean(with (VFGs_GLAMMS_Soil111_4, subset(sum, VGFs=="pefA" & Block=="B")))
 #[1] 2.564969e-06
 mean(with (VFGs_GLAMMS_Soil111_4, subset(sum, VGFs=="entB" & Block=="A")))
 
 # to understand what is going on, I will do some calculations with the data: 
 mean(with (VFGs_GLAMMS_Soil111_4, subset(sum, VGFs=="espF" & Block=="B")))
 #[1] 6.913914e-05
 mean(with (VFGs_GLAMMS_Soil111_4, subset(sum, VGFs=="espF" & Block=="A")))
 #[1] 8.23952e-05 
 
 # Also the standard error needs to be transformed back
 Gamma_VGFs_results_Soil_sigP.a$expStd.error<-exp(Gamma_VGFs_results_Soil_sigP.a$Std.error)
 
 # log 2 transformation is handy for plotting
 
 Gamma_VGFs_results_Soil_sigP.a$log2Fold.Change<-log2(Gamma_VGFs_results_Soil_sigP.a$Fold.Change)
 
 Gamma_VGFs_results_Soil_sigP.a$log2expStd.e<-log2(Gamma_VGFs_results_Soil_sigP.a$expStd.error)
 
 Gamma_VGFs_results_Soil_sigP.a[["change"]] = ifelse(Gamma_VGFs_results_Soil_sigP.a[["log2Fold.Change"]] < 0, "decrease", "increase")
 
 ####---- NOW WE PLOTS THE RESULTS, WE NEED TO GET FIRST OF AALL THE GENES THAT HAD SIGNIFICANT DIFFERENCES IN EACH OF 10 COMPARISONS
 # I want to plot the results  
 # I will need to get the genes that had significant differences in each 
 # comparison.
 
 str(Gamma_VGFs_results_Soil_sigP.a)
 getVFGscomparisons_Soil
 Gamma_VGFs_results_sigP_Soil_CoreInner<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="B - A")
 Gamma_VGFs_results_sigP_Soil_CoreInner<-droplevels(Gamma_VGFs_results_sigP_Soil_CoreInner)
 length(unique(Gamma_VGFs_results_sigP_Soil_CoreInner$VGFs))
 #[1] 19
 Gamma_VGFs_results_sigP_Soil_CoreOuter<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="C - A")
 Gamma_VGFs_results_sigP_Soil_CoreOuter<-droplevels(Gamma_VGFs_results_sigP_Soil_CoreOuter)
 length(unique(Gamma_VGFs_results_sigP_Soil_CoreOuter$VGFs))
 #[1] 27
 
 Gamma_VGFs_results_sigP_Soil_CoreExurban<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="D - A")
 Gamma_VGFs_results_sigP_Soil_CoreExurban<-droplevels(Gamma_VGFs_results_sigP_Soil_CoreExurban)
 length(unique(Gamma_VGFs_results_sigP_Soil_CoreExurban$VGFs))
 #[1] 16
 
 Gamma_VGFs_results_sigP_Soil_CoreR.Fringe<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="E - A")
 Gamma_VGFs_results_sigP_Soil_CoreR.Fringe<-droplevels(Gamma_VGFs_results_sigP_Soil_CoreR.Fringe)
 length(unique(Gamma_VGFs_results_sigP_Soil_CoreR.Fringe$VGFs))
 #[1] 47
 
 Gamma_VGFs_results_sigP_Soil_InnerOuter<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="C - B")
 Gamma_VGFs_results_sigP_Soil_InnerOuter<-droplevels(Gamma_VGFs_results_sigP_Soil_InnerOuter)
 length(unique(Gamma_VGFs_results_sigP_Soil_InnerOuter$VGFs))
 #[1] 10
 
 Gamma_VGFs_results_sigP_Soil_InnerExurban<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="D - B")
 Gamma_VGFs_results_sigP_Soil_InnerExurban<-droplevels(Gamma_VGFs_results_sigP_Soil_InnerExurban)
 length(unique(Gamma_VGFs_results_sigP_Soil_InnerExurban$VGFs))
 #[1] 14
 
 Gamma_VGFs_results_sigP_Soil_InnerR.Fringe<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="E - B")
 Gamma_VGFs_results_sigP_Soil_InnerR.Fringe<-droplevels(Gamma_VGFs_results_sigP_Soil_InnerR.Fringe)
 length(unique(Gamma_VGFs_results_sigP_Soil_InnerR.Fringe$VGFs))
 #[1] 20 
 
 Gamma_VGFs_results_sigP_Soil_OuterExurban<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="D - C")
 Gamma_VGFs_results_sigP_Soil_OuterExurban<-droplevels(Gamma_VGFs_results_sigP_Soil_OuterExurban)
 length(unique(Gamma_VGFs_results_sigP_Soil_OuterExurban$VGFs))
 #[1] 9 
 
 Gamma_VGFs_results_sigP_Soil_OuterR.Fringe<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="E - C")
 Gamma_VGFs_results_sigP_Soil_OuterR.Fringe<-droplevels(Gamma_VGFs_results_sigP_Soil_OuterR.Fringe)
 length(unique(Gamma_VGFs_results_sigP_Soil_OuterR.Fringe$VGFs))
 #[1] 21
 
 Gamma_VGFs_results_sigP_Soil_ExurbanR.Fringe<-subset(Gamma_VGFs_results_Soil_sigP.a, Comparison=="E - D")
 Gamma_VGFs_results_sigP_Soil_ExurbanR.Fringe<-droplevels(Gamma_VGFs_results_sigP_Soil_ExurbanR.Fringe)
 length(unique(Gamma_VGFs_results_sigP_Soil_ExurbanR.Fringe$VGFs))
 #[1] 29
 
 
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change >0))
 #[1] 6 12
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change <0))
 #[1] 13  12
 log2(1)
 log(2)
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change >2))
 #[1] 6 12
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change < -2))
 #[1] 13  12
 
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change >4))
 #[1] 6 12
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change < -4))
 #[1] 13  12
 
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change < -9.965784))
 #[1] 19 12
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change > 9.965784))
 #[1] 19 12
 
 log2(50)
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change < -5.643856))
 #[1]  9 11
 dim(subset(Gamma_VGFs_results_sigP_Soil_CoreInner, log2Fold.Change > 5.643856))
 
 #####--------------------1
 CoreInner<- Gamma_VGFs_results_sigP_Soil_CoreInner %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Inner-Urban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-6,4), breaks=seq(-6,4,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreInner <- CoreInner + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreInner
 #####--------------------2 
 CoreOuter<- Gamma_VGFs_results_sigP_Soil_CoreOuter %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Outer-Surburban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-3,3), breaks=seq(-3,3,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreOuter <- CoreOuter + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreOuter
 #####--------------------3 
 CoreExurban<- Gamma_VGFs_results_sigP_Soil_CoreExurban %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-3,3), breaks=seq(-3,3,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreExurban <- CoreExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreExurban
 #####--------------------4 
 CoreR.Fringe<- Gamma_VGFs_results_sigP_Soil_CoreR.Fringe %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,4.5), breaks=seq(-5,4.5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreR.Fringe <- CoreR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreR.Fringe
 #####--------------------5 
 InnerOuter<- Gamma_VGFs_results_sigP_Soil_InnerOuter %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to Outer-Suburban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-2,2), breaks=seq(-2,2,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerOuter <- InnerOuter + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerOuter
 #####--------------------6 
 InnerExurban<- Gamma_VGFs_results_sigP_Soil_InnerExurban %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-0,3), breaks=seq(-0,3,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerExurban <- InnerExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerExurban
 #####--------------------7
 InnerR.Fringe<- Gamma_VGFs_results_sigP_Soil_InnerR.Fringe %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-0,3), breaks=seq(-0,3,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerR.Fringe <- InnerR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerR.Fringe
 #####--------------------8
 OuterExurban<- Gamma_VGFs_results_sigP_Soil_OuterExurban %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from OuterSuburban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-0,2.5), breaks=seq(-0,2.5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 OuterExurban <- OuterExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 OuterExurban
 #####--------------------9
 OuterR.Fringe<- Gamma_VGFs_results_sigP_Soil_OuterR.Fringe %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from OuterSuburban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-2.5,5), breaks=seq(-2.5,5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 OuterR.Fringe <- OuterR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 OuterR.Fringe
 #####--------------------10
 ExurbanR.Fringe<- Gamma_VGFs_results_sigP_Soil_ExurbanR.Fringe %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#A9A9A9", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Exurban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold.change",limits=c(-4,4), breaks=seq(-4,4,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 ExurbanR.Fringe
 ExurbanR.Fringe <- ExurbanR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 ExurbanR.Fringe
 
 library(patchwork)# to compile the plots together 
 VGFscomparisons_Soil<-(CoreInner|CoreOuter|CoreExurban|CoreR.Fringe|InnerOuter)/(InnerExurban|InnerR.Fringe|OuterExurban|OuterR.Fringe|ExurbanR.Fringe)
 VGFscomparisons_Soil
 ggsave(filename = "VGFscomparisons_Soil_Gammafit.png", plot = VGFscomparisons_Soil, width = 20, height = 25, dpi = 2500, units = "cm")
 ggsave(filename = "VGFscomparisons_Soil_Gammafit.pdf", plot = VGFscomparisons_Soil, width = 20, height = 25, dpi = 2500, units = "cm")
 ########-----------------------------------------------------------------------------------------------------------------------------------------------------###########
 
 
 
 ######--------   VFGS IN URBAN SOIL -------------------### 
 VFGs_GLAMMS_Phyllo111_4
 names(VFGs_GLAMMS_Phyllo111_4)
 
 ########--------------------------------------------------------------------------------------------###### 
 VFGs_GLAMMS_Phyllo111_4$sum[VFGs_GLAMMS_Phyllo111_4$sum ==0] <- 1.99999e-06 ##-- Replace 0 value by the lowest value more than the one any value revealed in all samples
 VFGs_GLAMMS_Phyllo111_4$Block<-as.factor(VFGs_GLAMMS_Phyllo111_4$Block)
 VFGs_GLAMMS_Phyllo111_4$Block<-factor(VFGs_GLAMMS_Phyllo111_4$Block, levels=c("A","B","C","D","E"))
 str(VFGs_GLAMMS_Phyllo111_4)
 
 
 ############################################################################################################################ 
 #########----------------- GAMMAS ANALYSIS IN Phyllo ---------------------------------#####
 VFGs_GLAMMS_Phyllo111_4<-droplevels(VFGs_GLAMMS_Phyllo111_4)
 levels(VFGs_GLAMMS_Phyllo111_4$Block)
 str(VFGs_GLAMMS_Phyllo111_4)
 names(VFGs_GLAMMS_Phyllo111_4)
 library(multcomp)
 
 JackIsTheBest_Phyllo <- function(VGFx) {
   TEMP <- subset(VFGs_GLAMMS_Phyllo111_4, VGFs == VGFx)
   
   # Create the gamma fit model
   gammafit <- tryCatch(
     {
       glm(sum ~ Block, data = TEMP, family = Gamma(link = "log"), maxit = 10000)
     },
     error = function(e) {
       message("Error creating gammafit model: ", e$message)
       return(NULL)
     }
   )
   
   # Check if gammafit object was created successfully
   if (!is.null(gammafit)) {
     # Create the multiple comparisons object
     mcp_obj <- mcp(Block = "Tukey")
     
     glht.gamma <- glht(gammafit, mcp_obj)
     gamma_glht_df <- as.data.frame(summary(glht(glht.gamma))$test[-(1:2)])
     return(gamma_glht_df)
   } else {
     message("Unable to create gammafit model for VFGs: ", VGFx)
     return(NULL)
   }
 }
 
 JackIsTheBest_Phyllo("phoQ") 
 
 gamma_tmpVGFs_Phyllo <- unique(VFGs_GLAMMS_Phyllo111_4$VGFs)
 gamma_tmpVGFs_Phyllo_test <- lapply(gamma_tmpVGFs_Phyllo, JackIsTheBest_Phyllo)
 #There were 50 or more warnings (use warnings() to see the first 50)
 # IT WORKS!!!!! So Spring water was my problem!!
 head(gamma_tmpVGFs_Phyllo_test)
 names(gamma_tmpVGFs_Phyllo_test)
 names(gamma_tmpVGFs_Phyllo_test) <- gamma_tmpVGFs_Phyllo
 head(gamma_tmpVGFs_Phyllo_test)
 gamma_tmpVGFs_Phyllo_test.m<- melt(gamma_tmpVGFs_Phyllo_test)
 head(gamma_tmpVGFs_Phyllo_test.m)
 dim(gamma_tmpVGFs_Phyllo_test.m)
 #[1] 3240    4
 
 levels(gamma_tmpVGFs_Phyllo_test.m$variable)
 coefDF<-subset(gamma_tmpVGFs_Phyllo_test.m, variable=="coefficients")
 stderDF<-subset(gamma_tmpVGFs_Phyllo_test.m, variable=="sigma")
 tstatDF<-subset(gamma_tmpVGFs_Phyllo_test.m, variable=="tstat")
 pvalDF<-subset(gamma_tmpVGFs_Phyllo_test.m, variable=="pvalues")
 coefDF<-coefDF[,-c(1,2)]
 colnames(coefDF)<-c("coefficients","VFGs_c")
 stderDF<-stderDF[,-c(1,2)]
 colnames(stderDF)<-c("std_error","VFGs_s")
 tstatDF<-tstatDF[,-c(1,2)]
 colnames(tstatDF)<-c("tstat","VFGs_t")
 pvalDF<-pvalDF[,-c(1,2)]
 colnames(pvalDF)<-c("p-value","VFGs_p")
 
 Gamma_VGFs_results_Phyllo<-cbind(coefDF,stderDF,tstatDF,pvalDF)
 
 #I need the comparisons LIST
 df_VFGscomp_gamma_Phyllo<-as.data.frame(lapply(gamma_tmpVGFs_Phyllo, JackIsTheBest_Phyllo))
 getVFGscomparisons_Phyllo<-rownames(df_VFGscomp_gamma_Phyllo)
 
 getVFGscomparisons_Phyllo
 
 #CHECKING IF MY COMPARISONS MEET MATCH WITH THE COMPARISONS LIST 2,150/215=10##
 dim(Gamma_VGFs_results_Phyllo)
 length(unique(VFGs_GLAMMS_Phyllo111_4$VGFs))
 810 /81
 
 ################## ======AADDINNG THE COMPARISONS COLUMNS ON MY RESULTS ============================######## 
 
 #I will add a column that has the comparison
 Gamma_VGFs_results_Phyllo$comparison<-rep(getVFGscomparisons_Phyllo,81)
 #And I want to remove columns that were used for checking the data
 Gamma_VGFs_results_Phyllo$VGFs<-Gamma_VGFs_results_Phyllo$VFGs_c
 Gamma_VGFs_results_Phyllo<-Gamma_VGFs_results_Phyllo[,-c(2,4,6,8)]
 #Renaming columns
 colnames(Gamma_VGFs_results_Phyllo)<-c("Delta.Estimate", "Std.error", "z-value", "p.value","Comparison", "VGFs")
 #And finally the rownames
 rownames(Gamma_VGFs_results_Phyllo)<-interaction(Gamma_VGFs_results_Phyllo$Comparison,Gamma_VGFs_results_Phyllo$VGFs, sep="_")
 head(rownames(Gamma_VGFs_results_Phyllo))
 
 ###############------------------- IDENTIFYING THE VGFs which are statistically significance------------------------------------###
 Gamma_VGFs_results_Phyllo_sigP<-subset(Gamma_VGFs_results_Phyllo, p.value <= 0.1)
 #sig.PC_NC$p.adjFDR<-p.adjust(sig.PC_NC$p.value,method="BH")
 Gamma_VGFs_results_Phyllo_sigP$p.adj<-p.adjust(Gamma_VGFs_results_Phyllo_sigP$p.value, method="BH")
 
 #Gamma_VGFs_results_Phyllo_sigP$p.adj<-p.adjust(Gamma_VGFs_results_Phyllo_sigP$p.value, method="bonferroni")
 
 Gamma_VGFs_results_Phyllo_sigP.a<-subset(Gamma_VGFs_results_Phyllo_sigP, p.adj <= 0.1)
 dim(Gamma_VGFs_results_Phyllo_sigP.a)
 dim(Gamma_VGFs_results_Phyllo_sigP)
 ########------------------------------CALCULATE THE UNIQUE VGFs IN EACH Phyllo samples -----------------------------######
 
 unique(Gamma_VGFs_results_Phyllo_sigP.a$VGFs)
 
 #######------------- CHECKING THE FOLD CHANGE EFFICIENCY ---------------------------------####
 
 # I can calculate the fold changes:
 Gamma_VGFs_results_Phyllo_sigP.a$Fold.Change<-exp(Gamma_VGFs_results_Phyllo_sigP.a$Delta.Estimate)
 
 # About the fold changes:
 JackIsTheBest_Phyllo("phoQ")
 
 phoQglm<-subset(VFGs_GLAMMS_Phyllo111_4, VGFs=="phoQ")
 phoQglmgamma1<-glm(sum ~ Block, data=phoQglm, family=Gamma (link="log"))
 summary(phoQglmgamma1)
 ######============= MAKE REVISION TO SEE HOW MY RESULTS WILL BE LOOK LIKE
 mean(with (VFGs_GLAMMS_Phyllo111_4, subset(sum, VGFs=="phoQ" & Block=="B")))
 
 mean(with (VFGs_GLAMMS_Phyllo111_4, subset(sum, VGFs=="phoQ" & Block=="A")))
 # to understand what is going on, I will do some calculations with the data: 
 mean(with (VFGs_GLAMMS_Phyllo111_4, subset(sum, VGFs=="pefA" & Block=="B")))
 
 mean(with (VFGs_GLAMMS_Phyllo111_4, subset(sum, VGFs=="entB" & Block=="A")))
 
 # to understand what is going on, I will do some calculations with the data: 
 mean(with (VFGs_GLAMMS_Phyllo111_4, subset(sum, VGFs=="espF" & Block=="B")))
 
 mean(with (VFGs_GLAMMS_Phyllo111_4, subset(sum, VGFs=="espF" & Block=="A")))
 
 
 # Also the standard error needs to be transformed back
 Gamma_VGFs_results_Phyllo_sigP.a$expStd.error<-exp(Gamma_VGFs_results_Phyllo_sigP.a$Std.error)
 
 # log 2 transformation is handy for plotting
 
 Gamma_VGFs_results_Phyllo_sigP.a$log2Fold.Change<-log2(Gamma_VGFs_results_Phyllo_sigP.a$Fold.Change)
 
 Gamma_VGFs_results_Phyllo_sigP.a$log2expStd.e<-log2(Gamma_VGFs_results_Phyllo_sigP.a$expStd.error)
 
 Gamma_VGFs_results_Phyllo_sigP.a[["change"]] = ifelse(Gamma_VGFs_results_Phyllo_sigP.a[["log2Fold.Change"]] < 0, "decrease", "increase")
 
 ####---- NOW WE PLOTS THE RESULTS, WE NEED TO GET FIRST OF AALL THE GENES THAT HAD SIGNIFICANT DIFFERENCES IN EACH OF 10 COMPARISONS
 # I want to plot the results  
 # I will need to get the genes that had significant differences in each 
 # comparison.
 
 str(Gamma_VGFs_results_Phyllo_sigP.a)
 getVFGscomparisons_Phyllo
 Gamma_VGFs_results_sigP_Phyllo_CoreInner<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="B - A")
 Gamma_VGFs_results_sigP_Phyllo_CoreInner<-droplevels(Gamma_VGFs_results_sigP_Phyllo_CoreInner)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_CoreInner$VGFs))
 #[1] 10
 Gamma_VGFs_results_sigP_Phyllo_CoreOuter<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="C - A")
 Gamma_VGFs_results_sigP_Phyllo_CoreOuter<-droplevels(Gamma_VGFs_results_sigP_Phyllo_CoreOuter)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_CoreOuter$VGFs))
 #[1] 20
 
 Gamma_VGFs_results_sigP_Phyllo_CoreExurban<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="D - A")
 Gamma_VGFs_results_sigP_Phyllo_CoreExurban<-droplevels(Gamma_VGFs_results_sigP_Phyllo_CoreExurban)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_CoreExurban$VGFs))
 #[1] 10
 
 Gamma_VGFs_results_sigP_Phyllo_CoreR.Fringe<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="E - A")
 Gamma_VGFs_results_sigP_Phyllo_CoreR.Fringe<-droplevels(Gamma_VGFs_results_sigP_Phyllo_CoreR.Fringe)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_CoreR.Fringe$VGFs))
 #[1] 18
 
 Gamma_VGFs_results_sigP_Phyllo_InnerOuter<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="C - B")
 Gamma_VGFs_results_sigP_Phyllo_InnerOuter<-droplevels(Gamma_VGFs_results_sigP_Phyllo_InnerOuter)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_InnerOuter$VGFs))
 #[1] 14
 
 Gamma_VGFs_results_sigP_Phyllo_InnerExurban<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="D - B")
 Gamma_VGFs_results_sigP_Phyllo_InnerExurban<-droplevels(Gamma_VGFs_results_sigP_Phyllo_InnerExurban)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_InnerExurban$VGFs))
 #[1] 11
 
 Gamma_VGFs_results_sigP_Phyllo_InnerR.Fringe<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="E - B")
 Gamma_VGFs_results_sigP_Phyllo_InnerR.Fringe<-droplevels(Gamma_VGFs_results_sigP_Phyllo_InnerR.Fringe)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_InnerR.Fringe$VGFs))
 #[1] 19 
 
 Gamma_VGFs_results_sigP_Phyllo_OuterExurban<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="D - C")
 Gamma_VGFs_results_sigP_Phyllo_OuterExurban<-droplevels(Gamma_VGFs_results_sigP_Phyllo_OuterExurban)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_OuterExurban$VGFs))
 #[1] 8 
 
 Gamma_VGFs_results_sigP_Phyllo_OuterR.Fringe<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="E - C")
 Gamma_VGFs_results_sigP_Phyllo_OuterR.Fringe<-droplevels(Gamma_VGFs_results_sigP_Phyllo_OuterR.Fringe)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_OuterR.Fringe$VGFs))
 #[1] 23
 
 Gamma_VGFs_results_sigP_Phyllo_ExurbanR.Fringe<-subset(Gamma_VGFs_results_Phyllo_sigP.a, Comparison=="E - D")
 Gamma_VGFs_results_sigP_Phyllo_ExurbanR.Fringe<-droplevels(Gamma_VGFs_results_sigP_Phyllo_ExurbanR.Fringe)
 length(unique(Gamma_VGFs_results_sigP_Phyllo_ExurbanR.Fringe$VGFs))
 #[1] 18
 
 
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change >0))
 #[1] 6 12
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change <0))
 #[1] 13  12
 log2(1)
 log(2)
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change >2))
 #[1] 6 12
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change < -2))
 #[1] 13  12
 
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change >4))
 #[1] 6 12
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change < -4))
 #[1] 13  12
 
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change < -9.965784))
 #[1] 19 12
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change > 9.965784))
 #[1] 19 12
 
 log2(50)
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change < -5.643856))
 #[1]  9 11
 dim(subset(Gamma_VGFs_results_sigP_Phyllo_CoreInner, log2Fold.Change > 5.643856))
 
 #####--------------------1
 CoreInner<- Gamma_VGFs_results_sigP_Phyllo_CoreInner %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Inner-Urban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-5,7), breaks=seq(-5,7,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreInner <- CoreInner + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreInner
 #####--------------------2 
 CoreOuter<- Gamma_VGFs_results_sigP_Phyllo_CoreOuter %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Outer-Surburban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-8,8), breaks=seq(-8,8,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreOuter <- CoreOuter + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreOuter
 #####--------------------3 
 CoreExurban<- Gamma_VGFs_results_sigP_Phyllo_CoreExurban %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-7,7), breaks=seq(-7,7,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreExurban <- CoreExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreExurban
 #####--------------------4 
 CoreR.Fringe<- Gamma_VGFs_results_sigP_Phyllo_CoreR.Fringe %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Core-Urban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-8,9.5), breaks=seq(-8,9.5,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 CoreR.Fringe <- CoreR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 CoreR.Fringe
 #####--------------------5 
 InnerOuter<- Gamma_VGFs_results_sigP_Phyllo_InnerOuter %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to Outer-Suburban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-7,6), breaks=seq(-7,6,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerOuter <- InnerOuter + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerOuter
 #####--------------------6 
 InnerExurban<- Gamma_VGFs_results_sigP_Phyllo_InnerExurban %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-7,7), breaks=seq(-7,7,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerExurban <- InnerExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerExurban
 #####--------------------7
 InnerR.Fringe<- Gamma_VGFs_results_sigP_Phyllo_InnerR.Fringe %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Inner-Urban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-9,8), breaks=seq(-9,8,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 InnerR.Fringe <- InnerR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 InnerR.Fringe
 #####--------------------8
 OuterExurban<- Gamma_VGFs_results_sigP_Phyllo_OuterExurban %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from OuterSuburban to Exurban \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-9,9), breaks=seq(-9,9,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 OuterExurban <- OuterExurban + theme(plot.title = element_text(size = 6, face = "bold"))
 OuterExurban
 #####--------------------9
 OuterR.Fringe<- Gamma_VGFs_results_sigP_Phyllo_OuterR.Fringe %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from OuterSuburban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-9,9), breaks=seq(-9,9,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 OuterR.Fringe <- OuterR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 OuterR.Fringe
 #####--------------------10
 ExurbanR.Fringe<- Gamma_VGFs_results_sigP_Phyllo_ExurbanR.Fringe %>% 
   ggplot( aes(x = VGFs, y = log2Fold.Change, fill = change)) + 
   geom_bar(stat="identity",width=.6) +
   scale_fill_manual(values = c("increase" = "#4F7942", "decrease" = "#800000")) +
   theme_bw(base_family="Helvetica")+
   geom_errorbar(aes(ymin=log2Fold.Change-log2expStd.e, ymax=log2Fold.Change+log2expStd.e), width=.05,
                 position=position_dodge(.9)) +
   theme(axis.text.y=element_text(colour= "black",size=6))+
   theme(axis.text.x = element_text(colour= "black", hjust=0.5, vjust=0, size=6))+
   theme(aspect.ratio = 2/1)+
   ggtitle("Fold change from Exurban to U.Rural-Fringe \n> 10 fold difference (p<0.05)")+
   scale_y_continuous(name="log2fold",limits=c(-8,8), breaks=seq(-8,8,1))+
   coord_flip()+
   theme(legend.position = "bottom", 
         legend.title = element_blank(),
         legend.text = element_text (size = 6))
 ExurbanR.Fringe
 ExurbanR.Fringe <- ExurbanR.Fringe + theme(plot.title = element_text(size = 6, face = "bold"))
 ExurbanR.Fringe
 
 library(patchwork)# to compile the plots together 
 VGFscomparisons_Phyllo<-(CoreInner|CoreOuter|CoreExurban|CoreR.Fringe|InnerOuter)/(InnerExurban|InnerR.Fringe|OuterExurban|OuterR.Fringe|ExurbanR.Fringe)
 VGFscomparisons_Phyllo
 ggsave(filename = "VGFscomparisons_Phyllo_Gammafit.png", plot = VGFscomparisons_Phyllo, width = 20, height = 25, dpi = 2500, units = "cm")
 ggsave(filename = "VGFscomparisons_Phyllo_Gammafit.pdf", plot = VGFscomparisons_Phyllo, width = 20, height = 25, dpi = 2500, units = "cm")
 ########-----------------------------------------------------------------------------------------------------------------------------------------------------###########
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 