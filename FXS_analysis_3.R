#Fix install.packages
trace(utils:::unpackPkgZip, edit=TRUE)
#Line 142 = 2

# Install and Load Packages
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(Hmisc)

# Import InstoIns xlsx data
df = read.xlsx("InstoIns.xlsx", colNames = TRUE)
df$QC = as.numeric(df$QC)

ggplot(df) + geom_point(aes(x=QC1, y=Geno1, color="red", shape="3"), size=3, position = position_jitter(width = 0.1)) + 
  geom_point(aes(x=QC1, y=Geno2, color = "red", shape="3"), size=3, position = position_jitter(width = 0.1)) + 
  geom_point(aes(x=QC1, y=Geno3, color = "red", shape="3"), size=3, position = position_jitter(width = 0.1)) +
  geom_point(aes(x=QC2, y=Geno4, color = "blue", shape="4"), size=3, position = position_jitter(width = 0.1)) + 
  geom_point(aes(x=QC2, y=Geno5, color = "blue", shape="4"), size=3, position = position_jitter(width = 0.1)) + 
  geom_point(aes(x=QC2, y=Geno6, color = "blue", shape="4"), size=3, position = position_jitter(width = 0.1)) +
  theme_bw() +
  scale_shape_discrete(solid=F)

# Import Mosaic xlsx data
df_Mosaic_FM = read.xlsx("FM_Mos.xlsx", colNames = TRUE, sheet=1)
df_Mosaic_FM$Percentage = as.factor(df_Mosaic_FM$Percentage)

p1 = ggplot(df_Mosaic_FM) + 
  geom_point(aes(x=Genotype1, y=RFU1, color=Percentage), size=3, position = position_jitter(width = 0.1)) + 
  theme_bw() +
  theme(legend.position="none", text = element_text(size=16)) +
  labs(x=NULL, y="RFU") +
  scale_x_discrete(limits = c(33,35))

p2 = ggplot(df_Mosaic_FM) + 
  geom_point(aes(x=Genotype2, y=RFU2, color=Percentage), size=3, position = position_jitter(width = 0.1)) +
  theme_bw() +
  theme(text = element_text(size=16)) +
  labs(x= NULL, y=NULL) +
  scale_fill_discrete(breaks = levels(df_Mosaic_FM$Percentage)) +
  scale_x_discrete(limits = c(200))
  

p3 = ggplot(df_Mosaic_FM) + 
  geom_point(aes(x=Genotype3, y=RFU3, color=Percentage), size=3, position = position_jitter(width = 0.1)) +
  theme_bw() +
  theme(text = element_text(size=12)) +
  labs(x= NULL, y=NULL) +
  scale_fill_discrete(breaks = levels(df_Mosaic_FM$Percentage)) +
  scale_x_discrete(limits = c(33,35))

grid.arrange(p1, p2, ncol=2)

# PM
df_Mosaic_PM = read.xlsx("FM_Mos.xlsx", colNames = TRUE, sheet=2)
df_Mosaic_PM$Percentage = as.factor(df_Mosaic_PM$Percentage)

p4 = ggplot(df_Mosaic_PM) + 
  geom_point(aes(x=Genotype1, y=RFU1, color=Percentage), size=3, position = position_jitter(width = 0.1)) + 
  theme_bw() +
  theme(legend.position="none", text = element_text(size=16)) +
  labs(x=NULL, y="RFU") +
  scale_x_discrete(limits = c(30))

p5 = ggplot(df_Mosaic_PM) + 
  geom_point(aes(x=Genotype2, y=RFU2, color=Percentage), size=3, position = position_jitter(width = 0.1)) + 
  theme_bw() +
  theme(legend.position="none", text = element_text(size=16)) +
  labs(x=NULL, y=NULL) +
  scale_x_discrete(limits = c(34,35))

p6 = ggplot(df_Mosaic_PM) + 
  geom_point(aes(x=Genotype3, y=RFU3, color=Percentage), size=3, position = position_jitter(width = 0.1)) +
  theme_bw() +
  theme(text = element_text(size=16)) +
  labs(x= NULL, y=NULL) +
  scale_fill_discrete(breaks = levels(df_Mosaic_PM$Percentage)) +
  scale_x_discrete(limits = c(56,57))

grid.arrange(p4, p5, p6, ncol=3)

# Population data
df_Population = read.xlsx("Population_Data_Analysis_1.xlsx", colNames = TRUE, sheet=1)

# Histogram by largest allele
ggplot(data=df_Population, aes(Largest_Allele)) + 
  geom_histogram(breaks= seq(10, 80, by=1), fill = "grey50", colour = "black") + 
  labs(x= "CGG Repeat Length", y="Frequency") + 
  scale_x_continuous(breaks=seq(10,80,5), minor_breaks = seq(1, 80, 1), expand = c(0,0)) + 
  theme_bw()

# Calculate the most abundant allele
df_RFU = select(df_Population, RFU_1:RFU_3)

z<-apply(df_RFU,1,which.max) 

df_Geno = select(df_Population, Genotype_1:Genotype_3)
df_Geno = mutate(df_Geno, z)

df_Geno$Highest = ifelse (df_Geno$z==1, df_Geno$Genotype_1, df_Geno$Genotype_2)
df_Geno$HighestRFU = ifelse (df_Geno$z==1, df_Population$RFU_1, df_Population$RFU_2)


library(ggthemes)
ggplot(df_Geno, aes(Highest, HighestRFU, color=factor(df_Population$Category))) +
  geom_point(size = 3, position = position_jitter(width = 0.5)) +
  scale_colour_brewer(palette="Set1") + 
  labs(x= "Highest CGG Repeat ", y="RFU", color="Category")
