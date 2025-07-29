#upload libraries
library(tidyverse,ggplot2)
library(lubridate)
library (readxl)
library (dplyr)

library(forcats)
library(cowplot)
library(ggpubr)

library(dplyr)
library(tidyr)
library(ggpubr)
library(rlang)

# install.packages("gridGraphics") # required
# install.packages("ggpubr")

#save files here
filepath <- "~/PFAS/Plotting/"

#data that we are using
dataFish <- read_excel(paste(filepath, "Fish_UK_SP", ".xlsx", sep=""))

# Sum of some original columns
dataFish$Total <- dataFish$PFOA + dataFish$PFNA + dataFish$PFDcA + dataFish$PFUnA + dataFish$PFDoDA + dataFish$PFOS + dataFish$PFOSA + dataFish$`6:2FTS`
  

british_fish <- dataFish %>%filter(grepl("United Kingdom", Country))
spanish_fish <- dataFish %>%filter(!grepl("United Kingdom", Country))


# non-pretty FPOA graph 
ggplot(british_fish, aes(Fish_lenght_cm, PFOA)) + geom_point() + geom_smooth(method="lm")
ggplot(british_fish, aes(Fish_lenght_cm, PFNA)) + geom_point() + geom_smooth(method="lm")

ggsave("Plotting/British_length.png")
ggplot(spanish_fish, aes(Fish_lenght_cm, PFOA)) + geom_point() + geom_smooth(method="lm")
ggsave("Plotting/Spanish_length.png")

ggplot(british_fish, aes(Fish_weight_Kg, PFOA)) + geom_point() + geom_smooth(method="lm")
ggsave("Plotting/British_weight.png")

ggplot(spanish_fish, aes(Fish_weight_Kg, PFOA)) + geom_point() + geom_smooth(method="lm")
ggsave("Plotting/Spanish_weight.png")

#plots all dataFish
ggplot(dataFish, aes(Fish_weight_Kg, PFDcA)) + geom_point() + geom_smooth(method="lm")
ggplot(dataFish, aes(Fish_lenght_cm, PFOA)) + geom_point() + geom_smooth(method="lm")

#together British fish PFOS
dataFish_long <- british_fish %>%
  tidyr::pivot_longer(cols = c(Fish_weight_Kg, Fish_lenght_cm), 
                      names_to = "Variable", 
                      values_to = "Value")

ggplot(dataFish_long, aes(Value, PFOS)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~Variable, scales = "free_x") +
  labs(x = "Fish measurement", y = "PFOS", title = "Relation between wegith and longitud of the fish and PFOS Concentration") +
  theme_minimal()



########PLOT WEIGHT AND LENGHT##########

# reorder data
dataFish_long <- dataFish %>%
  pivot_longer(cols = c(Fish_weight_Kg, Fish_lenght_cm), 
               names_to = "Variable", 
               values_to = "Value") %>%
  filter(PFOA > 0.029)  #Filter data according to criteria

# rename variables
dataFish_long$Variable <- recode(dataFish_long$Variable,
                                 "Fish_weight_Kg" = "Fish weight (Kg)",
                                 "Fish_lenght_cm" = "Fish length (cm)")

# Create the plot
plotweightlenght <- ggplot(dataFish_long, aes(Value, PFOA)) + 
  geom_point(color = "black", alpha = 0.6, size = 2) +  #Blue color, transparency, point size
  geom_smooth(method = "lm", color = "blue", se = TRUE, fill = "grey", linewidth = 1) +  #Red line with 95% confidence interval
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +  # AddR² and p-value
  facet_wrap(~Variable, scales = "free_x") +
  labs(
    x = "Fish measurement",
    y = "PFOA concentration (ng/g)", 
    ) +
  theme_bw() +  # improve background
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # Size and bold font in facet titles
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Center the title and make it larger
    axis.title = element_text(size = 14),  # Axis label size
    axis.text = element_text(size = 12),  # Axis tick number size
    legend.position = "none"
  )



ggplot(dataFish, aes(Fish_weight_Kg, Fish_lenght_cm)) + geom_point() + geom_smooth(method="lm") + labs(title = "Longer fish heavier", x = "Weight (Kg)", y = "Length (cm)")

ggsave("Plotting/weight_vs_length.png",
       plot = plotweightlenght,
       dpi = 1000,
       width = 6,
       height = 4)




###########BOXPLOT FISH BY COUNTRY VS PFAS###############

# Define lists of PFAS and fish species
box_pfas <- c("PFOA", "PFNA", "PFDcA", "PFUnA", "PFDoDA", "PFOS", "PFOSA", "6:2FTS", "Total")
box_fish <- c("Coley", "Dab", "Hake", "Lemon Sole", "Mackerel", "Plaice", "Sea Bass")

# Create a list of plots
plots <- list()

plots[[length(plots)+1]] <- as_ggplot(text_grob(""))

for (fish in box_fish) {
  name <- fish
  plots[[length(plots)+1]] <- as_ggplot(text_grob(name))
}

header_size <- length(plots)
ylim <- c()
ylim["PFOA"] <- 10
ylim["PFNA"] <- 20
ylim["PFDcA"] <- 4
ylim["PFUnA"] <- 6
ylim["PFDoDA"] <- 12
ylim["PFOS"] <- 50
ylim["PFOSA"] <- 6
ylim["6:2FTS"] <- 20
ylim["Total"] <- 100



for (pfas in box_pfas) {
  
  plots[[length(plots)+1]] <- as_ggplot(text_grob(ifelse(pfas=="Total", "Total PFAS", pfas)))
  

    for (fish in box_fish) {
      
    f <- dataFish %>%
      filter(Fish_specie == fish, Tissue_type == "muscle fillet")
    
    f <- f %>% mutate(current = .[[pfas]])
    
    res <- kruskal.test(current ~ Country, data = f)
    
    group1 <- dplyr::filter(f, 
                            Country == "United Kingdom")
    
    group2 <- dplyr::filter(f, 
                            Country == "Spain")
    
    res2 <- wilcox.test(group1$current,
                       group2$current,
                       alternative = "two.sided")
    
    
    ymax <- NA # ylim[pfas] # set to NA for more meaningful plot, log10 likes 
    
    p <- ggplot(f, aes(x = Country, y = (!!sym(pfas)), fill = Country)) +
      geom_boxplot(alpha = 0.6, size = 0.5, outlier.shape = NA) +
      geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
      scale_y_continuous(limits = c(0, ymax)) + # pin them all to zero lower bound
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_blank()) +
      annotate("text",
               x = 1.5,
               y = Inf, #max(f[[pfas]], na.rm = TRUE) * 0.9,
               vjust=1,
               label = sprintf("p=%.3f", res$p.value), size = 3, color = "red")
    
    plots[[length(plots) + 1]] <- p
  }
}

box_legend <- cowplot::get_plot_component( 
  plots[[length(plots)]] + theme(legend.position = "bottom") 
  + theme(legend.text = element_text(size = 12)),
  "guide-box", return_all = TRUE)[[3]]

for (i in (header_size+1):length(plots))
{
  plots[[i]] <- plots[[i]] + theme(legend.position="none")
}

plots[[length(plots)+1]] <- as_ggplot(text_grob(""))

for (fish in box_fish) {
  these_fish <- dataFish[dataFish$Fish_specie == fish,]
  sp_fish = nrow(these_fish[these_fish$Country == "Spain",])
  uk_fish = nrow(these_fish[these_fish$Country == "United Kingdom",])
  
  name <- sprintf("SP=%d UK=%d", sp_fish, uk_fish)
  thing <- text_grob(name) # works

  plots[[length(plots)+1]] <- as_ggplot(thing)
}

# Combinar todos los gráficos
final_plot <- plot_grid(plotlist = plots, 
                        rel_heights = c(0.25, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.25),
                        rel_widths = c(0.75, 1, 1, 1, 1, 1, 1, 1),
                        ncol = 1+length(box_fish),
                        align = "vh") 

title <- element_blank()

final_output <- 
  plot_grid(title, final_plot, box_legend, ncol = 1, rel_heights = c(0.05, 1, 0.05)) +
  theme(plot.background = element_rect(fill = "white"))

final_output
print(final_output)
# rstudioapi::executeCommand("refreshPlot") # actually draw it


ggsave("Plotting/box_plots.png",
       plot = final_output,
       dpi = 1000,
       width = 12,
       height = 12)





############BAR PLOT###########

# non-compound headers
want <- c("ID","Country","Year","Excel_name","Sample_date","Sample_location_EA","Lat","Long","River_basin","Sample_point_name_EA","Fish_specie","Species_code","Tissue_type","Type_of_water","Number_in_pool","Replicate","Fish_lengths_indiv","Fish_lenght_cm","Fish_weight_Kg","Grams_analysed","Sample_location_UoB","Unit_wet_weight","Wild_or_farmed","Supermarket")

pfas_order <- c("PFOA", "PFNA", "PFDcA", "PFUnA", "PFDoDA", "PFTrDA", "PFOS", "PFOSA", "6:2FTS","TFA", "TFSA")

# Discarding compounds other than these
want <- c(want, pfas_order)


spanish_selected <- spanish_fish[, want]
british_selected <- british_fish[, want]


#pivot_longer
data_muscle<-spanish_selected%>%pivot_longer(cols = 25:length(spanish_selected), values_to = "Value", names_to = "PFAS")
#unique(data_muscle$PFAS)

#Plot: muscle
muscle<-data_muscle%>%filter(Tissue_type=="muscle fillet")%>%
  group_by(Fish_specie, PFAS)%>%summarise(meanVal=mean(Value, na.rm = T))

datSum<-muscle%>%group_by(Fish_specie)%>%summarise(sumVal=sum(meanVal, na.rm = T))
print("spanish")
print(datSum)

muscle<-merge(muscle, datSum, by="Fish_specie")

#calculate relative concentration
muscle<-muscle%>%mutate(perVal=meanVal/sumVal*100)


spanish_fish_order <- c("Coley","Dab", "Hake", "Lemon Sole", "Mackerel", "Plaice", "Sardines", "Sea Bass", "Mullet")
british_fish_order <- c("Coley","Dab", "Hake", "Lemon Sole", "Mackerel", "Plaice", "Sardines", "Sea Bass", "Trout", "Whiting", "Cod", "Flounder", "Herring", "Anchovies")


#Plots muscle british

#pivot_longer
data_muscle<-british_selected%>%pivot_longer(cols = 25:length(british_selected), values_to = "Value", names_to = "PFAS")
#unique(data_muscle$PFAS)

#Plot: muscle
muscle<-data_muscle%>%filter(Tissue_type=="muscle fillet")%>%
  group_by(Fish_specie, PFAS)%>%summarise(meanVal=mean(Value, na.rm = T))

datSum<-muscle%>%group_by(Fish_specie)%>%summarise(sumVal=sum(meanVal, na.rm = T))


muscle<-merge(muscle, datSum, by="Fish_specie")

#calculate relative concentration
muscle<-muscle%>%mutate(perVal=meanVal/sumVal*100)

#Combined with mean and % composition
#1) PLOTS BRITISH MUSCLE

british_row <- muscle %>%
  ggplot(aes(x = Fish_specie, y = meanVal, fill = factor(PFAS, levels = pfas_order))) +
  geom_col(width = 0.7) +
  theme_minimal() +
  xlab("Fish species") + 
  ylab("Concentration (ng/g)") +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  scale_x_discrete(limits = british_fish_order) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", 
                               "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#F9A825")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

# Save the image with higher quality
ggsave("Plotting/muscle_british.png", plot = british_row, dpi = 300, width = 10, height = 6)


british_percentage <- muscle %>%
  ggplot(aes(x = Fish_specie, y = perVal, fill = factor(PFAS, levels = pfas_order))) +
  geom_col(width = 0.7) +
  theme_minimal() +
  xlab("Fish species") + 
  ylab("Composition (%)") +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  scale_x_discrete(limits = rev(british_fish_order)) + 
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", 
                               "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#F9A825")) +
  coord_flip()

# Save the image with higher quality
ggsave("Plotting/muscle_british_percentage.png", plot = british_percentage, dpi = 300, width = 10, height = 6)


# get_legend only works when the legend is on the right, work around
# https://github.com/wilkelab/cowplot/issues/202
# british_legend <- get_legend(british_row) # buggy

british_legend <- cowplot::get_plot_component(british_row  + theme(legend.position = "bottom"), "guide-box", return_all = TRUE)[[3]]


british_row <- british_row + theme(legend.position = "none")
british_percentage <- british_percentage + theme(legend.position="none")


prow <- plot_grid(british_row, british_percentage, nrow=1)
both <- plot_grid(prow, british_legend, nrow=2,  rel_heights = c(1, 0.25))


ggsave("Plotting/muscle_british_combined.png",
       plot = both,
       dpi = 1000,
       width = 10,
       height = 8)



#2) PLOTS MUSCLE SPANISH


#pivot_longer
data_muscle<-spanish_selected%>%pivot_longer(cols = 25:length(spanish_selected), values_to = "Value", names_to = "PFAS")
#unique(data_muscle$PFAS)

#Plot: muscle
muscle<-data_muscle%>%filter(Tissue_type=="muscle fillet")%>%
  group_by(Fish_specie, PFAS)%>%summarise(meanVal=mean(Value, na.rm = T))

datSum<-muscle%>%group_by(Fish_specie)%>%summarise(sumVal=sum(meanVal, na.rm = T))


muscle<-merge(muscle, datSum, by="Fish_specie")

#calculate relative concentration
muscle<-muscle%>%mutate(perVal=meanVal/sumVal*100)

#Combined with mean and % composition
#Spanish muscle

spanish_row <- muscle %>%
  ggplot(aes(x = Fish_specie, y = meanVal, fill = factor(PFAS, levels = pfas_order))) +
  geom_col(width = 0.7) +
  theme_minimal() +
  xlab("Fish species") + 
  ylab("Concentration (ng/g)") +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
  scale_x_discrete(limits = spanish_fish_order) +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", 
                               "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#F9A825")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

ggsave("Plotting/muscle_spanish.png", plot = spanish_row, dpi = 300, width = 10, height = 6)



spanish_percentage <- muscle %>%
  ggplot(aes(x = Fish_specie, y = perVal, fill = factor(PFAS, levels = pfas_order))) +
  geom_col(width = 0.7) +
  theme_minimal() +
  xlab("Fish species") + 
  ylab("Composition (%)") +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
  scale_x_discrete(limits = rev(spanish_fish_order)) + 
  theme(legend.title=element_blank()) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", 
                               "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF", "#F9A825")) +
  coord_flip()

# Save the image with higher quality
ggsave("Plotting/muscle_spanish_percentage.png", plot = spanish_percentage, dpi = 300, width = 10, height = 6)


# get_legend only works when the legend is on the right, work around
# https://github.com/wilkelab/cowplot/issues/202
# spanish_legend <- get_legend(spanish_row) # buggy

spanish_legend <- cowplot::get_plot_component(spanish_row  + theme(legend.position = "bottom"), "guide-box", return_all = TRUE)[[3]]


spanish_row <- spanish_row + theme(legend.position = "none")
spanish_percentage <- spanish_percentage + theme(legend.position="none")


prow <- plot_grid(spanish_row, spanish_percentage, nrow=1)
both <- plot_grid(prow, spanish_legend, nrow=2,  rel_heights = c(1, 0.25))


ggsave("Plotting/muscle_spanish_combined.png")


spanish_row <- spanish_row + theme(axis.title.x = element_blank())
british_row <- british_row + theme(axis.title.x = element_blank())
spanish_percentage <- spanish_percentage + theme(axis.title.y = element_blank())
british_percentage <- british_percentage + theme(axis.title.y = element_blank())

prow <- plot_grid(spanish_row, spanish_percentage, british_row, british_percentage,nrow=2)
both <- plot_grid(prow, spanish_legend, nrow=2,  rel_heights = c(1, 0.25))
  
both <- both + theme(plot.background = element_rect(fill = "white"))




ggsave("Plotting/muscle_britishspanish_combined.png",
       plot = both,
       dpi = 1000,
       width = 10,
       height = 8)

