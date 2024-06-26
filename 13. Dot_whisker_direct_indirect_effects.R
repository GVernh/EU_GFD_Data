
# Please be aware that this script deducts the total direct and indirect results from the data frame...
  # Hence lines such as line 142: "SEM_rich = SEM_rich[1:c(nrow(SEM_rich) - 2),]"
  # Remove these lines of code if total effects have not been calculated within the models

# Author: G.Vernham
# Date: 06/02/2024

libs <- c("dotwhisker", "broom","tidyverse","lavaan","effectsize","stringr","ggpubr",
          "gridExtra")

installed_libs <- libs %in% rownames(
  installed.packages())

if (any(installed_libs == F)) {
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs,
  library,
  character.only = T
))
rm(list=ls())

###########################
####### LOAD DATA #########
###########################

All_models_100 = read.csv("./Analysis/Finalised_dataset/Spatially_corrected_data/FD_coefs_100m_spatially_corrected.csv")
All_models_400 = read.csv("./Analysis/Finalised_dataset/Spatially_corrected_data/FD_coefs_400m_spatially_corrected.csv")
All_models_1000 = read.csv("./Analysis/Finalised_dataset/Spatially_corrected_data/FD_coefs_1000m_spatially_corrected.csv")

All_models_100  = rename(All_models_100, dimension = model)
All_models_400  = rename(All_models_400, dimension = model)
All_models_1000  = rename(All_models_1000, dimension = model)

All_models_100$model = "100m²"
All_models_400$model = "400m²"
All_models_1000$model = "1000m²"

x = rbind(All_models_100,All_models_400,All_models_1000)

x <- x |> 
  filter(term != "(Intercept)") |> 
  mutate(label = case_when(p.value < 0.01 ~ "**",
                               p.value < 0.05 ~ "*",
                               .default = ""))

x_left = x %>% 
  filter(term %in% c("Landform het.", "Topographic het.", "Soil Cec het.", "Soil C het.", "Soil pH het."))

x_right = x %>% 
  filter(term %in% c( "Land use intensity", "Soil sand het.", "Soil silt het.",
                     "Annual precipitation"))

x_Centre = x %>% 
  filter(term %in% c("Annual temperature", "Soil clay het.","TWI het.", "Soil N het."))



# Generate a 'small multiple' plot
dir_left = small_multiple(x_left,
               dot_args = list(aes(shape = dimension, color = dimension), size = 2.5),
               whisker_args = list(aes(linetype = dimension),
                                   shape = shape, color = shape),
               dodge_size = 0.7
               ) +
  theme_bw(base_size = 25) + ylab("") +
  geom_hline(yintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    strip.text = element_text(size = 25)
  ) +
  geom_text(
    data = x_left,
    aes(x = factor(model), y = 1, label = label, color = dimension),
    position = position_dodge(.7),
    vjust = .9,
    size = 35 / .pt
  ) +
  scale_color_manual(values = c("#FF1F5B", "#009ADE", "#FFC61E")) +
  ylim(-0.8, 1)


dir_right = small_multiple(x_right,
               dot_args = list(aes(shape = dimension, col = dimension), size = 2.5),
               whisker_args = list(aes(linetype = dimension)),
               dodge_size = 0.7
) +
  theme_bw(base_size = 25) + ylab("") +
  geom_hline(yintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    strip.text = element_text(size = 25)
  ) +
  geom_text(
    data = x_right,
    aes(x = factor(model), y = 1, label = label, color = dimension),
    position = position_dodge(.7),
    vjust = .9,
    size = 35 / .pt
  ) +
  scale_color_manual(values = c("#FF1F5B", "#009ADE", "#FFC61E")) +
  ylim(-0.8, 1)




dir_centre = small_multiple(x_Centre,
                       dot_args = list(aes(shape = dimension, col = dimension), size = 2.5),
                       whisker_args = list(aes(linetype = dimension)),
                       dodge_size = 0.7
) +
  theme_bw(base_size = 25) + ylab("") +
  geom_hline(yintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    strip.text = element_text(size = 25)
  ) +
  geom_text(
    data = x_Centre,
    aes(x = factor(model), y = 1, label = label, color = dimension),
    position = position_dodge(.7),
    vjust = .9,
    size = 35 / .pt
  ) +
  scale_color_manual(values = c("#FF1F5B", "#009ADE", "#FFC61E")) +
  ylim(-0.8, 1)
# ---------------------- INDIRECT EFFECTS ------------------
source("./Analysis/Final_SEM_models/SEM_model_1000.R")

SEM_rich = sem(model = model_1000_Rich, data = data_1000)
SEM_eve = sem(model = model_1000_Eve, data = data_1000)
SEM_div = sem(model = model_1000_Div, data = data_1000)

SEM_rich = tidy(SEM_rich)
SEM_rich = SEM_rich[1:c(nrow(SEM_rich) - 2),]
SEM_eve = tidy(SEM_eve)
SEM_eve = SEM_eve[1:c(nrow(SEM_eve) - 2),]
SEM_div = tidy(SEM_div)
SEM_div = SEM_div[1:c(nrow(SEM_div) - 2),]

SEM_rich$model = "Trait richness"
SEM_eve$model = "Trait evenness"
SEM_div$model = "Trait divergence"

y = rbind(SEM_rich,SEM_eve,SEM_div)

y[y$label == "Top_total_indirect", "label"] = "Topographic het."
y[y$label == "LF_total_indirect", "label"] = "Landform het."
y[y$label == "TWI_total_indirect", "label"] = "TWI het."
y[y$label == "Temp_total_indirect", "label"] = "Annual temperature"
y[y$label == "Precipitation_total_indirect", "label"] = "Annual precipitation"
y[y$label == "CeC_total_indirect", "label"] = "Soil Cec het."
y[y$label == "Clay_total_indirect", "label"] = "Soil clay het."
y[y$label == "Silt_total_indirect", "label"] = "Soil silt het."
y[y$label == "Sand_total_indirect", "label"] = "Soil sand het."
y[y$label == "SoC_total_indirect", "label"] = "Soil C het."
y[y$label == "pH_total_indirect", "label"] = "Soil pH het."
y[y$label == "LI_total_indirect", "label"] = "Land use intensity"

SEM_1000 = y %>% filter( grepl( "total" , term))
SEM_1000$term = SEM_1000$label

source("./Analysis/Final_SEM_models/SEM_model_400.R")

SEM_rich = sem(model = model_400_Rich, data = data_400)
SEM_eve = sem(model = model_400_Eve, data = data_400)
SEM_div = sem(model = model_400_Div, data = data_400)

SEM_rich = tidy(SEM_rich)
SEM_rich = SEM_rich[1:c(nrow(SEM_rich) - 2),]
SEM_eve = tidy(SEM_eve)
SEM_eve = SEM_eve[1:c(nrow(SEM_eve) - 2),]
SEM_div = tidy(SEM_div)
SEM_div = SEM_div[1:c(nrow(SEM_div) - 2),]

SEM_rich$model = "Trait richness"
SEM_eve$model = "Trait evenness"
SEM_div$model = "Trait divergence"

y = rbind(SEM_rich,SEM_eve,SEM_div)

y[y$label == "Top_total_indirect", "label"] = "Topographic het."
y[y$label == "LF_total_indirect", "label"] = "Landform het."
y[y$label == "TWI_total_indirect", "label"] = "TWI het."
y[y$label == "Temp_total_indirect", "label"] = "Annual temperature"
y[y$label == "Precipitation_total_indirect", "label"] = "Annual precipitation"
y[y$label == "CeC_total_indirect", "label"] = "Soil Cec het."
y[y$label == "Clay_total_indirect", "label"] = "Soil clay het."
y[y$label == "Silt_total_indirect", "label"] = "Soil silt het."
y[y$label == "Sand_total_indirect", "label"] = "Soil sand het."
y[y$label == "SoC_total_indirect", "label"] = "Soil C het."
y[y$label == "pH_total_indirect", "label"] = "Soil pH het."
y[y$label == "LI_total_indirect", "label"] = "Land use intensity"

SEM_400 = y %>% filter( grepl( "total" , term))
SEM_400$term = SEM_400$label

# 100m plots
source("./Analysis/Final_SEM_models/SEM_model_100.R")

SEM_rich = sem(model = model_100_Rich, data = data_100)
SEM_eve = sem(model = model_100_Eve, data = data_100)
SEM_div = sem(model = model_100_Div, data = data_100)

SEM_rich = tidy(SEM_rich)
SEM_rich = SEM_rich[1:c(nrow(SEM_rich) - 2),]
SEM_eve = tidy(SEM_eve)
SEM_eve = SEM_eve[1:c(nrow(SEM_eve) - 2),]
SEM_div = tidy(SEM_div)
SEM_div = SEM_div[1:c(nrow(SEM_div) - 2),]

SEM_rich$model = "Trait richness"
SEM_eve$model = "Trait evenness"
SEM_div$model = "Trait divergence"

y = rbind(SEM_rich,SEM_eve,SEM_div)

y[y$label == "Top_total_indirect", "label"] = "Topographic het."
y[y$label == "LF_total_indirect", "label"] = "Landform het."
y[y$label == "TWI_total_indirect", "label"] = "TWI het."
y[y$label == "Temp_total_indirect", "label"] = "Annual temperature"
y[y$label == "Precipitation_total_indirect", "label"] = "Annual precipitation"
y[y$label == "CeC_total_indirect", "label"] = "Soil Cec het."
y[y$label == "Clay_total_indirect", "label"] = "Soil clay het."
y[y$label == "Silt_total_indirect", "label"] = "Soil silt het."
y[y$label == "Sand_total_indirect", "label"] = "Soil sand het."
y[y$label == "SoC_total_indirect", "label"] = "Soil C het."
y[y$label == "pH_total_indirect", "label"] = "Soil pH het."
y[y$label == "LI_total_indirect", "label"] = "Land use intensity"

SEM_100 = y %>% filter( grepl( "total" , term))
SEM_100$term = SEM_100$label

# CREATE PLOTS

SEM_100  = rename(SEM_100, dimension = model)
SEM_400  = rename(SEM_400, dimension = model)
SEM_1000  = rename(SEM_1000, dimension = model)

SEM_100$model = "100m²"
SEM_400$model = "400m²"
SEM_1000$model = "1000m²"

x = rbind(SEM_100,SEM_400,SEM_1000)

x <- x |> 
  filter(term != "(Intercept)") |> 
  mutate(label = case_when(p.value < 0.01 ~ "**",
                           p.value < 0.05 ~ "*",
                           .default = ""))

x_left = x %>% 
  filter(term %in% c("Landform het.", "Topographic het.", "Soil C het.", "Soil pH het."))
x_right = x %>% 
  filter(term %in% c( "Land use intensity", "Soil sand het.", "Soil silt het.",
                      "Annual precipitation"))
x_Centre = x %>% 
  filter(term %in% c("Annual temperature", "Soil clay het.","TWI het.", "Soil N het.", "Soil Cec het."))


ind_left = small_multiple(x_left,
                      dot_args = list(aes(shape = dimension, col = dimension), size = 2.5),
                      whisker_args = list(aes(linetype = dimension),
                                          shape = shape, color = shape),
                      dodge_size = 0.7
) +
  theme_bw(base_size = 25) + ylab("") +
  geom_hline(yintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    strip.text = element_text(size = 25)
  ) +
  geom_text(
    data = x_left,
    aes(x = factor(model), y = 1, label = label, color = dimension),
    position = position_dodge(.7),
    vjust = .9,
    size = 35 / .pt
  ) +
  scale_color_manual(values = c("#FF1F5B", "#009ADE", "#FFC61E")) +
  ylim(-0.8, 1)


ind_right = small_multiple(x_right,
                       dot_args = list(aes(shape = dimension, col = dimension), size = 2.5),
                       whisker_args = list(aes(linetype = dimension)),
                       dodge_size = 0.7
) +
  theme_bw(base_size = 25) + ylab("") +
  geom_hline(yintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    strip.text = element_text(size = 25)
  ) +
  geom_text(
    data = x_right,
    aes(x = factor(model), y = 1, label = label, color = dimension),
    position = position_dodge(.7),
    vjust = .9,
    size = 35 / .pt
  ) +
  scale_color_manual(values = c("#FF1F5B", "#009ADE", "#FFC61E")) +
  ylim(-0.8, 1)


ind_centre = small_multiple(x_Centre,
                        dot_args = list(aes(shape = dimension, col = dimension), size = 2.5),
                        whisker_args = list(aes(linetype = dimension)),
                        dodge_size = 0.7
) +
  theme_bw(base_size = 25) + ylab("") +
  geom_hline(yintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 30),
    strip.text = element_text(size = 25)
  ) +
  geom_text(
    data = x_Centre,
    aes(x = factor(model), y = 1, label = label, color = dimension),
    position = position_dodge(.7),
    vjust = .9,
    size = 35 / .pt
  ) +
  scale_color_manual(values = c("#FF1F5B", "#009ADE", "#FFC61E")) +
  ylim(-0.8, 1)

# For legend to crop
legend = small_multiple(x_Centre,
                dot_args = list(aes(shape = dimension, col = dimension), size = 2.5),
                whisker_args = list(aes(linetype = dimension))
) +
  theme_bw(base_size = 25) + ylab("") +
  geom_hline(yintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 25)
  ) +
  scale_color_manual(values = c("#FF1F5B", "#009ADE", "#FFC61E")) +
  ylim(-0.8, 1) + 
  guides(
  shape = guide_legend("Dimension"),
  colour = guide_legend("Dimension")) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.text = element_text(size = 30),
        legend.title=element_blank())

# ----------------- CREATE PLOTS ------------------------------
dir.create("./Figures_Tables/", 
           showWarnings = F)

all_legend <- get_legend(legend)

jpeg(file= paste0("./Figures_Tables/Indirect_effects_dot_whisker.jpeg")
     ,width = 2000, height = 1685)
grid.arrange(arrangeGrob(ind_left + theme(legend.position="none"), 
                         ind_centre + theme(legend.position="none"),
                         ind_right + theme(legend.position="none"), ncol=3), 
             all_legend, 
             nrow=2,heights=c(20, 1))
dev.off()

jpeg(file= paste0("./Figures_Tables/Direct_effects_dot_whisker.jpeg")
     ,width = 2000, height = 1685)
grid.arrange(arrangeGrob(dir_left + theme(legend.position="none"), 
                         dir_centre + theme(legend.position="none"),
                         dir_right + theme(legend.position="none"), ncol=3), 
             all_legend, 
             nrow=2,heights=c(20, 1))
dev.off()