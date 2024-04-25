options(scipen = 100)
# -------------------- PACKAGES ------------------
libs <- c("lavaan", "piecewiseSEM", "lavaanPlot", "effectsize", "stringr", "tidyverse", "broom", "DiagrammeRsvg", "rsvg")

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


# -------------- 100m path diagrams ------------------
source("./Analysis/Final_SEM_models/SEM_model_100.R")

SEM_rich = sem(model = model_100_Rich, data = data_100)
SEM_eve = sem(model = model_100_Eve, data = data_100)
SEM_div = sem(model = model_100_Div, data = data_100)

labels_rich <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
               BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
               pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
               Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.rich = "Trait richness")

labels_eve <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                    BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                    pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                    Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.eve = "Trait evenness")

labels_div <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                    BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                    pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                    Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.div = "Trait divergence")


rich = lavaanPlot(model=SEM_rich, labels = labels_rich, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
save_png(rich, "./Figures_Tables/Appendices/SEM_100_Frich.png")
eve = lavaanPlot(model=SEM_eve, labels = labels_eve, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
save_png(eve, "./Figures_Tables/Appendices/SEM_100_Feve.png")
div = lavaanPlot(model=SEM_div, labels = labels_div, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
save_png(div, "./Figures_Tables/Appendices/SEM_100_Fdiv.png")


#----------------------------- 400m path diagrams --------------------
source("./Analysis/Final_SEM_models/SEM_model_400.R") 

SEM_rich = sem(model = model_400_Rich, data = data_400)
SEM_eve = sem(model = model_400_Eve, data = data_400)
SEM_div = sem(model = model_400_Div, data = data_400)

labels_rich <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                    BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                    pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                    Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.rich = "Trait richness")

labels_eve <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                   BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                   pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                   Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.eve = "Trait evenness")

labels_div <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                   BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                   pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                   Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.div = "Trait divergence")

rich = lavaanPlot(model=SEM_rich, labels = labels_rich, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
save_png(rich, "./Figures_Tables/Appendices/SEM_400_Frich.png")
eve = lavaanPlot(model=SEM_eve, labels = labels_eve, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
save_png(eve, "./Figures_Tables/Appendices/SEM_400_Feve.png")
div = lavaanPlot(model=SEM_div, labels = labels_div, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
save_png(div, "./Figures_Tables/Appendices/SEM_400_Fdiv.png")

# ---------------------------- 1000m path diagrams -----------------------
source("./Analysis/Final_SEM_models/SEM_model_1000.R")

SEM_rich = sem(model = model_1000_Rich, data = data_1000)
SEM_eve = sem(model = model_1000_Eve, data = data_1000)
SEM_div = sem(model = model_1000_Div, data = data_1000)

labels_rich <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                    BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                    pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                    Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.rich = "Trait richness")

labels_eve <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                   BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                   pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                   Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.eve = "Trait evenness")

labels_div <- list(Top_Rao= "Topographic het.", Landform_sha = "Landform het.", BIO12_Annual_Precipitation = "Annual precipitation",
                   BIO1_Annual_Mean_Temperature = "Annual temperature", Silt_Rao = "Soil silt het.", TWI_Rao = "TWI het.", Sand_Rao = "Soil sand het.",
                   pH_Rao = "Soil pH het.", Clay_Rao = "Soil clay het.", Cec_Rao = "Soil Cec het.", intensity_ord_N = "Land use intensity",
                   Soc_Rao = "Soil C het.", N_Rao = "Soil N het.", F.div = "Trait divergence")

rich = lavaanPlot(model=SEM_rich, labels = labels_rich, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
save_png(rich, "./Figures_Tables/Appendices/SEM_1000_Frich.png")
eve = lavaanPlot(model=SEM_eve, labels = labels_eve, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
save_png(eve, "./Figures_Tables/Appendices/SEM_1000_Feve.png")
div = lavaanPlot(model=SEM_div, labels = labels_div, coefs = T, edge_options = list(color = "grey"), stars = c("regress"))
save_png(div, "./Figures_Tables/Appendices/SEM_1000_Frdiv.png")

