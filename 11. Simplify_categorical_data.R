rm(list=ls())
if ("dplyr" %in% rownames(installed.packages()) == FALSE) { install.packages("dplyr")}
library(dplyr)

setwd("D:/Europe_study_data/Analysis/Finalised_dataset/Transformed/")


data_100 = read.csv("./Full_dataset_100m_plots_transformed.csv")
data_100$Land_system_names = as.factor(data_100$Land_system_names)
levels(data_100$Land_system_names)


df <- list.files(path = "./",
                 pattern = ".csv",
                 recursive = F)


for (i in 1:length(df)) {
  x = read.csv(df[[i]])
  x$Land_system_names <- as.factor(x$Land_system_names)
  x$intensity_bi <- dplyr::recode_factor(x$Land_system_names,
                                              "bare and rocks" = "low",                              
                                              "extensive perm-crops" = "high",                       
                                              "forest/shrubs and bare mosaics" = "low",              
                                              "forest/shrubs and cropland mosaics" = "low",         
                                              "forest/shrubs and grassland mosaics" = "low",         
                                              "forest/shrubs and mixed agriculture mosaics" = "low",
                                              "glacier" = "low",                                     
                                              "high-intensity agricultural mosaics" = "high",        
                                              "high-intensity cropland" = "high",                     
                                              "high-intensity forest" = "high",                      
                                              "high-intensity grassland" = "high",                    
                                              "high intensity settlement" = "high",                  
                                              "intensive perm-crops" ="high",                        
                                              "low-intensity agricultural mosaics" = "low",         
                                              "low-intensity cropland" = "low",                      
                                              "low-intensity forest" = "low",                        
                                              "low-intensity grassland" = "low",                     
                                              "low-intensity settlement" = "low",                   
                                              "medium-intensity agricultural mosaics" = "low",       
                                              "medium-intensity cropland" = "low",                  
                                              "medium-intensity forest" = "low",                   
                                              "medium-intensity grassland" = "low",                 
                                              "medium intensity settlement" = "low",                 
                                              "shrub" = "low",                                      
                                              "water body" = "low",
                                              "wetland" = "low"
                                              )
  x$intensity_ord <- dplyr::recode_factor(x$Land_system_names,
                                          "bare and rocks" = "none",                              
                                          "extensive perm-crops" = "high",                       
                                          "forest/shrubs and bare mosaics" = "none",              
                                          "forest/shrubs and cropland mosaics" = "low",         
                                          "forest/shrubs and grassland mosaics" = "none",         
                                          "forest/shrubs and mixed agriculture mosaics" = "low",
                                          "glacier" = "none",                                     
                                          "high-intensity agricultural mosaics" = "high",        
                                          "high-intensity cropland" = "high",                     
                                          "high-intensity forest" = "high",                      
                                          "high-intensity grassland" = "high",                    
                                          "high intensity settlement" = "high",                  
                                          "intensive perm-crops" ="high",                        
                                          "low-intensity agricultural mosaics" = "low",         
                                          "low-intensity cropland" = "low",                      
                                          "low-intensity forest" = "low",                        
                                          "low-intensity grassland" = "low",                     
                                          "low-intensity settlement" = "low",                   
                                          "medium-intensity agricultural mosaics" = "medium",       
                                          "medium-intensity cropland" = "medium",                  
                                          "medium-intensity forest" = "medium",                   
                                          "medium-intensity grassland" = "medium",                 
                                          "medium intensity settlement" = "medium",                 
                                          "shrub" = "none",                                      
                                          "water body" = "none",
                                          "wetland" = "none"
                                          )
  x$intensity_bi_N <- dplyr::recode_factor(x$Land_system_names,
                                          "bare and rocks" = 0,                              
                                          "extensive perm-crops" = 1,                       
                                          "forest/shrubs and bare mosaics" = 0,              
                                          "forest/shrubs and cropland mosaics" = 0,         
                                          "forest/shrubs and grassland mosaics" = 0,         
                                          "forest/shrubs and mixed agriculture mosaics" = 0,
                                          "glacier" = 0,                                     
                                          "high-intensity agricultural mosaics" = 1,        
                                          "high-intensity cropland" = 1,                     
                                          "high-intensity forest" = 1,                      
                                          "high-intensity grassland" = 1,                    
                                          "high intensity settlement" = 1,                  
                                          "intensive perm-crops" = 1,                        
                                          "low-intensity agricultural mosaics" = 0,         
                                          "low-intensity cropland" = 0,                      
                                          "low-intensity forest" = 0,                        
                                          "low-intensity grassland" = 0,                     
                                          "low-intensity settlement" = 0,                   
                                          "medium-intensity agricultural mosaics" = 1,       
                                          "medium-intensity cropland" = 1,                  
                                          "medium-intensity forest" = 1,                   
                                          "medium-intensity grassland" = 1,                 
                                          "medium intensity settlement" = 1,                 
                                          "shrub" = 0,                                      
                                          "water body" = 0,
                                          "wetland" = 0
  )
  x$intensity_ord_N <- dplyr::recode_factor(x$Land_system_names,
                                            "bare and rocks" = 1,                              
                                            "extensive perm-crops" = 4,                       
                                            "forest/shrubs and bare mosaics" = 1,              
                                            "forest/shrubs and cropland mosaics" = 2,         
                                            "forest/shrubs and grassland mosaics" = 1,         
                                            "forest/shrubs and mixed agriculture mosaics" = 2,
                                            "glacier" = 1,                                     
                                            "high-intensity agricultural mosaics" = 4,        
                                            "high-intensity cropland" = 4,                     
                                            "high-intensity forest" = 4,                      
                                            "high-intensity grassland" = 4,                    
                                            "high intensity settlement" = 4,                  
                                            "intensive perm-crops" = 4,                        
                                            "low-intensity agricultural mosaics" = 2,         
                                            "low-intensity cropland" = 2,                      
                                            "low-intensity forest" = 2,                        
                                            "low-intensity grassland" = 2,                     
                                            "low-intensity settlement" = 2,                   
                                            "medium-intensity agricultural mosaics" = 3,       
                                            "medium-intensity cropland" = 3,                  
                                            "medium-intensity forest" = 3,                   
                                            "medium-intensity grassland" = 3,                 
                                            "medium intensity settlement" = 3,                 
                                            "shrub" = 1,                                      
                                            "water body" = 1,
                                            "wetland" = 1
  )
  write.csv(x, file = paste0("./", df[[i]]), row.names = F)
  }

