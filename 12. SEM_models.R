rm(list=ls())
options(scipen = 100)
# -------------------- PACAKGES ------------------
if ("lavaan" %in% rownames(installed.packages()) == FALSE) {install.packages("lavaan")}
library(lavaan)
if ("piecewiseSEM" %in% rownames(installed.packages()) == FALSE) {install.packages("piecewiseSEM")}
library(piecewiseSEM)
if ("lavaanPlot" %in% rownames(installed.packages()) == FALSE) {install.packages("lavaanPlot")}
library(lavaanPlot)
if ("effectsize" %in% rownames(installed.packages()) == FALSE) {install.packages("effectsize")}
library(effectsize)

# ----------------- DATA ---------------------
setwd("D:/Europe_study_data/Analysis/Finalised_dataset/Transformed/")
data_100 = read.csv("./Full_dataset_100m_plots_transformed.csv")
data_400 = read.csv("./Full_dataset_400m_plots_transformed.csv")
data_1000 = read.csv("./Full_dataset_1000m_plots_transformed.csv")

data_100$intensity_ord_N = factor(data_100$intensity_ord_N, ordered = T, levels = c("1","2", "3", "4"))
data_100$intensity_bi_N = factor(data_100$intensity_bi_N, ordered = T)

data_400$intensity_ord_N = factor(data_400$intensity_ord_N, ordered = T, levels = c("1","2", "3", "4"))
data_400$intensity_bi_N = factor(data_400$intensity_bi_N, ordered = T)
# ------------------- RUN 100m PLOT MODEL ---------------------------
data_100$intensity_ord_N


str(data_100)

model_100 = 
  " # Regressions
        F.eve ~ TR*Top_Rao + TWR*TWI_Rao + NR*N_Rao + CeR*Cec_Rao + SCR*Soc_Rao + PR*pH_Rao + ATR*BIO1_Annual_Mean_Temperature +
        APR*BIO12_Annual_Precipitation + LR*Landform_sha + CR*Clay_Rao + intensity_ord_N
        
        N_Rao ~ TN*TWI_Rao + APN*BIO12_Annual_Precipitation + CeN*Cec_Rao + SN*Sand_Rao + CN*Clay_Rao + SCN*Soc_Rao + PN*pH_Rao
        
        Soc_Rao ~ TSC*Top_Rao + SSC*Sand_Rao + ATSC*BIO1_Annual_Mean_Temperature + APSC*BIO12_Annual_Precipitation + CeSC*Cec_Rao + 
        LSC*Landform_sha + SCC*Clay_Rao
        
        pH_Rao ~ TP*Top_Rao + ATP*BIO1_Annual_Mean_Temperature  + APP*BIO12_Annual_Precipitation + SiP*Silt_Rao + CP*Clay_Rao + 
        CeP*Cec_Rao + TWP*TWI_Rao
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha
        
        Clay_Rao ~ TC*Top_Rao + LC*Landform_sha + APC*BIO12_Annual_Precipitation + ATC*BIO1_Annual_Mean_Temperature
        
        TWI_Rao ~ TTW*Top_Rao
        
        Silt_Rao ~ LSi*Landform_sha + TSi*Top_Rao
        
        Cec_Rao ~ SCe*Sand_Rao + ATCe*BIO1_Annual_Mean_Temperature + APCe*BIO12_Annual_Precipitation + TWCe*TWI_Rao + CCe*Clay_Rao + 
        SiCe*Silt_Rao
        
       # Covariances
          TWI_Rao ~~ BIO12_Annual_Precipitation 
          Landform_sha ~~ Top_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + intensity_ord_N
          Top_Rao ~~ BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + intensity_ord_N
          BIO1_Annual_Mean_Temperature ~~ BIO12_Annual_Precipitation + intensity_ord_N
          BIO12_Annual_Precipitation ~~ intensity_ord_N
          Soc_Rao ~~  pH_Rao 
          N_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao"


SEM= sem(model_100, data = data_100)
summary(SEM, standardized = T, rsq = T)
semPlot::semPaths(SEM)

# MODEL FIT 
# interpret(SEM)
lavaan::fitMeasures(SEM, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM)
head(mi[order(mi$mi, decreasing = T),],30)

# P test iterator (for large sample sizes)
df <- data.frame(matrix(ncol = 6, nrow = 1000))
colnames(df) = c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr")

for (i in seq (1:1000)) {
  x = dplyr::slice_sample(data_100, n = 500, replace = F)
  SEM = lavaan::sem(model_100, data = x, estimator = "WLSMV")
  FM <- lavaan::fitMeasures(SEM, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
  df[i,] = FM
}

# Summary statistics of iterator
mean(df$pvalue, na.rm = T)
range(df$pvalue, na.rm = T)
median(df$pvalue, na.rm = T)

# Model comparison
anova(SEM1, SEM2)









# ------------------- RUN 400m PLOT MODEL ---------------------------
str(data_400)
data_400$BIO1_Annual_Mean_Temperature

model_400 = 
  " # Regressions
        F.rich ~ Top_Rao + BIO1_Annual_Mean_Temperature
        
    # Covariances
        Top_Rao ~~ BIO1_Annual_Mean_Temperature"

SEM = sem(model_400, data = data_400)
summary(SEM, standardized = T, rsq = T)
semPlot::semPaths(SEM)


# ------------------- RUN 1000m PLOT MODEL ---------------------------
str(data_400)
data_400$BIO1_Annual_Mean_Temperature

model_1000 = 
  " # Regressions
        F.div ~ Top_Rao + BIO1_Annual_Mean_Temperature
        
    # Covariances
        Top_Rao ~~ BIO1_Annual_Mean_Temperature"

SEM = sem(model_1000, data = data_1000)
summary(SEM, standardized = T, rsq = T)
semPlot::semPaths(SEM)





# --------- OLD MODEL FROM MANUSCRIPT ROUND 1 -------------- 

model2 = 
  "# Regressions
        F_div ~ TR*Top_Rao + TWR*TWI_Rao + NR*Nit_Rao + CeR*Cec_Rao + SCR*Soc_Rao + PR*pH_Rao + ATR*Ann_temp + 
        APR*Ann_prec + LR*Landform_Sha+ CR*Clay_Rao + HR*Hyd_feat_Sha
        Nit_Rao ~ TN*TWI_Rao + APN*Ann_prec + CeN*Cec_Rao + SN*Sand_Rao + CN*Clay_Rao 
        Soc_Rao ~ TSC*Top_Rao + SSC*Sand_Rao + ATSC*Ann_temp + APSC*Ann_prec + CeSC*Cec_Rao + LSC*Landform_Sha + SCC*Clay_Rao
        pH_Rao ~ TP*Top_Rao + ATP*Ann_temp  + APP*Ann_prec + SiP*Silt_Rao + CP*Clay_Rao + CeP*Cec_Rao
        Sand_Rao ~ TS*Top_Rao + LS*Landform_Sha
        Clay_Rao ~ TC*Top_Rao + LC*Landform_Sha + APC*Ann_prec + ATC*Ann_temp
        TWI_Rao ~ TTW*Top_Rao
        Silt_Rao ~ LSi*Landform_Sha + TSi*Top_Rao
        Cec_Rao ~ SCe*Sand_Rao + ATCe*Ann_temp + APCe*Ann_prec + TWCe*TWI_Rao + CCe*Clay_Rao + SiCe*Silt_Rao
        
        
        # Covariances
          TWI_Rao ~~ Ann_prec + Hyd_feat_Sha
          Landform_Sha ~~ Top_Rao + Ann_temp + Ann_prec
          Top_Rao ~~ Ann_temp + Ann_prec
          Ann_temp ~~ Ann_prec
          Soc_Rao ~~  Nit_Rao + pH_Rao
          Nit_Rao ~~ pH_Rao
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao


        # Derived quantities
          # Topography
          Top_via_TWI := TTW*TR
          Top_via_Clay := TC*CR
          Top_via_pH := TP*PR
          Top_via_SoC := TSC*SCR
          Top_Total_ind := TSC*SCR + TP*PR + TC*CR + TTW*TR
          
          # Temperature
          Temp_via_Cec := ATCe*CeR
          Temp_via_Clay := ATC*CR
          Temp_via_pH := ATP*PR
          Temp_via_SoC := ATSC*SCR
          Temp_Total_ind := ATCe*CeR + ATC*CR + ATP*PR + ATSC*SCR
          
          # Precipitation
          Prec_via_Cec := APCe*CeR
          Prec_via_Clay := APC*CR
          Prec_via_pH := APP*PR
          Prec_via_SoC := APSC*SCR
          Prec_via_Nit := APN*NR
          Prec_Total_ind := APCe*CeR + APC*CR + APP*PR + APSC*SCR + APN*NR
          
          # Landform
          LF_via_Clay := LC*CR
          LF_via_SoC := LSC*SCR
          LF_Total_ind := LC*CR + LSC*SCR
          
          #Sand
          Sand_via_Cec := SCe*CeR
          Sand_via_SoC := SSC*SCR
          Sand_via_Nit := SN*NR
          Sand_Total_ind := SCe*CeR + SSC*SCR + SN*NR
          
          # Silt
          Silt_via_pH := SiP*PR
          Silt_via_Cec := SiCe*CeR
          Silt_total_ind := SiP*PR + SiCe*CeR
          
          # Clay
          Clay_via_pH := CP*PR
          Clay_via_SoC := SCC*CR
          Clay_via_Cec := CCe*CeR
          Clay_via_Nit := CN*NR
          Clay_Total_ind := CP*PR + SCC*CR + CN*NR
          
          # Cec
          CeC_via_pH := CeP*PR
          CeC_via_SoC := CeSC*SCR
          CeC_via_Nit : = CeN*NR
          CeC_Total_ind := CeP*PR + CeSC*SCR + CeN*NR
          
         # Total effects
          Total_direct := TR + TWR + NR + CeR + SCR + PR + ATR + APR + LR + CR + HR
          Total_indirect := CeP*PR + CeSC*SCR + CeN*NR + CP*PR + SCC*CR + CN*NR + SiP*PR + SiCe*CeR +
                            SCe*CeR + SSC*SCR + SN*NR + LC*CR + LSC*SCR + APCe*CeR + APC*CR + APP*PR + APSC*SCR + APN*NR +
                            ATCe*CeR + ATC*CR + ATP*PR + ATSC*SCR + TSC*SCR + TP*PR + TC*CR + TTW*TR
          
"


# TEST MODEL

simp_model_1 = 
  " # Regressions
        F.rich ~ TR*Top_Rao + TWR*TWI_Rao + NR*N_Rao + CeR*Cec_Rao + SCR*Soc_Rao + PR*pH_Rao + ATR*BIO1_Annual_Mean_Temperature +
        APR*BIO12_Annual_Precipitation + LR*Landform_sha + CR*Clay_Rao + intensity_ord_N
        
        N_Rao ~ APN*BIO12_Annual_Precipitation + CeN*Cec_Rao + CN*Clay_Rao + SCN*Soc_Rao + PN*pH_Rao
        
        Soc_Rao ~ TSC*Top_Rao + SSC*Sand_Rao + ATSC*BIO1_Annual_Mean_Temperature + APSC*BIO12_Annual_Precipitation + CeSC*Cec_Rao + 
        LSC*Landform_sha + SCC*Clay_Rao + intensity_ord_N
        
        pH_Rao ~ TP*Top_Rao + ATP*BIO1_Annual_Mean_Temperature  + APP*BIO12_Annual_Precipitation + SiP*Silt_Rao + CP*Clay_Rao + 
        CeP*Cec_Rao + TWP*TWI_Rao
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha
        
        Clay_Rao ~ TC*Top_Rao + LC*Landform_sha + APC*BIO12_Annual_Precipitation + ATC*BIO1_Annual_Mean_Temperature
        
        TWI_Rao ~ TTW*Top_Rao
        
        Silt_Rao ~ LSi*Landform_sha + TSi*Top_Rao
        
        Cec_Rao ~ SCe*Sand_Rao + ATCe*BIO1_Annual_Mean_Temperature + APCe*BIO12_Annual_Precipitation + CCe*Clay_Rao + 
        SiCe*Silt_Rao
        
       # Covariances
          TWI_Rao ~~ BIO12_Annual_Precipitation 
          Landform_sha ~~ Top_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + intensity_ord_N
          Top_Rao ~~ BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + intensity_ord_N
          BIO1_Annual_Mean_Temperature ~~ BIO12_Annual_Precipitation + intensity_ord_N
          BIO12_Annual_Precipitation ~~ intensity_ord_N
          Soc_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao"

# Removed soc~ sand
simp_model_2 = 
  " # Regressions
        F.rich ~ TR*Top_Rao + TWR*TWI_Rao + NR*N_Rao + CeR*Cec_Rao + SCR*Soc_Rao + PR*pH_Rao + ATR*BIO1_Annual_Mean_Temperature +
        APR*BIO12_Annual_Precipitation + LR*Landform_sha + CR*Clay_Rao + intensity_ord_N
        
        N_Rao ~ APN*BIO12_Annual_Precipitation + CeN*Cec_Rao + CN*Clay_Rao + SCN*Soc_Rao + PN*pH_Rao
        
        Soc_Rao ~ TSC*Top_Rao + ATSC*BIO1_Annual_Mean_Temperature + APSC*BIO12_Annual_Precipitation + CeSC*Cec_Rao + 
        LSC*Landform_sha + SCC*Clay_Rao + intensity_ord_N
        
        pH_Rao ~ TP*Top_Rao + ATP*BIO1_Annual_Mean_Temperature  + APP*BIO12_Annual_Precipitation + SiP*Silt_Rao + CP*Clay_Rao + 
        CeP*Cec_Rao + TWP*TWI_Rao
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha
        
        Clay_Rao ~ TC*Top_Rao + LC*Landform_sha + APC*BIO12_Annual_Precipitation + ATC*BIO1_Annual_Mean_Temperature
        
        TWI_Rao ~ TTW*Top_Rao
        
        Silt_Rao ~ LSi*Landform_sha + TSi*Top_Rao
        
        Cec_Rao ~ SCe*Sand_Rao + ATCe*BIO1_Annual_Mean_Temperature + APCe*BIO12_Annual_Precipitation + CCe*Clay_Rao + 
        SiCe*Silt_Rao
        
       # Covariances
          TWI_Rao ~~ BIO12_Annual_Precipitation 
          Landform_sha ~~ Top_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + intensity_ord_N
          Top_Rao ~~ BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + intensity_ord_N
          BIO1_Annual_Mean_Temperature ~~ BIO12_Annual_Precipitation + intensity_ord_N
          BIO12_Annual_Precipitation ~~ intensity_ord_N
          Soc_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao"


# Removed Landform ~ clay
# Accepted model
simp_model_3= 
  " # Regressions
        F.rich ~ TR*Top_Rao + TWR*TWI_Rao + NR*N_Rao + CeR*Cec_Rao + SCR*Soc_Rao + PR*pH_Rao + ATR*BIO1_Annual_Mean_Temperature +
        APR*BIO12_Annual_Precipitation + LR*Landform_sha + CR*Clay_Rao + intensity_ord_N
        
        N_Rao ~ APN*BIO12_Annual_Precipitation + CeN*Cec_Rao + CN*Clay_Rao + SCN*Soc_Rao + PN*pH_Rao
        
        Soc_Rao ~ TSC*Top_Rao + ATSC*BIO1_Annual_Mean_Temperature + APSC*BIO12_Annual_Precipitation + CeSC*Cec_Rao + 
        LSC*Landform_sha + SCC*Clay_Rao + intensity_ord_N
        
        pH_Rao ~ TP*Top_Rao + ATP*BIO1_Annual_Mean_Temperature  + APP*BIO12_Annual_Precipitation + SiP*Silt_Rao + CP*Clay_Rao + 
        CeP*Cec_Rao + TWP*TWI_Rao
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha
        
        Clay_Rao ~ TC*Top_Rao + APC*BIO12_Annual_Precipitation + ATC*BIO1_Annual_Mean_Temperature
        
        TWI_Rao ~ TTW*Top_Rao
        
        Silt_Rao ~ LSi*Landform_sha + TSi*Top_Rao
        
        Cec_Rao ~ SCe*Sand_Rao + ATCe*BIO1_Annual_Mean_Temperature + APCe*BIO12_Annual_Precipitation + CCe*Clay_Rao + 
        SiCe*Silt_Rao
        
       # Covariances
          TWI_Rao ~~ BIO12_Annual_Precipitation 
          Landform_sha ~~ Top_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + intensity_ord_N
          Top_Rao ~~ BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + intensity_ord_N
          BIO1_Annual_Mean_Temperature ~~ BIO12_Annual_Precipitation + intensity_ord_N
          BIO12_Annual_Precipitation ~~ intensity_ord_N
          Soc_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao"

SEM1 = sem(simp_model_1, data = data_400, ordered = "intensity_ord_N", estimator = "WLSMV")
SEM2 = sem(simp_model_2, data = data_400, ordered = "intensity_ord_N", estimator = "WLSMV")
SEM3 = sem(simp_model_3, data = data_400, ordered = "intensity_ord_N", estimator = "WLSMV")
summary(SEM1, standardized = T, rsq = T)
summary(SEM2, standardized = T, rsq = T)
summary(SEM3, standardized = T, rsq = T)

# MODEL FIT 
# interpret(SEM)
lavaan::fitMeasures(SEM3, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
mi = modificationindices(SEM3)
head(mi[order(mi$mi, decreasing = T),],30)

# P test iterator (for large sample sizes)
df <- data.frame(matrix(ncol = 6, nrow = 1000))
colnames(df) = c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr")

for (i in seq (1:1000)) {
  tryCatch({
  x = dplyr::slice_sample(data_400, n = 400, replace = F)
  SEM = lavaan::sem(simp_model_3, data = x, estimator = "WLSMV")
  FM <- lavaan::fitMeasures(SEM, c("pvalue", "cfi", "rmsea", "chisq", "ifi", "srmr"))
  df[i,] = FM
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Summary statistics of iterator
mean(df$pvalue, na.rm = T)
range(df$pvalue, na.rm = T)
median(df$pvalue, na.rm = T)

# Model comparison
anova(SEM1, SEM2)

?sem
