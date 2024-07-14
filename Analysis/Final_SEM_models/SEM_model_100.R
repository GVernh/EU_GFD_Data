data_100 = read.csv("./Analysis/Finalised_dataset/Transformed/Full_dataset_100m_trans.csv")

model_100_Rich = 
  " # Regressions
        F.rich ~ TR*Top_Rao + TWR*TWI_Rao + NR*N_Rao + CeR*Cec_Rao + SCR*Soc_Rao + PR*pH_Rao + ATR*BIO1_Annual_Mean_Temperature +
        APR*BIO12_Annual_Precipitation + LR*Landform_sha + CR*Clay_Rao + IR*intensity_ord_N + SR*Sand_Rao + SiR*Silt_Rao
        
        N_Rao ~ APN*BIO12_Annual_Precipitation + CeN*Cec_Rao + CN*Clay_Rao + SCN*Soc_Rao + PN*pH_Rao + TN*Top_Rao + TWN*TWI_Rao +
        IN*intensity_ord_N
        
        Soc_Rao ~ TSC*Top_Rao + ATSC*BIO1_Annual_Mean_Temperature + APSC*BIO12_Annual_Precipitation + 
        SCI*intensity_ord_N + SSC*Sand_Rao
        
        pH_Rao ~ ATP*BIO1_Annual_Mean_Temperature  + APP*BIO12_Annual_Precipitation + SiP*Silt_Rao + CP*Clay_Rao + 
        + TWP*TWI_Rao + LP*Landform_sha
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha + AS*BIO1_Annual_Mean_Temperature
        
        Clay_Rao ~ TC*Top_Rao + APC*BIO12_Annual_Precipitation + ATC*BIO1_Annual_Mean_Temperature + LC*Landform_sha  + IC*intensity_ord_N
        
        TWI_Rao ~ TTW*Top_Rao + LTW*Landform_sha + APTW*BIO12_Annual_Precipitation 
        
        Silt_Rao ~ LSi*Landform_sha + TSi*Top_Rao
        
        Cec_Rao ~ SCe*Sand_Rao + ATCe*BIO1_Annual_Mean_Temperature + APCe*BIO12_Annual_Precipitation + 
        SiCe*Silt_Rao + TCe*Top_Rao + PCe*pH_Rao
        
       # Covariances
          Soc_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao
          
          
       # Derived quantities
          # Topography
            Top_via_C := TSC*SCR
            Top_via_sand := TS*SR
            Top_via_clay := TC*CR
            Top_via_TWI := TTW*TWR
            Top_via_N := TN*NR
            Top_via_CeC := TCe*CeR
            Top_total_indirect := TSC*SCR + TS*SR + TC*CR + TTW*TWR + TN*NR + TCe*CeR
            
            # Landform
            LF_via_sand := LS*SR
            LF_via_clay := LC*CR
            LF_via_pH := LP*PR
            LF_via_TWI := LTW*TWR
            LF_total_indirect := LS*SR + LC*CR + LP*PR + LTW*TWR
            
            # TWI
            TWI_via_pH := TWP*PR
            TWI_via_N := TWN*NR
            TWI_total_indirect := TWP*PR + TWN*NR
            
            # Temperature
            Temp_via_C := ATSC*SCR
            Temp_via_pH := ATP*PR
            Temp_via_clay := ATC*CR
            Temp_via_CeC := ATCe*CeR
            Temp_via_Sand := AS*SR
            Temp_total_indirect := ATSC*SCR + ATP*PR + ATC*CR + ATCe*CeR + AS*SR
            
            # Precipitation
            Prec_via_N := APN*NR
            Prec_via_C := APSC*SCR
            Prec_via_pH := APP*PR
            Prec_via_clay := APC*CR
            APCe_via_CeR := APCe*CeR
            APTW_via_TWR := APTW*TWR
            Precipitation_total_indirect := APN*NR + APSC*SCR + APP*PR + APC*CR + APCe*CeR + APTW*TWR
            
            # Cation Exchange capactity
            CeC_via_N := CeN*NR
            CeC_total_indirect := CeN*NR
            
            # Clay
            Clay_via_N := CN*NR
            Clay_via_pH := CP*PR
            Clay_total_indirect := CN*NR + CP*PR
            
            # Silt
            Silt_via_pH := SiP*PR
            Silt_via_CeC := SiCe*CeR
            Silt_total_indirect := SiP*PR + SiCe*CeR
            
            # Sand
            Sand_via_CeC := SCe*CeR
            Sand_via_SoC := SSC*SCR
            Sand_total_indirect := SCe*CeR + SSC*SCR
            
            # Organic Carbon
            SoC_via_N := SCN*NR
            SoC_total_indirect := SCN*NR
            
            # pH
            pH_via_N := PN*NR
            pH_via_CeC := PCe*CeR
            pH_total_indirect := PN*NR + PCe*CeR
            
            # Land use intensity
            LI_via_C := SCI*SCR
            LI_via_Clay := IC*CR
            LI_via_N := IN*NR
            LI_total_indirect := SCI*SCR + IC*CR +IN*NR
            
        # Total effects
            Total_direct := TR + TWR + NR + CeR + SCR + PR + ATR + APR + LR + CR + IR + SR + SiR
            Total_indirect := LI_total_indirect + pH_total_indirect + SoC_total_indirect + Sand_total_indirect + Silt_total_indirect +
            Clay_total_indirect + CeC_total_indirect + Precipitation_total_indirect + Temp_total_indirect + TWI_total_indirect + 
            LF_total_indirect + Top_total_indirect
"

model_100_Eve = 
  " # Regressions
        F.eve ~ TR*Top_Rao + TWR*TWI_Rao + NR*N_Rao + CeR*Cec_Rao + SCR*Soc_Rao + PR*pH_Rao + ATR*BIO1_Annual_Mean_Temperature +
        APR*BIO12_Annual_Precipitation + LR*Landform_sha + CR*Clay_Rao + IR*intensity_ord_N + SR*Sand_Rao + SiR*Silt_Rao
        
        N_Rao ~ APN*BIO12_Annual_Precipitation + CeN*Cec_Rao + CN*Clay_Rao + SCN*Soc_Rao + PN*pH_Rao + TN*Top_Rao + TWN*TWI_Rao +
        IN*intensity_ord_N
        
        Soc_Rao ~ TSC*Top_Rao + ATSC*BIO1_Annual_Mean_Temperature + APSC*BIO12_Annual_Precipitation + 
        SCI*intensity_ord_N + SSC*Sand_Rao
        
        pH_Rao ~ ATP*BIO1_Annual_Mean_Temperature  + APP*BIO12_Annual_Precipitation + SiP*Silt_Rao + CP*Clay_Rao + 
        + TWP*TWI_Rao + LP*Landform_sha
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha + AS*BIO1_Annual_Mean_Temperature
        
        Clay_Rao ~ TC*Top_Rao + APC*BIO12_Annual_Precipitation + ATC*BIO1_Annual_Mean_Temperature + LC*Landform_sha  + IC*intensity_ord_N
        
        TWI_Rao ~ TTW*Top_Rao + LTW*Landform_sha + APTW*BIO12_Annual_Precipitation 
        
        Silt_Rao ~ LSi*Landform_sha + TSi*Top_Rao
        
        Cec_Rao ~ SCe*Sand_Rao + ATCe*BIO1_Annual_Mean_Temperature + APCe*BIO12_Annual_Precipitation + 
        SiCe*Silt_Rao + TCe*Top_Rao + PCe*pH_Rao
        
       # Covariances
          Soc_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao
          
          
       # Derived quantities
          # Topography
            Top_via_C := TSC*SCR
            Top_via_sand := TS*SR
            Top_via_clay := TC*CR
            Top_via_TWI := TTW*TWR
            Top_via_N := TN*NR
            Top_via_CeC := TCe*CeR
            Top_total_indirect := TSC*SCR + TS*SR + TC*CR + TTW*TWR + TN*NR + TCe*CeR
            
            # Landform
            LF_via_sand := LS*SR
            LF_via_clay := LC*CR
            LF_via_pH := LP*PR
            LF_via_TWI := LTW*TWR
            LF_total_indirect := LS*SR + LC*CR + LP*PR + LTW*TWR
            
            # TWI
            TWI_via_pH := TWP*PR
            TWI_via_N := TWN*NR
            TWI_total_indirect := TWP*PR + TWN*NR
            
            # Temperature
            Temp_via_C := ATSC*SCR
            Temp_via_pH := ATP*PR
            Temp_via_clay := ATC*CR
            Temp_via_CeC := ATCe*CeR
            Temp_via_Sand := AS*SR
            Temp_total_indirect := ATSC*SCR + ATP*PR + ATC*CR + ATCe*CeR + AS*SR
            
            # Precipitation
            Prec_via_N := APN*NR
            Prec_via_C := APSC*SCR
            Prec_via_pH := APP*PR
            Prec_via_clay := APC*CR
            APCe_via_CeR := APCe*CeR
            APTW_via_TWR := APTW*TWR
            Precipitation_total_indirect := APN*NR + APSC*SCR + APP*PR + APC*CR + APCe*CeR + APTW*TWR
            
            # Cation Exchange capactity
            CeC_via_N := CeN*NR
            CeC_total_indirect := CeN*NR
            
            # Clay
            Clay_via_N := CN*NR
            Clay_via_pH := CP*PR
            Clay_total_indirect := CN*NR + CP*PR
            
            # Silt
            Silt_via_pH := SiP*PR
            Silt_via_CeC := SiCe*CeR
            Silt_total_indirect := SiP*PR + SiCe*CeR
            
            # Sand
            Sand_via_CeC := SCe*CeR
            Sand_via_SoC := SSC*SCR
            Sand_total_indirect := SCe*CeR + SSC*SCR
            
            # Organic Carbon
            SoC_via_N := SCN*NR
            SoC_total_indirect := SCN*NR
            
            # pH
            pH_via_N := PN*NR
            pH_via_CeC := PCe*CeR
            pH_total_indirect := PN*NR + PCe*CeR
            
            # Land use intensity
            LI_via_C := SCI*SCR
            LI_via_Clay := IC*CR
            LI_via_N := IN*NR
            LI_total_indirect := SCI*SCR + IC*CR +IN*NR
            
        # Total effects
            Total_direct := TR + TWR + NR + CeR + SCR + PR + ATR + APR + LR + CR + IR + SR + SiR
            Total_indirect := LI_total_indirect + pH_total_indirect + SoC_total_indirect + Sand_total_indirect + Silt_total_indirect +
            Clay_total_indirect + CeC_total_indirect + Precipitation_total_indirect + Temp_total_indirect + TWI_total_indirect + 
            LF_total_indirect + Top_total_indirect
"

model_100_Div = 
  " # Regressions
        F.div ~ TR*Top_Rao + TWR*TWI_Rao + NR*N_Rao + CeR*Cec_Rao + SCR*Soc_Rao + PR*pH_Rao + ATR*BIO1_Annual_Mean_Temperature +
        APR*BIO12_Annual_Precipitation + LR*Landform_sha + CR*Clay_Rao + IR*intensity_ord_N + SR*Sand_Rao + SiR*Silt_Rao
        
        N_Rao ~ APN*BIO12_Annual_Precipitation + CeN*Cec_Rao + CN*Clay_Rao + SCN*Soc_Rao + PN*pH_Rao + TN*Top_Rao + TWN*TWI_Rao +
        IN*intensity_ord_N
        
        Soc_Rao ~ TSC*Top_Rao + ATSC*BIO1_Annual_Mean_Temperature + APSC*BIO12_Annual_Precipitation + 
        SCI*intensity_ord_N + SSC*Sand_Rao
        
        pH_Rao ~ ATP*BIO1_Annual_Mean_Temperature  + APP*BIO12_Annual_Precipitation + SiP*Silt_Rao + CP*Clay_Rao + 
        + TWP*TWI_Rao + LP*Landform_sha
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha + AS*BIO1_Annual_Mean_Temperature
        
        Clay_Rao ~ TC*Top_Rao + APC*BIO12_Annual_Precipitation + ATC*BIO1_Annual_Mean_Temperature + LC*Landform_sha  + IC*intensity_ord_N
        
        TWI_Rao ~ TTW*Top_Rao + LTW*Landform_sha + APTW*BIO12_Annual_Precipitation 
        
        Silt_Rao ~ LSi*Landform_sha + TSi*Top_Rao
        
        Cec_Rao ~ SCe*Sand_Rao + ATCe*BIO1_Annual_Mean_Temperature + APCe*BIO12_Annual_Precipitation + 
        SiCe*Silt_Rao + TCe*Top_Rao + PCe*pH_Rao
        
       # Covariances
          Soc_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao
          
          
       # Derived quantities
          # Topography
            Top_via_C := TSC*SCR
            Top_via_sand := TS*SR
            Top_via_clay := TC*CR
            Top_via_TWI := TTW*TWR
            Top_via_N := TN*NR
            Top_via_CeC := TCe*CeR
            Top_total_indirect := TSC*SCR + TS*SR + TC*CR + TTW*TWR + TN*NR + TCe*CeR
            
            # Landform
            LF_via_sand := LS*SR
            LF_via_clay := LC*CR
            LF_via_pH := LP*PR
            LF_via_TWI := LTW*TWR
            LF_total_indirect := LS*SR + LC*CR + LP*PR + LTW*TWR
            
            # TWI
            TWI_via_pH := TWP*PR
            TWI_via_N := TWN*NR
            TWI_total_indirect := TWP*PR + TWN*NR
            
            # Temperature
            Temp_via_C := ATSC*SCR
            Temp_via_pH := ATP*PR
            Temp_via_clay := ATC*CR
            Temp_via_CeC := ATCe*CeR
            Temp_via_Sand := AS*SR
            Temp_total_indirect := ATSC*SCR + ATP*PR + ATC*CR + ATCe*CeR + AS*SR
            
            # Precipitation
            Prec_via_N := APN*NR
            Prec_via_C := APSC*SCR
            Prec_via_pH := APP*PR
            Prec_via_clay := APC*CR
            APCe_via_CeR := APCe*CeR
            APTW_via_TWR := APTW*TWR
            Precipitation_total_indirect := APN*NR + APSC*SCR + APP*PR + APC*CR + APCe*CeR + APTW*TWR
            
            # Cation Exchange capactity
            CeC_via_N := CeN*NR
            CeC_total_indirect := CeN*NR
            
            # Clay
            Clay_via_N := CN*NR
            Clay_via_pH := CP*PR
            Clay_total_indirect := CN*NR + CP*PR
            
            # Silt
            Silt_via_pH := SiP*PR
            Silt_via_CeC := SiCe*CeR
            Silt_total_indirect := SiP*PR + SiCe*CeR
            
            # Sand
            Sand_via_CeC := SCe*CeR
            Sand_via_SoC := SSC*SCR
            Sand_total_indirect := SCe*CeR + SSC*SCR
            
            # Organic Carbon
            SoC_via_N := SCN*NR
            SoC_total_indirect := SCN*NR
            
            # pH
            pH_via_N := PN*NR
            pH_via_CeC := PCe*CeR
            pH_total_indirect := PN*NR + PCe*CeR
            
            # Land use intensity
            LI_via_C := SCI*SCR
            LI_via_Clay := IC*CR
            LI_via_N := IN*NR
            LI_total_indirect := SCI*SCR + IC*CR +IN*NR
            
        # Total effects
            Total_direct := TR + TWR + NR + CeR + SCR + PR + ATR + APR + LR + CR + IR + SR + SiR
            Total_indirect := LI_total_indirect + pH_total_indirect + SoC_total_indirect + Sand_total_indirect + Silt_total_indirect +
            Clay_total_indirect + CeC_total_indirect + Precipitation_total_indirect + Temp_total_indirect + TWI_total_indirect + 
            LF_total_indirect + Top_total_indirect
"

model_100_Rich_simp = 
  " # Regressions
        F.rich ~ Top_Rao + TWI_Rao + N_Rao + Cec_Rao + Soc_Rao + pH_Rao + BIO1_Annual_Mean_Temperature +
        BIO12_Annual_Precipitation + Landform_sha + Clay_Rao + intensity_ord_N + Sand_Rao + Silt_Rao
        
        N_Rao ~ BIO12_Annual_Precipitation + Cec_Rao + Clay_Rao + Soc_Rao + pH_Rao + Top_Rao + TWI_Rao +
        intensity_ord_N
        
        Soc_Rao ~ Top_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + 
        intensity_ord_N + Sand_Rao
        
        pH_Rao ~ BIO1_Annual_Mean_Temperature  + BIO12_Annual_Precipitation + Silt_Rao + Clay_Rao + 
        + TWI_Rao + Landform_sha
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha+ AS*BIO1_Annual_Mean_Temperature
        
        Clay_Rao ~ Top_Rao + BIO12_Annual_Precipitation + BIO1_Annual_Mean_Temperature + Landform_sha  + intensity_ord_N
        
        TWI_Rao ~ Top_Rao + Landform_sha + BIO12_Annual_Precipitation 
        
        Silt_Rao ~ Landform_sha + Top_Rao
        
        Cec_Rao ~ Sand_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + 
        Silt_Rao + Top_Rao + pH_Rao
        
       # Covariances
          Soc_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao"

model_100_Eve_simp = 
  " # Regressions
        F.eve ~ Top_Rao + TWI_Rao + N_Rao + Cec_Rao + Soc_Rao + pH_Rao + BIO1_Annual_Mean_Temperature +
        BIO12_Annual_Precipitation + Landform_sha + Clay_Rao + intensity_ord_N + Sand_Rao + Silt_Rao
        
        N_Rao ~ BIO12_Annual_Precipitation + Cec_Rao + Clay_Rao + Soc_Rao + pH_Rao + Top_Rao + TWI_Rao +
        intensity_ord_N
        
        Soc_Rao ~ Top_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + 
        intensity_ord_N + Sand_Rao
        
        pH_Rao ~ BIO1_Annual_Mean_Temperature  + BIO12_Annual_Precipitation + Silt_Rao + Clay_Rao + 
        + TWI_Rao + Landform_sha
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha+ AS*BIO1_Annual_Mean_Temperature
        
        Clay_Rao ~ Top_Rao + BIO12_Annual_Precipitation + BIO1_Annual_Mean_Temperature + Landform_sha  + intensity_ord_N
        
        TWI_Rao ~ Top_Rao + Landform_sha + BIO12_Annual_Precipitation 
        
        Silt_Rao ~ Landform_sha + Top_Rao
        
        Cec_Rao ~ Sand_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + 
        Silt_Rao + Top_Rao + pH_Rao
        
       # Covariances
          Soc_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao"


model_100_Div_simp = 
  " # Regressions
        F.div ~ Top_Rao + TWI_Rao + N_Rao + Cec_Rao + Soc_Rao + pH_Rao + BIO1_Annual_Mean_Temperature +
        BIO12_Annual_Precipitation + Landform_sha + Clay_Rao + intensity_ord_N + Sand_Rao + Silt_Rao
        
        N_Rao ~ BIO12_Annual_Precipitation + Cec_Rao + Clay_Rao + Soc_Rao + pH_Rao + Top_Rao + TWI_Rao +
        intensity_ord_N
        
        Soc_Rao ~ Top_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + 
        intensity_ord_N + Sand_Rao
        
        pH_Rao ~ BIO1_Annual_Mean_Temperature  + BIO12_Annual_Precipitation + Silt_Rao + Clay_Rao + 
        + TWI_Rao + Landform_sha
        
        Sand_Rao ~ TS*Top_Rao + LS*Landform_sha+ AS*BIO1_Annual_Mean_Temperature
        
        Clay_Rao ~ Top_Rao + BIO12_Annual_Precipitation + BIO1_Annual_Mean_Temperature + Landform_sha  + intensity_ord_N
        
        TWI_Rao ~ Top_Rao + Landform_sha + BIO12_Annual_Precipitation 
        
        Silt_Rao ~ Landform_sha + Top_Rao
        
        Cec_Rao ~ Sand_Rao + BIO1_Annual_Mean_Temperature + BIO12_Annual_Precipitation + 
        Silt_Rao + Top_Rao + pH_Rao
        
       # Covariances
          Soc_Rao ~~ pH_Rao 
          Silt_Rao ~~ Sand_Rao + Clay_Rao 
          Clay_Rao ~~ Sand_Rao"