
#nDSPA: VVVVVVVVVVVVVVVVVVVVVVVVVVV

#==========Dependency Checks==================================
  #packrat method

  #source("dependencies.R")
  # load all packages
  #lapply(required_packages, require, character.only = TRUE)


if (.Platform$OS.type == "windows"){
  assign("t.flag", TRUE, envir = .GlobalEnv)
} else{ 
  assign("t.flag", FALSE, envir = .GlobalEnv)

}





#==========General Functions==================================

  
  #========Quick Operations================================
  
  #Convert factor/character/numeric to numeric
  to.numeric <- function(x) as.numeric(as.character(x))  
  
  #Geomean calculated by log(array) -> mean(array) -> exp(val)
  gmean <- function(x,method="log") {
    if (method == "log"){
      #Safer method does not produce overflows
      gm <- exp(mean(log(x)))
      
    }else if (method == "mult"){
      gm <- prod(x)^(1/len(x))
    }
    return(gm)
  }


  
  #========Image Functions=================================
  
  #========Blank ggplot=================================
  #Blank ggplot for when no data present
  blank_Plot <- function(label='No Data'){
    ggplot() +
      theme_void() +
      geom_text(aes(0,0,label=label)) +
      xlab(NULL)
  }


#==========nDSPA Specific Functions===========================

  #Note Will be replaced with import nDSPA package
  
  
  
  #========nDSPA ERCC Scale Factor=========================
    #Creates Scaling factor for QC
  ERCC_Scale_factor <- function(df_probe, df_val_all){
    ERCC_Probes <- df_probe$`ProbeName (display name)`[df_probe$CodeClass == "Positive" & df_probe$`Analyte type` == "SpikeIn"]
    
    #check before here that input values are data.matrix with numeric data
    PosCtrl_mat <- df_val_all[ERCC_Probes,]
    
    
    #Need trycatch handling
    if (is.null(dim(PosCtrl_mat))){
      if(length(PosCtrl_mat)==0){
        print("There are no ERCC_Probes") #Change to error condition
      }else if (is.numeric(PosCtrl_mat)){
        normfactors <- PosCtrl_mat 
      }else{
        mode(PosCtrl_mat) <- "numeric"
        normfactors <- PosCtrl_mat
      }
    }else{
      normfactors <- PosCtrl_mat %>% 
        apply(2, as.numeric) %>%
        apply(2, log) %>%
        apply(2, mean) %>%
        exp()
    }
    
    scalefactor <- mean(normfactors)/normfactors
    
    return(scalefactor)
  }
  
  #========nDSPA filter IDs by positive control Scaling====
    #returns names of IDs which pass filter check
  filter_IDs_PCF <- function(anno, scalefactor, PCF_min=0.3, PCF_max=3.0){
    filtered_scalefactor <- scalefactor[scalefactor >= PCF_min & scalefactor<= PCF_max]
    
    filtred_out_by_posctrl <- names(scalefactor[scalefactor <= PCF_min & scalefactor>= PCF_max])
    if(length(filtred_out_by_posctrl) == 0){
      cat("No Samples filtred out by Positive Control Factor \n")
    }else{
      cat(paste0("Failed Positive Control Factor: \n", paste(names(scalefactor[scalefactor >= PCF_min & scalefactor<= PCF_max]),collapse = ", "),"\n"))
    }
    return(names(filtered_scalefactor))
  }

    
  #========nDSPA QC Function===============================

  DSP_QC <- function(anno, val_all_df, scalefactor, thresh_filt=FALSE, PCF_filt=FALSE){
      if (!isFALSE(thresh_filt)){
          anno <- anno %>% filter(ID %in% thresh_filt)
      }
      if (!isFALSE(PCF_filt)){
          anno <- anno %>% filter(ID %in% PCF_filt)
      }

      sf <- scalefactor[anno$ID]
      vadf <- val_all_df[,anno$ID]
      if (all.equal(names(scalefactor), colnames(val_all_df))){
          QCdf <- t(t(vadf)*sf)
      }else{
          cat("Scale values not in filtered data: /n")
          print(names(scalefactor[!names(scalefactor) %in% anno$ID]))

          QCdf <- t(t(vadf)*sf)
      }
      return(QCdf) ##<< this is the DF not the anno
  }

  
  
  #========nDSPA Scaling Function==========================
  DSP_Scale <- function(df_anno, mat_val_all,method="area"){
    if (!tolower(method) %in% c("area","nuclei")){
      cat("Error in method: Must be area or nuclei\n")
    } else if(method=="area") {
      geomean_scale_area <- (df_anno$`AOI surface area` %>% as.character() %>% as.numeric() %>% gmean())/(df_anno$`AOI surface area` %>% as.character() %>% as.numeric())
      scaled_df <- t(t(mat_val_all)*geomean_scale_area)
    } else {
      geomean_scale_nuclei <- (df_anno$`AOI nuclei count` %>% as.character() %>% as.numeric() %>% gmean())/(df_anno$`AOI nuclei count` %>% as.character() %>% as.numeric())
      scaled_df <- t(t(mat_val_all)*geomean_scale_nuclei)
    }
    return(scaled_df)
  }
  
  #========nDSPA Normalization Function====================
  DSP_normalization <- function(df_probe,mat_val_all,method="geomean", probes="all"){
    if (probes == "all") {
      Controls <- df_probe$`ProbeName (display name)`[df_probe$CodeClass == "Control" & df_probe$`Analyte type` == "RNA"]
    } else {
      #check if probes selected in probe list
      ctrl_set <- df_probe$`ProbeName (display name)`[df_probe$CodeClass == "Control" & df_probe$`Analyte type` == "RNA"]
      not_in_set <- probes[!(probes %in% ctrl_set)]
      cat("Probes not in control probe set: \n")
      print(not_in_set)
      if (all(probes %in% ctrl_set)) {
        Controls <- probes
      } else {
        cat("Not all probes within controls \n")
        Controls <- probes
      }
    }
    Ctrl_mat <- mat_val_all[Controls,]
    
    if (method=="geomean") {
      if (is.null(dim(Ctrl_mat))){
        if(length(Ctrl_mat)==0){
          print("There are no Control Probes") #Change to error condition
        }else if (is.numeric(Ctrl_mat)){
          normfactors <- Ctrl_mat
        }else{
          mode(Ctrl_mat) <- "numeric"
          normfactors <- Ctrl_mat
        }
      }else{
        normfactors <- Ctrl_mat %>%
          apply(2, as.numeric) %>%
          apply(2, log) %>%
          apply(2, mean) %>%
          exp()
      }
      
      scalefactor <- mean(normfactors)/normfactors
    }
    
    if (method=="mean") {
      if (is.null(dim(Ctrl_mat))){
        if(length(Ctrl_mat)==0){
          print("There are no Control Probes") #Change to error condition
        }else if (is.numeric(Ctrl_mat)){
          normfactors <- Ctrl_mat
        }else{
          mode(Ctrl_mat) <- "numeric"
          normfactors <- Ctrl_mat
        }
      }else{
        normfactors <- Ctrl_mat %>%
          apply(2, as.numeric) %>%
          apply(2, mean)
      }
      
      scalefactor <- mean(normfactors)/normfactors
    }
    norm_mat <- t(t(mat_val_all)*scalefactor)
    
    return(norm_mat)
  }
  
  #========nDSPA SNR Function==============================
  DSP_SNR <- function(df_probe,mat_val_all,method="geomean", probes = "all"){
      if (probes == "all") {
          Isotype <- df_probe$`ProbeName (display name)`[df_probe$CodeClass == "Negative" & df_probe$`Analyte type` == "RNA"]
      } else {
          #check if probes selected in probe list
          iso_set <- df_probe$`ProbeName (display name)`[df_probe$CodeClass == "Negative" & df_probe$`Analyte type` == "RNA"]
          not_in_set <- probes[!(probes %in% iso_set)]
          cat("Probes not in Isotype probe set: \n")
          print(not_in_set)
          if (all(probes %in% iso_set)) {
              Isotype <- probes
          } else {
              cat("Not all probes within controls \n")
              Isotype <- probes
          }
      }
      Iso_mat <- mat_val_all[Isotype,]

      if (method=="geomean") {
          if (is.null(dim(Iso_mat))){
              if(length(Iso_mat)==0){
                  print("There are no Control Probes") #Change to error condition
              }else if (is.numeric(Iso_mat)){
                  normfactors <- Iso_mat
              }else{
                  mode(Iso_mat) <- "numeric"
                  normfactors <- Iso_mat
              }
          }else{
              normfactors <- Iso_mat %>%
                  apply(2, as.numeric) %>%
                  apply(2, log) %>%
                  apply(2, mean) %>%
                  exp()
          }


      }
      
      if (method=="mean") {
        if (is.null(dim(Iso_mat))){
          if(length(Iso_mat)==0){
            print("There are no Control Probes") #Change to error condition
          }else if (is.numeric(Iso_mat)){
            normfactors <- Iso_mat
          }else{
            mode(Iso_mat) <- "numeric"
            normfactors <- Iso_mat
          }
        }else{
          normfactors <- Iso_mat %>%
            apply(2, as.numeric) %>%
            apply(2, mean)
        }
        
        
      }
      norm_mat <- t(t(mat_val_all)*(1/normfactors))

      return(norm_mat)
  }
  
 
#==========Reactable Color Funcs==============================
  
  
  #========Anno Filtered Color Funcs=======================
    #========Scale Area Colors==========================
  SA_color <- function(value) {
    # normalized <- (value - min(data$Petal.Length)) / (max(data$Petal.Length) - min(data$Petal.Length))
    # color <- orange_pal(normalized)
    # list(background = color)
    value <- to.numeric(value)
    SA_color_func <- function(x){
      ifelse(x <= to.numeric(input$Min_Area), rgb(255,0,0,maxColorValue = 255),rgb(255,255,255,maxColorValue = 255))
    }
    color <- SA_color_func(value)
    
    return( list(background = color) )
    
  }
    
    #========Nuclei Count Colors========================
  NC_color <- function(value) {
    # normalized <- (value - min(data$Petal.Length)) / (max(data$Petal.Length) - min(data$Petal.Length))
    # color <- orange_pal(normalized)
    # list(background = color)
    value <- to.numeric(value)
    NC_color_func <- function(x){
      xx <- ifelse(x <= to.numeric(input$Min_Nuc), rgb(255,0,0,maxColorValue = 255),rgb(255,255,255,maxColorValue = 255))
      return(xx)
    }
    color <- NC_color_func(value)
    
    return( list(background = color) )
    
  }