#' DSP_QC
#'
#'
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
#'
#'
#'
#'

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


#' {FOV=75, BD=c(0.1,2.25), SF=c(0.3,3), Min_nuc = 200, Min_Area = 16000}

#'
#'
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

#'
#'
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

