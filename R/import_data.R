#' Import Data
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

import_DSP <- function(Data ,type="TSV"){

  if (type == "TSV"){
    #Imp function/test data type
    df <- readr::read_tsv(file = Data, col_names = TRUE)
  } else if (type == "Excel"){
    df <- readxl::read_excel(path = Data, col_names = TRUE)
  } else if (type == "Data"){
    #import_data
    df <- Data
  } else {
    errorCondition("Selected data type not available.")
  }

  anno <- cut_anno(df)
  probes <- cut_probes(df)
  val.All <- cut_vals(df,anno,probes)

  se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts=val.All),
                                                   rowData = probes,
                                                   colData = anno)

  return(se)
}

cut_anno <- function(df){

  anno <- df %>%
    dplyr::slice(1:which(df$`Segment displayed name` == "#Probe Group")-1) %>%
    tibble::column_to_rownames(var="Segment displayed name") %>%
    dplyr::select(4:dim(.)[2]) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    t()  %>%
    as.data.frame(drop=FALSE) %>%
    dplyr::mutate(Original_ID = rownames(.)) %>%
    dplyr::mutate(ID=gsub(" ", "",paste(.data$`Scan name`,.$`ROI (label)`,.$`Segment tags`,sep = "|")) )

  return(anno)
}

cut_probes <- function(df){

  probes <- df %>%
    dplyr::slice(-(1:which(df$`Segment displayed name` == "#Probe Group")-1)) %>%
    dplyr::select((1:4)) %>%
    `colnames<-`(.[1,]) %>%
    dplyr::slice(-1)

  return(probes)
}

cut_vals <- function(df, anno, probes){
  val <- df %>%
    dplyr::slice(-(1:which(df$`Segment displayed name` == "#Probe Group"))) %>%
    select(-(1:4)) %>%
    `colnames<-`(anno$ID) %>%
    apply(2,to.numeric) %>%
    data.matrix() %>%
    `rownames<-`(probes$`ProbeName (display name)`)

  return(val)
}
