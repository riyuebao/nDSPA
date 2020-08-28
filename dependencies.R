
#nDSPA Dependencies

#-------------ISNTALL PKGS----------------#


required_packages <- c(
  'conflicted',
  'lmerTest',
  'emmeans',
  'reshape2',
  'multcomp',
  'viridis',
  'ggpubr',
  'ggsci',
  'RColorBrewer',
  'circlize',
  'ComplexHeatmap',
  'ggrepel',
  'patchwork',
  'shiny',
  'shinyjs',
  'DT',
  'readr',
  'dplyr',
  'tidyr',
  'shinydashboard',
  'plotly',
  'reactable',
  'shinyhelper',
  'shinyWidgets',
  'shinyBS',
  'grid',
  'magick',
  'GGally',
  'factoextra',
  'heatmaply'
)
# install missing packages

new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

rm(new.packages)

#-------------STAT MODULE DEPS----------------#

##Source Libraries
# library(checkpoint)
# checkpoint(snapshotDate ='2020-08-20')


##Confliced to resolve select issue
#install.packages("conflicted")


library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("box", "shinydashboard")
conflict_prefer("dataTableOutput", "DT")
conflict_prefer("filter", "dplyr")
conflict_prefer("lmer", "lmerTest")

#Fix for repo issue on Shinyapps.io
library(BiocManager)
options(repos = BiocManager::repositories())

#-------------STAT MODULE DEPS----------------#

#library(ggplot2)
library(lmerTest)
library(emmeans)
library(reshape2)
library(multcomp)
library(viridis)
library(ggpubr)
library(ggsci)
library(RColorBrewer)
library(circlize)
library(ComplexHeatmap)
library(ggrepel)
library(patchwork)




#------------Main Module Deps -----------------#

library(shiny)
library(shinyjs)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(plotly)
library(reactable)
library(shinyhelper)
library(shinyWidgets)
library(shinyBS)
library(grid)
library(magick)
library(GGally)
library(factoextra)
library(heatmaply)

