#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


    #----Source Dependencies----#
source('dependencies.R')
source('global.R') #Do not need to source if splitting into server.R and ui.R

    #----Dashboard Page ----#
ui <- dashboardPage(
    skin="purple",
    title = "nDSPA",
    
    #----HEADER----#
    dashboardHeader(title = span(img(src = "logo.png", height = 50), "nDSPA")
                    ),
    
    #----SIDEBAR----#
    dashboardSidebar(
        sidebarMenu(id = "dashmenu",
            menuItem("QC and Normalization", tabName = "Import", icon = icon("file-import")),
            #menuItem("Import and Normalize Data", icon = icon("file-import"),
                     #menuSubItem("Import Data", tabName = "Import"),
                     #menuSubItem("Data Plots",tabName = "Data_Plots")),
            #menuItem("Expression Map",tabName = "Map",icon = icon("image")),
            #menuItem("Run-To-Run Comparisons",tabName = "Run2run",icon = icon("clipboard-check")),
            #####_Currently Disabled_#####
            menuItem("Statistical Analysis",tabName = "StatAnalysis", icon = icon("chart-bar")),
            hr(),
            menuItem("About",tabName = "about",icon = icon("id-card"))
        )
    ),
    
    #----BODY----#
    dashboardBody(
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "body_style.css")
        ),
        
        useShinyjs(),
        
        tabItems(
            #----SINGLE DATA MENU----#
            tabItem(tabName = "Import",
                fluidRow(
                    column(width = 12,
                           
                           
                           bsButton(inputId = "import_btn",
                                    label = "Import Data",
                                    style = "success"
                           ),
                           
                           bsButton(inputId = "plot_btn",
                                    label = "Data Plots",
                                    style = "default",
                                    disabled = TRUE
                                      
                           ), #On data import, update button to disabled=FALSE
                           
                           bsButton(inputId = "map_btn",
                                    label = "Expression Map",
                                    style = "default",
                                    disabled = TRUE
                                      
                           )#,
                           # bsButton(inputId = "run2run_btn",
                           #          label = "Run-to-Run Correlation",
                           #          style = "default",
                           #          disabled = TRUE
                           # )
                           
                    )
                ),
                    
                    
                    
                    
                    
                    
                #----FluidRow for Import opts----#    
                fluidRow(
                    
                    #----DIV for Import Options----#
                    div(id="Import_opts",
                        
                    box(title = "nDSP tsv data", width = 12,
                        #----Inputer with help for Data----#
                    fileInput(inputId ="inputfile",
                                label = "Choose the raw data file in the scale of choice",
                                accept=c(".csv",".xlsx",".txt")
                              ) %>% helper(type = "markdown",
                                           content = "Import"
                                           ),
                    
                
                    
                        #----Material Switch----#
                    materialSwitch(inputId = "QCorNot",
                                   label = "QC & Filter", 
                                   value = FALSE,
                                   right = TRUE),
                    materialSwitch(inputId = "SCALENORM_flag",
                                   label = "Scale & Normalize", 
                                   value=FALSE, 
                                   right = TRUE),
                    
                    
                    fileInput(inputId = "inputROIclass",
                              label = "Input Metadata Table",
                              accept = c(".txt")
                              )
                    #htmlOutput("select_roiClass")
                        ),
                    #Matrix plots with tabs
                    div(id = "QCbox",
                        column(width = 12,
                             tabBox(selected = "QC Filters",
                                    width = 12,
                                    tabPanel(
                                        title = "QC Filter Options",
                                        value = "QC_Filter_box",
                                        fluidRow(
                                            column(width = 4,
                                                   uiOutput("Scan_Sel_prefilt1"),
                                                   uiOutput("QCopt"),
                                                   textOutput("filtered_n")
                                            ),
                                            column(width = 7,
                                                   dataTableOutput("filtered_samples")
                                            )
                                        )
                                        
                                        
                                    ),
                                    tabPanel(
                                        title = "Filtered Data",
                                        value = "QC_Filtered_Data",
                                        style = "overflow-x: scroll;",
                                        #dataTableOutput(outputId = "anno_filtered")
                                        reactableOutput("anno_filtered")
                                        
                                    )
                                    
                                    
                             )
                        )
                             
                    ),
                    
                    div(id ="SCALENORMbox",
                             box(
                                 title = "Scale & Normalization Options",
                                 uiOutput("NORMopt"),width = 12
                             )
                    )
                    )#</div Import_opts>
                    
                    
                ), #</fluidrow Import Opts>
                
                
                #----Fluid Row Basic Plots----#
                fluidRow(
                    shinyjs::hidden(
                    #===DIV Basic Plots----#
                        
                        #--Notes for sizing
                        #https://stackoverflow.com/questions/53519783/set-minimum-maximum-width-for-box-with-shinydashboard-r
                        #https://github.com/rstudio/shinydashboard/issues/270
                    div(id = "Data_plots",
                            fluidRow(
                                div(class = "col-sm-12 col-md-12 col-lg-6",
                                    tabBox(width = '100%',height = '100%',
                                        id = "matplots",
                                        selected = "PCA Probes",
                                        tabPanel("PCA Probes",
                                                 plotlyOutput(outputId = "pca_ind")
                                                 
                                        ),
                                        tabPanel("PCA Samples",
                                                 plotlyOutput(outputId = "pca_var")
                                                 
                                        ),
                                        tabPanel("Density",
                                                 #plotOutput(outputId = "density")
                                                 plotlyOutput(outputId = "density")
                                        ),
                                        tabPanel("Heatmap",
                                                 plotlyOutput(outputId = "mainHeat")
                                        ),
                                        tabPanel("BG v HK",
                                                 plotlyOutput(outputId = "IsovHK_plt")
                                        ),
                                        tabPanel("HK Corr",
                                                 plotlyOutput(outputId = "HK_Sct_mt")
                                        ),
                                        tabPanel("SNR Levels",
                                                 plotlyOutput(outputId = "SNR_boxplot")
                                        )
                                        
                                        
                                    )
                                )
                            ),
                            fluidRow(
                                tabBox(
                                    selected = "Annotations",
                                    width = 12,
                                    tabPanel("Annotations",
                                             style = "overflow-x: scroll;",
                                             dataTableOutput(outputId = "anno")
                                    ),
                                    tabPanel("All Values", 
                                             style = "overflow-x: scroll;",
                                             dataTableOutput(outputId = "val_all")
                                    ),
                                    tabPanel("Data Matrix",
                                             style = "overflow-x: scroll;",
                                             dataTableOutput(outputId = "val_Endo")
                                    ),
                                    tabPanel("Probes",
                                             style = "overflow-x: scroll;",
                                             dataTableOutput(outputId = "probes")
                                    )
                                    
                                    
                                )
                            )
                            
                    ) #</DIV data plots>
                    ) #</hidden>    
                        
                        
                    ), #</fluidRow for data plots>
                
                fluidRow(
                    shinyjs::hidden(
                    div(id="EXPR_map",
                            fluidRow(
                                dropdownButton(icon = icon("gear"),
                                               
                                        h3("Expression Map Selector"),
                                        fileInput("impImage", "Choose a Scan Image", accept = c('image/png', 'image/jpeg')) %>% 
                                            helper(type = "markdown",
                                                   content = "Imgmap"
                                                   ),
                                        htmlOutput("select_ImgScan"),
                                        htmlOutput("select_ImgProbes"),
                                        selectInput(inputId = "Img_Anno_Filt",
                                                    label = "Analysis Set",
                                                    choices = c("All ROI"="Immap_ROI_All",
                                                                "Filtered ROI Only" = "Immap_ROI_Filt"),
                                                    selected = "Immap_ROI_All"
                                        )
                                ),
                                
                                    uiOutput("map.ui")
                                
                            ),
                            #h2("Expression Map Contents")
                    )#</div for im map>
                    )#</hidden>
                ), #</fluidRow for Image Map>
                
                #----Run 2 Run fluidpage----#
                fluidRow(
                    shinyjs::hidden(
                        div(id = "Run2run",
                        ) #</DIV data plots>
                    ) #</hidden>    
                    
                    
                )
                
                ), #</tabItem Import>
            
            
            
            
            #---Statistical Analysis----#
            tabItem(tabName = "StatAnalysis",
                    
                    fluidRow(
                        div(id = "Stat_imports",
                            
                            box(title = "Import Data File", width = 12,
                                #----Inputer with help for Data----#
                                fileInput(inputId ="statgroupimport",
                                          label = "Import grouping file",
                                          accept=c(".csv",".xlsx",".txt")
                                ), #%>% helper(type = "markdown",
                                             #content = "Import"
                                #),
                                
                                
                                
                                #----Material Switch----#
                                # materialSwitch(inputId = "Lmem_low_exp_flag",
                                #                label = "Filter Low Expressed Probes?", 
                                #                value = FALSE,
                                #                right = TRUE),
                                
                                
                                selectInput(inputId = "Lmem_model",
                                            label = "Select Model Method",
                                            choices = c("lmer"="lmer"),
                                            selected = "lmer",
                                            multiple = FALSE,
                                            selectize = TRUE,
                                            width = NULL,
                                            size = NULL
                                ),
                                selectInput(inputId = "Lmem_test_method",
                                            label = "Test for Contrasts",
                                            choices = c("t Test"="t.test",
                                                        "Z Test"="z.test"),
                                            selected = "t.test",
                                            multiple = FALSE,
                                            selectize = TRUE,
                                            width = NULL,
                                            size = NULL
                                ),
                                
                                # selectInput(inputId = "Lmem_p_filt",
                                #             label = "P.Value filtering",
                                #             choices = c("Raw P Value"="p.value",
                                #                         "FDR"="fdr"),
                                #             selected = "p.value",
                                #             multiple = FALSE,
                                #             selectize = TRUE,
                                #             width = NULL,
                                #             size = NULL
                                # ),
                                selectInput(inputId = "Lmem_FE",
                                            label = "Select Fixed Effect",
                                            choices = c("Subject ID"="PID",
                                                        "group (classification variable)"="group"),
                                            selected = "group",
                                            multiple = FALSE,
                                            selectize = TRUE,
                                            width = NULL,
                                            size = NULL
                                ),
                                selectInput(inputId = "Lmem_RE",
                                            label = "Select Random Effect",
                                            choices = c("Subject ID"="PID",
                                                        "group (classification variable)"="group"),
                                            selected = "PID",
                                            multiple = FALSE,
                                            selectize = TRUE,
                                            width = NULL,
                                            size = NULL
                                ),
                                htmlOutput("stat_seg_select"),
                                htmlOutput("stat_g1_select"),
                                htmlOutput("stat_g2_select"),
                                actionButton("stat_go", "Run Stat Calculations") 
                                #htmlOutput("select_roiClass")
                            ),
                            column(width = 12,
                                   box(style = "overflow-x: scroll;", width = 12,
                                       dataTableOutput(outputId = "stat_df")
                                   )
                            ),
                            column(width = 12,
                                   box(style = "overflow-x: scroll;", width = 12,
                                       dataTableOutput(outputId = "data_stats")
                                   )
                            )
                            
                            
                        )
                        
                        
                    )
                    
            ),
            
            
            #----About----#
            
            tabItem(tabName = "about",
                    fluidRow(
                        box(width = 12,
                            column(12,verbatimTextOutput("cwd") ),
                            column(12,verbatimTextOutput("fileload") ),
                            column(12, verbatimTextOutput("session_info")) )
                        ),
                    fluidRow(includeHTML("docs/about.html"))
                    )
        ) #</tabItems Closed>
    ) #</Dashboard Body>
) #</Dashboard Page>
















server <- function(input, output, session) {
    #----Observe Helpers sets dir for help .md files and session to observe---#
    observe_helpers(session = session)
    
    #Insert Function or package to test types of input. Wrap in condition based on type.
    #Add another function to read lines and determine if it is a DSP data file
    
    #----Data Import Reactive Value----#
    rawdata <- reactive({
        if( is.null(input$inputfile) ) return(NULL)
        
        read_tsv(file=input$inputfile$datapath, trim_ws=TRUE)
        
        })
    class_data <- reactive({
        req(input$inputROIclass)
        df <- read_tsv(file=input$inputROIclass$datapath, trim_ws=TRUE)
        
        if (t.flag) View(df)
        
        return(df)
        
    })
    
    #----Enable buttons if data is loaded----#
    observe({
        if (!is.null(rawdata() )){
            updateButton(session = session,
                         inputId =  "plot_btn", 
                         disabled = FALSE)
            updateButton(session = session,
                         inputId =  "map_btn", 
                         disabled = FALSE)
            updateButton(session = session,
                         inputId =  "run2run_btn", 
                         disabled = FALSE)
            
            shinyjs::show(id = "QCorNot")
            shinyjs::show(id = "SCALENORM_flag")
            shinyjs::show(id = "inputROIclass")
        }else{
            updateButton(session = session,
                         inputId =  "plot_btn", 
                         disabled = TRUE)
            updateButton(session = session,
                         inputId =  "map_btn", 
                         disabled = TRUE)
            updateButton(session = session,
                         inputId =  "run2run_btn", 
                         disabled = TRUE)
            
            shinyjs::hide(id = "QCorNot")
            shinyjs::hide(id = "SCALENORM_flag")
            shinyjs::hide(id = "inputROIclass")
        }
    })
    #----Rendering for About Page data
    #----==Troubleshooting var for showing rundir of application----#
    # output$cwd <- renderText({
    #     print(paste0(getwd()))
    #     cat(paste("Current Working Directory: \n",getwd()))
    # })
    
    output$cwd <- renderPrint({
        
        cat(paste("Current Working Directory: \n",getwd()))
    })
    output$session_info <- renderPrint(sessionInfo())
    
    
    #Add ability on part 2 to modify names of runs
    # anno <- rawdata() %>% slice(1:17) %>% t() %>% `colnames<-`(.[1,]) %>% .[-c(1:4),] %>% 
    #     as.data.frame() %>%
    #     mutate(ID=gsub(" ", "",paste(Run,ROI_ID,`Segment tags`,sep = "|"))) %>% `rownames<-`(.[,"ID"])
    # 
    # probes <- rawdata() %>% .[-c(1:17),c(1:4)] %>% `colnames<-`(gsub("#","",.[1,])) %>% .[-1,] %>% as.data.frame() %>% `rownames<-`(.[,"ProbeName (display name)"])
    # values.all <- rawdata() %>% .[-c(1:18),-c(1:4)] %>% `colnames<-`(anno$ID) %>% 
    #     as.data.frame() %>% `rownames<-`(probes$`ProbeName (display name)`)
    # values.Endo <- values.all[rownames(values.all) %in% probes$`ProbeName (display name)`[probes$CodeClass == "Endogenous"],]
    # 
    
    #----Shows path of loaded data in single file run----#
    output$fileload <- renderText({
        if( is.null(rawdata()) ){
            print("No data loaded...")
        }else{ print(paste("Input File Path: \n",input$inputfile$datapath))}
    })
    
    observeEvent(input$QCorNot, {
        print(paste0("Value of QCorNot: ", input$QCorNot))
        if (input$QCorNot){
            shinyjs::show(id = "QCbox")
        }else{
            shinyjs::hide(id = "QCbox")
        }
    })
    
    observeEvent(input$SCALENORM_flag, {
        print(paste0("Value of SCALENORM_flag: ", input$SCALENORM_flag))
        if (input$SCALENORM_flag){
            shinyjs::show(id = "SCALENORMbox")
        }else{
            shinyjs::hide(id = "SCALENORMbox")
        }
    }) 
    
    #----Toggle Buttons Analysis Menu----#
    #---==Button Show Import Opts----#
    observeEvent(input$import_btn,{
        #Update Button State
        # observeEvent(btn) -> Toggle True/False on var
        # reactive if var true then
        #     hidedivs/showdivs
        #     changebuttoncolor
        
        
        updateButton(session = session,
                     inputId =  "import_btn", 
                     style = "success")
        updateButton(session = session,
                     inputId =  "plot_btn", 
                     style = "default")
        updateButton(session = session,
                     inputId =  "map_btn", 
                     style = "default")
        
        shinyjs::hide(id = "Data_plots",anim = TRUE)
        shinyjs::hide(id = "EXPR_map",anim = TRUE)
        shinyjs::hide(id = "Run2run",anim = TRUE)
        shinyjs::show(id = "Import_opts",anim = TRUE)
    })
    
    observeEvent(input$plot_btn,{
        #Update Button State
        # observeEvent(btn) -> Toggle True/False on var
        # reactive if var true then
        #     hidedivs/showdivs
        #     changebuttoncolor
        
        
        updateButton(session = session,
                     inputId =  "import_btn", 
                     style = "default")
        updateButton(session = session,
                     inputId =  "plot_btn", 
                     style = "success")
        updateButton(session = session,
                     inputId =  "map_btn", 
                     style = "default")
        
        shinyjs::hide(id = "EXPR_map",anim = TRUE)
        shinyjs::hide(id = "Import_opts",anim = TRUE)
        shinyjs::hide(id = "Run2run",anim = TRUE)
        shinyjs::show(id = "Data_plots",anim = TRUE)
    })
    
    observeEvent(input$map_btn,{
        #Update Button State
        # observeEvent(btn) -> Toggle True/False on var
        # reactive if var true then
        #     hidedivs/showdivs
        #     changebuttoncolor
        
        
        updateButton(session = session,
                     inputId =  "import_btn", 
                     style = "default")
        updateButton(session = session,
                     inputId =  "plot_btn", 
                     style = "default")
        updateButton(session = session,
                     inputId =  "map_btn", 
                     style = "success")
        
        shinyjs::hide(id = "Data_plots",anim = TRUE)
        shinyjs::hide(id = "Import_opts",anim = TRUE)
        shinyjs::hide(id = "Run2run",anim = TRUE)
        shinyjs::show(id = "EXPR_map",anim = TRUE)
    })
    
    observeEvent(input$run2run_btn,{
        #Update Button State
        # observeEvent(btn) -> Toggle True/False on var
        # reactive if var true then
        #     hidedivs/showdivs
        #     changebuttoncolor
        
        
        updateButton(session = session,
                     inputId =  "import_btn", 
                     style = "default")
        updateButton(session = session,
                     inputId =  "plot_btn", 
                     style = "default")
        updateButton(session = session,
                     inputId =  "map_btn", 
                     style = "success")
        
        shinyjs::hide(id = "Data_plots",anim = TRUE)
        shinyjs::hide(id = "Import_opts",anim = TRUE)
        shinyjs::hide(id = "EXPR_map",anim = TRUE)
        shinyjs::show(id = "Run2run",anim = TRUE)
    })
    
    
    # shinyjs::onclick(id = "import_btn",
    #                  #Change Button State to active!!!!
    # 
    #                  shinyjs::hide(id = "Data_plots",anim = TRUE)
    #                  shinyjs::hide(id = "EXPR_map",anim = TRUE)
    #                  shinyjs::show(id = "Import_opts",
    #                                anim = TRUE)
    #                  #shinyjs::hide()
    # 
    # )
    
    
    output$Scan_Sel_prefilt1 <- renderUI({
        req(rawdata())
        
        Scans <- rawdata() %>%
            slice(1:17) %>%
            t() %>%
            `colnames<-`(.[1,]) %>%
            .[-c(1:4),] %>%
            as.data.frame() %>%
            select(Scan_ID) %>% 
            unlist() %>%
            unique()
        
        str(unique(Scans))
        selectInput(inputId = "Scans_to_prefilt",
                    label = "Select Scans to Process",
                    choices = Scans, 
                    selected = Scans,
                    multiple = TRUE
                    )
        
        
    })
    
    observeEvent(input$Scans_to_prefilt, { 
        #cat(paste("Prefilt Scans:",input$Scans_to_prefilt,"\n", sep = " "))
        str(input$Scans_to_prefilt)
    })
    
    #----QC OPTIONS UI----#
    output$QCopt <- renderUI({
        req(rawdata())
        
        if (input$QCorNot){
            tagList(
                h4("Filter Data"),
                
                materialSwitch(
                    inputId = "Filt_switch",
                    value = TRUE
                ) %>% helper(type = "markdown",
                             content = "QC"
                ),
                
                div(id="QC_sliders",
                
                    br(),
                    br(),
                    
                    
                    numericInput(inputId = "FOV_Flag", 
                                 label = "Minimum FOV", 
                                 value = 75, 
                                 min = 0, max = 280, step = 1,
                                 width = NULL),
                    
                    sliderInput(inputId = "BD_Flag", 
                                label = "Binding Density Range", 
                                value = c(0.1,2.25), 
                                min = 0, max = 3, step = 0.01,
                                width = NULL),
                    
                    sliderInput(inputId = "SF_Flag", 
                                label = "Scaling Factor Range", 
                                value = c(0.3,3), 
                                min = 0, max = 5, step = 0.01,
                                width = NULL),
                    
                    #Check if nuclei data available before offering option
                    sliderInput(inputId = "Min_Nuc", 
                                label = "Minimum Nucleus Count in AOI", 
                                value = 200, 
                                min = 0, max = 500, step = 10,
                                width = NULL),
                    
                    sliderInput(inputId = "Min_Area", 
                                label = "Minimum Area of AOI", 
                                value = 16000, 
                                min = 0, max = 20000, step = 500,
                                width = NULL)
                )
            )
            
        } 
        
        
        
    })
    
    observe({
        
        if (!is.null(input$Filt_switch)){
            if (input$Filt_switch) {
                shinyjs::show(id = "QC_sliders")
            } else {
                shinyjs::hide(id = "QC_sliders")
            }  
        }
        
    })
    
    
    #----NORMALIZATION OPTIONS UI----#
    output$NORMopt <- renderUI({
        req(rawdata())
        req(probes())
        
        
        if (input$SCALENORM_flag){
            HK <- probes() %>%
                filter(`Analyte type` == "RNA" & CodeClass == "Control") %>% 
                select(`ProbeName (display name)`) %>% unlist() %>% unname()
            Iso <- probes() %>%
                filter(`Analyte type` == "RNA" & CodeClass == "Negative") %>% 
                select(`ProbeName (display name)`) %>% unlist() %>% unname()
            
            tagList(
                selectInput(inputId = "Scale_method",
                            label = "Scaling by area or nuclei?",
                            choices = c("Nuclei"="Nuclei",
                                        "Area"="Area",
                                        "None"="None"),
                            selected = "None",
                            multiple = FALSE,
                            selectize = TRUE,
                            width = NULL,
                            size = NULL
                ) %>% helper(type = "markdown",
                             content = "Normalization"
                             ), 
                #Change to only ask if method is Nuclei or Area
                selectInput(inputId = "Scale_calc",
                            label = "Calculation Method for Scaling",
                            choices = c("Geometric Mean"="geomean",
                                        "Mean"="mean"),
                            selected = NULL,
                            multiple = FALSE,
                            selectize = TRUE,
                            width = NULL,
                            size = NULL
                ),
                br(),
                selectInput(inputId = "Norm_SNR_select",
                            label = "Normalization Method",
                            choices = c("HouseKeeping Normalization"="HKnorm",
                                        "Signal-Noise-Ratio (SNR)"="SNR",
                                        "No Normalization"="none"),
                            selected = "none",
                            multiple = FALSE,
                            selectize = TRUE,
                            width = NULL,
                            size = NULL
                ),
                br(),
                div(id = "HKNORMopts",
                    selectInput(inputId = "HK_For_Norm",
                                label = "Select Housekeeping Probes for Normalization",
                                choices = HK, 
                                selected = HK,
                                multiple = TRUE
                    ),
                    selectInput(inputId = "Norm_calc",
                                label = "Calculation Method for HK Normalization",
                                choices = c("Geometric Mean"="geomean",
                                            "Mean"="mean",
                                            "No Normalization"="none"),
                                selected = "none",
                                multiple = FALSE,
                                selectize = TRUE,
                                width = NULL,
                                size = NULL
                    )
                    
                ),
                div(id = "SNRopts",
                    selectInput(inputId = "Iso_For_Norm",
                                label = "Select Background Negative Controls for SNR",
                                choices = Iso, 
                                selected = Iso,
                                multiple = TRUE
                    ),
                    selectInput(inputId = "SNR_calc",
                                label = "Calculation Method for SNR",
                                choices = c("Geometric Mean"="geomean",
                                            "Mean"="mean",
                                            "No Normalization"="none"),
                                selected = "none",
                                multiple = FALSE,
                                selectize = TRUE,
                                width = NULL,
                                size = NULL
                    )
                )
                 
                
            )
            
        } 

    })
    
    
    #----Observer to hide Normalization options not being utilized----#
    observe({
        if (input$SCALENORM_flag){
            if (!is.null(input$Norm_SNR_select)) {
                if (input$Norm_SNR_select == "none"){
                    
                    shinyjs::hide(id = "HKNORMopts")
                    shinyjs::hide(id = "SNRopts")
                    
                }else if (input$Norm_SNR_select == "HKnorm"){
                    
                    shinyjs::show(id = "HKNORMopts")
                    shinyjs::hide(id = "SNRopts")
                }else{
                    shinyjs::hide(id = "HKNORMopts")
                    shinyjs::show(id = "SNRopts")
                }
            }
            
        }
        
    })
    
    
    
    # output$FOV_flag <- renderText({
    #     print(input$FOV_Flag[1])
    # })
    observeEvent(input$FOV_Flag, { 
        cat(paste("FOV_Flag: ",input$FOV_Flag,"\n"))
    })
    #reactive({print(paste0("FOV_Flag: ",input$FOV_Flag))})
    
    
    
    
    
    #-----BUILDING DATA FOR REST OF APP FROM RAW DATA----#
    
    #Dynamically request Clasifiction Table
        #-Change to dynamic UI that asks for table or dynamically make in UI
    
    ##Annotation before any filtering
    anno_pre <- reactive({
        req(rawdata())
        
        anno <- rawdata() %>%
            slice(1:17) %>%
            t() %>%
            `colnames<-`(.[1,]) %>%
            .[-c(1:4),] %>%
            as.data.frame() %>%
            mutate(ID=gsub(" ", "",paste(Scan_ID,ROI_ID,`Segment tags`,sep = "|")) )
        
        anno
    })
    ##Probes before any filtering
    probes <- reactive({
        req(rawdata())
        probes <- rawdata() %>%
            .[-c(1:17),c(1:4)] %>% 
            `colnames<-`(gsub("#","",.[1,])) %>% 
            .[-1,] %>% as.data.frame() %>% 
            `rownames<-`(.[,"ProbeName (display name)"])
        probes
    })
    ##Only Endogenous probes. Change to use Probes() to filter
    probes_Endo <- reactive({
        req(rawdata())
        probes <- rawdata() %>%
            .[-c(1:17),c(1:4)] %>% 
            `colnames<-`(gsub("#","",.[1,])) %>% 
            .[-1,] %>% as.data.frame() %>% 
            `rownames<-`(.[,"ProbeName (display name)"]) %>%
            filter(CodeClass == "Endogenous")
        probes
    })
    ##RAW VALUE MATRIX. 
    val_all <- reactive({
        anno <- anno_pre()
        probes <- probes()
        values.all <- rawdata() %>% 
            .[-c(1:18),-c(1:4)] %>% 
            `colnames<-`(anno$ID) %>%
            as.data.frame() %>% 
            `rownames<-`(probes$`ProbeName (display name)`) %>%
            data.matrix()
        
        values.all
    })
    ##VALUES of all data after QC##
    val_all_QC <- reactive({
        #Can Add this transformation back into val_all instead and remove redundant data if we don't care about displaying raw values
        val_all <- val_all()
        anno <- anno_pre()
        probes <- probes()
        
        
        
        # DSP_QC <- function(anno, val_all_df, scalefactor, thresh_filt=FALSE, PCF_filt=FALSE){
        #     if (!isFALSE(thresh_filt)){
        #         anno <- anno %>% filter(ID %in% thresh_filt)
        #     }
        #     if (!isFALSE(PCF_filt)){
        #         anno <- anno %>% filter(ID %in% PCF_filt)
        #     }
        #     
        #     sf <- scalefactor[anno$ID]
        #     vadf <- val_all_df[,anno$ID]
        #     if (all.equal(names(scalefactor), colnames(val_all_df))){
        #         QCdf <- t(t(vadf)*sf)
        #     }else{
        #         cat("Scale values not in filtered data: /n")
        #         print(names(scalefactor[!names(scalefactor) %in% anno$ID]))
        #         
        #         QCdf <- t(t(vadf)*sf)
        #     }
        #     return(QCdf) ##<< this is the DF not the anno
        # }
        # 
        # ERCC_Scale_factor <- function(df_probe, df_val_all){
        #     ERCC_Probes <- df_probe$`ProbeName (display name)`[df_probe$CodeClass == "Positive" & df_probe$`Analyte type` == "SpikeIn"]
        #     
        #     #check before here that input values are data.matrix with numeric data
        #     PosCtrl_mat <- df_val_all[ERCC_Probes,]
        #     
        #     
        #     #Need trycatch handling
        #     if (is.null(dim(PosCtrl_mat))){
        #         if(length(PosCtrl_mat)==0){
        #             print("There are no ERCC_Probes") #Change to error condition
        #         }else if (is.numeric(PosCtrl_mat)){
        #             normfactors <- PosCtrl_mat 
        #         }else{
        #             mode(PosCtrl_mat) <- "numeric"
        #             normfactors <- PosCtrl_mat
        #         }
        #     }else{
        #         normfactors <- PosCtrl_mat %>% 
        #             apply(2, as.numeric) %>%
        #             apply(2, log) %>%
        #             apply(2, mean) %>%
        #             exp()
        #     }
        #     
        #     scalefactor <- mean(normfactors)/normfactors
        #     
        #     return(scalefactor)
        # }
        
        val_all_QC <- DSP_QC(anno = anno, val_all_df = val_all, scalefactor = ERCC_Scale_factor(df_probe = probes, df_val_all = val_all))
        
       
        
        val_all_QC
        
    })
    ##Scale factor: Used for filtering by data out of range of Positive Control Factor in QC
    ScaleFactor <- reactive({
        req(probes())
        req(val_all())
        
        # ERCC_Scale_factor <- function(df_probe, df_val_all){
        #     ERCC_Probes <- df_probe$`ProbeName (display name)`[df_probe$CodeClass == "Positive" & df_probe$`Analyte type` == "SpikeIn"]
        #     
        #     #check before here that input values are data.matrix with numeric data
        #     PosCtrl_mat <- df_val_all[ERCC_Probes,]
        #     
        #     
        #     #Need trycatch handling
        #     if (is.null(dim(PosCtrl_mat))){
        #         if(length(PosCtrl_mat)==0){
        #             print("There are no ERCC_Probes") #Change to error condition
        #         }else if (is.numeric(PosCtrl_mat)){
        #             normfactors <- PosCtrl_mat 
        #         }else{
        #             mode(PosCtrl_mat) <- "numeric"
        #             normfactors <- PosCtrl_mat
        #         }
        #     }else{
        #         normfactors <- PosCtrl_mat %>% 
        #             apply(2, as.numeric) %>%
        #             apply(2, log) %>%
        #             apply(2, mean) %>%
        #             exp()
        #     }
        #     
        #     scalefactor <- mean(normfactors)/normfactors
        #     
        #     return(scalefactor)
        # }
        
        ScaleFactor <- ERCC_Scale_factor(df_probe = probes(), df_val_all = val_all()) %>% as.data.frame() %>% `colnames<-`("QC_SF") %>% tibble::rownames_to_column(var="ID")
        
        ScaleFactor
    })
    #----Annotations: Built using raw Anno and raw Values.----#
        #Does calculations for Scale factor over again. Probably can import
        
    anno <- reactive({
        #req(rawdata())
        req(anno_pre())
        if (input$QCorNot){
            req(val_all_QC()) #IS this used?
            req(val_all())
        }else{
            req(val_all())  
        }
        req(probes())
        
        
        
        Scans <- !is.null(input$Scans_to_prefilt)
        FOV <- !is.null(input$FOV_Flag)
        BD <- !is.null(input$BD_Flag)
        Min_nuc <- !is.null(input$Min_Nuc)
        Min_area <- !is.null(input$Min_Area)
        SF <- !is.null(input$SF_Flag)
        
        # ERCC_Scale_factor <- function(df_probe, df_val_all){
        #     ERCC_Probes <- df_probe$`ProbeName (display name)`[df_probe$CodeClass == "Positive" & df_probe$`Analyte type` == "SpikeIn"]
        #     
        #     #check before here that input values are data.matrix with numeric data
        #     PosCtrl_mat <- df_val_all[ERCC_Probes,]
        #     
        #     
        #     #Need trycatch handling
        #     if (is.null(dim(PosCtrl_mat))){
        #         if(length(PosCtrl_mat)==0){
        #             print("There are no ERCC_Probes") #Change to error condition
        #         }else if (is.numeric(PosCtrl_mat)){
        #             normfactors <- PosCtrl_mat 
        #         }else{
        #             mode(PosCtrl_mat) <- "numeric"
        #             normfactors <- PosCtrl_mat
        #         }
        #     }else{
        #         normfactors <- PosCtrl_mat %>% 
        #             apply(2, as.numeric) %>%
        #             apply(2, log) %>%
        #             apply(2, mean) %>%
        #             exp()
        #     }
        #     
        #     scalefactor <- mean(normfactors)/normfactors
        #     
        #     return(scalefactor)
        # }
        
        # filter_IDs_PCF <- function(anno, scalefactor, PCF_min=0.3, PCF_max=3.0){
        #     filtered_scalefactor <- scalefactor[scalefactor >= PCF_min & scalefactor<= PCF_max]
        #     
        #     filtred_out_by_posctrl <- names(scalefactor[scalefactor <= PCF_min & scalefactor>= PCF_max])
        #     if(length(filtred_out_by_posctrl) == 0){
        #         cat("No Samples filtered out by Positive Control Factor \n")
        #     }else{
        #         cat(paste0("Failed Positive Control Factor: \n", paste(names(scalefactor[scalefactor >= PCF_min & scalefactor<= PCF_max]),collapse = ", "),"\n"))
        #     }
        #     return(names(filtered_scalefactor))
        # }
        
        
        
        PCF_Filt <- filter_IDs_PCF(anno = anno_pre(), scalefactor =  ERCC_Scale_factor(df_probe = probes(), df_val_all = val_all()),PCF_min = input$SF_Flag[1], PCF_max = input$SF_Flag[2])
        anno <- anno_pre()
        #Conditionals to filter Annotations#
        if (!is.null(input$Filt_switch)){
            
            if (input$Filt_switch){
               
                anno <- anno %>%
                    {if(Scans) filter(.,Scan_ID %in% input$Scans_to_prefilt) else .} %>% #Scan Filter
                    {if(FOV) filter(.,as.numeric(as.character(`Fov counted`)) >= input$FOV_Flag) else .} %>% #FOV Filter
                    {if(BD) filter(.,as.numeric(as.character(BindingDensity)) > input$BD_Flag[1] & as.numeric(as.character(BindingDensity)) <= input$BD_Flag[2]) else .} %>% #BD Filter
                    {if(Min_nuc) filter(.,as.numeric(as.character(`AOI nuclei count`)) >= input$Min_Nuc) else .} %>% #Min filter for Nuclei Count
                    {if(Min_area) filter(.,as.numeric(as.character(`AOI surface area`)) >= input$Min_Area) else .} %>%
                    {if(SF) filter(.,ID %in% PCF_Filt) else .}
                #`rownames<-`(.[,"ID"])
                #tibble::rownames_to_column(var="ID") #%>%
                
                #Add QC Scale Factor to annotations. Prob move to anno_pre with functions and conditional for QC
                if (input$QCorNot) {
                    QC_SF <- ERCC_Scale_factor(df_probe = probes(), df_val_all = val_all()) %>% as.data.frame() %>% `colnames<-`("QC_SF") %>% tibble::rownames_to_column(var="ID")
                    anno <- left_join(anno, QC_SF)
                }
                
            }
        }
        
        
        
        anno
    })
    
    #----Annotations of filtered out Data----#
    anno_filtered <- reactive({
        req(anno())
        req(anno_pre())
        
        anno <- anno()
        anno_pre <- anno_pre()
        
        anno_filtered <- filter(anno_pre, ID %in% anno_pre$ID[!anno_pre$ID %in% anno$ID]) %>%
            select(ROI,ID,Scan_ID, ROI_ID, `Segment tags`, AOI,`AOI surface area`,`AOI nuclei count`,`Fov counted`,BindingDensity) %>%
            {if(input$QCorNot) left_join(.,ScaleFactor()) else .}
        if (t.flag) View(anno_filtered)
        anno_filtered
    })
    
    #----Values of Endogenous probes of unscaled/normalized for future use----#
    val_Endo_pre <- reactive({
        anno <- anno_pre()
        probes <- probes()
        values.all <- val_all()
        
        values.Endo <- values.all[rownames(values.all) %in% probes$`ProbeName (display name)`[probes$CodeClass == "Endogenous"],]
        
        values.Endo
    })
    #----Values of Endogenous probes only Scaling performed here----X
    val_Endo <- reactive({
        IDs <- anno()$ID
        anno <- anno()
        probe <- probes()
        #val_Endo <- data.matrix(val_Endo_pre()[,IDs])
        if(input$QCorNot){
            val_all <- data.matrix(val_all_QC()[,IDs])
        }else{
            val_all <- data.matrix(val_all()[,IDs])
        }
        
        
        #input$Scale_method  #Single Char Value
        #input$Scale_calc    #Single Char Value
        #input$HK_For_Norm   #Char Array
        #input$Norm_calc     #Single Char Value
         

        
        
        if (input$SCALENORM_flag){
            
            if (!input$Scale_method == "None"){
                val_all <- DSP_Scale(df_anno = anno, mat_val_all = val_all, method = input$Scale_method)
            }
            
            if (!is.null(input$Norm_SNR_select)){
                
                if (input$Norm_SNR_select == "HKnorm"){
                    
                    if (!input$Norm_calc == "none"){
                        val_all <- DSP_normalization(df_probe = probe, mat_val_all = val_all, method = input$Norm_calc, probes = input$HK_For_Norm)
                    } 
                }
                
                if (input$Norm_SNR_select == "SNR"){
                    if (!input$SNR_calc == "none") 
                        val_all <- DSP_SNR(df_probe = probe ,mat_val_all = val_all , method = input$SNR_calc, probes = input$Iso_For_Norm)
                    
                }
            }
            
            
            
        }
        
        Endo_probes <- probe$`ProbeName (display name)`[probe$CodeClass == "Endogenous"]
        cat(Endo_probes)
        val_Endo <- val_all[rownames(val_all) %in% Endo_probes,]
        #val_Endo <- val_all
        val_Endo
    })
    
    #----Method to scale all values regardless of filtering----#
        #Discuss best strategy to deal with this issue#
    val_Endo_ALL <- reactive({
        
        anno <- anno_pre()
        probe <- probes()
        
        if(input$QCorNot){
            val_all <- data.matrix(val_all_QC())
        }else{
            val_all <- data.matrix(val_all())
        }
        
        
        #input$Scale_method  #Single Char Value
        #input$Scale_calc    #Single Char Value
        #input$HK_For_Norm   #Char Array
        #input$Norm_calc     #Single Char Value
        
        
        
        
        if (input$SCALENORM_flag){
            
            if (!input$Scale_method == "None"){
                val_all <- DSP_Scale(df_anno = anno, mat_val_all = val_all, method = input$Scale_method)
            }
            
            if (!is.null(input$Norm_SNR_select)){
                
                if (input$Norm_SNR_select == "HKnorm"){
                    
                    if (!input$Norm_calc == "none"){
                        val_all <- DSP_normalization(df_probe = probe, mat_val_all = val_all, method = input$Norm_calc, probes = input$HK_For_Norm)
                    } 
                }
                
                if (input$Norm_SNR_select == "SNR"){
                    if (!input$SNR_calc == "none") 
                        val_all <- DSP_SNR(df_probe = probe ,mat_val_all = val_all , method = input$SNR_calc, probes = input$Iso_For_Norm)
                    
                }
            }
            
            
            
        }
        
        Endo_probes <- probe$`ProbeName (display name)`[probe$CodeClass == "Endogenous"]
        cat(Endo_probes)
        val_Endo <- val_all[rownames(val_all) %in% Endo_probes,]
        #val_Endo <- val_all
        val_Endo
    })


    #-----------DATA TABLE RENDERS FOR OUTPUT DATA-------------------#
    output$anno <- DT::renderDataTable({
        if ( is.null(rawdata()) )   return(NULL)

        #Add if column ID does not exist, make it from scan_id, ROI_ID
        #If col ID exists then leave as is and use as identifier for all
        anno()
        })


    output$probes <- DT::renderDataTable({
        if ( is.null(rawdata()) )   return(NULL)

        #Add if column ID does not exist, make it from scan_id, ROI_ID
        #If col ID exists then leave as is and use as identifier for all
        probes ()
    })
    
    output$val_all <- DT::renderDataTable({
        req(rawdata())
        
        
        val_all()
    })
    output$val_Endo <- DT::renderDataTable({
        req(rawdata())

        val_Endo()
    })
    
    
    #----Show Filtered Samples next to QC Options----#
    filtered_samps <- reactive({
        req(rawdata())
        req(anno_filtered())
        
        a <- anno_filtered()
        filt <- a[,c("ID","ROI_ID")]
        # n <- length(filt)
        # filt_num <- paste("# of ROI's Filtered: ", n)
        # 
        # cat(filt_num)
        # 
        # return(filt_num)
        return(filt)
    })
    
    output$filtered_n <- renderText({
        req(filtered_samps())
        
        
        filt <- filtered_samps()[,"ID"] %>% unlist()
        n <- length(filt)
        filt_num <- paste("# of ROI's Filtered: ", n)
        
        cat(filt_num)
        
        return(filt_num)
    })
    
    output$filtered_samples <- DT::renderDataTable({
        req(filtered_samps())
        
        return(filtered_samps())
    })
    
    output$anno_filtered <- reactable::renderReactable({
        req(rawdata())
        req(anno_filtered())
        anno_filtered <- anno_filtered()
        
        
        to.numeric <- function(x) as.numeric(as.character(x))
        
        SA_color <- function(value) {
            # normalized <- (value - min(data$x)) / (max(data$x) - min(data$xh))
            # color <- orange_pal(normalized)
            # list(background = color)
            value <- to.numeric(value)
            SA_color_func <- function(x){
                ifelse(x <= to.numeric(input$Min_Area), rgb(255,0,0,maxColorValue = 255),rgb(255,255,255,maxColorValue = 255))
            }
            color <- SA_color_func(value)
            
            return( list(background = color) )
            
        }
        
        NC_color <- function(value) {
            # normalized <- (value - min(data$x)) / (max(data$x) - min(data$x))
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
        
        reactable::reactable(data = anno_filtered, 
                             columns = list(
                `AOI surface area` = colDef(style = function(value){SA_color(value)}),
                `AOI nuclei count` = colDef(style = function(value){NC_color(value)})#,
                #`Fov counted` = colDef(style = function(value){}),
                #BindingDensity = colDef(style = function(value){}),
            )
                             )
    })
    
    
    
    
    
    
    
    #----Import From Classification Table----#
    
    #Class table object for DEG. Requires another function to pull classes
    class_table <- reactive({
        req(class_data()) #returns NULL if class_data does not exist
        
        #Read Colnames
        
    })
    
    Map_pos_data <- reactive({
        req(class_data())
        
        if (all(c("ROI","x","y") %in% colnames(class_data()))){
            df <- class_data()[,c("ROI","x","y")]
            df$ROI <- as.character(df$ROI)
            df$x <- to.numeric(df$x)
            df$y <- to.numeric(df$y)
            
            map_pos_df <- df
            if (t.flag) View(map_pos_df)
            
            return(df)
        }
    })
    
    
    #Page 1 plots. Visualize Matrix
    
    cdata <- session$clientData
    
    #----Import Data Plots----#
        #Blank Plot
    blank_Plot <- function(label='No Data'){
        #df <- data.frame()
        #p <- ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100) + ggtitle("No Data")
        ggplot() +
            theme_void() +
            geom_text(aes(0,0,label=label)) +
            xlab(NULL)
    }
    output$pca_ind <- renderPlotly({
        library(factoextra)
        #library(plotly)
        if (is.null(rawdata())) {
            p <- blank_Plot()
            return(p)
            #ggplotly(p)
        }
        val_Endo <- val_Endo()
        anno <- anno()
        p <- data.matrix(val_Endo) %>% t() %>% prcomp() %>% fviz_pca_ind(label="ind", title = "Samples",geom=c("point") ,habillage=anno$Scan_ID, addEllipses=TRUE, ellipse.level=0.95)  + theme_minimal()
        pp <- ggplotly(p,width = cdata$output_pid_width, height=cdata$output_pid_height) %>% plotly_build()
        #Rebuild with base ggplot or plotly code
        
        pp$x$data[[1]]$text <- 
            with(p$data, paste0("name: ", name, 
                                  "</br></br>x: ", x, 
                                  "</br>y: ", y, 
                                  "</br>coord: ", coord, 
                                  "</br>cos2: ", cos2, 
                                  "</br>contrib: ", contrib))
        pp$x$data[[2]]$text <- 
            with(p$data, paste0("name: ", name, 
                                "</br></br>x: ", x, 
                                "</br>y: ", y, 
                                "</br>coord: ", coord, 
                                "</br>cos2: ", cos2, 
                                "</br>contrib: ", contrib))
        pp
    })
    
    output$pca_var <- renderPlotly({
        library(factoextra)
        if (is.null(rawdata())) {
            p <- blank_Plot()
            return(p)
            #ggplotly(p)
        }
        val_Endo <- val_Endo()
        p <- data.matrix(val_Endo) %>% t() %>% prcomp() %>% fviz_pca_var(label="var",title = "Probes", geom = c("point", "text"), col.var = 'contrib' ) + theme_minimal()
        ggplotly(p)
    })
    
    output$density <- renderPlotly({
        library(ggplot2)
        library(dplyr)
        library(tidyr)
        if (is.null(rawdata())) {
            p <- blank_Plot()
            return(p)
        }
        
        val_Endo <- val_Endo()
        anno <- anno()
        p <- data.matrix(val_Endo) %>% 
              log2() %>% 
              as.data.frame() %>% 
              tibble::rownames_to_column(var="row.name") %>% 
              gather(key="ID",value="expr",-row.name) %>% 
              left_join(anno, by = "ID") %>% 
              ggplot() + geom_density(aes(x=expr,color=ID),show.legend = FALSE) + theme_minimal() + facet_wrap(~Scan_ID)
        
        ggplotly(p)
        
        #return(p)
        
    })
    
    library(plotly)
    output$mainHeat <- renderPlotly({
        library(dplyr)
        library(heatmaply)
        if (is.null( rawdata() )){
            return(plotly::plotly_empty())
        }
        
        #Add imports for ScanIDs to paste into heatmap name
        val_Endo <- val_Endo()
        anno <- anno()
        
        val_Endo %>% data.matrix() %>% log2() %>% 
            heatmaply(
                main = "Heatmap",
                col_side_colors = anno[,"Scan_ID",drop=FALSE],
                plot_method="plotly",
                scale = "row",
                colors=RdBu
            )
    })
    
    output$IsovHK_plt <- renderPlotly({
        if (is.null( val_all() )){
            p <- blank_Plot()
            return(p)
        }
        
        #Conditional switch for val_all/val_all_QC needed
        val_all <- val_all()
        
        HK <- probes() %>%
            filter(`Analyte type` == "RNA" & CodeClass == "Control") %>% 
            select(`ProbeName (display name)`) %>% unlist() %>% unname()
        Iso <- probes() %>%
            filter(`Analyte type` == "RNA" & CodeClass == "Negative") %>% 
            select(`ProbeName (display name)`) %>% unlist() %>% unname()
        
        Isos <- val_all[Iso,] %>%
            apply(2, as.numeric) %>%
            apply(2, log) %>%
            apply(2, mean) %>%
            exp()
        
        HKs <- val_all[HK,] %>%
            apply(2, as.numeric) %>%
            apply(2, log) %>%
            apply(2, mean) %>%
            exp()
        
        df <- data.frame(Isos,HKs)
        
        p <- ggplot(df) + geom_point(aes(x=HKs, y=Isos)) + xlab("HouseKeeping (Geomean)") + ylab("Background Negative (Geomean)")
        
        ggplotly(p)
    })
    
    output$HK_Sct_mt <- renderPlotly({
        if (is.null( val_all() )){
            p <- blank_Plot()
            return(p)
        }
        
        #Conditional switch for val_all/val_all_QC needed
        val_all <- val_all()
        
        HK <- probes() %>%
            filter(`Analyte type` == "RNA" & CodeClass == "Control") %>% 
            select(`ProbeName (display name)`) %>% unlist() %>% unname()
        
        
        HKs <- val_all[HK,] %>% t() %>% as.data.frame()
           
        p <- GGally::ggpairs(HKs)
        
        
        ggplotly(p)
    })
    
    output$SNR_boxplot <- renderPlotly({
        if (is.null( val_all() )){
            p <- blank_Plot()
            return(p)
        }
        
        #Conditional switch for val_all/val_all_QC needed
        val_all <- val_all()
        probes <- probes()
        anno <- anno_pre() #Will not change based on data
        
        Endo_probes <- probes$`ProbeName (display name)`[probes$CodeClass == "Endogenous"]
        
        val_all_SNR <- DSP_SNR(df_probe = probes, mat_val_all = val_all)
        p <-  val_all_SNR[Endo_probes,] %>% 
            as.data.frame() %>% 
            tibble::rownames_to_column(var="probe") %>% 
            gather(key="ID", value="expr",-probe) %>% 
            mutate(expr = as.numeric(as.character(expr))) %>% 
            left_join(anno, by="ID") %>% 
            #ggplot(aes(x=ROI_ID,y=log2(expr))) + geom_boxplot(aes(fill=Scan_ID)) + geom_dotplot(binaxis='y',stackdir='center', dotsize = .2) + theme_bw() + labs(subtitle="SNR, log2(expr)", title= "Comparison of ROIs per Scan", x= "Scan Name") + facet_grid(Scan_ID ~ .)  
            ggplot(aes(x=ROI_ID,y=log2(expr),fill=Scan_ID)) + geom_boxplot() + theme_bw() + labs(subtitle="SNR, log2(SNR)", title= "Comparison of ROIs per Scan", x= "ROI") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x=element_blank()) + facet_grid(Scan_ID ~ .)  
        
        #RNA005_ScaleNuclei_SNR.values.Endo %>% as.data.frame() %>% tibble::rownames_to_column(var="probe") %>% gather(key="ID", value="expr",-probe) %>% mutate(expr = as.numeric(as.character(expr))) %>% left_join(RNA005_ScaleNuclei_SNR.anno, by="ID") %>% ggplot(aes(x=ROI_ID,y=log2(expr))) + geom_boxplot(aes(fill=Scan_ID)) + geom_dotplot(binaxis='y',stackdir='center', dotsize = .2) + theme_bw() + labs(subtitle="SNR, log2(expr)", title= "Comparison of ROIs per Scan in RNA005 Run", x= "Scan Name") + facet_grid(Scan_ID ~ .)
        
        ggplotly(p)
    })
    
    
    
    #---------------------------------------------Image Map-----------------------------------------------------#
    
    
    #----Menu for Image Import----#
    output$select_ImgScan <- renderUI({
        if (is.null( rawdata() )) return(NULL)
        if (is.null( anno() )) return(NULL)
        anno <- anno()
        
        selectInput(inputId = "scanselect1",
                    label = "Select Scan ID of Image",
                    choices = unique(anno$Scan_ID),
                    selected = unique(anno$Scan_ID)[1])
        
    })
            #----Data Check----#
    observeEvent(input$scanselect1, { 
        cat(paste("Image Map Scan Selected: ",input$scanselect1,"\n"))
        cat(paste("Structure of IM_map Scan selection", str(input$scanselect1),"\n"))
    })
    
    output$select_ImgProbes <- renderUI({
        if (is.null( rawdata() )) return(NULL)
        if (is.null( probes() )) return(NULL)
        probes <-probes()
        #Add conditional to select probes after selecting ROI centerpoints
        selectizeInput(inputId = "probeselect1",
                       label = "Probe of Interest",
                       choices = rownames(probes))
    })
    
    
    
            #----Data Check----#
    observeEvent(input$probeselect1, { 
        cat(paste("Image Map Probe Selected: ",input$probeselect1,"\n"))
        cat(paste("Structure of IM_map Probe selection", str(input$probeselect1),"\n"))
    })
    
    #----returns Values from Class Table for Image Map----#
    immap_Vals <- reactive({
        
        req(input$scanselect1)
        req(input$probeselect1)
        req(val_Endo())
        req(anno())
        
        
        if (input$Img_Anno_Filt == "Immap_ROI_All"){
            anno <- anno_pre()
            val_Endo <- val_Endo_ALL()
        } else {
            anno <- anno()
            val_Endo <- val_Endo()
        }
        
        
        anno_filt_1 <- anno %>% filter(Scan_ID == input$scanselect1)
        if (t.flag) View(anno_filt_1)
        dup_ROI <- any(duplicated( anno_filt_1[,"ROI_ID"] )) #logical
        
        #If ROI duplicated then AOI functions required
        x_ID <- anno_filt_1 %>% select(ID) %>% unlist()
        if (t.flag) View(x_ID)
        x_Val <- val_Endo[input$probeselect1,x_ID] #named value
        if (t.flag) View(x_Val)
        #pull Anno in order of named vector, extract ROI_ID in that order, apply names to x_Val
        x_Val_ROInames <- anno[match(names(x_Val),anno$ID),]$ROI_ID %>% as.character()
        
        df_Val <- data.frame(ID=names(x_Val), expr=x_Val, ROI=x_Val_ROInames)
        if (t.flag) View(df_Val)
        
        return(df_Val)
        
    })
    
    #!!!!!!!!!!!Currently Unused - Building method to select ROI's without Table!!!!!!!!!!!!!!!!!!!!
    output$select_ImgROIs <- renderUI({
        if (is.null( rawdata() )) return(NULL)
        if (is.null( anno() )) return(NULL)
        
        anno <- anno()
        selection <- input$scanselect1
        ROIs <- anno %>% filter(Scan_ID == selection) %>% select(ROI_ID)
        ROIs <- ROIs[sort(ROIs)] # ? method for sorting???
        
        
        ROIbuttons <- lapply(1:length(ROIs), function(i) {
            
            tabPanel(title = glue::glue("Select {ROIs[i]}"),
                     h3(glue::glue("Content {i}"))
            )
        })
        do.call(tabsetPanel, myTabs)
    })
    
    #Populate list of ROIs
    #If ROIs are not unique display error
    
    # im <- image_read("noimage.png")
    # observeEvent(input$impImage, {
    #     cat(input$impImage$datapath,"\n")
    #     if (length(input$impImage$datapath)){
    #         im <<- image_read(input$impImage$datapath)}
    #     info <- image_info(im)
    # })
    
    #----Default image if none imported----#
    default_im <- image_read("noimage.png")
    im <- reactive({
        if(is.null(input$impImage)){
            output <- default_im
        } else {
            cat(input$impImage$datapath,"\n")
            output <- image_read(input$impImage$datapath)
        }
    })
    

   #----Determines Image Size----#
    mapImDimW <- reactive({
        if( is.null( input$impImage )) {return(as.numeric(400))}
        else{return(as.numeric(magick::image_info(im())$width))}
    })
    mapImDimH <- reactive({
        if( is.null( input$impImage )) {return(as.numeric(400))}
        else{return(as.numeric(magick::image_info(im())$height))}
    })
    
    #----Generate Output for Plot----#
        #----render UI used to build scaling function for plot data----#
    output$map.ui <- renderUI({
        plotOutput("mapImage", width = mapImDimW(), height = mapImDimH())
    })
    
    output$mapImage <- renderPlot({
        
        im <- im()
        
        info <- image_info(im) #info$width, #info$height for dimensions of image
        h <- info$height
        w <- info$width
        cat(paste("Image Width: ",w, "\n"))
        cat(paste("Image Height: ",h, "\n"))
        

        

        #g <- rasterGrob(im,width=unit(1,"npc"), height=unit(1,"npc"), interpolate = FALSE )
        g <- rasterGrob(im,width=unit(1,"npc"), height=unit(1,"npc") )
        
        
        if (is.null(input$impImage)) return(blank_Plot(label = 'No Image')) #Plot if no Data
        #UPDATE>>>#if is.null(class_data()) then shinyHTML popup no Please import classification Table
        #return(blank_Plot(label= 'No Class Table'))
        #if (is.null(class_data())) return(blank_Plot(label = 'No Class Table'))
        #if (!"x" %in% colnames(class_data())) return(blank_Plot(label = 'No x cord in class Table'))
        
        
        anno <- anno()
        val_Endo <- val_Endo()
        probes <- probes()
        
        
        
        #left_join(immap_Vals(), class_data()) 
        #class_data <- class_data()
        
        if (is.null(class_data()) ){
            return(blank_Plot(label = 'No Class Table'))
        }
        
        Map_pos_data <- Map_pos_data()
        immap_Vals <- immap_Vals()
        if (t.flag) View(Map_pos_data[,"ROI"])
        if (t.flag) View(immap_Vals[,"ROI"])
        
        if (input$Img_Anno_Filt == "Immap_ROI_Filt"){
            
            Map_pos_data <- Map_pos_data[Map_pos_data$ROI %in% immap_Vals[,"ROI"],]
            
        }
        
        #cat(paste( (length(Map_pos_data$ROI) != length(immap_Vals[,"ROI"])) ))
        
        if (length(Map_pos_data$ROI) != length(immap_Vals[,"ROI"]) ){
            cat(paste("length Map_pos_data$ROI",length(Map_pos_data$ROI),"\n"))
            cat(paste("length immap_Vals()$ROI",length(immap_Vals[,"ROI"]),"\n"))
            return(blank_Plot(label = 'ROI Length Mismatch'))
        }
        
        
        immap_order <- immap_Vals[,"ROI"][match(as.character(Map_pos_data$ROI),as.character(immap_Vals[,"ROI"]))]
        immap_Vals_expr <- immap_Vals[,"expr"][immap_order]
        
        if (t.flag) View(immap_Vals_expr)
    
        
        nroi = length(immap_Vals)
        iy <- Map_pos_data$y
        ix <- Map_pos_data$x
        iname <- Map_pos_data$ROI
        isize <- immap_Vals_expr
        
        i_df <- data.frame(iname, ix, iy, isize)
        if (t.flag) View(i_df)
        
        logFunc <- function(mt){
            #if (input$logfunc == "log2"){}
            log2(mt)
            }
        #If Scan_ID Not Selected Do Not begin Image Plot
         mat_mi<- val_Endo[,anno$ID[anno$Scan_ID == input$scanselect1]] %>%
             logFunc() %>%
             as.data.frame() %>%
             tibble::rownames_to_column(var = "probes")
        
        #If no Probe selected, use a base label to help identify Positions
        
        #imgplt <- ggplot() + annotation_custom(g,-Inf,Inf,Inf,-Inf) + geom_point(aes(x=ix,y=iy,size=isize, color="red")) + geom_text(aes(x=ix,y=iy,label = iname, color = "yellow")) + theme_void() + theme(legend.position="none")
        #imgplt <- ggplot() + annotation_custom(g, 0,w,0,-h) #+ geom_point(aes(x=ix,y=iy,size=isize, color="red")) + geom_text(aes(x=ix,y=iy,label = iname, color = "yellow")) + theme_void() + theme(legend.position="none")
        imgplt <- ggplot(i_df) +
            annotation_custom(g,0,w,0,-h) +
            scale_x_continuous(limits = c(0,w),expand = c(0,0)) +
            scale_y_reverse(limits=c(h,0), expand = c(0,0)) +
            coord_equal() +
            geom_point(aes(x=ix,y=iy,size=isize^2), fill="red",color="yellow", alpha=.5) +
            geom_text(aes(x=ix,y=iy,label = iname), color = "yellow") +
            scale_size(range = c(2,30))
        imgplt
    })

    #ggplot(rgd) + annotation_custom(rg,0,wei,0,-hei) + geom_point(aes(x=x,y=y,size=10,color="red")) + scale_y_reverse(limits=c(hei,0), expand = c(0,0)) + scale_x_continuous(limits = c(0,hei),expand = c(0,0)) + theme_void()
    #R1541T_Initial.values.Endo["BATF3",] %>% t() %>% data.frame() %>% tibble::rownames_to_column(var="ID") %>% mutate(BATF3 = as.numeric(as.character(BATF3))) %>% plot_ly(y=~BATF3, text=~ID, type="box") %>% layout(xaxis=list(title="BATF3"))
    
    #----------------------------------------------------------- Run 2 Run Comparisons -----------------------------------------------#
        #Observer to trigger options based on whether data is available or not
    observe({
        if (!is.null(anno())){
            
            if ( length(unique(anno()[,"Scan_ID"])) >= 2 ){
                
                shinyjs::show(id = "run2run_btn")
                cat(paste("Number of Scans: ", length(unique(anno()[,"Scan_ID"])),"\n"))
                #Show/Hide DIV for run 2 run selection box here also
                
            } else {
                
                shinyjs::hide(id = "run2run_btn")
            }
            
        } else {
            
            shinyjs::hide(id = "run2run_btn")
            
        }
    })
    
    output$run2run_imp_menu <- reactiveUI({
        req(anno())
        
    })

    #--------------------------------------------------------STATS MODULE--------------------------------------------#
    
    
    #Read segments and select unique
    
    output$stat_seg_select <- renderUI({
        
        req(anno())
        
        anno <- anno()
        
        sam <- anno %>% select(`Segment tags`) %>% unlist() %>% unique()
        
        selectInput(inputId = "stat_my.seg",
                    label = "Select Static Segment",
                    choices = sam, 
                    selected = sam[1],
                    multiple = FALSE
        )
        
        
    })
    
    
    stat_df.group <- reactive({
        req(input$statgroupimport)
        
        a <- read_tsv(file=input$statgroupimport$datapath, trim_ws=TRUE)
        #a <- apply(a, 2, as.character)
        cat(paste("stat group import: ", str(a)))
        
        return(a)
        
    })
    
    
    output$stat_g1_select <- renderUI({
        
        req(stat_df.group())
        
        
        df <- stat_df.group()
        
        g1 <- df %>% select(group) %>% unlist() %>% unique()
        
        if (length(g1) < 2) {
            errorCondition(message = "Error: Less than 2 groups, cannot continue stat analysis")
            return(NULL)
        }
        
        selectInput(inputId = "stat_group1",
                    label = "First Group for Comparison",
                    choices = g1, 
                    selected = g1[1],
                    multiple = FALSE
        )
        
        
    })
    
    output$stat_g2_select <- renderUI({
        req(input$stat_group1)
        
        g1 <- input$stat_group1
        
        df <- stat_df.group()
        g2 <- df %>% select(group) %>% unlist() %>% unique() %>% .[. != g1]
        
        selectInput(inputId = "stat_group2",
                    label = "Second Group for Comparison",
                    choices = g2, 
                    selected = g2[1],
                    multiple = FALSE
        )
        
        
    })
    
    
    #-----Assemble dsp.df and Display to table----#
    stat_dsp.df <- reactive({
        req(anno())
        req(val_Endo())
        req(stat_df.group())
        
        anno <- anno()
        val.Endo <- val_Endo()
        df.group <- stat_df.group()
        
        df1 <- val.Endo %>% 
            as.data.frame() %>% 
            tibble::rownames_to_column(var="probe") %>% 
            gather(key = "ID", value = "expr", -probe) %>% 
            mutate(expr=as.numeric(as.character(expr))) %>% 
            spread(key="probe", value = "expr")
        
        df2 <- anno %>% 
            select(ID,Scan_ID, ROI_ID,`Segment tags`)
        
        df3 <- left_join(df1,df2) %>% 
            rename("Segment"="Segment tags") %>% 
            select(-ID) %>% 
            select(Scan_ID, Segment, ROI_ID, everything()) 

        
        dsp.df <- left_join(df3, df.group)
        
        row.names(dsp.df) = paste0(
            dsp.df$Scan_ID,'!',dsp.df$Segment,'!',dsp.df$ROI_ID
        )
        
        if (t.flag) View(dsp.df)
        
        dsp.df
        
        
    })
    
    output$stat_df <- DT::renderDataTable({
        req(stat_dsp.df())
        
        stat_dsp.df()
    })
    
    #----Stat module asks for probe array----#
    stat_genes <- reactive({
        x <- val_Endo() %>% rownames()
        return(x)
    })
    
    
    observeEvent(input$Lmem_low_exp_flag,{
        cat("==========STAT VARIABLES==========\n")
        #cat(paste("",,"\n"))
        cat(paste("Lmem_low_exp_flat",input$Lmem_low_exp_flag,"\n"))
        cat(paste("Lmem_test_method",input$Lmem_test_method,"\n"))
        #cat(paste("Lmem_p_filt",input$Lmem_p_filt,"\n"))
        cat(paste("stat_my.seg",input$stat_my.seg,"\n"))
        cat(paste("stat_group1",input$stat_group1,"\n"))
        cat(paste("stat_group2",input$stat_group2,"\n"))
        cat(paste("stat_dsp.df",str(stat_dsp.df()),"\n"))
        cat(paste("stat_genes",stat_genes(),"\n"))
    })
    
    
    
    
    #----Trying to fit all of the stats into 1 function ----#
    
    stat_calc <- reactive({
        #req(input$Lmem_low_exp_flag)
        req(input$Lmem_test_method)
        #req(input$Lmem_p_filt)
        req(input$stat_my.seg)
        req(input$stat_group1)
        req(input$stat_group2)
        req(stat_dsp.df())
        req(stat_genes())
        
        
        genes <- stat_genes()
        dsp.df <- stat_dsp.df()
        g1 <- input$stat_group1
        g2 <- input$stat_group2
        stat_my.seg <- input$stat_my.seg
        
        plot.colors = c('Y' = '#CC0000','N' = '#C0C0C0')
        
        # if (input$Lmem_low_exp_flag){
        #     lowexpr.gene.flag = 1
        # } 
        # if (!input$Lmem_low_exp_flag){
        #     lowexpr.gene.flag = 0
        # }
        lowexpr.gene.flag = 1
        
        if (input$Lmem_test_method == "t.test"){
            t.test.flag = 1
            z.test.flag = 0
        }
        
        if (input$Lmem_test_method == "z.test"){
            t.test.flag = 0
            z.test.flag = 1
        }
        
        # if (input$Lmem_p_filt == "p.value"){
        #     rawp.flag = 1
        #     fdr.flag = 0
        #     #p.metho = ?rawp?
        # }
        # 
        # if (input$Lmem_p_filt == "fdr"){
        #     rawp.flag = 0
        #     fdr.flag = 1
        #     p.method = "fdr"
        # }
        rawp.flag = 0
        fdr.flag = 1
        
        #dir.create('stat_output')
        
        if(TRUE) {
            
            segments = sort(unique(dsp.df$Segment))
            
            print(segments)
            
            group1 = g1
            group2 = g2
            
            my.seg = stat_my.seg #variable made in function
            
            #output = paste0('stat_output\\',my.seg,'.',group1,'_vs_',group2)
            
            #print(output)
            
            ## ----------------------------------------------------------------
            
            ## prep input 
            if(TRUE) {
                
                rm(data.plot)
                
                data.plot = dsp.df[dsp.df$Segment %in% my.seg,,drop=F]
                
                # write.csv(data.plot,
                #           file = paste0(output,'.data.csv'))
                
            }
            
            ## ----------------------------------------------------------------
            
            
            #min(dsp.df[,genes]) ## 0.02; so we perhaps have to log transform data???
            #max(dsp.df[,genes]) ## 518.74; so we perhaps have to log transform data???
            
            
            ## options 
            if(TRUE) {
                
                ## which test to run 
                #z.test.flag = 0 ## recommended if sample size > 30
                #t.test.flag = 1 ## recommended if sample size < 30
                
                ## whether to show gene names for those passing unadjusted or adjusted p<0.05
                #rawp.flag = 0
                #fdr.flag = 1
                
                ## whether to filter low expr genes (e.g. low SNR) in certain number of samples?
                ## e.g. keep genes with SNR>1 in at least 3 samples (scans)
                ## default is off 
                #lowexpr.gene.flag = 1 ## default is 0
                if(lowexpr.gene.flag==1) {
                    min.expr = 1
                    min.sm = 3 
                }
                
                
            }
            
            
            ## ----------------------------------------------------------------
            
            ## run stats 
            if(TRUE) {
                
                data.stats = NULL 
                
                for(i in 1:length(genes)) {
                    my.gene = genes[i]
                    
                    print(my.gene)
                    
                    
                    ## ----------------------------------------------------------------
                    
                    my.df = NULL 
                    my.stats = NULL 
                    
                    #my.df = data.plot[,c('Scan_ID','Segment','ROI_ID','group',my.gene)]
                    my.df = data.plot[,c('PID','Scan_ID','Segment','ROI_ID','group',my.gene)]
                    
                    colnames(my.df)[6] = 'Gene'
                    
                    ## ----------------------------------------------------------------
                    
                    ## filter low expr genes if needed 
                    if(lowexpr.gene.flag == 1) {
                        
                        if(sum(my.df$Gene > min.expr, na.rm=T) >= min.sm) {
                            ## keep this gene!
                        } else {
                            cat(paste0(my.gene,' was filtered out!'))
                            next 
                        }
                        
                        
                    }
                    
                    ## ----------------------------------------------------------------
                    
                    ## set levels for factor in the correct order ... group1 vs group2
                    my.df$group = factor(my.df$group, levels = c(group1, group2))
                    
                    ## ----------------------------------------------------------------
                    
                    ## log transform if needed 
                    my.df$Gene = log2(my.df$Gene)
                    
                    ## ----------------------------------------------------------------
                    
                    ## calculate fold change 
                    mean1 = NULL
                    mean2 = NULL 
                    log2fc = NULL 
                    if(TRUE) {
                        
                        ## take the avg within each sample across ROIs, then take avg across all samples
                        for(my.sm in sort(unique(my.df$Scan_ID))) {
                            mean1 = c(mean1, 
                                      mean(my.df$Gene[my.df$group==g1 & my.df$Scan_ID==my.sm], na.rm = T))
                            mean2 = c(mean2, 
                                      mean(my.df$Gene[my.df$group==g2 & my.df$Scan_ID==my.sm], na.rm = T))
                            
                        }
                        
                        mean1 = mean(mean1, na.rm = T)
                        mean2 = mean(mean2, na.rm = T)
                        
                        ## since data were log transformed, log2fc is just mean1 - mean2 (and not ratio!!)
                        log2fc = mean1 - mean2 
                    }
                    
                    ## ----------------------------------------------------------------
                    
                    # fit a linear mixed effects model
                    ## random effect: Scan_ID
                    ## fixed effect: group 
                    rm(lmer.out)
                    
                    if (input$Lmem_FE == "group" & input$Lmem_RE == "PID") {
                        lmer.out = lmer(Gene ~  0 + group + (1 | PID), data = my.df, REML = T)
                        #lmer.out = lmer(Gene ~  0 + group  + (1 | Scan_ID), data = my.df, REML = T)
                    } else if (input$Lmem_FE == "PID" & input$Lmem_RE == "group") {
                        lmer.out = lmer(Gene ~  0 + PID + (1 | group), data = my.df, REML = T)
                        #lmer.out = lmer(Gene ~  0 + Scan_ID + (1 | group), data = my.df, REML = T)
                    } else if (input$Lmem_FE == "PID" & input$Lmem_RE == "PID") {
                        lmer.out = lmer(Gene ~  0 + PID + (1 | PID), data = my.df, REML = T)
                        #lmer.out = lmer(Gene ~  0 + Scan_ID + (1 | group), data = my.df, REML = T)
                    } else if (input$Lmem_FE == "group" & input$Lmem_RE == "group") {
                        lmer.out = lmer(Gene ~  0 + group + (1 | group), data = my.df, REML = T)
                        #lmer.out = lmer(Gene ~  0 + Scan_ID + (1 | group), data = my.df, REML = T)
                    }
                    
                    
                    #lmer.out = lmer(Gene ~  0 + group  + (1 | Scan_ID), data = my.df, REML = T)
                    
                    # parameter estimates
                    summary(lmer.out)
                    
                    # # anova table
                    # my.stats = anova(lmer.out)
                    # my.stats
                    
                    # # double check the order and list of the fixed effect parameters
                    # data.frame(fixef(lmer.out))
                    # # Fixed effects:
                    # #   Estimate Std. Error      df t value Pr(>|t|)
                    # # groupNR  -0.8464     0.9378  3.7437  -0.902    0.421
                    # # groupR   -0.9996     0.6817  4.1157  -1.466    0.214
                    
                    ## ----------------------------------------------------------------
                    
                    # set up contrasts: group1 vs group2
                    contrast.matrix = rbind(
                        group1.vs.group2 = c(-1,1)
                    )
                    
                    # run contrast
                    rm(comp.stats)
                    if(z.test.flag==1) {
                        # z-test if sample size is large!
                        rm(comp.z)
                        comp.z = glht(lmer.out, contrast.matrix)
                        # summary(comp.z, test = adjusted("none"))
                        summary(comp.z, test = adjusted("BH"))
                        
                        comp.stats = comp.z
                    }
                    
                    if(t.test.flag == 1) {
                        # t-test if sample size is small!
                        rm(comp.t)
                        comp.t = contest(lmer.out, L= contrast.matrix, joint = FALSE,
                                         ddf='Satterthwaite',
                                         check_estimability = TRUE)
                        comp.t$p.adj = p.adjust(comp.t$`Pr(>|t|)`, method = 'BH')
                        comp.t
                        
                        comp.stats = comp.t
                    }
                    
                    ## ----------------------------------------------------------------
                    
                    data.stats = rbind(data.stats,
                                       data.frame(Gene = my.gene,
                                                  Segment = my.seg,
                                                  Comp = paste0(group1, '_vs_',group2),
                                                  Group1.smtotal = sum(my.df$group==g1),
                                                  Group2.smtotal = sum(my.df$group==g2),
                                                  Mean1 = mean1,
                                                  Mean2 = mean2,
                                                  logFC = log2fc,
                                                  comp.stats,
                                                  is.singular = isSingular(lmer.out),
                                                  stringsAsFactors = F))
                    
                }
                
                data.stats$p.value = data.stats$Pr...t..
                data.stats$p.adj = p.adjust(data.stats$p.value, method = 'fdr')
                
                row.names(data.stats) = 1:nrow(data.stats)
                
                # write.csv(data.stats,
                #           file = paste0('stat_output\\',output, '.lmer_stats.csv'))
                
                data.stats[data.stats$p.adj<0.05,]
                data.stats[data.stats$p.adj<0.05 & abs(data.stats$logFC)>=log2(1.5),]
                
                ## ----------------------------------------------------------------
                
                ## would be good if people can pick raw p or fdr p or fc cutoffs by themselves??? e.g. default is 0.05 and 1.5 but people can pick 
                
                ## here I am just saving everything ....
                ## save files 
                if(TRUE) {
                    dim(data.stats) ## 52
                    data.stats.sig = data.stats[data.stats$p.adj < 0.05,]
                    dim(data.stats.sig) ## 1
                    
                    # write.csv(data.stats.sig,
                    #           file = paste0('stat_output\\',output, '.lmer_stats.fdr0.05.csv'))
                    
                    data.stats.sig.2 = data.stats[data.stats$p.adj < 0.05 & abs(data.stats$logFC)>=log2(1.5),]
                    dim(data.stats.sig.2) ## 1
                    
                    # write.csv(data.stats.sig.2,
                    #           file = paste0('stat_output\\',output, '.lmer_stats.fdr0.05_fc1.5.csv'))
                    # 
                    # data.stats.sig.3 = data.stats[data.stats$p.adj < 0.1 & abs(data.stats$logFC)>=log2(1.5),]
                    # dim(data.stats.sig.3) ## 1
                    # 
                    # write.csv(data.stats.sig.3,
                    #           file = paste0('stat_output\\',output, '.lmer_stats.fdr0.1_fc1.5.csv'))
                    # 
                    # data.stats.sig.4= data.stats[data.stats$p.value < 0.05 & abs(data.stats$logFC)>=log2(1.5),]
                    # dim(data.stats.sig.4) ## 1
                    # 
                    # write.csv(data.stats.sig.4,
                    #           file = paste0('stat_output\\',output, '.lmer_stats.rawp0.05_fc1.5.csv'))
                }
                
            }
            
            ## ----------------------------------------------------------------
            
            plot.order = row.names(data.plot)
            
            ## making plots 
            if(TRUE) { 
                
                rm(p1,p2,p3)
                
                ## heatmap: sig genes 
                if(TRUE & nrow(data.stats.sig)>0) {
                    
                    rm(centered_data)
                    centered_data = data.plot[,genes,drop=F]
                    
                    centered_data = t(centered_data)
                    min(centered_data)
                    max(centered_data)
                    
                    ## log if needed 
                    centered_data = log2(centered_data)
                    min(centered_data)
                    max(centered_data)
                    
                    
                    centered_data = centered_data[row.names(centered_data) %in% 
                                                      data.stats.sig$Gene,,drop=F]
                    dim(centered_data)
                    
                    centered_data = t(scale(t(as.matrix(centered_data)), scale=T))
                    
                    if(TRUE) {
                        
                        ## add annotation 
                        
                        sample.anno = data.plot[,c('PID','group','Segment')]
                        sample.anno[is.na(sample.anno)] = 'NA'
                        
                        sample.anno$Sample = row.names(sample.anno)
                        
                        ## sort sample anno same as expression matrix
                        sample.anno = sample.anno[sample.anno$Sample %in% colnames(centered_data),]
                        sample.anno = sample.anno[order(match(sample.anno$Sample,
                                                              colnames(centered_data))),]
                        row.names(sample.anno) = sample.anno$Sample
                        # sample.anno = sample.anno[,-1]
                        print(all.equal(sample.anno$Sample, colnames(centered_data)))
                        
                        # sample.anno.colors = list(
                        #   Group_amp = plot.colors,
                        #   Group_mets = plot.colors
                        # )
                        
                        plot.anno = HeatmapAnnotation(df = sample.anno[,c('group','Segment')]
                                                      # ,col = sample.anno.colors
                        )
                        
                    }
                    
                    table(sample.anno$group)
                    
                    myheatcol = colorRamp2(c(-1.5, 0, 1.5), c("blue", "white", "red"))
                    
                    col.title = paste0(paste(my.seg,collapse = '_vs_'), ' ',ncol(centered_data), 
                                       ' samples')
                    row.title = paste0(nrow(centered_data), ' features')
                    
                    p1 = Heatmap(centered_data,
                                 na_col = "#C0C0C0",
                                 col = myheatcol,
                                 rect_gp = gpar(col = NA),
                                 show_heatmap_legend = T,
                                 column_title = col.title,
                                 row_title = row.title,
                                 # column_title_side = 'bottom',
                                 column_names_side = 'bottom',
                                 row_dend_width = unit(5, "cm"),
                                 column_dend_height = unit(5, "cm"),
                                 # km = 2,
                                 cluster_rows = T,
                                 cluster_columns = T,
                                 clustering_distance_rows = "euclidean",
                                 clustering_method_rows = "ward.D2",
                                 clustering_distance_columns = "euclidean",
                                 clustering_method_columns = "ward.D2",
                                 show_row_names = T,
                                 show_column_names = T,
                                 top_annotation = plot.anno,
                                 heatmap_legend_param = list(title = 'log2(SNR)', 
                                                             color_bar = "continuous")
                    )
                    
                    # pdf(paste0('stat_output\\',output,'.lmer_fdr0.05.heatmap.pdf'), 
                    #     width = 11, height = 8)
                    # print(p1)
                    # dev.off()
                    
                    # write.csv(centered_data,
                    #           file = paste0('stat_output\\',output,'.lmer_fdr0.05.heatmap.csv'))
                    
                    
                }
                
                ## boxplot: sig genes 
                if(TRUE & nrow(data.stats.sig)>0) {
                    
                    data.plot.2 = data.plot[,c('Scan_ID','ROI_ID','Segment','group',
                                               data.stats.sig$Gene)]
                    
                    data.plot.2 = melt(data.plot.2)
                    
                    colnames(data.plot.2) = c('Scan_ID','ROI','Segment','group','Gene','SNR')
                    
                    data.plot.2$log2SNR = log2(data.plot.2$SNR)
                    
                    p2 = ggplot(data.plot.2, aes(group, log2SNR)) +
                        geom_boxplot(width = 0.5, color = '#C0C0C0', lwd = 1, outlier.shape = NA) +
                        geom_point() +
                        # geom_line(aes(group = SID_ROI), linetype = 'dotted') +
                        ggtitle(paste0(my.seg, g1,' vs ', g2)) + 
                        # scale_y_continuous(trans = 'log2')+
                        # annotation_logticks(sides="l") + 
                        theme_pubr() +
                        facet_grid( Segment ~ Gene) 
                    # theme(axis.text.x = element_text(angle = 45, hjust=1))
                    
                    p2
                    
                    # pdf(paste0('stat_output\\',output,'.lmer_fdr0.05.box.pdf'), 
                    #     width = 11, height = 8)
                    # print(p2)
                    # dev.off()
                    # 
                    # write.csv(data.plot.2,
                    #           file = paste0('stat_output\\',output,'.lmer_fdr0.05.box.csv'))
                    
                }
                
                ## vlcano plot: sig genes 
                if(TRUE & nrow(data.stats.sig)>0) {
                    
                    data.plot.3 = data.stats
                    data.plot.3$sig = 'N'
                    data.plot.3$sig[data.plot.3$Gene %in% data.stats.sig$Gene] = 'Y'
                    
                    plot.x.max = max(data.plot.3$logFC)
                    print(plot.x.max)
                    
                    plot.y.line = max(data.plot.3$p.value[data.plot.3$sig=='Y'])
                    print(plot.y.line)
                    
                    p3 = ggplot(data.plot.3, aes(x = logFC, y = -log10(p.value))) +
                        geom_point(aes(color = sig)) +
                        scale_color_manual(values = plot.colors) +
                        theme_bw(base_size = 12) + theme(legend.position = "bottom") +
                        geom_text_repel(
                            data = data.plot.3[data.plot.3$sig=='Y',],
                            aes(label = Gene),
                            size = 3,
                            box.padding = unit(0.35, "lines"),
                            point.padding = unit(0.3, "lines")
                        ) +
                        ggtitle(paste0(my.seg,g1, ' vs ', g2)) + 
                        xlim(c(-plot.x.max,plot.x.max)) +
                        theme_pubr() +
                        facet_grid( Segment ~ .) +
                        # geom_hline(yintercept = -log10(plot.y.line), linetype = 'dashed') +
                        # geom_vline(xintercept = c(-log2(1.5),0, log2(1.5)), linetype = 'dashed')
                        geom_vline(xintercept = 0, linetype = 'dashed')
                    
                    # theme(axis.text.x = element_text(angle = 45, hjust=1))
                    
                    p3
                    
                    # setEPS()
                    # postscript(paste0('stat_output\\',output,'.lmer_fdr0.05.volcano.eps'), 
                    #            width = 6, height = 6)
                    # print(p3)
                    # dev.off()
                    # 
                    # pdf(paste0('stat_output\\',output,'.lmer_fdr0.05.volcano.pdf'), 
                    #     width = 6, height = 6)
                    # print(p3)
                    # dev.off()
                    # 
                    # write.csv(data.plot.3,
                    #           file = paste0('stat_output\\',output,'.lmer_fdr0.05_fc1.5.volcano.csv'))
                    
                    
                }
                
                # print(p1)
                # print(p2 + p3)
                
            }
            
            
            
        }
        if(t.flag) View(data.stats)
        return(data.stats)
    })
    
    observeEvent(input$stat_go, {
        req(stat_calc())
        a<-stat_calc()
        output$data_stats<-DT::renderDataTable({a})
    })
    
    # output$data_stats <- DT::renderDataTable({
    #     req(stat_calc())
    # 
    # 
    #     stat_calc()
    # })
       
}




# Run the application 
shinyApp(ui = ui, server = server)
