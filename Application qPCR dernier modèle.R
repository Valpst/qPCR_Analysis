library(shiny)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(plotly)
library(DT)
library(shinycssloaders)
library(bslib)
library(writexl)
library(shinyFiles)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Poppins"),
    heading_font = font_google("Poppins"),
    primary = "#2c3e50"
  ),
  
  div(
    style = "background-color: #2c3e50; padding: 20px; margin-bottom: 20px;",
    h1("Plateforme Multianalyse qPCR", 
       style = "color: white; text-align: center; font-weight: 700; font-size: 2.5em;")
  ),
  
  tabsetPanel(
    id = "onglets",
    type = "tabs",
    
    tabPanel("Accueil",
             br(),
             h3("Charger vos fichiers de qPCR"),
             fluidRow(
               column(6,
                      fileInput("file_melt", "Fichier Melting Curve (.xlsx)", accept = ".xlsx"),
                      fileInput("file_cq", "Fichier Cq (.xlsx)", accept = ".xlsx"),
                      fileInput("file_amp", "Fichier Amplification (.xlsx)", accept = ".xlsx"),
                      fileInput("file_plate", "Fichier Plan de Plaque (.xlsx)", accept = ".xlsx")
               )
             ),
             br(),
             textOutput("status")
    ),
    
    tabPanel("Melting Curve",
             sidebarLayout(
               sidebarPanel(
                 actionButton("select_all", "Tout sélectionner / désélectionner"),
                 selectInput("well", "Choisir un ou plusieurs puits :", choices = NULL, multiple = TRUE),
                 sliderInput("x_range", "Plage Température (X)", min = 60, max = 100, value = c(60, 100)),
                 sliderInput("y_range", "Plage Dérivée (Y)", min = -10, max = 500, value = c(-10, 500)),
                 textInput("title_melt", "Titre du graphique :", value = "Courbes de fusion")
               ),
               mainPanel(
                 withSpinner(plotlyOutput("meltingPlot")),
                 downloadButton("download_melt", "Télécharger le graphique Melting"),
                 withSpinner(plotOutput("plateView_melting", click = "plateClick_melt"))
               )
             )
    ),
    
    tabPanel("Tm par puits",
             fluidRow(
               column(6, DTOutput("tmTable"), downloadButton("download_tm_table", "Télécharger le tableau Tm")),
               column(6, withSpinner(plotOutput("tmPlot")), downloadButton("download_tm", "Télécharger le graphique Tm"))
             )
    ),
    
    tabPanel("Quantification",
             fluidRow(
               column(6, DTOutput("cqTable"), downloadButton("download_cq_table", "Télécharger le tableau Cq")),
               column(6, withSpinner(plotOutput("cqPlot")), downloadButton("download_cq", "Télécharger le graphique Cq"))
             )
    ),
    
    tabPanel("Amplification",
             sidebarLayout(
               sidebarPanel(
                 actionButton("select_all_amp", "Tout sélectionner / désélectionner"),
                 selectInput("amp_wells", "Choisir un ou plusieurs puits :", choices = NULL, multiple = TRUE),
                 sliderInput("x_range_amp", "Plage Cycles (X)", min = 0, max = 50, value = c(0, 50)),
                 sliderInput("y_range_amp", "Plage RFU (Y)", min = 0, max = 3000, value = c(0, 3000)),
                 textInput("title_amp", "Titre du graphique :", value = "Courbes d'amplification")),
               mainPanel(
                 withSpinner(plotlyOutput("amplificationPlot")),
                 downloadButton("download_amp", "Télécharger le graphique Amplification"),
                 withSpinner(plotOutput("plateView_amp", click = "plateClick_amp"))
               )
             )
    ),
    
    tabPanel("Détection positive",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("cq_range", "Plage de Cq (cycles détectés)", min = 0, max = 50, value = c(15, 30)),
                 downloadButton("export_positifs", "Exporter les puits positifs")
               ),
               mainPanel(
                 withSpinner(plotOutput("positiveDetectionPlot"))
               )
             )
    )
  ) # ← fermeture correcte du tabsetPanel
)   # ← fermeture correcte du fluidPage


server <- function(input, output, session) {
  selected_wells <- reactiveVal(character(0))
  
  file_path_melt <- reactiveVal(NULL)
  file_path_cq <- reactiveVal(NULL)
  file_path_amp <- reactiveVal(NULL)
  file_path_plate <- reactiveVal(NULL)
  
  observeEvent(input$file_melt, {
    path <- file.path(tempdir(), input$file_melt$name)
    file.copy(input$file_melt$datapath, path, overwrite = TRUE)
    file_path_melt(path)
  })
  
  observeEvent(input$file_cq, {
    path <- file.path(tempdir(), input$file_cq$name)
    file.copy(input$file_cq$datapath, path, overwrite = TRUE)
    file_path_cq(path)
  })
  
  observeEvent(input$file_amp, {
    path <- file.path(tempdir(), input$file_amp$name)
    file.copy(input$file_amp$datapath, path, overwrite = TRUE)
    file_path_amp(path)
  })
  
  observeEvent(input$file_plate, {
    path <- file.path(tempdir(), input$file_plate$name)
    file.copy(input$file_plate$datapath, path, overwrite = TRUE)
    file_path_plate(path)
  })
  
  handleClick <- function(coord) {
    if (!is.null(coord)) {
      col_clicked <- sprintf("%02d", round(coord$x))
      row_clicked <- LETTERS[8:1][round(coord$y)]
      well_clicked <- toupper(paste0(row_clicked, col_clicked))
      wells <- selected_wells()
      if (well_clicked %in% wells) {
        selected_wells(setdiff(wells, well_clicked))
      } else {
        selected_wells(union(wells, well_clicked))
      }
    }
  }
  
  observeEvent(input$plateClick_melt, { handleClick(input$plateClick_melt) })
  observeEvent(input$plateClick_amp, { handleClick(input$plateClick_amp) })
  observeEvent(event_data("plotly_click", source = "melting"), {
    d <- event_data("plotly_click", source = "melting")
    if (!is.null(d)) {
      clicked_well <- d$curveNumber + 1
      wells_list <- unique(data_melt()$Well)
      if (clicked_well <= length(wells_list)) {
        well <- wells_list[clicked_well]
        current <- selected_wells()
        if (well %in% current) {
          selected_wells(setdiff(current, well))
        } else {
          selected_wells(union(current, well))
        }
      }
    }
  })
  
  observeEvent(event_data("plotly_click", source = "amplification"), {
    d <- event_data("plotly_click", source = "amplification")
    if (!is.null(d)) {
      clicked_well <- d$curveNumber + 1
      wells_list <- unique(data_amp()$Well)
      if (clicked_well <= length(wells_list)) {
        well <- wells_list[clicked_well]
        current <- selected_wells()
        if (well %in% current) {
          selected_wells(setdiff(current, well))
        } else {
          selected_wells(union(current, well))
        }
      }
    }
  })
  
  observe({
    updateSelectInput(session, "well", selected = selected_wells())
    updateSelectInput(session, "amp_wells", selected = selected_wells())
  })
  
  observeEvent(input$well, { selected_wells(input$well) })
  observeEvent(input$amp_wells, { selected_wells(input$amp_wells) })
  
  observeEvent(input$select_all, {
    wells <- unique(data_melt()$Well)
    if (setequal(wells, selected_wells())) {
      selected_wells(character(0))
    } else {
      selected_wells(wells)
    }
  })
  
  observeEvent(input$select_all_amp, {
    wells <- unique(data_amp()$Well)
    if (setequal(wells, selected_wells())) {
      selected_wells(character(0))
    } else {
      selected_wells(wells)
    }
  })
  
  data_melt <- reactive({
    req(file_path_melt())
    df <- read_excel(file_path_melt(), sheet = 1)
    df_long <- pivot_longer(df, cols = -Temperature, names_to = "Well", values_to = "Derivative")
    df_long$Derivative <- as.numeric(gsub(",", ".", as.character(df_long$Derivative)))
    df_long$Temperature <- as.numeric(df_long$Temperature)
    df_long <- df_long %>% mutate(
      Well = toupper(gsub("\\s+", "", Well)),
      Row = substr(Well, 1, 1),
      Col = substr(Well, 2, nchar(Well)),
      Col = ifelse(nchar(Col) == 1, paste0("0", Col), Col),
      Well = paste0(Row, Col)
    )
    df_long
  })
  
  data_cq <- reactive({
    req(file_path_cq())
    df <- read_excel(file_path_cq(), sheet = 1)
    df <- df %>%
      select(Well, Sample, Cq) %>%
      filter(!is.na(Cq)) %>%
      mutate(Well = toupper(gsub("\\s+", "", Well)))
    df
  })
  
  data_amp <- reactive({
    req(file_path_amp())
    df <- read_excel(file_path_amp(), sheet = 1)
    df_long <- pivot_longer(df, cols = -Cycle, names_to = "Well", values_to = "RFU") %>%
      mutate(
        Well = toupper(gsub("\\s+", "", Well)),
        Row = substr(Well, 1, 1),
        Col = substr(Well, 2, nchar(Well)),
        Col = ifelse(nchar(Col) == 1, paste0("0", Col), Col),
        Well = paste0(Row, Col)
      )
    df_long$RFU <- as.numeric(df_long$RFU)
    df_long$Cycle <- as.numeric(df_long$Cycle)
    df_long
  })
  
  plate_data <- reactive({
    req(file_path_plate())
    df <- read_excel(file_path_plate())
    df_rows <- df[seq(2, nrow(df), by = 3), ]
    colnames(df_rows)[1] <- "Row"
    plate_long <- pivot_longer(df_rows, cols = -Row, names_to = "Col", values_to = "Sample") %>%
      mutate(
        Col = gsub("^([1-9])$", "0\\1", as.character(Col)),
        Well = toupper(paste0(Row, Col))
      )
    plate_long
  })
  
  plot_plate_colored <- function(df, color_col) {
    df <- df %>% mutate(Selected = Well %in% selected_wells())
    ggplot(df, aes(x = as.numeric(Col), y = Row, fill = .data[[color_col]])) +
      geom_tile(aes(alpha = Selected), color = "black") +
      geom_text(aes(label = Well), color = "white", size = 3) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = FALSE) +
      scale_y_discrete(limits = rev(LETTERS[1:8])) +
      scale_x_continuous(breaks = 1:12) +
      theme_minimal() +
      theme(panel.grid = element_blank()) +
      labs(x = "Colonne", y = "Ligne")
  }
  
  output$meltingPlot <- renderPlotly({
    req(selected_wells(), data_melt(), plate_data())
    
    df <- data_melt()
    df_plate <- plate_data()
    
    df <- left_join(df, df_plate, by = "Well") %>%
      filter(Well %in% selected_wells())
    if (nrow(df) == 0) return(NULL)
    
    x_min <- input$x_range[1]
    x_max <- input$x_range[2]
    y_min <- input$y_range[1]
    y_max <- input$y_range[2]
    
    p <- ggplot(df, aes(x = Temperature, y = Derivative, color = Sample, group = Well)) +
      geom_line(linewidth = 0.6) +
      coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
      theme_minimal() +
      labs(
        title = gsub("\\\\n", "\n", input$title_melt),
        x = "Température",
        y = "-d(RFU)/dT",
        color = "Échantillon"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, lineheight = 1.2)
      )
    
    ggplotly(p, source = "melting")
  })
  
  output$amplificationPlot <- renderPlotly({
    req(selected_wells(), data_amp(), plate_data())
    
    df <- data_amp()
    df_plate <- plate_data()
    
    df <- left_join(df, df_plate, by = "Well") %>%
      filter(Well %in% selected_wells())
    if (nrow(df) == 0) return(NULL)
    
    x_min <- input$x_range_amp[1]
    x_max <- input$x_range_amp[2]
    y_min <- input$y_range_amp[1]
    y_max <- input$y_range_amp[2]
    
    p <- ggplot(df, aes(x = Cycle, y = RFU, color = Sample, group = Well)) +
      geom_line(linewidth = 0.6) +
      coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
      theme_minimal() +
      labs(
        title = gsub("\\\\n", "\n", input$title_amp),
        x = "Cycle",
        y = "RFU",
        color = "Échantillon"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, lineheight = 1.2)
      )
    ggplotly(p, source = "amplification")
  })
  
  output$plateView_melting <- renderPlot({
    plot_plate_colored(plate_data(), "Sample")
  })
  
  output$plateView_amp <- renderPlot({
    plot_plate_colored(plate_data(), "Sample")
  })
  
  output$positiveDetectionPlot <- renderPlot({
    req(data_cq(), plate_data())
    df_cq <- data_cq()
    df_plate <- plate_data()
    cq_min <- input$cq_range[1]
    cq_max <- input$cq_range[2]
    
    df_merged <- left_join(df_plate, df_cq, by = "Well") %>%
      mutate(status = case_when(
        is.na(Cq) ~ "NA",
        Cq >= cq_min & Cq <= cq_max ~ "Positif",
        TRUE ~ "Hors plage"
      ))
    
    ggplot(df_merged, aes(x = as.numeric(Col), y = Row, fill = status)) +
      geom_tile(color = "black") +
      geom_text(aes(label = Well), color = "white", size = 3) +
      scale_y_discrete(limits = rev(LETTERS[1:8])) +
      scale_x_continuous(breaks = 1:12) +
      scale_fill_manual(values = c("Positif" = "forestgreen", "Hors plage" = "firebrick", "NA" = "lightgrey")) +
      theme_minimal() +
      labs(title = "Détection positive selon plage de Cq", x = "Colonne", y = "Ligne")
  })
  
  positifs_data <- reactive({
    req(data_cq(), plate_data())
    cq_min <- input$cq_range[1]
    cq_max <- input$cq_range[2]
    
    df <- left_join(plate_data(), data_cq(), by = "Well") %>%
      filter(!is.na(Cq) & Cq >= cq_min & Cq <= cq_max)
    
    df
  })
  
  output$tmTable <- renderDT({
    df <- data_melt()
    tm <- df %>%
      group_by(Well) %>%
      filter(Derivative == max(Derivative, na.rm = TRUE)) %>%
      select(Well, Tm = Temperature, Max_Derivative = Derivative) %>%
      distinct()
    datatable(tm)
  })
  
  output$tmPlot <- renderPlot({
    df <- data_melt()
    tm <- df %>%
      group_by(Well) %>%
      filter(Derivative == max(Derivative, na.rm = TRUE)) %>%
      select(Well, Tm = Temperature, Max_Derivative = Derivative) %>%
      distinct()
    mean_tm <- mean(tm$Tm, na.rm = TRUE)
    ggplot(tm, aes(x = Well, y = Tm, fill = abs(Tm - mean_tm) > 1)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"), guide = FALSE) +
      theme_minimal() +
      labs(x = "Puits", y = "Tm (°C")
  })
  
  output$cqTable <- renderDT({
    datatable(data_cq())
  })
  
  output$cqPlot <- renderPlot({
    df <- data_cq()
    ggplot(df, aes(x = Sample, y = Cq)) +
      geom_boxplot(fill = "skyblue") +
      geom_jitter(width = 0.2) +
      theme_minimal()
  })
  
  output$export_positifs <- downloadHandler(
    filename = function() {
      paste0("puits_positifs_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(positifs_data(), file, row.names = FALSE)
    }
  )
  # === EXPORT Tm ===
  output$download_tm_table <- downloadHandler(
    filename = function() { paste0("tableau_Tm_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- data_melt() %>%
        group_by(Well) %>%
        filter(Derivative == max(Derivative, na.rm = TRUE)) %>%
        select(Well, Tm = Temperature, Max_Derivative = Derivative) %>%
        distinct()
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_tm <- downloadHandler(
    filename = function() { paste0("graphique_Tm_", Sys.Date(), ".png") },
    content = function(file) {
      df <- data_melt() %>%
        group_by(Well) %>%
        filter(Derivative == max(Derivative, na.rm = TRUE)) %>%
        select(Well, Tm = Temperature, Max_Derivative = Derivative) %>%
        distinct()
      mean_tm <- mean(df$Tm, na.rm = TRUE)
      p <- ggplot(df, aes(x = Well, y = Tm, fill = abs(Tm - mean_tm) > 1)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "red"), guide = FALSE) +
        theme_minimal() +
        labs(x = "Puits", y = "Tm (°C)")
      ggsave(file, plot = p, width = 8, height = 5, bg = "white")
    }
  )
  
  # === EXPORT Cq ===
  output$download_cq_table <- downloadHandler(
    filename = function() { paste0("tableau_Cq_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(data_cq(), file, row.names = FALSE)
    }
  )
  
  output$download_cq <- downloadHandler(
    filename = function() { paste0("graphique_Cq_", Sys.Date(), ".png") },
    content = function(file) {
      df <- data_cq()
      p <- ggplot(df, aes(x = Sample, y = Cq)) +
        geom_boxplot(fill = "skyblue") +
        geom_jitter(width = 0.2) +
        theme_minimal()
      ggsave(file, plot = p, width = 8, height = 5, bg = "white")
    }
  )
  
  output$download_melt <- downloadHandler(
    filename = function() { paste0("graphique_Melting_", Sys.Date(), ".png") },
    content = function(file) {
      df <- data_melt() %>% filter(Well %in% selected_wells())
      df <- left_join(df, plate_data(), by = "Well")
      
      x_min <- input$x_range[1]
      x_max <- input$x_range[2]
      y_min <- input$y_range[1]
      y_max <- input$y_range[2]
      
      # Convertir \n en retour à la ligne
      title_melt_clean <- gsub("\\\\n", "\n", input$title_melt)
      
      p <- ggplot(df, aes(x = Temperature, y = Derivative, color = Sample, group = Well)) +
        geom_line(linewidth = 0.6) +
        coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
        theme_minimal() +
        labs(x = "Température", y = "-d(RFU)/dT", color = "Échantillon") + ggtitle(title_melt_clean) +
        theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2),
          plot.background = element_rect(fill = "white", color = NA))
      
      ggsave(file, plot = p, width = 8, height = 5, dpi = 300, bg = "white")
    }
  )
  
  # === EXPORT Amplification ===
  output$download_amp <- downloadHandler(
    filename = function() { paste0("graphique_Amplification_", Sys.Date(), ".png") },
    content = function(file) {
      df <- data_amp() %>% filter(Well %in% selected_wells())
      df <- left_join(df, plate_data(), by = "Well")
      
      title_amp_clean <- gsub("\\\\n", "\n", input$title_amp)
      
      p <- ggplot(df, aes(x = Cycle, y = RFU, color = Sample, group = Well)) +
        geom_line(linewidth = 0.6) +
        coord_cartesian(xlim = input$x_range_amp, ylim = input$y_range_amp) +
        theme_minimal() +
        labs(x = "Cycle", y = "RFU", color = "Échantillon") + ggtitle(title_amp_clean) +
        theme(plot.title = element_text(hjust = 0.5, lineheight = 1.2),
          plot.background = element_rect(fill = "white", color = NA))
      
      ggsave(file, plot = p, width = 8, height = 5, dpi = 300, bg = "white")
    }
  )
}

shinyApp(
  ui = tagList(
    ui,
    tags$footer(
      tags$div("© 2025 Valérian Post - Tous droits réservés", style = "text-align:center; padding:10px; font-size:0.9em; color:gray;")
    )
  ),
  server = server
)

