library(shiny)
library(shinyglide)
library(ggplot2)
library(dplyr)
library(tidyr)
library(mc2d)
library(ggplot2)
library(ggthemes)
library(scales)
library(showtext)
library(flextable)
library(bslib)
library(DT)
library(officer)
library(shinyjs)
library(mschart)


ccs_theme <- bs_theme(
  bootswatch = "minty",
  # bg = "#FFFFFF",
  # fg = "#000000",
  primary = "#254a5d",
  secondary = "#ecb22d",
  success = "#65baaf",
  info = "#2dccd3",
  warning = "#e1523d",
  danger = "#e1523d",
  base_font = "Karla"
)

dollar_k_m_b_fmt <- function(x){
  label_number(scale_cut = cut_short_scale(), prefix = "$", accuracy = 0.1)(x)
}

get_hist_freq_values <- function(p) {
  d <- ggplot_build(p)$data[[1]]
  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y)
}

# font_add(family = "Aptos", 
#          regular = "Aptos-Regular.ttf",
#          bold = "Aptos-Bold.ttf",
#          italic = "Aptos-Italic.ttf",
#          bolditalic = "Aptos-BoldItalic.ttf")
# 
# showtext_auto()


set_flextable_defaults(
  font.size = 10, font.family = "Aptos",
  font.color = "#757575",
  table.layout = "fixed",
  border.color = "#718A96")

rounded_currency <- function(){
  label_number(scale_cut = cut_short_scale(), prefix = "$", accuracy = 0.01)
}

# Define UI for data upload app ----
ui_file_upload <-
  fluidRow(
    column(
      4,
      style = "background-color:#ebeef0;padding:25px",

      textInput("inp_txt_org_name", label = "Organization", placeholder = "enter organization name"),
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
        multiple = TRUE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      # tags$hr(),
      h4("Map Columns"),
      fluidRow(
        column(
          4,
          selectInput("inp_sel_low_amt", "Est. Floor", choices = NULL)
        ),
        column(
          4,
          selectInput("inp_sel_likely_amt", "Est. Likely", choices = NULL)
        ),
        column(
          4,
          selectInput("inp_sel_stretch_amt", "Est. Aspirational", choices = NULL)
        )

      ),
      tags$hr(),
      p("Note: The floor, likely, and aspirational values are required. Ensure that those columns are accurately mapped.")
    ),
    column(
      8,
      DT::DTOutput("op_tbl_data_preview", width = "100%"),
      verbatimTextOutput("op_validation_msgs")
    )
  )
  

ui_simulation_results <- 

  # two column output for a histogram plot and a summary table
  fluidRow(
    column(
      width = 3,
      wellPanel(
      # sidebar panel for inputs: 1) number of simulations as a slider, 2) download button for a powerpoint presentation

        sliderInput("inp_sld_number_simulations", "Number of Simulations", min = 1000, max = 10000, value = 10000, step = 1000),
        actionButton("inp_btn_run_simulation", "Run Simulation", style = "width: 90%"),
        br(),
        br(),
        # --- NEW (1): chart type toggle ---
        radioButtons(
          inputId  = "inp_rdo_chart_type",
          label    = "PowerPoint Chart Type",
          choices  = c("Editable Chart" = "mschart",
                       "Static Image"              = "ggplot"),
          selected = "mschart",
          inline   = FALSE
        ),
        # --- END NEW ---
        br(),
        # https://stackoverflow.com/a/53638835 keep the button hidden until results are gen
        shinyjs::hidden(downloadButton(outputId = "op_btn_download_pptx", label = "Download PowerPoint", style = "width: 90%" ))

    )),
    column(width = 6, plotOutput("op_plot_histogram")),
    column(width = 3, uiOutput("op_tbl_summary"))
  )

#plot_table_side_by_side
#graph_caption
#title
#subtitle
#left_plot
#right_table

ui <- fluidPage(
  theme = ccs_theme,
  shinyjs::useShinyjs(),

  # App title ----
  titlePanel("Campaign Simulation"),
  glide(
    height = "100%",
    controls_position = "top",
    screen(
      h1("Upload Data"),
      ui_file_upload,
      # Disable Next button until inp_sel_low_amt, inp_sel_likely_amt, and inp_sel_stretch_amt has values
      next_condition = "input.inp_sel_low_amt.length > 0 && input.inp_sel_likely_amt.length > 0  && input.inp_sel_stretch_amt.length > 0 && output.op_validation_msgs.length == 0"
      #next_condition = "output.op_tbl_data_preview !== undefined"
    ),
    screen(
      h1("Simulation Results"),
      ui_simulation_results
    )
  )
)


# Define server logic to read selected file ----
server <- function(input, output, session) {
  donor_input_data <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    df <- read.csv(input$file1$datapath,
      header = TRUE
    )
  })
  
  observe({
    updateSelectInput(session, "inp_sel_low_amt", choices = names(donor_input_data()), selected = '')
    updateSelectInput(session, "inp_sel_likely_amt", choices = names(donor_input_data()), selected = '')
    updateSelectInput(session, "inp_sel_stretch_amt", choices = names(donor_input_data()), selected = '')
  })
  
  
  # donor_input_data_amt_colnames <- reactive({
  #   req(donor_input_data())
  #   
  #   donor_input_data() %>% 
  #     select(ends_with("amount")) %>% 
  #     colnames()
  # })
  
  output$op_tbl_data_preview <- DT::renderDT({
    donor_input_data() %>% 
      DT::datatable(
        rownames = FALSE,
        options = list(autoWidth = TRUE, dom = 'tp', scrollX = TRUE)
        ) #%>% 
      #formatCurrency(columns = donor_input_data_amt_colnames(), digits = 0)
  })
  
  ## rename the columns
  donor_data_renamed <- reactive({
    req(input$inp_sel_low_amt, input$inp_sel_likely_amt, input$inp_sel_stretch_amt)
    
    df <- donor_input_data() %>%
      rename(
        low_gift_amount = input$inp_sel_low_amt, 
        likely_gift_amount = input$inp_sel_likely_amt, 
        stretch_gift_amount = input$inp_sel_stretch_amt)
    
    df
  })
  
  check_row_values <- reactive({
    
    req(donor_data_renamed())
    
    # First check for NULL/NA values
    null_check_df <- donor_data_renamed() %>%
      summarise(
        low_nulls = sum(is.na(low_gift_amount)),
        likely_nulls = sum(is.na(likely_gift_amount)),
        stretch_nulls = sum(is.na(stretch_gift_amount))
      ) %>%
      pivot_longer(everything(), names_to = "column", values_to = "null_count") %>%
      filter(null_count > 0) %>%
      mutate(
        column = case_when(
          column == "low_nulls" ~ "Floor",
          column == "likely_nulls" ~ "Likely",
          column == "stretch_nulls" ~ "Aspirational"
        ),
        error_msg = glue::glue("Warning: {column} column contains {null_count} missing values\n")
      )
    
    # Then do the existing value comparison checks
    value_check_df <- donor_data_renamed() %>%
      mutate(
        is_likely_lower_than_low = low_gift_amount > likely_gift_amount,
        is_stretch_lower_than_likely = likely_gift_amount > stretch_gift_amount,
        is_stretch_lower_than_low =  low_gift_amount > stretch_gift_amount
      ) %>%
      mutate(any_value_lower_than_exp = is_likely_lower_than_low +  is_stretch_lower_than_likely + is_stretch_lower_than_low) %>% 
      mutate(row_id = row_number()) %>% 
      filter(any_value_lower_than_exp > 0) %>%
      mutate(
        error_msg = glue::glue("Check row number {row_id} for accuracy. One of the lower end values is greater than the higher end values.\n Low: {dollar(low_gift_amount)}, Likely: {dollar(likely_gift_amount)}, Aspirational: {dollar(stretch_gift_amount)}")
      )
    
    # Combine both validation messages
    list(
      null_check = null_check_df,
      value_check = value_check_df
    )
  })
  
  output$op_validation_msgs <- renderPrint({
    req(check_row_values())
    
    # Print NULL/NA warnings first
    if (nrow(check_row_values()$null_check) > 0) {
      cat("Missing Value Warnings:\n")
      cat(check_row_values()$null_check$error_msg, sep = "")
      cat("\n")
    }
    
    # Print value comparison warnings
    if (nrow(check_row_values()$value_check) > 0) {
      cat("Value Comparison Warnings:\n")
      cat(check_row_values()$value_check$error_msg, sep = "\n")
    }
  })

  
  # generate rpert simulations
  donor_data_with_pert <- reactive({
    donor_data_renamed() %>% 
        rowwise() %>%
      # this step will create n random values per row
      mutate(random_deviates = list(
        rpert(n = input$inp_sld_number_simulations, 
              min = low_gift_amount, 
              mode = likely_gift_amount, 
              max = stretch_gift_amount)))
  }) %>% # bind event to run only when inp_btn_run_simulation is clicked
    bindEvent(input$inp_btn_run_simulation)
  
  # sum the totals for each simulation
  sim_totals <- reactive({
    req(donor_data_with_pert())
    # this step adds the simulation values by column; so all simulation # 1 run values get added
    data.frame(sim_totals = Reduce(`+`, donor_data_with_pert()$random_deviates))
  })

  ## create a summary df
  sim_totals_smry <- reactive({
    
    req(sim_totals())
    
    n_records <- nrow(donor_data_renamed())
    
    sim_totals() %>%
      summarize(
        lowest_total = min(sim_totals),
        highest_total = max(sim_totals),
        avg_total = mean(sim_totals),
        med_total = median(sim_totals),
        bottom_10_pct = quantile(sim_totals, probs = .05), # the bottom 5 percentile
        top_10_pct = quantile(sim_totals, probs = .95) # the top 10 percentile
      ) %>%
      mutate(across(.fns = ~ label_number(scale_cut = cut_short_scale(), prefix = "$", accuracy = 0.1)(.x), .names = "{.col}_fmt")) %>%
      mutate(n90_pct_range = paste(bottom_10_pct_fmt, "-", top_10_pct_fmt),
      n_records_fmt = comma(n_records))
  })

  
  sim_hist <- reactive({
    req(sim_hist_for_pptx())
    sim_hist_for_pptx()
  })
  
  #create editable bar chart version for PowerPoint export
  
  round_to_half_million <- function(x) {
    round(x / 5e5) * 5e5
  }
  
  sim_hist_freq_values <- reactive({
    req(sim_hist())

    df <- get_hist_freq_values(sim_hist()) %>% 
      mutate(
        x_range = ifelse(
          dplyr::row_number() %% 4 == 1,
          dollar_k_m_b_fmt(.data$x),
          ""
        )
      )
    df
  })
  
   ms_bar_chart <- reactive({
  
     req(sim_hist_freq_values())
     this_ms_barchart <- ms_barchart(data = sim_hist_freq_values(),
                                     x = "x_range",
                                     y = "y") %>%
       chart_data_labels(show_legend_key = FALSE) %>%
       chart_labels() %>%
       chart_data_fill(values = "#133C50") %>%
       chart_data_stroke(values = "white" ) %>%
       chart_ax_x(cross_between = "midCat", major_tick_mark = "none") %>%
       chart_settings(gap_width = 0)
  
     this_chart_theme <- mschart_theme(
       legend_position = "n",
       grid_major_line = fp_border(color = NA),
       axis_ticks_x = fp_border(color = "black", width = 1),
       axis_ticks_y = fp_border(color = NA, width = 0),
       axis_text_y = fp_text(color = NA),
       axis_text_x = fp_text(
         font.family = "Aptos",
         bold = TRUE,
         color = "#133C50"
       )
     )
  
     set_theme(this_ms_barchart, this_chart_theme)
  
   })
  
   # NEW (2) reactive: ggplot histogram formatted for PowerPoint export
   
   choose_x_break_step <- function(x_min, x_max) {
     span <- x_max - x_min
     
     if (span <= 5e6) {
       5e5       # 0.5M
     } else if (span <= 2e7) {
       2e6       # 2M
     } else if (span <= 5e7) {
       5e6       # 5M
     } else if (span <= 1.5e8) {
       1e7       # 10M
     } else {
       2.5e7     # 25M
     }
   }
   
   sim_hist_for_pptx <- reactive({
     req(sim_totals())
     
     x_vals <- sim_totals()$sim_totals
     x_min <- floor(min(sim_totals()$sim_totals, na.rm = TRUE) / 5e5) * 5e5
     x_max <- ceiling(max(sim_totals()$sim_totals, na.rm = TRUE) / 5e5) * 5e5
     x_step <- choose_x_break_step(x_min, x_max)
     
     p <- ggplot(sim_totals(), aes(x = sim_totals)) +
       geom_histogram(color = "white", fill = "#133C50", bins = 30, linewidth = 0.25) +
       theme_minimal(base_family = "Aptos", base_size = 10) +
       theme(
         axis.text.y      = element_blank(),
         axis.ticks.y     = element_blank(),
         axis.title       = element_blank(),
         axis.line.y      = element_blank(),
         axis.line.x      = element_blank(),
         axis.ticks.x     = element_blank(),
         axis.text.x      = element_text(
              family = "Aptos",
              face = "bold",
              size = 10,
              color = "#133C50"
         ),
         panel.grid.major.x = element_line(color = "#FFFFFF", linewidth = 0.6),
         panel.grid.major.y = element_line(color = "#FFFFFF", linewidth = 0.6),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "#F3F6F6", color = NA),
         plot.background  = element_rect(fill = "#F3F6F6", color = NA),
         plot.margin = margin(t = 8, r = 12, b = 8, l = 12)
       ) +
       scale_x_continuous(
         breaks = seq(x_min, x_max, by = x_step),
         labels = label_number(
           scale_cut = cut_short_scale(),
           prefix    = "$",
           accuracy  = 0.1
         ),
         minor_breaks = NULL,
         expand = expansion(mult = c(0, 0.01))
       )
     
     p
   })
   
  sim_smry_ft <- reactive({
    req(sim_totals_smry())

    smry_table_ft <- sim_totals_smry() %>%
      select(
        `Constituents` = n_records_fmt,
        Lowest = lowest_total_fmt,
        Highest = highest_total_fmt,
        Average = avg_total_fmt,
        Median = med_total_fmt,
        `90% Range` = n90_pct_range
      ) %>%
      pivot_longer(cols = everything()) %>%
      flextable() %>%
      set_header_labels(values = list(name = "Simulation Results", value = "")) %>%
      merge_at(j = c(1, 2), part = "header") %>%
      bg(part = "header", bg = "#133C50") %>%
      color(part = "header", color = "white") %>%
      bold(part = "header", bold = TRUE) %>%
      color(part = "body", color = "#133C50") %>%
      bg(i = c(1, 3, 5), bg = "#F3F6F6", part = "body") %>%
      border_remove() %>%
      hline_top(part = "header", border = fp_border(color = "#133C50", width = 1.5)) %>%
      hline_bottom(part = "body", border = fp_border(color = "#133C50", width = 1.5)) %>%
      align(j = 1, align = "left", part = "body") %>%
      align(j = 2, align = "right", part = "body") %>%
      align(align = "left", part = "header") %>%
      fontsize(size = 10, part = "all") %>%
      padding(padding = 2, part = "all") %>%
      height(part = "header", height = 0.31) %>% 
      height(part = "body", height = 0.31) %>%
      width(j = 1, width = 1.2) %>%
      width(j = 2, width = 2.0) %>% 
      set_table_properties(layout = "fixed")
    
    smry_table_ft
  })

  #     hline( i = 1, j = 1:2, part = "header", border = fp_border(color = "#133C50") ) %>%
  #     hline_top(j = 1:2, part = "body", border = fp_border(color = "#133C50") ) %>%
  #     merge_at(j = c(1, 2), part = "header") %>%
  #     hline( i = 1, j = 1:2, part = "header", border = fp_border(color = "#133C50") ) %>%
  #   #  hline( i = 1, j = 1:2, part = "body", border = fp_border(color = "red") ) %>%
  #     set_header_labels(values = list(name = "Simulation Results", value = "")) %>%
  #     align(j = 2, align = "right", part = "all") %>%
  #     width(width = c(1.2, 2))
  # 
  # 
  # 
  # 
  #   smry_table_ft
  # })
  
  output$op_plot_histogram <- renderPlot({
    sim_hist()
    
    # annotation within the plot
    # annotation_text_for_range <- with(sim_totals_smry,
    #                                   paste("90% of outcomes fall between", n90_pct_range))
    # 
    # g <- g + geom_text(
    #   aes(
    #     y = 99,
    #     x = sim_totals_smry$avg_total,
    #     label = annotation_text_for_range
    #   ),
    #   size = rel(4),
    #   inherit.aes = FALSE
    # )
  }) %>% # bind event to run only when inp_btn_run_simulation is clicked
    bindEvent(input$inp_btn_run_simulation)

  output$op_tbl_summary <- renderUI({
    
    sim_smry_ft() %>% 
      htmltools_value

  }) %>% # bind event to run only when inp_btn_run_simulation is clicked
    bindEvent(input$inp_btn_run_simulation)
  
  # https://stackoverflow.com/a/53638835 
  # show the button after hist and tables are created
  observeEvent(input$inp_btn_run_simulation, {
    if (anyNA(c(sim_hist(), sim_smry_ft())))
      shinyjs::hide("op_btn_download_pptx")
    else
      shinyjs::show("op_btn_download_pptx")
  })
  
  # 
  sim_pres_slides <- reactive({

    range_text <- sim_totals_smry()$n90_pct_range
    brace_img_file <- file.path("curly_brace_v2.png")

    my_pres <- read_pptx(path = "template.pptx") 
    
    my_pres <- on_slide(my_pres, index = 1)
    ## add org name and date
    my_pres <- ph_with(my_pres,
                       value = format(Sys.Date(), "%B %Y"),
                       location = ph_location_label(
                         ph_label = "month_year"
                       )
    )
    
    ## add org name and date
    my_pres <- ph_with(my_pres,
                       value = input$inp_txt_org_name,
                       location = ph_location_label(
                         ph_label = "orgname"
                       )
    )
    
    
    
    # month_year format(Sys.Date(), "%B %Y")
    # orgname
    # Presentation_TItle
    
    my_pres <- on_slide(my_pres, index = 2)
    
    # my_pres <- my_pres %>%
    #   add_slide(layout = "plot_table_side_by_side")

    # add title
    my_pres <- ph_with(my_pres,
      value = "Fundraising Simulation Outcomes",
      location = ph_location_label(
        ph_label = "title"
      )
    )

    # add subtitle/descriptor text
    my_pres <- ph_with(my_pres,
      value = paste0("The simulation results highlight that the likeliest fundraising outcomes generated from these prospects fall between ", 
                    range_text,
                    "."
                    ),
      location = ph_location_label(
        ph_label = "subtitle"
      )
    )

    # add graph caption
    my_pres <- ph_with(my_pres,
      value = paste("90% of outcomes fall between", range_text),
      location = ph_location_label(
        ph_label = "graph_caption"
      )
    )
    
    # add the number of simulations in the footer
    my_pres <- ph_with(my_pres,
                       value = paste("We ran", comma(input$inp_sld_number_simulations), "simulations for this exercise."),
                       location = ph_location_label(
                         ph_label = "source_footer"
                       )
    )
    
    ## comment out ggplot plot
    # my_pres <- ph_with(my_pres,
    #   value = sim_hist(),
    #   location = ph_location_label(
    #     ph_label = "left_plot"
    #   )
    # )
    
    ## add editable chart
    # my_pres <- ph_with(my_pres,
    #   value = ms_bar_chart(),
    #   location = ph_location_label(
    #     ph_label = "left_plot"
    #   )
    # )
    
    ## ----- Chart insertion: branch on user's chart-type choice -----
    if (input$inp_rdo_chart_type == "mschart") {
      
      # === EXISTING PATH: editable ms_barchart ===
      my_pres <- ph_with(my_pres,
                         value    = ms_bar_chart(),
                         location = ph_location_label(ph_label = "left_plot")
      )
      
    } else {
      
      # === NEW PATH: ggplot as static image ===
      # 1. Save ggplot to a temporary PNG file
      tmp_png <- tempfile(fileext = ".png")
      ggsave(
        filename = tmp_png,
        plot     = sim_hist_for_pptx(),
        width    = 8.27,      # inches — adjust to match your placeholder
        height   = 4.92,      # inches — adjust to match your placeholder
        dpi      = 300,
        bg       = "#F3F6F6"
      )
      
      # 2. Insert the image into the PowerPoint placeholder
      my_pres <- ph_with(my_pres,
                         value    = external_img(
                           src    = tmp_png,
                           width  = 8.27,
                           height = 4.82
                         ),
                         location = ph_location_label(ph_label = "left_plot")
      )
      
    }
    ## ----- End chart insertion -----

    my_pres <- ph_with(my_pres,
      value = sim_smry_ft(),
      location = ph_location_label(
        ph_label = "right_table"
      )
    )
    
    my_pres <- ph_with(
      x = my_pres,
      external_img(brace_img_file),
      location = ph_location_label(
        ph_label = "brace_img"
      ),
      use_loc_size = TRUE
    )
    

    return(my_pres)

  })  %>% # bind event to run only when inp_btn_run_simulation is clicked
    bindEvent(input$inp_btn_run_simulation)
  
  
  # Download Handler
  output$op_btn_download_pptx <- shiny::downloadHandler(
    
   filename = "MC_simulation_results.pptx",
   
   content = function(file) {
     print(x = sim_pres_slides(), target = file)
   }
 )
}


# Run the app ----
shinyApp(ui, server)