library(readr)
library(tidyr)
library(dplyr)
library(here)
library(glue)
library(ggplot2)
library(ggridges)
library(hrbrthemes)
# library(gt)
library(shiny)
library(bslib)
library(reactable)
library(htmlwidgets)
library(ragg)
library(shinyWidgets)

ui <- page_sidebar(
  # theme = bs_theme(version = 5, bootswatch = "yeti"),

  title = "Data Analysis",

  sidebar = sidebar(
    width = 500,
    # title = "input",
    fileInput(
      "file",
      "Select TSV file",
      multiple = F,
      accept = c(".tsv"),
      width = "100%"
    ),
    selectInput(
      "dist",
      "Distribution",
      choices = c(),
      multiple = T,
      width = "100%"
    ),
    switchInput("switch", "Sort by Median", F, labelWidth = "110px")
  ),

  page_navbar(
    # title = "Data Analysis",
    navbar_options = navbar_options(
      bg = "#1c2951", # "#1c2841",  # "#0062cc",
      underline = T
    ),

    nav_panel(
      "Density",
      downloadLink("download_hist", "Download Plot"),
      plotOutput("hist")
    ),
    nav_panel(
      "Line",
      downloadLink("download_line", "Download Plot"),
      plotOutput("line")
    ),
    nav_panel(
      "Summary Statistics",
      downloadLink("download_table", "Download Data"),
      downloadLink("download_table_as_html", "Download HTML"),
      # tableOutput("table")
      reactableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 500 * 1024^2)

  df_raw <- reactive({
    req(input$file)

    read_tsv(
      input$file$datapath,
      show_col_types = F
    )
  })

  file_name <- reactive({
    tools::file_path_sans_ext(input$file$name)
  })

  observeEvent(input$file, {
    list_dist <- unique(df_raw()$distribution)

    freezeReactiveValue(input, "dist")

    updateSelectInput(
      session,
      "dist",
      choices = list_dist,
      selected = list_dist[1:3],
    )

    df <- reactive({
      df_raw() |>
        filter(distribution %in% input$dist)
    })

    df_stats <- reactive({
      df() |>
        group_by(distribution) |>
        summarize(
          Mean = mean(data),
          Median = median(data),
          Min = min(data),
          Max = max(data),
          "Std Dev" = sd(data),
          Num = n(),
          .groups = "drop_last"
        ) |>
        ungroup()
    })

    observeEvent(input$switch, {
      if (input$switch) {
        ordered_distribution <- reactive({
          df_stats() |> arrange(Median) |> pull(distribution)
        })
      } else {
        ordered_distribution <- reactive(rev(input$dist))
      }

      #
      # distribution plot
      #
      hist_plot <- reactive({
        ggplot(
          df() |>
            mutate(
              distribution = factor(
                distribution,
                levels = ordered_distribution()
              )
            ),
          aes(y = distribution, x = data),
        ) +
          stat_density_ridges(
            quantile_lines = T,
            alpha = 0.4,
            scale = 0.9,
            quantiles = 2
          ) +
          labs(title = file_name(), y = NULL) +
          theme_ipsum_rc() +
          scale_color_ft() +
          scale_fill_ft()
      })

      output$hist <- renderPlot(
        {
          hist_plot()
        },
        height = exprToFunction(150 * length(input$dist))
      )

      output$download_hist <- downloadHandler(
        filename = function() {
          glue("{file_name()}_hist.png")
        },
        content = function(file) {
          agg_png(
            file,
            width = 6,
            height = ifelse(
              length(input$dist) < 5,
              3.7,
              3.7 / 4 * length(input$dist)
            ),
            units = "in",
            res = 300,
            scaling = 0.5
          )
          print(hist_plot())
          dev.off()
          # ggsave(
          #   file,
          #   plot = hist_plot(),
          #   width = 10,
          #   height = ifelse(
          #     length(input$dist) < 5,
          #     6.18,
          #     6.18 / 4 * length(input$dist)
          #   ),
          #   bg = "white"
          # )
        }
      )

      #
      # line plot
      #
      line_plot <- reactive({
        ggplot(
          df() |>
            mutate(
              distribution = factor(
                distribution,
                levels = rev(ordered_distribution())
              )
            ),
          aes(x = n, y = data),
        ) +
          geom_line() +
          geom_point() +
          facet_grid(
            rows = vars(distribution),
            scales = "free_y"
          ) +
          labs(title = file_name(), y = NULL) +
          theme_ipsum_rc() +
          scale_color_ft() +
          scale_fill_ft() +
          theme(strip.text.y = element_text(angle = 0, hjust = 0))
      })

      output$line <- renderPlot(
        {
          line_plot()
        },
        height = exprToFunction(200 * length(input$dist))
      )

      output$download_line <- downloadHandler(
        filename = function() {
          glue("{file_name()}_line.png")
        },
        content = function(file) {
          agg_png(
            file,
            width = 6,
            height = ifelse(
              length(input$dist) < 5,
              3.7,
              3.7 / 4 * length(input$dist)
            ),
            units = "in",
            res = 300,
            scaling = 0.5
          )
          print(line_plot())
          dev.off()
          # ggsave(
          #   file,
          #   plot = line_plot(),
          #   width = 10,
          #   height = ifelse(
          #     length(input$dist) < 5,
          #     6.18,
          #     6.18 / 4 * length(input$dist)
          #   ),
          #   bg = "white"
          # )
        }
      )

      #
      # format reactable
      #
      BuRd <- function(x) {
        rgb(colorRamp(c("#7fb7d7", "#fc8d59"))(x), maxColorValue = 255)
      }
      tbl <- reactive({
        reactable(
          df_stats(),
          columns = list(
            distribution = colDef(name = "Distribution", align = "left"),
            Mean = colDef(align = "center", format = colFormat(digits = 2)),
            Median = colDef(
              align = "center",
              format = colFormat(digits = 2),
              style = function(value) {
                normalized <- (value - min(df_stats()$Median)) /
                  (max(df_stats()$Median) - min(df_stats()$Median))
                color <- BuRd(normalized)
                list(background = color)
              }
            ),

            Min = colDef(align = "center", format = colFormat(digits = 2)),
            Max = colDef(align = "center", format = colFormat(digits = 2)),
            `Std Dev` = colDef(
              align = "center",
              format = colFormat(digits = 2)
            ),
            Num = colDef(align = "center")
          ),
          pagination = F,
          striped = T,
          compact = T,
          filterable = F,
          highlight = T
        )
      })

      output$table <- renderReactable(tbl())

      #
      # format gt table
      #
      # output$table <- render_gt(
      #   df_stats() |>
      #     gt() |>
      #     data_color(
      #       columns = starts_with("Median"),
      #       method = "numeric",
      #       palette = "RdBu",
      #       domain = c(
      #         min(df_stats()$Median),
      #         max(df_stats()$Median)
      #       )
      #     ) |>
      #     cols_align(
      #       align = "center",
      #       columns = everything()
      #     ) |>
      #     cols_align(
      #       align = "left",
      #       columns = distribution
      #     ) |>
      #     # cols_width(
      #     #   everything() ~ px(100)
      #     # ) |>
      #     # cols_width(
      #     #   distribution ~ px(200)
      #     # ) |>
      #     cols_label(
      #       distribution = md("**Distribution**")
      #     ) |>
      #     fmt_number(c(Min, Mean, Median, Max, `Std Dev`), decimals = 2) |>
      #     # tab_options(
      #     #   container.width = pct(100),
      #     #   table.width = px(800)
      #     # ) |>
      #     opt_interactive(
      #       use_pagination = F,
      #       use_sorting = T,
      #       use_text_wrapping = F,
      #       use_compact_mode = T
      #     )
      # )

      #
      # write gt table to html
      #
      # output$download_table <- downloadHandler(
      #   filename = function() {
      #     "summary_statistics.html"
      #   },
      #   content = function(file) {
      #     df_stats() |>
      #       gt() |>
      #       # opt_interactive() |>
      #       gtsave(file)
      #   }
      # )

      #
      # write reactable to html
      #
      output$download_table_as_html <- downloadHandler(
        filename = function() {
          "summary_statistics.html"
        },
        content = function(file) {
          saveWidget(
            tbl(),
            file,
            selfcontained = T
          )
        }
      )

      # write table to csv file
      output$download_table <- downloadHandler(
        filename = function() {
          glue("{file_name()}_summary_statistics.tsv")
        },
        content = function(file) {
          write_tsv(df_stats(), file)
        }
      )
    })
  })
}

shinyApp(ui, server)
