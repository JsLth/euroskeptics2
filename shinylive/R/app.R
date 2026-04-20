context <- c(0.025, 0.05, 0.1, 0.15, 0.2, 0.25)
ages <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65+")

y_titles <- list(
  unempl = "Arbeitslos",
  non_working = "Nicht erwerbst\u00e4tig",
  unempl_share = "Nationale Arbeitslosenquote",
  lt_unempl_share = "Nationale Langzeit-Arbeitslosenquote"
)

#' @import shiny
ui <- function() {
  bslib::page_sidebar(
    title = tagList(
      "Sitzung 2: Arbeitslosigkeit und Euroskeptizismus",
      actionButton("source", label = "Datenquelle", icon = icon("diaspora"))
    ),
    fillable = TRUE,
    padding = 10,
    sidebar = bslib::sidebar(
      shinyWidgets::virtualSelectInput(
        "y",
        label = "Y-Variable",
        choices = list("Vertrauen in die EU" = "treu")
      ),

      tags$blockquote(
        tags$p(
          tags$b("Frage:"), tags$br(),
          "Wie viel Vertrauen haben Sie in bestimmte Institutionen? Wie ist es mit der Europ\u00e4ischen Union?",
          style = "display:block;font-size:14px;"
        )
      ),

      shinyWidgets::virtualSelectInput(
        "x",
        label = span(
          "X-Variable",
          bslib::popover(
            icon("circle-question"),
            title = "Hilfe",
            HTML(
              "Die X-Variable (auch Pr\u00e4diktor genannt) ist ein Indikator, von dem angenommen wird, dass er einen statistischen Effekt auf die Y-Variable (hier: Vertrauen in die EU) hat.",
              "In dieser Sitzung gehen wir davon aus, dass Arbeitslosigkeit einen erheblichen Einfluss auf das Vertrauen in die EU hat, daher beziehen sich alle Variablen auf Arbeitslosigkeit.",
              "<br><br>",
              "Wir unterscheiden hierbei zwischen Variablen auf der Personenebene und der Kontextebene.<br><br>",
              "Die <b>Personenebene</b> bezeichnet Dinge, die die Person betreffen, die an der Umfrage teilgenommen hat, bspw. \"Wenn ich arbeitslos bin, wie beeinflusst das mein Vertrauen in die EU?\"<br><br>",
              "Die <b>Kontextebene</b> bezeichnet Dynamiken, die im Kontext dieser Person passieren, z.B. \"Wenn viele Menschen in meiner Umgebung arbeitslos sind, wie beeinflusst das mein Vertrauen in die EU?\""
            )
          )
        ),
        choices = shinyWidgets::prepare_choices(
          data.frame(
            var = c("unempl", "non_working", "unempl_share", "lt_unempl_share"),
            label = c("Arbeitslos", "Nicht erwerbst\u00e4tig", "Nationale Arbeitslosenquote", "Nationale Langzeit-Arbeitslosenquote"),
            group = rep(c("Personenebene", "Kontextebene"), each = 2)
          ),
          label = label,
          value = var,
          group_by = group
        ),
        selected = "unempl"
      ),

      uiOutput("x_text"),

      conditionalPanel(
        "['unempl_share', 'lt_unempl_share'].indexOf(input.x) > -1",
        shinyWidgets::virtualSelectInput(
          "context",
          label = span(
            "W\u00e4hle eine Arbeitslosenquote als Referenz",
            bslib::popover(
              icon("circle-question"),
              title = "Hilfe",
              HTML(
                "Diese Option bestimmt die Referenzquote f\u00fcr Kontextvariablen.",
                "Bei bin\u00e4ren Variablen (z.B. Arbeitslos? Ja/Nein) ist die Alternative zur Referenz (nicht arbeitslos) immer das Gegenteil (arbeitslos).",
                "Quoten hingegen haben deutlich mehr als zwei Auspr\u00e4gungen. Mit dieser Option kann kontrolliert werden, welche Arbeitslosenquote als Alternative gelten soll.",
                "Theoretisch sind auch Werte zwischen den Kategorien (z.B. 5 % und 10 %) m\u00f6glich, hier zur Einfachheit ausgelassen.",
                "<br><br>",
                "Die angezeigten Werte stehen immer im Vergleich zu einer Arbeitslosenquote von 0 % (der Referenz). Ein Wert von 2,5 % w\u00fcrde also fragen \"Wie verh\u00e4lt sich EU-Vertrauen, wenn die Arbeitslosenquote von 0 % auf 2,5 % steigt?\"",
              )
            )
          ),
          choices = stats::setNames(context, paste(context * 100, "%"))
        )
      ),

      shinyWidgets::virtualSelectInput(
        "group",
        label = span(
          "W\u00e4hle eine Vergleichsvariable",
          bslib::popover(
            icon("circle-question"),
            title = "Hilfe",
            HTML(
              "Diese Option differenziert die Statistiken nach demographischen Variablen.",
              "Wird beispielsweise die Option \"Alter\" ausgew\u00e4hlt, zeigen die beiden",
              "unteren Abbildungen die Unterschiede zwischen verschiedenen Altersgruppen an.",
              "<br><br>",
              "<b>Anmerkung:</b> Non-bin\u00e4re Geschlechtsidentit\u00e4ten k\u00f6nnen aufgrund geringer Fallzahlen nicht separat ausgewertet werden."
            )
          )
        ),
        choices = stats::setNames(
          c("", "gender", "age"),
          c("Keine", "Geschlecht", "Alter")
        ),
        selected = ""
      ),

      shinyWidgets::virtualSelectInput(
        "stat",
        label = span(
          "W\u00e4hle eine Statistik",
          bslib::popover(
            icon("circle-question"),
            title = "Hilfe",
            HTML(
              "Diese Option \u00e4ndert den angezeigten statistischen Kennwert.<br><br>",
              "<b>Referenz:</b><br>Der Referenzwert repr\u00e4sentiert die Ausgangssituation, also der Wert den EU-Vertrauen annimmt unter der Annahme, dass es keine Arbeitslosigkeit gibt.<br><br>",
              "<b>Alternative:</b><br>Der Alternativwert repr\u00e4sentiert verschiedene Situationen unter der Annahme, dass Arbeitslosigkeit einen Einfluss hat. F\u00fcr Individualvariablen bedeutet dies Arbeitslosigkeit, f\u00fcr Kontextvariablen Arbeitslosenquoten \u00fcber 0 %.<br><br>",
              "<b>Abweichung:</b><br>Die Abweichung ist die Differenz zwischen dem Alternativwert und dem Referenzwert. Je h\u00f6her die Abweichung, desto h\u00f6her der Effekt von Arbeitslosigkeit auf Vertrauen. Ist die Abweichung negativ, hat Arbeitslosigkeit einen negativen Effekt auf Vertrauen."
            )
          )
        ),
        choices = stats::setNames(
          c("baseline", "probability", "change"),
          c("Referenz", "Alternative", "Abweichung")
        ),
        selected = "change"
      )
    ),

    tags$head(
      tags$style(HTML("
      .card { position: relative; }
      .waiter-overlay {
        border-radius: calc(0.375rem - 1px);
      }
    "))
    ),

    waiter::useWaiter(),
    waiter::waiterShowOnLoad(waiter::spin_2()),
    waiter::autoWaiter(
      c("map", "heatmap", "ts", "probs"),
      html = tagList(
        waiter::bs5_spinner(), br(),
        span("Wird visualisiert...", style = "color: black; font-family: sans-serif; margin-top: 10px;")
      ),
      color = "white"
    ),

    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(
        bslib::card_body(plotOutput("map", click = "mapclick"), class = "p-0"),
        full_screen = TRUE
      ),

      bslib::card(
        bslib::card_body(plotOutput("heatmap")),
        full_screen = TRUE
      ),

      bslib::card(
        bslib::card_body(plotOutput("ts", click = "tsclick")),
        full_screen = TRUE
      ),

      bslib::card(
        bslib::card_body(plotOutput("probs")),
        full_screen = TRUE
      )
    )
  )
}



server <- function(input, output) {
  waiter::waiter_hide()

  observe({
    showModal(
      modalDialog(
        tags$table(
          style = "border-collapse: separate; border-spacing: 10px;",
          tags$tr(
            tags$th(tags$b("Data source:"), style = "vertical-align: top;"),
            tags$th(HTML("This application uses data from Russo & Br\u00e4utigam's (2023) harmonized Eurobarometer dataset (<a href='https://doi.org/10.7802/2539'>10.7802/2539</a>) as well additional Eurobarometer waves 96.3, 97.5, 98.2, 99.4, 100.3, and 101.5.<br><br>Unemployment statistics are taken from Eurostat (Unemployment: <a href='https://doi.org/10.2908/LFST_R_LFU3RT'>10.2908/LFST_R_LFU3RT</a>, Long-term unemployment: <a href='https://doi.org/10.2908/LFST_R_LFU2LTU'>10.2908/LFST_R_LFU2LTU</a>)"), style = "vertical-align: top;"),
            style = "padding-bottom: 10px;"
          ),
          tags$tr(
            tags$th(tags$b("Source attribution:"), style = "vertical-align: top;"),
            tags$th("Russo & Br\u00e4utigam (2023), European Commission / GESIS Data Archive", style = "vertical-align: top;"),
            style = "padding-bottom: 10px;"
          ),
          tags$tr(
            tags$th(tags$b("Disclaimer:"), style = "vertical-align: top;"),
            tags$th("The author of this app has modified the original data for visualization purposes. The European Commission and GESIS bear no responsibility for the results or interpretations presented in this tool.", style = "vertical-align: top;")
          )
        )
      )
    )
  }) |>
    bindEvent(input$source)

  output$x_text <- renderUI({
    tags$blockquote(
      tags$p(
        switch(
          input$x,
          unempl = "Bin\u00e4rer Indikator, der angibt, ob eine Person arbeitslos ist ohne in Rente oder in Ausbildung zu sein.",
          non_working = "Bin\u00e4rer Indikator, der angibt, ob eine Person nicht arbeitet, d.h. arbeitssuchend, arbeitsunf\u00e4hig, in Rente oder in Ausbildung ist.",
          unempl_share = "Nationaler Anteil an Arbeitssuchenden zwischen 15 und 74 Jahren.",
          lt_unempl_share = "Nationaler Anteil an Arbeitssuchenden zwischen 15 und 74 Jahren, die seit mehr als 12 Monaten arbeitssuchend sind."
        ),
        style = "display:block;font-size:14px;"
      )
    )
  })

  .data <- reactive({
    filter_data(input, ranef_cxt, ranef_ind)
  })

  .data_probs <- reactive({
    filter_data(input, ranef_cxt, ranef_ind, context_filter = FALSE)
  })

  .data_spatial <- reactive({
    .data <- filter_data(input, ranef_cxt, ranef_ind, group_filter = FALSE)

    .data <- .data |>
      dplyr::filter(is.na(age), is.na(gender)) |>
      dplyr::group_by(country) |>
      dplyr::summarise(
        bl_prob = mean(bl_prob, na.rm = TRUE),
        prob = mean(prob, na.rm = TRUE),
        prob_change = mean(prob_change, na.rm = TRUE)
      )
    sf::st_as_sf(dplyr::left_join(.data, countries, by = "country"))
  })

  country_cache <- NULL
  sel_country <- reactive({
    click <- input$mapclick %||% return(country_cache)
    pt <- sf::st_sfc(sf::st_point(unlist(click[c("x", "y")])), crs = 3035)
    country <- sf::st_filter(countries, pt)$country
    req(country, cancelOutput = TRUE)
    country_cache <<- country
    country
  })

  year_cache <- NULL
  sel_year <- reactive({
    click <- input$tsclick %||% return(year_cache)
    idx <- round(click$x)
    year <- click$domain$discrete_limits$x[[idx]]
    year_cache <<- year
    year
  })

  output$map <- renderPlot({
    .data <- .data_spatial()
    lims <- list(x = c(2500000, 7200000), y = c(1500000, 5200000))
    fill <- switch(
      input$stat,
      baseline = "bl_prob",
      probability = "prob",
      change = "prob_change"
    )

    ggplot2::ggplot(.data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill]])) +
      ggplot2::labs(
        fill = switch(input$stat, change = "Anstieg\nWahrsch.", "Wahrsch."),
        title = "Der Effekt von Arbeitslosigkeit auf EU-Vertrauen",
        subtitle = wrap(sprintf("Lies: %s", read_as(
          input$x,
          stat = input$stat,
          context = input$context
        )))
      ) +
      ggplot2::coord_sf(xlim = lims$x, ylim = lims$y) +
      colorspace::scale_fill_binned_divergingx(
        palette = "RdYlBu",
        labels = \(x) paste(as.numeric(x) * 100, "%")
      ) +
      ggplot2::theme_void()
  }, res = 90)

  output$heatmap <- renderPlot({
    .data <- .data()
    fill <- switch(
      input$stat,
      baseline = "bl_prob",
      probability = "prob",
      change = "prob_change"
    )

    ggplot2::ggplot(.data) +
      ggplot2::geom_tile(
        ggplot2::aes(
          x = year,
          y = country,
          fill = .data[[fill]]
        )
      ) +
      colorspace::scale_fill_binned_divergingx(
        palette = "RdYlBu",
        labels = \(x) paste(as.numeric(x) * 100, "%")
      ) +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        fill = switch(input$stat, change = "Anstieg\nWahrsch.", "Wahrsch."),
        title = "Der Effekt von Arbeitslosigkeit auf EU-Vertrauen",
        subtitle = wrap(sprintf("Lies: %s", read_as(
          input$x,
          stat = input$stat,
          context = input$context,
          year = TRUE
        )))
      ) +
      ggplot2::scale_y_discrete(breaks = rev) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }, res = 90)

  output$ts <- renderPlot({
    country <- sel_country()
    if (is.null(country)) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 1,
          y = 1,
          label = "Klicke auf ein Land in der Karte oben,\num dessen Zeitverlauf anzuzeigen.",
          size = 5
        ) +
        ggplot2::theme_void()
      return(p)
    }

    .data <- dplyr::filter(.data(), country == !!country)
    group <- input$group
    country <- countrycode::countrycode(country, "country.name", "country.name.de")
    fill <- switch(
      input$stat,
      baseline = "bl_prob",
      probability = "prob",
      change = "prob_change"
    )
    fill_label <- switch(
      input$stat,
      change = "Anstieg Wahrscheinlichkeit (in %)",
      "Wahrscheinlichkeit (in %)"
    )

    if (nzchar(group)) {
      .data <- .data |>
        dplyr::mutate(dplyr::across(c(age, gender), ~dplyr::if_else(is.na(.x), "Gesamt", .x))) |>
        dplyr::mutate(
          age = factor(age, levels = c("Gesamt", ages)),
          gender = factor(gender, levels = c("Gesamt", "Female", "Male"))
        )
    }

    ggplot2::ggplot(.data) +
      ggplot2::geom_line(ggplot2::aes(
        x = year,
        y = .data[[!!fill]],
        group = if (nzchar(group)) .data[[!!group]] else country,
        color = if (nzchar(group)) .data[[!!group]]
      ), linewidth = 1) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::labs(
        x = NULL,
        y = fill_label,
        color = switch(group, age = "Alter", gender = "Geschlecht"),
        title = sprintf("Arbeitslosigkeit und EU-Vertrauen in %s", country),
        subtitle = wrap(sprintf("Lies: %s", read_as(
          input$x,
          stat = input$stat,
          context = input$context,
          ts = TRUE
        )))
      ) +
      ggplot2::scale_y_continuous(labels = \(x) as.numeric(x) * 100) +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }, res = 90)

  output$probs <- renderPlot({
    country <- sel_country()
    year <- sel_year()

    if (!input$x %in% c("unempl_share", "lt_unempl_share")) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 1,
          y = 1,
          label = "W\u00e4hle eine Kontextvariable im Men\u00fc links aus,\num marginale Wahrscheinlichkeiten anzuzeigen.",
          size = 5
        ) +
        ggplot2::theme_void()
      return(p)
    }

    if (is.null(country)) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 1,
          y = 1,
          label = "Klicke auf ein Land in der Karte oben,\num marginale Wahrscheinlichkeiten anzuzeigen.",
          size = 5
        ) +
        ggplot2::theme_void()
      return(p)
    }

    if (is.null(year)) {
      p <- ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 1,
          y = 1,
          label = "Klicke auf ein Jahr in der Abbildung links,\num dessen marginale Wahrscheinlichkeiten anzuzeigen.",
          size = 5
        ) +
        ggplot2::theme_void()
      return(p)
    }

    .data <- .data_probs() |>
      dplyr::filter(country == !!country) |>
      dplyr::filter(year == !!year)

    country <- countrycode::countrycode(country, "country.name", "country.name.de")
    group <- input$group
    fill <- switch(
      input$stat,
      baseline = "bl_prob",
      probability = "prob",
      change = "prob_change"
    )
    fill_label <- switch(
      input$stat,
      change = "Anstieg Wahrscheinlichkeit (in %)",
      "Wahrscheinlichkeit (in %)"
    )

    if (nzchar(group)) {
      .data <- .data |>
        dplyr::mutate(dplyr::across(c(age, gender), ~dplyr::if_else(is.na(.x), "Gesamt", .x))) |>
        dplyr::mutate(
          age = factor(age, levels = c("Gesamt", ages)),
          gender = factor(gender, levels = c("Gesamt", "Female", "Male"))
        )
    }

    ggplot2::ggplot(.data) +
      ggplot2::geom_line(ggplot2::aes(
        x = context,
        y = .data[[fill]],
        group = if (nzchar(group)) .data[[!!group]] else country,
        color = if (nzchar(group)) .data[[!!group]]
      ), linewidth = 1) +
      ggplot2::labs(
        x = paste(switch(
          input$x,
          unempl_share = "Arbeitslosenquote",
          lt_unempl_share = "Langzeit-Arbeitslosenquote"
        ), "(in %)"),
        y = fill_label,
        color = switch(group, age = "Alter", gender = "Geschlecht"),
        title = sprintf(
          "EU-Vertrauen f\u00fcr verschiedene Grade an Arbeitslosenquoten (%s, %s)",
          country, year
        ),
        subtitle = wrap(sprintf(
          "Lies: F\u00fcr Arbeitslosenquote X %s Y %%.",
          switch(
            input$stat,
            change = "steigt die Wahrscheinlichkeit, Vertrauen in die EU zu haben, um",
            "liegt die Wahrscheinlichkeit, Vertrauen in die EU zu haben, bei"
          )
        ))
      ) +
      ggplot2::scale_y_continuous(labels = \(x) as.numeric(x) * 100) +
      ggplot2::scale_x_continuous(labels = \(x) as.numeric(x) * 100) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
  }, res = 90)
}

