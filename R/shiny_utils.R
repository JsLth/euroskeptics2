"%||%" <- function(x, y) if (is.null(x)) y else x


filter_data <- function(input, ranef_cxt, ranef_ind, group_filter = TRUE, context_filter = TRUE) {
  y <- input$x
  context <- input$context

  if (y %in% c("unempl_share", "lt_unempl_share")) {
    .data <- ranef_cxt
    if (context_filter) {
      .data <- dplyr::filter(.data, context == !!context)
    }
  } else {
    .data <- ranef_ind
  }

  if (group_filter) {
    group <- input$group |> dplyr::na_if("")
    if (is.na(group)) {
      .data <- dplyr::filter(.data, is.na(age), is.na(gender))
    } else {
      .data <- switch(
        group,
        age = dplyr::filter(.data, (!is.na(age) & is.na(gender)) | is.na(age) & is.na(gender)),
        gender = dplyr::filter(.data, (is.na(age) & !is.na(gender)) | is.na(age) & is.na(gender))
      )
    }
  }

  dplyr::filter(.data, predictor == !!y)
}


read_as <- function(y, stat = "baseline", context = NULL, year = FALSE, ts = FALSE) {
  context <- paste(as.numeric(context %||% 0) * 100, "%")

  if (ts) {
    stat_label <- switch(
      stat,
      change = "Wie ver\u00e4ndert sich der Anstieg der Wahrscheinlichkeit",
      "Wie ver\u00e4ndert sich die Wahrscheinlichkeit"
    )
  } else {
    stat_label <- switch(
      stat,
      change = "Der Anstieg der Wahrscheinlichkeit",
      "Die Wahrscheinlichkeit"
    )
  }

  cy_label <- ifelse(
    year,
    " im Jahr X und Land Y",
    ""
  )

  prefix <- sprintf("%s%s, Vertrauen in die EU zu haben, wenn", stat_label, cy_label)

  switch(
    stat,
    baseline = paste(prefix, switch(
      y,
      unempl = "man arbeitet.",
      non_working = "man arbeitet.",
      unempl_share = "die nationale Arbeitslosenquote unver\u00e4ndert ist.",
      lt_unempl_share = "die nationale Langzeit-Arbeitslosenquote unver\u00e4ndert ist."
    )),
    probability = paste(prefix, switch(
      y,
      unempl = "man arbeitslos ist.",
      non_working = "man nicht arbeitet.",
      unempl_share = sprintf("die nationale Arbeitslosenquote bei %s liegt.", context),
      lt_unempl_share = sprintf("die nationale Langzeit-Arbeitslosenquote bei %s liegt.", context)
    )),
    change = paste(prefix, switch(
      y,
      unempl = "man arbeitslos ist.",
      non_working = "man nicht arbeitet.",
      unempl_share = sprintf("die nationale Arbeitslosenquote bei %s liegt.", context),
      lt_unempl_share = sprintf("die nationale Langzeit-Arbeitslosenquote bei %s liegt.", context)
    ))
  )
}


wrap <- function(x) {
  scales::label_wrap(100)(x)
}
