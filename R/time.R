#' Align longitudinal measurements to a reference date
#'
#' Convert calendar-date observations into numeric days relative to either a
#' participant-specific reference column (for example, each participant's
#' vaccination date) or one shared reference date. The original date columns
#' are retained and a new numeric column is added.
#'
#' `measurement_date` and a reference column must contain base R [Date]
#' values. `POSIXct` is deliberately not converted implicitly because a hidden
#' timezone or time-of-day conversion can shift calendar dates; convert it
#' explicitly with [as.Date()] first. Negative values are retained, making
#' pre-reference observations visible during data checking. The current
#' single-exposure model itself accepts observations on or after time zero.
#'
#' @param data A data frame containing longitudinal observations.
#' @param measurement_date Name of the `Date` column containing measurement or
#'   sample-collection dates.
#' @param reference Either the name of a `Date` column in `data`, normally
#'   repeated within participant, or one shared `Date` value.
#' @param id Optional participant-id column. When `reference` names a column,
#'   supplying `id` checks that its non-missing values agree within each
#'   participant.
#' @param time Name of the numeric output column to add.
#' @param missing How missing measurement or reference dates are handled.
#'   `"error"` (the default) reports them; `"keep"` retains the rows and writes
#'   `NA_real_` to `time`.
#'
#' @return `data` with its original columns unchanged and an additional numeric
#'   time column measured in days relative to `reference`.
#' @export
#'
#' @examples
#' observations <- data.frame(
#'   participant = c("A", "A", "B", "B"),
#'   exposure_date = as.Date(c(
#'     "2024-01-10", "2024-01-10", "2024-02-01", "2024-02-01"
#'   )),
#'   sample_date = as.Date(c(
#'     "2024-01-08", "2024-01-17", "2024-01-30", "2024-02-15"
#'   ))
#' )
#'
#' align_time_to_reference(
#'   observations,
#'   measurement_date = "sample_date",
#'   reference = "exposure_date",
#'   id = "participant",
#'   time = "time_since_exposure"
#' )
#'
#' align_time_to_reference(
#'   observations,
#'   measurement_date = "sample_date",
#'   reference = as.Date("2024-01-01"),
#'   time = "time_since_study_start"
#' )
align_time_to_reference <- function(
    data,
    measurement_date,
    reference,
    id = NULL,
    time = "time_since_reference",
    missing = c("error", "keep")) {
  missing <- match.arg(missing)
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame or an object inheriting from it.",
         call. = FALSE)
  }
  if (!is.character(measurement_date) || length(measurement_date) != 1L ||
      is.na(measurement_date) || !nzchar(measurement_date)) {
    stop("'measurement_date' must be one non-empty column name.",
         call. = FALSE)
  }
  if (!measurement_date %in% names(data)) {
    stop("Measurement-date column '", measurement_date,
         "' is not present in 'data'.", call. = FALSE)
  }
  if (!is.character(time) || length(time) != 1L || is.na(time) ||
      !nzchar(time)) {
    stop("'time' must be one non-empty output column name.", call. = FALSE)
  }
  if (time %in% names(data)) {
    stop("Output column '", time, "' already exists in 'data'.",
         call. = FALSE)
  }
  if (!is.null(id)) {
    if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
      stop("'id' must be NULL or one non-empty column name.", call. = FALSE)
    }
    if (!id %in% names(data)) {
      stop("Participant-id column '", id, "' is not present in 'data'.",
           call. = FALSE)
    }
    if (anyNA(data[[id]])) {
      stop("Participant IDs must not be missing when 'id' is supplied.",
           call. = FALSE)
    }
  }

  measured <- data[[measurement_date]]
  validate_alignment_dates(measured, measurement_date)

  reference_is_column <- is.character(reference) && length(reference) == 1L &&
    !is.na(reference) && reference %in% names(data)
  if (reference_is_column) {
    reference_name <- reference
    reference_value <- data[[reference_name]]
    validate_alignment_dates(reference_value, reference_name)
    if (!is.null(id)) {
      validate_alignment_reference(
        reference_value,
        data[[id]],
        reference_name
      )
    }
  } else {
    if (is.character(reference)) {
      stop("Reference-date column '", paste(reference, collapse = "', '"),
           "' is not present in 'data'.", call. = FALSE)
    }
    if (!inherits(reference, "Date") || length(reference) != 1L) {
      stop("'reference' must be one Date or the name of a Date column.",
           call. = FALSE)
    }
    reference_value <- rep(reference, nrow(data))
  }

  missing_measurement <- is.na(measured)
  missing_reference <- is.na(reference_value)
  if (missing == "error" && any(missing_measurement | missing_reference)) {
    details <- c(
      if (any(missing_measurement)) {
        paste0(sum(missing_measurement), " missing measurement date(s)")
      },
      if (any(missing_reference)) {
        paste0(sum(missing_reference), " missing reference date(s)")
      }
    )
    stop("Cannot align time: ", paste(details, collapse = "; "),
         ". Use missing = \"keep\" to retain these rows with missing time.",
         call. = FALSE)
  }

  out <- data
  out[[time]] <- as.numeric(measured - reference_value)
  out
}

validate_alignment_dates <- function(x, column) {
  if (inherits(x, "POSIXt")) {
    stop("Column '", column, "' must contain Date values, not POSIXct; ",
         "convert it explicitly with as.Date().", call. = FALSE)
  }
  if (!inherits(x, "Date")) {
    stop("Column '", column, "' must contain Date values.", call. = FALSE)
  }
  invisible(x)
}

validate_alignment_reference <- function(reference, id, column) {
  groups <- split(reference, id, drop = TRUE)
  inconsistent <- names(groups)[vapply(groups, function(value) {
    length(unique(value[!is.na(value)])) > 1L
  }, logical(1))]
  if (length(inconsistent)) {
    stop("Reference-date column '", column,
         "' must be constant within participant; conflicting dates found for ",
         paste(utils::head(inconsistent, 5L), collapse = ", "), ".",
         call. = FALSE)
  }
  invisible(reference)
}
