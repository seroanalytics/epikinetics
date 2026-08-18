test_that("participant-specific reference dates produce signed day differences", {
  data <- data.frame(
    participant = c("A", "A", "B", "B"),
    exposure_date = as.Date(c(
      "2024-01-10", "2024-01-10", "2024-02-01", "2024-02-01"
    )),
    sample_date = as.Date(c(
      "2024-01-08", "2024-01-17", "2024-01-30", "2024-02-15"
    ))
  )
  original <- data

  aligned <- align_time_to_reference(
    data,
    measurement_date = "sample_date",
    reference = "exposure_date",
    id = "participant",
    time = "time_since_exposure"
  )

  expect_identical(data, original)
  expect_identical(aligned$sample_date, data$sample_date)
  expect_identical(aligned$exposure_date, data$exposure_date)
  expect_equal(aligned$time_since_exposure, c(-2, 7, -2, 14))
  expect_type(aligned$time_since_exposure, "double")
})

test_that("one shared reference date aligns every observation", {
  data <- data.frame(
    sample_date = as.Date(c("2024-01-01", "2024-01-08", "2023-12-30"))
  )
  aligned <- align_time_to_reference(
    data,
    measurement_date = "sample_date",
    reference = as.Date("2024-01-01"),
    time = "study_day"
  )
  expect_equal(aligned$study_day, c(0, 7, -2))
})

test_that("missing dates are either reported or retained explicitly", {
  data <- data.frame(
    participant = c("A", "A", "B"),
    exposure_date = as.Date(c("2024-01-01", "2024-01-01", NA)),
    sample_date = as.Date(c("2024-01-02", NA, "2024-02-01"))
  )

  expect_error(
    align_time_to_reference(
      data, "sample_date", "exposure_date", id = "participant"
    ),
    "1 missing measurement date.*1 missing reference date"
  )
  kept <- align_time_to_reference(
    data,
    "sample_date",
    "exposure_date",
    id = "participant",
    missing = "keep"
  )
  expect_equal(kept$time_since_reference, c(1, NA, NA))
})

test_that("date columns, selectors, and output names are validated", {
  dates <- data.frame(
    participant = "A",
    exposure_date = as.Date("2024-01-01"),
    sample_date = as.Date("2024-01-02")
  )

  expect_error(
    align_time_to_reference(NULL, "sample_date", as.Date("2024-01-01")),
    "must be a data.frame"
  )
  expect_error(
    align_time_to_reference(dates, character(), "exposure_date"),
    "'measurement_date' must be one non-empty"
  )
  expect_error(
    align_time_to_reference(dates, "missing", "exposure_date"),
    "Measurement-date column 'missing'"
  )
  expect_error(
    align_time_to_reference(dates, "sample_date", "missing"),
    "Reference-date column 'missing'"
  )
  expect_error(
    align_time_to_reference(dates, "sample_date", "exposure_date", id = "x"),
    "Participant-id column 'x'"
  )
  expect_error(
    align_time_to_reference(
      dates, "sample_date", "exposure_date", id = ""
    ),
    "'id' must be NULL or one non-empty"
  )
  expect_error(
    align_time_to_reference(
      dates, "sample_date", "exposure_date", time = NA_character_
    ),
    "'time' must be one non-empty"
  )
  expect_error(
    align_time_to_reference(
      dates, "sample_date", "exposure_date", time = "sample_date"
    ),
    "already exists"
  )

  characters <- dates
  characters$sample_date <- as.character(characters$sample_date)
  expect_error(
    align_time_to_reference(characters, "sample_date", "exposure_date"),
    "must contain Date values"
  )

  datetimes <- dates
  datetimes$sample_date <- as.POSIXct(datetimes$sample_date)
  expect_error(
    align_time_to_reference(datetimes, "sample_date", "exposure_date"),
    "not POSIXct.*as.Date"
  )

  missing_id <- dates
  missing_id$participant <- NA_character_
  expect_error(
    align_time_to_reference(
      missing_id, "sample_date", "exposure_date", id = "participant"
    ),
    "Participant IDs must not be missing"
  )
  expect_error(
    align_time_to_reference(
      dates, "sample_date", as.POSIXct("2024-01-01", tz = "UTC")
    ),
    "'reference' must be one Date"
  )
})

test_that("repeated participant reference dates must agree", {
  data <- data.frame(
    participant = c("A", "A", "B", "B"),
    exposure_date = as.Date(c(
      "2024-01-01", "2024-01-02", "2024-02-01", "2024-02-01"
    )),
    sample_date = as.Date(c(
      "2024-01-03", "2024-01-04", "2024-02-03", "2024-02-04"
    ))
  )
  expect_error(
    align_time_to_reference(
      data, "sample_date", "exposure_date", id = "participant"
    ),
    "constant within participant.*A"
  )
})
