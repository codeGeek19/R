# Laboratory Assignment 8: High-Performance Big Data Analytics Using R
# Dataset: NYC TLC Yellow Taxi Trip Records + Taxi Zone Lookup Table
#
# Before running:
# 1. Download a Yellow Taxi Trip Records file (Parquet or CSV) from:
#    https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page
# 2. Download the Taxi Zone Lookup Table from the same page.
# 3. Set TRIP_FILE and ZONE_FILE below to the actual file paths.
# 4. Run this script in RStudio.
#
# This script measures performance on your machine. Do not report benchmark
# values until you have run it with your chosen dataset.

# --------------------------- 0. Configuration -------------------------------

TRIP_FILE <- "yellow_tripdata_2025-01.parquet" # change to your downloaded file
ZONE_FILE <- "taxi_zone_lookup.csv"            # change to your downloaded file
OUTPUT_DIR <- "r_taxi_output"
MAX_PLOT_ROWS <- 100000L  # maximum rows used for the distance-vs-fare scatterplot
PARALLEL_WORKERS <- max(1L, parallel::detectCores(logical = FALSE) - 1L)

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# --------------------------- 1. Packages ------------------------------------

required_packages <- c(
  "data.table", "ggplot2", "lubridate", "foreach", "doParallel"
)
optional_packages <- c("arrow", "microbenchmark", "purrr")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}
install_if_missing(required_packages)
install_if_missing(optional_packages)

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(lubridate)
  library(foreach)
  library(doParallel)
})

# --------------------------- 2. Read data -----------------------------------

if (!file.exists(TRIP_FILE)) {
  stop(
    "Trip dataset not found: ", TRIP_FILE,
    "\nDownload a TLC Yellow Taxi Trip Records file and update TRIP_FILE."
  )
}
if (!file.exists(ZONE_FILE)) {
  stop(
    "Zone lookup file not found: ", ZONE_FILE,
    "\nDownload the TLC Taxi Zone Lookup Table and update ZONE_FILE."
  )
}

read_trip_data <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read Parquet. Install it with install.packages('arrow').")
    }
    return(as.data.table(arrow::read_parquet(path)))
  } else if (ext %in% c("csv", "txt")) {
    return(data.table::fread(path))
  } else {
    stop("Unsupported trip file extension: ", ext, ". Use .parquet or .csv.")
  }
}

message("Reading trip dataset: ", TRIP_FILE)
trips <- read_trip_data(TRIP_FILE)
zones <- fread(ZONE_FILE)

cat("\n--- Initial data inspection ---\n")
print(dim(trips))
print(names(trips))
print(str(trips))
print(summary(trips))
cat("Duplicate rows before cleaning:", sum(duplicated(trips)), "\n")

# TLC files may use different names in older/newer releases. Standardize the
# common names used by this assignment.
rename_if_present <- function(DT, old, new) {
  if (old %in% names(DT) && !(new %in% names(DT))) {
    setnames(DT, old, new)
  }
}
rename_if_present(trips, "tpep_pickup_datetime", "pickup_datetime")
rename_if_present(trips, "tpep_dropoff_datetime", "dropoff_datetime")
rename_if_present(trips, "PULocationID", "pickup_location_id")
rename_if_present(trips, "DOLocationID", "dropoff_location_id")
rename_if_present(trips, "passenger_count", "passenger_count")
rename_if_present(trips, "trip_distance", "trip_distance")
rename_if_present(trips, "fare_amount", "fare_amount")
rename_if_present(trips, "tip_amount", "tip_amount")
rename_if_present(trips, "tolls_amount", "tolls_amount")
rename_if_present(trips, "total_amount", "total_amount")
rename_if_present(trips, "payment_type", "payment_type")

required_columns <- c(
  "pickup_datetime", "dropoff_datetime", "pickup_location_id",
  "dropoff_location_id", "trip_distance", "fare_amount", "total_amount"
)
missing_columns <- setdiff(required_columns, names(trips))
if (length(missing_columns)) {
  stop(
    "The selected dataset is missing required columns: ",
    paste(missing_columns, collapse = ", "),
    "\nCheck the file and column names."
  )
}

# Reduce memory use by retaining columns relevant to the analysis.
candidate_columns <- c(
  "pickup_datetime", "dropoff_datetime", "pickup_location_id",
  "dropoff_location_id", "passenger_count", "trip_distance",
  "fare_amount", "tip_amount", "tolls_amount", "total_amount",
  "payment_type", "VendorID", "RatecodeID", "store_and_fwd_flag"
)
keep_columns <- intersect(candidate_columns, names(trips))
trips <- trips[, ..keep_columns]

# Convert timestamps. TLC Parquet timestamps are usually read as POSIXct;
# CSV timestamps are parsed explicitly.
if (!inherits(trips$pickup_datetime, "POSIXct")) {
  trips[, pickup_datetime := ymd_hms(as.character(pickup_datetime), quiet = TRUE,
                                     tz = "America/New_York")]
}
if (!inherits(trips$dropoff_datetime, "POSIXct")) {
  trips[, dropoff_datetime := ymd_hms(as.character(dropoff_datetime), quiet = TRUE,
                                      tz = "America/New_York")]
}

numeric_columns <- intersect(
  c("pickup_location_id", "dropoff_location_id", "passenger_count",
    "trip_distance", "fare_amount", "tip_amount", "tolls_amount",
    "total_amount", "payment_type"),
  names(trips)
)
for (column in numeric_columns) {
  set(trips, j = column, value = suppressWarnings(as.numeric(trips[[column]])))
}

# Remove exact duplicates and clearly invalid/incomplete records.
before_cleaning <- nrow(trips)
trips <- unique(trips)
trips <- trips[
  !is.na(pickup_datetime) &
    !is.na(dropoff_datetime) &
    !is.na(pickup_location_id) &
    !is.na(dropoff_location_id) &
    !is.na(trip_distance) &
    !is.na(fare_amount) &
    !is.na(total_amount) &
    trip_distance > 0 &
    fare_amount >= 0 &
    total_amount >= 0 &
    dropoff_datetime >= pickup_datetime
]
if ("passenger_count" %in% names(trips)) {
  trips <- trips[is.na(passenger_count) | passenger_count >= 0]
}
cat("Rows removed by duplicate/validity cleaning:", before_cleaning - nrow(trips), "\n")
cat("Rows retained:", nrow(trips), "\n")

# Temporal features.
trips[, `:=`(
  hour = hour(pickup_datetime),
  day = day(pickup_datetime),
  weekday = wday(pickup_datetime, label = TRUE, abbr = FALSE, week_start = 1),
  month = month(pickup_datetime, label = TRUE, abbr = FALSE),
  month_number = month(pickup_datetime),
  pickup_date = as.Date(pickup_datetime),
  trip_minutes = as.numeric(difftime(dropoff_datetime, pickup_datetime, units = "mins"))
)]

# Standardize zone lookup column names.
rename_if_present(zones, "LocationID", "location_id")
rename_if_present(zones, "Borough", "borough")
rename_if_present(zones, "Zone", "zone")
if (!"location_id" %in% names(zones)) {
  stop("Zone lookup must contain a LocationID column.")
}
if (!"borough" %in% names(zones)) zones[, borough := NA_character_]
if (!"zone" %in% names(zones)) zones[, zone := NA_character_]
zones <- unique(zones[, .(location_id = as.numeric(location_id), borough, zone)])

# Efficient joins with data.table.
setkey(zones, location_id)
trips[, pickup_location_id := as.numeric(pickup_location_id)]
trips[, dropoff_location_id := as.numeric(dropoff_location_id)]
trips <- zones[trips, on = .(location_id = pickup_location_id)]
setnames(trips, c("borough", "zone"), c("pickup_borough", "pickup_zone"))
trips <- zones[trips, on = .(location_id = dropoff_location_id)]
setnames(trips, c("borough", "zone"), c("dropoff_borough", "dropoff_zone"))

# Save a compact cleaned data file.
saveRDS(trips, file.path(OUTPUT_DIR, "cleaned_taxi_trips.rds"), compress = "gzip")
cat("\n--- Cleaned data structure ---\n")
print(dim(trips))
print(summary(trips[, .(trip_distance, fare_amount, total_amount, trip_minutes)]))

# --------------------------- 3. Core analytics ------------------------------

cat("\n--- Task 2: Transportation analytics ---\n")

trips_by_hour <- trips[, .(trip_count = .N), by = hour][order(hour)]
trips_by_weekday <- trips[, .(trip_count = .N), by = weekday][order(weekday)]
trips_by_month <- trips[, .(trip_count = .N), by = .(month_number, month)][order(month_number)]
fare_revenue_by_month <- trips[, .(
  trips = .N,
  average_fare = mean(fare_amount, na.rm = TRUE),
  total_revenue = sum(total_amount, na.rm = TRUE)
), by = .(month_number, month)][order(month_number)]

top_routes <- trips[, .(
  trip_count = .N,
  total_revenue = sum(total_amount, na.rm = TRUE),
  average_fare = mean(fare_amount, na.rm = TRUE)
), by = .(pickup_location_id, pickup_zone, pickup_borough,
          dropoff_location_id, dropoff_zone, dropoff_borough)][
            order(-trip_count)][1:min(10L, .N)]

top_revenue_routes <- trips[, .(
  trip_count = .N,
  total_revenue = sum(total_amount, na.rm = TRUE),
  average_fare = mean(fare_amount, na.rm = TRUE)
), by = .(pickup_location_id, pickup_zone, pickup_borough,
          dropoff_location_id, dropoff_zone, dropoff_borough)][
            order(-total_revenue)][1:min(10L, .N)]

distance_fare_summary <- trips[, .(
  correlation = cor(trip_distance, fare_amount, use = "complete.obs"),
  average_distance = mean(trip_distance, na.rm = TRUE),
  average_fare = mean(fare_amount, na.rm = TRUE)
)]

if ("payment_type" %in% names(trips)) {
  payment_by_borough <- trips[, .(trip_count = .N),
                              by = .(pickup_borough, payment_type)][order(pickup_borough, -trip_count)]
  payment_counts <- trips[, .(trip_count = .N), by = payment_type][order(-trip_count)]
} else {
  payment_by_borough <- data.table()
  payment_counts <- data.table()
  warning("payment_type is not present; payment analysis skipped.")
}

top_pickup_zones <- trips[, .(trip_count = .N),
                          by = .(pickup_location_id, pickup_zone, pickup_borough)][
                            order(-trip_count)][1:min(10L, .N)]
top_dropoff_zones <- trips[, .(trip_count = .N),
                           by = .(dropoff_location_id, dropoff_zone, dropoff_borough)][
                             order(-trip_count)][1:min(10L, .N)]

# Save analysis tables.
fwrite(trips_by_hour, file.path(OUTPUT_DIR, "trips_by_hour.csv"))
fwrite(trips_by_weekday, file.path(OUTPUT_DIR, "trips_by_weekday.csv"))
fwrite(trips_by_month, file.path(OUTPUT_DIR, "trips_by_month.csv"))
fwrite(fare_revenue_by_month, file.path(OUTPUT_DIR, "fare_revenue_by_month.csv"))
fwrite(top_routes, file.path(OUTPUT_DIR, "top_routes_by_frequency.csv"))
fwrite(top_revenue_routes, file.path(OUTPUT_DIR, "top_routes_by_revenue.csv"))
fwrite(top_pickup_zones, file.path(OUTPUT_DIR, "top_pickup_zones.csv"))
fwrite(top_dropoff_zones, file.path(OUTPUT_DIR, "top_dropoff_zones.csv"))
if (nrow(payment_counts)) fwrite(payment_counts, file.path(OUTPUT_DIR, "payment_counts.csv"))
if (nrow(payment_by_borough)) fwrite(payment_by_borough, file.path(OUTPUT_DIR, "payment_by_borough.csv"))

cat("\nTrips by hour:\n"); print(trips_by_hour)
cat("\nTrips by weekday:\n"); print(trips_by_weekday)
cat("\nMonthly trips and revenue:\n"); print(fare_revenue_by_month)
cat("\nMost frequent routes:\n"); print(top_routes)
cat("\nHighest-revenue routes:\n"); print(top_revenue_routes)
cat("\nDistance/fare summary:\n"); print(distance_fare_summary)
if (nrow(payment_counts)) { cat("\nPayment type counts:\n"); print(payment_counts) }

# --------------------------- 4. Functional vs optimized ---------------------

cat("\n--- Task 3: Functional, vectorized and data.table comparisons ---\n")

# Same task: count trips by pickup hour using several approaches.
hour_vectorized <- function(DT) {
  tabulate(DT$hour + 1L, nbins = 24L)
}
hour_datatable <- function(DT) {
  DT[, .N, by = hour][order(hour)]
}
hour_split_lapply <- function(DT) {
  split_data <- split(DT$hour, DT$hour)
  vapply(split_data, length, integer(1))
}
hour_apply <- function(DT) {
  # apply() works on matrices; this example demonstrates a simple repeated
  # operation over a small summary matrix, not the preferred full-data method.
  counts <- tabulate(DT$hour + 1L, nbins = 24L)
  apply(matrix(counts, nrow = 1), 2, identity)
}
hour_purrr_map <- function(DT) {
  if (!requireNamespace("purrr", quietly = TRUE)) {
    return(NA)
  }
  purrr::map_int(0:23, function(h) sum(DT$hour == h, na.rm = TRUE))
}

# Use a sample for repeated timing so benchmarks do not exhaust memory/time.
set.seed(42)
sample_n <- min(nrow(trips), 200000L)
benchmark_data <- trips[sample.int(nrow(trips), sample_n)]

timing_results <- data.table(
  implementation = character(),
  elapsed_seconds = numeric(),
  stringsAsFactors = FALSE
)

time_once <- function(label, expression) {
  elapsed <- system.time(force(expression))[["elapsed"]]
  timing_results <<- rbind(
    timing_results,
    data.table(implementation = label, elapsed_seconds = as.numeric(elapsed))
  )
  invisible(elapsed)
}

time_once("Vectorized tabulate", hour_vectorized(benchmark_data))
time_once("data.table grouping", hour_datatable(benchmark_data))
time_once("lapply/split", hour_split_lapply(benchmark_data))
time_once("apply summary matrix", hour_apply(benchmark_data))
if (requireNamespace("purrr", quietly = TRUE)) {
  time_once("purrr::map_int", hour_purrr_map(benchmark_data))
}

# microbenchmark provides more repeated measurements if installed.
if (requireNamespace("microbenchmark", quietly = TRUE)) {
  mb <- microbenchmark::microbenchmark(
    vectorized = hour_vectorized(benchmark_data),
    data_table = hour_datatable(benchmark_data),
    lapply_split = hour_split_lapply(benchmark_data),
    times = 10L
  )
  capture.output(print(mb), file = file.path(OUTPUT_DIR, "microbenchmark.txt"))
}

fwrite(timing_results, file.path(OUTPUT_DIR, "implementation_timings.csv"))
cat("\nSingle-run timings (seconds; your results may differ):\n")
print(timing_results)

# --------------------------- 5. Sequential vs parallel ----------------------

cat("\n--- Task 4: Sequential vs parallel processing ---\n")

# Divide rows into logical chunks. The same operation (trip counts by hour)
# is computed on each chunk and then combined.
n_chunks <- min(max(1L, PARALLEL_WORKERS), max(1L, nrow(trips)))
chunk_id <- cut(seq_len(nrow(trips)), breaks = n_chunks, labels = FALSE)
chunks <- split(trips[, .(hour)], chunk_id)

sequential_start <- system.time({
  sequential_parts <- lapply(chunks, function(part) {
    data.table(hour = part$hour)[, .(trip_count = .N), by = hour]
  })
  sequential_result <- rbindlist(sequential_parts)[, .(trip_count = sum(trip_count)), by = hour][order(hour)]
})[["elapsed"]]

parallel_start <- NA_real_
parallel_result <- NULL
cluster <- NULL
tryCatch({
  workers <- min(PARALLEL_WORKERS, length(chunks))
  if (workers < 1L) workers <- 1L
  cluster <- parallel::makeCluster(workers)
  doParallel::registerDoParallel(cluster)

  parallel_start <- system.time({
    parallel_parts <- foreach(part = chunks, .combine = "rbind",
                              .packages = "data.table") %dopar% {
      data.table(hour = part$hour)[, .(trip_count = .N), by = hour]
    }
    parallel_result <- as.data.table(parallel_parts)[
      , .(trip_count = sum(trip_count)), by = hour][order(hour)]
  })[["elapsed"]]
}, error = function(e) {
  warning("Parallel run failed: ", conditionMessage(e))
}, finally = {
  if (!is.null(cluster)) {
    try(parallel::stopCluster(cluster), silent = TRUE)
    foreach::registerDoSEQ()
  }
})

speedup <- if (!is.na(parallel_start) && parallel_start > 0) {
  sequential_start / parallel_start
} else {
  NA_real_
}

parallel_summary <- data.table(
  method = c("Sequential", "Parallel"),
  elapsed_seconds = c(sequential_start, parallel_start)
)
parallel_summary[, speedup_vs_sequential := c(1, speedup)]
fwrite(parallel_summary, file.path(OUTPUT_DIR, "sequential_parallel_timings.csv"))
cat("Sequential elapsed seconds:", sequential_start, "\n")
cat("Parallel elapsed seconds:", parallel_start, "\n")
cat("Speedup (sequential / parallel):", speedup, "\n")
if (!is.null(parallel_result) && nrow(parallel_result)) {
  cat("Sequential/parallel outputs should match; inspect the tables if needed.\n")
  print(all.equal(
    sequential_result[order(hour)],
    parallel_result[order(hour)],
    check.attributes = FALSE
  ))
}
cat("Note: parallel overhead can make small workloads slower.\n")

# --------------------------- 6. Visualizations ------------------------------

cat("\n--- Task 6: Creating visualizations ---\n")

save_plot <- function(plot, filename, width = 9, height = 5) {
  ggsave(file.path(OUTPUT_DIR, filename), plot = plot,
         width = width, height = height, dpi = 300)
}

p1 <- ggplot(trips_by_hour, aes(x = hour, y = trip_count)) +
  geom_col() +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "NYC Yellow Taxi Trips by Hour",
       x = "Pickup hour (0–23)", y = "Number of trips") +
  theme_minimal()
save_plot(p1, "trips_by_hour.png")

p2 <- ggplot(trips_by_weekday, aes(x = weekday, y = trip_count)) +
  geom_col() +
  labs(title = "Taxi Demand by Day of Week",
       x = "Day of week", y = "Number of trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_plot(p2, "trips_by_weekday.png")

p3 <- ggplot(trips_by_month, aes(x = factor(month, levels = month[order(month_number)]),
                                 y = trip_count)) +
  geom_col() +
  labs(title = "Monthly Taxi Demand",
       x = "Pickup month", y = "Number of trips") +
  theme_minimal()
save_plot(p3, "trips_by_month.png")

p4 <- ggplot(fare_revenue_by_month, aes(x = month_number, y = total_revenue)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = fare_revenue_by_month$month_number,
                     labels = as.character(fare_revenue_by_month$month)) +
  labs(title = "Total Taxi Revenue by Month",
       x = "Pickup month", y = "Total amount collected (dataset currency units)") +
  theme_minimal()
save_plot(p4, "revenue_by_month.png")

plot_data <- if (nrow(trips) > MAX_PLOT_ROWS) {
  trips[sample.int(nrow(trips), MAX_PLOT_ROWS)]
} else {
  trips
}
p5 <- ggplot(plot_data, aes(x = trip_distance, y = fare_amount)) +
  geom_point(alpha = 0.25) +
  coord_cartesian(xlim = quantile(plot_data$trip_distance, c(0, 0.99), na.rm = TRUE),
                  ylim = quantile(plot_data$fare_amount, c(0, 0.99), na.rm = TRUE)) +
  labs(title = "Trip Distance vs Fare Amount (up to 99th percentile shown)",
       x = "Trip distance (miles)", y = "Fare amount") +
  theme_minimal()
save_plot(p5, "distance_vs_fare.png")

if (nrow(payment_counts)) {
  p6 <- ggplot(payment_counts, aes(x = factor(payment_type), y = trip_count)) +
    geom_col() +
    labs(title = "Taxi Trips by Payment Type",
         x = "Payment type code", y = "Number of trips") +
    theme_minimal()
  save_plot(p6, "payment_type_distribution.png")
}

if (nrow(top_pickup_zones)) {
  top_pickup_zones[, zone_label := ifelse(
    is.na(pickup_zone) | pickup_zone == "",
    paste("Location", pickup_location_id),
    paste(pickup_zone, pickup_borough, sep = ", ")
  )]
  p7 <- ggplot(top_pickup_zones, aes(x = reorder(zone_label, trip_count),
                                     y = trip_count)) +
    geom_col() + coord_flip() +
    labs(title = "Top Pickup Zones",
         x = "Pickup zone", y = "Number of trips") +
    theme_minimal()
  save_plot(p7, "top_pickup_zones.png")
}

if (nrow(top_routes)) {
  top_routes[, route_label := paste0(
    ifelse(is.na(pickup_zone), paste0("ID ", pickup_location_id), pickup_zone),
    " → ",
    ifelse(is.na(dropoff_zone), paste0("ID ", dropoff_location_id), dropoff_zone)
  )]
  p8 <- ggplot(top_routes, aes(x = reorder(route_label, trip_count),
                               y = trip_count)) +
    geom_col() + coord_flip() +
    labs(title = "Most Frequently Travelled Routes",
         x = "Pickup → drop-off zone", y = "Number of trips") +
    theme_minimal()
  save_plot(p8, "top_routes.png")
}

# --------------------------- 7. Critical analysis ----------------------------

analysis_text <- c(
  "Critical analysis guide (complete using your actual results):",
  "",
  "1. data.table uses efficient grouping, joins and by-reference updates, making it",
  "   suitable for large tabular datasets.",
  "2. Vectorized operations can be fast for simple calculations because work is",
  "   performed in optimized low-level code rather than explicit R loops.",
  "3. apply/lapply/purrr can improve code organization for repeated tasks, but they",
  "   are not automatically faster than vectorized or data.table code.",
  "4. Parallel processing helps only when work is large enough to offset worker",
  "   startup, data transfer and result-combination overhead.",
  "5. Use implementation_timings.csv and sequential_parallel_timings.csv to support",
  "   performance claims. Do not invent speedups or claim an approach won without",
  "   checking the measurements.",
  "6. This script benchmarks a sample for programming-style comparisons; the main",
  "   data.table analytics run on all cleaned records.",
  "",
  paste("Rows analyzed:", nrow(trips)),
  paste("Number of columns after enrichment:", ncol(trips)),
  paste("Distance/fare correlation:", round(distance_fare_summary$correlation, 4)),
  paste("Sequential elapsed seconds:", round(sequential_start, 4)),
  paste("Parallel elapsed seconds:", round(parallel_start, 4)),
  paste("Measured speedup:", round(speedup, 4))
)
writeLines(analysis_text, file.path(OUTPUT_DIR, "critical_analysis_template.txt"))

cat("\n--- Script completed ---\n")
cat("Output directory:", normalizePath(OUTPUT_DIR), "\n")
cat("Generated cleaned data, CSV summary tables, benchmark files, and PNG plots.\n")
cat("Use the real measured timings and plots when writing your lab report.\n")
