# Extract detections of one or more species from the BAMDataset DuckDB
#
# The BAMDataset contains one row per detected species and survey in `bird`,
# while `visit` contains the survey-level attributes. This script joins those
# tables, assigns each detection to a Bird Conservation Region (BCR), filters
# to the requested BCR(s), and writes the result to a CSV file.
#
# The output can contain detections only or be zero-filled so every survey in
# the requested BCR(s) has one row for every requested species.

# USER SETTINGS ---------------------------------------------------------------

# Four-character species codes used in bird$species.
species_codes <- c("CONW")

# One or more BCR identifiers. Use character values because some BCRs have
# subunits such as "4-0". A parent value such as "4" includes all 4-* subunits.
bcrs <- c("77-0")

# TRUE: include every survey in the requested BCR(s), setting non-detections
# to count = 0. FALSE: return only detections recorded in the bird table.
include_zero_counts <- TRUE

# BAMDataset DuckDB to query. Update this when a new database is released.
database_path <- paste0(
  "G:/Shared drives/BAM_AvianData/BAMDataset/",
  "03_BAMDataset_WT-2026-11-20_EBd-Jun-2026_PRISM-2026-10-09.duckdb"
)

# BCR polygons used when the BAMDataset was assembled.
bcr_path <- paste0(
  "G:/Shared drives/BAM_AvianData/BAMDataset/regions/model/",
  "BAM_BCRNMv5_3978.shp"
)

# Output location. Keep this as an absolute path so results are never written
# to the repository simply because it is the current working directory.
output_directory <- "G:/Shared drives/BAM_NationalModels5/review/CONW"
output_filename <- "species_observations.csv"
output_file <- file.path(output_directory, output_filename)

# SCRIPT ----------------------------------------------------------------------

run_extraction <- function() {
  required_packages <- c("DBI", "duckdb", "sf")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0L) {
    stop(
      "Install the required package(s) before running this script: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  species_codes <- unique(trimws(as.character(species_codes)))
  bcrs <- unique(trimws(as.character(bcrs)))
  bcrs <- bcrs[nzchar(bcrs)]

  if (
    length(species_codes) == 0L ||
      anyNA(species_codes) ||
      any(!nzchar(species_codes))
  ) {
    stop(
      "species_codes must contain at least one non-missing species code.",
      call. = FALSE
    )
  }

  if (length(bcrs) == 0L || anyNA(bcrs)) {
    stop(
      "bcrs must contain at least one non-missing BCR identifier.",
      call. = FALSE
    )
  }

  if (
    length(include_zero_counts) != 1L ||
      is.na(include_zero_counts) ||
      !is.logical(include_zero_counts)
  ) {
    stop("include_zero_counts must be either TRUE or FALSE.", call. = FALSE)
  }

  if (!file.exists(database_path)) {
    stop("BAMDataset DuckDB not found: ", database_path, call. = FALSE)
  }

  if (!file.exists(bcr_path)) {
    stop("BCR shapefile not found: ", bcr_path, call. = FALSE)
  }

  # Read and select the requested BCR polygons. Matching a parent code such as
  # "4" also selects subunits "4-0", "4-1", and so on.
  bcr_layer <- sf::st_read(bcr_path, quiet = TRUE)

  if (!"subUnit" %in% names(bcr_layer)) {
    stop(
      "The BCR shapefile does not contain the expected 'subUnit' field.",
      call. = FALSE
    )
  }

  matches_requested_bcr <- vapply(
    as.character(bcr_layer$subUnit),
    function(region) {
      if (is.na(region)) {
        return(FALSE)
      }

      any(region == bcrs | startsWith(region, paste0(bcrs, "-")))
    },
    logical(1)
  )

  selected_bcr <- bcr_layer[matches_requested_bcr, ]

  if (nrow(selected_bcr) == 0L) {
    available_bcrs <- sort(unique(stats::na.omit(as.character(
      bcr_layer$subUnit
    ))))
    stop(
      "None of the requested BCRs were found. Available BCR subunits are: ",
      paste(available_bcrs, collapse = ", "),
      call. = FALSE
    )
  }

  # Open the DuckDB read-only so this extraction cannot modify the source data.
  connection <- tryCatch(
    DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = database_path,
      read_only = TRUE
    ),
    error = function(error) {
      message_text <- conditionMessage(error)

      if (
        grepl("newer version|version number", message_text, ignore.case = TRUE)
      ) {
        stop(
          "The installed duckdb package cannot read this database version. ",
          "Install the DuckDB version used to create the BAMDataset, then retry.\n",
          "Original error: ",
          message_text,
          call. = FALSE
        )
      }

      stop("Could not open the BAMDataset: ", message_text, call. = FALSE)
    }
  )

  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  tables <- DBI::dbListTables(connection)
  required_tables <- c("bird", "visit")

  if (!all(required_tables %in% tables)) {
    stop(
      "The DuckDB must contain both the 'bird' and 'visit' tables.",
      call. = FALSE
    )
  }

  bird_fields <- DBI::dbListFields(connection, "bird")
  visit_fields <- DBI::dbListFields(connection, "visit")

  if (!all(c("survey_id", "species", "count") %in% bird_fields)) {
    stop(
      "The bird table is missing survey_id, species, or count.",
      call. = FALSE
    )
  }

  if (!all(c("survey_id", "longitude", "latitude") %in% visit_fields)) {
    stop(
      "The visit table is missing survey_id, longitude, or latitude.",
      call. = FALSE
    )
  }

  # Select every bird field and every ordinary visit attribute. Spatial
  # geometry can be named `geometry` or `geom`; omit it because DuckDB returns
  # it as a list column that CSV cannot encode. Longitude and latitude are
  # retained instead.
  visit_attributes <- setdiff(
    visit_fields,
    c("survey_id", "geometry", "geom")
  )

  qualified_fields <- function(table_alias, fields) {
    paste0(
      table_alias,
      ".",
      vapply(
        fields,
        function(field) as.character(DBI::dbQuoteIdentifier(connection, field)),
        character(1)
      )
    )
  }

  # Filter rows to the exact BCR polygons and add BCR labels. Normally a point
  # intersects one polygon. For a point exactly on a boundary, concatenate its
  # matches so the survey is not duplicated.
  filter_to_bcr <- function(data) {
    points <- sf::st_as_sf(
      data,
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    )
    points <- sf::st_transform(points, sf::st_crs(selected_bcr))

    bcr_hits <- sf::st_intersects(points, selected_bcr)
    keep <- lengths(bcr_hits) > 0L
    data <- data[keep, , drop = FALSE]
    bcr_hits <- bcr_hits[keep]

    collapse_hit_values <- function(field) {
      vapply(
        bcr_hits,
        function(hit) {
          paste(
            unique(as.character(selected_bcr[[field]][hit])),
            collapse = ";"
          )
        },
        character(1)
      )
    }

    data$bcr_subunit <- collapse_hit_values("subUnit")

    if ("country" %in% names(selected_bcr)) {
      data$bcr_country <- collapse_hit_values("country")
    }

    if ("bcr" %in% names(selected_bcr)) {
      data$bcr_id <- collapse_hit_values("bcr")
    }

    data
  }

  species_placeholders <- paste(
    rep("?", length(species_codes)),
    collapse = ", "
  )

  if (include_zero_counts) {
    # First get every visit that could fall inside the selected BCRs. The
    # longitude/latitude bounding box limits data transfer from DuckDB; the
    # exact polygon intersection is applied immediately afterward in R.
    selected_bcr_lonlat <- sf::st_transform(selected_bcr, 4326)
    bcr_bounds <- sf::st_bbox(selected_bcr_lonlat)
    visit_select_fields <- qualified_fields(
      "v",
      c("survey_id", visit_attributes)
    )

    visit_query <- paste(
      "SELECT",
      paste(visit_select_fields, collapse = ", "),
      "FROM visit AS v",
      "WHERE v.longitude BETWEEN ? AND ?",
      "AND v.latitude BETWEEN ? AND ?"
    )

    visits <- DBI::dbGetQuery(
      connection,
      visit_query,
      params = as.list(as.numeric(bcr_bounds[c(
        "xmin",
        "xmax",
        "ymin",
        "ymax"
      )]))
    )

    if (nrow(visits) == 0L) {
      stop("No surveys were found in the requested BCR(s).", call. = FALSE)
    }

    visits <- filter_to_bcr(visits)

    if (nrow(visits) == 0L) {
      stop("No surveys were found in the requested BCR(s).", call. = FALSE)
    }

    # Retrieve detections for the requested species, then keep only detections
    # belonging to the selected surveys.
    bird_query <- paste(
      "SELECT",
      paste(qualified_fields("b", bird_fields), collapse = ", "),
      "FROM bird AS b",
      "WHERE b.species IN (",
      species_placeholders,
      ")"
    )
    bird_records <- DBI::dbGetQuery(
      connection,
      bird_query,
      params = as.list(species_codes)
    )

    species_not_found <- setdiff(
      species_codes,
      unique(bird_records$species)
    )
    if (length(species_not_found) > 0L) {
      warning(
        "No detections were found anywhere in the database for: ",
        paste(species_not_found, collapse = ", "),
        ". Their output rows will contain zero counts.",
        call. = FALSE
      )
    }

    bird_records <- bird_records[
      bird_records$survey_id %in% visits$survey_id,
      ,
      drop = FALSE
    ]

    # Expand to one row per selected survey and requested species, then copy
    # bird values onto the detected combinations.
    visit_rows <- rep(seq_len(nrow(visits)), each = length(species_codes))
    observations <- visits[visit_rows, , drop = FALSE]
    observations$species <- rep(species_codes, times = nrow(visits))

    output_keys <- paste(
      observations$survey_id,
      observations$species,
      sep = "\r"
    )
    detection_keys <- paste(
      bird_records$survey_id,
      bird_records$species,
      sep = "\r"
    )
    detection_index <- match(output_keys, detection_keys)
    bird_value_fields <- setdiff(bird_fields, c("survey_id", "species"))

    for (field in bird_value_fields) {
      observations[[field]] <- bird_records[[field]][detection_index]
    }

    no_detection <- is.na(detection_index)
    if ("count" %in% bird_value_fields) {
      observations$count[no_detection] <- 0
    }
    if ("count_ai_reclassified" %in% bird_value_fields) {
      observations$count_ai_reclassified[no_detection] <- 0
    }
    if ("flag_err_count" %in% bird_value_fields) {
      observations$flag_err_count[no_detection] <- FALSE
    }

    bcr_output_fields <- intersect(
      c("bcr_subunit", "bcr_country", "bcr_id"),
      names(observations)
    )
    output_fields <- unique(c(
      bird_fields,
      visit_attributes,
      bcr_output_fields
    ))
    observations <- observations[, output_fields, drop = FALSE]
  } else {
    # Detections-only mode joins bird records directly to their visit data.
    select_fields <- c(
      qualified_fields("b", bird_fields),
      qualified_fields("v", visit_attributes)
    )
    query <- paste(
      "SELECT",
      paste(select_fields, collapse = ", "),
      "FROM bird AS b",
      "INNER JOIN visit AS v ON b.survey_id = v.survey_id",
      "WHERE b.species IN (",
      species_placeholders,
      ")"
    )
    observations <- DBI::dbGetQuery(
      connection,
      query,
      params = as.list(species_codes)
    )

    if (nrow(observations) == 0L) {
      stop(
        "No observations were found for the requested species codes: ",
        paste(species_codes, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    species_not_found <- setdiff(species_codes, unique(observations$species))
    if (length(species_not_found) > 0L) {
      warning(
        "No observations were found for: ",
        paste(species_not_found, collapse = ", "),
        call. = FALSE
      )
    }

    observations <- filter_to_bcr(observations)
  }

  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }

  utils::write.csv(observations, output_file, row.names = FALSE, na = "")

  row_description <- if (include_zero_counts) {
    " survey-species row(s)"
  } else {
    " observation(s)"
  }

  detection_counts <- vapply(
    species_codes,
    function(species_code) {
      sum(
        observations$species == species_code &
          !is.na(observations$count) &
          observations$count > 0
      )
    },
    numeric(1)
  )
  detection_lines <- paste0(
    "  ",
    species_codes,
    ": ",
    format(detection_counts, big.mark = ",", scientific = FALSE, trim = TRUE)
  )

  survey_line <- if (include_zero_counts) {
    paste0(
      "\nTotal surveys: ",
      format(
        length(unique(observations$survey_id)),
        big.mark = ",",
        scientific = FALSE
      )
    )
  } else {
    ""
  }

  message(
    "Wrote ",
    format(nrow(observations), big.mark = ","),
    row_description,
    " for ",
    paste(species_codes, collapse = ", "),
    " in BCR(s) ",
    paste(bcrs, collapse = ", "),
    "\nDetections by species (surveys with count > 0):\n",
    paste(detection_lines, collapse = "\n"),
    survey_line,
    "\nOutput:\n",
    normalizePath(output_file, winslash = "/", mustWork = FALSE)
  )
}

run_extraction()
