dms_to_decimal <- function(deg, min = 0, sec = 0, hem = "") {
  dd <- abs(as.numeric(deg)) + as.numeric(min) / 60 + as.numeric(sec) / 3600
  if (grepl("[SWsw-]", hem)) dd <- -dd
  dd
}

clean_coord_str <- function(s) {
  s <- trimws(s)
  s <- gsub("\u00c2", "", s)
  s <- gsub("\u2212|\u2013|\u2014", "-", s)  # unicode minus/en-dash/em-dash
  s <- gsub(" \u030a", "\u00b0", s)  # combining ring above -> degree sign
  s <- gsub("\u0307", "\u00b0", s)    # combining dot above -> degree sign
  s <- gsub("\\s+and\\s+", " ", s, ignore.case = TRUE)
  s <- gsub("\\s+latitudes\\s+", " ", s, ignore.case = TRUE)
  s <- gsub("\\s+east\\s+", "E ", s, ignore.case = TRUE)
  s <- gsub("\\s+north\\s+", "N ", s, ignore.case = TRUE)
  s <- gsub("\\s+south\\s+", "S ", s, ignore.case = TRUE)
  s <- gsub("\\s+west\\s+", "W ", s, ignore.case = TRUE)
  s <- gsub("\\(approx\\.?\\)", "", s)
  s <- gsub("\\(State center\\)", "", s)
  s <- gsub("[,]+\\s*$", "", s)
  s <- gsub(",\\.", ",", s)  # stray period after comma
  s
}

# Shared regex building blocks
DEG <- "[\u00b0\u00ba\u25e6\u02da]"
MIN <- "[\u2032\u2019\u02bc'\u02b9\u00b4\u2018\u201a`]"
SEC <- paste0("(?:", MIN, "{2}|[\u2033\u201d\"\u201c\u02ba])")
SEP <- "[,; \\t]+"

parse_single_coord <- function(s) {
  if (is.na(s) || s == "" || grepl("not (listed|reported)", s, ignore.case = TRUE)) {
    return(c(lat = NA_real_, lon = NA_real_))
  }

  s <- clean_coord_str(s)

  # Strategy 1: Decimal degrees with hemisphere (8.1694°N, 77.8981°W or 3.0708° S 60.3604° W)
  pat1 <- paste0(
    "(-?[0-9]+\\.?[0-9]*)\\s*", DEG, "?\\s*([NSns])\\s*", SEP, "\\s*",
    "(-?[0-9]+\\.?[0-9]*)\\s*", DEG, "?\\s*([EWew])"
  )
  m <- regmatches(s, regexec(pat1, s, perl = TRUE))[[1]]
  if (length(m) == 5) {
    lat <- as.numeric(m[2]) * ifelse(grepl("[Ss]", m[3]), -1, 1)
    lon <- as.numeric(m[4]) * ifelse(grepl("[Ww]", m[5]), -1, 1)
    return(c(lat = lat, lon = lon))
  }

  # Strategy 2: DMS with hemisphere (3°11′N, 12°48′E or 20◦17′50″N, 87◦30′59″W)
  dms_part <- paste0(
    "(-?\\d+)\\s*", DEG, "\\s*",
    "(\\d+(?:\\.\\d+)?)\\s*", MIN, "\\s*",
    "(?:(\\d+(?:\\.\\d+)?)\\s*", SEC, "\\s*)?",
    "([NSEWnsew])?"
  )
  pat2 <- paste0(dms_part, "\\s*", SEP, "\\s*", dms_part)
  m <- regmatches(s, regexec(pat2, s, perl = TRUE))[[1]]
  if (length(m) >= 9 && nchar(m[1]) > 0) {
    v1 <- dms_to_decimal(m[2], m[3], ifelse(m[4] == "", 0, m[4]), m[5])
    v2 <- dms_to_decimal(m[6], m[7], ifelse(m[8] == "", 0, m[8]), m[9])
    if (!is.na(v1) && !is.na(v2)) {
      # If first part has E/W hemisphere, it's lon — swap
      if (grepl("[EWew]", m[5]) && grepl("[NSns]", m[9])) {
        return(c(lat = v2, lon = v1))
      }
      return(c(lat = v1, lon = v2))
    }
  }

  # Strategy 3: Prefix hemisphere (N5.42, E118.05 or N5.423742°, E118.055597°)
  pat3 <- paste0(
    "([NSns])\\s*(-?[0-9]+\\.?[0-9]*)\\s*", DEG, "?\\s*", SEP, "\\s*",
    "([EWew])\\s*(-?[0-9]+\\.?[0-9]*)\\s*", DEG, "?"
  )
  m <- regmatches(s, regexec(pat3, s, perl = TRUE))[[1]]
  if (length(m) == 5) {
    lat <- as.numeric(m[3]) * ifelse(grepl("[Ss]", m[2]), -1, 1)
    lon <- as.numeric(m[5]) * ifelse(grepl("[Ww]", m[4]), -1, 1)
    return(c(lat = lat, lon = lon))
  }

  # Strategy 3b: Prefix hemisphere DMS (N5°18′ W52°53′)
  dms_prefix <- paste0(
    "([NSns])\\s*(\\d+)\\s*", DEG, "\\s*",
    "(\\d+(?:\\.\\d+)?)\\s*", MIN, "\\s*",
    "(?:(\\d+(?:\\.\\d+)?)\\s*", SEC, "\\s*)?",
    SEP, "\\s*",
    "([EWew])\\s*(\\d+)\\s*", DEG, "\\s*",
    "(\\d+(?:\\.\\d+)?)\\s*", MIN, "\\s*",
    "(?:(\\d+(?:\\.\\d+)?)\\s*", SEC, "\\s*)?"
  )
  m <- regmatches(s, regexec(dms_prefix, s, perl = TRUE))[[1]]
  if (length(m) >= 9 && nchar(m[1]) > 0) {
    lat <- dms_to_decimal(m[3], m[4], ifelse(m[5] == "", 0, m[5]),
                          ifelse(grepl("[Ss]", m[2]), "S", "N"))
    lon <- dms_to_decimal(m[7], m[8], ifelse(m[9] == "", 0, m[9]),
                          ifelse(grepl("[Ww]", m[6]), "W", "E"))
    if (!is.na(lat) && !is.na(lon)) return(c(lat = lat, lon = lon))
  }

  # Strategy 4: Plain signed decimal with optional degree symbol
  # Handles: -2.23333°, 103.31667° and 15.098, 107.905
  pat4 <- paste0(
    "^\\s*(-?[0-9]+\\.?[0-9]*)\\s*", DEG, "?\\s*", SEP,
    "\\s*(-?[0-9]+\\.?[0-9]*)\\s*", DEG, "?\\s*$"
  )
  m <- regmatches(s, regexec(pat4, s, perl = TRUE))[[1]]
  if (length(m) == 3 && nchar(m[1]) > 0) {
    v1 <- as.numeric(m[2])
    v2 <- as.numeric(m[3])
    if (abs(v1) <= 90 && abs(v2) <= 180) return(c(lat = v1, lon = v2))
    if (abs(v2) <= 90 && abs(v1) <= 180) return(c(lat = v2, lon = v1))
  }

  # Strategy 5: Degrees + decimal minutes (23°54.9′N, 46°38.2′W)
  dm_part <- paste0(
    "(-?\\d+)\\s*", DEG, "\\s*",
    "(\\d+(?:\\.\\d+)?)\\s*", MIN, "\\s*",
    "([NSEWnsew])"
  )
  pat5 <- paste0(dm_part, "\\s*", SEP, "\\s*", dm_part)
  m <- regmatches(s, regexec(pat5, s, perl = TRUE))[[1]]
  if (length(m) == 7 && nchar(m[1]) > 0) {
    lat <- dms_to_decimal(m[2], m[3], 0, m[4])
    lon <- dms_to_decimal(m[5], m[6], 0, m[7])
    return(c(lat = lat, lon = lon))
  }

  c(lat = NA_real_, lon = NA_real_)
}

# Parse a single DMS/DM half like "9º59′10″S" -> value with sign
parse_half_coord <- function(s) {
  s <- clean_coord_str(s)

  # DMS half: 9º59′10″S
  pat <- paste0(
    "(-?\\d+)\\s*", DEG, "\\s*",
    "(\\d+(?:\\.\\d+)?)\\s*", MIN, "\\s*",
    "(?:(\\d+(?:\\.\\d+)?)\\s*", SEC, "\\s*)?",
    "([NSEWnsew])"
  )
  m <- regmatches(s, regexec(pat, s, perl = TRUE))[[1]]
  if (length(m) >= 5 && nchar(m[1]) > 0) {
    hem <- m[5]
    val <- dms_to_decimal(m[2], m[3], ifelse(m[4] == "", 0, m[4]), hem)
    type <- ifelse(grepl("[NSns]", hem), "lat", "lon")
    return(list(value = val, type = type))
  }

  # Decimal half: 3.0708° S or 10°N
  pat2 <- paste0("(-?[0-9]+\\.?[0-9]*)\\s*", DEG, "?\\s*([NSEWnsew])")
  m <- regmatches(s, regexec(pat2, s, perl = TRUE))[[1]]
  if (length(m) == 3 && nchar(m[1]) > 0) {
    hem <- m[3]
    val <- as.numeric(m[2]) * ifelse(grepl("[SWsw]", hem), -1, 1)
    type <- ifelse(grepl("[NSns]", hem), "lat", "lon")
    return(list(value = val, type = type))
  }

  NULL
}

parse_coord_string <- function(raw_str) {
  if (is.na(raw_str) || raw_str == "" ||
      grepl("not (listed|reported)", raw_str, ignore.case = TRUE)) {
    return(c(lat = NA_real_, lon = NA_real_))
  }

  swap_if_needed <- function(r) {
    if (abs(r["lat"]) > 90 && abs(r["lon"]) <= 90) {
      r <- c(lat = r["lon"], lon = r["lat"])
    }
    r
  }

  # Handle decimal-range patterns: "5.11–4.41° N, 116.99–117.49° E"
  # Replace "A–B° H" with midpoint
  collapse_ranges <- function(s) {
    s <- gsub("\u2013|\u2014", "-", s)
    # Decimal range with hemisphere: 5.11394-4.41325◦ N -> midpoint◦ N
    range_pat <- paste0(
      "([0-9]+\\.?[0-9]*)\\s*-\\s*([0-9]+\\.?[0-9]*)\\s*(",
      DEG, "?\\s*[NSEWnsew])"
    )
    while (grepl(range_pat, s, perl = TRUE)) {
      m <- regmatches(s, regexec(range_pat, s, perl = TRUE))[[1]]
      mid <- (as.numeric(m[2]) + as.numeric(m[3])) / 2
      s <- sub(range_pat, paste0(mid, m[4]), s, perl = TRUE)
    }
    s
  }

  raw_str_clean <- collapse_ranges(raw_str)

  # First try parsing the whole string as a single coordinate
  result <- parse_single_coord(raw_str_clean)
  if (!any(is.na(result))) return(swap_if_needed(result))

  # Split on semicolons and try each part as a complete coordinate
  parts <- trimws(unlist(strsplit(raw_str_clean, ";")))
  parts <- parts[nchar(parts) > 0]

  coords <- lapply(parts, parse_single_coord)
  valid <- !sapply(coords, function(x) any(is.na(x)))

  if (any(valid)) {
    coords <- coords[valid]
    lats <- sapply(coords, `[`, "lat")
    lons <- sapply(coords, `[`, "lon")
    result <- c(lat = mean(lats), lon = mean(lons))
    return(swap_if_needed(result))
  }

  # Try parsing semicolon-separated halves (e.g., "9º59′10″S; 68º11′33″W")
  halves <- lapply(parts, parse_half_coord)
  halves <- halves[!sapply(halves, is.null)]
  if (length(halves) >= 2) {
    lat_vals <- sapply(halves[sapply(halves, function(h) h$type == "lat")], function(h) h$value)
    lon_vals <- sapply(halves[sapply(halves, function(h) h$type == "lon")], function(h) h$value)
    if (length(lat_vals) >= 1 && length(lon_vals) >= 1) {
      return(c(lat = mean(lat_vals), lon = mean(lon_vals)))
    }
  }

  c(lat = NA_real_, lon = NA_real_)
}
