# ---------------------------------------------------------------------
# build_dataset.R -- Construct the analysis dataset from ORIGINAL sources
#
# Sources (all downloaded from official/public APIs, raw responses kept
# in data/raw_dapoer/ for verification):
#
#   1. INDO-DAPOER (Indonesia Database for Policy and Economic Research),
#      World Bank & BPS-Statistics Indonesia. Retrieved via the World Bank
#      API v2, source id 45, on 2026-07-14.
#      https://api.worldbank.org/v2/sources/45/...
#      Series used (year 2019, district level):
#        HOU.ELC.ACSN.ZS        Household access to electricity (% households)
#        IDX.HDI.REV            Human Development Index (revised method)
#        HOU.XPD.PC.CR          Household per-capita expenditure (IDR/month)
#        SI.POV.NAPR.ZS         Poverty rate (% of population)
#        SP.POP.TOTL            Total population
#        NA.GDP.INC.OG.SNA08.KR Total GRDP incl. oil & gas, constant price (IDR mn)
#        HOU.H2O.ACSN.ZS        Household access to safe water (% households)
#        SE.LITR.15UP.ZS        Literacy rate, age 15+ (%)
#
#   2. geoBoundaries v6, ADM2 (kabupaten/kota), open licence (CC BY 4.0).
#      Runfola et al. (2020), PLoS ONE 15(4): e0231866.
#      File: shapefiles/geoBoundaries-IDN-ADM2_simplified.geojson
#
# Output:
#   data/dapoer_district_2019.csv  -- tidy district-level table
#   data/district_centroids.csv    -- centroid + area per matched district
# ---------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
  library(sf)
  library(stringi)
})

RAW_DIR <- "data/raw_dapoer"
GEO     <- "shapefiles/geoBoundaries-IDN-ADM2_simplified.geojson"

# ---- 0. Automatic download of raw data (HarvardX CYO requirement) ----
# The raw API responses are included with the submission, but if any file
# is missing this section downloads it automatically from the original
# public sources, so the pipeline is fully reproducible from scratch.
SERIES_IDS <- c("HOU.ELC.ACSN.ZS", "IDX.HDI.REV", "HOU.XPD.PC.CR",
                "SI.POV.NAPR.ZS", "SP.POP.TOTL", "NA.GDP.INC.OG.SNA08.KR",
                "HOU.H2O.ACSN.ZS", "SE.LITR.15UP.ZS")

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(GEO), recursive = TRUE, showWarnings = FALSE)

download_if_missing <- function(path, url) {
  if (!file.exists(path)) {
    cat("Downloading", basename(path), "...\n")
    download.file(url, destfile = path, mode = "wb", quiet = TRUE)
  }
}

# 0a. INDO-DAPOER locations (provinces + districts), World Bank API v2
download_if_missing(
  file.path(RAW_DIR, "_locations.json"),
  "https://api.worldbank.org/v2/sources/45/provinces?format=json&per_page=1000"
)

# 0b. INDO-DAPOER indicator series, year 2019, district level
for (sid in SERIES_IDS) {
  download_if_missing(
    file.path(RAW_DIR, paste0(gsub("\\.", "_", sid), "_YR2019.json")),
    paste0("https://api.worldbank.org/v2/sources/45/provinces/all/series/",
           sid, "/time/YR2019/data?format=json&per_page=600")
  )
}

# 0c. geoBoundaries gbOpen IDN ADM2 simplified boundaries (CC BY)
if (!file.exists(GEO)) {
  gb_meta <- fromJSON("https://www.geoboundaries.org/api/current/gbOpen/IDN/ADM2/")
  download_if_missing(GEO, gb_meta$simplifiedGeometryGeoJSON)
}

# ---- 1. Locations (provinces + districts) ---------------------------
loc_raw <- fromJSON(file.path(RAW_DIR, "_locations.json"), simplifyVector = FALSE)
loc_var <- loc_raw$source[[1]]$concept[[1]]$variable

locations <- tibble(
  loc_id   = vapply(loc_var, function(x) as.character(x$id),    ""),
  loc_name = vapply(loc_var, function(x) as.character(x$value), "")
)

provinces <- locations %>%
  filter(!str_detect(loc_id, "^ID\\.[A-Z]+\\.")) %>%
  transmute(prov_id = loc_id,
            province = str_remove(loc_name, ",\\s*Prop\\.$"))

districts <- locations %>%
  filter(str_detect(loc_id, "^ID\\.[A-Z]+\\.")) %>%
  mutate(prov_id = str_extract(loc_id, "^ID\\.[A-Z]+")) %>%
  left_join(provinces, by = "prov_id") %>%
  mutate(
    district_type = if_else(str_detect(loc_name, ",\\s*Kota$|City$"), "city", "regency"),
    district      = loc_name %>%
      str_remove(",\\s*(Kab|Kota)\\.?$") %>%
      str_remove("^Adm\\.\\s*") %>%
      str_remove("\\s*City$") %>%
      str_trim()
  )

# ---- 2. Indicator values --------------------------------------------
read_series <- function(series_id, year = "YR2019") {
  path <- file.path(RAW_DIR, paste0(gsub("\\.", "_", series_id), "_", year, ".json"))
  j <- fromJSON(path, simplifyVector = FALSE)
  d <- j$source$data
  tibble(
    loc_id = vapply(d, function(x) as.character(x$variable[[1]]$id), ""),
    value  = vapply(d, function(x) {
      v <- x$value
      if (is.null(v) || length(v) == 0 || identical(v, "")) NA_real_ else as.numeric(v)
    }, numeric(1))
  ) %>% rename(!!series_id := value)
}

series_ids <- c("HOU.ELC.ACSN.ZS", "IDX.HDI.REV", "HOU.XPD.PC.CR",
                "SI.POV.NAPR.ZS", "SP.POP.TOTL", "NA.GDP.INC.OG.SNA08.KR",
                "HOU.H2O.ACSN.ZS", "SE.LITR.15UP.ZS")

values <- reduce(map(series_ids, read_series), full_join, by = "loc_id")

dat <- districts %>%
  inner_join(values, by = "loc_id") %>%
  rename(
    electricity_access = `HOU.ELC.ACSN.ZS`,
    hdi                = `IDX.HDI.REV`,
    expenditure_pc     = `HOU.XPD.PC.CR`,
    poverty_rate       = `SI.POV.NAPR.ZS`,
    population         = `SP.POP.TOTL`,
    grdp_total         = `NA.GDP.INC.OG.SNA08.KR`,
    water_access       = `HOU.H2O.ACSN.ZS`,
    literacy_rate      = `SE.LITR.15UP.ZS`
  )

# GRDP per capita in million IDR per person (GRDP is in IDR million)
dat <- dat %>%
  mutate(grdp_per_capita = grdp_total / population)  # IDR million per person

# ---- 3. Geometry: centroids + area ----------------------------------
g <- st_read(GEO, quiet = TRUE)

norm_key <- function(x) {
  x %>% tolower() %>% stri_trans_general("Latin-ASCII") %>% str_replace_all("[^a-z]", "")
}

g_tab <- g %>%
  mutate(
    district_type = if_else(str_detect(shapeName, "^Kota "), "city", "regency"),
    gname         = str_remove(shapeName, "^Kota ") %>% str_trim(),
    key           = paste0(district_type, "_", norm_key(gname))
  )

dat <- dat %>% mutate(key = paste0(district_type, "_", norm_key(district)))

# Manual fixes for districts whose names differ between the two sources
# (renames / alternate spellings; verified individually)
name_fixes <- c(
  "regency_admkepulauanseribu"    = "regency_kepulauanseribu",     # DKI Jakarta
  "regency_pontianak"             = "regency_mempawah",            # renamed 2014 (PP 58/2014)
  "regency_pasir"                 = "regency_paser",               # renamed 2007 (PP 49/2007)
  "regency_kotabaru"              = "regency_kotabaru",            # gb stores as "Kota Baru"
  "city_kepulauantidore"          = "city_tidorekepulauan",
  "regency_pangkajenekepulauan"   = "regency_pangkajenedankepulauan",
  "city_kendaricity"              = "city_kendari",
  "city_padangsidempuan"          = "city_padangsidimpuan",
  "regency_kepsiautagulandangbiaro" = "regency_siautagulandangbiaro"
)
# "Kota Baru" regency in gb has type prefix "Kota " stripped -> becomes city_baru;
# handle it explicitly:
g_tab <- g_tab %>%
  mutate(key = if_else(shapeName == "Kota Baru", "regency_kotabaru", key))

dat <- dat %>%
  mutate(key = if_else(key %in% names(name_fixes), unname(name_fixes[key]), key))

# Kendari city: DAPOER labels it "Kendari City" with type detection already ok?
# ("Kendari City" matches 'City$' -> city, and district = "Kendari") -- fine.

cent <- suppressWarnings(st_coordinates(st_centroid(g_tab)))
g_info <- g_tab %>%
  st_drop_geometry() %>%
  mutate(longitude = cent[, 1], latitude = cent[, 2],
         area_km2 = as.numeric(st_area(g_tab)) / 1e6) %>%
  select(key, shapeName, shapeISO, longitude, latitude, area_km2)

merged <- dat %>%
  inner_join(g_info, by = "key")

cat("Districts in DAPOER:", nrow(dat), "\n")
cat("Matched to geoBoundaries:", nrow(merged), "\n")
unmatched <- anti_join(dat, g_info, by = "key")
if (nrow(unmatched) > 0) {
  cat("UNMATCHED:\n"); print(unmatched %>% select(loc_id, loc_name, province))
}

# Population density (people per km^2) from population and polygon area
merged <- merged %>% mutate(population_density = population / area_km2)

# ---- 4. Export -------------------------------------------------------
out <- merged %>%
  select(loc_id, district, district_type, province,
         electricity_access, hdi, expenditure_pc, poverty_rate,
         population, grdp_total, grdp_per_capita,
         water_access, literacy_rate,
         longitude, latitude, area_km2, population_density)

write_csv(out, "data/dapoer_district_2019.csv")
cat("\nWrote data/dapoer_district_2019.csv with", nrow(out), "rows and",
    ncol(out), "columns\n")
print(summary(out %>% select(electricity_access, hdi, poverty_rate,
                             grdp_per_capita, population_density)))
