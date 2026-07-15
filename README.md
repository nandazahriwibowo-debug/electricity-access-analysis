# Data Sources

All files in this folder are derived from **original, publicly available,
official data sources**. Nothing is synthetic. Raw API responses are kept in
`raw_dapoer/` so every number can be traced back to its source.

## 1. INDO-DAPOER (World Bank & BPS-Statistics Indonesia)

The Indonesia Database for Policy and Economic Research (INDO-DAPOER) is a
joint World Bank / BPS database of district-level (kabupaten/kota) indicators.

- Access: World Bank API v2, source id **45**
- Endpoint pattern:
  `https://api.worldbank.org/v2/sources/45/provinces/all/series/{SERIES}/time/YR2019/data?format=json&per_page=600`
- Retrieved: **2026-07-14**
- Licence: CC BY 4.0 (World Bank Open Data)
- Catalog page: https://datacatalog.worldbank.org/search/dataset/0038570

Series downloaded (year 2019, the most recent year with full district
coverage; raw JSON stored in `raw_dapoer/`):

| Series code              | Description                                              | File |
|--------------------------|----------------------------------------------------------|------|
| HOU.ELC.ACSN.ZS          | Household access to electricity (% of households)        | `HOU_ELC_ACSN_ZS_YR2019.json` |
| IDX.HDI.REV              | Human Development Index (revised method, 0–100)          | `IDX_HDI_REV_YR2019.json` |
| HOU.XPD.PC.CR            | Household per-capita expenditure (IDR/month)             | `HOU_XPD_PC_CR_YR2019.json` |
| SI.POV.NAPR.ZS           | Poverty rate (% of population)                           | `SI_POV_NAPR_ZS_YR2019.json` |
| SP.POP.TOTL              | Total population                                         | `SP_POP_TOTL_YR2019.json` |
| NA.GDP.INC.OG.SNA08.KR   | Total GRDP incl. oil & gas, constant 2010 price (IDR mn) | `NA_GDP_INC_OG_SNA08_KR_YR2019.json` |
| HOU.H2O.ACSN.ZS          | Household access to safe water (% of households)         | `HOU_H2O_ACSN_ZS_YR2019.json` |
| SE.LITR.15UP.ZS          | Literacy rate, population 15+ (%)                        | `SE_LITR_15UP_ZS_YR2019.json` |

`raw_dapoer/_locations.json` holds the province/district location list
(548 geographies: 34 provinces + 514 districts) and
`raw_dapoer/_all_indicators.txt` the full indicator catalogue.

## 2. geoBoundaries (ADM2 administrative boundaries)

- File: `../shapefiles/geoBoundaries-IDN-ADM2_simplified.geojson`
- Source: geoBoundaries Global Database of Political Administrative
  Boundaries, gbOpen release, Indonesia ADM2 (519 units, 2020 vintage)
- URL: https://www.geoboundaries.org/ (API: `/api/current/gbOpen/IDN/ADM2/`)
- Licence: CC BY 4.0
- Citation: Runfola, D. et al. (2020). geoBoundaries: A global database of
  political administrative boundaries. *PLoS ONE* 15(4): e0231866.

## 3. Derived file: `dapoer_district_2019.csv`

Built by `../build_dataset.R`. One row per district (514 rows). Steps:

1. Parse the eight DAPOER series and join them by location id.
2. Compute `grdp_per_capita` = GRDP total / population (IDR million/person).
3. Match districts to geoBoundaries polygons by normalised name + type
   (regency/city). Nine districts required documented manual fixes for
   official renames or spelling variants (e.g. Kab. Pontianak -> Mempawah,
   PP 58/2014; Kab. Pasir -> Paser, PP 49/2007).
4. Compute polygon centroids (`longitude`, `latitude`) and areas
   (`area_km2`), then `population_density` = population / area.

All 514 DAPOER districts matched a boundary polygon. Six districts are
dropped by the analysis script for incomplete 2019 data: Buton Selatan,
Buton Tengah, and Muna Barat (created in 2014, no population in DAPOER,
hence no density/GRDP-per-capita) and Deiyai, Mamberamo Tengah, and Puncak
in Papua (no safe-water figure). The modelling sample is therefore
508 districts.
