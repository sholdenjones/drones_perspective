# Exploratory figures (lkw)

Map and sensor figures for the UAV tropical ecology review.

## Files

| File | Description |
|---|---|
| `drones_figures_lkw.Rmd` | Main analysis — renders to HTML + Word |
| `parse_coordinates_lkw.R` | Coordinate parser (DMS, DD, unicode variants) |
| `data/data_cleaned_lkw.csv` | Cleaned coordinates with geocoded fallbacks |

## Figures produced

**Sensor type over time** (3 variants): stacked bar, proportional bar, area chart

**Study location maps** (6 variants):

1. Dot map by topic (lumped to top 6 + Other)
2. Dot map by topic (all 12 topics)
3. Pie chart map by country
4. Choropleth + dot overlay
5. Dot map + bar chart below (top 15 countries + Other)
6. Faceted mini-maps by topic

## Usage

Requires `data/DataExtraction_drone.xlsx` in the `data/` folder.

```r
rmarkdown::render("drones_figures_lkw.Rmd", output_format = "all")
```

## Dependencies

tidyverse, readxl, sf, rnaturalearth, rnaturalearthdata, scales, patchwork, tidygeocoder, scatterpie
