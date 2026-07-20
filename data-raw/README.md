# data-raw

Two pipelines live here, plus `internal.R`, which is the single entry point
that writes the package's internal data (`R/sysdata.rda`).

| | `lod/` | `legacy/` |
|---|---|---|
| Source | e-Stat LOD (SPARQL) | e-Stat area-code CSVs (scraped) |
| Status | **New — in migration** | **Deprecated, frozen** |
| Feeds `R/sysdata.rda` | not yet (see below) | yes, via a cache (see below) |

Run every script from the **project root** (paths are root-relative).

## `internal.R` — builds `R/sysdata.rda`

```r
source("data-raw/internal.R")
```

Sources `lod/use-data.R` (cheap to re-run), reads the cached result of
`legacy/use-data.R` from `legacy/data/legacy.rds` (expensive, so not
re-run on every build), and makes the single
`usethis::use_data(internal = TRUE)` call. Currently only the legacy
objects (`graph_city_legacy`, `city_desig_code_legacy`,
`string_pref_name_legacy`) are written to `sysdata.rda`, since the
exported (`_legacy`-suffixed) functions still read them; `area_data`
moves into this call once the R layer is migrated to the id-based
objects.

## `lod/` — e-Stat LOD (new)

```
lod/
  update-data.R              # download: SPARQL -> data/StandardAreaCode.rds
  update-data/
    StandardAreaCode.R       #   the SPARQL query + LIMIT/OFFSET pagination
  use-data.R                 # build: -> data/standard_area_code.rds, data/area_data.rds
  use-data/
    graph_area.R             #   entity collapse + succession graph + ancestors/descendants
  data/                      # cached artifacts (gitignored except StandardAreaCode.rds)
```

`update-data.R` is network-bound and only needs re-running when e-Stat publishes
new codes. `use-data.R` is the one to re-run while iterating on the data model;
it works entirely offline from the cached download, and is cheap enough to
source on every `internal.R` run.

The pipeline produces `area_data`, an id-keyed master:

- `nodes` — one row per **undated** entity (5-digit code): name / kana / en,
  `admin_class`, `pref_name`, `parent_code`, `check_digit`, `interval`
- `dated` — one row per **dated** version (`C{code}-{issued}`), the LOD's native
  period-tagged resource
- `ancestors` / `descendants` — the merge/split closure, with intervals truncated
  at the succession dates
- `interval_code` — total lifespan per code

## `legacy/` — e-Stat CSV scraping (deprecated, frozen)

```
legacy/
  update-data.R              # scrape areacode CSVs at two dates + merger/split table
  update-data/{areacode,absorption_separation}.R
  use-data.R                 # -> data/legacy.rds (graph_city_legacy, city_desig_code_legacy, string_pref_name_legacy)
  use-data/{graph_city,city_desig_code,string_pref_name}.R
  data/                      # cached artifacts, including the derived legacy.rds cache
```

Kept solely so the current `R/sysdata.rda` stays reproducible during the
migration. It also hardcodes the Okinawa reversion (1972) and the Northern
Territories (1983), which the LOD carries natively. Do not extend it.

The ancestor/descendant graph computation in `use-data/graph_city.R` is
expensive, so this script is **not** sourced on every build. Run it by hand
(`source("data-raw/legacy/use-data.R")`) only when the legacy raw data
actually changes; it writes its result to `data/legacy.rds`, which
`internal.R` reads instead. This whole directory (script, cache, and its
entry in `internal.R`) is removed once `R/` no longer reads the
`_legacy` objects.

## Shared

- `setup.R` — libraries + `pkgload::load_all()`, sourced by both pipelines
- `logo.R` — package logo, unrelated to either
