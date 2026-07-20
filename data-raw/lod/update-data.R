# Download step of the LOD pipeline (see data-raw/README.md).
#
# Hits the e-Stat SPARQL endpoint once and caches the raw response. Slow and
# network-bound, so it is kept separate from use-data.R, which is the script you
# re-run while iterating on the data model.

source("data-raw/setup.R")

# update-data-lod ---------------------------------------------------------

# StandardAreaCode
source("data-raw/lod/update-data/StandardAreaCode.R")

StandardAreaCode <- download_lod_StandardAreaCode()
write_rds(StandardAreaCode, "data-raw/lod/data/StandardAreaCode.rds")
