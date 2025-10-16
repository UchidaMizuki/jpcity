source("data-raw/setup.R")

# update-data-lod ---------------------------------------------------------

# StandardAreaCode
source("data-raw/update-data-lod-StandardAreaCode.R")

StandardAreaCode <- download_lod_StandardAreaCode()
write_rds(StandardAreaCode, "data-raw/update-data-lod/StandardAreaCode.rds")
