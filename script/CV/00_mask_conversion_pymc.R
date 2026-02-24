mask_root <- "runs/CV/20260126/masks"
files_all <- list.files(mask_root, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)

files <- files_all[grepl("downscaling_rep0[1-3]/", files_all)]
length(files)

for (f in files) {
  x <- readRDS(f)
  # make sure it’s a plain data.frame for max compatibility
  x <- as.data.frame(x)

  # write a new file alongside the old one
  out <- sub("\\.rds$", "_gzip_v2.rds", f)
  saveRDS(x, out, compress = "gzip", version = 2)
}

length(files)
