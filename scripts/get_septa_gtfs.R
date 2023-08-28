
# Download latest SEPTA GTFS ----------------------------------------------

zip.file = "data/gtfs_public.zip"

download.file("https://github.com/septadev/GTFS/releases/latest/download/gtfs_public.zip",
              destfile = zip.file)

unzip(zip.file, exdir = "data")
unzip("data/google_bus.zip", exdir = "data/gtfs")

file.remove(zip.file)
