
# httppipe

dt <- reticulate::import("docker.transport")

"NpipeHTTPAdapter" %in% names(dt)

# windows test

docr <- reticulate::import("docker")

client <- docr$from_env()
