#### ENVIRONMENTAL DATA PROCESSINg ####


###--------------------###
### 5. update JSON    ####
###--------------------###

# read existing json
json_ls <- fromJSON(file.path(extFolderName, "metadata.json"))

# define json content
json_ls$step_2b <- list(
  foo = "x"
)

# write json
jsonlite:::write_json(json_ls,
                      file.path(extFolderName, "metadata.json"), 
                      pretty = TRUE)

