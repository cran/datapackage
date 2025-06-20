
writedatapackage <- function(datapackage, path = attr(datapackage, "path"), 
    filename = attr(datapackage, "filename")) {
  tmp <- removeclasses(datapackage)
  #tmp <- unclass(datapackage)
  attr(tmp, "filename") <- NULL
  attr(tmp, "path") <- NULL
  as_array <- c("contributors/roles",
    "keywords",
    "resources/dialect/headerRows",
    "resources/dialect/commentRows",
    "resources/dialect/itemKeys",
    "resources/schema/fields/missingValues",
    "resources/schema/fields/categories",
    "resources/schema/fields/missingValues",
    "resources/schema/fields/constraints/enum",
    "resources/schema/fields/trueValues",
    "resources/schema/fields/falseValues",
    "resources/schema/primaryKe",
    "resources/schema/uniqueKeys",
    "resources/schema/missingValues")
  json <- to_json(tmp, pretty = TRUE, as_array = as_array)
  writeLines(json, file.path(path, filename))
  #jsonlite::write_json(tmp, file.path(path, filename), 
    #pretty = TRUE, auto_unbox = TRUE)
}

removeclasses <- function(x) {
  classes <- c("fielddescriptor", "datapackage", "dataresource")
  if (is.list(x)) {
    if (any(class(x) %in% classes)) x <- unclass(x)
    x <- lapply(x, removeclasses)
  }
  x
}
