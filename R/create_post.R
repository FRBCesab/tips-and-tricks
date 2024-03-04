#' Create a blog post file
#'
#' @param title a `character` of length 1. The title of the post.
#'
#' @return No return value.
#' 
#' @export
#'
#' @examples
#' ## create_post(title = "Code snippets in RStudio")

create_post <- function(title) {
  
  ## Check args ----
  
  if (missing(title)) {
    stop("Argument 'title' is required", call. = FALSE)
  }
  
  if (!is.character(title)) {
    stop("Argument 'title' must be a character", call. = FALSE)
  }
  
  if (length(title) != 1) {
    stop("Argument 'title' must be a character of length 1", call. = FALSE)
  }
  
  
  ## Create slug for folder & file name ----
  
  slug <- tolower(title)
  slug <- gsub("[[:punct:]]", "", slug)
  slug <- gsub("\\s+", " ", slug)
  slug <- trimws(slug)
  slug <- gsub("\\s", "-", slug)
  
  slug <- paste(Sys.Date(), slug, sep = "-")
  
  
  ## Create YAML header ----
  
  yaml <- "---"
  yaml <- c(yaml, paste0("title: \"", title, "\""))
  yaml <- c(yaml, paste0("author: \"\""))
  yaml <- c(yaml, paste0("date: \"", Sys.Date(), "\""))
  yaml <- c(yaml, paste0("categories: [tag1, tag2]"))
  yaml <- c(yaml, paste0("image: \"\""))
  yaml <- c(yaml, "---")
  yaml <- c(yaml, "")
  yaml <- c(yaml, "Post content...")
  yaml <- c(yaml, "")
  
  yaml <- paste0(yaml, collapse = "\n")
  
  
  ## Create folder and .qmd file ----
  
  dir.create(file.path("posts", slug), showWarnings = FALSE)
  
  if (file.exists(file.path("posts", slug, paste0(slug, ".qmd")))) {
    stop("This post already exists.", call. = FALSE)
  }
  
  cat(yaml, file = file.path("posts", slug, paste0(slug, ".qmd")))
  
  
  invisible(NULL)
}
