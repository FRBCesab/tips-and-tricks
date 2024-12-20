#' Create a blog post file
#'
#' @param title a `character` of length 1. The title of the post.
#' 
#' @param date a `character` of length 1. The date of the post in the format
#'   `YYYY/MM/DD`. If `NULL` the date of today will be used (default).
#'
#' @return No return value. A sub-folder and a `.qmd` file will be created in 
#' `posts/` and the `.qmd` file will be opened in the editor.
#' 
#' @export
#'
#' @examples
#' ## create_post(title = "Code snippets in RStudio")

create_post <- function(title, date = NULL) {
  
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
  
  if (!is.null(date)) {
    
    if (!is.character(date)) {
      stop("Argument 'date' must be a character", call. = FALSE)
    }
    
    if (length(date) != 1) {
      stop("Argument 'date' must be a character of length 1", call. = FALSE)
    }
    
    date_pattern <- "^\\d{4}(-|/)\\d{2}(-|/)\\d{2}$"
    
    if (length(grep(date_pattern, date)) == 0) {
      stop("The argument 'date' must have the format 'YYYY/MM/DD'",
           call. = FALSE)
    }
  }
  
  
  ## Prepare post date ----
  
  if (is.null(date)) {
    
    date <- Sys.Date()
    
  } else {
    
    date <- as.Date(date)
  }
  
  
  ## Create slug for branch, folder & file name ----
  
  slug <- tolower(title)
  slug <- gsub("[[:punct:]]", "", slug)
  slug <- gsub("\\s+", " ", slug)
  slug <- trimws(slug)
  slug <- gsub("\\s", "-", slug)
  
  slug <- paste(date, slug, sep = "-")
  
  
  ## Create new git branch & checkout ----
  
  cli::cli_alert_success("Creating git branch {.val {slug}}")
  cli::cli_alert_success("Switching to branch {.val {slug}}")
  
  gert::git_branch_create(slug, checkout = TRUE)
  
  
  ## Create YAML header ----
  
  yaml <- "---"
  yaml <- c(yaml, paste0("title: \"", title, "\""))
  yaml <- c(yaml, paste0("author: \"\""))
  yaml <- c(yaml, paste0("date: \"", date, "\""))
  yaml <- c(yaml, paste0("categories: [tag1, tag2]"))
  yaml <- c(yaml, paste0("image: \"\""))
  yaml <- c(yaml, paste0("toc: true"))
  yaml <- c(yaml, paste0("draft: false"))
  yaml <- c(yaml, paste0("lightbox: true"))
  yaml <- c(yaml, paste0("code-overflow: scroll"))
  yaml <- c(yaml, "---")
  yaml <- c(yaml, "")
  yaml <- c(yaml, "Post content...")
  yaml <- c(yaml, "")
  
  yaml <- paste0(yaml, collapse = "\n")
  
  post_dir <- file.path("posts", slug)
  post_qmd <- file.path("posts", slug, "index.qmd")
  
  
  ## Create post folder ----
  
  dir.create(post_dir, showWarnings = FALSE)
  
  if (file.exists(post_qmd)) {
    stop("This post already exists.", call. = FALSE)
  }
  
  cli::cli_alert_success("Creating {.val {post_dir}} directory")
  
  
  ## Creating .qmd file ----
  
  cli::cli_alert_success("Creating {.val {post_qmd}} file")
  
  cat(yaml, file = post_qmd)
  
  
  ## Opening .qmd file ----

  file.edit(file.path("posts", slug, "index.qmd"))
  
  
  invisible(NULL)
}
