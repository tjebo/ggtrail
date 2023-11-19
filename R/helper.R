## helper functions (copied from internal ggplot2)

`%||%` <- function (a, b) {
  if (!is.null(a))
    a
  else b
}

empty <- function (df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}

check_linewidth <- function (data, name) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    deprecate_soft0("3.4.0", I(paste0("Using the `size` aesthetic with ",
                                      name)), I("the `linewidth` aesthetic"))
    data$linewidth <- data$size
  }
  data
}

snake_class <- function (x) {
  snakeize(class(x)[1])
}

snakeize <- function (x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  to_lower_ascii(x)
}

to_lower_ascii <- function (x) chartr(upper_ascii, lower_ascii, x)

remove_missing <- function (df, na.rm = FALSE, vars = names(df), name = "", finite = FALSE) {
  check_bool(na.rm)
  missing <- detect_missing(df, vars, finite)
  if (any(missing)) {
    df <- df[!missing, , drop = FALSE]
    if (!na.rm) {
      if (name != "")
        name <- paste(" ({.fn ", name, "})", sep = "")
      msg <- paste0("Removed {sum(missing)} rows containing ",
                    if (finite)
                      "non-finite"
                    else "missing", " values", name, ".")
      cli::cli_warn(msg)
    }
  }
  df
}

detect_missing <- function (df, vars, finite = FALSE){
  vars <- intersect(vars, names(df))
  !cases(df[, vars, drop = FALSE], if (finite)
    is_finite
    else is_complete)
}

deprecate_soft0 <- function (..., user_env = NULL) {
  user_env <- user_env %||% getOption("ggplot2_plot_env") %||%
    caller_env(2)
  lifecycle::deprecate_soft(..., user_env = user_env)
}


stop_input_type <- function (x, what, ..., allow_na = FALSE, allow_null = FALSE,
                             show_value = TRUE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  cli <- rlang::env_get_list(nms = c("format_arg", "format_code"),
                             last = topenv(), default = function(x) sprintf("`%s`",
                                                                            x), inherit = TRUE)
  if (allow_na) {
    what <- c(what, cli$format_code("NA"))
  }
  if (allow_null) {
    what <- c(what, cli$format_code("NULL"))
  }
  if (length(what)) {
    what <- oxford_comma(what)
  }
  message <- sprintf("%s must be %s, not %s.", cli$format_arg(arg),
                     what, obj_type_friendly(x, value = show_value))
  abort(message, ..., call = call, arg = arg)
}

oxford_comma <- function (chr, sep = ", ", final = "or"){
  n <- length(chr)
  if (n < 2) {
    return(chr)
  }
  head <- chr[seq_len(n - 1)]
  last <- chr[n]
  head <- paste(head, collapse = sep)
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  }
  else {
    paste0(head, " ", final, " ", last)
  }
}

obj_type_friendly <- function (x, value = TRUE) {
  if (is_missing(x)) {
    return("absent")
  }
  if (is.object(x)) {
    if (inherits(x, "quosure")) {
      type <- "quosure"
    }
    else {
      type <- paste(class(x), collapse = "/")
    }
    return(sprintf("a <%s> object", type))
  }
  if (!is_vector(x)) {
    return(.rlang_as_friendly_type(typeof(x)))
  }
  n_dim <- length(dim(x))
  if (!n_dim) {
    if (!is_list(x) && length(x) == 1) {
      if (is_na(x)) {
        return(switch(typeof(x), logical = "`NA`", integer = "an integer `NA`",
                      double = if (is.nan(x)) {
                        "`NaN`"
                      } else {
                        "a numeric `NA`"
                      }, complex = "a complex `NA`", character = "a character `NA`",
                      .rlang_stop_unexpected_typeof(x)))
      }
      show_infinites <- function(x) {
        if (x > 0) {
          "`Inf`"
        }
        else {
          "`-Inf`"
        }
      }
      str_encode <- function(x, width = 30, ...) {
        if (nchar(x) > width) {
          x <- substr(x, 1, width - 3)
          x <- paste0(x, "...")
        }
        encodeString(x, ...)
      }
      if (value) {
        if (is.numeric(x) && is.infinite(x)) {
          return(show_infinites(x))
        }
        if (is.numeric(x) || is.complex(x)) {
          number <- as.character(round(x, 2))
          what <- if (is.complex(x))
            "the complex number"
          else "the number"
          return(paste(what, number))
        }
        return(switch(typeof(x), logical = if (x) "`TRUE`" else "`FALSE`",
                      character = {
                        what <- if (nzchar(x)) "the string" else "the empty string"
                        paste(what, str_encode(x, quote = "\""))
                      }, raw = paste("the raw value", as.character(x)),
                      .rlang_stop_unexpected_typeof(x)))
      }
      return(switch(typeof(x), logical = "a logical value",
                    integer = "an integer", double = if (is.infinite(x)) show_infinites(x) else "a number",
                    complex = "a complex number", character = if (nzchar(x)) "a string" else "\"\"",
                    raw = "a raw value", .rlang_stop_unexpected_typeof(x)))
    }
    if (length(x) == 0) {
      return(switch(typeof(x), logical = "an empty logical vector",
                    integer = "an empty integer vector", double = "an empty numeric vector",
                    complex = "an empty complex vector", character = "an empty character vector",
                    raw = "an empty raw vector", list = "an empty list",
                    .rlang_stop_unexpected_typeof(x)))
    }
  }
  vec_type_friendly(x)
}

vec_type_friendly <- function (x, length = FALSE) {
  if (!is_vector(x)) {
    abort("`x` must be a vector.")
  }
  type <- typeof(x)
  n_dim <- length(dim(x))
  add_length <- function(type) {
    if (length && !n_dim) {
      paste0(type, sprintf(" of length %s", length(x)))
    }
    else {
      type
    }
  }
  if (type == "list") {
    if (n_dim < 2) {
      return(add_length("a list"))
    }
    else if (is.data.frame(x)) {
      return("a data frame")
    }
    else if (n_dim == 2) {
      return("a list matrix")
    }
    else {
      return("a list array")
    }
  }
  type <- switch(type, logical = "a logical %s", integer = "an integer %s",
                 numeric = , double = "a double %s", complex = "a complex %s",
                 character = "a character %s", raw = "a raw %s", type = paste0("a ",
                                                                               type, " %s"))
  if (n_dim < 2) {
    kind <- "vector"
  }
  else if (n_dim == 2) {
    kind <- "matrix"
  }
  else {
    kind <- "array"
  }
  out <- sprintf(type, kind)
  if (n_dim >= 2) {
    out
  }
  else {
    add_length(out)
  }
}

.rlang_as_friendly_type <- function (type) {
  switch(type, list = "a list", `NULL` = "`NULL`", environment = "an environment",
         externalptr = "a pointer", weakref = "a weak reference",
         S4 = "an S4 object", name = , symbol = "a symbol", language = "a call",
         pairlist = "a pairlist node", expression = "an expression vector",
         char = "an internal string", promise = "an internal promise",
         ... = "an internal dots object", any = "an internal `any` object",
         bytecode = "an internal bytecode object", primitive = ,
         builtin = , special = "a primitive function", closure = "a function",
         type)
}

.rlang_stop_unexpected_typeof <- function (x, call = caller_env()) {
  abort(sprintf("Unexpected type <%s>.", typeof(x)), call = call)
}

cases <- function(x, fun) {
  ok <- vapply(x, fun, logical(nrow(x)))

  # Need a special case test when x has exactly one row, because rowSums
  # doesn't respect dimensions for 1x1 matrices. vapply returns a vector (not
  # a matrix when the input has one row.
  if (is.vector(ok)) {
    all(ok)
  } else {
    # Find all the rows where all are TRUE
    rowSums(as.matrix(ok)) == ncol(x)
  }
}

is_complete <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    !is.na(x)
  }
}

is.waive <- function(x) inherits(x, "waiver")
