options(expiry = 7, # units: days
        key = Sys.getenv("AES_KEY"),
        mysql = httr::parse_url(Sys.getenv("DATABASE_URL")),
        stringsAsFactors = FALSE,
        warn = -1)
for (pkg in readLines("packages.txt")) library(pkg, character.only = TRUE)
rm(list = ls())


#####################
# PREDEFINED VALUES #
#####################

valid_zips = c(
  "94102", "94103", "94104", "94105", "94107",
  "94108", "94109", "94110", "94111", "94112",
  "94114", "94115", "94116", "94117", "94118",
  "94119", "94120", "94121", "94122", "94123",
  "94124", "94125", "94126", "94127", "94129",
  "94130", "94131", "94132", "94133", "94134",
  "94137", "94139", "94140", "94141", "94142",
  "94143", "94144", "94145", "94146", "94147",
  "94151", "94158", "94159", "94160", "94161",
  "94163", "94164", "94172", "94177", "94188"
)


########################
# ENCRYPTION UTILITIES #
########################

issue_token = function(user_pk, key = options()$key, expiry = options()$expiry) {
  claim = jwt_claim(user = user_pk, exp = unclass(Sys.time() + expiry * 86400))
  jwt_encode_hmac(claim = claim, secret = charToRaw(key))
}

verify_token = function(jwt, user_pks, key = options()$key) {
  claim = tryCatch(
    expr = jwt_decode_hmac(jwt = jwt, secret = charToRaw(key)),
    error = function(e) list(exp = 0)
  )
  if (claim$exp > Sys.time() && claim$user %in% user_pks) claim$user else NULL
}


######################
# DATABASE UTILITIES #
######################

connect = function() {
  dbConnect(drv = MySQL(),
            user = options()$mysql$username,
            password = options()$mysql$password,
            host = options()$mysql$hostname,
            port = as.integer(options()$mysql$port),
            dbname = options()$mysql$path)
}

run_query = function(query) {
  con = connect()
  suppressWarnings(dbGetQuery(con, query))
  dbDisconnect(con)
}

load_table = function(table) {
  query = paste("SELECT * FROM", table, if (table == "tokens") "WHERE expires_on > NOW();" else ";")
  con = connect()
  x = suppressWarnings(dbGetQuery(con, query))
  dbDisconnect(con)
  # Format datetime fields
  cols = grep("_on$", colnames(x))
  if (length(cols) > 0) for (col in cols) x[, col] = with_tz(as_datetime(x[, col]), Sys.timezone())
  # Format boolean fields
  cols = grep("^is_", colnames(x))
  if (length(cols) > 0) for (col in cols) x[, col] = as.logical(x[, col])
  x
}


##################
# DATA UTILITIES #
##################

lookup = function(lookup_table, mask = "desc") {
  lookup_table = as.data.frame(lookup_table)
  select_options = lookup_table[, min(grep("_pk$", colnames(lookup_table)))]
  names(select_options) = lookup_table[, grep(sprintf("_%s$", mask), colnames(lookup_table))]
  select_options[order(names(select_options))]
}


#########################
# APPLICATION UTILITIES #
#########################

generate_error = function(error_list = NULL) {
  showModal(modalDialog(
    HTML(paste0(
      "Please fix the following errors and try again:<br /><ul>",
      paste0("<li>", error_list, "</li>", collapse = ""),
      "</ul>"
    )),
    easyClose = TRUE,
    footer = NULL,
    size = "s"
  ))
}

generate_message = function(message) {
  showModal(modalDialog(
    tags$div(HTML(message), style = "text-align: center;"),
    easyClose = TRUE,
    footer = NULL,
    size = "s"
  ))
}


#######################
# TEMPORARY BUG FIXES #
#######################

textAreaInput2 = function(inputId, label, value = "", rows = NULL, placeholder = NULL) {
  value = restoreInput(id = inputId, default = value)
  div(class = "form-group",
      tags$label(label, `for` = inputId),
      tags$textarea(id = inputId, class = "form-control",
                    placeholder = placeholder, style = "max-width: 100%;",
                    rows = rows, value))
}


jsCode = sprintf('
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    } else {
      var cookie = "";
      Shiny.onInputChange("jscookie", cookie);
    }
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: %s }); // units: days
    Shiny.onInputChange("jscookie", params);
  }
  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.onInputChange("jscookie", "");
  }
', options()$expiry)

