#' Get the current user role
#'
#' Determines the user role from the dashboard configuration.
#' Valid roles: `"admin"` (NRO/national), `"clinical"` (own centre),
#' `"research"` (aggregated + exports).
#'
#' @param config A `dashboard_config` object.
#'
#' @return Character string: `"admin"`, `"clinical"`, or `"research"`.
#'
#' @export
#' @family access-control
get_user_role <- function(config = dashboard_config()) {
  role <- config$user_role %||% "admin"
  match.arg(role, c("admin", "clinical", "research"))
}

#' Get the user's assigned centre
#'
#' Returns the centre code for clinical users. Returns `NULL` for
#' admin users (who can see all centres).
#'
#' @param config A `dashboard_config` object.
#'
#' @return Character centre code or `NULL`.
#'
#' @export
#' @family access-control
get_user_centre <- function(config = dashboard_config()) {
  centre <- config$user_centre %||% ""
  if (nzchar(centre)) centre else NULL
}

#' Filter data by user role
#'
#' Applies role-based data restrictions:
#' - **Admin:** No filtering — sees all centres.
#' - **Clinical:** Sees only own centre data.
#' - **Research:** Sees all centres (aggregated views only; patient lists
#'   enabled via drill-down).
#'
#' @param data A tibble of audit data.
#' @param role Character: `"admin"`, `"clinical"`, or `"research"`.
#' @param user_centre Character centre code for clinical users.
#'
#' @return Filtered tibble.
#'
#' @export
#' @family access-control
filter_by_role <- function(data, role = "admin", user_centre = NULL) {
  if (role == "clinical" && !is.null(user_centre)) {
    data <- data |>
      dplyr::filter(.data$centre_code == user_centre)
  }
  data
}

#' Role display label
#'
#' Returns a human-readable label for the role indicator badge.
#'
#' @param role Character role string.
#' @param user_centre Optional centre code.
#'
#' @return Character string for display.
#'
#' @export
#' @family access-control
role_display_label <- function(role, user_centre = NULL) {
  switch(role,
    admin    = "Admin (National)",
    clinical = paste0("Clinical", if (!is.null(user_centre)) paste0(" \u2014 ", user_centre) else ""),
    research = "Research",
    role
  )
}
