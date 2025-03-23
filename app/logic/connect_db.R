
box::use(
  RSQLite[dbConnect, SQLite, dbDisconnect]
)

#' @export
connect <- function() { dbConnect(drv =  SQLite(), "data/airbnb_sg.sqlite") }


