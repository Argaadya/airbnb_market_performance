
box::use(RSQLite[dbConnect, SQLite, dbDisconnect, dbGetQuery],
         stringi[stri_unescape_unicode]
         )

box::use(app/logic/connect_db)

# Room Type ---------------

in_room <- function() {
  con_db <- connect_db$connect()
  room_list <- dbGetQuery(con_db, 
                          "select distinct room_type
                          from listing
                          order by room_type
                          ")
  
  dbDisconnect(con_db)
  
  room_list <- lapply(room_list$room_type, function(x) {
    list(key = x, text = x)
    })
  
  return(room_list)
}

# Neighbourhood ---------------

in_neighbour <- function() {
  con_db <- connect_db$connect()
  neighbour_list <- dbGetQuery(con_db, 
                               "select neighbourhood, count(distinct id) as n_listing
                               from listing
                               group by neighbourhood
                               order by n_listing desc
                               ")
  
  dbDisconnect(con_db)
  
  neighbour_list <- lapply(neighbour_list$neighbourhood, function(x) {
    list(key = x, text = x)
    })
  
  return(neighbour_list)
}

# Amenities ---------------

in_amenities <- function() {
  con_db <- connect_db$connect()
  
  amenities_list <- dbGetQuery(con_db, 
                               "select amenities, count(distinct listing_id) as n_listing
                               from amenities
                               group by amenities
                               order by n_listing desc
                               ") 
  
  dbDisconnect(con_db)
  
  list_out <- lapply(amenities_list$amenities, function(x) {
    x
  })
  
  names(list_out) <- stri_unescape_unicode(amenities_list$amenities)
  
  return(list_out)
}


# Beds, Accommodates, Bathroom ---------------

in_number <- function() {
  con_db <- connect_db$connect()
  
  max_number <- dbGetQuery(con_db, 
                           "
                           select max(beds) as max_bed, max(accommodates) as max_acommodate, max(bathrooms) as max_bathroom
                           from listing
                           ") 
  
  dbDisconnect(con_db)

return(max_number)
}

