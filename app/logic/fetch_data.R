
box::use(
  scales[comma],
  dplyr[case_when, mutate],
  shiny,
  RSQLite[dbConnect, SQLite, dbDisconnect, dbGetQuery]
)

box::use(app/logic/connect_db)

#' @export
listing_performance <- function(start_date, end_date, neighbourhood = NULL, room = NULL, accom_num, bed_num, bath_num, is_superhost = F, is_instant = F, amenities_provided = NULL) {
  
  input_start <- strftime(start_date, "%Y-%m-%d")
  input_end <- strftime(end_date, "%Y-%m-%d")
  
  input_neighbour <- if ( length(neighbourhood) > 0  ) {
      
    x_temp <- paste0("'", neighbourhood, "'") |> 
      paste(collapse = ",")
    
    paste0(" and a.neighbourhood in (",  x_temp,")")
    
  } else {
    ""
  }
  
  input_room <- if ( length(room) > 0 ) {
    
    x_temp <- paste0("'", room, "'") |> 
      paste(collapse = ",") 
    
    paste0(" and a.room_type in (",  x_temp,")") 
  } else {
    ""
  }
  
  input_superhost <- if ( is_superhost == T) {
    paste0(" and b.host_is_superhost = TRUE")
  } else {
    ""
  }
  
  input_instant <- if ( is_instant == T ) {
    paste0(" and a.instant_bookable  = TRUE")
  } else {
    ""
  }  
  
  con_db <- connect_db$connect()
  
  query <- if ( length(amenities_provided) > 0  ) {
    
    input_amenities <- paste0("'", amenities_provided, "'") |> 
      paste(collapse = ",")
    
    input_amenities <- 
      paste0("filterd_list as (
                select distinct listing_id
                from amenities
                where
                  amenities in (",  input_amenities,")
             )"
      )
    
    query <- paste0("
                  with 
                  summary_review as (
                    select
                      listing_id,
                      count(distinct id) as number_review,
                      sum(case when sentiment = 'Positive' then 1.0 else 0 end) as positive_review,
                      sum(case when sentiment = 'Negative' then 1.0 else 0 end) as negative_review,
                      sum(case when sentiment = 'Positive' then 1.0 else 0 end) / count(distinct id) as percent_positive,
                      sum(case when sentiment = 'Negative' then 1.0 else 0 end) / count(distinct id) as percent_negative
                    from review
                    group by listing_id
                  ),
                  performance as (
                  
                  select
                    listing_id,
                    sum(available) as available,
                    count(distinct date_period) as n_day,
                    1 - cast( sum( available + 0.0 ) as numeric) / count(distinct date_period) as occupancy_rate,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) as earning,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) / ( count(distinct date_period) - sum( available)  ) as average_daily_rate
                  from listing_calendar 
                  where 
                    date_period between '", input_start ,"' and '", input_end,"' 
                  group by listing_id
                  ),
                  
                  listing_join as (
                  select
                    a.id,
                    a.name,
                    a.neighbourhood,
                    a.latitude,
                    a.longitude,
                    a.room_type,
                    a.accommodates,
                    a.bathrooms,
                    a.beds,
                    a.instant_bookable,
                    a.review_scores_rating,
                    a.listing_url,
                    a.picture_url,
                    b.host_id,
                    b.host_name,
                    b.host_is_superhost,
                    b.host_url,
                    b.host_picture_url,
                    b.host_since
                    
                from listing a
                left join host b
                  on a.host_id = b.host_id
                where
                  coalesce(a.accommodates, 0) between ", accom_num[1], " and ", accom_num[2],"
                  and coalesce(a.beds, 0) between ", bed_num[1], " and ", bed_num[2],"
                  and coalesce(a.bathrooms, 0) between ", bath_num[1], " and ", bath_num[2],"
                  ",
                    input_room,
                    input_neighbour,
                    input_superhost, 
                    input_instant, "
                  ), 
                  ", input_amenities,"
                  
                  select
                    a.*,
                    
                    c.available,
                    c.n_day,
                    c.occupancy_rate,
                    c.earning,
                    c.average_daily_rate,
                    
                    b.number_review,
                    b.positive_review,
                    b.negative_review,
                    b.percent_positive,
                    b.percent_negative
                    
                  from listing_join a
                  left join summary_review b
                    on a.id = b.listing_id
                  left join performance c
                    on a.id = c.listing_id
                  join filterd_list d
                    on a.id = d.listing_id
                  ")
    
  } else {
    
    query <- paste0("
                  with 
                  summary_review as (
                    select
                      listing_id,
                      count(distinct id) as number_review,
                      sum(case when sentiment = 'Positive' then 1.0 else 0 end) as positive_review,
                      sum(case when sentiment = 'Negative' then 1.0 else 0 end) as negative_review,
                      sum(case when sentiment = 'Positive' then 1.0 else 0 end) / count(distinct id) as percent_positive,
                      sum(case when sentiment = 'Negative' then 1.0 else 0 end) / count(distinct id) as percent_negative
                    from review
                    group by listing_id
                  ),
                  performance as (
                  
                  select
                    listing_id,
                    sum(available) as available,
                    count(distinct date_period) as n_day,
                    1 - cast( sum( available + 0.0 ) as numeric) / count(distinct date_period) as occupancy_rate,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) as earning,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) / ( count(distinct date_period) - sum( available)  ) as average_daily_rate
                  from listing_calendar 
                  where 
                    date_period between '", input_start ,"' and '", input_end,"' 
                  group by listing_id
                  ),
                  
                  listing_join as (
                  select
                    a.id,
                    a.name,
                    a.neighbourhood,
                    a.latitude,
                    a.longitude,
                    a.room_type,
                    a.accommodates,
                    a.bathrooms,
                    a.beds,
                    a.instant_bookable,
                    a.review_scores_rating,
                    a.listing_url,
                    a.picture_url,
                    b.host_id,
                    b.host_name,
                    b.host_is_superhost,
                    b.host_url,
                    b.host_picture_url,
                    b.host_since
                    
                from listing a
                left join host b
                  on a.host_id = b.host_id
                where
                  coalesce(a.accommodates, 0) between ", accom_num[1], " and ", accom_num[2],"
                  and coalesce(a.beds, 0) between ", bed_num[1], " and ", bed_num[2],"
                  and coalesce(a.bathrooms, 0) between ", bath_num[1], " and ", bath_num[2],"
                  ",
                    input_room,
                    input_neighbour,
                    input_superhost, 
                    input_instant, "
                  )
                  select
                    a.*,
                    
                    c.available,
                    c.n_day,
                    c.occupancy_rate,
                    c.earning,
                    c.average_daily_rate,
                    
                    b.number_review,
                    b.positive_review,
                    b.negative_review,
                    b.percent_positive,
                    b.percent_negative
                    
                  from listing_join a
                  left join summary_review b
                    on a.id = b.listing_id
                  left join performance c
                    on a.id = c.listing_id
                  ")
  }  
  
  result <- dbGetQuery(con_db, query) |> 
    mutate(review_scores_rating = ifelse(is.na(number_review), NA, review_scores_rating))
  
  dbDisconnect(con_db)
  
  return(result)
}


#' @export
host_performance <- function(start_date, end_date, neighbourhood = NULL, room = NULL, accom_num, bed_num, bath_num, is_superhost = F, is_instant = F, amenities_provided = NULL) {
  
  input_start <- strftime(start_date, "%Y-%m-%d")
  input_end <- strftime(end_date, "%Y-%m-%d")
  
  input_neighbour <- if ( length(neighbourhood) > 0  ) {
    
    x_temp <- paste0("'", neighbourhood, "'") |> 
      paste(collapse = ",")
    
    paste0(" and a.neighbourhood in (",  x_temp,")")
    
  } else {
    ""
  }
  
  input_room <- if ( length(room) > 0 ) {
    
    x_temp <- paste0("'", room, "'") |> 
      paste(collapse = ",") 
    
    paste0(" and a.room_type in (",  x_temp,")") 
  } else {
    ""
  }
  
  input_superhost <- if ( is_superhost == T) {
    paste0(" and b.host_is_superhost = TRUE")
  } else {
    ""
  }
  
  input_instant <- if ( is_instant == T ) {
    paste0(" and a.instant_bookable  = TRUE")
  } else {
    ""
  }
  
  con_db <- connect_db$connect()
  
  query <- if ( length(amenities_provided) > 0  ) {
    
    input_amenities <- paste0("'", amenities_provided, "'") |> 
      paste(collapse = ",")
    
    input_amenities <- 
      paste0("filterd_list as (
                select distinct listing_id
                from amenities
                where
                  amenities in (",  input_amenities,")
             )"
      )
    
    query <- paste0("
                  with 
                  
                  ", input_amenities," ,
                  
                  summary_review as (
                    select
                      b.host_id,
                      count(distinct a.id) as number_review,
                      sum(case when sentiment = 'Positive' then 1.0 else 0 end) as positive_review,
                      sum(case when sentiment = 'Negative' then 1.0 else 0 end) as negative_review,
                      sum(case when sentiment = 'Positive' then 1.0 else 0 end) / count(distinct a.id) as percent_positive,
                      sum(case when sentiment = 'Negative' then 1.0 else 0 end) / count(distinct a.id) as percent_negative
                    from review a
                    left join listing b
                      on a.listing_id = b.id
                    join filterd_list c
                      on a.listing_id = c.listing_id
                    group by b.host_id
                  ),
                  performance as (
                  
                  select
                    b.host_id,
                    sum(available) as available,
                    count(1) as n_day,
                    1 - cast( sum( available + 0.0 ) as numeric) / count(1) as occupancy_rate,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) as earning,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) / ( count(1) - sum( available)  ) as average_daily_rate
                  from listing_calendar a
                  left join listing b
                    on a.listing_id = b.id
                  join filterd_list c
                    on a.listing_id = c.listing_id
                  where 
                    a.date_period between '", input_start ,"' and '", input_end,"' 
                  group by b.host_id
                  ),
                  
                  listing_join as (
                  select
                    a.host_id,
                    b.host_name,
                    b.host_is_superhost,
                    b.host_url,
                    b.host_picture_url,
                    b.host_since,
                    count(distinct a.id) as n_listing,
                    avg(a.review_scores_rating) as review_scores_rating,
                    sum(case when room_type = 'Entire home/apt' then 1 else 0 end) as n_entire_home,
                    sum(case when room_type = 'Hotel room' then 1 else 0 end) as n_hotel_room,
                    sum(case when room_type = 'Private room' then 1 else 0 end) as n_private_room,
                    sum(case when room_type = 'Shared room' then 1 else 0 end) as n_shared_room
                    
                  from listing a
                  left join host b
                    on a.host_id = b.host_id
                  join filterd_list c
                    on a.id = c.listing_id
                    
                  where
                    coalesce(a.accommodates, 0) between ", accom_num[1], " and ", accom_num[2],"
                    and coalesce(a.beds, 0) between ", bed_num[1], " and ", bed_num[2],"
                    and coalesce(a.bathrooms, 0) between ", bath_num[1], " and ", bath_num[2],"
                    ",
                    input_room,
                    input_neighbour,
                    input_superhost, 
                    input_instant, "
                      
                  group by
                    a.host_id,
                    b.host_name,
                    b.host_is_superhost,
                    b.host_url,
                    b.host_picture_url,
                    b.host_since
                    
                    )
                    
                    select
                      a.*,
                      
                      c.available,
                      c.n_day,
                      c.occupancy_rate,
                      c.earning,
                      c.average_daily_rate,
                      
                      b.number_review,
                      b.positive_review,
                      b.negative_review,
                      b.percent_positive,
                      b.percent_negative
                      
                    from listing_join a
                    left join summary_review b
                      on a.host_id = b.host_id
                    left join performance c
                      on a.host_id = c.host_id
                      
                    order by c.earning desc
                  ")
    
    
    
  } else {
    
    query <- paste0("
                  with 
                  summary_review as (
                    select
                      b.host_id,
                      count(distinct a.id) as number_review,
                      sum(case when sentiment = 'Positive' then 1.0 else 0 end) as positive_review,
                      sum(case when sentiment = 'Negative' then 1.0 else 0 end) as negative_review,
                      sum(case when sentiment = 'Positive' then 1.0 else 0 end) / count(distinct a.id) as percent_positive,
                      sum(case when sentiment = 'Negative' then 1.0 else 0 end) / count(distinct a.id) as percent_negative
                    from review a
                    left join listing b
                      on a.listing_id = b.id
                    group by b.host_id
                  ),
                  performance as (
                  
                  select
                    b.host_id,
                    sum(available) as available,
                    count(1) as n_day,
                    1 - cast( sum( available + 0.0 ) as numeric) / count(1) as occupancy_rate,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) as earning,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric)/ ( count(1) - sum( available)  ) as average_daily_rate
                  from listing_calendar a
                  left join listing b
                    on a.listing_id = b.id
                  where 
                    a.date_period between '", input_start ,"' and '", input_end,"' 
                  group by b.host_id
                  ),
                  
                  listing_join as (
                  select
                    a.host_id,
                    b.host_name,
                    b.host_is_superhost,
                    b.host_url,
                    b.host_picture_url,
                    b.host_since,
                    count(distinct a.id) as n_listing,
                    avg(a.review_scores_rating) as review_scores_rating,
                    sum(case when room_type = 'Entire home/apt' then 1 else 0 end) as n_entire_home,
                    sum(case when room_type = 'Hotel room' then 1 else 0 end) as n_hotel_room,
                    sum(case when room_type = 'Private room' then 1 else 0 end) as n_private_room,
                    sum(case when room_type = 'Shared room' then 1 else 0 end) as n_shared_room
                    
                from listing a
                left join host b
                  on a.host_id = b.host_id
                where
                  coalesce(a.accommodates, 0) between ", accom_num[1], " and ", accom_num[2],"
                  and coalesce(a.beds, 0) between ", bed_num[1], " and ", bed_num[2],"
                  and coalesce(a.bathrooms, 0) between ", bath_num[1], " and ", bath_num[2],"
                  ",
                    input_room,
                    input_neighbour,
                    input_superhost, 
                    input_instant, "
                    
                group by
                  a.host_id,
                  b.host_name,
                  b.host_is_superhost,
                  b.host_url,
                  b.host_picture_url,
                  b.host_since
                  
                  )
                  select
                    a.*,
                    
                    c.available,
                    c.n_day,
                    c.occupancy_rate,
                    c.earning,
                    c.average_daily_rate,
                    
                    b.number_review,
                    b.positive_review,
                    b.negative_review,
                    b.percent_positive,
                    b.percent_negative
                    
                  from listing_join a
                  left join summary_review b
                    on a.host_id = b.host_id
                  left join performance c
                    on a.host_id = c.host_id
                    
                  order by c.earning desc
                  ")
  }  
  
  result <- dbGetQuery(con_db, query) |> 
    mutate(review_scores_rating = ifelse(is.na(number_review), NA, review_scores_rating))
  
  dbDisconnect(con_db)
  
  return(result)
}


#' @export
trend_performance <- function(start_date, end_date, neighbourhood = NULL, room = NULL, accom_num, bed_num, bath_num, is_superhost = F, is_instant = F, amenities_provided = NULL) {
  
  input_start <- strftime( start_date, "%Y-%m-%d")
  input_end <- strftime( end_date, "%Y-%m-%d")
  
  input_neighbour <- if ( length(neighbourhood) > 0  ) {
    
    x_temp <- paste0("'", neighbourhood, "'") |> 
      paste(collapse = ",")
    
    paste0(" and a.neighbourhood in (",  x_temp,")")
    
  } else {
    ""
  }
  
  input_room <- if ( length(room) > 0 ) {
    
    x_temp <- paste0("'", room, "'") |> 
      paste(collapse = ",") 
    
    paste0(" and a.room_type in (",  x_temp,")") 
  } else {
    ""
  }
  
  input_superhost <- if ( is_superhost == T) {
    paste0(" and b.host_is_superhost = TRUE")
  } else {
    ""
  }
  
  input_instant <- if ( is_instant == T ) {
    paste0(" and a.instant_bookable  = TRUE")
  } else {
    ""
  }
  
  con_db <- connect_db$connect()
  
  query <- if ( length(amenities_provided) > 0  ) {
    
    input_amenities <- paste0("'", amenities_provided, "'") |> 
      paste(collapse = ",")
    
    query <- paste0("
                  with 
                  filtered_list as (
                    select distinct listing_id
                    from amenities
                    where
                      amenities in (", input_amenities,")
                  ),
                  listing_join as (
                  select
                    a.id
                    
                  from listing a
                  left join host b
                    on a.host_id = b.host_id
                  join filtered_list c
                    on a.id = c.listing_id
                  where
                    coalesce(a.accommodates, 0) between ", accom_num[1], " and ", accom_num[2],"
                    and coalesce(a.beds, 0) between ", bed_num[1], " and ", bed_num[2],"
                    and coalesce(a.bathrooms, 0) between ", bath_num[1], " and ", bath_num[2],"
                    ",
                    input_room,
                    input_neighbour,
                    input_superhost, 
                    input_instant, "
                  ),
                  trend_performance as (
                  
                  select
                    date_period,
                    sum(available) as available,
                    count(distinct listing_id) as n_listing,
                    1 - cast( sum( available + 0.0 ) as numeric) / count(distinct listing_id) as occupancy_rate,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) as earning,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) / ( count(distinct listing_id) - sum( available)  ) as average_daily_rate
                  from listing_calendar a
                  join listing_join b
                    on a.listing_id = b.id
                  where 
                    date_period between '", input_start ,"' and '", input_end,"' 
                  group by date_period
                  )
                  
                  select * 
                  from trend_performance a
                  ")
    
  } else {
    
    query <- paste0("
                  with 
                  listing_join as (
                  select
                    a.id
                    
                  from listing a
                  left join host b
                    on a.host_id = b.host_id
                  where
                    coalesce(a.accommodates, 0) between ", accom_num[1], " and ", accom_num[2],"
                    and coalesce(a.beds, 0) between ", bed_num[1], " and ", bed_num[2],"
                    and coalesce(a.bathrooms, 0) between ", bath_num[1], " and ", bath_num[2],"
                    ",
                    input_room,
                    input_neighbour,
                    input_superhost, 
                    input_instant, "
                  ),
                  trend_performance as (
                  
                  select
                    date_period,
                    sum(available) as available,
                    count(distinct listing_id) as n_listing,
                    1 - cast( sum( available + 0.0 ) as numeric) / count(distinct listing_id) as occupancy_rate,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) as earning,
                    cast(sum(case
                        when available = 1 then 0
                        else coalesce(adjusted_price, price)
                        end
                        ) * 1.0 as numeric) / ( count(distinct listing_id) - sum( available)  ) as average_daily_rate
                  from listing_calendar a
                  join listing_join b
                    on a.listing_id = b.id
                  where 
                    date_period between '", input_start ,"' and '", input_end,"' 
                  group by date_period
                  )
                  
                  select * 
                  from trend_performance
                  ")
  }  
  
  result <- dbGetQuery(con_db, query)
  
  dbDisconnect(con_db)
  
  return(result)
}


#' @export
get_amenities <- function(start_date, end_date, neighbourhood = NULL, room = NULL, accom_num, bed_num, bath_num, is_superhost = F, is_instant = F, amenities_provided = NULL) {
  
  input_start <- strftime(start_date, "%Y-%m-%d")
  input_end <- strftime(end_date, "%Y-%m-%d")
  
  input_neighbour <- if ( length(neighbourhood) > 0  ) {
    
    x_temp <- paste0("'", neighbourhood, "'") |> 
      paste(collapse = ",")
    
    paste0(" and a.neighbourhood in (",  x_temp,")")
    
  } else {
    ""
  }
  
  input_room <- if ( length(room) > 0 ) {
    
    x_temp <- paste0("'", room, "'") |> 
      paste(collapse = ",") 
    
    paste0(" and a.room_type in (",  x_temp,")") 
  } else {
    ""
  }
  
  input_superhost <- if ( is_superhost == T) {
    paste0(" and b.host_is_superhost = TRUE")
  } else {
    ""
  }
  
  input_instant <- if ( is_instant == T ) {
    paste0(" and a.instant_bookable  = TRUE")
  } else {
    ""
  }
  
  con_db <- connect_db$connect()
  
  query <- if ( length(amenities_provided) > 0  ) {
    
    input_amenities <- paste0("'", amenities_provided, "'") |> 
      paste(collapse = ",")
    
    query <- paste0("
                  with 
                  listing_join as (
                  select
                    a.id
                    
                  from listing a
                  left join host b
                    on a.host_id = b.host_id
                  where
                    coalesce(a.accommodates, 0) between ", accom_num[1], " and ", accom_num[2],"
                    and coalesce(a.beds, 0) between ", bed_num[1], " and ", bed_num[2],"
                    and coalesce(a.bathrooms, 0) between ", bath_num[1], " and ", bath_num[2],"
                    ",
                    input_room,
                    input_neighbour,
                    input_superhost, 
                    input_instant, "
                  ),
                  filtered_calendar as (
                  select
                    distinct listing_id
                  from listing_calendar
                  where 
                    date_period between '", input_start ,"' and '", input_end,"' 
                  )
                  
                  select 
                    amenities,
                    count(distinct id) as n_listing
                  from amenities a
                  join listing_join b
                    on a.listing_id = b.id
                  join filtered_calendar c
                    on a.listing_id = c.listing_id
                  where
                    a.amenities in (", input_amenities,")
                  group by amenities
                  order by n_listing desc
                  ")
    
  } else {
    
    query <- paste0("
                  with 
                  listing_join as (
                  select
                    a.id
                    
                  from listing a
                  left join host b
                    on a.host_id = b.host_id
                  where
                    coalesce(a.accommodates, 0) between ", accom_num[1], " and ", accom_num[2],"
                    and coalesce(a.beds, 0) between ", bed_num[1], " and ", bed_num[2],"
                    and coalesce(a.bathrooms, 0) between ", bath_num[1], " and ", bath_num[2],"
                    ",
                    input_room,
                    input_neighbour,
                    input_superhost, 
                    input_instant, "
                  ),
                  filtered_calendar as (
                  select
                    distinct listing_id
                  from listing_calendar
                  where 
                    date_period between '", input_start ,"' and '", input_end,"' 
                  )
                  
                  select 
                    amenities,
                    count(distinct id) as n_listing
                  from amenities a
                  join listing_join b
                    on a.listing_id = b.id
                  join filtered_calendar c
                    on a.listing_id = c.listing_id
                  group by amenities
                  order by n_listing desc
                  ")
    
  }  
  
  result <- dbGetQuery(con_db, query)
  
  dbDisconnect(con_db)
  
  return(result)
}





