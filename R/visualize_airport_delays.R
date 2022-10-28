#' Handling large datasets with dplyr
#' @importFrom ggplot2 ggplot geom_point labs
#' @import nycflights13
#' @importFrom dplyr group_by summarise inner_join
#' @export
# library(ggplot2)
# library(dplyr)

visualize_airport_delays<- function(){
  airports <- nycflights13::airports
  flights <- nycflights13::flights


  mean_delay<- flights %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(delay=mean(arr_delay,na.rm=TRUE)) %>%
    dplyr::inner_join(airports ,by= c("dest"="faa"))

  mean_delay %>%
    ggplot2::ggplot(aes(lon,lat,color=delay))+
    ggplot2::geom_point(aes(color=delay),size=2)+
    ggplot2::labs(title= "Visualizing the mean delay of flights for different airports", x= "Longitude",y="Latitude")



}
# visualize_airport_delays()
