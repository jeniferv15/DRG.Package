library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(data.table)
library(magrittr)
options(scipen = 999)

#' Payment DRG Function
#'
#' @param df a data frame
#' @param payment can select: Average Covered Charges, Average Total Payments, or Average Medicare Payments. Must be in " " when using.
#'
#' @return A box plot with all DRG codes on the x axis and payment parameter plotted on y axis
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' library(data.table)
#' df.test <- data.table::fread('https://data.cms.gov/api/views/97k6-zzx3/rows.csv?accessType=DOWNLOAD')
#' payment_DRG(df.test, "Average Covered Charges")
#'
payment_DRG <- function(df, payment) {
  data <- df %>%
    dplyr::select(
      ##selecting appropriate data
      `DRG Definition`,
      `Average Covered Charges`,
      `Average Total Payments`,
      `Average Medicare Payments`
    ) %>%
    tidyr::separate(col = `DRG Definition`, c("DRG Code", "DRG Definition"), " - ") %>% ## separating DRG Definition col into 2 col
    dplyr::group_by(`DRG Code`) %>%  ## group by code
    dplyr::arrange(`DRG Code`) ## ordering DRG code in increasing order

  if (payment %in% c("Average Covered Charges" ,
                     ## payment options
                     "Average Total Payments" ,
                     "Average Medicare Payments")) {
    ggplot2::ggplot(data, ggplot2::aes(x = `DRG Code`, y = get(payment) / 10000)) + ## ggplot set up
      ggplot2::geom_boxplot() + ## adding box plot
      ggplot2::scale_y_continuous(trans = 'log10') + ## transform to log base 10 scale
      ggplot2::ylab('Per Ten Thousand US Dollars (Log Base 10)') + ## changing y lab
      ggplot2::ggtitle(paste0(payment, ' Box Plot')) + ## changing plot title
      ggplot2::theme_bw() + ## changing theme
      ggplot2::theme(
        axis.text.x = element_text(angle = 60, ## changing orientation of x axis
                                   hjust = 1),
        text = element_text(size = 9),
        ## changing text size
        plot.title = element_text(hjust = 0.5)
      ) ## centering title
  }
  else {
    print(
      "Error: Please select Average Covered Charges, Average Total Payments, or Average Medicare Payments."
    )
  }
}

#' Statistic DRG Function
#'
#' @param stat Select statistic of interest: Mean, median, or standard deviation. Must be in " " when using.
#' @param DRG_code Select DRG code of interest. Must be in " " when using.
#' @param df Data frame
#'
#' @return The mean, median, or sd of average medicare payments for DRG code of interest
#' @export
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' library(data.table)
#' df.test <- data.table::fread('https://data.cms.gov/api/views/97k6-zzx3/rows.csv?accessType=DOWNLOAD')
#' stat_DRG(df.test, "mean", "065")
#'
stat_DRG <- function(df, stat, DRG_code) {
  data <- df %>%
    dplyr::select(##selecting appropriate data
      `DRG Definition`,
      `Average Medicare Payments`) %>%
    tidyr::separate(col = `DRG Definition`, c("DRG.Code", "DRG.Definition"), " - ") %>% ## separating DRG Definition col into 2 col
    dplyr::group_by(DRG.Code)%>% ## group by code
    dplyr::filter(DRG.Code == DRG_code) ## filter DRG code alone

  if (DRG_code %in% unique(data$DRG.Code)) {
    if (stat %in% c("Mean", "mean")) {
      paste0(
        'The mean for averge medicare payment for DRG code #',
        paste0(DRG_code),' - ', paste0(data$DRG.Definition[1]),
        ' is $',
        round(mean(data$`Average Medicare Payments`), 2),
        '.'
      )
    }
    else if (stat %in% c("Median", "median")) {
      paste0(
        'The median for averge medicare payment for DRG code #',
        paste0(DRG_code),' - ', paste0(data$DRG.Definition[1]),
        ' is $',
        round(median(data$`Average Medicare Payments`), 2),
        '.'
      )
    }
    else if (stat %in% c("SD", "sd", "standard deviation", "Standard Deviation")) {
      paste0(
        'The standard deviation for averge medicare payment for DRG code #',
        paste0(DRG_code),' - ', paste0(data$DRG.Definition[1]),
        ' is $',
        round(sd(data$`Average Medicare Payments`), 2),
        '.'
      )
    }
    else {
      print(
        "Error: Please select statistic of interest, mean, median, or sd (standard deviation)."
      )

    }
  } else {
    print("Error: Please select DRG code.")
  }
}
