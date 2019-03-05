# Generates cleaned purchase data for a given item
library(data.table)
library(purrr)
library(furrr)
library(stringr)
library(geosphere)
plan(multiprocess)
threads <- 8

# Files
path <- "/scratch/upenn/hossaine/"
retailers <- fread(paste0(path, "retailers.csv"))
panel <- fread(paste0(path, "fullPanel.csv"))
zipImpute <- fread(paste0(path, "zipImpute.csv"))
pce <- fread("Nielsen/pce.csv")

#' Gets purchase data for item from Nielsen
#'
#' @param prod data.table of products to pull
#'
#' @return
#' @export
#'
#' @examples
getItem <- function(prod) {
  print("Getting purchases")
  itemPurch <- getPurch(prod)
  itemPurch <- merge(itemPurch, zipImpute, by = c("retailer_code", "store_code_uc"), all.x = TRUE)
  itemPurch[, c("store_zip3", "total_spent") := NULL]
  itemPurch <- merge(itemPurch, prod, by = c("upc", "upc_ver_uc"))
  itemPurch <- merge(itemPurch, panel, by = c("household_code", "panel_year"))
  itemPurch[, "brand_code_uc" := ifelse(brand_code_uc == 536746,
                                      paste0(brand_code_uc, retailer_code),
                                      as.character(brand_code_uc))]

  # Getting real spending
  itemPurch[, "month" := as.integer(substr(purchase_date, 6, 7))]
  itemPurch <- merge(itemPurch, pce, by = c("panel_year", "month"))
  setnames(itemPurch, "value", "pce")
  itemPurch[, "total_price_paid_real" := total_price_paid / pce * 100]

  # # Computing distances
  # print("Computing distances")
  # hh <- as.matrix(itemPurch[, .(lon, lat)])
  # store <- as.matrix(itemPurch[, .(store_lon, store_lat)])
  # coords <- cbind(hh, store)
  # distance <- apply(coords, 1, function(x) distm(x = x[1:2], y = x[3:4]))
  # itemPurch[, "distKM" := distance / 1000]
  return(itemPurch)
}

#' Combines all purchases together
#'
#' @param prod data.table of products to compile
#'
#' @return data.table of all purchases
getPurch <- function(prod) {
  purch <- fread(paste0(path, "purchase.csv"), nThread = threads)
  purch <- merge(purch, prod, by = c("upc", "upc_ver_uc"))
  purch[deal_flag_uc == 0 & coupon_value == 0, "deal_type_ind" := 1L]    # No deal
  purch[deal_flag_uc == 1 & coupon_value == 0, "deal_type_ind" := 10L]  # Sale only
  purch[deal_flag_uc == 1 & coupon_value > 0, "deal_type_ind" := 100L] # Sale and/or coupon

  purch <- purch[, .(quantity = sum(quantity),
                     total_price_paid = sum(total_price_paid),
                     coupon = sum(coupon_value),
                     deal_type_ind = sum(deal_type_ind)),
                 by = .(upc, upc_ver_uc, trip_code_uc)]
  purch[, "deal_type" := cut(deal_type_ind, c(0, 9, 99, 999),
                             c("No Deal", "Sale Only", "Sale and/or Coupon"))]
  purch[, "deal_type_ind" := NULL]

  trips <- fread(paste0(path, "trips.csv"), nThread = threads)
  fullData <- merge(purch, trips, by = "trip_code_uc")
  fullData <- merge(fullData, retailers, by = "retailer_code")
  return(fullData)
}
