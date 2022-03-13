# Source for "stock data" file ---------------------------------------------------------------------------------------------

# Libraries
library(DBI)
library(RPostgres)


# Source functions
source("C:/Users/brent/Documents/VS_Code/postgres/postgres/return_attributes.R")


# Connect to db
con <- stock_master_connect()


# Read data
sql1 <- "
select 
ra.* 
,fa.fin_nonfin
,fa.report_date
,fa.publish_date
,fa.cash_equiv_st_invest
,fa.total_cur_assets
,fa.intang_asset
,fa.total_noncur_assets
,fa.total_assets
,fa.st_debt
,fa.total_cur_liab
,fa.lt_debt
,fa.total_noncur_liab
,fa.total_liab
,fa.total_equity
,fa.net_income_qtly
,fa.cash_ratio
,fa.ttm_earnings
,fa.ttm_earnings_max
,fa.total_equity_cln
,fa.asset_growth
,fa.roa
,fa.roe
,fa.leverage
,fa.other_ca_ratio
,fa.sue
,fa.intang_ratio
,fa.shares_os
,fa.mkt_cap
,fa.book_price
,fa.ttm_earn_yld
,fa.ttm_earn_yld_max
,fa.log_pb
,fa.pbroe_rsdl_ols
,fa.pbroe_rsq_ols
,fa.pbroe_rsdl_ts
,fa.fnmdl_rsdl_ts
,fa.pbroe_rsdl_ols_rnk
,fa.pbroe_rsdl_ts_rnk
,fa.book_price_rnk
,fa.ttm_earn_yld_rnk
,fa.fnmdl_rsdl_ts_rnk
,fa.pbroe_rsdl_ols_z
,fa.pbroe_rsdl_ts_z
,fa.book_price_z
,fa.ttm_earn_yld_z
,fa.fnmdl_rsdl_ts_z
,fa.agg_valuation
from access_layer.return_attributes ra 
inner join access_layer.fundamental_attributes fa
on ra.symbol = fa.ticker
and ra.date_stamp = fa.date_stamp
order by ra.symbol, ra.date_stamp
"
qry1 <- dbSendQuery(conn = con, statement = sql1) 
stock_data <- dbFetch(qry1)

save(stock_data, file = "C:/Users/brent/Documents/R/romerb/data/stock_data.rda")
