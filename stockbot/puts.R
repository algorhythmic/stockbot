require(quantmod)
require(ggplot2)
require(data.table)
require(plotly)
require(scales)

symbol = 'ISRG'
minimum_daily_return = 0.00

# Get puts into a single data frame with additional columns
current_price = getQuote(symbol)$Last
options = getOptionChain(symbol, NULL)
all_puts = lapply(options, function (df) df$puts)
all_puts = Filter(function(x) dim(x)[1] > 0, all_puts)
puts = lapply(names(all_puts), function(date) {
  df = all_puts[[date]]
  breakeven = df$Strike - df$Bid
  cbind(df,
        BidPremium = df$Bid / breakeven,
        BidBreakeven = breakeven,
        BidDailyPremium = df$Bid / breakeven / rep(as.numeric(as.Date(date, "%b.%d.%Y") - Sys.Date()), length(df$Strike)),
        ExpirationDate = date,
        DTE = rep(as.numeric(as.Date(date, "%b.%d.%Y") - Sys.Date()), length(df$Strike))
        )})
names(puts) = names(all_puts)
puts = rbindlist(puts)

# Filter out the ones we want
#puts = puts[puts$Strike >= 650,]
puts = puts[puts$Strike <= current_price,]
puts = puts[puts$BidDailyPremium >= minimum_daily_return,]

graph = ggplot(puts, aes(
  x=BidBreakeven,
  group=ExpirationDate,
  color=ExpirationDate,
  text=paste(
    ExpirationDate, "\n",
    "Strike: ", Strike, "\n",
    "Breakeven: ", BidBreakeven, "\n",
    "Premium: ", percent(BidPremium, accuracy=0.01), "\n",
    "Daily Premium: ", percent(BidDailyPremium, accuracy=0.01), "\n",
    "Days Till Expiration: ", DTE, "\n",
    "Bid-Ask: ", Bid, "-", Ask, "\n",
    "OI: ", OI, "\n",
    "Volume: ", Vol
  ))) +
  geom_line(aes(y=BidDailyPremium)) +
  geom_point(aes(y=BidDailyPremium, size=OI)) +
  scale_x_continuous(breaks=seq(floor(min(puts$BidBreakeven)), floor(current_price), 1)) +
  scale_y_continuous(labels=percent) +
  labs(title=paste(symbol, "Daily Premium Percentage"), x="Breakeven Price", y="Daily Premium Percentage") +
  theme(legend.title=element_blank())

ggplotly(graph, tooltip="text")