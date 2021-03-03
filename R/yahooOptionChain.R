# library(trader)
# symbol <- "SPY"
# option_type <- "puts"
# #=========================================================
# spot <- trader::getQuoteAV(symbol)$price
# optionChain <- quantmod::getOptionChain(symbol, Exp = NULL)
# #=========================================================
# # Error check, sometimes Yahoo has no data intermittenly
# if(is.null(names(optionChain)))
# {
#   stop(paste("No expiries returned for", symbol))
# }
# # Convert date-format from Month.dd.yyyy to yyyy-mm-dd
# expiries <- gsub("\\.", "-", names(optionChain))
# expiries <- as.Date(expiries, format = "%b-%d-%Y")
# maturity <- trader::date_yte(expiries) # convert to YTE
# # Loop through all expiries and merge by strike, for given payoff-type
# optionSurface <- lapply(optionChain, function(x) data.frame(Strike = x[[option_type]]$Strike, Last = x$puts$Last))
# # Suppress duplicate column warning
# suppressWarnings(optionSurface <- Reduce(function(...) merge(..., all = TRUE, by="Strike"), optionSurface))
# optionSurface <- optionSurface[complete.cases(optionSurface), ]
# colnames(optionSurface) <- c("Strike", as.character(expiries))
# # Check output
# print(optionSurface)
# plot3D::persp3D(z = as.matrix(optionSurface)[,-1], phi = 10, theta = 60)

