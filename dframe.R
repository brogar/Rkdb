dyn.load("test.so")

DF <- data.frame( stringsAsFactors = FALSE,
                 xx = rnorm(10),	
                 yy = rpois(10, 20),
                 cc = paste( sample(LETTERS, 10), sample(letters,10)),
                 ll = sample(c(TRUE,FALSE), 10, replace = TRUE),
                 dd = as.Date( sample(1:10000L, 10), origin = "1970-01-01"),
                 zz = factor( sample( sample(LETTERS, 4), 10 , replace = TRUE) ),
                 oo = interaction(
                     ordered( sample( sample(letters, 5), 10, replace = TRUE) ),
                     ordered( sample( sample(letters, 5), 10, replace = TRUE) )
                     ),
                 uu = as.POSIXct( runif(10,0, 20), origin = "2000-01-01"),
                 dc = complex(10)
                 )


options(digits.secs = 6)

DF$xx[c(2,5)] <- NA
DF$xx[9] <- -Inf
DF$xx[3] <- Inf
DF$yy[c(1,6)] <- NA
DF$dd[c(3,5,9)] <- NA
DF$uu[c(1,3,8,9)] <-NA
DF$oo[c(3,7)] <- NA
DF$zz[c(1,2,9,10)] <- NA
DF$cc[c(8,10)] <- NA_character_

storage.mode(DF$xx) <- "double"
storage.mode(DF$yy) <- "integer"
storage.mode(DF$dd) <- "double"
storage.mode(DF$uu) <- "double"
storage.mode(DF$ll) <- "logical"
storage.mode(DF$cc) <- "character"

#
conn <- list(
    host = "localhost",
    port = 5001L)

CONN <- .Call("kx_r_open_connection", conn)

res <- .Call("interface", DF, as.character("t1"), as.integer(CONN) )

.Call("kx_r_close_connection", CONN)


dyn.unload("test.so")

