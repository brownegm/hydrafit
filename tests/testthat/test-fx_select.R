test_that("best fitting model selected", {

# create test data
  lin <- data.frame(species = c("A", "B", "C"),
                    data.type = "lin",
                    AICcorr = c(188, 400, 1231))
  log <- data.frame(species = c("A", "B", "C"),
                    data.type = "log",
                    AICcorr = c(101, 2, 1))
  sig <- data.frame(species = c("A", "B", "C"),
                    data.type = "sig",
                    AICcorr = c(231, 221, 242))
  exp1 <- data.frame(species = c("A", "B", "C"),
                     data.type = "exp1",
                     AICcorr = c(4, 142, 2))
  exp2 <- data.frame(species = c("A", "B", "C"),
                     data.type = "exp2",
                     AICcorr = c(333, 53, 3124))


  #establish objects
  output <- lin|> apply( MARGIN = c(1,2), FUN = \(x) x<-NA)
   ## create list of input dataframes
  dfs <-
    list(lin,
         log,
         sig,
         exp1,
         exp2)

  for(i in seq_along(lin$species)){
    ## determine the lowest AICc for each species across each model
    minAIC=min(c(lin[i,"AICcorr"],
                 log[i,"AICcorr"],
                 sig[i,"AICcorr"],
                 exp1[i,"AICcorr"],
                 exp2[i,"AICcorr"]))

    ## select the row from the appropriate df to add to output
    lowestAICc<-lapply(dfs, function(df)

      if(df[[i,"AICcorr"]]==minAIC){

        return(df[i,])

      }else{

      }
    )
    # clean output
    ## lapply creates NULLs in the list; remove here
    lowestAICc[sapply(lowestAICc, is.null)] <- NULL
    ## save row for given species from appropriate df in the output
    output[i,]<-unlist(lowestAICc)#
  }

   # Function to transform a dataframe into a list of lists
  transform_to_list <- function(df) {
    pl <- lapply(1:nrow(df), function(i) {
      list(species = df$species[i], AICcorr = df$AICcorr[i])
    })
    return(pl)
  }

  # Apply the transformation to each dataframe in the list
  all_lists <- lapply(dfs, transform_to_list)

function_output <- fx_select( all_lists[[1]],
                              all_lists[[2]],
                              all_lists[[3]],
                              all_lists[[4]],
                              all_lists[[5]], silent=T)|>
  lapply(function(x) unlist(x))

# expected_message <- "The following species have alternative models within 2 AICc units:
#   Species: C, Alternatives: NULL"

expect_match(function_output[[1]]["AICcorr"], output[1, "AICcorr"])
expect_match(function_output[[2]]["AICcorr"], output[2, "AICcorr"])
expect_match(function_output[[3]]["AICcorr"], output[3, "AICcorr"])
#expect_message(function_output, expected_message)
})

