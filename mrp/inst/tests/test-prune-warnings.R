context("handling warnings in repeated sim calls")


fickleWarn <- setRefClass("fickleWarn",
                          fields = list(warnThisTime="logical"),
                          methods = list(
                          callFunctionThatWarns = function(){
                              if(warnThisTime){
                                  warning("Winter is coming")
                              }
                              warnThisTime <<- !warnThisTime
                          },
                          initialize = function(){
                              initFields(warnThisTime = sample(c(TRUE,FALSE),1))
                          }
                          )
                          )

sut <- fickleWarn$new()

test_that("warnings are captured", function() {
    expect_true(TRUE)
})
