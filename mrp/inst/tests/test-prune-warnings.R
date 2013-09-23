context("handling warnings in repeated sim calls")


fickleWarn <- setRefClass("fickleWarn",
                          fields = list(warnThisTime="logical"),
                          methods = list(
                          doSomethingThatMightWarn = function(){
                              if(warnThisTime){
                                  warning("Winter is coming")
                              }
                              warnThisTime <<- !warnThisTime
                              rnorm(1)
                          },
                          initialize = function(){
                              initFields(warnThisTime = TRUE)
                          }
                          )
                          )

does_not_throw_error <- function (regexp = NULL) {
    function(expr) {
        res <- try(force(expr), TRUE)
        error <- inherits(res, "try-error")
        if (error) {
            return(expectation(FALSE, "code did generate an error"))
        }
        if (!is.null(regexp)) {
            matches(regexp)(res)
        }
        else {
            expectation(TRUE, "")
        }
    }
}

does_not_give_warning <- function (regexp = NULL) {
    function(expr) {
        res <- evaluate::evaluate(substitute(expr), parent.frame())
        warnings <- sapply(Filter(evaluate::is.warning, res), "[[", "message")
        if (!is.null(regexp)) {
            matches(regexp, all = FALSE)(warnings)
        }
        else {
            expectation(length(warnings) == 0, "warnings given")
        }
    }
}


test_that("warning fixture does, then does not, warn, and does not throw", function(){
    sut <- fickleWarn$new()
    expect_that(sut$doSomethingThatMightWarn(), gives_warning())
    expect_that(sut$doSomethingThatMightWarn(), does_not_give_warning())
    expect_that(sut$doSomethingThatMightWarn(), does_not_throw_error())
    expect_that(sut$doSomethingThatMightWarn(), does_not_throw_error())
})

test_that("warnings are captured", function() {
    sut <- fickleWarn$new()
    nsims <- 3
    listOfResults <- lapply(1:nsims, , sut$doSomethingThatMightWarn())
    expect_is(attr(listOfResults,"warnings"), "list")
})
