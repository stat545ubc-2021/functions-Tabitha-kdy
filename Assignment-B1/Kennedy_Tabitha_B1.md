Assignment B-1: Making a Function
================
Tabitha Kennedy

## Exercise 1 & 2: Make a Function, and Document It:

For this Assignment, I decided to make a function that takes the average
of one numerical object, ‘a’, per a second numerical object, ‘b’.

I decided to create this function, as it would be useful in calculations
such as determining the average number of apartment units per storey for
a particular ward in Toronto (using the **apt\_buildings** dataset from
the **datateachr** package).

Above my function, I included roxygen2 format tags to document my
function with.

``` r
#' Calculate the Average of 'a' per 'b'
#' 
#' This function calculates the average of one object, 'a', per a second object, 'b'. Object must be numerical.
#' 
#' @params a Is a vector of numbers which we are primarily interested in. I chose to name the parameter 'a', as logically, this object should be inserted into the function prior to b. 
#' @params b Is a vector of numbers for which object 'a' is measure against. I chose to name the parameter 'b', as logically, this object should come after 'a', given that we are dividing the sum of 'a' over the sum of 'b'.
#' @return A vector of numbers resulting from taking the average of 'a' per 'b'.


avg_a_per_b <- function (a, b) {
  if (!is.numeric(a)) {
    stop("Error: object for 'a' must be numeric. You have input an object of class: ", class(a)[1])
  }
  if (!is.numeric(b)) {
    stop("Error: object for 'b' must be numeric. You have input an object of class: ", class(b)[1])
  }
  one <- sum(a, na.rm = TRUE)
  two <- sum(b, na.rm = TRUE)
  return(one/two)
}
```

Within my function, I made sure to incorporate an error code which would
appear if the user input an object class other than numeric for either
object ‘a’ or ‘b’. The error code is also programmed to inform the user
of the class type of the object they input that created the error.

## Exercise 3: Include Examples:

I will provide examples for the use of my function below:

First, let’s load the packages required to work with the dataset
**apt\_buildings**.

``` r
library(datateachr)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Next, let’s filter the data from **apt\_buildings** to contain only
entries for ward 12.

``` r
ward12 <- apt_buildings %>%
  filter(ward == "12") %>%
  drop_na()
```

Now we can apply my function to find out the average number of apartment
units per storey, for ward 12 in Toronto:

``` r
avg_a_per_b(ward12$no_of_units, ward12$no_of_storeys)
```

    ## [1] 11.95767

The function returns 11.95767, indicating that there are approximately
12 apartment units per storey in apartment buildings located in ward 12.

Now, let’s use my function to answer a new question. How many
barrier-free accessible units are there in ward 12 per number of units?

``` r
avg_a_per_b(ward12$no_barrier_free_accessible_units, ward12$no_of_units)
```

    ## [1] 0.07433628

The function returns 0.07433628, indicating that on average, for every 1
apartment unit in ward 12, 0.07 apartment units are barrier-free.

## Exercise 4: Test the Function:

I used the testthat package below to test whether my function was
working as expected.

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

Test 1: Is my function calculating correctly?

``` r
test_that("Test 1", {
expect_equal(11.95767, avg_a_per_b(ward12$no_of_units, ward12$no_of_storeys), tolerance=1e-3) })
```

    ## Test passed

Test 2: Will my function return an error if I enter a character object?

``` r
test_that("Test 2", {
expect_error(avg_a_per_b(ward12$air_conditioning, ward12$no_of_storeys)) })
```

    ## Test passed

Test 3: Are the objects we selected for ‘a’ and ‘b’ numeric?

``` r
test_that("Test 3", {

        test3 <- avg_a_per_b(ward12$no_barrier_free_accessible_units, ward12$no_of_storeys)

        expect_that( test3, is_a("numeric") )})
```

    ## Test passed

Thank you for reviewing my submission for Assignment B-1. :)
