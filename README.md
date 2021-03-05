
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgtracker

<!-- badges: start -->
<!-- badges: end -->

The goal of rgtracker is to allow UBC MDS lecturers to record, analyze
and adjust grades for students in a particular program. Users can record
grades from each course, generate a summary report to determine which
class is more challenging than the rest, or identify students who may
need help. Finally, the package can suggest ways to adjust the grades
for students to ensure the average grades match predefined benchmarks.
It contains six functions: register\_courses, record\_grades,
generate\_course\_statistics, rank\_courses, rank\_students and
suggest\_grade\_adjustment.

The main components of this package are:

-   Register courses
-   Read/store the courses information as a dataframe
-   Record grades for students
    -   Read/store the students’ grades for each assessment as a
        dataframe
-   Generate course statistics
    -   Provide grade statistics on the courses, including mean, 1st
        quantile, median and 3rd quantile
-   Rank courses
    -   Provide the rankings of courses based on courses’ average grades
-   Rank students
    -   Provide the rankings of students based on their average grades
        for the selected course (or the whole program).
-   Suggest grade adjustment
    -   Suggest grade adjustments for any course based on predefined
        benchmarks

# Relevance in R ecosystem

## Installation

You can install the released version of rgtracker from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rgtracker")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UBC-MDS/rgtracker")
```

## Code of Conduct

Please note that the rgtracker project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
