
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgtracker

<!-- badges: start -->

[![R-CMD-check](https://github.com/UBC-MDS/rgtracker/workflows/R-CMD-check/badge.svg)](https://github.com/UBC-MDS/rgtracker/actions)
<!-- badges: end -->

The goal of rgtracker is to allow UBC MDS lecturers to record, analyze
and adjust grades for students in a particular program. Users can record
grades from each course, generate a summary report to determine which
class is more challenging than the rest, or identify students who may
need help. Finally, the package can suggest ways to adjust the grades
for students to ensure the average grades match predefined benchmarks.
It contains six functions: `register_courses`, `record_grades`,
`generate_course_statistics`, `rank_courses`,`rank_students` and
`suggest_grade_adjustment`.

The main components of this package are:

-   Register courses
-   Read and store the courses information as a dataframe
-   Record grades for students
    -   Read and store the students’ grades for each assessment as a
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

## Relevance in R ecosystem

Through a search of the [CRAN library of available R packages to
date](https://cran.r-project.org/web/packages/available_packages_by_date.html),
we have found that there is no exact package that performs the
functionality of our functions as one package together. There are
existing packages that grade students papers,
[gradeR](https://cran.r-project.org/web/packages/gradeR/vignettes/gradeR.html)
and returns the results of the grading for students. There is also
another package called
[ProfessR](https://cran.r-project.org/web/packages/ProfessR/index.html)
that scales grades, plots the grades and calculates summary statistics
(maximum, mean and minimum).

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
