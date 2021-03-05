# function1 start


# function1 end

# function2 start

# function2 end

# function3 start

#' Calculate the summary statistics for specified courses including mean, median and quantiles.
#'
#' @param course_ids A list including all course IDs for which the summary statistics are
#' calculated for.
#'
#' @return A dataframe containing the summary statistics for specified courses
#' @export
#'
#' @examples

generate_course_statistics <- function(course_ids) {
  print("NULL")
}

# function3 end

# function4 start

#' Calculate students' course grades to rank courses in ascending/descending order by a
#' specified method.
#'
#' @param method The method applied to rank the courses, should be "mean", "1st-quantile",
#' "median", or "3rd-quantile". Defaults to "mean"
#' @param descending A logical value to decide if the rank should be in descending or
#' ascending order.
#'
#' @return A dataframe containing the rank for specified courses
#' @export
#'
#' @examples

rank_courses <- function(method="mean", descending=True) {
  print("NULL")
}

# function4 end

# function5 start

#' Ranks students by their grade for a course or the program.
#'
#' Calculate the average grade for a specified number of students and ranks them for a specific
#' course or for the whole program completed thus far.
#'
#' @param course_id A string representing the course ID for which the ranking
#' should be calculated for. Defaults to "all" for all courses completed thus far.
#' @param n An integer value that represents the number of students for which the
#' ranking is required for. Defaults to 3.
#' @param ascending A logical value indicating whether the top or bottom ranking
#' of students is required. Defaults to FALSE.
#'
#' @return A dataframe containing the rank of the students for the course or
#' the program
#' @export
#'
#' @examples
#' rank_students()
#' rank_students(course_id = "511", n = 3, ascending = TRUE)
rank_students <- function(course_id = "all", n = 3, ascending = FALSE) {
  print("NULL")
}

# function5 end

# function6 start

# function6 end
