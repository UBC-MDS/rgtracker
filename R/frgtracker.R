# function1 start


# function1 end

# function2 start

# function2 end

# function3 start

# function3 end

# function4 start

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

# start Suggest Grade Adjustment

#' Adjust students' grades upwards in a course based on predefined benchmarks so
#' that average grades for those components meet or exceed these benchmarks.
#'
#' Students' grades in labs and quizzes will first be adjusted upwards to meet
#' or exceed benchmarks for labs and quizzes. However, if the average grade for
#' the course is still below the course benchmark, we will continue to adjust
#' upwards each lab / quiz until this benchmark is reached.
#'
#'
#' @param course_id A string representing the course ID for which grades should
#' be adjusted.
#' @param benchmark_course A double value representing the benchmark of which
#' the average grade for the whole course must meet or exceed. Defaults to 90.
#' @param benchmark_lab A double value representing the benchmark of which the
#' average grade for each lab must meet or exceed. Defaults to 85.
#' @param benchmark_quiz A double value representing the benchmark of which the
#' average grade for each quiz must meet or exceed. Defaults to 85.
#'
#' @return A dataframe containing adjusted grades for all students in a course.
#' @export
#'
#' @examples
#' suggest_grade_adjustment(course_id = "511")
#' suggest_grade_adjustment(course_id = "511", benchmark_course = 98)
suggest_grade_adjustment <- function(course_id, benchmark_course = 90,
                                     benchmark_lab = 85, benchmark_quiz = 85)
  {

}

# end Suggest Grade Adjustment

# start Calculate Final Grade

#' Calculate final grades for all students in a course
#'
#' For each student, the mark for each component will be multiplied with its
#' associated weight, and summed up altogether.
#'
#'
#' @param course_id A string representing the course ID for which final grades
#' should be calculated.
#'
#' @return A dataframe containing final grades for all students in a course.
#' @export
#'
#' @examples
#' calculate_final_grade(course_id = "511")
calculate_final_grade <- function(course_id)
{

}

# end Calculate Final Grade
