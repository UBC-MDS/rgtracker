

# register_courses start

#' Read and store the input dataframe as a DataFrame.
#'
#' @param df dataframe: A tidy dataframe:
#' course_id: str, assessment_id: str, ex: lab1, quiz1, weight: float, ex: 0.2,
#' All assessment components for a course must sum up to 1.
#'
#' @return None
#' @export
#'
register_courses <- function(df){
  print('Placeholder')
}

# register_courses end

# record_grades start

#'Record grades for students for a specified course and its assessments.
#'
#' @param df DataFrame: A tidy dataframe
#'course_id: str,student_id: str, assessment_id: str, grade: float
#'
#'
#' @return None
#' @export
#'
record_grades <- function(df){
  print('Placeholder')
}

# record_grades end

# function3 start

# function3 end

# function4 start

# function4 end

# function5 start

#' Calculate the average grade for all students and ranks them.
#'
#' @param course_id string
#' @param n integer
#' @param ascending boolean
#'
#' @return NULL
#' @export
#'
#' @examples
rank_students <- function(course_id="all", n=3, ascending=FALSE){
  print("Placeholder")
}

# function5 end
