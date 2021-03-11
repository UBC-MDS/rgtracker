# register_courses start

#'Read and store the input data frame into a data frame of courses to be registered.
#'
#'For each of the course, the weights of all assessments should sum up to 1.
#'The weights of individual assessment should be between 0 and 1.
#'
#' @param df A tidy data frame containing course information,
#' with course id, all of the available assessments and corresponding weights.
#'
#'
#' @return None
#' @export
#'
#' @example
#'register_courses(course_df)
register_courses <- function(df){

}

# register_courses end

# record_grades start

#'Record grades for students of a specified course and its assessments.
#'
#'The grades are recorded to be out of 100.
#'
#' @param df A tidy data frame as a student gradebook,
#' with course id, student id, corresponding assessment id and grades.
#'
#'
#' @return None
#' @export
#'
#' @example
#'record_grades(grade_df)
record_grades <- function(df){

}

# record_grades end

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
#' generate_course_statistics(course_ids = "511")
#' generate_course_statistics(course_ids = c("511", "522"))

generate_course_statistics <- function(course_ids) {
  if (!is.character(course_ids)){
    stop("course_ids must be a vector including characters")
  }
  if (length(subset(courses$course_id, courses$course_id == course_ids))==0) {
    stop("The course is currently not a part of the courses list")
  }

  final_grade <- calculate_final_grade(courses, grades, course_ids)
  statistics <- data.frame(matrix(ncol=5, nrow=0))
  colnames(statistics) <- c("course_id", "mean", "1st-quantile", "median", "3rd-quantile")
  for (i in 1:length(course_ids)){
    temp_df <- final_grade %>%
      dplyr::filter(.data$course_id == course_ids[i])
    statistics[i,] <- c(course_ids[i],
                    mean(temp_df$grade),
                    quantile(temp_df$grade, 0.25),
                    median(temp_df$grade),
                    quantile(temp_df$grade, 0.75))
  }
  
  statistics
}

# function3 end

# function4 start

#' Calculate students' course grades to rank courses in ascending/descending order by a
#' specified method.
#'
#' @param method one of "method", "median", "lst-quantile", "3rd-quantile", defining
#' the method for calculating the course rankings.
#' @param descending A logical value to decide if the rank should be in descending or
#' ascending order. Default to True
#'
#' @return A dataframe containing the rank for specified courses
#' @export
#'
#' @examples
#' rank_courses("mean")
#' rank_courses("median", descending=FALSE)

rank_courses <- function(method=c("course_id", "mean", "1st-quantile", "median", "3rd-quantile"), descending=TRUE) {
  valid = c("mean", "median", "lst-quantile", "3rd-quantile")
  if (length(subset(valid, valid == method))==0){
    stop("method only accepts 'mean', '1st-quantile', 'median' or '3rd-quantile'")
  }
  if (!is.logical(descending)){
    stop("descending must be logical value")
  }

  course_list = as.character(courses$course_id)

  course_rank <- generate_course_statistics(course_list) %>%
    dplyr::select(c("course_id", as.character(method)))
  colnames(course_rank) <- c("course_id", "grade")
  course_rank <- course_rank[order(course_rank$grade, decreasing = descending),]

  course_rank
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

# start Suggest Grade Adjustment

#' Adjust students' grades upwards in a course based on predefined benchmarks so
#' that average grades for those components meet or exceed these benchmarks.
#'
#' Students' grades in labs and quizzes will first be adjusted upwards to meet
#' or exceed benchmarks for labs and quizzes. However, if the average grade for
#' the course is still below the course benchmark, we will continue to adjust
#' upwards each lab / quiz until this benchmark is reached.
#'
#' @param courses A dataframe containing component weights for each course
#' @param grades A dataframe containing grades for students
#' @param id A string representing the course ID for which grades should
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
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' grades <- data.frame(
#' course_id = c("511"),
#' student_id = c("tom"),
#' lab1 = c(100),
#' lab2 = c(80)
#' )
#' courses <- data.frame(
#' course_id = c("511"),
#' lab1 = c(0.45),
#' lab2 = c(0.55)
#' )
#' suggest_grade_adjustment(courses, grades, id = "511")
suggest_grade_adjustment <- function(courses, grades, id,
                                     benchmark_course = 90, benchmark_lab = 85,
                                     benchmark_quiz = 85)
  {
  if(!is.data.frame(courses)){
    stop("courses must be a dataframe")
  }

  if(!is.data.frame(grades)){
    stop("grades must be a dataframe")
  }

  if(!is.character(id)){
    stop("course_id must be a vector")
  }

  if (!is.numeric(benchmark_course) || benchmark_course < 0 ||
      benchmark_course > 100) {
    stop("benchmark_course must be a number between 0 and 100 (inclusive")
  }

  if (!is.numeric(benchmark_lab) || benchmark_lab < 0 || benchmark_lab > 100) {
    stop("benchmark_lab must be a number between 0 and 100 (inclusive")
  }

  if (!is.numeric(benchmark_quiz) || benchmark_quiz < 0 ||
      benchmark_quiz > 100) {
    stop("benchmark_quiz must be a number between 0 and 100 (inclusive")
  }

  # filter course component and grades for this course only
  courses <- courses %>%
    dplyr::filter(.data$course_id == id)

  grades <- grades %>%
    dplyr::filter(.data$course_id == id)

  components <- courses %>%
    tidyr::pivot_longer(
      !.data$course_id,
      names_to = "component",
      "values_to" = "weight") %>%
    dplyr::filter(.data$weight > 0) %>%
    dplyr::pull(.data$component)

  # adjust quizzes and labs
  for (i in 1:length(components)) {
    component <- components[i]

    benchmark <- benchmark_lab
    if (startsWith(component, "quiz")) {
      benchmark <- benchmark_quiz
    }

    component_grades <- grades %>%
      dplyr::pull(get(component))

    avg_component <- component_grades %>%
      mean()

    while (avg_component < benchmark) {
      component_grades <- component_grades %>%
        sapply(function(x) min(x + 1L, 100L))

      avg_component <- component_grades %>%
        mean()
    }
    grades[component] <- component_grades
  }

  # adjust course
  for (i in 1:length(components)) {
    component <- components[i]

    avg_course <- calculate_final_grade(courses, grades, id) %>%
      dplyr::pull(.data$grade) %>%
      mean()

    if (avg_course >= benchmark_course) {
      break
    }

    component_grades <- grades %>%
      dplyr::pull(get(component))

    avg_component <- component_grades %>%
      mean()

    component_weight <- courses %>%
      dplyr::pull(get(component))

    diff <- (100 - avg_component) * component_weight

    if (avg_course + diff < benchmark_course) {
      # let everyone have 100 marks
      grades[component] <- rep(100, n=length(component_grades))
    } else {
      # increase gradually until it meets the benchmark
      while (TRUE) {
        component_grades <- component_grades %>%
          sapply(function(x) min(x + 1L, 100L))

        diff <- (mean(component_grades) - avg_component) * component_weight

        if (avg_course + diff >= benchmark_course) {
          grades[component] <- component_grades
          break
        }
      }
    }
  }

  grades
}

# end Suggest Grade Adjustment

# start Calculate Final Grade

#' Calculate final grades for all students in a course
#'
#' For each student, the mark for each component will be multiplied with its
#' associated weight, and summed up altogether.
#'
#'
#' @param courses A dataframe containing component weights for each course
#' @param grades A dataframe containing grades for students
#' @param course_ids A vector of strings representing the course IDs for which
#' final grades should be calculated.
#'
#' @return A dataframe containing final grades for all students in a course.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' grades <- data.frame(
#' course_id = c("511"),
#' student_id = c("tom"),
#' lab1 = c(100),
#' lab2 = c(80)
#' )
#' courses <- data.frame(
#' course_id = c("511"),
#' lab1 = c(0.45),
#' lab2 = c(0.55)
#' )
#' calculate_final_grade(courses, grades, course_ids = c("511"))
calculate_final_grade <- function(courses, grades, course_ids)
{
  if(!is.data.frame(courses)){
    stop("courses must be a dataframe")
  }

  if(!is.data.frame(grades)){
    stop("grades must be a dataframe")
  }

  if(!is.character(course_ids)){
    stop("course_ids must be a vector")
  }


  LARGE <- 25 * 150
  courses_col <- character(LARGE)
  students_col <- character(LARGE)
  grades_col <- numeric(LARGE)
  index <- 1

  vector <- character()

  for (i in 1:length(course_ids)) {
    id <- course_ids[i]

    weights <- courses %>%
      dplyr::filter(.data$course_id == id) %>%
      dplyr::select(-.data$course_id)

    course_grades <- grades %>%
      dplyr::filter(.data$course_id == id) %>%
      dplyr::select(-.data$course_id)

    student_ids <- course_grades %>%
      dplyr::pull(.data$student_id)

    course_grades <- course_grades %>%
      dplyr::select(-.data$student_id)

    if (nrow(course_grades) > 1) {
      temp <- data.frame(mapply(`*`,course_grades, weights[1,])) %>% rowSums()
    } else {
      temp <- data.frame(mapply(`*`,course_grades, weights[1,])) %>% sum()
    }

    num_elements <- course_grades %>%
      nrow()

    end_index <- index + num_elements - 1

    courses_col[index:end_index] <- rep(id, n = num_elements)
    students_col[index:end_index] <- student_ids
    grades_col[index:end_index] <- temp

    index <- end_index + 1
  }

  final_grades <- data.frame(
    course_id = courses_col[1:index-1],
    student_id = students_col[1:index-1],
    grade = grades_col[1:index-1]
    )

  final_grades
}

# end Calculate Final Grade
