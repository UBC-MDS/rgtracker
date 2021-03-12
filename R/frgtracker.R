mds_courses <- c(511,
                 512,
                 513,
                 521,
                 522,
                 523,
                 524,
                 525,
                 531,
                 532,
                 541,
                 542,
                 551,
                 552,
                 553,
                 554,
                 561,
                 562,
                 563,
                 571,
                 572,
                 573,
                 574,
                 575,
                 591)

mds_assess <- c("lab1",
                "lab2",
                "lab3",
                "lab4",
                "milestone1",
                "milestone2",
                "milestone3",
                "milestone4",
                "feedback",
                "quiz1",
                "quiz2")
# register_courses start

#'Read and store the input data frame into a data frame of courses to be
#'registered.
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
#' @examples
#' course_df <- data.frame(
#' course_id = c(rep(511, 6)),
#' assessment_id = c("lab1","lab2","lab3","lab4","quiz1","quiz2"),
#' weight = c(rep(.15, 4), rep(.2, 2)))
#'
#' register_courses(course_df)
register_courses <- function(df){

  if (!all(df$course_id %in% mds_courses)) {
    stop("I only work on MDS courses! You have at least one is not.")
  }

  if (!all(df$assessment_id %in% mds_assess)) {
    stop("I only work on MDS assessments! You have at least one is not.")
  }

  if (!all(df$weight >= 0)) {
    stop("You have at least one assessment weight is negative!")
  }

  w_sum_df <- df %>%
    dplyr::group_by(.data$course_id) %>%
    dplyr::summarise(w_sum = sum(.data$weight))

  if (!all(w_sum_df$w_sum == 1)) {
    stop("The sum of all assessment weights should be 1 for individual
         courses!")
  }



  df <- df %>%
    tidyr::pivot_wider(names_from = .data$assessment_id,
                       values_from = .data$weight,
                values_fill = 0)
  df$course_id <- as.character(df$course_id)
  df <- as.data.frame(df)

  df
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
#' @examples
#' grade_df <- data.frame(course_id=rep(511, 6),
#' student_id=rep("Kiki", 6),
#' assessment_id = c('lab1', 'lab2', 'lab3', 'lab4', 'quiz1', 'quiz2'),
#' grade=c(rep(92.1, 3), rep(80.2, 3)))
#'
#' record_grades(grade_df)
record_grades <- function(df){

  if (!all(df$course_id %in% mds_courses)) {
    stop("I only work on MDS courses! You have at least one is not.")
  }

  if (!all(df$assessment_id %in% mds_assess)) {
    stop("I only work on MDS assessments! You have at least one is not.")
  }

  if(any(df$grade < 0) | any(df$grade > 100)){
    stop("The grade range should be between 0 and 100!")
  }


  df <- df %>%
    tidyr::pivot_wider(names_from = .data$assessment_id,
                       values_from = .data$grade,
                values_fill = 0)
  df$course_id <- as.character(df$course_id)
  df <- as.data.frame(df)

  df
}

# record_grades end

# function3 start

#' Calculate the summary statistics for specified courses including mean, median and quantiles.
#'
#' @param courses A dataframe containing component weights for each course
#' @param grades A dataframe containing grades for students
#' @param course_ids A list including all course IDs for which the summary statistics are
#' calculated for.
#'
#' @return A dataframe containing the summary statistics for specified courses
#' @export
#'
#' @examples
#' grades <- data.frame(
#'   course_id = c(511, 511, 511, 511, 522, 522, 522, 522),
#'   student_id = c("tom", "tiff", "mike", "joel", "tom", "tiff", "mike", "joel"),
#'   lab1 = c(100, 87.6, 84.4, 100, 0, 0, 0, 0),
#'   lab2 = c(100, 100, 79.6, 100, 0, 0, 0, 0),
#'   lab3 = c(79.2, 81.2, 75.2, 99.6, 0, 0, 0, 0),
#'   lab4 = c(83.6, 89.2, 98.8, 71.2, 0, 0, 0, 0),
#'   quiz1 = c(75.6, 100, 84.8, 96.8, 0, 0, 0, 0),
#'   quiz2 = c(75.6, 73.2, 100, 79.2, 0, 0, 0, 0),
#'   milestone1 = c(0, 0, 0, 0, 100, 100, 92, 98.4),
#'   milestone2 = c(0, 0, 0, 0, 97.6, 77.2, 75.6, 85.6),
#'   milestone3 = c(0, 0, 0, 0, 80, 76.8, 97.6, 96.8),
#'   milestone4 = c(0, 0, 0, 0, 100, 100, 84.4, 100),
#'   feedback = c(0, 0, 0, 0, 100, 85.6, 98.8, 82.4)
#' )
#' courses <- data.frame(
#'   course_id = c(511, 522),
#'   lab1 = c(0.15, 0),
#'   lab2 = c(0.15, 0),
#'   lab3 = c(0.15, 0),
#'   lab4 = c(0.15, 0),
#'   quiz1 = c(0.2, 0),
#'   quiz2 = c(0.2, 0),
#'   milestone1 = c(0, 0.1),
#'   milestone2 = c(0, 0.2),
#'   milestone3 = c(0, 0.2),
#'   milestone4 = c(0, 0.3),
#'   feedback = c(0, 0.2)
#' )
#' generate_course_statistics(courses, grades, course_ids = "511")
#' generate_course_statistics(courses, grades, course_ids = c("511", "522"))

generate_course_statistics <- function(courses, grades, course_ids) {
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
                    stats::quantile(temp_df$grade, 0.25),
                    stats::median(temp_df$grade),
                    stats::quantile(temp_df$grade, 0.75))
  }

  statistics
}

# function3 end

# function4 start

#' Calculate students' course grades to rank courses in ascending/descending order by a
#' specified method.
#'
#' @param courses A dataframe containing component weights for each course
#' @param grades A dataframe containing grades for students
#' @param method one of "method", "median", "lst-quantile", "3rd-quantile", defining
#' the method for calculating the course rankings.
#' @param descending A logical value to decide if the rank should be in descending or
#' ascending order. Default to True
#'
#' @return A dataframe containing the rank for specified courses
#' @export
#'
#' @examples
#' grades <- data.frame(
#'   course_id = c(511, 511, 511, 511, 522, 522, 522, 522),
#'   student_id = c("tom", "tiff", "mike", "joel", "tom", "tiff", "mike", "joel"),
#'   lab1 = c(100, 87.6, 84.4, 100, 0, 0, 0, 0),
#'   lab2 = c(100, 100, 79.6, 100, 0, 0, 0, 0),
#'   lab3 = c(79.2, 81.2, 75.2, 99.6, 0, 0, 0, 0),
#'   lab4 = c(83.6, 89.2, 98.8, 71.2, 0, 0, 0, 0),
#'   quiz1 = c(75.6, 100, 84.8, 96.8, 0, 0, 0, 0),
#'   quiz2 = c(75.6, 73.2, 100, 79.2, 0, 0, 0, 0),
#'   milestone1 = c(0, 0, 0, 0, 100, 100, 92, 98.4),
#'   milestone2 = c(0, 0, 0, 0, 97.6, 77.2, 75.6, 85.6),
#'   milestone3 = c(0, 0, 0, 0, 80, 76.8, 97.6, 96.8),
#'   milestone4 = c(0, 0, 0, 0, 100, 100, 84.4, 100),
#'   feedback = c(0, 0, 0, 0, 100, 85.6, 98.8, 82.4)
#' )
#' courses <- data.frame(
#'   course_id = c(511, 522),
#'   lab1 = c(0.15, 0),
#'   lab2 = c(0.15, 0),
#'   lab3 = c(0.15, 0),
#'   lab4 = c(0.15, 0),
#'   quiz1 = c(0.2, 0),
#'   quiz2 = c(0.2, 0),
#'   milestone1 = c(0, 0.1),
#'   milestone2 = c(0, 0.2),
#'   milestone3 = c(0, 0.2),
#'   milestone4 = c(0, 0.3),
#'   feedback = c(0, 0.2)
#' )
#' rank_courses(courses, grades, "mean")
#' rank_courses(courses, grades, "median", descending=FALSE)

rank_courses <- function(
  courses, grades,
  method=c("mean", "1st-quantile", "median", "3rd-quantile"),
  descending=TRUE)
  {
  valid = c("mean", "median", "lst-quantile", "3rd-quantile")
  if (length(subset(valid, valid == method))==0){
    stop("method only accepts 'mean', '1st-quantile', 'median' or '3rd-quantile'")
  }
  if (!is.logical(descending)){
    stop("descending must be logical value")
  }

  course_list = as.character(courses$course_id)

  course_rank <- generate_course_statistics(courses, grades, course_list) %>%
    dplyr::select(c("course_id", as.character(method)))
  colnames(course_rank) <- c("course_id", "grade")
  course_rank <- course_rank[order(course_rank$grade, decreasing = descending),]

  course_rank
}

# function4 end

# start Rank Students

#' Ranks students by their grade for a course or the program.
#'
#' Calculate the average grade for a specified number of students and ranks
#' them for a specific course or for the whole program completed thus far.
#'
#' @param courseid A string representing the course ID for which the ranking
#' should be calculated for. Defaults to "all" for all courses completed thus far.
#' @param n An integer value that represents the number of students for which the
#' ranking is required for. Defaults to 3.
#' @param ascending A logical value indicating whether the top or bottom ranking
#' of students is required. Defaults to FALSE.
#' @param df A dataframe containing the final grades for each student per course.
#'
#' @return A dataframe containing the rank of the students for the course or
#' for all the program completed thus far.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   course_id = c(rep("511", 4)),
#'   student_id = c("tom", "tiff", "mike", "joel"),
#'   grade = c(90, 80, 70, 67)
#' )
#'
#' rank_students(df = df)
#' rank_students(df = df, courseid = "511", n = 3, ascending = TRUE)
rank_students <- function(df, courseid = "all", n = 4, ascending = FALSE) {
  if (!is.numeric(n)) {
    stop("Input value n argument can only by a numeric value")
  }

  if (!is.character(courseid)) {
    stop("Course id can only be a string")
  }

  if (!is.logical(ascending)) {
    stop("Ascending value can only be a logical value")
  }

  if (!is.data.frame(df)) {
    stop("Input for df argument can only be a dataframe")
  }

  if (n <= 0) {
    stop("The input for n can only be a positive integer.")
  }

  if (n %% 1 != 0) {
    stop("The input for n can only be a integer.")
  }

  if (n > length(unique(df$student_id))) {
    stop("The input for n can not greater than the total number of students")
  }

  if (!(courseid %in% c(unique(df$course_id), "all"))) {
    stop("This course is currently not a part of the courses list")
  }

  slicer <- ifelse(ascending, dplyr::slice_min, dplyr::slice_max)

  temp_df <- df

  if (courseid != "all") {
    temp_df <- df %>%
      dplyr::filter(.data$course_id == courseid)
  }

  avg_df <- temp_df %>%
    dplyr::group_by(.data$student_id) %>%
    dplyr::summarise(grade = mean(.data$grade)) %>%
    dplyr::mutate(rank = rank(-.data$grade,
                              ties.method = "random", na.last = NA
    )) %>%
    slicer(.data$grade, n = n)

  as.data.frame(avg_df)
}

# end Rank Students

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

