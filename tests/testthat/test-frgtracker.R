#register_courses function start
#remove when actual tests are added


generate_input_courses_df <- function() {
  courses <- data.frame(
    course_id = c(rep(511, 6), rep(522, 5)),
    assessment_id = c(
    "lab1",
    "lab2",
    "lab3",
    "lab4",
    "quiz1",
    "quiz2",
    "milestone1",
    "milestone2",
    "milestone3",
    "milestone4",
    "feedback"
  ),
  weight = c(
    rep(.15, 4), rep(.2, 2), .1, .2, .2, .3, .2
  ))

  courses
}


generate_input_grades_df <- function(course_id=c(rep(511, 6)),
                                     student_id = c(rep('james', 6)),
                                     assessment_id=c("lab1", "lab2", "lab3",
                                                     "lab4", "quiz1", "quiz2"),
                                     grade = c(rep(88, 4), rep(93.2, 2))) {
  grades <- data.frame(
    course_id = course_id,
    student_id = student_id,
    assessment_id = assessment_id,
    grade = grade
  )

  grades
}


test_that("At least one of you course id is not MDS-courses as far as I know!",{

  c_df <- generate_input_courses_df()
  c_df$course_id <- c(rep(571, 5), 577)

  g_df <- generate_input_grades_df()
  g_df$course_id[6] <- 533

  expect_error(register_courses(c_df))
  expect_error(record_grades(g_df))
})


test_that("At least one of your assessment id is not MDS-courses as far as I
          know!",{
  c_df <- generate_input_courses_df()
  c_df$assessment_id <- c("lab5")

  g_df <- generate_input_grades_df()
  g_df$assessment_id[2] <- "quiz0"

  expect_error(register_courses(c_df))
  expect_error(record_grades(g_df))
})

test_that("The all weights for individual MDS-courses should add up to 1!",{
  df <- generate_input_courses_df()
  df$weight <- c(.16, rep(.15, 3), rep(.2, 2))

  expect_error(register_courses(df))
})



test_that("I saw you have at least one negative course weight, which should be
          between 0 and 1 :)", {
            df <- generate_input_courses_df()
            df$weight <- c(-.15, .45, rep(.15, 2), rep(.2, 2))

            expect_error(register_courses(df))

          })


test_that("The output of the courses data frame is incorrect!", {

  in_df <- generate_input_courses_df()
  out_df <- generate_courses_calculate_final_grade()
  out_df$course_id <- as.character(out_df$course_id)

  expect_equal(register_courses(in_df),
               out_df)
})

#function1 end

#function2 start

test_that("Oops I only deal with grade records out of 100!", {


})

test_that("The output of the grades data frame is incorrect!", {

  in_df <- generate_input_grades_df()
  out_df <- generate_grades_calculate_final_grade()
  out_df$course_id <- "511"
  out_df$student_id <- "james"
  out_df$lab1 <- 88
  out_df$lab2 <- 88
  out_df$lab3 <- 88
  out_df$lab4 <- 88
  out_df$quiz1 <- 93.2
  out_df$quiz2 <- 93.2

  expect_equal(record_grades(in_df), out_df)
})

#function2 end

#function3 start

#function3 end

#function4 start

#function4 end

#function5 start

#function5 end

#function6 start

#function6 end

#tests for calculate_final_grade start
generate_courses_calculate_final_grade <- function() {
  courses <- data.frame(
    course_id = c(511, 522),
    lab1 = c(0.15, 0),
    lab2 = c(0.15, 0),
    lab3 = c(0.15, 0),
    lab4 = c(0.15, 0),
    quiz1 = c(0.2, 0),
    quiz2 = c(0.2, 0),
    milestone1 = c(0, 0.1),
    milestone2 = c(0, 0.2),
    milestone3 = c(0, 0.2),
    milestone4 = c(0, 0.3),
    feedback = c(0, 0.2)
  )

  courses
}

generate_grades_calculate_final_grade <- function() {
  grades <- data.frame(
    course_id = c(511, 511, 511, 511, 522, 522, 522, 522),
    student_id = c("tom", "tiff", "mike", "joel", "tom", "tiff", "mike", "joel"),
    lab1 = c(100, 87.6, 84.4, 100, 0, 0, 0, 0),
    lab2 = c(100, 100, 79.6, 100, 0, 0, 0, 0),
    lab3 = c(79.2, 81.2, 75.2, 99.6, 0, 0, 0, 0),
    lab4 = c(83.6, 89.2, 98.8, 71.2, 0, 0, 0, 0),
    quiz1 = c(75.6, 100, 84.8, 96.8, 0, 0, 0, 0),
    quiz2 = c(75.6, 73.2, 100, 79.2, 0, 0, 0, 0),
    milestone1 = c(0, 0, 0, 0, 100, 100, 92, 98.4),
    milestone2 = c(0, 0, 0, 0, 97.6, 77.2, 75.6, 85.6),
    milestone3 = c(0, 0, 0, 0, 80, 76.8, 97.6, 96.8),
    milestone4 = c(0, 0, 0, 0, 100, 100, 84.4, 100),
    feedback = c(0, 0, 0, 0, 100, 85.6, 98.8, 82.4)
  )

  grades
}

generate_final_grade_calculate_final_grade <- function(course_id, grade) {
  final_grades <- data.frame(
    course_id = rep(course_id, n=4),
    student_id = c("tom", "tiff", "mike", "joel"),
    grade = grade
  )

  final_grades
}

test_that("The parameters for calculate_final_grade are not valid", {
  courses <- generate_courses_calculate_final_grade()
  grades <- generate_grades_calculate_final_grade()
  course_id <- c("511")

  expect_error(calculate_final_grade(5, grades, course_id))
  expect_error(calculate_final_grade(courses, c(1, 3), course_id))
  expect_error(calculate_final_grade(courses, grades, 5))
})

test_that("The output of calculate_final_grade is not valid", {
  courses <- generate_courses_calculate_final_grade()
  grades <- generate_grades_calculate_final_grade()
  course_ids <- c("511")

  output <- calculate_final_grade(courses, grades, course_ids)
  expect_true(is.data.frame(output))
})

test_that("The output of calculate_final_grade is incorrect", {
  courses <- generate_courses_calculate_final_grade()
  grades <- generate_grades_calculate_final_grade()
  course_ids <- c("511")

  output <- calculate_final_grade(courses, grades, course_ids)
  expected_output <- generate_final_grade_calculate_final_grade(
    "511",
    c(84.66, 88.34, 87.66, 90.82)
    )
  expect_equal(output, expected_output)

  course_ids <- c("522")

  output <- calculate_final_grade(courses, grades, course_ids)
  expected_output <- generate_final_grade_calculate_final_grade(
    "522",
    c(95.52, 87.92, 88.92, 92.80)
  )
  expect_equal(output, expected_output)
})
#tests for calculate_final_grade end

