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

# tests for register_courses function start

test_that("At least one of you course id is not MDS-courses as far as I know!",{

  c_df <- generate_input_courses_df()
  c_df$course_id[7] <- 577

  g_df <- generate_input_grades_df()
  g_df$course_id[6] <- 533

  expect_error(register_courses(c_df))
  expect_error(record_grades(g_df))
})


test_that("At least one of your assessment id is not MDS-courses as far as I
          know!",{
  c_df <- generate_input_courses_df()
  c_df$assessment_id[8] <- "lab5"

  g_df <- generate_input_grades_df()
  g_df$assessment_id[2] <- "quiz0"

  expect_error(register_courses(c_df))
  expect_error(record_grades(g_df))
})

test_that("The all weights for individual MDS-courses should add up to 1!",{
  df <- generate_input_courses_df()
  df$weight[10] <- .29

  df1 <- generate_input_courses_df()
  df1$weight[11] <- .21

  expect_error(register_courses(df))
  expect_error(register_courses(df1))
})



test_that("I saw you have at least one negative course weight, which should be
          between 0 and 1 :)", {
            df <- generate_input_courses_df()
            df$weight[c(1, 2)] <- c(-.15, .45)

            expect_error(register_courses(df))
          })


test_that("The output of the courses data frame is incorrect!", {

  in_df <- generate_input_courses_df()
  out_df <- generate_courses_calculate_final_grade()
  out_df$course_id <- as.character(out_df$course_id)

  expect_equal(register_courses(in_df),
               out_df)
})

#tests for register_courses function end

#tests for record_courses function start

test_that("Oops I only deal with grade records out of 100!", {
  df <- generate_input_grades_df()
  df$grade[5] <- 100.1

  df1 <- generate_input_grades_df()
  df1$grade[2] <- -89.8

  expect_error(record_grades(df))
  expect_error(record_grades(df1))

})


test_that("The output of the grades data frame is incorrect!", {

  in_df <- generate_input_grades_df()

  out_df <- data.frame(course_id = "511",
                       student_id = "james",
                       lab1 = 88,
                       lab2 = 88,
                       lab3 = 88,
                       lab4 = 88,
                       quiz1 = 93.2,
                       quiz2 = 93.2)

  expect_equal(record_grades(in_df), out_df)
})

#tests for record_grades function end

#tests for generate_course_statistics start
test_that("The input of generate_course_statistics is not valid", {
  courses <- generate_courses_calculate_final_grade()
  grades <- generate_grades_calculate_final_grade()

  course_ids <- c("530", "540")
  expect_error(generate_course_statistics(courses, grades, course_ids))

  course_ids <- c(511)
  expect_error(generate_course_statistics(courses, grades, course_ids))
})

test_that("The output of generate_course_statistics is incorrect", {
  courses <- generate_courses_calculate_final_grade()
  grades <- generate_grades_calculate_final_grade()
  course_ids <- c("511", "522")
  grade_511 <- c(84.66, 88.34, 87.66, 90.82)
  grade_522 <- c(95.52, 87.92, 88.92, 92.80)

  stats <- generate_course_statistics(courses, grades, course_ids)
  output <- as.numeric(stats[1,2])
  expected_output <- mean(grade_511)
  expect_equal(output, expected_output)

  output<- as.numeric(stats[2,4])
  expected_output <- stats::median(grade_522)
  expect_equal(output, expected_output)

  output <- as.numeric(stats[1,3])
  expected_output <- as.numeric(stats::quantile(grade_511, 0.25))
  expect_equal(output, expected_output)

  output <- as.numeric(stats[2,5])
  expected_output <- as.numeric(stats::quantile(grade_522, 0.75))
  expect_equal(output, expected_output)
})


test_that("The output of generate_course_statistics is not valid", {
  courses <- generate_courses_calculate_final_grade()
  grades <- generate_grades_calculate_final_grade()
  course_ids <- c("511", "522")

  output <- generate_course_statistics(courses, grades, course_ids)

  expect_true(is.data.frame(output))
  expect_equal(colnames(output), c("course_id", "mean", "1st-quantile", "median", "3rd-quantile"))
  expect_equal(nrow(output), 2)
})

#tests for generate_course_statistics end

#tests for rank_courses start

test_that("The input of rank_courses is not valid", {
  courses <- generate_courses_calculate_final_grade()
  grades <- generate_grades_calculate_final_grade()

  expect_error(rank_courses(courses, grades, method="avg", descending=TRUE))
  expect_error(rank_courses(courses, grades, method="mean", descending="TRUE"))
})

test_that("The output of rank_courses is incorrect", {
  courses <- generate_courses_calculate_final_grade()
  grades <- generate_grades_calculate_final_grade()

  grade_511 <- c(84.66, 88.34, 87.66, 90.82)
  grade_522 <- c(95.52, 87.92, 88.92, 92.80)

  output <- as.numeric(rank_courses(courses, grades, method="mean")[2,2])
  expected_output <- mean(grade_511)
  expect_equal(output, expected_output)

  output<- as.numeric(rank_courses(courses, grades, method="median")[1,2])
  expected_output <- median(grade_522)
  expect_equal(output, expected_output)

  output <- as.numeric(rank_courses(courses, grades, method="mean",
                                    descending=FALSE)[2,2])
  expected_output <- mean(grade_522)
  expect_equal(output, expected_output)

  output<- as.numeric(rank_courses(courses, grades, method="median",
                                   descending=FALSE)[1,2])
  expected_output <- median(grade_511)
  expect_equal(output, expected_output)
})

test_that("The output of rank_courses is not valid", {
  courses <- generate_courses_calculate_final_grade()
  grades <- generate_grades_calculate_final_grade()

  output <- rank_courses(courses, grades, method="mean")

  expect_true(is.data.frame(output))
  expect_equal(colnames(output), c("course_id", "grade"))
})

#tests for rank_courses end

# tests for rank_students start

generate_fake_dataframe <- function() {
  df <- data.frame(
    course_id = c(rep("511", 4), rep("522", 4)),
    student_id = c(
      "tom", "tiff", "mike", "joel",
      "tom", "tiff", "mike", "joel"
    ),
    grade = c(90, 70, 60, 50, 95, 91, 85, 80)
  )
  df
}

generate_fake_dataframe_outputs <- function(grade) {
  df <- data.frame(
    student_id = c("tom", "tiff", "mike", "joel"),
    grade = grade,
    rank = c(1, 2, 3, 4)
  )
  df
}

generate_fake_matrix <- function() {
  matrix_ex <- matrix(
    data = c(
      c(rep("511", 4), rep("522", 4)),
      c(
        "tom", "tiff", "mike", "joel", "tom",
        "tiff", "mike", "joel"
      ),
      c(
        84.66, 88.34, 87.66, 90.82, 95.52,
        87.92, 88.92, 92.80
      )
    ),
    nrow = 8,
    ncol = 3
  )
  matrix_ex
}


test_that("Incorrect input types should throw an error", {
  temp_df <- generate_fake_dataframe()
  expect_error(rank_students(df = generate_fake_matrix()))
  expect_error(rank_students(df = temp_df, courseid = 512))
  expect_error(rank_students(df = temp_df, n = "3"))
  expect_error(rank_students(df = temp_df, n = -3))
  expect_error(rank_students(df = temp_df, n = 3.5))
  expect_error(nrow(rank_students(df = temp_df, courseid = "511", n = 5)))
  expect_error(rank_students(df = temp_df, ascending = "TRUE"))
})

test_that("Dataframe should be equal", {
  temp_df <- generate_fake_dataframe()
  expect_equal(
    rank_students(df = temp_df, courseid = "511"),
    generate_fake_dataframe_outputs(c(90, 70, 60, 50))
  )
  expect_equal(
    rank_students(df = temp_df, courseid = "522"),
    generate_fake_dataframe_outputs(c(95, 91, 85, 80))
  )
  expect_equal(
    rank_students(df = temp_df, courseid = "all"),
    generate_fake_dataframe_outputs(c(92.5, 80.5, 72.5, 65))
  )
})

test_that("Grade should be between 0 and 100", {
  temp_df <- generate_fake_dataframe()
  expect_false(any(temp_df$grade < 0))
  expect_false(any(temp_df$grade > 100))
})

test_that("NAs should be dropped", {
  temp_df <- generate_fake_dataframe()
  expect_false(is.null(rank_students(df = temp_df)))
})

# tests for rank_students end

# tests for suggest_grade_adjustment start

generate_courses_suggest_grade_adjustment <- function() {
  courses <- data.frame(
    course_id = c(511),
    lab1 = c(0.15),
    lab2 = c(0.15),
    lab3 = c(0.15),
    lab4 = c(0.15),
    quiz1 = c(0.2),
    quiz2 = c(0.2)
  )

  courses
}

generate_grades_suggest_grade_adjustment <- function(
  course_id, student_id, lab1, lab2, lab3, lab4, quiz1, quiz2)
  {
  grades <- data.frame(
    course_id = c(course_id),
    student_id = c(student_id),
    lab1 = c(lab1),
    lab2 = c(lab2),
    lab3 = c(lab3),
    lab4 = c(lab4),
    quiz1 = c(quiz1),
    quiz2 = c(quiz2)
  )

  grades
}

test_that("The parameters for suggest_grade_adjustment are not valid", {
  courses <- generate_courses_suggest_grade_adjustment()
  grades <- generate_grades_suggest_grade_adjustment(
    511, "tom", 90, 90, 90, 90, 85, 85
  )
  course_id <- c("511")

  expect_error(suggest_grade_adjustment(5, grades, course_id))
  expect_error(suggest_grade_adjustment(courses, c(1, 3), course_id))
  expect_error(suggest_grade_adjustment(courses, grades, 5))
  expect_error(suggest_grade_adjustment(courses, grades, course_id, "a"))
  expect_error(suggest_grade_adjustment(courses, grades, course_id, 90, "a"))
  expect_error(suggest_grade_adjustment(courses, grades, course_id, 90, 85, "a"))
  expect_error(suggest_grade_adjustment(courses, grades, course_id, 105, 85, 100))
  expect_error(suggest_grade_adjustment(courses, grades, course_id, -5, 85, 100))
  expect_error(suggest_grade_adjustment(courses, grades, course_id, 95, 185, 100))
  expect_error(suggest_grade_adjustment(courses, grades, course_id, 95, -20, 100))
  expect_error(suggest_grade_adjustment(courses, grades, course_id, 95, 90, 120))
  expect_error(suggest_grade_adjustment(courses, grades, course_id, 95, 90, -20))
})

test_that("The outputs for suggest_grade_adjustment are incorrect", {
  courses <- generate_courses_suggest_grade_adjustment()
  grades <- generate_grades_suggest_grade_adjustment(
    511, "tom", 90, 90, 90, 90, 85, 85
  )
  course_id <- c("511")

  no_adjust <- suggest_grade_adjustment(courses, grades, course_id, 85, 85, 85)
  expect_equal(no_adjust, grades)

  labs_adjust <- suggest_grade_adjustment(courses, grades, course_id, 85, 95, 85)
  expect_equal(labs_adjust, generate_grades_suggest_grade_adjustment(
    511, "tom", 95, 95, 95, 95, 85, 85
  ))

  quiz_adjust <- suggest_grade_adjustment(courses, grades, course_id, 85, 85, 90)
  expect_equal(quiz_adjust, generate_grades_suggest_grade_adjustment(
    511, "tom", 90, 90, 90, 90, 90, 90
  ))

  course_adjust <- suggest_grade_adjustment(courses, grades, course_id, 98, 85, 90)
  expect_equal(course_adjust, generate_grades_suggest_grade_adjustment(
    511, "tom", 100, 100, 100, 100, 100, 90
  ))
})

# tests for suggest_grade_adjustment end

# tests for calculate_final_grade start

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

# tests for calculate_final_grade end
