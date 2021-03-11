# function1 start

# function1 end

# function2 start

# function2 end

# function3 start

# function3 end

# function4 start

# function4 end

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


generate_fake_dataframe_outputs <- function(courseid, grade) {
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
    generate_fake_dataframe_outputs(courseid = "511",
                                    c(90, 70, 60, 50))
  )
  expect_equal(
    rank_students(df = temp_df, courseid = "522"),
    generate_fake_dataframe_outputs(courseid = "522",
                                    c(95, 91, 85, 80))
  )
  expect_equal(
    rank_students(df = temp_df, courseid = "all"),
    generate_fake_dataframe_outputs(courseid = "all",
                                    c(92.5, 80.5, 72.5, 65))
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

# function6 start

# function6 end

# tests for calculate_final_grade start
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
    course_id = rep(course_id, n = 4),
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
