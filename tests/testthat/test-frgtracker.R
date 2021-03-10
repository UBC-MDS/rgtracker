#function1 start
#remove when actual tests are added
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
#function1 end

#function2 start

#function2 end

#function3 start

#function3 end

#function4 start

#function4 end

#function5 start

#function5 end

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

generate_grades_suggest_grade_adjustment <- function() {
  grades <- data.frame(
    course_id = c(511),
    student_id = c("tom"),
    lab1 = c(90),
    lab2 = c(90),
    lab3 = c(90),
    lab4 = c(90),
    quiz1 = c(85),
    quiz2 = c(85)
  )

  grades
}

test_that("The parameters for suggest_grade_adjustment are not valid", {
  courses <- generate_courses_suggest_grade_adjustment()
  grades <- generate_grades_suggest_grade_adjustment()
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

# tests for suggest_grade_adjustment end

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
