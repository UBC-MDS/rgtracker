#function1 start

#function1 end

#function2 start

#function2 end

#function3 start

#function3 end

#function4 start

#function4 end

#function5 start
generate_fake_dataframe <- function(){
  df <- data.frame (.studentid = c("joel", "mike", "tiff", "tom"),
                    grade = c(90.82, 87.66, 88.34, 84.66),
                    rank = c(1, 2, 3, 4)
  )
  df
}

generate_fake_matrix <- function(){
  matrix_ex <- matrix(data = c(c(rep("511",4), rep("522", 4)),
                               c("tom", "tiff","mike","joel","tom","tiff","mike","joel"),
                               c(84.66, 88.34, 87.66, 90.82, 95.52, 87.92, 88.92, 92.80)),
                      nrow = 8,
                      ncol = 3)
}

test_that("Incorrect input types should throw an error", {
  expect_error(rank_students(df = generate_fake_matrix()))
  expect_error(rank_students(.courseid = 512))
  expect_error(rank_students(n = "3"))
  expect_error(rank_students(n = -3))
  expect_error(rank_students(n = 3.5))
  expect_error(nrow(rank_students(df,.courseid="511", n = 5)))
  expect_error(rank_students(ascending = "TRUE"))
})

test_that("Dataframe should be equal"){
  dplyr::all_equal(rank_students(df,.courseid="511"), generate_fake_dataframe())
}



#function5 end

#function6 start

#function6 end

#function7 start

#function7 end
