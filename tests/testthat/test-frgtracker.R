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

test_that("Incorrect input types should throw an error", {
  expect_error(rank_students(.courseid = 512))
  expect_error(rank_students(n = "3"))
  expect_error(rank_students(ascending = "TRUE"))
})

test_that("Dataframe should be equal"){
  dplyr::all_equal(rank_students(.courseid="511"), generate_fake_dataframe())
}
#function5 end

#function6 start

#function6 end

#function7 start

#function7 end
