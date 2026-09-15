# Problem 1
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# PART A
#Step 1
section <- factor(section, levels = c("A", "B"))

#Step 2
students <- data.frame(student_id, section, quiz1, quiz2, passed)

#Step 3
score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id
colnames(score_matrix) <- c("quiz1", "quiz2")

#Step 4
course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

#Step 5
typeof(section)
class(section)
length(section)
str(section)

#Step 6
typeof(students)
class(students)
length(students)
dim(students)
str(students)

#Step 7
typeof(score_matrix)
class(score_matrix)
length(score_matrix)
dim(score_matrix)
str(score_matrix)

#Step 8
typeof(course_record)
class(course_record)
length(course_record)
str(course_record)

#Step 9
# A vector stores multiple values of the same basic data type.
# A list can contain different kinds of objects, while a matrix stores one data type in rows and columns.
# A factor represents categorical data using a defined set of levels.
# A data frame stores data in rows and columns, and its columns can have different data types.

# Part B
#Step 1
score_matrix["S04", "quiz2"]

#Step 2
score_matrix[1:2, , drop = FALSE]

#Step 3
course_record["course"]

#Step 4
course_record[["course"]]

#Step 5
course_record$course

# Step 6
  # Single brackets [ return a subset while preserving the original container, such as a list.
  # Double brackets [[ extract one individual element from a list.
  # The $ operator extracts one component by its name.

#Part C
#Step 1
students$average <- rowMeans(students[, c("quiz1", "quiz2")])

#Step 2
students$excellent <- students$average >= 90

#Step 3
section_a_80 <- students[students$section == "A" & students$average >= 80,]

#Step 4
section_a_80 <- section_a_80[, c("student_id", "section", "average")]

#Step 5
student_averages <- students$average
names(student_averages) <- students$student_id

#Step 6
# These calculations are vectorized because they operate on entire rows or columns at once without using a loop.

#============================================================================================================================#

#Problem 2
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

#Part A
#Step 1
measurements <- read.csv(text = csv_text, na.strings = c("", "NA"))

#Step 2
head(measurements)
str(measurements)
dim(measurements)
names(measurements)

#Step 3
colSums(is.na(measurements))

#Step 4
measurements_complete <- measurements[complete.cases(measurements), ]

#Step 5
removed_ids <- measurements$sample_id[!complete.cases(measurements)]

#Step 6
# x == NA is not a valid missing-value test because comparisons with NA return NA rather than TRUE or FALSE.
# The is.na() function should be used to test whether a value is missing.

#Part B
#Step 1
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

#Step 2
levels(measurements$site)
levels(measurements$status)

#Step 3
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

#Step 4
measurements$ph_below_7 <- measurements$ph < 7

#Step 5
selected_measurements <- measurements[complete.cases(measurements[, c("temp_c", "ph")]) &  measurements$site %in% c("North", "South") & measurements$status == "ok", c("sample_id", "site", "temp_c", "temp_f", "ph")]

#Step 6
overall_mean_temp <- mean(measurements$temp_c, na.rm = TRUE)

#Step 7
south_mean_temp <- mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

#Step 8
  #These calculations use vectorized operations because they process entire columns
  #and logical conditions at once without using a loop.

#Part C
#Step 1
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

#Step 2
element_result <- A * B
element_result

#Step 3
matrix_result <- A %*% B
matrix_result

#Step 4
dim(element_result)
dim(matrix_result)

#Step 5
 #The * operator performs element-wise multiplication, so values in matching
 #positions in A and B are multiplied together.
 #The %*% operator performs matrix multiplication by multiplying rows by columns
 #and adding the products.

#============================================================================================================================#

#Problem 3
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)

#Part A
#Step 1
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {}

#Step 2
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {if (is.na(score)) {return(NA_character_)}}

#Step 3
grade_one <- function(score, a_min = 90, b_min = 80, c_min = 70, d_min = 60) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

#Step 4
grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)

#Part b
#Step 1
grades <- rep(NA_character_, length(scores))

#Step 2
for (i in seq_along(scores)) {grades[i] <- grade_one(scores[i])}

#Step 3
names(grades) <- student_id

#Step 4
  #During the loop, i represents the position of the current score and grade in their vectors.

#Part C-1
#Step 1
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  c(
    total = length(x),
    missing = sum(is.na(x)),
    mean = round(mean(x, na.rm = na.rm), digits),
    standard_deviation = round(sd(x, na.rm = na.rm), digits),
    minimum = round(min(x, na.rm = na.rm), digits),
    maximum = round(max(x, na.rm = na.rm), digits)
  )
}

#Step 2
summarize_scores(scores)

#Step 3
summarize_scores(x = scores, na.rm = TRUE, digits = 2)

#Part C-2
#Step 1
plot_scores <- function(x, ...) {plot(seq_along(x), x, ...)}

#Step 2
plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores")

#Step 3
  #The graph plots each score against its position in the scores vector.
  #The argument type = "b" displays both points and lines, and pch = 19 uses solid circles.
  #Position 3 is missing because its score is NA, so no point is plotted there.
  #The ... argument allows additional plotting options to be passed from plot_scores() to plot().










