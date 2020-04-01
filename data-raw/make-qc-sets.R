
# -------------------------------------------------------------------------
# Make two sets of data.frames to test QC function
# -------------------------------------------------------------------------

set.seed(42)

bad_months <- c("January", "Febuary", "March", "Aprl", "Whatever",
                "June", "July", "August", "September",
                "October", "November", "Fesenber")
doubles <- round(runif(11), 3)
bad_doubles <- doubles + round(runif(12, -0.10, .22), 1)

ref_index <- c(2:9, 11)
test_data_reference <- data.frame(
  index = ref_index,
  character = month.name[ref_index],
  factor = factor(letters[ref_index], levels = letters),
  ordered = factor(LETTERS[ref_index], levels = LETTERS, ordered = TRUE),
  integer = sample(2:3, 9, replace = TRUE),
  double = doubles[ref_index],
  empty = character(9),
  stringsAsFactors = FALSE,
  row.names = NULL
)

target_index <- c(1:9, 12)
test_data_target <- data.frame(
  index = target_index,
  character = bad_months[target_index],
  factor = factor(c(letters[target_index[1:7]],
                    letters[target_index[10:8]]),
                  levels = letters),
  ordered = factor(c(LETTERS[target_index[1:7]],
                     LETTERS[target_index[10:8]]),
                   levels = LETTERS,
                   ordered = TRUE),
  integer = sample(1:3, 10, replace = TRUE),
  double = bad_doubles[target_index],
  stringsAsFactors = FALSE,
  row.names = NULL
)

usethis::use_data(test_data_reference, overwrite = TRUE)
usethis::use_data(test_data_target, overwrite = TRUE)

qc(test_data_target, test_data_reference, "index")

reindex(test_data_target, "index", test_data_reference$index)
reindex(test_data_target, "index", test_data_reference$index, add_empty = TRUE)

