## This is basically the same, though stripped a bit of complexity, as
## context_log.
provisionr_log <- function(topic, value) {
  n <- length(value) - 1L
  assert_scalar_character(topic)
  assert_scalar_character(value)
  message(trimws(sprintf("[ %-9s ]  %s", topic, value)))
}
