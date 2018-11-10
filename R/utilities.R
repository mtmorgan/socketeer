is_scalar_character <-
    function(x)
{
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

is_scalar_integer <-
    function(x)
{
    is.integer(x) && length(x) == 1L && !is.na(x)
}
