library(supc)
supc:::.set_num_threads(2)
# test get_sorted_index

r <- supc:::.get_sorted_index(1:10)
stopifnot(isTRUE(all.equal(r, 0:9)))

r <- supc:::.get_sorted_index(10:1)
stopifnot(isTRUE(all.equal(r, 9:0)))

r <- supc:::.get_sorted_index(c(1, 3, 2, 4, 0, -1))
stopifnot(isTRUE(all.equal(r, c(5, 4, 0, 2, 1, 3))))

r <- supc:::.get_sorted_index(c(1, 1, 2, 2))
stopifnot(isTRUE(all.equal(r, c(0, 1, 2, 3))))

r <- supc:::.get_sorted_index(c(2, 1, 1, 1))
stopifnot(isTRUE(all.equal(r, c(1, 2, 3, 0))))

r <- supc:::.get_sorted_index(numeric(0))
stopifnot(isTRUE(all.equal(r, integer(0))))

# test get_inverted_index_for_sorted_index

r <- supc:::.get_inverted_index_for_sorted_index(0:9)
stopifnot(isTRUE(all.equal(r, 0:9)))

r <- supc:::.get_inverted_index_for_sorted_index(9:0)
stopifnot(isTRUE(all.equal(r, 9:0)))

r <- supc:::.get_inverted_index_for_sorted_index(c(5, 4, 0, 2, 1, 3))
stopifnot(isTRUE(all.equal(r, c(2, 4, 3, 5, 1, 0))))

r <- supc:::.get_inverted_index_for_sorted_index(c(1, 2, 3, 0))
stopifnot(isTRUE(all.equal(r, c(3, 0, 1, 2))))

r <- supc:::.get_inverted_index_for_sorted_index(integer(0))
stopifnot(isTRUE(all.equal(r, integer(0))))

