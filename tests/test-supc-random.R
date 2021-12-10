library(supc)
supc:::.set_num_threads(2)
X <- structure(
  c(-0.125290762148466, 2.03672866484442, 0.832874277517991,
    6.31905616042756, 8.06590155436307, 6.8359063231764,
    3.0974858104857,  5.14766494102584, 4.1151562703307,
    0.164244239019618, 2.1187802642435,  1.18379547432164,
    6.15642726014621, 8.01491299667304, 6.60212966082732, 
    3.12396514957894, 4.9887742520942, 3.96884089865893, 
    -0.0788579907420699,  1.98813732065776, 1.22000507439678,
    6.15263514969151, 7.96709528074928,  6.9493276639727,
    3.13939267508095, 5.11133263973473, 3.8622488610901,
    -0.0610776774312712, 0.30235623369017, 1.07796864728229,
    -0.124248116108361,  -0.4429399774355, 1.22498618362862,
    2.99101327819695, 2.99676194738021,  4.18876724213706,
    -0.294150476779855, -0.0956300110217241, 1.08358831203994,
    0.271735910305809, -0.0205575454685991, 1.07753432231187,
    2.98923899188342,  2.72458808863428, 3.91700108734006,
    -0.141499031392424, 0.0729163924273661,  1.15370658490308,
    -0.0224692424300456, 0.176221545290843, 1.07962117607341,
    2.87759472134985, 3.06822393828489, 3.77412738078384
  ),
  .Dim = c(27L,  2L)
)
.group <- list(
  c(1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 
    1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 2L), 
  c(1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 
    1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L),
  c(1L, 1L, 1L, 2L, 2L, 2L, 
    2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 
    1L, 1L, 1L, 2L, 1L),
  c(1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 
    2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
    2L),
  c(2L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 
    1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L),
  c(2L, 1L, 
    2L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 
    1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L),
  c(2L, 1L, 2L, 1L, 1L, 2L, 
    2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 
    2L, 1L, 1L, 2L, 1L),
  c(2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 
    2L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 
    1L),
  c(2L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 
    1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 2L, 1L),
  c(2L, 1L, 
    1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 
    1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L), 
  c(1L, 1L, 1L, 2L, 2L, 2L, 
    1L, 1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 
    2L, 1L, 2L, 1L, 2L), 
  c(2L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 
    1L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 
    2L)
)
check.names.ref <- c("x", "r", "cluster", "centers", "size", "result", "iteration")
# Checking with reference object

dist.mode("stats")
obj.supc1 <- tryCatch({
  supc1(X, r = 0.9, t = 0.75, verbose = TRUE)
}, error = function(e) {
  if (conditionMessage(e) == supc:::.check.compatibility.error.msg) NULL else stop(conditionMessage(e))
})
obj.random.R <- supc.random(X, r = 0.9, t = 0.75, k = 2, implementation = "R", groups = .group, verbose = TRUE)
obj.random.cpp <- tryCatch({
  supc.random(X, r = 0.9, t = 0.75, k = 2, implementation = "cpp", groups = .group, verbose = TRUE)
}, error = function(e) {
  if (conditionMessage(e) == supc:::.check.compatibility.error.msg) NULL else stop(conditionMessage(e))
})

if (!is.null(obj.supc1) & !is.null(obj.random.cpp)) {
  stopifnot(isTRUE(all.equal(obj.supc1$cluster, obj.random.R$cluster)))
  stopifnot(isTRUE(all.equal(obj.supc1$cluster, obj.random.cpp$cluster)))
  
  stopifnot(isTRUE(all.equal(obj.random.R[check.names.ref], obj.random.cpp[check.names.ref])))
  stopifnot(is.null(obj.supc1$d0))
  stopifnot(is.null(obj.random.cpp$d0))
}


stopifnot(is.null(obj.random.R$d0))

## check supclist
objs <- tryCatch({
  .k <- 5
  .idx <- c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L,  1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L)
  .group <- list(
    c(2L, 4L, 5L, 1L, 2L, 5L, 4L, 4L, 5L, 1L, 2L, 3L, 4L, 1L,  4L, 3L, 2L, 1L, 3L, 1L, 1L, 5L, 3L, 3L, 2L, 5L, 2L),
    c(2L, 1L,  4L, 3L, 1L, 3L, 1L, 2L, 5L, 3L, 4L, 4L, 5L, 4L, 1L, 3L, 1L, 1L,  2L, 3L, 5L, 2L, 2L, 2L, 5L, 5L, 4L),
    c(2L, 1L, 3L, 1L, 5L, 4L,  5L, 3L, 1L, 3L, 2L, 2L, 2L, 4L, 3L, 5L, 5L, 1L, 2L, 4L, 4L, 5L,  4L, 2L, 1L, 1L, 3L),
    c(4L, 1L, 3L, 5L, 5L, 2L, 2L, 5L, 1L, 1L,  1L, 3L, 3L, 4L, 2L, 5L, 1L, 4L, 2L, 5L, 3L, 4L, 2L, 2L, 3L, 4L,  1L),
    c(5L, 2L, 1L, 3L, 2L, 2L, 1L, 2L, 4L, 3L, 3L, 5L, 2L, 4L,  4L, 3L, 1L, 3L, 4L, 2L, 5L, 1L, 4L, 5L, 5L, 1L, 1L),
    c(2L, 1L,  5L, 5L, 3L, 1L, 2L, 1L, 4L, 2L, 3L, 1L, 4L, 5L, 4L, 1L, 5L, 4L,  2L, 3L, 2L, 3L, 2L, 1L, 4L, 5L, 3L),
    c(3L, 1L, 5L, 5L, 2L, 3L,  3L, 5L, 5L, 2L, 4L, 1L, 4L, 4L, 3L, 1L, 1L, 2L, 2L, 1L, 2L, 3L,  4L, 4L, 5L, 1L, 2L),
    c(4L, 1L, 1L, 5L, 5L, 2L, 1L, 1L, 1L, 5L,  5L, 3L, 1L, 4L, 3L, 3L, 4L, 2L, 4L, 2L, 5L, 4L, 3L, 2L, 3L, 2L,  2L),
    c(2L, 3L, 3L, 2L, 5L, 4L, 1L, 3L, 2L, 1L, 5L, 5L, 2L, 2L,  4L, 1L, 1L, 5L, 3L, 4L, 3L, 1L, 5L, 2L, 1L, 4L, 4L),
    c(1L, 5L,  1L, 5L, 5L, 1L, 4L, 5L, 3L, 2L, 3L, 2L, 3L, 4L, 4L, 1L, 3L, 4L,  5L, 3L, 2L, 1L, 2L, 4L, 2L, 2L, 1L),
    c(4L, 2L, 1L, 3L, 2L, 3L,  5L, 4L, 5L, 3L, 2L, 4L, 4L, 5L, 5L, 1L, 1L, 4L, 3L, 2L, 1L, 3L,  1L, 1L, 2L, 5L, 2L),
    c(1L, 1L, 5L, 1L, 4L, 2L, 3L, 3L, 2L, 5L,  1L, 2L, 2L, 4L, 3L, 5L, 1L, 4L, 5L, 1L, 4L, 2L, 4L, 3L, 3L, 5L,  2L),
    c(2L, 4L, 2L, 3L, 1L, 4L, 4L, 4L, 1L, 2L, 4L, 2L, 3L, 5L,  3L, 5L, 1L, 2L, 3L, 1L, 5L, 1L, 5L, 3L, 1L, 2L, 5L),
    c(4L, 1L,  4L, 1L, 2L, 1L, 5L, 4L, 4L, 2L, 3L, 3L, 5L, 1L, 3L, 2L, 2L, 3L,  5L, 5L, 2L, 1L, 4L, 3L, 1L, 5L, 2L),
    c(1L, 5L, 2L, 4L, 3L, 5L,  3L, 2L, 2L, 4L, 1L, 5L, 5L, 1L, 3L, 1L, 4L, 2L, 2L, 3L, 4L, 5L,  2L, 1L, 4L, 3L, 1L),
    c(3L, 2L, 2L, 1L, 5L, 1L, 3L, 4L, 4L, 3L,  2L, 5L, 5L, 4L, 3L, 5L, 2L, 1L, 5L, 2L, 3L, 4L, 4L, 1L, 1L, 2L,  1L),
    c(3L, 3L, 5L, 1L, 2L, 4L, 2L, 4L, 4L, 3L, 3L, 2L, 1L, 2L,  1L, 5L, 3L, 5L, 5L, 4L, 1L, 2L, 1L, 5L, 2L, 1L, 4L),
    c(5L, 5L,  2L, 2L, 2L, 4L, 3L, 5L, 4L, 3L, 1L, 1L, 3L, 1L, 1L, 5L, 1L, 4L,  4L, 2L, 3L, 5L, 2L, 3L, 2L, 1L, 4L),
    c(3L, 4L, 3L, 5L, 1L, 5L,  2L, 2L, 5L, 1L, 5L, 2L, 1L, 2L, 4L, 5L, 4L, 1L, 1L, 3L, 2L, 3L,  4L, 4L, 1L, 3L, 2L),
    c(4L, 4L, 4L, 4L, 1L, 1L, 3L, 1L, 3L, 3L,  2L, 5L, 1L, 2L, 2L, 5L, 1L, 2L, 5L, 5L, 4L, 3L, 2L, 5L, 2L, 1L,  3L),
    c(4L, 1L, 1L, 5L, 5L, 2L, 2L, 1L, 3L, 5L, 3L, 4L, 3L, 1L,  5L, 1L, 2L, 4L, 2L, 3L, 1L, 2L, 2L, 4L, 3L, 4L, 5L),
    c(4L, 4L,  4L, 2L, 3L, 5L, 3L, 1L, 2L, 1L, 3L, 3L, 5L, 4L, 1L, 3L, 5L, 2L,  1L, 4L, 1L, 5L, 2L, 5L, 1L, 2L, 2L),
    c(5L, 3L, 3L, 2L, 5L, 1L,  2L, 5L, 2L, 1L, 1L, 2L, 1L, 2L, 4L, 5L, 5L, 4L, 4L, 1L, 3L, 4L,  2L, 1L, 3L, 3L, 4L),
    c(1L, 1L, 4L, 1L, 5L, 3L, 2L, 1L, 4L, 2L,  2L, 2L, 4L, 3L, 5L, 2L, 4L, 5L, 2L, 3L, 1L, 5L, 1L, 5L, 3L, 4L,  3L),
    c(1L, 1L, 5L, 5L, 5L, 1L, 1L, 4L, 4L, 1L, 2L, 1L, 4L, 3L,  3L, 5L, 4L, 2L, 2L, 3L, 3L, 4L, 3L, 5L, 2L, 2L, 2L),
    c(5L, 1L,  3L, 3L, 2L, 5L, 1L, 2L, 4L, 1L, 1L, 4L, 2L, 5L, 3L, 4L, 4L, 4L,  3L, 5L, 3L, 2L, 5L, 1L, 2L, 2L, 1L),
    c(3L, 3L, 3L, 1L, 4L, 1L,  1L, 5L, 4L, 2L, 1L, 4L, 1L, 2L, 2L, 2L, 3L, 1L, 3L, 5L, 4L, 5L,  5L, 4L, 5L, 2L, 2L),
    c(2L, 5L, 4L, 2L, 3L, 5L, 5L, 1L, 3L, 1L,  2L, 5L, 4L, 1L, 2L, 3L, 3L, 1L, 2L, 4L, 4L, 4L, 1L, 5L, 2L, 1L,  3L),
    c(5L, 5L, 2L, 1L, 3L, 5L, 4L, 2L, 2L, 5L, 2L, 3L, 3L, 2L,  2L, 5L, 1L, 4L, 1L, 3L, 1L, 4L, 3L, 1L, 4L, 4L, 1L),
    c(2L, 3L,  3L, 4L, 5L, 2L, 1L, 4L, 3L, 2L, 1L, 1L, 1L, 1L, 4L, 5L, 4L, 2L,  3L, 1L, 5L, 2L, 2L, 5L, 4L, 5L, 3L),
    c(5L, 5L, 4L, 2L, 5L, 4L,  3L, 2L, 1L, 5L, 1L, 1L, 3L, 1L, 3L, 2L, 2L, 3L, 3L, 4L, 4L, 5L,  1L, 1L, 4L, 2L, 2L),
    c(1L, 2L, 5L, 2L, 2L, 3L, 5L, 1L, 5L, 2L,  3L, 3L, 4L, 1L, 4L, 3L, 5L, 2L, 3L, 2L, 1L, 5L, 4L, 4L, 4L, 1L,  1L),
    c(1L, 5L, 5L, 2L, 4L, 4L, 1L, 4L, 2L, 5L, 4L, 1L, 2L, 3L,  2L, 4L, 1L, 2L, 1L, 5L, 5L, 3L, 1L, 3L, 3L, 2L, 3L),
    c(1L, 3L,  5L, 4L, 3L, 5L, 3L, 4L, 3L, 4L, 3L, 5L, 2L, 5L, 1L, 2L, 2L, 2L,  4L, 1L, 2L, 2L, 5L, 1L, 1L, 1L, 4L),
    c(5L, 2L, 3L, 1L, 1L, 4L,  5L, 1L, 1L, 4L, 5L, 5L, 4L, 1L, 2L, 4L, 3L, 2L, 3L, 2L, 4L, 3L,  2L, 5L, 3L, 2L, 1L),
    c(3L, 2L, 4L, 4L, 2L, 2L, 1L, 3L, 5L, 1L,  1L, 5L, 2L, 2L, 1L, 5L, 1L, 5L, 4L, 3L, 2L, 4L, 3L, 1L, 4L, 5L,  3L),
    c(1L, 2L, 4L, 1L, 5L, 1L, 2L, 1L, 3L, 1L, 1L, 4L, 3L, 3L,  4L, 3L, 5L, 3L, 5L, 2L, 4L, 2L, 5L, 2L, 2L, 4L, 5L),
    c(2L, 5L,  3L, 4L, 4L, 2L, 1L, 5L, 1L, 1L, 4L, 3L, 3L, 2L, 1L, 2L, 3L, 2L,  4L, 5L, 2L, 5L, 1L, 5L, 3L, 1L, 4L),
    c(5L, 2L, 1L, 1L, 3L, 4L,  1L, 5L, 2L, 1L, 3L, 4L, 2L, 2L, 5L, 1L, 2L, 4L, 4L, 3L, 5L, 3L,  1L, 4L, 2L, 5L, 3L),
    c(1L, 2L, 5L, 3L, 2L, 1L, 1L, 4L, 5L, 4L,  5L, 2L, 4L, 3L, 2L, 3L, 2L, 2L, 3L, 4L, 1L, 3L, 1L, 5L, 1L, 4L,  5L),
    c(1L, 2L, 2L, 4L, 3L, 2L, 5L, 1L, 3L, 5L, 2L, 1L, 4L, 3L,  4L, 2L, 1L, 3L, 1L, 4L, 2L, 5L, 3L, 4L, 5L, 5L, 1L),
    c(3L, 5L,  4L, 5L, 1L, 2L, 1L, 3L, 2L, 1L, 1L, 3L, 4L, 4L, 2L, 3L, 5L, 2L,  1L, 5L, 5L, 4L, 4L, 2L, 3L, 2L, 1L),
    c(1L, 4L, 1L, 5L, 3L, 5L,  4L, 2L, 4L, 2L, 3L, 1L, 4L, 1L, 4L, 5L, 3L, 2L, 3L, 3L, 1L, 1L,  5L, 2L, 2L, 2L, 5L),
    c(1L, 1L, 2L, 4L, 5L, 1L, 3L, 3L, 5L, 1L,  1L, 4L, 5L, 5L, 3L, 5L, 2L, 2L, 2L, 2L, 2L, 4L, 3L, 1L, 3L, 4L,  4L),
    c(1L, 5L, 3L, 4L, 2L, 1L, 2L, 2L, 5L, 4L, 1L, 5L, 5L, 3L,  3L, 5L, 3L, 1L, 3L, 2L, 4L, 1L, 1L, 2L, 2L, 4L, 4L),
    c(2L, 5L,  2L, 5L, 2L, 1L, 5L, 1L, 5L, 4L, 1L, 3L, 1L, 4L, 4L, 3L, 2L, 3L,  5L, 4L, 1L, 1L, 3L, 4L, 2L, 3L, 2L),
    c(2L, 4L, 2L, 4L, 2L, 5L,  5L, 3L, 2L, 1L, 2L, 5L, 1L, 2L, 4L, 3L, 5L, 1L, 1L, 3L, 3L, 5L,  4L, 1L, 3L, 4L, 1L),
    c(5L, 1L, 1L, 2L, 4L, 3L, 3L, 2L, 3L, 2L,  4L, 2L, 4L, 2L, 1L, 1L, 4L, 5L, 2L, 5L, 5L, 4L, 5L, 3L, 1L, 3L,  1L),
    c(5L, 2L, 2L, 2L, 3L, 2L, 1L, 5L, 4L, 1L, 3L, 1L, 1L, 3L,  5L, 4L, 2L, 2L, 4L, 3L, 1L, 3L, 5L, 1L, 5L, 4L, 4L),
    c(2L, 1L,  3L, 2L, 5L, 2L, 5L, 1L, 1L, 4L, 1L, 3L, 4L, 5L, 3L, 1L, 5L, 2L,  4L, 5L, 4L, 4L, 1L, 3L, 2L, 2L, 3L),
    c(5L, 2L, 1L, 2L, 3L, 1L,  5L, 5L, 3L, 4L, 2L, 5L, 2L, 2L, 4L, 2L, 3L, 4L, 1L, 4L, 1L, 4L,  1L, 1L, 3L, 3L, 5L),
    c(3L, 2L, 2L, 5L, 1L, 1L, 1L, 2L, 1L, 4L,  5L, 1L, 2L, 4L, 2L, 4L, 1L, 2L, 5L, 4L, 3L, 3L, 4L, 5L, 3L, 3L,  5L),
    c(1L, 5L, 4L, 5L, 2L, 2L, 4L, 4L, 5L, 3L, 3L, 3L, 1L, 5L,  2L, 2L, 1L, 2L, 5L, 3L, 4L, 1L, 4L, 1L, 3L, 1L, 2L),
    c(1L, 1L,  2L, 1L, 5L, 3L, 5L, 5L, 3L, 4L, 2L, 2L, 3L, 5L, 2L, 3L, 4L, 4L,  1L, 2L, 5L, 1L, 2L, 1L, 4L, 3L, 4L),
    c(3L, 2L, 4L, 1L, 5L, 1L,  1L, 3L, 2L, 3L, 5L, 2L, 5L, 2L, 2L, 5L, 4L, 5L, 1L, 3L, 2L, 4L,  1L, 3L, 1L, 4L, 4L),
    c(3L, 5L, 2L, 1L, 5L, 1L, 4L, 5L, 1L, 3L,  2L, 2L, 3L, 2L, 4L, 1L, 3L, 3L, 4L, 2L, 2L, 4L, 5L, 4L, 1L, 1L,  5L),
    c(4L, 5L, 5L, 2L, 1L, 1L, 2L, 4L, 3L, 3L, 4L, 3L, 2L, 2L,  1L, 4L, 2L, 5L, 5L, 2L, 3L, 1L, 1L, 1L, 3L, 5L, 4L),
    c(1L, 1L,  1L, 2L, 1L, 2L, 1L, 5L, 5L, 2L, 2L, 4L, 3L, 4L, 1L, 4L, 5L, 4L,  3L, 3L, 2L, 4L, 2L, 3L, 3L, 5L, 5L),
    c(4L, 2L, 1L, 1L, 3L, 3L,  2L, 4L, 2L, 5L, 4L, 5L, 2L, 5L, 1L, 3L, 5L, 4L, 4L, 3L, 2L, 1L,  1L, 2L, 5L, 1L, 3L),
    c(5L, 4L, 3L, 4L, 5L, 5L, 2L, 1L, 4L, 4L,  1L, 3L, 3L, 1L, 3L, 2L, 2L, 1L, 2L, 5L, 1L, 4L, 2L, 1L, 3L, 2L,  5L),
    c(1L, 2L, 5L, 4L, 2L, 2L, 1L, 3L, 4L, 5L, 1L, 3L, 1L, 1L,  2L, 5L, 3L, 2L, 3L, 4L, 3L, 4L, 1L, 2L, 5L, 4L, 5L),
    c(3L, 4L,  2L, 1L, 5L, 1L, 3L, 1L, 4L, 3L, 2L, 4L, 3L, 5L, 3L, 4L, 2L, 5L,  1L, 2L, 2L, 5L, 5L, 2L, 1L, 4L, 1L),
    c(2L, 1L, 5L, 4L, 4L, 3L,  2L, 3L, 1L, 1L, 4L, 2L, 3L, 1L, 5L, 4L, 5L, 3L, 3L, 1L, 2L, 2L,  1L, 5L, 5L, 2L, 4L),
    c(3L, 1L, 3L, 1L, 3L, 2L, 2L, 2L, 5L, 2L,  2L, 5L, 4L, 4L, 5L, 1L, 2L, 4L, 3L, 4L, 1L, 5L, 1L, 4L, 1L, 3L,  5L),
    c(5L, 1L, 2L, 3L, 2L, 4L, 5L, 2L, 1L, 4L, 2L, 1L, 4L, 5L,  1L, 2L, 3L, 4L, 1L, 5L, 1L, 5L, 3L, 4L, 3L, 2L, 3L),
    c(2L, 1L,  5L, 3L, 5L, 4L, 1L, 4L, 3L, 4L, 4L, 3L, 5L, 1L, 3L, 2L, 4L, 2L,  2L, 2L, 1L, 1L, 3L, 2L, 5L, 1L, 5L),
    c(4L, 2L, 4L, 5L, 1L, 3L,  4L, 1L, 5L, 5L, 2L, 2L, 1L, 5L, 3L, 1L, 3L, 2L, 5L, 2L, 3L, 2L,  1L, 4L, 1L, 4L, 3L),
    c(4L, 1L, 3L, 1L, 4L, 5L, 4L, 1L, 4L, 4L,  2L, 3L, 3L, 5L, 2L, 2L, 1L, 5L, 1L, 3L, 5L, 2L, 5L, 2L, 2L, 1L,  3L),
    c(3L, 1L, 5L, 1L, 1L, 1L, 3L, 2L, 5L, 3L, 4L, 5L, 4L, 4L,  3L, 1L, 1L, 2L, 5L, 2L, 2L, 4L, 4L, 5L, 2L, 3L, 2L),
    c(3L, 2L,  5L, 4L, 1L, 1L, 3L, 5L, 4L, 1L, 2L, 3L, 4L, 3L, 2L, 4L, 5L, 1L,  1L, 2L, 5L, 4L, 2L, 2L, 1L, 5L, 3L),
    c(2L, 1L, 2L, 2L, 1L, 2L,  1L, 1L, 5L, 3L, 4L, 5L, 4L, 3L, 2L, 4L, 1L, 4L, 5L, 3L, 3L, 1L,  4L, 3L, 5L, 5L, 2L),
    c(4L, 3L, 2L, 2L, 4L, 5L, 1L, 5L, 3L, 4L,  2L, 1L, 2L, 2L, 3L, 4L, 4L, 5L, 3L, 1L, 2L, 1L, 1L, 3L, 5L, 1L,  5L),
    c(1L, 5L, 2L, 5L, 2L, 5L, 4L, 5L, 3L, 2L, 4L, 1L, 2L, 4L,  2L, 1L, 4L, 3L, 3L, 1L, 5L, 4L, 1L, 1L, 2L, 3L, 3L),
    c(4L, 4L,  1L, 2L, 4L, 1L, 2L, 2L, 3L, 3L, 4L, 1L, 3L, 3L, 4L, 1L, 3L, 5L,  1L, 5L, 5L, 2L, 2L, 2L, 5L, 1L, 5L),
    c(2L, 3L, 5L, 1L, 3L, 2L,  1L, 1L, 5L, 5L, 4L, 1L, 4L, 4L, 2L, 5L, 2L, 1L, 4L, 3L, 2L, 5L,  4L, 3L, 2L, 1L, 3L),
    c(2L, 4L, 4L, 1L, 3L, 4L, 4L, 1L, 5L, 2L,  3L, 1L, 2L, 5L, 5L, 3L, 5L, 2L, 3L, 2L, 1L, 4L, 3L, 5L, 1L, 1L,  2L),
    c(5L, 1L, 3L, 1L, 2L, 5L, 2L, 3L, 4L, 3L, 1L, 3L, 5L, 1L,  2L, 2L, 5L, 4L, 4L, 2L, 4L, 4L, 3L, 2L, 1L, 5L, 1L),
    c(4L, 4L,  3L, 3L, 4L, 5L, 4L, 2L, 5L, 1L, 4L, 2L, 1L, 5L, 5L, 2L, 1L, 2L,  2L, 3L, 5L, 3L, 1L, 2L, 3L, 1L, 1L),
    c(1L, 1L, 2L, 2L, 1L, 1L,  5L, 3L, 3L, 4L, 3L, 1L, 3L, 4L, 2L, 5L, 2L, 2L, 1L, 2L, 4L, 3L,  5L, 4L, 4L, 5L, 5L),
    c(1L, 1L, 5L, 3L, 4L, 4L, 3L, 4L, 1L, 3L,  2L, 1L, 2L, 2L, 2L, 3L, 1L, 5L, 2L, 2L, 4L, 3L, 5L, 4L, 5L, 5L,  1L),
    c(2L, 1L, 1L, 3L, 5L, 5L, 3L, 3L, 3L, 1L, 1L, 5L, 5L, 4L,  4L, 2L, 4L, 5L, 3L, 2L, 2L, 2L, 2L, 1L, 4L, 1L, 4L),
    c(5L, 1L,  3L, 5L, 4L, 1L, 1L, 3L, 3L, 2L, 4L, 2L, 2L, 4L, 5L, 3L, 2L, 1L,  4L, 5L, 1L, 4L, 2L, 3L, 5L, 2L, 1L),
    c(1L, 4L, 3L, 4L, 2L, 5L,  4L, 1L, 5L, 3L, 1L, 2L, 1L, 5L, 3L, 5L, 2L, 4L, 3L, 2L, 2L, 3L,  1L, 1L, 4L, 5L, 2L),
    c(5L, 1L, 5L, 3L, 4L, 1L, 3L, 3L, 4L, 2L,  1L, 2L, 4L, 5L, 5L, 2L, 1L, 3L, 1L, 4L, 2L, 4L, 5L, 1L, 3L, 2L,  2L),
    c(1L, 5L, 2L, 1L, 4L, 4L, 3L, 4L, 1L, 5L, 4L, 1L, 5L, 1L,  3L, 5L, 3L, 5L, 3L, 2L, 2L, 2L, 2L, 2L, 1L, 3L, 4L),
    c(1L, 5L,  2L, 4L, 4L, 1L, 2L, 3L, 4L, 1L, 3L, 2L, 2L, 2L, 3L, 5L, 5L, 5L,  1L, 2L, 5L, 3L, 1L, 4L, 1L, 4L, 3L),
    c(5L, 1L, 4L, 2L, 5L, 2L,  5L, 2L, 3L, 2L, 2L, 3L, 5L, 4L, 3L, 3L, 5L, 1L, 1L, 4L, 4L, 3L,  1L, 1L, 4L, 1L, 2L),
    c(3L, 1L, 1L, 1L, 1L, 4L, 5L, 3L, 4L, 5L,  4L, 2L, 2L, 2L, 3L, 3L, 1L, 2L, 5L, 5L, 2L, 5L, 3L, 2L, 1L, 4L,  4L),
    c(5L, 4L, 3L, 3L, 1L, 4L, 1L, 4L, 5L, 1L, 2L, 1L, 3L, 1L,  1L, 3L, 5L, 5L, 3L, 2L, 4L, 2L, 5L, 2L, 2L, 2L, 4L),
    c(2L, 5L,  4L, 5L, 4L, 1L, 1L, 1L, 4L, 2L, 3L, 5L, 4L, 3L, 4L, 1L, 2L, 5L,  2L, 2L, 2L, 1L, 3L, 5L, 1L, 3L, 3L),
    c(4L, 5L, 3L, 2L, 1L, 4L,  1L, 1L, 2L, 3L, 1L, 1L, 5L, 2L, 4L, 5L, 2L, 5L, 2L, 5L, 4L, 3L,  3L, 4L, 3L, 1L, 2L),
    c(4L, 4L, 1L, 5L, 1L, 5L, 1L, 4L, 2L, 5L,  1L, 2L, 3L, 4L, 1L, 2L, 3L, 2L, 2L, 4L, 3L, 2L, 1L, 5L, 3L, 3L,  5L),
    c(1L, 2L, 4L, 2L, 3L, 3L, 3L, 5L, 4L, 5L, 5L, 2L, 1L, 1L,  2L, 4L, 5L, 1L, 1L, 2L, 3L, 1L, 4L, 3L, 4L, 2L, 5L),
    c(2L, 1L,  4L, 1L, 4L, 2L, 1L, 1L, 5L, 3L, 2L, 5L, 4L, 3L, 5L, 4L, 5L, 3L,  3L, 1L, 1L, 5L, 2L, 3L, 4L, 2L, 2L),
    c(2L, 4L, 5L, 2L, 3L, 5L,  1L, 3L, 1L, 1L, 3L, 2L, 4L, 4L, 1L, 2L, 1L, 1L, 2L, 4L, 5L, 5L,  4L, 5L, 3L, 3L, 2L),
    c(4L, 1L, 2L, 2L, 5L, 3L, 5L, 5L, 3L, 1L,  2L, 2L, 4L, 5L, 2L, 1L, 3L, 1L, 1L, 4L, 2L, 3L, 4L, 4L, 5L, 1L,  3L),
    c(1L, 4L, 1L, 5L, 2L, 5L, 1L, 5L, 4L, 3L, 2L, 1L, 3L, 2L,  3L, 5L, 5L, 4L, 4L, 1L, 3L, 3L, 2L, 2L, 2L, 1L, 4L),
    c(5L, 3L,  1L, 2L, 2L, 3L, 5L, 2L, 4L, 1L, 3L, 2L, 2L, 4L, 5L, 1L, 1L, 4L,  4L, 3L, 1L, 5L, 4L, 2L, 1L, 5L, 3L),
    c(3L, 2L, 2L, 1L, 5L, 3L,  4L, 2L, 3L, 1L, 5L, 5L, 4L, 1L, 2L, 5L, 1L, 3L, 4L, 4L, 3L, 4L,  1L, 1L, 5L, 2L, 2L),
    c(2L, 3L, 5L, 4L, 3L, 5L, 3L, 4L, 5L, 2L,  3L, 1L, 5L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 5L, 4L, 4L, 3L, 4L, 1L,  1L)
  )
  list(
    supc.random(X, r = c(.9, 1.7, 2.5), t = 0.75, k = .k, implementation = "R", groups = .group, verbose = TRUE),
    supc.random(X, r = c(.9, 1.7, 2.5), t = 0.75, k = .k, implementation = "cpp", groups = .group, verbose = TRUE)
  )
}, error = function(e) {
  if (conditionMessage(e) == supc:::.check.compatibility.error.msg) NULL else stop(conditionMessage(e))
})
if (!is.null(objs)) {
  stopifnot(sapply(objs, class) == "supclist")
  stopifnot(sapply(objs, length) == 3)
  check.names.ref <- c("x", "r", "cluster", "centers", "size", "iteration", "result")
  stopifnot(isTRUE(all.equal(
    objs[[1]][[1]][check.names.ref],
    objs[[2]][[1]][check.names.ref]
  )))
  stopifnot(isTRUE(all.equal(
    objs[[1]][[2]][check.names.ref],
    objs[[2]][[2]][check.names.ref]
  )))
  stopifnot(isTRUE(all.equal(
    objs[[1]][[3]][check.names.ref],
    objs[[2]][[3]][check.names.ref]
  )))
}
