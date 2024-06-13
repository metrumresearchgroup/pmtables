
library(testthat)
library(pmtables)

context("test-utils-table.R")

test_that("tex_bold and tex_it [PMT-TEST-0236]", {
  expect_error(tex_bold(1))
  expect_error(tex_it(1))
  ans <- tex_bold("a")
  expect_identical(ans, "\\textbf{a}")
  ans <- tex_it("a")
  expect_identical(ans, "\\textit{a}")
  ans <- tex_bold(c("a", "bxyz", "c"), "b")
  expect_identical(ans[1], "a")
  expect_identical(ans[2], "\\textbf{bxyz}")
  expect_identical(ans[3], "c")
  ans <- tex_bold(c("a", "", "c"))
  expect_identical(ans[1], "\\textbf{a}")
  expect_identical(ans[2], "")
  expect_identical(ans[3], "\\textbf{c}")
})

test_that("test-table-utils-stable_save [PMT-TEST-0237]", {
  tmp <- tempfile()
  x <- stable(data.frame(a = 1), output_file = tmp)
  expect_invisible(ans <- stable_save(x))
  expect_equal(ans, x)
  expect_true(file.exists(tmp))
  read <- readLines(tmp)
  expect_identical(as.character(x), read)
  expect_error(stable_save(as.character(x)), "x is not an 'stable' object")
  expect_error(stable_save(x, file = NULL), "Please provide a file name")
  x <- stable(data.frame(a = 1))
  expect_error(stable_save(x), "Please provide a file name")
})

test_that("save a list of tables [PMT-TEST-0238]", {
  a <- stable(stdata(), output_file = "a.tex")
  b <- stable(stdata(), output_file = "b.tex")
  l <- list(a,b)
  ans <- stable_save(l, dir = tempdir())
  expect_is(ans, "list")
})

test_that("set save caption flag at caption create", {
  cap <- as.caption("Table caption", write = TRUE)
  a <- stable(stdata(), caption = cap)
  ans0 <- stable_save(a, dir = tempdir(), file = "cap-save-0")
  text0 <- readLines(file.path(tempdir(), "cap-save-0"))
  expect_match(text0, "\\caption", fixed = TRUE, all = FALSE)
})

test_that("stable_save argument gets last word on caption save", {
  cap <- as.caption("Table caption", write = TRUE)
  a <- stable(stdata(), caption = cap)
  ans0 <- stable_save(
    a,
    dir = tempdir(),
    file = "cap-save-00",
    write_caption = FALSE
  )
  text0 <- readLines(file.path(tempdir(), "cap-save-00"))
  expect_no_match(text0, "\\caption", fixed = TRUE)
})

test_that("opt in to saving caption", {
  cap <- as.caption("Table caption", short = "caption")

  a <- stable(stdata(), caption = cap)
  ans1 <- stable_save(a, dir = tempdir(), file = "cap-save-1")
  text1 <- readLines(file.path(tempdir(), "cap-save-1"))
  expect_no_match(text1, "\\caption", fixed = TRUE)

  ans2 <- stable_save(a, dir = tempdir(), file = "cap-save-2",
                      write_caption = TRUE)
  text2 <- readLines(file.path(tempdir(), "cap-save-2"))
  expect_match(text2, "\\caption", fixed = TRUE, all = FALSE)

  b <- stable_long(stdata(), caption = cap)
  ans3 <- stable_save(b, dir = tempdir(), file = "cap-save-3",
                      write_caption = FALSE)
  text3 <- readLines(file.path(tempdir(), "cap-save-3"))
  expect_match(text3, "\\caption", fixed = TRUE, all = FALSE)
})

test_that("set output directory - default", {
  x <- pmtables::tab_notes(
    notes = letters,
    output_file = "table.tex"
  )
  expect_equal(x$output_dir, NULL)
  expect_equal(x$output_file, "table.tex")
  expect_equal(x$output_note, "table.tex")
  expect_null(x$stable_file_locked)

  data <- stdata()
  tab <- stable(data, output_file = "table.tex")
  expect_match(tab, "Source file: table.tex", fixed = TRUE, all = FALSE)
  expect_equal(attr(tab, "stable_file"), "table.tex")
  expect_null(attr(tab, "stable_file_locked"))
})

test_that("set output directory - pass dir", {
  x <- pmtables:::tab_notes(
    notes = letters,
    output_file = "table.tex",
    output_dir = "bar"
  )
  expect_equal(x$output_dir, "bar")
  expect_equal(x$output_file, "bar/table.tex")
  expect_equal(x$output_note, "table.tex")
  expect_true(x$stable_file_locked)

  data <- stdata()
  tab <- stable(data, output_file = "table.tex", output_dir = "bar")
  expect_match(tab, "Source file: table.tex", fixed = TRUE, all = FALSE)
  expect_equal(attr(tab, "stable_file"), "bar/table.tex")
  expect_true(attr(tab, "stable_file_locked"))
})

test_that("set output directory - set by option", {
  on.exit(options(pmtables.path.type = NULL, pmtables.dir = NULL), add = TRUE)
  options(pmtables.dir = "yak")
  x <- pmtables::tab_notes(
    notes = letters,
    output_file = "table.tex"
  )
  expect_equal(x$output_dir, "yak")
  expect_equal(x$output_file, "yak/table.tex")
  expect_equal(x$output_note, "table.tex")
  expect_true(x$stable_file_locked)

  data <- stdata()
  tab <- stable(data, output_file = "table.tex")
  expect_match(tab, "Source file: table.tex", fixed = TRUE, all = FALSE)
  expect_equal(attr(tab, "stable_file"), "yak/table.tex")
  expect_true(attr(tab, "stable_file_locked"))
  options(pmtables.dir = NULL)

  # Passing directory overrides option
  options(pmtables.dir = "yak")
  x <- pmtables::tab_notes(
    notes = letters,
    output_file = "table.tex",
    output_dir = "bar"
  )
  expect_equal(x$output_dir, "bar")
  expect_equal(x$output_file, "bar/table.tex")
  expect_equal(x$output_note, "table.tex")
  expect_true(x$stable_file_locked)
  options(pmtables.dir = NULL)

  data <- stdata()
  tab <- stable(data, output_file = "table.tex", output_dir = "bar")
  expect_match(tab, "Source file: table.tex", fixed = TRUE, all = FALSE)
  expect_equal(attr(tab, "stable_file"), "bar/table.tex")
  expect_true(attr(tab, "stable_file_locked"))
})

test_that("set output directory - pass as part of a file", {
  on.exit(options(pmtables.path.type = NULL, pmtables.dir = NULL), add = TRUE)

  # Pass directory as part of file
  options(pmtables.dir = "yak")
  x <- pmtables:::tab_notes(
    letters,
    output_file = "foo/table.tex",
    output_dir = "bar"
  )
  expect_equal(x$output_dir, "foo")
  expect_equal(x$output_file, "foo/table.tex")
  expect_equal(x$output_note, "table.tex")
  expect_true(x$stable_file_locked)

  data <- stdata()
  tab <- stable(data, output_file = "foo/table.tex", output_dir = "bar")
  expect_match(tab, "Source file: table.tex", fixed = TRUE, all = FALSE)
  expect_equal(attr(tab, "stable_file"), "foo/table.tex")
  expect_true(attr(tab, "stable_file_locked"))
})

test_that("option to format path - non project", {
  on.exit(options(pmtables.path.type = NULL, pmtables.dir = NULL), add = TRUE)

  options(pmtables.path.type = "none")
  x <- pmtables:::tab_notes(
    letters,
    output_file = "table.tex"
  )

  options(pmtables.path.type = "raw")
  tmp <- tempdir()
  file <- fs::path(tmp, "table.tex")
  x <- pmtables:::tab_notes(
    letters,
    output_file = file
  )
  expect_equal(x$output_note, file)
})

test_that("option to format path - path.type = proj", {
  on.exit(options(pmtables.path.type = NULL, pmtables.dir = NULL), add = TRUE)
  tdir <- tempfile("pmtables-test-") #normalizePath(tempdir(), "pmtables-test-")
  fs::dir_create(tdir)
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)

  file <- "bar.tex"
  dir <- "tables/foo"

  withr::with_dir(tdir, {
    expect_error(format_table_path(file, dir, path.type = "proj"),
                 "No RStudio project")
  })

  dir_proj <- file.path(tdir, "project")
  fs::dir_create(dir_proj)
  dir_proj <- normalizePath(dir_proj)
  cat("Version: 1.0\n", file = file.path(dir_proj, "foo.Rproj"))

  withr::with_dir(dir_proj, {
    expect_equal(
      format_table_path(
        file,
        dir,
        path.type = "proj"
      ), "tables/foo/bar.tex")
    expect_equal(
      format_table_path(
        file,
        file.path(dir_proj, dir),
        path.type = "proj"
      ), "tables/foo/bar.tex")

    expect_error(format_table_path(file, tdir, path.type = "proj"),
                 "not under root")
  })

  proj_subdir <- file.path(dir_proj, "subdir")
  fs::dir_create(proj_subdir)
  withr::with_dir(proj_subdir, {
    expect_equal(
      format_table_path(
        file,
        file.path("..", dir),
        path.type = "proj"
      ), "tables/foo/bar.tex")
    expect_equal(
      format_table_path(
        file,
        file.path(dir_proj, dir),
        path.type = "proj"
      ), "tables/foo/bar.tex")

    x <- pmtables:::tab_notes(
      output_file = "bar.tex",
      output_dir = "tables/foo",
      path.type = "proj"
    )
    expect_equal(x$file_notes, "Source file: subdir/tables/foo/bar.tex")

    options(pmtables.path.type = "proj")
    y <- pmtables:::tab_notes(
      output_file = "bar.tex",
      output_dir = "tables/foo"
    )
    expect_equal(x$notes, y$notes)
    options(pmtables.path.type = NULL)
  })
})

test_that("table-utils paste units [PMT-TEST-0239]", {
  cols <- LETTERS[c(2,5,4,3,1)]
  units <- list(C = "pounds", X = "none", B = "mg", D = "kg", Z = "liters")
  cols_new <- pmtables:::paste_units(cols, units)
  expect_equal(
    cols_new,
    c("B mg", "E", "D kg", "C pounds", "A")
  )
})
