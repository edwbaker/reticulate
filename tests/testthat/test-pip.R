
# a 'pip' module that shadows the real one, so that the probe in pip_version()
# can be given a known outcome without touching the environment's own pip
local_fake_pip <- function(code, envir = parent.frame()) {
  pythonpath <- withr::local_tempdir(.local_envir = envir)
  writeLines(code, file.path(pythonpath, "pip.py"))
  withr::local_envvar(PYTHONPATH = pythonpath, .local_envir = envir)
}

test_that("pip_version() reports a placeholder version when the probe fails", {

  skip_if_no_python()

  # a pip whose top-level files are missing imports as a namespace package,
  # so 'import pip' succeeds but reading pip.__version__ raises
  local_fake_pip(character())

  expect_warning(version <- pip_version(py_exe()), NA)
  expect_identical(version, numeric_version("0.0"))

  # which is how virtualenv_install() is told to reinstall pip
  expect_true(version < "8.1")

})

test_that("pip_version() reports a placeholder version for an unparseable version", {

  skip_if_no_python()

  local_fake_pip('__version__ = "unknown"')

  expect_identical(pip_version(py_exe()), numeric_version("0.0"))

})

test_that("pip_version() ignores what the interpreter writes to stderr", {

  skip_if_no_python()

  local_fake_pip(c(
    "import sys",
    'print("noise from startup", file = sys.stderr)',
    '__version__ = "24.0b1"'
  ))

  expect_identical(pip_version(py_exe()), numeric_version("24.0"))

})

test_that("pip_version() reports a placeholder version for a missing interpreter", {

  expect_identical(pip_version(tempfile()), numeric_version("0.0"))

})

test_that("pip_version() reports a placeholder version for an unrunnable interpreter", {

  # a path that exists but cannot be executed: system2() errors here on
  # Windows, rather than reporting a non-zero status
  python <- withr::local_tempfile(fileext = if (is_windows()) ".exe" else "")
  file.create(python)

  expect_warning(version <- pip_version(python), NA)
  expect_identical(version, numeric_version("0.0"))

})
