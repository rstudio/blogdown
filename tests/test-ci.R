# run tests on CI (these tests will install Hugo and themes)
if (!is.na(Sys.getenv('CI', NA))) testit::test_pkg('blogdown', 'test-ci')
