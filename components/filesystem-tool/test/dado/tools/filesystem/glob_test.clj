(ns dado.tools.filesystem.glob-test
  (:require [clojure.test :refer [deftest is testing]]
            [dado.tools.filesystem.glob :as glob]))

(deftest matches-glob?-test
  (testing "exact matches"
    (is (glob/matches-glob? "foo.txt" "foo.txt")
        "Exact file name")
    (is (glob/matches-glob? "path/to/foo.txt" "path/to/foo.txt")
        "Exact path"))

  (testing "single star wildcard"
    (is (glob/matches-glob? "foo.txt" "*.txt")
        "Match file extension")
    (is (glob/matches-glob? "foo.bar.txt" "*.txt")
        "Match extension with dots")
    (is (not (glob/matches-glob? "foo/bar.txt" "*.txt"))
        "Don't match across directories")
    (is (glob/matches-glob? "foo/bar.txt" "foo/*.txt")
        "Match in directory"))

  (testing "double star wildcard"
    (is (glob/matches-glob? "foo/bar.txt" "**/*.txt")
        "Match in any subdirectory")
    (is (glob/matches-glob? "foo/bar/baz.txt" "**/*.txt")
        "Match in deep subdirectory")
    (is (glob/matches-glob? "foo.txt" "**/*.txt")
        "Match in root"))

  (testing "question mark wildcard"
    (is (glob/matches-glob? "foo.txt" "fo?.txt")
        "Match single character")
    (is (not (glob/matches-glob? "fooo.txt" "fo?.txt"))
        "Don't match multiple characters")
    (is (glob/matches-glob? "f.txt" "?.txt")
        "Match single character at start"))

  (testing "multiple wildcards"
    (is (glob/matches-glob? "foo/bar/baz.txt" "foo/*/*.txt")
        "Multiple single stars")
    (is (glob/matches-glob? "foo/bar/baz/qux.txt" "foo/**/*.txt")
        "Mix of single and double stars")
    (is (glob/matches-glob? "foo/bar.txt" "*/*.txt")
        "Star and extension")
    (is (glob/matches-glob? "a/b/c/d.txt" "**/*.txt")
        "Double star with deep nesting"))

  (testing "edge cases"
    (is (glob/matches-glob? "foo" "foo")
        "No extension")
    (is (glob/matches-glob? "" "")
        "Empty string")
    (is (not (glob/matches-glob? "foo/bar" "foo*bar"))
        "Don't match across directory separator")
    (is (glob/matches-glob? "foo/bar/" "foo/bar/")
        "Trailing slash")
    (is (glob/matches-glob? ".hidden" ".hidden")
        "Hidden files")
    (is (glob/matches-glob? ".hidden.txt" ".*.txt")
        "Hidden files with pattern"))

  (testing "common patterns"
    (is (glob/matches-glob? "src/foo/bar.clj" "src/**/*.clj")
        "All Clojure files under src")
    (is (glob/matches-glob? "test/foo_test.clj" "test/*_test.clj")
        "Test files in test directory")
    (is (glob/matches-glob? ".git/config" ".git/**")
        "All files under .git")
    (is (glob/matches-glob? "docs/api/v1/spec.md" "docs/**/*.md")
        "All markdown files under docs")))
