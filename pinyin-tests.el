;;; pinyin-tests.el --- Tests for pinyin.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'pinyin)

(ert-deftest pinyin-to-style-NORMAL ()
  (should (equal (pinyin-to-style-NORMAL "a") "a"))
  (should (equal (pinyin-to-style-NORMAL "zhōng") "zhong")))

(ert-deftest pinyin-to-style-TONE2 ()
  (should (equal (pinyin-to-style-TONE2 "a") "a"))
  (should (equal (pinyin-to-style-TONE2 "zhōng") "zho1ng")))

(ert-deftest pinyin-to-style-TONE3 ()
  (should (equal (pinyin-to-style-TONE3 "a") "a"))
  (should (equal (pinyin-to-style-TONE3 "zhōng") "zhong1")))

(ert-deftest pinyin-to-style ()
  (should (equal (pinyin-to-style "zhōng" 'NORMAL) "zhong"))
  (should (equal (pinyin-to-style "zhōng" 'TONE) "zhōng"))
  (should (equal (pinyin-to-style "zhōng" 'TONE2) "zho1ng"))
  (should (equal (pinyin-to-style "zhōng" 'TONE3) "zhong1"))
  (should (equal (pinyin-to-style "zhōng" 'FIRST_LETTER) "z"))
  (should-error (pinyin-to-style "zhōng" 'unknown)))

(ert-deftest pinyin ()
  (should (equal (pinyin ?中) '("zhōng" "zhòng")))
  (should (equal (pinyin ?国) '("guó")))
  (should (equal (pinyin ?中 'TONE3) '("zhong1" "zhong4"))))

;;; pinyin-tests.el ends here
