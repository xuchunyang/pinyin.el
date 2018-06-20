;;; pinyin.el --- Convert Hanzi to Pinyin (汉字转拼音) -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/pinyin.el
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))
;; Keywords: extensions
;; Created: 2018-06-19
;; Version: 0

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

;; A library for converting Hanzi to Pinyin

;;; Code:

(require 'cl-lib)

;;;; 拼音数据

(defvar pinyin-data-file (expand-file-name
                          "pinyin-data/pinyin.txt"
                          (file-name-directory
                           (or load-file-name buffer-file-name)))
  "拼音数据 pinyin.txt 的绝对路径.")

(defvar pinyin-hash-table
  (let ((hash (make-hash-table
               ;; grep -c ^U pinyin.txt
               ;; 41445
               :size 41445)))
    (with-temp-buffer
      (insert-file-contents pinyin-data-file)
      (while (re-search-forward
              ;; U+3007: líng,yuán,xīng  # 〇
              "^U\\+\\([[:xdigit:]]+\\): \\([^ ]*\\) +#" nil t)
        (let ((hanzi (read (concat "#x" (match-string 1))))
              (pinyins (split-string (match-string 2) ",")))
          (puthash hanzi pinyins hash)))
      hash))
  "拼音数据的 Hash Table, 键为汉字(字符), 值为拼音列表.")

;;;; 拼音风格

;; http://pypinyin.readthedocs.io/zh_CN/master/api.html#style
(defvar pinyin-styles
  '(NORMAL
    TONE
    TONE2
    TONE3
    FIRST_LETTER)
  "拼音风格.

| 风格         | 说明                                                       | 举例                |
|--------------|------------------------------------------------------------|---------------------|
| NORMAL       | 普通风格，不带声调                                         | 中国 -> zhong guo   |
| TONE         | 标准声调风格，拼音声调在韵母第一个字母上（默认风格）       | 中国 -> zhōng guó   |
| TONE2        | 声调风格2，即拼音声调在各个韵母之后，用数字 [1-4] 进行表示 | 中国 -> zho1ng guo2 |
| TONE3        | 声调风格3，即拼音声调在各个拼音之后，用数字 [1-4] 进行表示 | 中国 -> zhong1 guo2 |
| FIRST_LETTER | 首字母风格，只返回拼音的首字母部分                         | 中国 -> z g         |
")

(defvar pinyin-phonetic-alist
  '(("ā" "a1")
    ("á" "a2")
    ("ǎ" "a3")
    ("à" "a4")
    ("ē" "e1")
    ("é" "é2")
    ("ě" "e3")
    ("è" "e4")
    ("ō" "o1")
    ("ó" "o2")
    ("ǒ" "o3")
    ("ò" "o4")
    ("ī" "i1")
    ("í" "i2")
    ("ǐ" "i3")
    ("ì" "i4")
    ("ū" "u1")
    ("ú" "u2")
    ("ǔ" "u3")
    ("ù" "u4")
    ;; üe
    ("ü" "v")
    ("ǖ" "v1")
    ("ǘ" "v2")
    ("ǚ" "v3")
    ("ǜ" "v4")
    ("ń" "n2")
    ("ň" "n3")
    ("ǹ" "n4")
    ;; "ḿ": "m2"
    ("\u1E3F" "m2"))
  "带音标字符.")

(defvar pinyin-phonetic-regexp
  (concat "[" (mapconcat #'car pinyin-phonetic-alist "") "]")
  "匹配带音标字符.")

(defun pinyin-to-style-TONE (pinyin)
  "默认风格."
  pinyin)

(defun pinyin-to-style-NORMAL (pinyin)
  "去掉拼音中的声调."
  (if (string-match pinyin-phonetic-regexp pinyin)
      (let* (;; ō
             (phonetic (match-string 0 pinyin)) ; ō
             ;; o1
             (phonetic2 (cadr (assoc phonetic pinyin-phonetic-alist)))
             ;; o
             (symbol (substring phonetic2 0 1)))
        (replace-match symbol t t pinyin))
    pinyin))

(defun pinyin-to-style-TONE2 (pinyin)
  "声调摆到声母之后."
  (if (string-match pinyin-phonetic-regexp pinyin)
      (let* (;; ō
             (phonetic (match-string 0 pinyin)) ; ō
             ;; o1
             (phonetic2 (cadr (assoc phonetic pinyin-phonetic-alist))))
        (replace-match phonetic2 t t pinyin))
    pinyin))

(defun pinyin-to-style-TONE3 (pinyin)
  "声调摆到整个拼音之后."
  (if (string-match pinyin-phonetic-regexp pinyin)
      (let* (;; ō
             (phonetic (match-string 0 pinyin)) ; ō
             ;; o1
             (phonetic2 (cadr (assoc phonetic pinyin-phonetic-alist)))
             ;; o
             (symbol (substring phonetic2 0 1))
             ;; 1
             (tone (substring phonetic2 1)))
        (concat (replace-match symbol t t pinyin) tone))
    pinyin))

(defun pinyin-to-style-FIRST_LETTER (pinyin)
  "返回拼音首字母."
  (substring pinyin 0 1))

(defun pinyin-to-style (pinyin style)
  "把 TONE 风格的拼音转成其它风格."
  (if (memq style pinyin-styles)
      (funcall (intern (format "pinyin-to-style-%s" style)) pinyin)
    (error "未知的拼音风格: %s" style)))

;;;; API

;;;###autoload
(cl-defun pinyin (hanzi &optional (style 'TONE))
  "返回汉字的拼音列表."
  (let ((pinyins (gethash hanzi pinyin-hash-table)))
    (when pinyins
      (mapcar (lambda (pinyin) (pinyin-to-style pinyin style))
              pinyins))))

(provide 'pinyin)
;;; pinyin.el ends here
