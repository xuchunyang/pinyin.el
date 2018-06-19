;;; pinyin.el --- Convert Hanzi to Pinyin (汉字转拼音) -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords:

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

;; http://pypinyin.readthedocs.io/zh_CN/master/api.html#style
(defvar pinyin-styles
  '(
    ;; 普通风格，不带声调。如： 中国 -> ``zhong guo``
    NORMAL
    ;; 标准声调风格，拼音声调在韵母第一个字母上（默认风格）。如： 中国 -> ``zhōng guó``
    TONE)
  "拼音风格.")

(defvar pinyin-phonetic-symbols
  '((?ā "a1")
    (?á "a2")
    (?ǎ "a3")
    (?à "a4")
    (?ē "e1")
    (?é "é2")
    (?ě "e3")
    (?è "e4")
    (?ō "o1")
    (?ó "o2")
    (?ǒ "o3")
    (?ò "o4")
    (?ī "i1")
    (?í "i2")
    (?ǐ "i3")
    (?ì "i4")
    (?ū "u1")
    (?ú "u2")
    (?ǔ "u3")
    (?ù "u4")
    ;; üe
    (?ü "v")
    (?ǖ "v1")
    (?ǘ "v2")
    (?ǚ "v3")
    (?ǜ "v4")
    (?ń "n2")
    (?ň "n3")
    (?ǹ "n4")
    ;; "ḿ": "m2"
    (#x1E3F "m2"))
  "带音标字符.")

(defun pinyin-replace-symbol-to-no-symbol (pinyin)
  "把带声调字符替换为没有声调的字符."
  (mapconcat (lambda (char)
               (let ((x (assq char pinyin-phonetic-alist)))
                 (if x
                     (substring (cadr x) 0 1)
                   (string char))))
             pinyin ""))

;;;###autoload
(cl-defun pinyin (hanzi &optional (style 'TONE))
  "返回汉字的拼音列表."
  (let ((pinyins (gethash hanzi pinyin-hash-table)))
    (when pinyins
      (unless (memq style pinyin-styles)
        (error "未知的拼音风格: %s" style))
      (pcase style
        ('TONE pinyins)
        ('NORMAL (mapcar #'pinyin-replace-symbol-to-no-symbol pinyins))))))

(provide 'pinyin)
;;; pinyin.el ends here
