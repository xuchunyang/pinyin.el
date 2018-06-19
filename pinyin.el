;;; pinyin.el --- 汉字转拼音                         -*- lexical-binding: t; -*-

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
  "PATH to pinyin.txt.")

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
      (setq pinyin-hash-table hash)))
  "Hash table listing Hanzi-Pinyin data.")

(provide 'pinyin)
;;; pinyin.el ends here
