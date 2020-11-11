[![MELPA](https://melpa.org/packages/pinyin-badge.svg)](https://melpa.org/#/pinyin)

# `pinyin.el` - 汉字转拼音 [![CircleCI](https://circleci.com/gh/xuchunyang/pinyin.el.svg?style=svg)](https://circleci.com/gh/xuchunyang/pinyin.el)

## API

### `(pinyin HANZI &optional (STYLE 'TONE))`

``` emacs-lisp
(pinyin ?中)
;; => ("zhōng" "zhòng")

(pinyin ?中 'NORMAL)
;; => ("zhong" "zhong")

(pinyin ?中 'TONE2)
;; => ("zho1ng" "zho4ng")

(pinyin ?中 'TONE3)
;; => ("zhong1" "zhong4")

(pinyin ?中 'FIRST_LETTER)
;; => ("z" "z")
```

#### 拼音风格

| 风格         | 说明                                                       | 举例                |
|--------------|------------------------------------------------------------|---------------------|
| NORMAL       | 普通风格，不带声调                                         | 中国 -> zhong guo   |
| TONE         | 标准声调风格，拼音声调在韵母第一个字母上（默认风格）       | 中国 -> zhōng guó   |
| TONE2        | 声调风格2，即拼音声调在各个韵母之后，用数字 [1-4] 进行表示 | 中国 -> zho1ng guo2 |
| TONE3        | 声调风格3，即拼音声调在各个拼音之后，用数字 [1-4] 进行表示 | 中国 -> zhong1 guo2 |
| FIRST_LETTER | 首字母风格，只返回拼音的首字母部分                         | 中国 -> z g         |


## 拼音数据

[mozillazg/pinyin-data: 汉字拼音数据](https://github.com/mozillazg/pinyin-data) 下的 `pinyin.txt`


## 相关项目

- [mozillazg/python-pinyin: 汉字转拼音(pypinyin)](https://github.com/mozillazg/python-pinyin)
