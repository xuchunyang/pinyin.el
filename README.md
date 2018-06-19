# 汉字转拼音

## API

### `(pinyin HANZI &optional (STYLE 'TONE))`

``` emacs-lisp
(pinyin ?中)
;; => ("zhōng" "zhòng")

(pinyin ?中 'NORMAL)
;; => ("zhong" "zhong")
```
