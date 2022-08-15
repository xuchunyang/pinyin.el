# ChangeLog

## [0.5.1] (2018-04-19)

* 更正 `卓`、`啥` 的拼音数据 via [#26] 。Thanks [shibingli](https://github.com/shibingli)
* 更新 `〇` 的拼音数据 via [#27]


## [0.5.0] (2018-03-18)

* 更新 Unihan 数据版本为 10.0.0 via [#19][#19]
* 新增 kMandarin_overwrite.txt 用于手工纠正 kMandarin.txt 中有误的拼音数据 via [#21][#21]
* 更正 `讽`、`识` 的最常用读音 via [#20][#20]
* 更正 埔,彷,珖,U+275C8 的常用发音 [635b238c4](https://github.com/mozillazg/pinyin-data/commit/635b238c4d21e55d8fd66299c8da3ae555253b3a)


## [0.4.1] (2017-02-12)

* `妳` 的最常用拼音调整为 `nǐ` via [eb08200](https://github.com/mozillazg/pinyin-data/commit/eb08200d0a203c57ecc62ec7a118765518430238)
* `钭` 的拼音更新为 `tǒu,dǒu` via [fb9e64e](https://github.com/mozillazg/pinyin-data/commit/fb9e64e6c0a20eb0e792e8a402dffbf8cc2dfa57)


## [0.4.0] (2016-10-17)

* Update PUA.txt 详见 [#7](https://github.com/mozillazg/pinyin-data/issues/7) thanks [@Artoria2e5][@Artoria2e5]
* Rename PUA.txt to GBK_PUA.txt 详见 [#7](https://github.com/mozillazg/pinyin-data/issues/7)
* Add kMandarin_8105.txt (《通用规范汉字表》里 8105 个汉字最常用的一个读音) [#9][#9] [#11][#11]
* Update pinyin.txt with latest data


## [0.3.0] (2016-08-19)

* Fixed format of zdic.txt via [b8e4394](https://github.com/mozillazg/pinyin-data/commit/b8e439490d2c6e8c711652983db52fb69136919b).
* Fixed some pinyin: 罗 via [468ffaa](https://github.com/mozillazg/pinyin-data/commit/468ffaa8eb678637c7565a02e6836255bd0df06c).
* Support Chinese that in PUA([Private Use Area](https://en.wikipedia.org/wiki/Private_Use_Areas>)) via [#2](https://github.com/mozillazg/pinyin-data/pull/2).
* pinyin.txt add line comments that startswith `#` via [9944f79](https://github.com/mozillazg/pinyin-data/commit/9944f795e191fb3606d65ada84b6fad5665f8776).


## [0.2.0] (2016-07-19)

* Update to the latest version of [Unihan Database](http://www.unicode.org/charts/unihan.html):

  > Date: 2016-06-01 07:01:48 GMT [JHJ]       
  > Unicode version: 9.0.0


## 0.1.0 (2016-03-11)

* Initial Release


[@Artoria2e5]: https://github.com/Artoria2e5
[#9]: https://github.com/mozillazg/pinyin-data/pull/9
[#11]: https://github.com/mozillazg/pinyin-data/pull/11
[#19]: https://github.com/mozillazg/pinyin-data/pull/19
[#20]: https://github.com/mozillazg/pinyin-data/pull/20
[#21]: https://github.com/mozillazg/pinyin-data/pull/21
[#26]: https://github.com/mozillazg/pinyin-data/pull/26
[#27]: https://github.com/mozillazg/pinyin-data/pull/27

[0.2.0]: https://github.com/mozillazg/pinyin-data/compare/v0.1.0...v0.2.0
[0.3.0]: https://github.com/mozillazg/pinyin-data/compare/v0.2.0...v0.3.0
[0.4.0]: https://github.com/mozillazg/pinyin-data/compare/v0.3.0...v0.4.0
[0.4.1]: https://github.com/mozillazg/pinyin-data/compare/v0.4.0...v0.4.1
[0.5.0]: https://github.com/mozillazg/pinyin-data/compare/v0.4.1...v0.5.0
[0.5.1]: https://github.com/mozillazg/pinyin-data/compare/v0.5.0...v0.5.1
