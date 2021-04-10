# monkey_rust

Rust勉強のためのmonkey言語実装.

## 実装

### 値

* 整数(i64)
* Bool
* 文字列
* 配列
* ハッシュ
* クロージャー

### 文法

* let文
* return文
* if文
* 関数コール
* 配列/ハッシュインデックス

```c
// source.monkey

let reduce = fn(arr, init, f) {
  let iter = fn(arr, result) {
    if (len(arr) == 0) {
      result;
    } else {
      iter(rest(arr), f(result, first(arr)));
    }
  };
  iter(arr, init);
};
let sum = fn(arr) {
  reduce(arr, 0, fn(init, el) { init + el; });
};
let hash = {"arr":[1, 2, 3, 4, 5, 6, 7, 8, 9]};
let arr = push(h["arr"], 10);
puts(sum(arr));
// => 55
```


## 参考文献

『Go言語でつくるインタプリタ』 (オライリー・ジャパン, 2018年)
