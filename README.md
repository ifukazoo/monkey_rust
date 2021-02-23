# monkey_rust

Rust勉強のためのmonkey言語実装.

## 実装

### 値

* 整数(i64)
* Bool
* クロージャー

### 文法

* let文
* return文
* if文
* 関数コール

```c
source.monkey

let f = fn(x, y) {
  return fn() {
    if (x == y) {
      true;
    } else {
      false;
    }
  };
};
let g = f(1, 2);
g(2, 3); 
```

## 参考文献

『Go言語でつくるインタプリタ』 (オライリー・ジャパン, 2018年)
