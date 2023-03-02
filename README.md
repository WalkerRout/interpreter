# interpreter
Pratt Parser based interpreter written in Nim

```
let add = fn(x, y) {
  x + y
};

let sub = fn(x, y) {
  x - y
};

let mul = fn(x, y) {
  x * y
};

let div = fn(x, y) {
  x / y
};

let t = true;
let f = false;

let res = if(div(20, 2) == 10) {
  let f = if(div(500, 10) == 50) { f };
  !f
} else {
  let t = if(mul(5, 5) == 25) { t };
  !t
};

res
```

The above will produce 'true' as the result of the full computation.
