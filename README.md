# Derivatives
> Take simple derivates in Haskell.

### How does that work?
- `1`: Equation applied to var
- `2`: AST
- `3`: Derivative Rules
- `4`: AST simplifier
- `5`: New equation

`(1) -> (2) -> (3) -> (4) -> (5)`

### Usage
- `ghc deriv.hs`
- `./derive <equation> <var>` _// see [Examples](#examples)_

### Examples
`x^7*4` (canonical form: `4x^7`)
```
./deriv x**7*4 x
----- Output
28*x**6
```

`x^7*4^2` (canonical form: `16x^7`)
```
./deriv x**7*4**2 x
----- Output
112*x**6
```

`4-3*x` (canonical form: `-3x+4`
```
./deriv 4-3*x x
----- Output
-3
```

`4*x-3*x^3` (canonical form: `-3x^3+4x`)
```
./deriv 4*x-3*x**3 y
----- Output
0
```

`4*x*y-3*x^3` (canonical form: `4xy-3x^3`
```
./deriv 4*x*y-3*x**3 y
----- Output
4*x
```

### Info
Many derivatives (such as trigonometric derivatives) are not (yet) implemented. Feel free to add them.

### Help
Pull requests are welcome :)
