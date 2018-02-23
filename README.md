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
- `./derive <equation> <var>` _./deriv "x^2+x" x_

### Info
Many derivatives (such as trigonometric derivatives) are not (yet) implemented. Feel free to add them.

### Help
Pull requests are welcome :)
