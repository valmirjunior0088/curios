<p align="center">
  <img width="350" height="183" src="https://github.com/valmirjunior0088/curios/raw/master/logo.png">
</p>

### Checklist

- [x] [Dependent types](https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/henk.pdf)
- [x] General and mutual recursion
- [x] [Very dependent types](http://www.nuprl.org/documents/Hickey/FormalObjectsinTypeTheory.pdf)
- [ ] System input and output

### How do I run this thing?

- `stack run print "[A: Type, a: A, A]"` will print the internal representation of the type of the identity function;
- `stack run check ~/example.crs example` will typecheck the `~/example.crs` file. The `example` argument is optional, and if supplied, will print the type and body of said definition;

### Example source

```
let identity: [A: Type, a: A, A] =
  {A, a, a}
end

let Boolean: Type =
	[self | P: [Boolean, Type], (P true), (P false), (P self)]
end

let true: Boolean =
	{P, p_true, p_false, p_true}
end

let false: Boolean =
  {P, p_true, p_false, p_false}
end
```
