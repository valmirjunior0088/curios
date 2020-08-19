<p align="center">
  <img width="350" height="183" src="https://github.com/valmirjunior0088/curios/raw/master/logo.png">
</p>

### Checklist

- [x] [Calculus of constructions](https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/henk.pdf)
- [ ] [Implicit calculus of constructions](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.99.4335&rep=rep1&type=pdf)
- [ ] [Calculus of dependent lambda eliminations](https://github.com/astump/cedille-core-spec/blob/master/spec.pdf)
- [ ] System input and output

### How do I run this thing?

- `stack run print "[a: type. value: a. a]"` will print the internal representation of the type of the identity function;
- `stack run check ~/example.crs example` will typecheck the `~/example.crs` file. The `example` argument is optional, and if supplied, will print the type and body of said definition;

### Example source

```
def identity
  {a: type. value: a.
    value
  }
end

def Pair
  {a: type. b: type.
    [z: type. f: [first: a. second: b. z]. z]
  }
end

def pair
  {a: type. b: type. first: a. second: b.
    {z: type. f: [first: a. second: b. z]. (f first second)}
  }
end

def example
  (identity (Pair integer integer) (pair integer integer 5 10))
end
```
