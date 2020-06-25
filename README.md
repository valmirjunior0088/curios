# Curios

Curios is a dependently typed functional programming language.

## Usage

Curios programs cannot yet be ran, but the internal representation of expressions can be viewed, and they can also be typechecked.

1. Clone the repo:

```
git clone https://github.com/valmirjunior0088/curios
```

2. Use the executable:

    1. View the internal representation of expressions:

    ```
    stack run print "{a: type, value: a, value}"
    ```

    2. Typecheck a file, and optionally print the type of a definition:

    ```
    stack run check ~/example.crs identity
    ```

## Example source

```
def identity
  {a, value, value}
end

mod pair
  def pair
    {a: type, b: type,
      [z: type, f: [first: a, second: b, z], z]
    }
  end

  def make
    {a: type, b: type, first: a, second: b,
      {z: type, f: [first: a, second: b, z], (f first second)}
    }
  end
    
  def first
    {a: type, b: type, data: (pair a b),
      (data a {first: a, second: b, first})
    }
  end
    
  def second
    {a: type, b: type, data: (pair a b),
      (data b {first: a, second: b, second})
    }
  end
end

mod either
  def either
    {a: type, b: type,
      [z: type, [value-left: a, z], [value-right: b, z], z]
    }
  end

  def left
    {a: type, b: type, value: a,
      {z: type, case-left: [value-left: a, z], case-right: [value-right: b, z],
        (case-left data)
      }
    }
  end
    
  def right
    {a: type, b: type, value: b,
      {z: type, case-left: [value-left: a, z], case-right: [value-right: b, z],
        (case-right data)
      }
    }
  end

  // To destructure an instance of either...
  // One must apply a function for each of the two possibilities:
  // (either-value string {value, "left branch"} {value, "right branch"})
end
```
