# Curios

Curios is a dependently typed functional programming language.

## Usage

Curios programs cannot yet be ran, but the internal representation of expressions can be viewed.

1. Clone the repo:

```
git clone https://github.com/valmirjunior0088/curios
```

2. View the internal representation of expressions:

    1. Inline expression:

    ```
    echo "[a: type, a, a]" | stack run
    ```

    2. Source file:

    ```
    cat source.crs | stack run
    ```

## Example source

```
define identity: [a: type, a, a] =
  {a, value, value}
end

module pair
  define pair: [type, type, type] =
    {a, b,
      [z: type, [a, b, z], z]
    }
  end

  define make: [a: type, b: type, a, b, (pair a b)] =
    {a, b, first, second,
      {z, f, (f first second)}
    }
  end
    
  define first: [a: type, b: type, (pair a b), a] =
    {a, b, data,
      (data a {first, second, first})
    }
  end
    
  define second: [a: type, b: type, (pair a b), b] =
    {a, b, data,
      (data b {first, second, second})
    }
  end
end

module either
  define either: [type, type, type] =
    {a, b,
      [z: type, [a, z], [b, z], z]
    }
  end

  define left: [a: type, b: type, a, (either a b)] =
    {a, b, value,
      {z, case-left, case-right, (case-left value)}
    }
  end
    
  define right: [a: type, b: type, b, (either a b)] =
    {a, b, value,
      {z, case-left, case-right, (case-right value)}
    }
  end

  // To destructure an instance of either...
  // One must apply a function for each of the two possibilities:
  // (either-value string {value, "left branch"} {value, "right branch"})
end

module natural
  define natural: type =
    // ?
  end

  define zero: natural =
    // ?
  end

  define successor: [natural, natural] =
    // ?
  end
end

module list
  define list: [type, type] =
    // ?
  end

  define empty: [a: type, (list a)] =
    // ?
  end

  define cell: [a: type, a, (list a), (list a)] =
    // ?
  end
end
```
