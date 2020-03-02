# Curios

Curios is an interpreter (and hopefully, in the future, a compiler) for a work-in-progress dependently typed functional programming language. The project is still very young, there is only an abstract syntax tree and a parser for it, but progress, albeit slow, is steady.

# Example source

```
(module examples
  (define identity <a:type. a. a>
    {a. value. value}
  )

  (module pair
    (define pair <type. type. type>
      {a. b.
        <c:type. <a. b. c>. c>
      }
    )

    (define make <a:type. b:type. a. b. [pair a b]>
      {a. b. x. y.
        {c. f. [f x y]}
      }
    )
  )
)
```
