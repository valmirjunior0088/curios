# Curios

Curios is a dependently typed functional programming language.

### Example source

```
(define identity <a:type. a. a>
  {a. value. value}
)

(module pair
  (define pair <type. type. type>
    {a. b.
      <z:type. <a. b. z>. z>
    }
  )

  (define make <a:type. b:type. a. b. [pair a b]>
    {a. b. first. second.
      {z. f. [f first second]}
    }
  )
    
  (define first <a:type. b:type. [pair a b]. a>
    {a. b. data.
      [data a {first. second. first}]
    }
  )
    
  (define second <a:type. b:type. [pair a b]. b>
    {a. b. data.
      [data b {first. second. second}]
    }
  )
)

(module either
  (define either <type. type. type>
    {a. b.
      <z:type. <a. z>. <b. z>. z>
    }
  )

  (define left <a:type. b:type. a. [either a b]>
    {a. b. value.
      {z. case-left. case-right. [case-left value]}
    }
  )
    
  (define right <a:type. b:type. b. [either a b]>
    {a. b. value.
      {z. case-left. case-right. [case-right value]}
    }
  )

  // To destructure an instance of either...
  // One must apply a function for each of the two possibilities:
  // [either-value string {value. "left branch"} {value. "right branch"}]
)

(module natural
  (define natural type
    // ?
  )

  (define zero natural
    // ?
  )

  (define successor <natural. natural>
    // ?
  )
)

(module list
  (define list <type. type>
    // ?
  )

  (define empty <a:type. [list a]>
    // ?
  )

  (define cell <a:type. a. [list a]. [list a]>
    // ?
  )
)
```
