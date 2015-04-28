Idris to VBA back end
---------------------

Based on Edwin Brady's PHP backend.

Generates .bas files which can be imported in to Microsoft Excel as a
module.

## Does this really work?

Yes! But it is not a complete backend by any means, there are many
situations which will not work just yet.

## Example

```
module Main

pythag : Int -> List (Int, Int, Int)
pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                          x * x + y *y == z * z]

main : IO ()
main = print (pythag 50)
```

![screenshot](https://github.com/jystic/idris-vba/raw/master/screenshot.png)
