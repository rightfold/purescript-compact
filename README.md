# purescript-compact

purescript-compact provides a sum type that represents values without any
overhead. Because the JavaScript VM already provides a discriminator, there is
no need for a redundant one, when the variants are primitive types like `Int`
and `String`.
