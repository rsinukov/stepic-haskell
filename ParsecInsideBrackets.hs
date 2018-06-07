import Text.Parsec

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces brL brR content = manyTill anyToken brL *> content <* brR

-- test = ignoreBraces (string "[[") (string "]]") (many1 letter)
-- parseTest test "[[ABC]]DEF"