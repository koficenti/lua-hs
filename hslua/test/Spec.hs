import Tokenizer (runParser, parseAST)
import Control.Applicative (Alternative(many))

main = do
    runCode "x = 5 \ny = 5.25 \nz = false \ntext = \"...some text...\"\nadd(1, 2)\n2 > 2\nx < y\nfunction add(x, y)\n    local z = x + y\n    return z\nend\nwhile fac(5) > fac(4) do\n    print(\"Testing\")\nend\nif 5 > 5 then\n    print(0)\nelseif 5 < 5 then\n    print(1)\nelse\n    print(2)\nend\nfunction greaterThan(x, y)\n    return if x > y then return x else return y end\nend\n\n"
runCode :: String -> IO()
runCode input = (\(Right (a, b)) -> putStrLn (input ++ "\n " ++ (unwords $ map ((++"\n") . show) a) ++ "\n")) $ runParser (many parseAST) input