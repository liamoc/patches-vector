import Test.DocTest

main :: IO ()
main = doctest ["Data/Patch/Internal.hs", "-i", "test", "-i."]
