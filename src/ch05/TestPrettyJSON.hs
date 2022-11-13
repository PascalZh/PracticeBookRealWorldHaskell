import Prettify (nest)
import PrettyJSON
import SimpleJSON (JValue (..))

main :: IO ()
main = do
  let value =
        renderJValue
          (JObject [ ("f", JNumber 1)
                   , ("q", JBool True)
                   , ("a", JArray (JNumber <$> [1..20]))
                   , ("d", JString "foobar")
                   ])
  putStrLn (nest 2 value)
  putStrLn (nest 20 value)
  putStrLn (nest 40 value)
  putStrLn (nest 2000 value)
