module Language.JSON.Helpers
import Language.JSON
import Language.JSON.Parser

public export
value : (json: JSON) -> (keyString: String) -> JSON
value object keyString = valueOf object (split (\x => x == '.') keyString)
  where
    valueOf : JSON -> List String -> JSON
    valueOf json [] = json
    valueOf json (x :: xs) = valueOf (valueLeaf json x) xs
      where
        valueLeaf : JSON -> String -> JSON
        valueLeaf (JObject xs) key = case filter (\(k,v) => k == key) xs of
          [] => JNull
          (k,v)::xs => v
        valueLeaf _ _ = JNull
    valueOf _ _ = JNull
