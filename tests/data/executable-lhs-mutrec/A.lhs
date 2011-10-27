> module A
>        where
>
> import qualified B
>
> exportedString :: String
> exportedString = "another string"
>
> aString :: String
> aString = B.exportedString
