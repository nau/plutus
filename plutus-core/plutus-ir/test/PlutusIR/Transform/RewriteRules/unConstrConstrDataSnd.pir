(let
    (nonrec)
    (datatypebind
      (datatype
        (tyvardecl MyD_1099 (type))

        MyD_match_1102
        (vardecl MyD_1100 (fun (con integer) MyD_1099))
        (vardecl MyD_1101 (fun (con bytestring) MyD_1099))
      )
    )
         [{ { (builtin sndPair) (con integer) } [ (con list) (con data) ] }
          [
            (builtin unConstrData)
            [ (builtin constrData)
                  (con integer 0)
                  [ { (builtin mkCons) (con data) } [ (builtin iData) (con integer 1) ]
                      [ (builtin mkNilData) (con unit ()) ]
                      ]
            ]
          ]
         ]
)

