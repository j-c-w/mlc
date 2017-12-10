val s1 = "string_one"
val s2 = "string_two"

val abc = "abc"
val abc' = "abc"

val res =
  if (s1 < s2) then
    if (s1 <= s2) then
      if (s2 > abc) then
        if (abc >= abc') then
          if (abc = abc') then
            if (abc <= abc') then
              if (s2 >= s1) then
                "pass"
              else ""
            else ""
          else ""
        else ""
      else ""
    else ""
  else ""

val _ = print res
