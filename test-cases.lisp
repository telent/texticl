
(IN-PACKAGE :TEXTICL) 
(LET ((C
       '(("and this one _too_!" "and this one <i>too</i>!")
         ("the last word in this sentence is _italic_."
          "the last word in this sentence is <i>italic</i>.")
         ("testing text case saving" "testing text case saving")
         ("hello _worlsd_ there \"boo\":http://www.example.com/_help_.html "
          "hello <i>worlsd</i> there <a href=\"http://www.example.com/_help_.html\" class=external>boo</a> ")
         ("pass thru" "pass thru")
         ("some _underlined_ text" "some <i>underlined</i> text")
         ("http://www.example.com"
          "<a href=\"http://www.example.com\" class=external>http://www.example.com</a>")
         ("ftp://www.example.com/ foo.html"
          "<a href=\"ftp://www.example.com/\" class=external>ftp://www.example.com/</a> foo.html")
         ("http://www.example.com/ foo.html"
          "<a href=\"http://www.example.com/\" class=external>http://www.example.com/</a> foo.html")
         ("hello @text@  " "hello <tt>text</tt>  ")
         ("hello @text <  > with _styff_ @  "
          "hello <tt>text &#60;  &#62; with _styff_ </tt>  "))))
  (IF (> (LENGTH C) (LENGTH *TEST-CASES*)) (SETF *TEST-CASES* C)
      (WARN "not overwriting, fewer tests"))) 
