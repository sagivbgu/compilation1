(let ((str "hi three"))
  (string-set! str 5 #\e)
  (string-set! str 6 #\r)
  str)

; Output: "hi there"