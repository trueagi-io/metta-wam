;; this is loaded as   !(import &self fileio)

!(in-space builtins::fileio)

!(inherit-from &corelib builtins::fileio)

; This add ther type to the type system
(: FileHandle Type)
; the system level get-type is nondetermninistic therefore returns all willing participants 
(= (get-type $obj)
   (call-for! FileHandle  ; call for returns the first argument
	 (@ (atomic $obj) ; stream might return an error if obj is not atomic
	 (stream $obj)))) ; if obj is a stream we are calling it a filehandle since prolog streams already support this api

(= (parse-mode $chars)
   (call-for! $mode
	 (@ 
	 (or
	   (and (memberchk 'r' $chars) (memberchk 'w' $chars) (= $mode update))
	   (and (memberchk 'a' $chars) (= $mode append))
	   (and (memberchk 'w' $chars) (= $mode write))
	   (and (memberchk 'r' $chars) (= $mode read))
	   (= $mode read)))))

(= (handle-create-options $path $chars)
   (call-unit!
	 (@ (if_t (and (memberchk 'n' $chars) (exists_file $path))
		 (throw (error (file_exists_error $path))))
	   (if_t (and (memberchk 'c' $chars) (not (exists_file $path)))
		 (setup_call_cleanup (open $path write $s) (close $s) true))
	   (if_t (and (memberchk 't' $chars) (exists_file $path))
		 (setup_call_cleanup (open $path write $s) (close $s) true)))))

(@doc file-open!
  (@desc "Function takes path to the file and open options (r, w, c, a, t) both in form of string, creates filehandle and
  returns it")
  (@params (
	(@param "Filepath (string atom)")
	(@param "Open options (string atom), r - read, w - write, c - create if file doesn't exist, a - append to file,
	  t - truncate file")))
  (@return "Filehandle or error if combination of path and open options is wrong (e.g. file doesn't exist and no 'c'
  in options; or 'rc' option provided, since 'c' demands for 'w')"))
(: file-open! (-> String String FileHandle))
; tells the compiler to return only the first successful clause
(iz file-open! Deterministic)
(= (file-open! $fpath $opt)
   (call-for! $stream (@
	 (string_chars $opt $chars)
	 (any_to_atom $fpath $path)
	 (= $mode (parse-mode $chars))
	 (handle-create-options $path $chars)
	 (open $path $mode $stream [ (type text) ]))))
(= (file-open-err! $path $opt)
   (call-for! $_err
	 (format (string $msg)
	   "Failed to open file with provided path=~w and options=~w"
	   [$path $opt])
	 (throw (error (file_open_error $msg)))))

(@doc file-read-to-string!
  (@desc "Function takes filehandle provided by file-open! reads its content from current cursor place till the end of
  file and returns content in form of string.")
  (@params (
	(@param "Filehandle")))
  (@return "File's content"))
(: file-read-to-string! (-> FileHandle String))
(= (file-read-to-string! $stream)
   (call-fn! read_string $stream $_ ))

(@doc file-write!
  (@desc "Function takes filehandle provided by file-open!, content to be written (string atom) and puts content into
  file associated with filehandle")
  (@params (
	(@param "Filehandle")
	(@param "Content (string atom)")))
  (@return "Unit atom"))
(: file-write! (-> FileHandle String Unit))
(= (file-write! $stream $content)
   (call-unit! (,
	 (write $stream $content)
	 (flush_output $stream))))

(@doc file-seek!
  (@desc "Function takes filehandle provided by file-open! and desired cursor position (number) and sets cursor to
  provided position")
  (@params (
	(@param "Filehandle")
	(@param "Desired cursor position (number)")))
  (@return "Unit atom"))
(: file-seek! (-> FileHandle Number Unit))
(= (file-seek! $stream $offset)
   (call-unit!
	 (@ (seek $stream $offset bof $_))))

(@doc file-read-exact!
  (@desc "Function takes filehandle provided by file-open! and desired number of bytes to read (number), reads content
  of file from current cursor position (number of read bytes <= input number of bytes to read) and returns it in form of
  string")
  (@params (
	(@param "Filehandle")
	(@param "Number of bytes to read")))
  (@return "File's content"))
(: file-read-exact! (-> FileHandle Number String))
(= (file-read-exact! $stream $bytes)
   (call-for! $content ; runs this code returning the binding of $content
	 (read_string $stream $bytes $content)))

(@doc file-get-size!
  (@desc "Function takes filehandle provided by file-open! and returns size of file")
  (@params (
	(@param "Filehandle")))
  (@return "Size of file"))
(: file-get-size! (-> FileHandle Number))
(= (file-get-size! $stream)
   (call-for! $size ; runs this code returning the binding of $size
	 (stream_property $stream (file_name $file))
	 (size_file $file $size)))

(@doc file-close!
  (@desc "Function takes filehandle provided by file-open! and closes it")
  (@params (
	(@param "Filehandle")))
  (@return "Unit atom"))
(: file-close! (-> FileHandle Unit))
(= (file-close! $stream)
   (call-unit!
	 (close $stream)))



;; Auto-close wrapper: like (with-file-handle (file-open! path opts) $fh (progn ...))
(@doc with-file-handle
  (@desc "Executes a body expression with a file handle obtained from an open expression, ensuring the file is automatically closed after execution. This is similar to `setup_call_cleanup` in Prolog, used for safe file I/O.
### Usage Pattern

```metta
!(with-file-handle 
  (file-open! \"somefile.txt\" \"wc\") 
  $fh 
  (file-write! $fh \"Hello\\n\"))
``` 



Or for read-and-return:

```metta
!(with-file-handle 
  (file-open! \"somefile.txt\" \"r\") 
  $fh 
  (file-read-to-string! $fh))
```

")
  (@params (
    (@param "File open expression (Atom) - typically a call to `file-open!`")
    (@param "Variable to bind the resulting file handle to")
    (@param "Body expression to execute using the file handle")))
  (@return "Result of the body expression. The file is closed afterward, even if the body fails or throws an error."))


(: with-file-handle! (-> Atom Variable Atom Atom))
(= (with-file-handle $open_expr $fh $body)
  (function
	;; open the file
	(let $fh (eval $open_expr)
	;; run the body, binding result
	 (let $result (eval $body)
	  ;; ensure closing
		(progn (file-close! $fh) 
		  (return $result))))))


; Load system libs (not required .. but more here for reference)
;!(call-unit!
;  (use_module (library apply))
;  (use_module (library filesex)))


!(in-space test::fileio)
!(inherit-from test::fileio builtins::fileio)

;; to run the IO tests
(= (run-io-tests)
   (match test::fileio (test::def $name $body)
		(assertSucceed $name $body )))

; Create a temporary file name
(: tmpfile-path (-> String))
(= (tmpfile-path)
   (call-fn string_concat "test_tmpfile_" (call-fn! random 6)))
	  

; Test writing to a new file and reading it back
(test::def file-write-read
  (let $path (tmpfile-path)
    (let $fh (file-open! $path "wc") ; write + create
	 (progn
      (file-write! $fh "Hello, world!")
      (file-close! $fh)
      (let $rfh (file-open! $path "r")
        (= (file-read-to-string! $rfh) "Hello, world!")
        (file-close! $rfh))))))

; Test append mode
(test::def file-append
  (let $path (tmpfile-path)
    (let $fh1 (file-open! $path "wc")
      (file-write! $fh1 "Line1\n")
      (file-close! $fh1)
      (let $fh2 (file-open! $path "a")
        (file-write! $fh2 "Line2\n")
        (file-close! $fh2)
        (let $rfh (file-open! $path "r")
          (= (file-read-to-string! $rfh) "Line1\nLine2\n")
          (file-close! $rfh))))))

; Test seek and partial read
(test::def file-seek-read
  (let $path (tmpfile-path)
    (let $fh (file-open! $path "wc")
	 (progn
      (file-write! $fh "abcdefg")
      (file-close! $fh)
      (let $rfh (file-open! $path "r")
	   (progn
        (file-seek! $rfh 2)
        (= (file-read-exact! $rfh 3) "cde")
        (file-close! $rfh)))))))

; Test file size
(test::def file-get-size
  (let $path (tmpfile-path)
    (let $fh (file-open! $path "wc")
	 (progn
      (file-write! $fh "1234567890")
      (file-close! $fh)
      (let $rfh (file-open! $path "r")
	   (progn
        (= (file-get-size! $rfh) 10)
        (file-close! $rfh)))))))

; Test error on reading nonexistent file
(test::def file-read-error
  (let $err (try-catch
              (file-open! "nonexistent_file.txt" "r")
              (lambda $e $e))
    (== (type-of $err) error)))


;;  !(write-file! (file-open! "foooo" "w") "in foo")


(test::def file-write-read
  (let $path (tmpfile-path)
    (with-file-handle (file-open! $path "wc") $fh
      (file-write! $fh "Hello, world!"))
    (with-file-handle (file-open! $path "r") $rfh
      (= (file-read-to-string! $rfh) "Hello, world!"))))



