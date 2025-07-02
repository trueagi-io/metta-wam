;; this is loaded as   !(import &self fileio)

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
	   (and (memberchk r $chars) (memberchk w $chars) (= $mode update))
	   (and (memberchk a $chars) (= $mode append))
	   (and (memberchk w $chars) (= $mode write))
	   (and (memberchk r $chars) (= $mode read))
	   (= $mode read)))))

(= (handle-create-options $path $chars)
   (call-unit!
	 (@ (if-then (and (memberchk n $chars) (exists_file $path))
	   (throw (error (file_exists_error $path))))
	 (if-then (and (memberchk c $chars) (not (exists_file $path)))
	   (setup_call_cleanup (open $path write $s) (close $s) true))
	 (if-then (and (memberchk t $chars) (exists_file $path))
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

; Load system libs (not required .. but more here for reference)
;!(call-unit!
;  (use_module (library apply))
;  (use_module (library filesex)))


