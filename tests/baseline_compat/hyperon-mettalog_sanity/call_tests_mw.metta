;; call-string!

!(assertEqualToResult (call-string! "member(1,[1,2,3])") (()))
!(assertEqualToResult (call-string! "member(1,[1,2,3])") ())
!(assertEqualToResult (call-string! "member(X,[1,2,3])") ((1) (2) (3)))
!(assertEqualToResult (call-string! "member(X,[[1,2],[3,4]])") (((1 2)) ((3 4))))
!(assertEqualToResult (call-string! "member([X,Y],[[1,2],[3,4]])") ((1 2) (3 4)))

;; call!

!(assertEqualToResult (call! parse_time "2006-12-08T15:29:44Z" iso_8601 1165591784.0) (True))
!(assertEqualToResult (call! parse_time "2006-12-08T15:29:44Z" iso_8601 $x) (True)) ;; handles metta variable (but doesn't return binding)
!(assertEqualToResult (call! parse_time "2006-12-08T15:29:44Z" iso_8601 X) (False)) ;; doesn't handle Prolog variable
!(assertEqualToResult (call! parse_time "2006-12-08T15:29:44Z" iso_8601 1165591700.0) (False))

;; call-p!

!(assertEqualToResult (call-p! parse_time "2006-12-08T15:29:44Z" iso_8601 1165591784.0) (True))
!(assertEqualToResult (call-p! parse_time "2006-12-08T15:29:44Z" iso_8601 $x) (True)) ;; handles metta variable (but doesn't return binding)
!(assertEqualToResult (call-p! parse_time "2006-12-08T15:29:44Z" iso_8601 X) (False)) ;; doesn't handle Prolog variable
!(assertEqualToResult (call-p! parse_time "2006-12-08T15:29:44Z" iso_8601 1165591700.0) (False))
