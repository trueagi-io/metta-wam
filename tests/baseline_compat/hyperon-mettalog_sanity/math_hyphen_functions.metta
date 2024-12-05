; Test pow-math with integer base and exponent, expecting 5^2 = 25
!(assertEqualToResult (pow-math 5 2) (25))

; Test pow-math with a large exponent, expecting an error about power being too big
!(assertEqualToResult (pow-math 5 200000000000000) ((Error (pow-math 5 200000000000000) "power argument is too big, try using float value")))

; Test pow-math with floating-point base and exponent, expecting 5.5^2.3 ≈ 38.2597
!(assertEqualToResult (pow-math 5.5 2.3) (38.2596979949377))

; Test pow-math with invalid base 'A', expecting an error about invalid arguments
!(assertEqualToResult (pow-math A 2) ((Error (pow-math A 2) "pow-math expects two arguments: number (base) and number (power)")))

; Test sqrt-math with positive integer input, expecting sqrt(4) = 2
!(assertEqualToResult (sqrt-math 4) (2))

; Test sqrt-math with negative input, expecting result to be NaN
!(assertEqualToResult (isnan-math (sqrt-math -4)) (True))

; Test sqrt-math with invalid input 'A', expecting an error
!(assertEqualToResult (sqrt-math A) ((Error (sqrt-math A) "sqrt-math expects one argument: number")))

; Test abs-math with positive input, expecting abs(4) = 4
!(assertEqualToResult (abs-math 4) (4))

; Test abs-math with negative input, expecting abs(-5) = 5
!(assertEqualToResult (abs-math -5) (5))

; Test abs-math with invalid input 'A', expecting an error
!(assertEqualToResult (abs-math A) ((Error (abs-math A) "abs-math expects one argument: number")))

; Test log-math with valid inputs, expecting log base 2 of 4 equals 2
!(assertEqualToResult (log-math 2 4) (2))

; Test log-math with zero inputs, expecting result to be NaN
!(assertEqualToResult (isnan-math (log-math 0 0)) (True))

; Test log-math resulting in infinity, expecting isinf-math to return true
!(assertEqualToResult (isinf-math (log-math 5 0)) (True))

; Test trunc-math with positive float input, expecting trunc(2.4) = 2
!(assertEqualToResult (trunc-math 2.4) (2))

; Test trunc-math with invalid input 'A', expecting an error
!(assertEqualToResult (trunc-math A) ((Error (trunc-math A) "trunc-math expects one argument: input number")))

; Test ceil-math with positive float input, expecting ceil(2.4) = 3
!(assertEqualToResult (ceil-math 2.4) (3))

; Test ceil-math with negative float input, expecting ceil(-2.4) = -2
!(assertEqualToResult (ceil-math -2.4) (-2))

; Test ceil-math with invalid input 'A', expecting an error
!(assertEqualToResult (ceil-math A) ((Error (ceil-math A) "ceil-math expects one argument: input number")))

; Test floor-math with positive float input, expecting floor(2.4) = 2
!(assertEqualToResult (floor-math 2.4) (2))

; Test floor-math with negative float input, expecting floor(-2.4) = -3
!(assertEqualToResult (floor-math -2.4) (-3))

; Test floor-math with invalid input 'A', expecting an error
!(assertEqualToResult (floor-math A) ((Error (floor-math A) "floor-math expects one argument: input number")))

; Test round-math with positive float input, expecting round(2.4) = 2
!(assertEqualToResult (round-math 2.4) (2))

; Test round-math with negative float input, expecting round(-2.7) = -3
!(assertEqualToResult (round-math -2.7) (-3))

; Test round-math with invalid input 'A', expecting an error
!(assertEqualToResult (round-math A) ((Error (round-math A) "round-math expects one argument: input number")))

; Test sin-math with input 0, expecting sin(0) = 0
!(assertEqualToResult (sin-math 0) (0))

; Test sin-math with input approximately π/2, expecting result close to 1
!(assertEqualToResult (< (abs-math (- (sin-math 1.570796327) 1.0)) 1e-10) (True))

; Test sin-math with invalid input 'A', expecting an error
!(assertEqualToResult (sin-math A) ((Error (sin-math A) "sin-math expects one argument: input number")))

; Test asin-math with input 0, expecting asin(0) = 0
!(assertEqualToResult (asin-math 0) (0))

; Test asin-math with sin(1), expecting asin(sin(1)) ≈ 1
!(assertEqualToResult (< (abs-math (- (asin-math (sin-math 1)) 1.0)) 1e-10) (True))

; Test asin-math with invalid input 'A', expecting an error
!(assertEqualToResult (asin-math A) ((Error (asin-math A) "asin-math expects one argument: input number")))

; Test cos-math with input 0, expecting cos(0) = 1
!(assertEqualToResult (cos-math 0) (1))

; Test cos-math with input approximately π/2, expecting result close to 0
!(assertEqualToResult (< (abs-math (cos-math 1.570796327)) 1e-10) (True))

; Test cos-math with invalid input 'A', expecting an error
!(assertEqualToResult (cos-math A) ((Error (cos-math A) "cos-math expects one argument: input number")))

; Test acos-math with input 1, expecting acos(1) = 0
!(assertEqualToResult (acos-math 1) (0))

; Test acos-math with cos(1), expecting acos(cos(1)) ≈ 1
!(assertEqualToResult (< (abs-math (- (acos-math (cos-math 1)) 1.0)) 1e-10) (True))

; Test acos-math with invalid input 'A', expecting an error
!(assertEqualToResult (acos-math A) ((Error (acos-math A) "acos-math expects one argument: input number")))

; Test tan-math with input 0, expecting tan(0) = 0
!(assertEqualToResult (tan-math 0) (0))

; Test tan-math with input approximately π/4, expecting result close to 1
!(assertEqualToResult (< (abs-math (- (tan-math 0.78539816339) 1.0)) 1e-10) (True))

; Test tan-math with invalid input 'A', expecting an error
!(assertEqualToResult (tan-math A) ((Error (tan-math A) "tan-math expects one argument: input number")))

; Test atan-math with input 0, expecting atan(0) = 0
!(assertEqualToResult (atan-math 0) (0))

; Test atan-math with tan(1), expecting atan(tan(1)) ≈ 1
!(assertEqualToResult (< (abs-math (- (atan-math (tan-math 1)) 1.0)) 1e-10) (True))

; Test atan-math with invalid input 'A', expecting an error
!(assertEqualToResult (atan-math A) ((Error (atan-math A) "atan-math expects one argument: input number")))

; Test isnan-math with a valid number, expecting false
!(assertEqualToResult (isnan-math 0) (False))

; Test isnan-math with NaN input from log-math(0,0), expecting true
!(assertEqualToResult (isnan-math (log-math 0 0)) (True))

; Test isnan-math with invalid input 'A', expecting an error
!(assertEqualToResult (isnan-math A) ((Error (isnan-math A) "isnan-math expects one argument: input number")))

; Test isinf-math with a valid number, expecting false
!(assertEqualToResult (isinf-math 0) (False))

; Test isinf-math with infinity input from log-math(5,0), expecting true
!(assertEqualToResult (isinf-math (log-math 5 0)) (True))

; Test isinf-math with invalid input 'A', expecting an error
!(assertEqualToResult (isinf-math A) ((Error (isinf-math A) "isinf-math expects one argument: input number")))
