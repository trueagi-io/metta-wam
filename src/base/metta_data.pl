
% Comparison Operators in Prolog
is_comp_op('=', 2).          % Unification
is_comp_op('\\=', 2).        % Not unifiable
is_comp_op('==', 2).         % Strict equality
is_comp_op('\\==', 2).       % Strict inequality
is_comp_op('@<', 2).         % Term is before
is_comp_op('@=<', 2).        % Term is before or equal
is_comp_op('@>', 2).         % Term is after
is_comp_op('@>=', 2).        % Term is after or equal
is_comp_op('=<', 2).         % Less than or equal
is_comp_op('<', 2).          % Less than
is_comp_op('>=', 2).         % Greater than or equal
is_comp_op('>', 2).          % Greater than
is_comp_op('is', 2).         % Arithmetic equality
is_comp_op('=:=', 2).        % Arithmetic exact equality
is_comp_op('=\\=', 2).       % Arithmetic inequality
% Arithmetic Functions
is_math_func('*', 2, exists).         % Multiplication
is_math_func('+', 2, exists).         % Addition
is_math_func('-', 2, exists).         % Subtraction
is_math_func('/', 2, exists).         % Division
is_math_func('abs', 1, exists).       % Absolute value
is_math_func('acos', 1, maybe).       % Arc cosine
is_math_func('asin', 1, maybe).       % Arc sine
is_math_func('atan', 1, maybe).       % Arc tangent
is_math_func('atan2', 2, maybe).      % Two-argument arc tangent
is_math_func('cbrt', 1, maybe).       % Cube root
is_math_func('ceil', 1, maybe).       % Ceiling function
is_math_func('ceiling', 1, exists).   % Ceiling value
is_math_func('copysign', 2, maybe).   % Copy the sign of a number
is_math_func('cos', 1, exists).       % Cosine function
is_math_func('cosh', 1, maybe).       % Hyperbolic cosine
is_math_func('degrees', 1, maybe).    % Convert radians to degrees
is_math_func('div', 2, exists).       % Integer Division
is_math_func('erf', 1, maybe).        % Error function
is_math_func('erfc', 1, maybe).       % Complementary error function
is_math_func('exp', 1, exists).       % Exponential function
is_math_func('expm1', 1, maybe).      % exp(x) - 1
is_math_func('fabs', 1, maybe).       % Absolute value (floating-point)
is_math_func('float', 1, exists).     % Convert rational to float
is_math_func('float_fractional_part', 1, exists). % Fractional part of float
is_math_func('float_integer_part', 1, exists).    % Integer part of float
is_math_func('floor', 1, exists).     % Floor value
is_math_func('fmod', 2, maybe).       % Floating-point modulo operation
is_math_func('frexp', 2, maybe).      % Get mantissa and exponent
is_math_func('fsum', 1, maybe).       % Accurate floating point sum
is_math_func('gamma', 1, maybe).      % Gamma function
is_math_func('gcd', 2, exists).       % Greatest Common Divisor
is_math_func('hypot', 2, maybe).      % Euclidean norm, square root of sum of squares
is_math_func('integer', 1, exists).   % Convert float to integer
is_math_func('isinf', 1, maybe).      % Check for infinity
is_math_func('isnan', 1, maybe).      % Check for Not a Number
is_math_func('lcm', 2, exists).       % Least Common Multiple
is_math_func('ldexp', 2, maybe).      % Load exponent of a floating point number
is_math_func('lgamma', 1, maybe).     % Log gamma
is_math_func('log', 1, exists).       % Logarithm base e
is_math_func('log10', 1, maybe).      % Base 10 logarithm
is_math_func('log1p', 1, maybe).      % log(1 + x)
is_math_func('log2', 1, maybe).       % Base 2 logarithm
is_math_func('max', 2, exists).       % Maximum of two values
is_math_func('min', 2, exists).       % Minimum of two values
is_math_func('mod', 2, exists).       % Modulo operation
is_math_func('modf', 1, maybe).       % Return fractional and integer parts
is_math_func('pow', 2, maybe).        % Exponentiation
is_math_func('radians', 1, maybe).    % Convert degrees to radians
is_math_func('random', 1, exists).    % Random number generator
is_math_func('rational', 1, exists).  % Convert float to rational
is_math_func('rem', 2, exists).       % Remainder
is_math_func('remainder', 2, maybe).  % Remainder of the division
is_math_func('round', 1, exists).     % Round to nearest integer
is_math_func('sign', 1, exists).      % Sign of the number (-1,0,1)
is_math_func('sin', 1, exists).       % Sine function
is_math_func('sinh', 1, maybe).       % Hyperbolic sine
is_math_func('sqrt', 1, maybe).       % Square root
is_math_func('sqrt', 1, exists).      % Square Root
is_math_func('tan', 1, exists).       % Tangent function
is_math_func('tanh', 1, maybe).       % Hyperbolic tangent
is_math_func('trunc', 1, maybe).      % Truncate to an integral value
is_math_func('truncate', 1, exists).  % Truncate float to integer

