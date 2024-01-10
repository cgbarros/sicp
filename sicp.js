// ; Exercise 1.3
// ; Declare a function that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

// (define (square x)
//     (* x x))


// (define (sum-of-squares x y)
//     (+ (square x) (square y)))

// (define (sos-2-largest x y z)
//     (cond ((> x y) 
//               (cond ((> y z) (sum-of-squares x y))
//                     ((> z y) (sum-of-squares x z))))
//           ((> x z) (sum-of-squares x y))
//           (else (sum-of-squares y z))))
      
// (sos-2-largest 3 2 1)

function square(x){
    return x * x;
}

function sumOfSquares(x, y){
    return square(x) + square(y);
}

function sos2Largest(x, y, z){
    return (
        x > y
        ? y > z
          ? sumOfSquares(x, y)
          : sumOfSquares(x, z)
        : x > z
          ? sumOfSquares(y, x)
          : sumOfSquares(y, z)
    );
}

// sos2Largest(1, 2, 3);
// sos2Largest(1, 3, 2);
// sos2Largest(3, 1, 2);
// sos2Largest(3, 2, 1);
// sos2Largest(2, 3, 1);
// sos2Largest(2, 1, 3);
