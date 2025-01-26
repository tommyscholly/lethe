fun fizzbuzz(n: int): string =
    case (n mod 3, n mod 5) of
        (0, 0) => "FizzBuzz"
      | (0, _) => "Fizz"
      | (_, 0) => "Buzz"
      | _ => n
