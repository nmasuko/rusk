func fib(n: Int) -> Int{
    if n < 2{
        n
    }
    else{
        fib(n-1) + fib(n-2)
    }
}
fib(40)