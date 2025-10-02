program calculator
    use class_parser
    implicit none
    
    type(Parser) :: p

    p = new_parser("x^2 + 3*x - 5 + 2sin4y")
    call tokenizer(p)
    p = new_parser("0.5x^2 + 3*x - 5 + sin(x)")
    call tokenizer(p)
    p = new_parser("4.5x^2 + 3*x - 5 + sinx")
    call tokenizer(p)
end program calculator
