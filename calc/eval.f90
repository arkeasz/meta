module evaluator
    use ast
    use math
    use, intrinsic :: iso_fortran_env, only : dw => real64
    use, intrinsic :: ieee_arithmetic
    implicit none
    private
    public :: eval
contains
  recursive function eval(node) result(val)
    implicit none

    ! COMMON CONSTANTS
    real, parameter :: PI = 4.0*atan(1.0)
    real(dw), parameter :: E = exp(1.0_dw)

    type(ASTNode), pointer, intent(in), optional :: node
    logical :: is_nan_value
    ! the value of the indentifier, SOON THE SYMTAB FOR HARD CALCULUS

    real(dw) :: val
    ! right, left and argument
    real(dw) :: lval, rval, arg
    ! the operator
    character(len=:), allocatable :: opname

    val = 0.0_dw
    if (.not. present(node)) then
      val = 0.0_dw
      return
    end if
    if (.not. associated(node)) then
      val = 0.0_dw
      return
    end if

    select case (node%kind)
        case (NODE_NUM)
            val = node%nval
        case (NODE_IDENT)
            if (node%name == "pi") then
                val = PI
            else if (node%name == "e") then 
                val = E
            else 
                val = 0.0_dw
            end if
            ! if the memory is actually asigned in the memory
        case (NODE_OP) ! (op, left, right)
            ! the left eval
            if (associated(node%left)) then 
                lval = eval(node%left)
            else
                lval = 0.0_dw
            end if
            
            ! the right eval
            if (associated(node%right)) then 
                rval = eval(node%right)
            else
                rval = 0.0_dw
            end if
            
            opname = trim(node%op)

            select case (opname)
                case("+")
                    val = lval + rval
                case("-")
                    val = lval - rval
                case("*")
                    val = lval * rval
                case("/")
                    if (rval == 0.0_dw) then
                        print *, "over zero? r u sirius?"
                    else 
                        val = lval/rval
                    end if
                case("^")
                    val = lval**rval
                case("neg")
                    val = -lval
                case default
                    print *, "unknown operator"
                    val = 0.0_dw
            end select

        case (NODE_FUNC) ! (name_func, left, null) only two args
            if (associated(node%left)) then 
                arg = eval(node%left) ! node%left maybe an expression
                select case(trim(node%op))
                    case("sin")
                        val = sin(arg)
                    case("cos")
                        val = cos(arg)
                    case("fact")
                        val = factorial(arg)
                    case("tan")
                        val = tan(arg)
                    case("asin")
                        val = asin(arg)
                    case("acos")
                        val = acos(arg)
                    case("atan")
                        val = atan(arg)
                    case("sqrt")
                        if (arg < 0.0_dw) then
                            print *, "negative value?"
                            val = 0.0_dw
                        else 
                            val = sqrt(arg)
                        end if
                    case default 
                        print *, "unknown operator"
                end select
            else 
                val = 0.0_dw
            end if
        case default
            write(*,'(A)') "Unknown node kind"
            val = 0.0_dw
    end select

    is_nan_value = ieee_is_nan(val)

    if (is_nan_value) then
        print *, "the value is NaN"
    end if
  end function eval


end module evaluator
