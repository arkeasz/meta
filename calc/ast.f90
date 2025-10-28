module ast
  implicit none
  private
  public :: ASTNode, NODE_NUM, NODE_IDENT, NODE_OP, NODE_FUNC
  public :: make_num, make_ident, make_func, make_binop, make_unop, print_tree

  integer, parameter :: NODE_NUM   = 0
  integer, parameter :: NODE_IDENT = 1
  integer, parameter :: NODE_OP    = 2
  integer, parameter :: NODE_FUNC  = 3

  type :: ASTNode
    integer :: kind = -1
    real(kind=8) :: nval = 0.0d0
    character(:), allocatable :: name
    character(:), allocatable :: op
    type(ASTNode), pointer :: left  => null()
    type(ASTNode), pointer :: right => null()
  end type ASTNode

contains
  function make_func(func_name, arg) result(n)
    character(len=*), intent(in) :: func_name
    type(ASTNode), pointer, intent(in) :: arg
    type(ASTNode), pointer :: n
    character(len=:), allocatable :: t

    allocate(n)
    n%kind = NODE_FUNC
    n%left => arg
    n%right => null()

    t = adjustl(func_name) ! sin, cos, atan
    allocate(character(len=len_trim(t)) :: n%op)
    n%op = trim(t)
  end function make_func

  function make_num(lex) result(n)
    character(len=*), intent(in) :: lex
    type(ASTNode), pointer :: n
    real(kind=8) :: val
    integer :: ios
    character(len=:), allocatable :: t

    allocate(n)
    n%kind = NODE_NUM
    n%left => null()
    n%right => null()

    t = adjustl(lex)
    val = 0.0d0
    read(t, *, iostat=ios) val
    if (ios /= 0) then
      write(*,*) "Warning: make_num: cannot convert '", trim(lex), "' to number; using 0.0"
      val = 0.0d0
    end if
    n%nval = val

    allocate(character(len=len_trim(t)) :: n%op)
    n%op = trim(t)
  end function make_num

  function make_ident(lex) result(n)
    character(len=*), intent(in) :: lex
    type(ASTNode), pointer :: n
    character(len=:), allocatable :: t

    allocate(n)
    n%kind = NODE_IDENT
    n%nval = 0.0d0
    n%left => null()
    n%right => null()

    t = adjustl(lex)
    allocate(character(len=len_trim(t)) :: n%name)
    n%name = trim(t)

    allocate(character(len=len_trim(t)) :: n%op)
    n%op = trim(t)
  end function make_ident

  function make_binop(op_lex, l, r) result(n)
    character(len=*), intent(in) :: op_lex
    type(ASTNode), pointer :: l, r
    type(ASTNode), pointer :: n
    character(len=:), allocatable :: t

    allocate(n)
    n%kind = NODE_OP
    n%left => l
    n%right => r

    t = adjustl(op_lex)
    allocate(character(len=len_trim(t)) :: n%op)
    n%op = trim(t)
  end function make_binop

  function make_unop(op_lex, child) result(n)
    character(len=*), intent(in) :: op_lex
    type(ASTNode), pointer :: child
    type(ASTNode), pointer :: n
    character(len=:), allocatable :: t

    allocate(n)
    n%kind = NODE_OP
    n%left => child
    n%right => null()

    t = adjustl(op_lex)
    allocate(character(len=len_trim(t)) :: n%op)
    n%op = trim(t)
  end function make_unop

  recursive subroutine print_tree(node, indent)
    type(ASTNode), pointer, intent(in), optional :: node
    integer, intent(in), optional :: indent
    integer :: lvl

    if (present(indent)) then
      lvl = indent
    else
      lvl = 0
    end if

    if (.not. present(node)) then
      write(*,*) "NULL AST"
      return
    end if
    if (.not. associated(node)) then
      write(*,'(A)') repeat(' ', lvl)//"(null)"
      return
    end if

    select case (node%kind)
    case (NODE_NUM)
      write(*,'(A)') repeat(' ', lvl)//"NUM: " // trim(node%op)
    case (NODE_IDENT)
      write(*,'(A)') repeat(' ', lvl)//"IDENT: " // trim(node%name)
    case (NODE_OP)
      write(*,'(A)') repeat(' ', lvl)//"OP: " // trim(node%op)
      if (associated(node%left)) call print_tree(node%left, lvl+2)
      if (associated(node%right)) call print_tree(node%right, lvl+2)
    case (NODE_FUNC)
      write(*,'(A)') repeat(' ', lvl)//"FUNC: " // trim(node%op)
      if (associated(node%left)) call print_tree(node%left, lvl+2)
    case default
      write(*,'(A)') repeat(' ', lvl)//"Unknown node kind"
    end select
  end subroutine print_tree

end module ast
